use std::{collections::HashMap, rc::Rc};

use naivc_lexer::{
    dfa::{DFAState, DFAStateHandle, DeterministicFiniteAutomaton},
    token::TokenType,
};
use proc_macro2::TokenStream;
use quote::quote;
use syn::{Error, Ident, Result, Variant};

use super::attr::LexerAttribute;

pub struct LexerDFABuilder {
    pub lexer_name: Ident,
    pub spec: Vec<LexerAttribute>,
}

impl LexerDFABuilder {
    pub fn new<'a>(lexer_name: Ident, spec: impl Iterator<Item = &'a Variant>) -> Result<Self> {
        let mut lexer_spec = Vec::new();
        for enum_variant in spec {
            match LexerAttribute::new(enum_variant.ident.clone(), enum_variant.attrs.clone()) {
                Ok(Some(attr)) => lexer_spec.push(attr),
                Ok(None) => {} // Skip variants without regex attribute
                Err(e) => return Err(e),
            }
        }
        Ok(LexerDFABuilder {
            lexer_name,
            spec: lexer_spec,
        })
    }

    pub fn build(self) -> LexerDFA {
        let token_types: Vec<Rc<TokenType>> = self
            .spec
            .iter()
            .map(|attr| {
                Rc::new(TokenType::new(
                    attr.token_type_name.to_string(),
                    attr.regex.regex.clone(),
                ))
            })
            .collect();
        let dfa = DeterministicFiniteAutomaton::from_lexer_spec(&token_types);
        let token_types_map: HashMap<_, _> = self
            .spec
            .into_iter()
            .map(|attr| (attr.token_type_name.to_string(), attr))
            .collect();
        LexerDFA::new(self.lexer_name, dfa, token_types_map)
    }
}

pub struct LexerDFA {
    pub lexer_name: Ident,
    pub dfa: DeterministicFiniteAutomaton,
    // TODO: We could consider make the TokenType generic and contain the LexerAttribute directly
    pub token_types_map: HashMap<String, LexerAttribute>,
    pub state_enum_ident: Ident,
    pub states_map: HashMap<DFAStateHandle, Ident>,
}

impl LexerDFA {
    pub fn new(
        lexer_name: Ident,
        dfa: DeterministicFiniteAutomaton,
        token_types_map: HashMap<String, LexerAttribute>,
    ) -> Self {
        let state_enum_ident = Ident::new(&format!("{}__State", lexer_name), lexer_name.span());
        let states_map = dfa
            .iter_states()
            .map(|(state_handle, _)| {
                let state_ident = Ident::new(
                    &format!("__{}__State{}", lexer_name, state_handle.index()),
                    lexer_name.span(),
                );
                (state_handle, state_ident)
            })
            .collect::<HashMap<_, _>>();
        LexerDFA {
            lexer_name,
            dfa,
            token_types_map,
            state_enum_ident,
            states_map,
        }
    }

    pub fn state_enum(&self) -> TokenStream {
        let state_enum_ident = &self.state_enum_ident;
        let state_variants = self.states_map.values().map(|state_ident| {
            quote! {
                #state_ident,
            }
        });
        quote! {
            #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
            pub enum #state_enum_ident {
                #(#state_variants)*
            }
        }
    }

    pub fn start_impl(&self) -> Result<TokenStream> {
        let state_enum_ident = &self.state_enum_ident;
        let start_state_ident = self.states_map.get(&self.dfa.start_state()).ok_or_else(|| {
                    Error::new(
                        self.lexer_name.span(),
                        format!(
                            "start state handle {:?} not found in states map",
                            &self.dfa.start_state()
                        ),
                    )
                })?;
        Ok(quote! {
            impl #state_enum_ident {
                pub fn start() -> Self {
                    #state_enum_ident::#start_state_ident
                }
            }
        })
    }

    pub fn transition_arms_impl(
        &self,
        current_ident: &Ident,
        matchable_ident: Ident,
        state: &DFAState,
    ) -> Result<TokenStream> {
        let state_enum_ident = &self.state_enum_ident;
        if state.transitions.is_empty() {
            return Ok(quote! {
                #state_enum_ident::#current_ident => None,
            });
        }
        let inner_armss: Result<Vec<_>> = state
            .transitions
            .iter()
            .map(|transition| {
                let target_ident = self.states_map.get(&transition.target).ok_or_else(|| {
                    Error::new(
                        self.lexer_name.span(),
                        format!(
                            "state handle {:?} not found in states map",
                            &transition.target
                        ),
                    )
                })?;
                let start_char = transition.range.start();
                let end_char = transition.range.end();
                if start_char == end_char {
                    Ok(quote! {
                        #start_char => Some(#state_enum_ident::#target_ident),
                    })
                } else {
                    Ok(quote! {
                        #start_char..=#end_char => Some(#state_enum_ident::#target_ident),
                    })
                }
            })
            .collect();
        let inner_arms = inner_armss?;
        Ok(quote! {
            #state_enum_ident::#current_ident => {
                match #matchable_ident {
                    #(#inner_arms)*
                    _ => None,
                }
            },
        })
    }

    pub fn on_impl(&self) -> Result<TokenStream> {
        let state_enum_ident = &self.state_enum_ident;
        let match_arms: Result<Vec<_>> = self
            .dfa
            .iter_states()
            .map(|(state_handle, state)| {
                let current_ident = self.states_map.get(&state_handle).ok_or_else(|| {
                    Error::new(
                        self.lexer_name.span(),
                        format!("state handle {:?} not found in states map", state_handle),
                    )
                })?;
                let matchable_ident = Ident::new("c", self.lexer_name.span());
                self.transition_arms_impl(current_ident, matchable_ident, &state)
            })
            .collect();
        let match_arms = match_arms?;
        Ok(quote! {
            impl #state_enum_ident {
                pub fn on(&self, c: char) -> Option<Self> {
                    match self {
                        #(#match_arms)*
                    }
                }
            }
        })
    }

    pub fn accept_impl(&self) -> Result<TokenStream> {
        let lexer_name = &self.lexer_name;
        let state_enum_ident = &self.state_enum_ident;
        let accept_arms: Result<Vec<_>> = self
            .dfa
            .iter_states()
            .map(|(state_handle, state)| {
                let current_ident = self.states_map.get(&state_handle).ok_or_else(|| {
                    Error::new(
                        lexer_name.span(),
                        format!("state handle {:?} not found in states map", state_handle),
                    )
                })?;
                if let Some(accept_pattern) = &state.accept_pattern {
                    let token_type_name = &accept_pattern.name;
                    let token_type_ident = &self
                        .token_types_map
                        .get(token_type_name)
                        .ok_or_else(|| {
                            Error::new(
                                lexer_name.span(),
                                format!(
                                    "token type '{}' not found in token types map",
                                    token_type_name
                                ),
                            )
                        })?
                        .token_type_name;
                    Ok(quote! {
                        #state_enum_ident::#current_ident => Some(#lexer_name::#token_type_ident),
                    })
                } else {
                    Ok(quote! {
                        #state_enum_ident::#current_ident => None,
                    })
                }
            })
            .collect();
        let accept_arms = accept_arms?;
        Ok(quote! {
            impl #state_enum_ident {
                pub fn accept(&self) -> Option<#lexer_name> {
                    match self {
                        #(#accept_arms)*
                    }
                }
            }
        })
    }

    pub fn gen_tokens(&self) -> Result<TokenStream> {
        let state_enum = self.state_enum();
        let start_impl = self.start_impl()?;
        let on_impl = self.on_impl()?;
        let accept_impl = self.accept_impl()?;
        Ok(quote! {
            #state_enum

            #start_impl

            #on_impl

            #accept_impl
        })
    }
}
