use proc_macro2::TokenStream;
use syn::{Result, Variant};
use quote::quote;

mod attr;
pub mod dfa;
pub mod iter;

pub fn gen_lexer<'a>(lexer_ident: &syn::Ident, enum_variants: impl Iterator<Item = &'a Variant>) -> Result<TokenStream> {
    let dfa_builder = dfa::LexerDFABuilder::new(lexer_ident.clone(), enum_variants)?;
    let lexer_dfa = dfa_builder.build();
    let dfa_tokens = lexer_dfa.gen_tokens()?;
    let iter = iter::LexerIter::new(lexer_ident.clone(), lexer_dfa.state_enum_ident.clone());
    let iter_ident = &iter.iter_ident;
    let iter_tokens = iter.gen_tokens();
    Ok(quote! {
        impl naivc_lexer::Lexer for #lexer_ident {
            fn tokenize<I>(
                chars: I,
            ) -> impl Iterator<Item = Result<naivc_lexer::Token<Self>, naivc_lexer::UmatchedError>>
            where I: Iterator<Item = char> {
                #iter_ident::new(chars)
            }
        }

        #dfa_tokens

        #iter_tokens
    })
}