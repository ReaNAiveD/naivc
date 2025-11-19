use proc_macro2::TokenStream;
use quote::quote;
use syn::Ident;

pub struct LexerIter {
    pub lexer_ident: Ident,
    pub state_enum_ident: Ident,
    pub iter_ident: Ident,
}

impl LexerIter {
    pub fn new(lexer_ident: Ident, state_enum_ident: Ident) -> Self {
        let iter_ident = Ident::new(&format!("__{}__Iter", lexer_ident), lexer_ident.span());
        LexerIter {
            lexer_ident,
            state_enum_ident,
            iter_ident,
        }
    }

    pub fn gen_tokens(&self) -> TokenStream {
        let state_enum_ident = &self.state_enum_ident;
        let iter_ident = &self.iter_ident;
        let lexer_name = &self.lexer_ident;
        quote! {
            struct #iter_ident<I: Iterator<Item = char>> {
                chars: I,
                buffer: Vec<char>,
                next_index: usize,
            }

            impl<I: Iterator<Item = char>> #iter_ident<I> {
                fn new(chars: I) -> Self {
                    Self {
                        chars,
                        buffer: Vec::new(),
                        next_index: 0,
                    }
                }

                fn update_buffer(&mut self, mut unidentified: Vec<char>, buf_index: usize) {
                    if buf_index < self.buffer.len() {
                        unidentified.extend_from_slice(&self.buffer[buf_index..]);
                    }
                    self.buffer = unidentified;
                }

                fn finalize_token(
                    &mut self,
                    last_accept: Option<naivc_lexer::Token<#lexer_name>>,
                    unidentified: Vec<char>,
                    buf_index: usize,
                    start_index: usize,
                ) -> Option<Result<naivc_lexer::Token<#lexer_name>, naivc_lexer::UmatchedError>> {
                    if let Some(accept) = last_accept {
                        // Successfully matched a token
                        self.update_buffer(unidentified, buf_index);
                        Some(Ok(accept))
                    } else {
                        // No match found, return error
                        let error_content: String = unidentified.into_iter().collect();
                        let error_len = error_content.len();
                        self.next_index += error_len;
                        self.update_buffer(Vec::new(), buf_index);
                        Some(Err(naivc_lexer::UmatchedError {
                            content: error_content,
                            start_index,
                            end_index: self.next_index,
                        }))
                    }
                }

                fn finalize_end_of_input(
                    &mut self,
                    last_accept: Option<naivc_lexer::Token<#lexer_name>>,
                    unidentified: Vec<char>,
                    start_index: usize,
                ) -> Option<Result<naivc_lexer::Token<#lexer_name>, naivc_lexer::UmatchedError>> {
                    if let Some(accept) = last_accept {
                        // Return the last accepted token
                        self.buffer = unidentified;
                        Some(Ok(accept))
                    } else if !unidentified.is_empty() {
                        // Return unmatched content as error
                        let error_content: String = unidentified.into_iter().collect();
                        let error_len = error_content.len();
                        self.next_index += error_len;
                        self.buffer.clear();
                        Some(Err(naivc_lexer::UmatchedError {
                            content: error_content,
                            start_index,
                            end_index: self.next_index,
                        }))
                    } else {
                        // Nothing left to process
                        None
                    }
                }
            }

            impl<'a, I: Iterator<Item = char>> Iterator for #iter_ident<I> {
                type Item = Result<naivc_lexer::Token<#lexer_name>, naivc_lexer::UmatchedError>;

                fn next(&mut self) -> Option<Self::Item> {
                    // Lexer logic would go here
                    let start_index = self.next_index;
                    let mut current_state = #state_enum_ident::start();
                    let mut last_accept: Option<naivc_lexer::Token<#lexer_name>> = None;
                    let mut buf_index = 0;
                    let mut unidentified = Vec::new();

                    loop {
                        // Get next character from buffer or input stream
                        let next_char = if buf_index < self.buffer.len() {
                            let ch = self.buffer[buf_index];
                            buf_index += 1;
                            Some(ch)
                        } else {
                            self.chars.next()
                        };

                        match next_char {
                            Some(ch) => {
                                unidentified.push(ch);

                                if let Some(next_state) = current_state.on(ch) {
                                    // Continue DFA traversal
                                    current_state = next_state;
                                    if let Some(token_type) = next_state.accept() {
                                        self.next_index += unidentified.len();
                                        let token = if let Some(mut token) = last_accept {
                                            token.lexeme.extend(unidentified.drain(..));
                                            token.end_index = self.next_index;
                                            token
                                        } else {
                                            naivc_lexer::Token {
                                                kind: token_type,
                                                lexeme: unidentified.drain(..).collect(),
                                                start_index,
                                                end_index: self.next_index,
                                            }
                                        };
                                        last_accept = Some(token);
                                    }
                                } else {
                                    // DFA cannot continue, finalize current token or error
                                    return self.finalize_token(
                                        last_accept,
                                        unidentified,
                                        buf_index,
                                        start_index,
                                    );
                                }
                            }
                            None => {
                                // End of input
                                return self.finalize_end_of_input(
                                    last_accept,
                                    unidentified,
                                    start_index,
                                );
                            }
                        }
                    }
                }
            }
        }
    }
}