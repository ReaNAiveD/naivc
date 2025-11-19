mod lexer;

use syn::{DeriveInput, Error, parse_macro_input};

#[proc_macro_derive(Lexer, attributes(regex))]
pub fn lexer_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Implementation of the procedural macro would go here.
    let ast: DeriveInput = parse_macro_input!(input);
    match ast.data {
        syn::Data::Enum(ref data_enum) => {
            match lexer::gen_lexer(&ast.ident, data_enum.variants.iter()) {
                Ok(tokens) => tokens.into(),
                Err(e) => e.to_compile_error().into(),
            }
        }
        _ => {
            // Return a compile error with a span pointing to the type
            return Error::new_spanned(&ast.ident, "Lexer can only be derived for enums")
                .to_compile_error()
                .into();
        }
    }
}
