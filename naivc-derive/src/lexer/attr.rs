use syn::{Attribute, Ident, Result, parse::Parse};

#[derive(Debug, Clone)]
pub struct LexerAttribute {
    pub token_type_name: Ident,
    pub token_raw_attr: Vec<Attribute>,
    pub regex: LexerRegex,
}

impl LexerAttribute {
    pub fn new(
        token_type_name: Ident,
        token_raw_attr: Vec<Attribute>,
    ) -> Result<Option<Self>> {
        if let Some(regex_attr) = token_raw_attr
            .iter()
            .find(|attr| attr.path().is_ident("regex"))
        {
            let list_meta = regex_attr.meta.require_list()?;
            let enum_regex = list_meta.parse_args::<LexerRegex>()?;
            Ok(Some(LexerAttribute {
                token_type_name,
                token_raw_attr,
                regex: enum_regex,
            }))
        } else {
            // At present, we allow variants without regex attributes, but we may change this later.
            Ok(None)
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct LexerRegex {
    pub regex: String,
}

impl Parse for LexerRegex {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let regex_lit: syn::LitStr = input.parse()?;
        Ok(LexerRegex {
            regex: regex_lit.value(),
        })
    }
}
