use syn::{parse::Parser, parse_quote, punctuated::Punctuated, Generics, Ident, ItemTrait, Token};

use crate::{
    attribute::MoqAttribute,
    utils,
    utils::{Delifetimifing, Staticizing},
    validate::validate,
};

#[derive(Debug)]
pub struct Context {
    #[allow(dead_code)]
    pub attrs: Vec<MoqAttribute>,

    pub trait_def: ItemTrait,

    pub mock_ident: Ident,
    pub mock_generics: Generics,

    pub action_collection_ident: Ident,
}

impl Context {
    pub fn from_ast(
        args: proc_macro2::TokenStream,
        input: proc_macro2::TokenStream,
    ) -> Result<Self, syn::Error> {
        let attrs = Punctuated::<MoqAttribute, Token![,]>::parse_terminated
            .parse2(args)?
            .into_iter()
            .collect::<Vec<_>>();
        let mut trait_def = syn::parse2::<ItemTrait>(input)?;
        for attr in &attrs {
            match attr {
                MoqAttribute::Rename(_) => trait_def.attrs.push(parse_quote! { #[moq(#attr)] }),
                _ => { /* do nothing */ }
            }
        }

        validate(&trait_def)?;

        let mock_ident = utils::format_mock_ident(&trait_def)?;
        let mock_generics = trait_def.generics.clone().delifetimified().staticized();
        let action_collection_ident = utils::format_action_collection_ident(&mock_ident);

        Ok(Self {
            attrs,
            trait_def,
            mock_ident,
            mock_generics,
            action_collection_ident,
        })
    }
}
