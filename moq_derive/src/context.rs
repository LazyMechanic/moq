use crate::utils;

use crate::utils::GenericsExt;
use syn::{Generics, Ident, ItemTrait};

#[derive(Debug)]
pub struct Context {
    pub trait_def: ItemTrait,

    pub mock_ident: Ident,
    pub mock_generics: Generics,

    pub action_collection_ident: Ident,
}

impl Context {
    pub fn from_ast(trait_def: ItemTrait) -> Result<Self, syn::Error> {
        let mock_ident = utils::format_mock_ident(&trait_def)?;
        let mock_generics = trait_def.generics.clone().delifetimified().staticized();
        let action_collection_ident = utils::format_action_collection_ident(&mock_ident);

        Ok(Self {
            trait_def,
            mock_ident,
            mock_generics,
            action_collection_ident,
        })
    }
}
