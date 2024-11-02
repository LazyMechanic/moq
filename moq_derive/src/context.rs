use crate::utils;
use std::collections::HashMap;

use crate::action::Action;
use crate::utils::{AttributesExt, GenericsExt};
use syn::parse::Parse;
use syn::{Attribute, Generics, Ident, ItemTrait, Token, TraitItem, Visibility};

#[derive(Debug)]
pub struct Context {
    pub trait_ident: Ident,
    pub trait_generics: Generics,
    pub trait_moq_attrs: Vec<Attribute>,
    pub trait_demoqified_attrs: Vec<Attribute>,
    pub trait_vis: Visibility,
    pub trait_unsafety: Option<Token![unsafe]>,
    pub trait_items: Vec<TraitItem>,

    pub mock_ident: Ident,
    pub mock_generics: Generics,
}

impl Context {
    pub fn from_ast(trait_def: ItemTrait) -> Self {
        let trait_ident = trait_def.ident;
        let trait_generics = trait_def.generics;
        let trait_moq_attrs = trait_def.attrs.moqified_iter().cloned().collect();
        let trait_demoqified_attrs = trait_def.attrs.demoqified_iter().cloned().collect();
        let trait_vis = trait_def.vis;
        let trait_unsafety = trait_def.unsafety;
        let trait_items = trait_def.items;
        let mock_ident = utils::format_mock_ident(&trait_ident);
        let mock_generics = trait_generics.clone().delifetimified().staticized();

        Self {
            trait_ident,
            trait_generics,
            trait_moq_attrs,
            trait_demoqified_attrs,
            trait_vis,
            trait_unsafety,
            trait_items,
            mock_ident,
            mock_generics,
        }
    }
}
