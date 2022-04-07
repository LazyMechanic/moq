use crate::symbols;

use quote::format_ident;
use syn::punctuated::Punctuated;
use syn::{ItemTrait, TraitItem, Type};

pub fn prepare_type(ty: Type) -> Type {
    match ty {
        Type::Reference(mut ty) => {
            if let Some(lt) = &mut ty.lifetime {
                lt.ident = format_ident!("__moq_{}", lt.ident);
            }
            Type::Reference(ty)
        }
        Type::Tuple(mut ty) => {
            let mut new_elems = Punctuated::new();
            for ty in ty.elems {
                new_elems.push(prepare_type(ty));
            }
            ty.elems = new_elems;
            Type::Tuple(ty)
        }
        _ => ty,
    }
}

pub fn clean_moq_attrs_trait_def(mut trait_def: ItemTrait) -> ItemTrait {
    trait_def.items = trait_def
        .items
        .into_iter()
        .map(|item| match item {
            TraitItem::Const(mut item) => {
                item.attrs = item
                    .attrs
                    .into_iter()
                    .filter(|attr| attr.path != symbols::MOQ)
                    .collect();
                TraitItem::Const(item)
            }
            TraitItem::Method(mut item) => {
                item.attrs = item
                    .attrs
                    .into_iter()
                    .filter(|attr| attr.path != symbols::MOQ)
                    .collect();
                TraitItem::Method(item)
            }
            TraitItem::Type(mut item) => {
                item.attrs = item
                    .attrs
                    .into_iter()
                    .filter(|attr| attr.path != symbols::MOQ)
                    .collect();
                TraitItem::Type(item)
            }
            TraitItem::Macro(mut item) => {
                item.attrs = item
                    .attrs
                    .into_iter()
                    .filter(|attr| attr.path != symbols::MOQ)
                    .collect();
                TraitItem::Macro(item)
            }
            item => item,
        })
        .collect();

    trait_def
}
