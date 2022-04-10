use crate::{symbols, utils};

use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{
    parse_quote, Attribute, FnArg, ItemTrait, Lit, Meta, NestedMeta, Path, TraitItem,
    TraitItemConst, TraitItemMethod, TraitItemType, Type,
};

pub struct Context {
    /// Original trait definition
    pub trait_def_orig: ItemTrait,
    /// Trait definition after processing
    pub trait_def: ItemTrait,
}

impl Parse for Context {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut trait_def: ItemTrait = input.parse()?;
        let trait_def_orig = utils::demoq_attr_trait_def(trait_def.clone());

        trait_def.items = process_trait_items(trait_def.items)?;

        Ok(Self {
            trait_def_orig,
            trait_def,
        })
    }
}

fn process_trait_items<I>(items: I) -> Result<Vec<TraitItem>, syn::Error>
where
    I: IntoIterator<Item = TraitItem>,
{
    items
        .into_iter()
        .map(|item| match item {
            TraitItem::Const(item) => Ok(TraitItem::Const(process_const(item)?)),
            TraitItem::Method(item) => Ok(TraitItem::Method(process_method(item)?)),
            TraitItem::Type(item) => Ok(TraitItem::Type(process_type(item)?)),
            item => Err(syn::Error::new_spanned(item, "unsupported item")),
        })
        .collect::<Result<_, syn::Error>>()
}

fn process_const(mut item: TraitItemConst) -> Result<TraitItemConst, syn::Error> {
    let (moq_attrs, other_attrs): (Vec<Attribute>, Vec<Attribute>) = item
        .attrs
        .into_iter()
        .partition(|attr| attr.path == symbols::MOQ);
    item.attrs = other_attrs;

    for attr in moq_attrs {
        match attr.parse_meta()? {
            Meta::List(meta) => {
                for meta in meta.nested {
                    match meta {
                        NestedMeta::Meta(meta) => {
                            let meta_ident = meta.path().get_ident().ok_or_else(|| {
                                syn::Error::new_spanned(&meta, "missing identifier")
                            })?;

                            match meta_ident {
                                x if x == symbols::DEFAULT => match meta {
                                    Meta::NameValue(meta) => {
                                        let val = meta.lit;
                                        let expr = parse_quote! { #val };
                                        item.default = Some((Default::default(), expr));
                                    }
                                    meta => {
                                        return Err(syn::Error::new_spanned(
                                            meta,
                                            "unsupported attribute",
                                        ))
                                    }
                                },
                                x if x == symbols::DEFAULT_WITH => match meta {
                                    Meta::NameValue(meta) => {
                                        let val = match meta.lit {
                                            Lit::Str(s) => s.value(),
                                            other => {
                                                return Err(syn::Error::new_spanned(
                                                    other,
                                                    "unsupported value type",
                                                ))
                                            }
                                        };
                                        let fn_path: Path = syn::parse_str(&val)?;
                                        let expr = parse_quote! { #fn_path() };
                                        item.default = Some((Default::default(), expr));
                                    }
                                    meta => {
                                        return Err(syn::Error::new_spanned(
                                            meta,
                                            "unsupported attribute",
                                        ))
                                    }
                                },
                                other => {
                                    return Err(syn::Error::new_spanned(
                                        other,
                                        "unsupported attribute",
                                    ))
                                }
                            }
                        }
                        meta => return Err(syn::Error::new_spanned(meta, "unsupported attribute")),
                    }
                }
            }
            meta => return Err(syn::Error::new_spanned(meta, "unsupported attribute")),
        }
    }

    Ok(item)
}

fn process_method(mut item: TraitItemMethod) -> Result<TraitItemMethod, syn::Error> {
    let item_span = item.span();
    let (moq_attrs, other_attrs): (Vec<Attribute>, Vec<Attribute>) = item
        .attrs
        .into_iter()
        .partition(|attr| attr.path == symbols::MOQ);
    item.attrs = other_attrs;

    // TODO: add supporting static func
    let is_static_func = !item
        .sig
        .inputs
        .iter()
        .any(|item| matches!(item, &FnArg::Receiver(_)));

    if is_static_func {
        return Err(syn::Error::new(
            item_span,
            "static functions are not supported yet",
        ));
    }

    let def_block = item.default.take();

    for attr in moq_attrs {
        match attr.parse_meta()? {
            Meta::List(meta) => {
                for meta in meta.nested {
                    match meta {
                        NestedMeta::Meta(meta) => {
                            let meta_ident = meta.path().get_ident().ok_or_else(|| {
                                syn::Error::new_spanned(&meta, "missing identifier")
                            })?;

                            match meta_ident {
                                x if x == symbols::DEFAULT => match meta {
                                    Meta::Path(path) => {
                                        match &def_block {
                                            Some(def_block) => item.default = Some(def_block.clone()),
                                            None => return Err(syn::Error::new_spanned(path, format!("attribute '{}' cannot be used if there is no default impl of function", symbols::DEFAULT))),
                                        }
                                    }
                                    meta => {
                                        return Err(syn::Error::new_spanned(
                                            meta,
                                            "unsupported attribute",
                                        ))
                                    }
                                },
                                other => {
                                    return Err(syn::Error::new_spanned(
                                        other,
                                        "unsupported attribute",
                                    ))
                                }
                            }
                        }
                        meta => return Err(syn::Error::new_spanned(meta, "unsupported attribute")),
                    }
                }
            }
            meta => return Err(syn::Error::new_spanned(meta, "unsupported attribute")),
        }
    }

    Ok(item)
}

fn process_type(mut item: TraitItemType) -> Result<TraitItemType, syn::Error> {
    let (moq_attrs, other_attrs): (Vec<Attribute>, Vec<Attribute>) = item
        .attrs
        .into_iter()
        .partition(|attr| attr.path == symbols::MOQ);
    item.attrs = other_attrs;

    for attr in moq_attrs {
        match attr.parse_meta()? {
            Meta::List(meta) => {
                for meta in meta.nested {
                    match meta {
                        NestedMeta::Meta(meta) => {
                            let meta_ident = meta.path().get_ident().ok_or_else(|| {
                                syn::Error::new_spanned(&meta, "missing identifier")
                            })?;

                            match meta_ident {
                                x if x == symbols::DEFAULT => match meta {
                                    Meta::NameValue(meta) => {
                                        let val = match meta.lit {
                                            Lit::Str(s) => s.value(),
                                            other => {
                                                return Err(syn::Error::new_spanned(
                                                    other,
                                                    "unsupported value type",
                                                ))
                                            }
                                        };
                                        let ty: Type = syn::parse_str(&val)?;
                                        item.default = Some((Default::default(), ty));
                                    }
                                    meta => {
                                        return Err(syn::Error::new_spanned(
                                            meta,
                                            "unsupported attribute",
                                        ))
                                    }
                                },
                                other => {
                                    return Err(syn::Error::new_spanned(
                                        other,
                                        "unsupported attribute",
                                    ))
                                }
                            }
                        }
                        meta => return Err(syn::Error::new_spanned(meta, "unsupported attribute")),
                    }
                }
            }
            meta => return Err(syn::Error::new_spanned(meta, "unsupported attribute")),
        }
    }

    Ok(item)
}
