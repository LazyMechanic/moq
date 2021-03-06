use crate::{symbols, utils};

use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{
    parse_quote, Attribute, FnArg, Generics, Ident, ItemTrait, Lit, Meta, NestedMeta, Path,
    ReturnType, TraitItem, TraitItemConst, TraitItemMethod, TraitItemType, Type,
};

pub struct Context {
    /// Original trait definition
    pub trait_def_orig: ItemTrait,
    /// Trait definition after processing
    pub trait_def: ItemTrait,

    pub mock_ident: Ident,
    pub mock_generics: Generics,

    pub action_collection_ident: Ident,
}

impl Parse for Context {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut trait_def: ItemTrait = input.parse()?;
        let trait_def_orig = utils::demoqify(trait_def.clone());

        let mock_ident = utils::format_mock_ident(&trait_def.ident);
        let mock_generics = {
            let trait_gen = utils::delifetimify_generics(&trait_def.generics);
            utils::staticize(trait_gen)
        };
        let action_collection_ident = utils::format_action_collection_ident(&trait_def.ident);

        trait_def.items = process_trait_items(
            trait_def.items,
            &trait_def.ident,
            &trait_def.generics,
            &mock_ident,
            &mock_generics,
        )?;

        Ok(Self {
            trait_def_orig,
            trait_def,
            mock_ident,
            mock_generics,
            action_collection_ident,
        })
    }
}

fn process_trait_items<I>(
    items: I,
    trait_ident: &Ident,
    trait_generics: &Generics,
    mock_ident: &Ident,
    mock_generics: &Generics,
) -> Result<Vec<TraitItem>, syn::Error>
where
    I: IntoIterator<Item = TraitItem>,
{
    items
        .into_iter()
        .map(|item| match item {
            TraitItem::Method(item) => {
                let item = apply_moq_attr_method(item)?;
                let item =
                    deselfify_method(item, trait_ident, trait_generics, mock_ident, mock_generics)?;
                Ok(TraitItem::Method(item))
            }
            TraitItem::Const(item) => Ok(TraitItem::Const(apply_moq_attr_const(item)?)),
            TraitItem::Type(item) => Ok(TraitItem::Type(apply_moq_attr_type(item)?)),
            item => Err(syn::Error::new_spanned(item, "unsupported item")),
        })
        .collect::<Result<_, syn::Error>>()
}

fn apply_moq_attr_const(mut item: TraitItemConst) -> Result<TraitItemConst, syn::Error> {
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

fn apply_moq_attr_method(mut item: TraitItemMethod) -> Result<TraitItemMethod, syn::Error> {
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

fn deselfify_method(
    mut item: TraitItemMethod,
    trait_ident: &Ident,
    trait_generics: &Generics,
    mock_ident: &Ident,
    mock_generics: &Generics,
) -> Result<TraitItemMethod, syn::Error> {
    for inp in &mut item.sig.inputs {
        match inp {
            FnArg::Receiver(_) => {}
            FnArg::Typed(arg) => {
                utils::deselfify_type(
                    &mut *arg.ty,
                    trait_ident,
                    trait_generics,
                    mock_ident,
                    mock_generics,
                )?;
            }
        }
    }

    if let ReturnType::Type(_, ty) = &mut item.sig.output {
        utils::deselfify_type(
            &mut **ty,
            trait_ident,
            trait_generics,
            mock_ident,
            mock_generics,
        )?;
    }

    Ok(item)
}

fn apply_moq_attr_type(mut item: TraitItemType) -> Result<TraitItemType, syn::Error> {
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
