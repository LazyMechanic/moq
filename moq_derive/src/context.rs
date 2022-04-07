use crate::{symbols, utils};
use quote::{format_ident, quote};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    parse_quote, Attribute, FnArg, GenericParam, Generics, Ident, ImplItem, ImplItemConst,
    ImplItemMacro, ImplItemMethod, ImplItemType, ItemTrait, Lit, Meta, NestedMeta, Pat, Path,
    Token, TraitItem, TraitItemConst, TraitItemMacro, TraitItemMethod, TraitItemType, Visibility,
};

pub struct Context {
    /// Original trait definition
    pub trait_def: ItemTrait,

    /// Mock struct ident
    pub struct_ident: Ident,
    /// Mock struct generics. Same as trait, but without lifetimes
    pub struct_generics: Generics,

    /// Mock action enum ident
    pub action_ident: Ident,
    /// Mock action enum generics. Same as trait, but without lifetimes
    pub action_generics: Generics,
    /// Mock action kind enum ident
    pub action_kind_ident: Ident,

    /// Original trait attributes
    pub trait_attrs: Vec<Attribute>,
    /// Trait ident
    pub trait_ident: Ident,
    /// Trait visibility
    pub trait_vis: Visibility,
    /// Trait unsafe ident
    pub trait_unsafety: Option<Token![unsafe]>,
    /// Trait generics
    pub trait_generics: Generics,
    /// Prepared trait items
    pub trait_impl_items: Vec<ImplItem>,
}

impl Parse for Context {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let trait_def: ItemTrait = input.parse()?;
        let trait_def_orig = utils::clean_moq_attrs_trait_def(trait_def.clone());

        let struct_ident = format_ident!("{}Mock", trait_def.ident);
        let struct_generics = clean_generics(trait_def.generics.clone());

        let action_ident = format_ident!("__{}Action", struct_ident);
        let action_generics = clean_generics(trait_def.generics.clone());
        let action_kind_ident = format_ident!("{}Kind", action_ident);

        let trait_attrs = trait_def.attrs;
        let trait_vis = trait_def.vis;
        let trait_ident = trait_def.ident;
        let trait_unsafety = trait_def.unsafety;
        let trait_generics = trait_def.generics;
        let trait_impl_items =
            implement_trait_items(trait_def.items, &action_ident, &action_kind_ident)?;

        Ok(Self {
            trait_def: trait_def_orig,
            struct_ident,
            struct_generics,
            action_ident,
            action_generics,
            action_kind_ident,
            trait_attrs,
            trait_ident,
            trait_vis,
            trait_unsafety,
            trait_generics,
            trait_impl_items,
        })
    }
}

fn clean_generics(mut gen: Generics) -> Generics {
    gen.params = gen
        .params
        .into_iter()
        .filter_map(|p| match p {
            GenericParam::Type(mut p) => {
                p.bounds = Punctuated::default();
                Some(GenericParam::Type(p))
            }
            GenericParam::Const(p) => Some(GenericParam::Const(p)),
            GenericParam::Lifetime(_) => None,
        })
        .collect();
    gen
}

fn implement_trait_items<I>(
    items: I,
    action_ident: &Ident,
    action_kind_ident: &Ident,
) -> Result<Vec<ImplItem>, syn::Error>
where
    I: IntoIterator<Item = TraitItem>,
{
    items
        .into_iter()
        .map(|item| match item {
            TraitItem::Const(item) => Ok(ImplItem::Const(parse_const(item)?)),
            TraitItem::Method(item) => Ok(ImplItem::Method(parse_method(
                item,
                action_ident,
                action_kind_ident,
            )?)),
            TraitItem::Type(item) => Ok(ImplItem::Type(parse_type(item)?)),
            TraitItem::Macro(item) => Ok(ImplItem::Macro(parse_macro(item)?)),
            item => Err(syn::Error::new_spanned(item, "unsupported item")),
        })
        .collect::<Result<_, syn::Error>>()
}

fn parse_const(item: TraitItemConst) -> Result<ImplItemConst, syn::Error> {
    let item_span = item.span();
    let (moq_attrs, other_attrs): (Vec<Attribute>, Vec<Attribute>) = item
        .attrs
        .into_iter()
        .partition(|attr| attr.path == symbols::MOQ);
    let mut new_expr = item.default.map(|(_, expr)| expr);

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
                                x if x == symbols::VALUE => match meta {
                                    Meta::NameValue(meta) => {
                                        let val = meta.lit;
                                        new_expr = Some(parse_quote!(#val));
                                    }
                                    meta => {
                                        return Err(syn::Error::new_spanned(
                                            meta,
                                            "unsupported attribute",
                                        ))
                                    }
                                },
                                x if x == symbols::VALUE_WITH => match meta {
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
                                        new_expr = Some(parse_quote! { #fn_path() });
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

    let new_item = ImplItemConst {
        attrs: other_attrs,
        vis: Visibility::Inherited,
        defaultness: None,
        const_token: <Token![const]>::default(),
        ident: item.ident,
        colon_token: <Token![:]>::default(),
        ty: item.ty,
        eq_token: <Token![=]>::default(),
        expr: new_expr.ok_or_else(move || syn::Error::new(item_span, "missing const value"))?,
        semi_token: <Token![;]>::default(),
    };

    Ok(new_item)
}

fn parse_method(
    item: TraitItemMethod,
    action_ident: &Ident,
    action_kind_ident: &Ident,
) -> Result<ImplItemMethod, syn::Error> {
    let item_span = item.span();
    let (moq_attrs, other_attrs): (Vec<Attribute>, Vec<Attribute>) = item
        .attrs
        .into_iter()
        .partition(|attr| attr.path == symbols::MOQ);

    let method_ident = &item.sig.ident;
    let args = item
        .sig
        .inputs
        .iter()
        .filter_map(|arg| match arg {
            FnArg::Receiver(_self_arg) => None,
            FnArg::Typed(arg) => match &*arg.pat {
                Pat::Ident(arg) => Some(arg.ident.clone()),
                _ => None,
            },
        })
        .collect::<Vec<_>>();
    let awaiting = if item.sig.asyncness.is_some() {
        Some(quote! { .await })
    } else {
        None
    };

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

    let mut new_block = parse_quote!(
        {
            let idx = self.cur_action_idx.fetch_add(1, ::std::sync::atomic::Ordering::SeqCst);
            assert!(
                idx < self.actions.len(),
                "an attempt to perform an extra action",
            );

            match &self.actions[idx] {
                #action_ident::#method_ident { func } => {
                    func.call((#(#args,)*))#awaiting
                }
                other => {
                    panic!(
                        "expected {:?} action, but handled {:?}",
                        #action_kind_ident::#method_ident,
                        other.kind(),
                    );
                }
            }
        }
    );

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
                                x if x == symbols::USE_DEFAULT => match meta {
                                    Meta::Path(path) => {
                                        match &item.default {
                                            Some(def) => {new_block = def.clone()}
                                            None => return Err(syn::Error::new_spanned(path, format!("attribute '{}' cannot be used if there is no default impl of function", symbols::USE_DEFAULT))),
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

    let new_item = ImplItemMethod {
        attrs: other_attrs,
        vis: Visibility::Inherited,
        defaultness: None,
        sig: item.sig,
        block: new_block,
    };

    Ok(new_item)
}

fn parse_type(item: TraitItemType) -> Result<ImplItemType, syn::Error> {
    let item_span = item.span();
    let (moq_attrs, other_attrs): (Vec<Attribute>, Vec<Attribute>) = item
        .attrs
        .into_iter()
        .partition(|attr| attr.path == symbols::MOQ);
    let mut new_ty = item.default.map(|(_, ty)| ty);

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
                                x if x == symbols::VALUE => match meta {
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
                                        new_ty = Some(syn::parse_str(&val)?);
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

    let new_item = ImplItemType {
        attrs: other_attrs,
        vis: Visibility::Inherited,
        defaultness: None,
        type_token: <Token![type]>::default(),
        ident: item.ident,
        generics: item.generics,
        eq_token: <Token![=]>::default(),
        ty: new_ty.ok_or_else(move || syn::Error::new(item_span, "missing type"))?,
        semi_token: <Token![;]>::default(),
    };

    Ok(new_item)
}

fn parse_macro(item: TraitItemMacro) -> Result<ImplItemMacro, syn::Error> {
    let new_item = ImplItemMacro {
        attrs: item.attrs,
        mac: item.mac,
        semi_token: item.semi_token,
    };

    Ok(new_item)
}
