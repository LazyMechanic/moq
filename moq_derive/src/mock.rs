use itertools::Itertools;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse_quote, punctuated::Punctuated, spanned::Spanned, Attribute, Block, Expr, ExprLit, FnArg,
    GenericParam, Generics, Ident, ImplItem, ImplItemConst, ImplItemFn, ImplItemType, ItemImpl,
    ItemStruct, Lit, Pat, Path, Token, TraitItem, TraitItemConst, TraitItemFn, TraitItemType, Type,
    Visibility, WhereClause,
};

use crate::{
    attribute::{
        AttributePresent, DefaultAttribute, DefaultAttributeValue, DefaultWithAttribute,
        MoqAttribute, Symboled,
    },
    context::Context,
    utils,
    utils::{Delifetimifing, Demoqifing, Deselfifing, Merging, Moqifing, Staticizing},
};

#[derive(Debug)]
pub struct Mock {
    self_ident: Ident,
    self_impl_items: Vec<ImplItem>,
    self_generics: Generics,
    action_collection_ident: Ident,
    trait_vis: Visibility,
    trait_attrs: Vec<Attribute>,
    trait_unsafety: Option<Token![unsafe]>,
    trait_ident: Ident,
    trait_generics: Generics,
    trait_impl_items: Vec<ImplItem>,
    phantom: Option<(Ident, Type)>,
}

impl Mock {
    pub fn from_ast(cx: &Context) -> Result<Self, syn::Error> {
        let self_ident = cx.mock_ident.clone();
        let self_impl_items = cx
            .trait_def
            .items
            .iter()
            .filter_map(|item| filter_map_exp_func(cx, item))
            .collect::<Result<Vec<_>, _>>()?;
        let self_generics = cx.mock_generics.clone();

        let action_collection_ident = cx.action_collection_ident.clone();

        let trait_vis = cx.trait_def.vis.clone();
        let trait_attrs = cx.trait_def.attrs.demoqified_iter().cloned().collect();
        let trait_unsafety = cx.trait_def.unsafety;
        let trait_ident = cx.trait_def.ident.clone();
        let trait_generics = cx.trait_def.generics.clone();

        let trait_impl_items = cx
            .trait_def
            .items
            .iter()
            .filter_map(|item| filter_map_trait_item(cx, item).transpose())
            .map_ok(|item| item.deselfified(cx))
            .collect::<Result<Vec<_>, _>>()?;

        let phantom = {
            let generics_ty = trait_generics
                .type_params()
                .map(|p| &p.ident)
                .collect::<Vec<_>>();
            if generics_ty.is_empty() {
                None
            } else {
                Some((
                    parse_quote! { _phantom },
                    parse_quote! { ::std::marker::PhantomData<fn() -> (#(#generics_ty,)*)> },
                ))
            }
        };

        Ok(Self {
            self_ident,
            self_impl_items,
            self_generics,
            action_collection_ident,
            trait_vis,
            trait_attrs,
            trait_unsafety,
            trait_ident,
            trait_generics,
            trait_impl_items,
            phantom,
        })
    }
}

impl ToTokens for Mock {
    fn to_tokens(&self, dst: &mut TokenStream) {
        let self_ident = &self.self_ident;
        let self_impl_items = &self.self_impl_items;
        let (_self_impl_generics, self_ty_generics, self_where_clause) =
            self.self_generics.split_for_impl();

        let action_collection_ident = &self.action_collection_ident;

        let trait_vis = &self.trait_vis;
        let trait_attrs = &self.trait_attrs;
        let trait_unsafety = &self.trait_unsafety;
        let trait_ident = &self.trait_ident;
        let (trait_impl_generics, trait_ty_generics, trait_where_clause) =
            self.trait_generics.split_for_impl();
        let trait_impl_items = &self.trait_impl_items;

        let (phantom_def, phantom_init) = match &self.phantom {
            None => (None, None),
            Some((ident, ty)) => (
                Some(quote! { #ident: #ty, }),
                Some(quote! { #ident: ::std::default::Default::default(), }),
            ),
        };

        let def: ItemStruct = parse_quote! {
            #trait_vis struct #self_ident #self_ty_generics #self_where_clause {
                actions: #action_collection_ident,
                #phantom_def
            }
        };

        let self_impl: ItemImpl = parse_quote! {
            impl #trait_impl_generics #self_ident #self_ty_generics #self_where_clause {
                pub fn new() -> Self {
                    Self {
                        actions: ::std::default::Default::default(),
                        #phantom_init
                    }
                }

                #(#self_impl_items)*
            }
        };

        let trait_impl: ItemImpl = parse_quote! {
            #(#trait_attrs)*
            #trait_unsafety impl #trait_impl_generics #trait_ident #trait_ty_generics for #self_ident #self_ty_generics #trait_where_clause {
                #(#trait_impl_items)*
            }
        };

        def.to_tokens(dst);
        self_impl.to_tokens(dst);
        trait_impl.to_tokens(dst);
    }
}

fn filter_map_exp_func(
    cx: &Context,
    trait_item: &TraitItem,
) -> Option<Result<ImplItem, syn::Error>> {
    match trait_item {
        TraitItem::Fn(item) => match exp_func(cx, item) {
            Ok(ok) => Some(Ok(ImplItem::Fn(ok))),
            Err(err) => Some(Err(err)),
        },
        _ => None,
    }
}

fn exp_func(cx: &Context, trait_func: &TraitItemFn) -> Result<ImplItemFn, syn::Error> {
    let action_path: Path = {
        let action_generics = {
            let trait_gen = cx.trait_def.generics.clone().delifetimified();
            let func_gen = trait_func.sig.generics.clone().delifetimified();

            trait_gen.merged(func_gen).staticized()
        };

        let action_ident = utils::format_action_ident(&cx.mock_ident, &trait_func.sig.ident);

        if action_generics.params.is_empty() {
            parse_quote! { #action_ident }
        } else {
            let (_action_impl_generics, action_ty_generics, _action_where_clause) =
                action_generics.split_for_impl();
            parse_quote! { #action_ident::#action_ty_generics }
        }
    };
    let exp_func_ident = format_ident!("expect_{}", trait_func.sig.ident);
    let exp_func_generics = {
        let exp_func_trait_bound = utils::make_exp_func_trait_bound(cx, trait_func)?;
        let exp_func_generics = {
            let params: Punctuated<GenericParam, Token![,]> = parse_quote! { __MoqFunc };
            let where_clause: WhereClause = parse_quote! { where __MoqFunc: #exp_func_trait_bound };
            Generics {
                lt_token: Some(<Token![<]>::default()),
                params,
                gt_token: Some(<Token![>]>::default()),
                where_clause: Some(where_clause),
            }
        };
        let func_gen = trait_func
            .sig
            .generics
            .clone()
            .delifetimified()
            .staticized();

        func_gen.merged(exp_func_generics)
    };
    let (_exp_func_impl_generics, exp_func_ty_generics, exp_func_where_clause) =
        exp_func_generics.split_for_impl();

    let item = parse_quote! {
        pub fn #exp_func_ident #exp_func_ty_generics (mut self, f: __MoqFunc) -> Self
        #exp_func_where_clause
        {
            let act = #action_path::new(f);
            self.actions.add(act);
            self
        }
    };

    Ok(item)
}

fn filter_map_trait_item(cx: &Context, item: &TraitItem) -> Result<Option<ImplItem>, syn::Error> {
    match item {
        TraitItem::Fn(item) => Ok(Some(ImplItem::Fn(trait_func(cx, item)?))),
        TraitItem::Const(item) => Ok(Some(ImplItem::Const(trait_const(item)?))),
        TraitItem::Type(item) => Ok(Some(ImplItem::Type(trait_type(item)?))),
        _ => Ok(None),
    }
}

fn trait_const(trait_cst: &TraitItemConst) -> Result<ImplItemConst, syn::Error> {
    let moq_attrs_iter = trait_cst.attrs.moqified_iter();
    let other_attrs = trait_cst.attrs.demoqified_iter().cloned().collect();

    let mut attr_present = AttributePresent::default();
    let mut expr = None;
    for attr in moq_attrs_iter {
        let nested_list =
            attr.parse_args_with(Punctuated::<MoqAttribute, Token![,]>::parse_terminated)?;
        for nested in nested_list {
            match nested {
                // #[moq(default = "123")]
                MoqAttribute::Default(attr) => {
                    attr_present.check_and_hit(&attr)?;
                    attr_present.check_conflict(
                        &DefaultWithAttribute::symbol(),
                        &DefaultAttribute::symbol(),
                        attr.span(),
                    )?;

                    match attr.value() {
                        DefaultAttributeValue::Expr(attr_expr) => {
                            expr = Some(parse_quote! { #attr_expr });
                        }
                        _ => return Err(attr.unsupported_error()),
                    }
                }
                // #[moq(default_with = "::path::to::func")]
                MoqAttribute::DefaultWith(attr) => {
                    attr_present.check_and_hit(&attr)?;
                    attr_present.check_conflict(
                        &DefaultAttribute::symbol(),
                        &DefaultWithAttribute::symbol(),
                        attr.span(),
                    )?;

                    let attr_path = attr.value();
                    expr = Some(parse_quote! { #attr_path() })
                }
                other => return Err(other.unsupported_error()),
            }
        }
    }

    let cst = ImplItemConst {
        attrs: other_attrs,
        vis: Visibility::Inherited,
        defaultness: None,
        const_token: Default::default(),
        ident: trait_cst.ident.clone(),
        generics: Default::default(),
        colon_token: Default::default(),
        ty: trait_cst.ty.clone(),
        eq_token: Default::default(),
        expr: expr
            .or_else(|| trait_cst.default.as_ref()
            .map(|(_, expr)| expr.clone()))
            .ok_or_else(||
                syn::Error::new_spanned(
                    trait_cst,
                    "missing value, use `#[moq(default = ...)]`, `#[moq(default_with = ...)]` or default"
                )
            )?,
        semi_token: Default::default(),
    };
    Ok(cst)
}

fn trait_type(trait_ty: &TraitItemType) -> Result<ImplItemType, syn::Error> {
    let moq_attrs_iter = trait_ty.attrs.moqified_iter();
    let other_attrs = trait_ty.attrs.demoqified_iter().cloned().collect();

    let mut attr_present = AttributePresent::default();
    let mut ty = None;
    for attr in moq_attrs_iter {
        let nested_list =
            attr.parse_args_with(Punctuated::<MoqAttribute, Token![,]>::parse_terminated)?;
        for nested in nested_list {
            match nested {
                MoqAttribute::Default(attr) => {
                    attr_present.check_and_hit(&attr)?;
                    match attr.value() {
                        DefaultAttributeValue::Expr(Expr::Lit(ExprLit {
                            lit: Lit::Str(lit),
                            ..
                        })) => {
                            ty = Some(lit.parse()?);
                        }
                        _ => {
                            return Err(syn::Error::new(
                                attr.span(),
                                "expected value as string literal with type",
                            ))
                        }
                    }
                }
                other => return Err(other.unsupported_error()),
            }
        }
    }

    let ty = ImplItemType {
        attrs: other_attrs,
        vis: Visibility::Inherited,
        defaultness: None,
        type_token: Default::default(),
        ident: trait_ty.ident.clone(),
        generics: Default::default(),
        eq_token: Default::default(),
        ty: ty
            .or_else(||
                trait_ty
                    .default
                    .as_ref()
                    .map(|(_, expr)| expr.clone())
            )
            .ok_or_else(||
                syn::Error::new_spanned(
                    trait_ty,
                    "missing type, use `#[moq(default = ...)]`, `#[moq(default_with = ...)]` or default"
                )
            )?,
        semi_token: Default::default(),
    };
    Ok(ty)
}

fn trait_func(cx: &Context, func: &TraitItemFn) -> Result<ImplItemFn, syn::Error> {
    let func_moq_attrs_iter = func.attrs.moqified_iter();
    let func_other_attrs_iter = func.attrs.demoqified_iter();
    let func_ident = &func.sig.ident;
    let func_constness = func.sig.constness;
    let func_asyncness = func.sig.asyncness;
    let func_unsafety = func.sig.unsafety;

    let (_func_impl_generics, func_ty_generics, func_where_clause) =
        func.sig.generics.split_for_impl();
    let func_args = &func.sig.inputs;
    let func_ret = &func.sig.output;
    let func_block = match &func.default {
        None => trait_func_block(cx, func)?,
        Some(def_block) => {
            let mut attr_present = AttributePresent::default();
            let mut block = None;
            for attr in func_moq_attrs_iter {
                let nested_list =
                    attr.parse_args_with(Punctuated::<MoqAttribute, Token![,]>::parse_terminated)?;
                for nested in nested_list {
                    match nested {
                        MoqAttribute::Default(attr) => {
                            attr_present.check_and_hit(&attr)?;
                            match attr.value() {
                                DefaultAttributeValue::Flag => block = Some(def_block.clone()),
                                _ => return Err(syn::Error::new(attr.span(), "unexpected value")),
                            }
                        }
                        MoqAttribute::Output(_) => { /* do nothing */ }
                        other => return Err(other.unsupported_error()),
                    }
                }
            }

            match block {
                None => trait_func_block(cx, func)?,
                Some(b) => b,
            }
        }
    };

    let method = parse_quote! {
        #(#func_other_attrs_iter)*
        #func_constness #func_asyncness #func_unsafety fn #func_ident #func_ty_generics (#func_args) #func_ret
        #func_where_clause
        {
            #func_block
        }
    };

    Ok(method)
}

fn trait_func_block(cx: &Context, func: &TraitItemFn) -> Result<Block, syn::Error> {
    let action_ident = utils::format_action_ident(&cx.mock_ident, &func.sig.ident);
    let call_generics = {
        let trait_gen = cx.trait_def.generics.clone().delifetimified();
        let func_gen = func.sig.generics.clone().delifetimified();
        trait_gen.merged(func_gen)
    };
    let (_impl_generics, ty_generics, _where_clause) = call_generics.split_for_impl();
    let awaiting = if func.sig.asyncness.is_some() {
        Some(quote! { .await })
    } else {
        None
    };
    let args = func
        .sig
        .inputs
        .iter()
        .filter_map(|arg| match arg {
            FnArg::Receiver(_) => None,
            FnArg::Typed(arg) => Some(arg),
        })
        .map(|arg| match &*arg.pat {
            Pat::Ident(pat) => Ok(&pat.ident),
            x => Err(syn::Error::new(
                x.span(),
                "unsupported function argument pattern",
            )),
        })
        .collect::<Result<Vec<_>, _>>()?;

    let block = parse_quote! {
        {
            self.actions
                .next::<#action_ident #ty_generics>()
                .call(#(#args),*)
                #awaiting
        }
    };
    Ok(block)
}
