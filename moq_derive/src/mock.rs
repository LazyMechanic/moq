use crate::context::Context;
use crate::utils;

use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    parse_quote, Attribute, FnArg, GenericParam, Generics, Ident, ImplItem, ImplItemConst,
    ImplItemFn, ImplItemType, ItemImpl, ItemStruct, Pat, Path, Token, TraitItem, TraitItemConst,
    TraitItemFn, TraitItemType, Type, Visibility, WhereClause,
};

pub struct Mock {
    self_ident: Ident,
    self_impl_items: Vec<ImplItem>,
    self_generics: Generics,
    actions_ident: Ident,
    trait_vis: Visibility,
    trait_attrs: Vec<Attribute>,
    trait_unsafety: Option<Token![unsafe]>,
    trait_ident: Ident,
    trait_generics: Generics,
    trait_impl_items: Vec<ImplItem>,
    phantom: Option<(Ident, Type)>,
}

impl Mock {
    pub fn parse(cx: &Context) -> Result<Self, syn::Error> {
        let self_ident = utils::format_mock_ident(&cx.trait_def.ident);
        let self_impl_items = cx
            .trait_def
            .items
            .iter()
            .filter_map(|item| filter_map_exp_func(cx, item))
            .collect::<Result<Vec<_>, _>>()?;
        let self_generics = {
            let trait_gen = utils::delifetimify_generics(&cx.trait_def.generics);
            utils::staticize(trait_gen)
        };

        let actions_ident = utils::format_action_collection_ident(&cx.trait_def.ident);

        let trait_vis = cx.trait_def.vis.clone();
        let trait_attrs = cx.trait_def.attrs.clone();
        let trait_unsafety = cx.trait_def.unsafety;
        let trait_ident = cx.trait_def.ident.clone();
        let trait_generics = cx.trait_def.generics.clone();
        let trait_impl_items = cx
            .trait_def
            .items
            .iter()
            .filter_map(|item| filter_map_trait_item(cx, item))
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
            actions_ident,
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

        let actions_ident = &self.actions_ident;

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
                actions: #actions_ident,
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

fn filter_map_exp_func(cx: &Context, item: &TraitItem) -> Option<Result<ImplItem, syn::Error>> {
    match item {
        TraitItem::Fn(item) => match exp_func(cx, item) {
            Ok(ok) => Some(Ok(ImplItem::Fn(ok))),
            Err(err) => Some(Err(err)),
        },
        _ => None,
    }
}

fn exp_func(cx: &Context, func: &TraitItemFn) -> Result<ImplItemFn, syn::Error> {
    let action_path: Path = {
        let action_generics = {
            let trait_gen = utils::delifetimify_generics(&cx.trait_def.generics);
            let func_gen = utils::delifetimify_generics(&func.sig.generics);
            let merged = utils::merge_generics(trait_gen, func_gen);
            utils::staticize(merged)
        };

        let action_ident = utils::format_action_ident(&cx.trait_def.ident, &func.sig.ident);

        if action_generics.params.is_empty() {
            parse_quote! { #action_ident }
        } else {
            let (_action_impl_generics, action_ty_generics, _action_where_clause) =
                action_generics.split_for_impl();
            parse_quote! { #action_ident::#action_ty_generics }
        }
    };
    let exp_func_ident = format_ident!("expect_{}", func.sig.ident);
    let exp_func_generics = {
        let exp_func_trait_bound = utils::make_exp_func_trait_bound(&func.sig)?;
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
        let func_gen = utils::delifetimify_generics(&func.sig.generics);
        let func_gen = utils::staticize(func_gen);
        utils::merge_generics(func_gen, exp_func_generics)
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

fn filter_map_trait_item(cx: &Context, item: &TraitItem) -> Option<Result<ImplItem, syn::Error>> {
    match item {
        TraitItem::Fn(item) => match trait_func(cx, item) {
            Ok(ok) => Some(Ok(ImplItem::Fn(ok))),
            Err(err) => Some(Err(err)),
        },
        TraitItem::Const(item) => Some(Ok(ImplItem::Const(trait_const(item)?))),
        TraitItem::Type(item) => Some(Ok(ImplItem::Type(trait_type(item)?))),
        _ => None,
    }
}

fn trait_const(cst: &TraitItemConst) -> Option<ImplItemConst> {
    let cst = ImplItemConst {
        attrs: cst.attrs.clone(),
        vis: Visibility::Inherited,
        defaultness: None,
        const_token: Default::default(),
        ident: cst.ident.clone(),
        generics: Default::default(),
        colon_token: Default::default(),
        ty: cst.ty.clone(),
        eq_token: Default::default(),
        expr: cst.default.clone()?.1,
        semi_token: Default::default(),
    };
    Some(cst)
}

fn trait_type(ty: &TraitItemType) -> Option<ImplItemType> {
    let ty = ImplItemType {
        attrs: ty.attrs.clone(),
        vis: Visibility::Inherited,
        defaultness: None,
        type_token: Default::default(),
        ident: ty.ident.clone(),
        generics: Default::default(),
        eq_token: Default::default(),
        ty: ty.default.clone()?.1,
        semi_token: Default::default(),
    };
    Some(ty)
}

fn trait_func(cx: &Context, func: &TraitItemFn) -> Result<ImplItemFn, syn::Error> {
    let func_ident = &func.sig.ident;
    let constness = func.sig.constness;
    let asyncness = func.sig.asyncness;
    let unsafety = func.sig.unsafety;

    let (_trait_impl_generics, trait_ty_generics, trait_where_clause) =
        func.sig.generics.split_for_impl();
    let args = &func.sig.inputs;
    let ret = &func.sig.output;
    let func_block = match &func.default {
        None => {
            let action_ident = utils::format_action_ident(&cx.trait_def.ident, &func.sig.ident);
            let call_generics = {
                let trait_gen = utils::delifetimify_generics(&cx.trait_def.generics);
                let func_gen = utils::delifetimify_generics(&func.sig.generics);
                utils::merge_generics(trait_gen, func_gen)
            };
            let (_impl_generics, ty_generics, _where_clause) = call_generics.split_for_impl();
            let awaiting = if asyncness.is_some() {
                Some(quote! { .await })
            } else {
                None
            };
            let args = args
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

            parse_quote! {
                {
                    self.actions
                        .next::<#action_ident #ty_generics>()
                        .call(#(#args),*)
                        #awaiting
                }
            }
        }
        Some(def_block) => def_block.clone(),
    };

    let method = parse_quote! {
        #constness #asyncness #unsafety fn #func_ident #trait_ty_generics (#args) #ret
        #trait_where_clause
        {
            #func_block
        }
    };

    Ok(method)
}
