use crate::action::{Action, ActionCollection};
use crate::context::Context;
use crate::mock::Mock;
use proc_macro2_diagnostics::Diagnostic;

use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::{ItemTrait, TraitItem};

pub fn automock_impl(
    _args: proc_macro2::TokenStream,
    input: proc_macro2::TokenStream,
) -> Result<proc_macro2::TokenStream, Diagnostic> {
    let p = syn::parse2::<AutomockMacro>(input)?;
    let output = quote! { #p };

    Ok(output)
}

pub struct AutomockMacro {
    trait_def: ItemTrait,
    actions_def: ActionCollection,
    actions: Vec<Action>,
    mock: Mock,
}

impl Parse for AutomockMacro {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let cx = Context::parse(input)?;
        let actions_def = ActionCollection::parse(&cx)?;
        let actions = cx
            .trait_def
            .items
            .iter()
            .filter_map(|item| match item {
                TraitItem::Fn(item) => Some(&item.sig),
                _ => None,
            })
            .map(|sig| Action::parse(&cx, sig))
            .collect::<Result<_, _>>()?;
        let mock = Mock::parse(&cx)?;

        Ok(Self {
            trait_def: cx.trait_def_orig,
            actions_def,
            actions,
            mock,
        })
    }
}

impl ToTokens for AutomockMacro {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.trait_def.to_tokens(tokens);
        self.actions_def.to_tokens(tokens);
        self.actions.iter().for_each(|act| act.to_tokens(tokens));
        self.mock.to_tokens(tokens);
    }
}
