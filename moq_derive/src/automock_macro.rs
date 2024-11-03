use crate::action::{Action, ActionCollection};
use crate::context::Context;
use crate::mock::Mock;
use proc_macro2_diagnostics::Diagnostic;

use crate::utils::ItemTraitExt;
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::{ItemTrait, TraitItem};

pub fn automock_impl(
    _args: proc_macro2::TokenStream,
    input: proc_macro2::TokenStream,
) -> Result<proc_macro2::TokenStream, Diagnostic> {
    let p = syn::parse2::<AutomockMacro>(input)?;
    let output = quote! { #p };
    //panic!("{}", output.to_string());

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
        let trait_def = input.parse::<ItemTrait>()?;
        let demoquified_trait_def = trait_def.clone().demoqified();
        let cx = Context::from_ast(trait_def)?;
        let actions_def = ActionCollection::from_ast(&cx)?;
        let actions = cx
            .trait_def
            .items
            .iter()
            .filter_map(|item| match item {
                TraitItem::Fn(item) => Some(item),
                _ => None,
            })
            .map(|trait_func| Action::from_ast(&cx, trait_func))
            .collect::<Result<_, _>>()?;
        let mock = Mock::from_ast(&cx)?;

        Ok(Self {
            trait_def: demoquified_trait_def,
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
