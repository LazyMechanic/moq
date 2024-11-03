use crate::action::{Action, ActionCollection};
use crate::context::Context;
use crate::mock::Mock;
use proc_macro2_diagnostics::Diagnostic;

use crate::utils::ItemTraitExt;
use quote::{quote, ToTokens};
use syn::{ItemTrait, TraitItem};

pub fn automock_impl(
    args: proc_macro2::TokenStream,
    input: proc_macro2::TokenStream,
) -> Result<proc_macro2::TokenStream, Diagnostic> {
    let p = AutomockMacro::from_ast(args, input)?;
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

impl AutomockMacro {
    pub fn from_ast(
        args: proc_macro2::TokenStream,
        input: proc_macro2::TokenStream,
    ) -> Result<Self, syn::Error> {
        let cx = Context::from_ast(args, input)?;
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
            trait_def: cx.trait_def.demoqified(),
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
