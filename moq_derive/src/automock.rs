use crate::action::{Action, ActionCollection};
use crate::context::Context;
use crate::mock::Mock;

use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::{ItemTrait, TraitItem};

pub struct Automock {
    trait_def: ItemTrait,
    actions_def: ActionCollection,
    actions: Vec<Action>,
    mock: Mock,
}

impl Parse for Automock {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let cx = Context::parse(input)?;
        let actions_def = ActionCollection::parse(&cx)?;
        let actions = cx
            .trait_def
            .items
            .iter()
            .filter_map(|item| match item {
                TraitItem::Method(item) => Some(&item.sig),
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

impl ToTokens for Automock {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.trait_def.to_tokens(tokens);
        self.actions_def.to_tokens(tokens);
        self.actions.iter().for_each(|act| act.to_tokens(tokens));
        self.mock.to_tokens(tokens);
    }
}
