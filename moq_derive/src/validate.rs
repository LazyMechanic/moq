use std::ops::Not;

use syn::{visit, visit::Visit, FnArg, ItemTrait, Signature, Type};

pub fn validate(trait_def: &ItemTrait) -> Result<(), syn::Error> {
    check_impl_trait(trait_def)?;
    check_anon_lifetime(trait_def)?;
    check_static_func(trait_def)?;
    Ok(())
}

fn check_impl_trait(trait_def: &ItemTrait) -> Result<(), syn::Error> {
    let mut vis = ImplTraitVisitor::default();
    vis.visit_item_trait(trait_def);
    vis.result
}

fn check_anon_lifetime(trait_def: &ItemTrait) -> Result<(), syn::Error> {
    let mut vis = AnonLifetimeVisitor::default();
    vis.visit_item_trait(trait_def);
    vis.result
}

fn check_static_func(trait_def: &ItemTrait) -> Result<(), syn::Error> {
    let mut vis = StaticFuncVisitor::default();
    vis.visit_item_trait(trait_def);
    vis.result
}

struct ImplTraitVisitor {
    result: Result<(), syn::Error>,
}

impl Default for ImplTraitVisitor {
    fn default() -> Self {
        Self { result: Ok(()) }
    }
}

impl Visit<'_> for ImplTraitVisitor {
    fn visit_fn_arg(&mut self, i: &FnArg) {
        match i {
            FnArg::Receiver(i) => {
                visit::visit_receiver(self, i);
            }
            FnArg::Typed(i) => match &*i.ty {
                Type::ImplTrait(_) if self.result.is_ok() => {
                    self.result = Err(syn::Error::new_spanned(
                        i,
                        "`impl Trait` in argument position not supported, use generics `T: Trait` instead",
                    ));
                }
                _ => visit::visit_pat_type(self, i),
            },
        }
    }
}

struct AnonLifetimeVisitor {
    result: Result<(), syn::Error>,
}

impl Default for AnonLifetimeVisitor {
    fn default() -> Self {
        Self { result: Ok(()) }
    }
}

impl Visit<'_> for AnonLifetimeVisitor {
    fn visit_lifetime(&mut self, i: &syn::Lifetime) {
        if i.ident == "_" && self.result.is_ok() {
            self.result = Err(syn::Error::new_spanned(
                i,
                "anonymized lifetimes not supported",
            ))
        }
    }
}

struct StaticFuncVisitor {
    result: Result<(), syn::Error>,
}

impl Default for StaticFuncVisitor {
    fn default() -> Self {
        Self { result: Ok(()) }
    }
}

impl Visit<'_> for StaticFuncVisitor {
    fn visit_signature(&mut self, i: &Signature) {
        if self.result.is_ok() {
            let is_static_func = i
                .inputs
                .iter()
                .any(|item| matches!(item, &FnArg::Receiver(_)))
                .not();

            if is_static_func {
                self.result = Err(syn::Error::new_spanned(i, "static functions not supported"));
            }
        }

        visit::visit_signature(self, i);
    }
}
