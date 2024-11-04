use std::ops::Not;
use syn::visit_mut::VisitMut;
use syn::{visit_mut, FnArg, ItemTrait, Signature, Type};

pub fn validate(trait_def: &mut ItemTrait) -> Result<(), syn::Error> {
    check_impl_trait(trait_def)?;
    check_anon_lifetime(trait_def)?;
    check_static_func(trait_def)?;
    Ok(())
}

fn check_impl_trait(trait_def: &mut ItemTrait) -> Result<(), syn::Error> {
    let mut vis = ImplTraitVisitor::default();
    vis.visit_item_trait_mut(trait_def);
    vis.result
}

fn check_anon_lifetime(trait_def: &mut ItemTrait) -> Result<(), syn::Error> {
    let mut vis = AnonLifetimeVisitor::default();
    vis.visit_item_trait_mut(trait_def);
    vis.result
}

fn check_static_func(trait_def: &mut ItemTrait) -> Result<(), syn::Error> {
    let mut vis = StaticFuncVisitor::default();
    vis.visit_item_trait_mut(trait_def);
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

impl VisitMut for ImplTraitVisitor {
    fn visit_fn_arg_mut(&mut self, i: &mut FnArg) {
        match i {
            FnArg::Receiver(i) => {
                visit_mut::visit_receiver_mut(self, i);
            }
            FnArg::Typed(i) => match &*i.ty {
                Type::ImplTrait(_) if self.result.is_ok() => {
                    self.result = Err(syn::Error::new_spanned(
                        i,
                        "`impl Trait` in argument position not supported, use generics `T: Trait` instead",
                    ));
                }
                _ => visit_mut::visit_pat_type_mut(self, i),
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

impl VisitMut for AnonLifetimeVisitor {
    fn visit_lifetime_mut(&mut self, i: &mut syn::Lifetime) {
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

impl VisitMut for StaticFuncVisitor {
    fn visit_signature_mut(&mut self, i: &mut Signature) {
        if self.result.is_ok() {
            let is_static_func = i
                .inputs
                .iter()
                .any(|item| matches!(item, &FnArg::Receiver(_)))
                .not();

            if is_static_func {
                self.result = Err(syn::Error::new_spanned(
                    &*i,
                    "static functions not supported",
                ));
            }
        }

        visit_mut::visit_signature_mut(self, i);
    }
    fn visit_fn_arg_mut(&mut self, i: &mut FnArg) {
        match i {
            FnArg::Receiver(i) => {
                visit_mut::visit_receiver_mut(self, i);
            }
            FnArg::Typed(i) => match &*i.ty {
                Type::ImplTrait(_) if self.result.is_ok() => {
                    self.result = Err(syn::Error::new_spanned(
                        i,
                        "`impl Trait` in argument position not supported, use generics `T: Trait` instead",
                    ));
                }
                _ => visit_mut::visit_pat_type_mut(self, i),
            },
        }
    }
}
