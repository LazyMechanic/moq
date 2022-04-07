//! # Moq
//!
//! This library provides macro that provides mock struct generating that implements trait.
//!
//! ## Example
//! ```
//! use moq::automock;
//!
//! #[automock]
//! trait Trait {
//!     fn func(&self, arg: i32) -> String;
//! }
//!
//! #[test]
//! fn test_ok() {
//!     let mock = TraitMock::new()
//!         .expect_call_func(|arg: i64| {
//!             assert_eq!(arg, 42);
//!             format!("Hello, {}", arg)
//!         })
//!         .expect_call_func(|arg: i64| {
//!             assert_eq!(arg, -1);
//!             format!("Hello again, {}", -1)
//!         });
//!     
//!     mock.func(42);
//!     mock.func(-1);
//! }
//! ```

pub use moq_derive::automock;

use std::future::Future;

pub trait Func<Args, Ret> {
    fn call(&self, args: Args) -> Ret;
}

macro_rules! impl_func (
    ($($param:ident)*) => {
        impl<Function, $($param,)* Ret> Func<($($param,)*), Ret> for Function
        where
            Function: Fn($($param),*) -> Ret,
        {
            #[inline]
            #[allow(non_snake_case)]
            fn call(&self, ($($param,)*): ($($param,)*)) -> Ret {
                (self)($($param,)*)
            }
        }
    }
);

impl_func!();
impl_func!(A);
impl_func!(A B);
impl_func!(A B C);
impl_func!(A B C D);
impl_func!(A B C D E);
impl_func!(A B C D E F);
impl_func!(A B C D E F G);
impl_func!(A B C D E F G H);
impl_func!(A B C D E F G H I);
impl_func!(A B C D E F G H I J);
impl_func!(A B C D E F G H I J K);
impl_func!(A B C D E F G H I J K L);

#[async_trait::async_trait]
pub trait AsyncFunc<Args, Ret> {
    async fn call(&self, arg: Args) -> Ret;
}

macro_rules! impl_async_func (
    ($($param:ident)*) => {
        #[async_trait::async_trait]
        impl<Function, $($param,)* Ret, Fut> AsyncFunc<($($param,)*), Ret> for Function
        where
            Function: Fn($($param),*) -> Fut + Sync + 'static,
            Fut: Future<Output = Ret> + Send,
            $($param: Send + 'static,)*
        {
            #[inline]
            #[allow(non_snake_case)]
            async fn call(&self, ($($param,)*): ($($param,)*)) -> Ret {
                (self)($($param,)*).await
            }
        }
    }
);

impl_async_func!();
impl_async_func!(A);
impl_async_func!(A B);
impl_async_func!(A B C);
impl_async_func!(A B C D);
impl_async_func!(A B C D E);
impl_async_func!(A B C D E F);
impl_async_func!(A B C D E F G);
impl_async_func!(A B C D E F G H);
impl_async_func!(A B C D E F G H I);
impl_async_func!(A B C D E F G H I J);
impl_async_func!(A B C D E F G H I J K);
impl_async_func!(A B C D E F G H I J K L);
