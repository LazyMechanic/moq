#![allow(dead_code)]

use std::future::Future;

use moq::moq;

trait ReturnTrait {}
impl ReturnTrait for () {}

#[cfg_attr(feature = "disabled-feature", moq::automock)]
trait Trait {
    #[moq(default = "i32")]
    type AssocType;

    #[moq(default = 42)]
    const CONST1: i32;

    #[moq(default_with = "make_const")]
    const CONST2: i32;

    #[moq(default)]
    fn f1(&self) {}

    #[moq(output = "std::pin::Pin<_>")]
    fn f2(&self) -> impl Future<Output = ()>;

    #[moq(output = "()")]
    fn f3(&self) -> impl ReturnTrait;
}

const fn make_const() -> i32 {
    42
}
