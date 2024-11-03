#[test]
fn test1() {
    #[moq::automock(rename = "FooBar")]
    trait Trait {
        fn f(&self);
    }

    let m = FooBar::new().expect_f(|| {});
    m.f();
}

#[test]
fn test2() {
    #[moq::automock]
    #[moq(rename = "FooBar")]
    trait Trait {
        fn f(&self);
    }

    let m = FooBar::new().expect_f(|| {});
    m.f();
}
