#[test]
fn test1() {
    trait DummyTrait {
        fn dummy(&self) -> i32;
    }
    impl DummyTrait for Box<dyn DummyTrait> {
        fn dummy(&self) -> i32 {
            (**self).dummy()
        }
    }
    impl DummyTrait for i32 {
        fn dummy(&self) -> i32 {
            *self
        }
    }

    #[moq::automock]
    trait Trait {
        fn f(&self) -> impl DummyTrait;
    }

    let m = TraitMock::new().expect_f(|| Box::new(42i32) as Box<dyn DummyTrait>);
    assert_eq!(m.f().dummy(), 42);
}
