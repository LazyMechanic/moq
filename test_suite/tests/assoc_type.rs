#[test]
fn test1() {
    trait DummyTrait {}
    impl DummyTrait for i32 {}

    #[moq::automock]
    trait Trait {
        #[moq(default = "i32")]
        type AssocType: DummyTrait;
    }

    let _: <MockTrait as Trait>::AssocType = 1i32;
}

#[test]
fn test2() {
    trait DummyTrait {}
    impl DummyTrait for i32 {}

    #[moq::automock]
    trait Trait {
        #[moq(default = "i32")]
        type AssocType: DummyTrait;

        fn f(&self, arg: Self::AssocType);
    }

    let m = MockTrait::new().expect_f(|arg: i32| assert_eq!(arg, 1));
    m.f(1);
}

#[test]
fn test3() {
    trait DummyTrait {}
    impl DummyTrait for i32 {}

    #[moq::automock]
    trait Trait<G> {
        #[moq(default = "i32")]
        type AssocType: DummyTrait;

        fn f(&self, arg: Self::AssocType);
    }

    let m = MockTrait::<()>::new().expect_f(|arg: i32| assert_eq!(arg, 1));
    m.f(1);
}

#[test]
fn test4() {
    trait DummyTrait {}
    impl DummyTrait for i32 {}

    #[moq::automock]
    trait Trait<G> {
        #[moq(default = "i32")]
        type AssocType: DummyTrait;

        fn f(&self) -> Self::AssocType;
    }

    let m = MockTrait::<()>::new().expect_f(|| 1);
    assert_eq!(m.f(), 1);
}

#[test]
fn test5() {
    trait DummyTrait {}
    impl DummyTrait for i32 {}

    #[moq::automock]
    trait Trait {
        #[moq(default = "i32")]
        type AssocType1: DummyTrait;
        #[moq(default = "Self::AssocType1")]
        type AssocType2: DummyTrait;
    }

    let _: <MockTrait as Trait>::AssocType2 = 1i32;
}
