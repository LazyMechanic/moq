#[test]
fn test1() {
    trait DummyTrait {}
    impl DummyTrait for i32 {}

    #[moq::automock]
    trait Trait {
        #[moq(default = "i32")]
        type AssocType: DummyTrait;
    }

    let _: <TraitMock as Trait>::AssocType = 1i32;
}

// TODO: fix assoc types
// #[test]
// fn test2() {
//     trait DummyTrait {}
//     impl DummyTrait for i32 {}
//
//     #[moq::automock]
//     trait Trait {
//         #[moq(value = "i32")]
//         type AssocType: DummyTrait;
//
//         fn f(&self, arg: Self::AssocType);
//     }
//
//     let m = TraitMock::new().expect_f(|arg: <TraitMock as Trait>::AssocType| assert_eq!(arg, 1));
//     m.f(1);
// }
