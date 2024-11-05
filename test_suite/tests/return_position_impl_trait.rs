use std::{future::Future, pin::Pin};

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

    let m = MockTrait::new().expect_f(|| Box::new(42i32) as Box<dyn DummyTrait>);
    assert_eq!(m.f().dummy(), 42);
}

#[test]
fn test2() {
    trait DummyTrait {
        fn dummy(&self) -> i32;
    }
    impl DummyTrait for i32 {
        fn dummy(&self) -> i32 {
            *self
        }
    }

    #[moq::automock]
    trait Trait {
        #[moq(output = "i32")]
        fn f(&self) -> impl DummyTrait;
    }

    let m = MockTrait::new().expect_f(|| 42);
    assert_eq!(m.f().dummy(), 42);
}

#[test]
fn test3() {
    struct DummyWrapper<T>(pub T);

    trait DummyTrait {
        fn dummy(&self) -> i32;
    }
    impl<T> DummyTrait for DummyWrapper<T>
    where
        T: DummyTrait,
    {
        fn dummy(&self) -> i32 {
            self.0.dummy()
        }
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
        #[moq(output = "DummyWrapper<_>")]
        fn f(&self) -> impl DummyTrait;
    }

    let m = MockTrait::new().expect_f(|| DummyWrapper(Box::new(42i32) as Box<dyn DummyTrait>));
    assert_eq!(m.f().dummy(), 42);
}

#[test]
fn test4() {
    #[moq::automock]
    trait Trait {
        #[moq(output = "Pin<_>")]
        fn f<R>(&self) -> impl Future<Output = R>
        where
            R: 'static;
    }

    let m = MockTrait::new()
        .expect_f(|| Box::pin(async { 42i32 }) as Pin<Box<dyn Future<Output = i32>>>);
    let _ = m.f::<i32>();
}
