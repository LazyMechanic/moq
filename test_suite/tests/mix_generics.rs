#[test]
fn test1() {
    #[moq::automock]
    trait Trait<T1> {
        fn f<F1>(&self)
        where
            F1: 'static;
    }

    let m = TraitMock::<&'static str>::new().expect_f::<i32, _>(|| {});
    m.f::<i32>();
}

#[test]
fn test2() {
    #[moq::automock]
    trait Trait<T1> {
        fn f<F1>(&self, arg1: T1, arg2: F1)
        where
            F1: 'static;
    }

    let m = TraitMock::new().expect_f(|arg1: i32, arg2: &'static str| {
        assert_eq!(arg1, 1);
        assert_eq!(arg2, "2");
    });
    m.f(1i32, "2");
}

#[test]
fn test3() {
    #[moq::automock]
    trait Trait<T1> {
        fn f<F1>(&self) -> (T1, F1)
        where
            F1: 'static;
    }

    let m = TraitMock::new().expect_f(|| (1i32, "2"));
    assert_eq!(m.f(), (1i32, "2"));
}

#[test]
fn test4() {
    #[moq::automock]
    trait Trait<T1> {
        fn f<F1>(&self, arg1: T1, arg2: F1) -> (T1, F1)
        where
            F1: 'static;
    }

    let m = TraitMock::new().expect_f(|arg1: i32, arg2: &'static str| {
        assert_eq!(arg1, 1);
        assert_eq!(arg2, "2");
        (arg1, arg2)
    });
    assert_eq!(m.f(1i32, "2"), (1i32, "2"));
}
