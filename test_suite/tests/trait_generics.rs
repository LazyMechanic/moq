#[test]
fn test1() {
    #[moq::automock]
    trait Trait<A> {
        fn f(&self);
    }

    let m = TraitMock::<i32>::new().expect_f(|| {});
    m.f();
}

#[test]
fn test2() {
    #[moq::automock]
    trait Trait<A> {
        fn f(&self, arg: A);
    }

    let m = TraitMock::new().expect_f(|arg: i32| assert_eq!(arg, 1));
    m.f(1i32);
}

#[test]
fn test3() {
    #[moq::automock]
    trait Trait<A> {
        fn f(&self) -> A;
    }

    let m = TraitMock::new().expect_f(|| 1i32);
    assert_eq!(m.f(), 1i32);
}

#[test]
fn test4() {
    #[moq::automock]
    trait Trait<A> {
        fn f(&self, arg: A) -> A;
    }

    let m = TraitMock::new().expect_f(|arg: i32| {
        assert_eq!(arg, 1);
        arg
    });
    assert_eq!(m.f(1), 1i32);
}

#[test]
fn test5() {
    #[moq::automock]
    trait Trait<A, B> {
        fn f(&self, arg: A) -> B;
    }

    let m = TraitMock::new().expect_f(|arg: String| {
        assert_eq!(arg, "1");
        2i32
    });
    assert_eq!(m.f("1".to_string()), 2);
}

#[test]
fn test6() {
    #[moq::automock]
    trait Trait<A, B> {
        fn f<'a>(&self, arg1: &'a A, arg2: B, arg3: &'a str);
    }

    fn t<'a>(arg1: &'a i32, arg2: f32, arg3: &'a str) {
        assert_eq!(arg1, &1);
        assert_eq!(arg2, 2f32);
        assert_eq!(arg3, "3");
    }
    let m = TraitMock::new().expect_f(t);
    m.f(&1, 2f32, "3");
}

#[test]
fn test7() {
    #[moq::automock]
    trait Trait<A, B> {
        fn f(&self) -> (&'static A, B, &'static str);
    }

    fn t() -> (&'static i32, f32, &'static str) {
        (&1, 2f32, "3")
    }
    let m = TraitMock::new().expect_f(t);
    assert_eq!(m.f(), (&1, 2f32, "3"));
}

#[test]
fn test8() {
    #[moq::automock]
    trait Trait<A, B> {
        fn f<'a>(&self, arg1: &'a A, arg2: B, arg3: &'a str) -> (&'a A, B, &'a str);
    }

    fn t<'a>(arg1: &'a i32, arg2: f32, arg3: &'a str) -> (&'a i32, f32, &'a str) {
        assert_eq!(arg1, &1);
        assert_eq!(arg2, 2f32);
        assert_eq!(arg3, "3");
        (arg1, arg2, arg3)
    }
    let m = TraitMock::new().expect_f(t);
    assert_eq!(m.f(&1, 2f32, "3"), (&1, 2f32, "3"));
}

#[test]
fn test9() {
    use std::fmt::Debug;

    #[derive(Debug, Eq, PartialEq)]
    struct Struct1<T>(T)
    where
        T: Debug + Eq + PartialEq;

    #[derive(Debug, Eq, PartialEq)]
    struct Struct2<T, I>(T, Struct1<I>)
    where
        T: Debug + Eq + PartialEq,
        I: Debug + Eq + PartialEq;

    #[moq::automock]
    trait Trait<A, B>
    where
        A: Debug + Eq + PartialEq,
        B: Debug + Eq + PartialEq,
    {
        fn f(&self, arg1: Struct1<A>, arg2: Struct2<A, B>) -> (Struct1<A>, Struct2<A, B>);
    }

    fn t(
        arg1: Struct1<i32>,
        arg2: Struct2<i32, &'static str>,
    ) -> (Struct1<i32>, Struct2<i32, &'static str>) {
        assert_eq!(arg1, Struct1(1));
        assert_eq!(arg2, Struct2(2, Struct1("3")));
        (arg1, arg2)
    }
    let m = TraitMock::new().expect_f(t);
    assert_eq!(
        m.f(Struct1(1), Struct2(2, Struct1("3"))),
        (Struct1(1), Struct2(2, Struct1("3")))
    );
}
