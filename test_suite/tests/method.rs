#[test]
fn test1() {
    #[moq::automock]
    trait Trait {
        fn f(&self);
    }

    let m = MockTrait::new().expect_f(|| {});
    m.f();
}

#[test]
fn test2() {
    #[moq::automock]
    trait Trait {
        fn f(&mut self);
    }

    let mut m = MockTrait::new().expect_f(|| {});
    m.f();
}

#[test]
fn test3() {
    #[moq::automock]
    trait Trait {
        fn f(&self, arg: i32);
    }

    let m = MockTrait::new().expect_f(|arg: i32| assert_eq!(arg, 42));
    m.f(42);
}

#[test]
fn test4() {
    #[moq::automock]
    trait Trait {
        fn f<'a>(&self, arg1: i32, arg2: String, arg3: &'a i32);
    }

    let m = MockTrait::new().expect_f(|arg1: i32, arg2: String, arg3: &i32| {
        assert_eq!(arg1, 1);
        assert_eq!(arg2, "2");
        assert_eq!(arg3, &3);
    });
    m.f(1, "2".to_string(), &3);
}

#[test]
fn test5() {
    #[moq::automock]
    trait Trait {
        fn f(&self) -> i32;
    }

    let m = MockTrait::new().expect_f(|| 42);
    assert_eq!(m.f(), 42);
}

#[test]
fn test6() {
    #[moq::automock]
    trait Trait {
        fn f(&self) -> (i32, String, &'static i32);
    }

    let m = MockTrait::new().expect_f(|| (1, "2".to_string(), &3));
    assert_eq!(m.f(), (1i32, "2".to_string(), &3i32));
}

#[test]
fn test7() {
    #[moq::automock]
    trait Trait {
        fn f<'a>(&self, arg1: i32, arg2: String, arg3: &'a i32) -> (i32, String, &'a i32);
    }

    fn t<'a>(arg1: i32, arg2: String, arg3: &'a i32) -> (i32, String, &'a i32) {
        assert_eq!(arg1, 1);
        assert_eq!(arg2, "2");
        assert_eq!(arg3, &3);
        (arg1, arg2, arg3)
    }
    let m = MockTrait::new().expect_f(t);
    assert_eq!(m.f(1, "2".to_string(), &3), (1i32, "2".to_string(), &3i32));
}

#[test]
fn test7_2() {
    #[moq::automock]
    trait Trait {
        fn f<'a>(&self, arg1: i32, arg2: String, arg3: &'a i32) -> (i32, String, &'a i32);
    }

    let m = MockTrait::new().expect_f(moq::lambda!(
        fn <'a>(arg1: i32, arg2: String, arg3: &'a i32) -> (i32, String, &'a i32) {
            assert_eq!(arg1, 1);
            assert_eq!(arg2, "2");
            assert_eq!(arg3, &3);
            (arg1, arg2, arg3)
        }
    ));
    assert_eq!(m.f(1, "2".to_string(), &3), (1i32, "2".to_string(), &3i32));
}

#[test]
fn test8() {
    #[moq::automock]
    trait Trait {
        fn f<'a, 'b>(&self, arg1: &'a i32, arg2: String, arg3: &'b i32) -> (i32, String, &'b i32);
    }

    fn t<'a, 'b>(arg1: &'a i32, arg2: String, arg3: &'b i32) -> (i32, String, &'b i32) {
        assert_eq!(arg1, &1);
        assert_eq!(arg2, "2");
        assert_eq!(arg3, &3);
        (*arg1, arg2, arg3)
    }
    let m = MockTrait::new().expect_f(t);
    assert_eq!(m.f(&1, "2".to_string(), &3), (1i32, "2".to_string(), &3i32));
}

#[test]
fn test8_2() {
    #[moq::automock]
    trait Trait {
        fn f<'a, 'b>(&self, arg1: &'a i32, arg2: String, arg3: &'b i32) -> (i32, String, &'b i32);
    }

    let m = MockTrait::new().expect_f(moq::lambda!(
        fn <'a, 'b>(arg1: &'a i32, arg2: String, arg3: &'b i32) -> (i32, String, &'b i32) {
            assert_eq!(arg1, &1);
            assert_eq!(arg2, "2");
            assert_eq!(arg3, &3);
            (*arg1, arg2, arg3)
        }
    ));
    assert_eq!(m.f(&1, "2".to_string(), &3), (1i32, "2".to_string(), &3i32));
}

#[test]
fn test9() {
    #[derive(Debug, Eq, PartialEq)]
    struct Struct1<'a>(&'a str);

    #[derive(Debug, Eq, PartialEq)]
    struct Struct2<'a, 'b>(&'a Struct1<'b>);

    #[moq::automock]
    trait Trait {
        fn f<'a, 'b>(
            &self,
            arg1: Struct1<'a>,
            arg2: Struct2<'a, 'b>,
            arg3: i32,
        ) -> (Struct1<'a>, Struct2<'a, 'b>, i32);
    }

    fn t<'a, 'b>(
        arg1: Struct1<'a>,
        arg2: Struct2<'a, 'b>,
        arg3: i32,
    ) -> (Struct1<'a>, Struct2<'a, 'b>, i32) {
        assert_eq!(arg1, Struct1("1"));
        assert_eq!(arg2, Struct2(&Struct1("2")));
        assert_eq!(arg3, 3);
        (arg1, arg2, arg3)
    }
    let m = MockTrait::new().expect_f(t);
    assert_eq!(
        m.f(Struct1("1"), Struct2(&Struct1("2")), 3),
        (Struct1("1"), Struct2(&Struct1("2")), 3)
    );
}

#[test]
fn test9_2() {
    #[derive(Debug, Eq, PartialEq)]
    struct Struct1<'a>(&'a str);

    #[derive(Debug, Eq, PartialEq)]
    struct Struct2<'a, 'b>(&'a Struct1<'b>);

    #[moq::automock]
    trait Trait {
        fn f<'a, 'b>(
            &self,
            arg1: Struct1<'a>,
            arg2: Struct2<'a, 'b>,
            arg3: i32,
        ) -> (Struct1<'a>, Struct2<'a, 'b>, i32);
    }

    let m = MockTrait::new().expect_f(moq::lambda!(
        fn <'a, 'b>(
            arg1: Struct1<'a>,
            arg2: Struct2<'a, 'b>,
            arg3: i32,
        ) -> (Struct1<'a>, Struct2<'a, 'b>, i32) {
            assert_eq!(arg1, Struct1("1"));
            assert_eq!(arg2, Struct2(&Struct1("2")));
            assert_eq!(arg3, 3);
            (arg1, arg2, arg3)
        }
    ));
    assert_eq!(
        m.f(Struct1("1"), Struct2(&Struct1("2")), 3),
        (Struct1("1"), Struct2(&Struct1("2")), 3)
    );
}
