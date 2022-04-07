#[derive(Debug, Eq, PartialEq)]
struct Struct(i32);

#[moq::automock]
trait Trait {
    fn method(&mut self, arg: i32);
    fn method_no_args(&mut self);
    fn method_lifetime<'a>(&'a mut self, arg: &'a i32);
    fn method_multiple_args<'a, 'b>(
        &mut self,
        arg1: i32,
        arg2: String,
        arg3: Struct,
        arg4: &'a i32,
        arg5: &'b i32,
    );

    fn method_def(&mut self, arg: i32) {
        self.method(arg)
    }
    fn method_no_args_def(&mut self) {
        self.method_no_args()
    }
    fn method_lifetime_def<'a>(&'a mut self, arg: &'a i32) {
        self.method_lifetime(arg)
    }
    fn method_multiple_args_def<'a, 'b>(
        &mut self,
        arg1: i32,
        arg2: String,
        arg3: Struct,
        arg4: &'a i32,
        arg5: &'b i32,
    ) {
        self.method_multiple_args(arg1, arg2, arg3, arg4, arg5)
    }

    #[moq(use_default)]
    fn method_def_use_default(&mut self, arg: i32) {
        self.method(arg)
    }
    #[moq(use_default)]
    fn method_no_args_def_use_default(&mut self) {
        self.method_no_args()
    }
    #[moq(use_default)]
    fn method_lifetime_def_use_default<'a>(&'a mut self, arg: &'a i32) {
        self.method_lifetime(arg)
    }
    #[moq(use_default)]
    fn method_multiple_args_def_use_default<'a, 'b>(
        &mut self,
        arg1: i32,
        arg2: String,
        arg3: Struct,
        arg4: &'a i32,
        arg5: &'b i32,
    ) {
        self.method_multiple_args(arg1, arg2, arg3, arg4, arg5)
    }

    fn method_ret(&mut self, arg: i32) -> i32;
    fn method_no_args_ret(&mut self) -> i32;
    fn method_lifetime_ret<'a>(&'a mut self, arg: &'a i32) -> &'a i32;
    fn method_multiple_args_ret<'a, 'b>(
        &mut self,
        arg1: i32,
        arg2: String,
        arg3: Struct,
        arg4: &'a i32,
        arg5: &'b i32,
    ) -> (i32, String, Struct, &'a i32, &'b i32);
}

// In rust you can't specify lifetimes for closure
// and auto lifetimes are wrong
fn method_lifetime_ret_check(arg: &i32) -> &i32 {
    assert_eq!(*arg, 1);
    arg
}

fn method_multiple_args_ret_check<'a, 'b>(
    arg1: i32,
    arg2: String,
    arg3: Struct,
    arg4: &'a i32,
    arg5: &'b i32,
) -> (i32, String, Struct, &'a i32, &'b i32) {
    assert_eq!(arg1, 1);
    assert_eq!(arg2, "2");
    assert_eq!(arg3, Struct(3));
    assert_eq!(arg4, &4);
    assert_eq!(arg5, &5);

    (arg1, arg2, arg3, arg4, arg5)
}

fn make_mock() -> impl Trait {
    TraitMock::new()
        .expect_call_method(|arg: i32| {
            assert_eq!(arg, 1);
        })
        .expect_call_method_no_args(|| {
            assert_eq!(true, true);
        })
        .expect_call_method_lifetime(|arg: &i32| {
            assert_eq!(*arg, 1);
        })
        .expect_call_method_multiple_args(
            |arg1: i32, arg2: String, arg3: Struct, arg4: &i32, arg5: &i32| {
                assert_eq!(arg1, 1);
                assert_eq!(arg2, "2");
                assert_eq!(arg3, Struct(3));
                assert_eq!(arg4, &4);
                assert_eq!(arg5, &5);
            },
        )
        .expect_call_method_def(|arg: i32| {
            assert_eq!(arg, 1);
        })
        .expect_call_method_no_args_def(|| {
            assert_eq!(true, true);
        })
        .expect_call_method_lifetime_def(|arg: &i32| {
            assert_eq!(*arg, 1);
        })
        .expect_call_method_multiple_args_def(
            |arg1: i32, arg2: String, arg3: Struct, arg4: &i32, arg5: &i32| {
                assert_eq!(arg1, 1);
                assert_eq!(arg2, "2");
                assert_eq!(arg3, Struct(3));
                assert_eq!(arg4, &4);
                assert_eq!(arg5, &5);
            },
        )
        .expect_call_method(|arg: i32| {
            assert_eq!(arg, 1);
        })
        .expect_call_method_no_args(|| {
            assert_eq!(true, true);
        })
        .expect_call_method_lifetime(|arg: &i32| {
            assert_eq!(*arg, 1);
        })
        .expect_call_method_multiple_args(
            |arg1: i32, arg2: String, arg3: Struct, arg4: &i32, arg5: &i32| {
                assert_eq!(arg1, 1);
                assert_eq!(arg2, "2");
                assert_eq!(arg3, Struct(3));
                assert_eq!(arg4, &4);
                assert_eq!(arg5, &5);
            },
        )
        .expect_call_method_ret(|arg: i32| {
            assert_eq!(arg, 1);
            arg
        })
        .expect_call_method_no_args_ret(|| {
            assert_eq!(true, true);
            1
        })
        .expect_call_method_lifetime_ret(method_lifetime_ret_check)
        .expect_call_method_multiple_args_ret(method_multiple_args_ret_check)
}

#[test]
fn test() {
    call_generic(make_mock());
    call_boxed(Box::new(make_mock()));
    call_ref(&mut make_mock());
}

fn call_generic<T: Trait>(mut obj: T) {
    obj.method(1);
    obj.method_no_args();
    obj.method_lifetime(&1);
    obj.method_multiple_args(1, "2".to_string(), Struct(3), &4, &5);

    obj.method_def(1);
    obj.method_no_args_def();
    obj.method_lifetime_def(&1);
    obj.method_multiple_args_def(1, "2".to_string(), Struct(3), &4, &5);

    obj.method_def_use_default(1);
    obj.method_no_args_def_use_default();
    obj.method_lifetime_def_use_default(&1);
    obj.method_multiple_args_def_use_default(1, "2".to_string(), Struct(3), &4, &5);

    assert_eq!(obj.method_ret(1), 1);
    assert_eq!(obj.method_no_args_ret(), 1);
    assert_eq!(obj.method_lifetime_ret(&1), &1);
    assert_eq!(
        obj.method_multiple_args_ret(1, "2".to_string(), Struct(3), &4, &5),
        (1, "2".to_string(), Struct(3), &4, &5)
    );
}

fn call_boxed(mut obj: Box<dyn Trait>) {
    obj.method(1);
    obj.method_no_args();
    obj.method_lifetime(&1);
    obj.method_multiple_args(1, "2".to_string(), Struct(3), &4, &5);

    obj.method_def(1);
    obj.method_no_args_def();
    obj.method_lifetime_def(&1);
    obj.method_multiple_args_def(1, "2".to_string(), Struct(3), &4, &5);

    obj.method_def_use_default(1);
    obj.method_no_args_def_use_default();
    obj.method_lifetime_def_use_default(&1);
    obj.method_multiple_args_def_use_default(1, "2".to_string(), Struct(3), &4, &5);

    assert_eq!(obj.method_ret(1), 1);
    assert_eq!(obj.method_no_args_ret(), 1);
    assert_eq!(obj.method_lifetime_ret(&1), &1);
    assert_eq!(
        obj.method_multiple_args_ret(1, "2".to_string(), Struct(3), &4, &5),
        (1, "2".to_string(), Struct(3), &4, &5)
    );
}

fn call_ref(obj: &mut dyn Trait) {
    obj.method(1);
    obj.method_no_args();
    obj.method_lifetime(&1);
    obj.method_multiple_args(1, "2".to_string(), Struct(3), &4, &5);

    obj.method_def(1);
    obj.method_no_args_def();
    obj.method_lifetime_def(&1);
    obj.method_multiple_args_def(1, "2".to_string(), Struct(3), &4, &5);

    obj.method_def_use_default(1);
    obj.method_no_args_def_use_default();
    obj.method_lifetime_def_use_default(&1);
    obj.method_multiple_args_def_use_default(1, "2".to_string(), Struct(3), &4, &5);

    assert_eq!(obj.method_ret(1), 1);
    assert_eq!(obj.method_no_args_ret(), 1);
    assert_eq!(obj.method_lifetime_ret(&1), &1);
    assert_eq!(
        obj.method_multiple_args_ret(1, "2".to_string(), Struct(3), &4, &5),
        (1, "2".to_string(), Struct(3), &4, &5)
    );
}
