#[test]
fn compile_error() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/compile_error_cases/*.rs");
}
