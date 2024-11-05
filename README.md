# Moq

[<img alt="Crates.io Version" src="https://img.shields.io/crates/v/moq?style=flat-square">](https://crates.io/crates/moq)
[<img alt="docs.rs" src="https://img.shields.io/docsrs/moq?style=flat-square">](https://docs.rs/moq)
[<img alt="GitHub Actions Workflow Status" src="https://img.shields.io/github/actions/workflow/status/LazyMechanic/moq/ci.yaml?branch=master&style=flat-square">](https://github.com/LazyMechanic/moq/actions/workflows/ci.yml)

This library provides macro that provides mock struct generating that implements trait.

```toml
[dependencies]
moq = "0.4"
```

## Usage example
```rust
#[moq::automock]
trait Trait {
    fn func(&self, arg: i64) -> String;
}

#[test]
fn test_ok() {
    let mock = MockTrait::new()
        .expect_func(|arg: i64| {
            assert_eq!(arg, 42);
            format!("Hello, {}", arg)
        })
        .expect_func(|arg: i64| {
            assert_eq!(arg, -1);
            format!("Hello again, {}", -1)
        });

    mock.func(42);
    mock.func(-1);
}

#[test]
#[should_panic]
fn test_failed_extra_call() {
    let mock = MockTrait::new()
        .expect_func(|arg: i64| {
            assert_eq!(arg, 42);
            format!("Hello, {}", arg)
        });

    mock.func(42);
    mock.func(-1); // Panic here
}

#[test]
#[should_panic]
fn test_failed_missing_call() {
    let mock = MockTrait::new()
        .expect_func(|arg: i64| {
            assert_eq!(arg, 42);
            format!("Hello, {}", arg)
        })
        .expect_func(|arg: i64| {
            assert_eq!(arg, -1);
            format!("Hello again, {}", -1)
        });

    mock.func(42);
    // Panic here
}
```

`async_trait` also works, but the main limitation is to specify the `automock` macro above the `async_trait`
```rust
#[moq::automock]
#[async_trait::async_trait]
trait Trait {
    async fn func(&self, arg: i64) -> String;
}

#[tokio::test]
async fn test_ok() {
    let mock = MockTrait::new()
        .expect_func(|arg: i64| async {
            assert_eq!(arg, 42);
            format!("Hello, {}", arg)
        });

    mock.func(42).await;
}
```

You can find more examples in the [tests](test_suite/tests).

## TODO
- [x] Supporting generic functions
- [ ] Supporting static functions
- [ ] Macro `moq::mock!(..)` for generating mock struct for external trait
- [ ] Generating mock struct for struct without trait
- [x] Capturing environment in `moq::lambda!`

#### License

<sup>
Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a> at your option.
</sup>

<br>

<sub>
Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this crate by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
</sub>