# Moq

This library provides macro that provides mock struct generating that implements trait.

```toml
[dependencies]
moq = "0.1"
```

## Usage example
```rust
#[moq::automock]
trait Trait {
    fn func(&self, arg: i64) -> String;
}

#[test]
fn test_ok() {
    let mock = TraitMock::new()
        .expect_call_func(|arg: i64| {
            assert_eq!(arg, 42);
            format!("Hello, {}", arg)
        })
        .expect_call_func(|arg: i64| {
            assert_eq!(arg, -1);
            format!("Hello again, {}", -1)
        });
    
    mock.func(42);
    mock.func(-1);
}

#[test]
fn test_failed_extra_call() {
    let mock = TraitMock::new()
        .expect_call_func(|arg: i64| {
            assert_eq!(arg, 42);
            format!("Hello, {}", arg)
        });

    mock.func(42);
    mock.func(-1); // Panic here
}

#[test]
fn test_failed_missing_call() {
    let mock = TraitMock::new()
        .expect_call_func(|arg: i64| {
            assert_eq!(arg, 42);
            format!("Hello, {}", arg)
        })
        .expect_call_func(|arg: i64| {
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

#[test]
fn test_ok() {
    let mock = TraitMock::new()
        .expect_call_func(|arg: i64| async {
            assert_eq!(arg, 42);
            format!("Hello, {}", arg)
        });
    
    mock.func(42).await;
}
```

You can find more examples in the tests.

## TODO
- Supporting static functions
- Supporting generic functions
- Macro `moq::mock!(..)` for generating mock struct for external trait
- Generating mock struct for struct without trait

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