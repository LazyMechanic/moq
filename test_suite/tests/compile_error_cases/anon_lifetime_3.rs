struct DummyWrapper<'a>(&'a ());

#[moq::automock]
trait Trait {
    fn f(&self) -> DummyWrapper<'_>;
}

fn main() {}
