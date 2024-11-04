trait DummyTrait {}

#[moq::automock]
trait Trait {
    fn f(&self, _: impl DummyTrait);
}

fn main() {}
