struct DummyWrapper<'a>(&'a ());

#[moq::automock]
trait Trait {
    fn f(&self, arg: DummyWrapper<'_>);
}

fn main() {}
