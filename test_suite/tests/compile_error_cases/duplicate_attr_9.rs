#[moq::automock]
trait Trait {
    #[moq(default, default)]
    fn f(&self) {}
}

fn main() {}
