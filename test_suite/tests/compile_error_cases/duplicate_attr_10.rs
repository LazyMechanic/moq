#[moq::automock]
trait Trait {
    #[moq(default)]
    #[moq(default)]
    fn f(&self) {}
}

fn main() {}
