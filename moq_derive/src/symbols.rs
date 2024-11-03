use quote::IdentFragment;
use std::fmt::Display;
use syn::{Ident, Path};

macro_rules! define_symbols(
    ($($name:ident => $value:literal),*,) => {
        $(pub const $name: Symbol<'static> = Symbol($value));*;
    };
);

define_symbols! {
    // Macro name
    MOQ => "moq",

    // Flags
    DEFAULT => "default",
    DEFAULT_WITH => "default_with",
    OUTPUT => "output",
    RENAME => "rename",
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Symbol<'a>(&'a str);

impl<'a> Symbol<'a> {
    pub fn inner(&self) -> &'a str {
        self.0
    }
}

impl PartialEq<Symbol<'_>> for String {
    fn eq(&self, other: &Symbol<'_>) -> bool {
        self == other.0
    }
}

impl<'a> PartialEq<Symbol<'_>> for &'a String {
    fn eq(&self, other: &Symbol<'_>) -> bool {
        *self == other.0
    }
}

impl PartialEq<Symbol<'_>> for str {
    fn eq(&self, other: &Symbol<'_>) -> bool {
        self == other.0
    }
}

impl PartialEq<Symbol<'_>> for Ident {
    fn eq(&self, other: &Symbol<'_>) -> bool {
        self == other.0
    }
}

impl<'a> PartialEq<Symbol<'_>> for &'a Ident {
    fn eq(&self, other: &Symbol<'_>) -> bool {
        *self == other.0
    }
}

impl PartialEq<Symbol<'_>> for Path {
    fn eq(&self, other: &Symbol<'_>) -> bool {
        self.is_ident(other.0)
    }
}

impl<'a> PartialEq<Symbol<'_>> for &'a Path {
    fn eq(&self, other: &Symbol<'_>) -> bool {
        self.is_ident(other.0)
    }
}

impl Display for Symbol<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

impl IdentFragment for Symbol<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(self.0)
    }
}
