# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.5.0] - 2024-11-05
### Changed
- Change default generated mock name by `automock` `%TRAIT%Mock` -> `Mock%TRAIT%`.
- Bump `syn` 1.0.x -> 2.0.x
### Added
- Supports `RPITIT` (Return Position Impl Trait In Trait).
- `#[moq(rename = "FooBar")]` / `#[moq::automock(rename = "FooBar")]` attribute for renaming generated mock struct.
- `#[moq(output = "Type")]` attribute for `RPITIT`

## Dark times without changelog...