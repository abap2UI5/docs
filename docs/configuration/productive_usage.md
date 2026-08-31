---
outline: [2, 4]
---
# Productive Usage

Technically, abap2UI5 is just an HTTP handler implementation — use it like any other HTTP service in production.

## Stable Version
The project evolves all the time, so there's no fixed "stable" version. But we keep changes to the public APIs minimal to avoid frequent app refactoring. Pin to a [release](https://github.com/abap2UI5/abap2UI5/releases/) instead of tracking the main branch, and update regularly to keep refactoring effort low.

## Quality Assurance
Every commit to the framework runs through CI before it reaches a release:

- Builds and static checks against **NW 7.02, Standard ABAP, and ABAP Cloud** — one workflow per release line, so compatibility across the supported stacks is verified continuously, not per release
- **Unit tests** executed on every commit via [open-abap](/technical/tools/open_abap), plus automated **browser tests** of the running framework
- The sample catalogues double as a regression corpus: hundreds of apps that are validated against the framework

## Renaming
If you're starting new development but already have abap2UI5 apps in production and want to avoid update risk, install multiple instances of abap2UI5 with the [renaming feature](/advanced/renaming). This lets you keep developing safely without disrupting your existing production apps.
