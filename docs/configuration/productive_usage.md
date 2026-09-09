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
- **Unit tests** executed on every commit via [open-abap](/technical/tools/#open-abap), plus automated **browser tests** of the running framework
- The sample catalogs double as a regression corpus: hundreds of apps that are validated against the framework

## Renaming
If you're starting new development but already have abap2UI5 apps in production and want to avoid update risk, install multiple instances of abap2UI5 with the [renaming feature](/advanced/renaming). This lets you keep developing safely without disrupting your existing production apps.

## Transport

Transport abap2UI5 to production like any other ABAP project.

For a smooth rollout, follow these steps:
1. Transport the abap2UI5 HTTP service and the framework first.
2. Activate the HTTP service explicitly if needed, and adjust the [UI5 bootstrap source](/configuration/setup/ui5_bootstrapping) if production should use a different UI5 version or delivery channel.
3. Test the "Hello World" app to confirm abap2UI5 works as expected.
4. Finally, transport your own apps.

Install the project via abapGit on your development system. Then use the standard transport process to deploy to production:
![Transport process from development to production via abapGit](/configuration/image-3.webp){ width=80% }
