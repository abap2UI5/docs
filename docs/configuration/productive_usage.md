---
outline: [2, 4]
---
# Productive Usage

Technically, abap2UI5 is just an HTTP handler implementation — use it like any other HTTP service in production.

#### Preparations
Transport abap2UI5 to production like any other ABAP project. For a smooth rollout, follow these steps:
1. Transport the abap2UI5 HTTP service and the framework first.
2. Activate the HTTP service explicitly if needed, and tweak the UI5 bootstrapping.
3. Test the "Hello World" app to confirm abap2UI5 works correctly.
4. Then transport your own apps.

#### Stable Version
The project evolves all the time, so there's no fixed "stable" version. But we keep changes to the public APIs minimal to avoid frequent app refactoring. Pin to a [release](https://github.com/abap2UI5/abap2UI5/releases/) instead of pulling the main branch, and update regularly to keep refactoring effort low.

#### Transport
Install the project via abapGit on your development system. Then use the standard transport process to deploy to production:
![Transport process from development to production via abapGit](/configuration/image-3.png){ width=80% }

#### Renaming
If you're starting new development but already have abap2UI5 apps in production and want to avoid update risk, install multiple instances of abap2UI5 with the [renaming feature](/advanced/renaming). This lets you keep developing safely without affecting your existing production apps.
