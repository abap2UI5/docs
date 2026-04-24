---
outline: [2, 4]
---
# Productive Usage

Technically, abap2UI5 is just an HTTP handler implementation, so you can use it like any other HTTP service in production.

#### Preparations
Transport abap2UI5 to production like any other ABAP project. For smooth operation, consider these steps:
1. Transport the abap2UI5 HTTP service and the framework first.
2. Activate the HTTP service explicitly if needed, and adjust the UI5 bootstrapping.
3. Test the "Hello World" app to confirm abap2UI5 works correctly.
4. Then transport your own apps.

#### Stable Version
The project evolves continuously, so there's no fixed "stable" version. But we keep changes to the public APIs minimal to avoid frequent app refactoring. Use the [releases](https://github.com/abap2UI5/abap2UI5/releases/) instead of pulling the main branch, and update periodically to reduce refactoring effort.

#### Transport
Install the project through abapGit in your development system. Then use the standard transport process to deploy to production:
![Transport process from development to production via abapGit](/configuration/image-3.png){ width=80% }

#### Renaming
If you're starting new development but already have abap2UI5 apps in production and want to avoid update risk, install multiple instances of abap2UI5 through the [renaming feature](/advanced/renaming). This lets you continue development safely without touching your existing production apps.
