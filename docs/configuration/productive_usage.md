---
outline: [2, 4]
---
# Productive Usage

abap2UI5 is technically just an implementation of an HTTP handler, so you can use it like any other HTTP service in a productive scenario.

#### Preparations
You can transport abap2UI5 to production like any other ABAP development. For smooth operation, consider the following steps:
1. Transport the abap2UI5 HTTP service and the framework first.
2. You may need to explicitly activate the HTTP service and adjust the UI5 bootstrapping.
3. Test the "Hello World" app to confirm that abap2UI5 works correctly.
4. Then transport your custom apps.

#### Stable Version
The project evolves continuously, so no specific "stable" version exists. However, we keep changes to the public APIs to a minimum to avoid frequent refactoring of apps. Use the [releases](https://github.com/abap2UI5/abap2UI5/releases/) instead of pulling the main branch and update periodically to reduce refactoring effort.

#### Transport
Install the project via abapGit into your development system. Then use the normal transport process to deploy to production:
![Transport process from development to production via abapGit](/configuration/image-3.png){ width=80% }

#### Renaming
If you're starting new development but already have abap2UI5 apps in production and want to avoid the risk of updates, consider installing multiple instances of abap2UI5 via the [renaming feature](/advanced/renaming). This lets you continue development safely without affecting your existing production apps.
