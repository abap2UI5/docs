---
outline: [2, 4]
---
# Productive Usage

abap2UI5 is technically just an implementation of an HTTP handler and can be used like any other HTTP service in a productive scenario.

#### Preparations
You can transport abap2UI5 to production just like any other ABAP development. To ensure everything works smoothly, consider the following steps:
1. Transport the abap2UI5 HTTP service and the framework first.
2. Sometimes, extra activation of the HTTP service is needed, along with an adjustment of the UI5 bootstrapping.
3. Test the "Hello World" app to ensure that abap2UI5 works correctly.
4. Afterward, continue by transporting your custom apps.

#### Stable Version
The project is continuously developed, so there's no specific "stable" version. However, changes to the public APIs are kept to a minimum to avoid frequent refactoring of apps. You can use the [releases](https://github.com/abap2UI5/abap2UI5/releases/) instead of pulling the main branch and update periodically to reduce refactoring effort.

#### Transport
Install the project using abapGit into your development system. Then, use the normal transport process for deployment to production:
![Transport process from development to production via abapGit](/configuration/image-3.png){ width=80% }

#### Renaming
If you're starting new development but already have abap2UI5 apps in production and want to avoid the risk of updates, consider installing multiple instances of abap2UI5 using the [renaming feature](/advanced/renaming). This lets you continue development safely without affecting your existing production apps.
