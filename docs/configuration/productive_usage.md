# Use in Production

abapUI5 is technically just an implementation of an HTTP handler and can be used as any other HTTP Service also in a productive scenario.

#### Preparations

You can transport abap2UI5 to production just like any other ABAP development. To ensure everything works smoothly, consider the following steps:
1. Transport the abap2UI5 HTTP service and the framework first
2. Sometimes an extra activation of the HTTP service is needed, along with an adjustment of the UI5 bootstrapping
3. Test the "hello world" app to ensure that abap2UI5 works correctly
4. Now continue with tranporting your custom apps...

#### Stable Version
The project will be continuously further developed. Therefore, there is no specific "stable" version. However, adjustments to the public APIs will be kept to a minimum to avoid frequent refactoring of apps. You can use [releases](https://github.com/abap2ui5/abap2ui5/releases/) instead of pulling the main branch and only update from time to time to reduce refactoring efforts.

#### Renaming

#### Transport

PICTURE abapGit and then transport