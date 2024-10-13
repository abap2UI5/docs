# Productive Usage

#### **1. Can abap2UI5 used in a productive system?**
The project is technically just an implementation of an HTTP handler and can be used as any other HTTP Service also in a productive scenario.
#### **2. Are there any dependencies or preparations needed before using abap2UI5 in a productiv scenario?**
No, but it is recommended to follow these steps before using abap2UI5 apps in a productive scenario:
1. Transport the abap2UI5 HTTP service and the framework first
2. Sometimes an extra activation of the HTTP service is needed, along with an adjustment of the UI5 bootstrapping
3. Test the "hello world" app to ensure that abap2UI5 works correctly
4. Now continue with tranporting your custom apps...
#### **3. Does a stable version of abap2UI5 exist?**
The project will be continuously further developed. Therefore, there is no specific "stable" version. However, adjustments to the public APIs will be kept to a minimum to avoid frequent refactoring of apps. You can use [releases](https://github.com/abap2ui5/abap2ui5/releases/) instead of pulling the main branch and only update from time to time to reduce refactoring efforts.
