---
outline: [2, 4]
---
# UI5 Versions
The frontend of abap2UI5 is built using the SAP UI5 framework, which is available in various versions. You can select the version to use for abap2UI5 during [bootstrapping](/configuration/setup.html#ui5-bootstrapping). 

It is recommended to bootstrap abap2UI5 in the same way as other UI5 apps in your system.

### OpenUI5
OpenUI5 is the open-source version of UI5. It has slightly fewer controls than the full UI5 framework but is generally suitable for most applications. Most samples run with this version, and incompatibilities are typically only encountered with very specific use cases.

### UI5
UI5 (SAPUI5) is the default version and has been part of every ABAP system since a specific release. This is likely the version already in use in your system.

### UI5 2.x
UI5 2.x is the newest version of UI5, where deprecated APIs have been removed. abap2UI5 is continuously tested with this release to ensure compatibility with upcoming releases.

### Release-Specific
Certain controls and properties are only available in specific UI5 releases. The abap2UI5 framework and its samples are designed to support a wide range of UI5 versions, minimizing compatibility issues. However, when developing your own applications, it is essential to verify compatibility with the specific UI5 version in use on your system.
