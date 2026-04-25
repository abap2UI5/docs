---
outline: [2, 4]
---
# UI5 Versions
The abap2UI5 frontend uses the SAP UI5 framework, which ships in several versions. Choose the version for abap2UI5 during [bootstrapping](/configuration/setup#ui5-bootstrapping).

Bootstrap abap2UI5 the same way as other UI5 apps on your system.

### OpenUI5
OpenUI5 is the open-source version of UI5. It has slightly fewer controls than the full UI5 framework but suits most apps. Most samples run on this version, and incompatibilities show up only in specific use cases.

### UI5
UI5 (SAPUI5) is the default version and ships with every ABAP system from a certain release onward. This is probably the version your system already uses.

### UI5 2.x
UI5 2.x is the newest version of UI5, with deprecated APIs removed. We test abap2UI5 against this release on every change to stay compatible with upcoming releases.

### Release-Specific
Some controls and properties are only available on specific UI5 releases. The abap2UI5 framework and its samples support many UI5 versions, reducing compatibility issues. But when building your own apps, check compatibility with the UI5 version your system uses.
