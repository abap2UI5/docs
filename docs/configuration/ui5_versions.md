---
outline: [2, 4]
---
# UI5 Versions
The abap2UI5 frontend uses the SAP UI5 framework, which ships in various versions. You can select the version to use for abap2UI5 during [bootstrapping](/configuration/setup#ui5-bootstrapping).

We recommend bootstrapping abap2UI5 like other UI5 apps in your system.

### OpenUI5
OpenUI5 is the open-source version of UI5. It has slightly fewer controls than the full UI5 framework but suits most applications. Most samples run with this version, and incompatibilities typically arise only in specific use cases.

### UI5
UI5 (SAPUI5) is the default version and has been part of every ABAP system since a specific release. This is likely the version your system already uses.

### UI5 2.x
UI5 2.x is the newest version of UI5, with deprecated APIs removed. We continuously test abap2UI5 with this release to maintain compatibility with upcoming releases.

### Release-Specific
Certain controls and properties are only available in specific UI5 releases. The abap2UI5 framework and its samples support many UI5 versions, minimizing compatibility issues. However, when developing your own applications, verify compatibility with the UI5 version your system uses.
