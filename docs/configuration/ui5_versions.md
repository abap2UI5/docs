# UI5 Versions

abap2UI5 is compatible with every ABAP system. The UI is created with the UI5 framework, which exists in different versions, and you can choose the version for abap2UI5 during bootstrapping. It’s recommended to bootstrap abap2UI5 using the same repository that you use for other UI5 apps in your system.

### OpenUI5
This is the open-source version of UI5. It has slightly fewer controls than the full UI5 framework. Most samples work with this version, and issues should only arise with very specific apps.

### UI5
This version is used by default and has been part of the ABAP system since a certain release. Most likely, this is already the version you’re using.

### UI5 2.x
The newest version of UI5, where deprecated APIs have been removed. abap2UI5 is continuously tested with this release to ensure a smooth transition to the latest version. Bootstrap your apps with this UI5 version to check if they are future-ready.
