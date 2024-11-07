---
outline: [2, 4]
---
# UI5 Versions
The frontend UI for abap2UI5 is built using the UI5 framework, which comes in different versions. You can select the version to use for abap2UI5 during bootstrapping. It is recommended to bootstrap abap2UI5 using the same repository that is used for other UI5 apps in your system.

### OpenUI5
OpenUI5 is the open-source version of UI5. It has slightly fewer controls than the full UI5 framework but is generally suitable for most applications. Most samples are compatible with this version, and issues are typically only encountered with very specific use cases.

### UI5
UI5 (SAPUI5) is the default version and has been part of the ABAP system since a specific release. This is likely the version already in use in your system.

### UI5 2.x
UI5 2.x is the newest version of UI5, where deprecated APIs have been removed. abap2UI5 is continuously tested with this release to ensure smooth compatibility with the latest features. It is recommended to bootstrap your apps with this version to ensure they are future-ready.

### Release-Specific
Some controls and properties are only available in specific releases. The UI5 framework and the samples are designed to support a wide range of releases and should generally not cause issues. However, it's important to ensure compatibility with the version you are using, as not all features may be available across different releases.