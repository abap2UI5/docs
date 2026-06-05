---
outline: [2, 4]
---
# UI5 Versions
The abap2UI5 frontend uses the SAP UI5 framework, which ships in several versions. Choose the version for abap2UI5 during [bootstrapping](/configuration/setup/ui5_bootstrapping).

Bootstrap abap2UI5 the same way as other UI5 apps on your system.

### OpenUI5
OpenUI5 is the open-source version of UI5. It has slightly fewer controls than the full UI5 framework but suits most apps. Most samples run on this version, and incompatibilities show up only in specific use cases.

### UI5
UI5 (SAPUI5) is the default version and ships with every ABAP system from a certain release onward. This is probably the version your system already uses.

### UI5 2.x
UI5 2.x is the newest version of UI5, with deprecated APIs removed. We test abap2UI5 against this release on every change to stay compatible with upcoming releases.

### Legacy-Free
A dedicated frontend variant based on OpenUI5's legacy-free distribution removes deprecated features such as jQuery dependencies and synchronous APIs, providing a preview of the UI5 2.x API surface. For details, see [UI5 Legacy-Free](../advanced/legacy_free.md).

### Release-Specific
Some controls and properties are only available on specific UI5 releases. The abap2UI5 framework and its samples support many UI5 versions, reducing compatibility issues. But when building your own apps, check compatibility with the UI5 version your system uses.

## Reading the UI5 Version at Runtime
abap2UI5 ships the current frontend state with every roundtrip. Read it from `client->get( )` — no custom control, no extra event needed.

`client->get( )-s_ui5` returns the runtime UI5 framework details — handy for version-dependent branching or for logging which build a user runs on.

```abap
DATA(ui5) = client->get( )-s_ui5.

DATA(version)         = ui5-version.           " e.g. `1.141.0`
DATA(build_timestamp) = ui5-build_timestamp.
DATA(gav)             = ui5-gav.               " group, artifact, version
DATA(theme)           = ui5-theme.             " e.g. `sap_horizon`
```

### OpenUI5 or SAPUI5
The `gav` (group–artifact–version) field tells you which UI5 distribution is loaded: SAPUI5 ships the `com.sap.ui5` modules, OpenUI5 does not. Checking whether `gav` contains `com.sap.ui5` is the same logic the framework itself uses internally to detect the distribution. No custom control and no extra roundtrip are needed — the info is part of every request, so it already works in `check_on_init( )`:

```abap
IF client->check_on_init( ).

  DATA(ui5) = client->get( )-s_ui5.

  DATA(text) = COND string(
    WHEN ui5-gav IS INITIAL          THEN |UI5 distribution unknown ({ ui5-version })|
    WHEN ui5-gav CS `com.sap.ui5`    THEN |SAPUI5 is running ({ ui5-version })|
    ELSE                                  |OpenUI5 is running ({ ui5-version })| ).

  client->message_box_display( text ).

ENDIF.
```

::: tip
`VersionInfo.load()` can fail on the frontend, leaving `gav` empty. Treat an initial `gav` as *unknown* rather than assuming OpenUI5 — hence the explicit `IS INITIAL` check above.
:::
