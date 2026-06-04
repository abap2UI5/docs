---
outline: [2, 4]
---
# Info

abap2UI5 ships the current frontend state with every roundtrip. Read it from `client->get( )` — no custom control, no extra event needed. The relevant sub-structures are `s_device`, `s_ui5`, `s_focus`, and `s_scroll`.

#### Device

For reading device information via `client->get( )-s_device`, see [Device Model](../model/device_model.md).

#### UI5

`client->get( )-s_ui5` returns the runtime UI5 framework details — handy for version-dependent branching or for logging which build a user runs on.

```abap
DATA(ui5) = client->get( )-s_ui5.

DATA(version)         = ui5-version.           " e.g. `1.141.0`
DATA(build_timestamp) = ui5-build_timestamp.
DATA(gav)             = ui5-gav.               " group, artifact, version
DATA(theme)           = ui5-theme.             " e.g. `sap_horizon`
```

##### OpenUI5 or SAPUI5

The `gav` (group–artifact–version) field tells you which UI5 [distribution](../../configuration/ui5_versions.md) is loaded: SAPUI5 ships the `com.sap.ui5` modules, OpenUI5 does not. Checking whether `gav` contains `com.sap.ui5` is the same logic the framework itself uses internally to detect the distribution. No custom control and no extra roundtrip are needed — the info is part of every request, so it already works in `check_on_init( )`:

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

#### Focus

For reading the current focus via `client->get( )-s_focus`, see [Focus](../browser_interaction/focus.md).

#### Scroll

For reading scroll positions via `client->get( )-s_scroll`, see [Scrolling](../browser_interaction/scrolling.md).
