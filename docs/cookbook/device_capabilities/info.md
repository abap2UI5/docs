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

#### Focus

For reading the current focus via `client->get( )-s_focus`, see [Focus](../browser_interaction/focus.md).

#### Scroll

For reading scroll positions via `client->get( )-s_scroll`, see [Scrolling](../browser_interaction/scrolling.md).
