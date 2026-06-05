---
outline: [2, 4]
---
# Info

abap2UI5 ships the current frontend state with every roundtrip. Read it from `client->get( )` — no custom control, no extra event needed. The relevant sub-structures are `s_device`, `s_ui5`, `s_focus`, and `s_scroll`.

#### Device

For reading device information via `client->get( )-s_device`, see [Device Model](../model/device_model.md).

#### UI5

For reading the runtime UI5 framework details via `client->get( )-s_ui5`, see [UI5 Versions](../../configuration/ui5_versions.md).

#### Focus

For reading the current focus via `client->get( )-s_focus`, see [Focus](../browser_interaction/focus.md).

#### Scroll

For reading scroll positions via `client->get( )-s_scroll`, see [Scrolling](../browser_interaction/scrolling.md).
