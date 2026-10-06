---
outline: [2, 4]
---
# Bootstrap Attributes

The UI5 bootstrap script tag in `index.html` accepts a long list of `data-sap-ui-*` attributes — they control asynchronous loading, the compatibility version, clickjacking protection, locale, preloaded libraries and many more. abap2UI5 already sets sensible defaults:

```html
<script id="sap-ui-bootstrap"
        src="…/sap-ui-core.js"
        data-sap-ui-theme="sap_horizon"
        data-sap-ui-resourceroots='{ "z2ui5": "./" }'
        data-sap-ui-oninit="onInitComponent"
        data-sap-ui-compatVersion="edge"
        data-sap-ui-async="true"
        data-sap-ui-frameOptions="trusted"
        data-sap-ui-bindingSyntax="complex">
</script>
```

## Add Attributes

To add an attribute, append a row to `cs_config-t_add_config`. Each row contributes one `name='value'` pair to the script tag — after the defaults above, which therefore cannot be overridden this way (see the note below the table):

```abap
METHOD z2ui5_if_ui5_exit~set_config_http_get.

    cs_config-t_add_config = VALUE #(
      ( n = `data-sap-ui-libs`         v = `sap.m,sap.ui.table` )
      ( n = `data-sap-ui-language`     v = `en` )
      ( n = `data-sap-ui-preload`      v = `async` ) ).

ENDMETHOD.
```

## Useful Attributes

| Attribute                       | Purpose |
|---------------------------------|---------|
| `data-sap-ui-libs`              | Comma-separated list of UI5 libraries to preload (e.g. `sap.m,sap.ui.table`). Trade load time against startup speed. |
| `data-sap-ui-language`          | UI5 locale; overrides the browser language. See [Language](/configuration/setup/logon_language). |
| `data-sap-ui-compatVersion`     | Compatibility version, controls UI5 behavior for deprecated APIs. Set to `edge` by abap2UI5 — fixed. |
| `data-sap-ui-async`             | Asynchronous module loading. Set to `true` by abap2UI5 — fixed. |
| `data-sap-ui-preload`           | Module preloading strategy: `async`, `sync` or empty (off). |
| `data-sap-ui-frameOptions`      | Clickjacking protection: `trusted`, `allow`, `deny`. Set to `trusted` by abap2UI5 — fixed. |
| `data-sap-ui-allowlistService`  | Endpoint for the URL allowlist service. |
| `data-sap-ui-bindingSyntax`     | Binding syntax: `complex` or `simple`. Set to `complex` by abap2UI5 — fixed, and its expressions require it. |
| `data-sap-ui-resourceroots`     | Resource roots for custom libraries. Set by abap2UI5 (`z2ui5`) — fixed; your own controls load through the reserved [`z2ui5_ccc` root](/advanced/extensibility/custom_control) instead. |
| `data-sap-ui-xx-componentpreload` | Component-preload strategy for very large apps. |

The attributes abap2UI5 writes itself (`data-sap-ui-async`, `data-sap-ui-frameOptions`, `data-sap-ui-compatVersion`, `data-sap-ui-bindingSyntax`, `data-sap-ui-theme`, …) cannot be overridden here: the rows are appended after them, and a browser keeps the first of two attributes with the same name. `t_add_config` adds attributes; the theme has its own field (`cs_config-theme`).

## See Also

- Official UI5 [configuration options and URL parameters reference](https://sapui5.hana.ondemand.com/#/topic/91f2d03b6f4d1014b6dd926db0e91070) — the authoritative list of every supported attribute.
- [Bootstrapping](/configuration/setup/ui5_bootstrapping) — how to change the bootstrap script source.
