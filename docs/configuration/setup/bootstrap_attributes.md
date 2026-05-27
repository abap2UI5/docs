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

## Add or Override Attributes

To add an attribute — or override one of the defaults — append a row to `cs_config-t_add_config`. Each row contributes one `name='value'` pair to the script tag:

```abap
METHOD z2ui5_if_exit~set_config_http_get.

    cs_config-t_add_config = VALUE #(
      ( n = `data-sap-ui-libs`         v = `sap.m,sap.ui.table` )
      ( n = `data-sap-ui-language`     v = `en` )
      ( n = `data-sap-ui-frameOptions` v = `allow` )
      ( n = `data-sap-ui-preload`      v = `async` ) ).

ENDMETHOD.
```

## Useful Attributes

| Attribute                       | Purpose |
|---------------------------------|---------|
| `data-sap-ui-libs`              | Comma-separated list of UI5 libraries to preload (e.g. `sap.m,sap.ui.table`). Trade load time against startup speed. |
| `data-sap-ui-language`          | UI5 locale; overrides the browser language. See [Language](/configuration/setup/logon_language). |
| `data-sap-ui-compatVersion`     | Compatibility version, controls UI5 behaviour for deprecated APIs. abap2UI5 defaults to `edge`. |
| `data-sap-ui-async`             | Asynchronous module loading. Default `true` — only change for legacy reasons. |
| `data-sap-ui-preload`           | Module preloading strategy: `async`, `sync` or empty (off). |
| `data-sap-ui-frameOptions`      | Clickjacking protection: `trusted`, `allow`, `deny`. Default `trusted`. |
| `data-sap-ui-allowlistService`  | Endpoint for the URL allowlist service. |
| `data-sap-ui-bindingSyntax`     | Binding syntax: `complex` (default) or `simple`. abap2UI5 expressions require `complex`. |
| `data-sap-ui-resourceroots`     | Additional resource roots for custom libraries. |
| `data-sap-ui-xx-componentpreload` | Component-preload strategy for very large apps. |

Attributes set by abap2UI5 by default can be overridden — a row with the same name wins over the framework default.

## See Also

- Official UI5 [configuration options and URL parameters reference](https://sapui5.hana.ondemand.com/#/topic/91f2d03b6f4d1014b6dd926db0e91070) — the authoritative list of every supported attribute.
- [Bootstrapping](/configuration/setup/ui5_bootstrapping) — how to change the bootstrap script source.
