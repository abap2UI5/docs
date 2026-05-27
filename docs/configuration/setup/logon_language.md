---
outline: [2, 4]
---
# Language

abap2UI5 picks the user's logon language automatically from the SAP session — every translated text retrieved via `z2ui5_cl_util=>trans_get_text( )`, every OData/CDS label, every message class is returned in that language. For most applications no extra configuration is required.

## Override the Backend Language

To force a specific SAP logon language at startup, append the `sap-language` URL parameter:

```
…/z2ui5/sample?sap-language=EN
…/z2ui5/sample?sap-language=DE
```

The value is a one- or two-character SAP language key (typically ISO 639-1, e.g. `EN`, `DE`, `FR`, `JA`). The server sets the user's session language for this request, so backend reads, OData calls and translated texts all use that locale.

## Override the UI5 Frontend Locale

The UI5 framework keeps a *separate* locale for date and number formatting, control labels ("Search", "Cancel", …) and the calendar week. By default UI5 derives it from the browser. You can pin it with the URL parameter `sap-ui-language`:

```
…/z2ui5/sample?sap-ui-language=de
```

Most apps don't need this — the browser locale and the SAP logon language agree in practice. Set it explicitly when you need full control, e.g. in test environments.

## Forcing the Frontend Locale at Startup

You can also pin the UI5 locale via the bootstrap, instead of relying on a URL parameter:

```abap
METHOD z2ui5_if_exit~set_config_http_get.

    cs_config-t_add_config = VALUE #(
      ( n = `data-sap-ui-language` v = `en` ) ).

ENDMETHOD.
```

See [Bootstrap Attributes](/configuration/setup/bootstrap_attributes) for more on this mechanism.

## See Also

- [SAP URL parameter reference](https://help.sap.com/doc/saphelp_nw75/7.5.5/en-US/8b/46468c433b40c3b87b2e07f34dea1b/content.htm?no_cache=true) — full list of supported parameters.
- Official UI5 documentation on [language and locale](https://sapui5.hana.ondemand.com/#/topic/91f21f176f4d1014b6dd926db0e91070).
