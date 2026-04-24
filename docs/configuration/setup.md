---
outline: [2, 4]
---
# General

You can run abap2UI5 with various custom configurations. The default setup runs automatically. To customize it, implement the interface `z2ui5_if_exit`:

```abap
CLASS zcl_a2ui5_user_exit DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES  z2ui5_if_exit.

ENDCLASS.
```

### Theme
For example, to change the theme:
```abap
METHOD z2ui5_if_exit~set_config_http_get.

    cs_config-theme = `sap_horizon_dark`.

ENDMETHOD.
```

### UI5 Bootstrapping
To specify the source for bootstrapping UI5:
```abap
METHOD z2ui5_if_exit~set_config_http_get.

    cs_config-src = `https://ui5.sap.com/1.116.0/resources/sap-ui-core.js`.

ENDMETHOD.
```
Note the bootstrapping variants and SAP limitations, documented in the [SAP UI5 documentation](https://sapui5.hana.ondemand.com/#/topic/2d3eb2f322ea4a82983c1c62a33ec4ae).

### Title
To set a custom title for the app:
```abap
METHOD z2ui5_if_exit~set_config_http_get.

    cs_config-title = `my title`.

ENDMETHOD.
```

### Style / CSS
To apply custom styles or CSS:
```abap
METHOD z2ui5_if_exit~set_config_http_get.

    cs_config-styles_css = `<<own css>>`.

ENDMETHOD.
```

### Logon Language
To set the logon language, use the `sap-language` URL parameter. For other URL parameters, see [SAP's documentation](https://help.sap.com/doc/saphelp_nw75/7.5.5/en-US/8b/46468c433b40c3b87b2e07f34dea1b/content.htm?no_cache=true).
