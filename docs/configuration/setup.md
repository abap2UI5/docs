---
outline: [2, 4]
---

# General

You can run abap2UI5 with various custom configurations. The default setup is used automatically. For custom configurations, just implement the interface `z2ui5_if_exit`:

```abap
CLASS zcl_a2ui5_user_exit DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES  z2ui5_if_exit.

ENDCLASS.
```

### Theme
For example, to change the theme, the implementation would look like this:
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
Keep in mind the variants and SAP limitations for bootstrapping, which you can find [here.](https://sapui5.hana.ondemand.com/#/topic/2d3eb2f322ea4a82983c1c62a33ec4ae)

### Title
To set a custom title for the application:
```abap
METHOD z2ui5_if_exit~set_config_http_get.

    cs_config-title = `my title`.

ENDMETHOD.
```

### Style / CSS
To apply custom styles or CSS, use the following:
```abap
METHOD z2ui5_if_exit~set_config_http_get.

    cs_config-styles_css = `<<own css>>`.

ENDMETHOD.
```

### Logon Language
To set the logon language, use the `sap-language` URL parameter. For more options via URL parameters, check out the documentation [here.](https://help.sap.com/doc/saphelp_nw75/7.5.5/de-DE/8b/46468c433b40c3b87b2e07f34dea1b/content.htm?no_cache=true)
