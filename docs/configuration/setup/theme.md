---
outline: [2, 4]
---
# Theme

The theme defines the look and feel of every UI5 control — colours, fonts, paddings, spacings. In a hand-written `index.html` it is set via the `data-sap-ui-theme` attribute on the bootstrap script. In abap2UI5 you set it from ABAP:

```abap
METHOD z2ui5_if_exit~set_config_http_get.

    cs_config-theme = `sap_horizon_dark`.

ENDMETHOD.
```

If you leave the field empty, abap2UI5 falls back to `sap_horizon`.

## Available Themes

The themes shipped with current UI5 versions are:

| Theme name           | Description                          |
|----------------------|--------------------------------------|
| `sap_horizon`        | Default Horizon (light)              |
| `sap_horizon_dark`   | Horizon dark                         |
| `sap_horizon_hcb`    | Horizon high-contrast black          |
| `sap_horizon_hcw`    | Horizon high-contrast white          |
| `sap_fiori_3`        | Quartz Light (previous default)      |
| `sap_fiori_3_dark`   | Quartz Dark                          |
| `sap_fiori_3_hcb`    | Quartz high-contrast black           |
| `sap_fiori_3_hcw`    | Quartz high-contrast white           |
| `sap_belize`         | Belize (older, blue)                 |
| `sap_belize_plus`    | Belize Plus (older, grey background) |

See the official [list of available themes](https://sapui5.hana.ondemand.com/#/topic/4cfe7eff3001447a9d4b0abeaba95166) for the most up-to-date catalogue.

## End-User Override

The user can override the theme at runtime with the URL parameter `sap-ui-theme`:

```
…/z2ui5/sample?sap-ui-theme=sap_horizon_dark
```

This is convenient for trying out a theme without redeploying.

## Custom Themes

For brand-specific colours, fonts or logos use the SAP [UI Theme Designer](https://sapui5.hana.ondemand.com/#/topic/be8f7c61bb2444299b3f3429b986e8be) to generate a self-contained theme. Host it on your SAP system or any web server and point `cs_config-theme` to its theme ID (the bootstrap source must reach it via the standard UI5 theme loader).

See the official [theming documentation](https://sapui5.hana.ondemand.com/#/topic/91f2cebe7c8e4d289fd80a4f0c0bd2ca) for the bigger picture.
