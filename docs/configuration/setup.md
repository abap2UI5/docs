---
outline: [2, 4]
---
# Setup

Every UI5 application has an `index.html` that bootstraps the framework, picks a theme, defines the page title and so on. With abap2UI5 you don't maintain that file by hand — the framework generates it on every page load. To change what ends up in it, implement the `z2ui5_if_exit` interface in your own ABAP class:

```abap
CLASS zcl_a2ui5_user_exit DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_exit.

ENDCLASS.

CLASS zcl_a2ui5_user_exit IMPLEMENTATION.

  METHOD z2ui5_if_exit~set_config_http_get.
    " your configuration goes here
  ENDMETHOD.

ENDCLASS.
```

`set_config_http_get` is called once per page request — i.e. when the browser asks for the HTML shell. The changing parameter `cs_config` holds every value that ends up in the generated `index.html`.

## What You Can Configure

| Page                                                              | Field on `cs_config`         | Affects in `index.html` |
|-------------------------------------------------------------------|------------------------------|-------------------------|
| [Theme](/configuration/setup/theme)                               | `theme`                      | `data-sap-ui-theme` on the bootstrap script |
| [Bootstrapping](/configuration/setup/ui5_bootstrapping)           | `src`                        | `src` of the bootstrap script (UI5 version + delivery channel) |
| [Bootstrap Attributes](/configuration/setup/bootstrap_attributes) | `t_add_config`               | Additional `data-sap-ui-*` attributes |
| [Style / CSS](/configuration/setup/style_css)                     | `styles_css`                 | Inline `<style>` block in the page `<head>` |
| [Language](/configuration/setup/logon_language)                   | URL parameter `sap-language` | SAP session language + UI5 locale |

Security-relevant headers and the Content Security Policy meta tag are configured separately — see [Security](/configuration/security).

## See Also

- Official SAP documentation on [UI5 bootstrapping](https://sapui5.hana.ondemand.com/#/topic/91f2cebe7c8e4d289fd80a4f0c0bd2ca) and [configuration options](https://sapui5.hana.ondemand.com/#/topic/91f2d03b6f4d1014b6dd926db0e91070).
- The [User Exit](/advanced/extensibility/user_exits) page documents every hook on `z2ui5_if_exit`.
