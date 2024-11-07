---
outline: [2, 4]
---

# General

abap2UI5 can be run with various custom configurations. The default setup can be invoked using the following call:

```abap
METHOD if_http_extension~handle_request.

  z2ui5_cl_http_handler=>run( server ).

ENDMETHOD.
```
For custom configurations, simply populate the config structure and pass it to the run method:

### Theme
For example, to change the theme, the source code would look like this:
```abap
METHOD if_http_extension~handle_request.

  z2ui5_cl_http_handler=>run( server = server config = VALUE #(
    theme = `sap_belize`
  ) ).

ENDMETHOD.
``` 

### UI5 Bootstrapping
To specify the source for bootstrapping UI5:
```abap
METHOD if_http_extension~handle_request.

  z2ui5_cl_http_handler=>run( server = server config = VALUE #(
    src = `https://ui5.sap.com/1.116.0/resources/sap-ui-core.js`
  ) ).

ENDMETHOD.
```

### Title
To set a custom title for the application:
```abap
METHOD if_http_extension~handle_request.

  z2ui5_cl_http_handler=>run( server = server config = VALUE #(
    title = `My Title`
  ) ).

ENDMETHOD.
```

### Style / CSS
To apply custom styles or CSS, use the following:
```abap
METHOD if_http_extension~handle_request.

  z2ui5_cl_http_handler=>run( server = server config = VALUE #(
    styles_css =  `<< style definiiton here>>`
  ) ).

ENDMETHOD.
```

### Logon Language
To set the logon language, use the `sap-language` URL parameter. For more options via URL parameters, check out the documentation [here.](https://help.sap.com/doc/saphelp_nw75/7.5.5/de-DE/8b/46468c433b40c3b87b2e07f34dea1b/content.htm?no_cache=true)

### URL Parameters
You can read URL parameters using the following snippet:
```abap
METHOD z2ui5_if_app~main.

  DATA(lv_search) = client->get( )-s_config-search.s

ENDMETHOD.
```