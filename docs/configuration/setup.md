---
outline: [2, 4]
---

# Setup

abap2UI5 can be run with various custom configurations. This is the call for the default setup:
```abap
METHOD if_http_extension~handle_request.

  z2ui5_cl_http_handler=>run( server ).

ENDMETHOD.
```
For custom configurations, simply fill the config structure and import it to the run method:

### Theme
eg. for changing the theme the source code looks like this:
```abap
METHOD if_http_extension~handle_request.

  z2ui5_cl_http_handler=>run( server = server config = VALUE #(
    theme = `sap_belize`
  ) ).

ENDMETHOD.
``` 

### UI5 Bootstrapping

```abap
METHOD if_http_extension~handle_request.

  z2ui5_cl_http_handler=>run( server = server config = VALUE #(
    src = `https://ui5.sap.com/1.116.0/resources/sap-ui-core.js`
  ) ).

ENDMETHOD.
```

### Title

```abap
METHOD if_http_extension~handle_request.

  z2ui5_cl_http_handler=>run( server = server config = VALUE #(
    title = `My Title`
  ) ).

ENDMETHOD.
```

### Style / CSS

```abap
METHOD if_http_extension~handle_request.

  z2ui5_cl_http_handler=>run( server = server config = VALUE #(
    styles_css =  `<< style definiiton here>>`
  ) ).

ENDMETHOD.
```

### URL Parameters
Use the following snippet to read URL parameters:
```abap
METHOD z2ui5_if_app~main.

  DATA(lv_search) = client->get( )-s_config-search.s

ENDMETHOD.
```
You also have the option to do further configuration, check it out [**here.**](https://help.sap.com/doc/saphelp_nw75/7.5.5/de-DE/8b/46468c433b40c3b87b2e07f34dea1b/content.htm?no_cache=true)


### Logon Language
Set the url parameter sap-language for this and check all other options [here.](https://help.sap.com/doc/saphelp_nw75/7.5.5/de-DE/8b/46468c433b40c3b87b2e07f34dea1b/content.htm?no_cache=true)