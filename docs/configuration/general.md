---
outline: [2, 4]
---

# General

### HTTP Handler

abap2UI5 can be run with various custom configurations. This is the call for the default setup:
```abap
  METHOD if_http_extension~handle_request.

    z2ui5_cl_http_handler=>factory( server )->main( ).

  ENDMETHOD.
```
For custom configurations, simply create a config variable and import it to the main method:

### Theme
eg. for changing the theme the source code looks like this:
```abap
  METHOD if_http_extension~handle_request.

    DATA(lo_server) = z2ui5_cl_http_handler=>factory( server ).
    lo_server->main( VALUE #(
        theme = `sap_belize`
    ) ).

  ENDMETHOD.
``` 

### UI5 Bootstrapping

```abap
    DATA(lo_server) = z2ui5_cl_http_handler=>factory( server ).
    lo_server->main( VALUE #(
        src = `https://ui5.sap.com/1.116.0/resources/sap-ui-core.js`
    ) ).
```

### Title

```abap
    DATA(lo_server) = z2ui5_cl_http_handler=>factory( server ).
    lo_server->main( VALUE #(
        title = `My Title`
    ) ).
```

### Style / CSS

```abap
    DATA(lo_server) = z2ui5_cl_http_handler=>factory( server ).
    lo_server->main( VALUE #(
        styles_css =  `<< style definiiton here>>`
    ) ).
```

## URL Parameter
Further configuration can be done by changing the URL [**here.**](https://help.sap.com/doc/saphelp_nw75/7.5.5/de-DE/8b/46468c433b40c3b87b2e07f34dea1b/content.htm?no_cache=true)
