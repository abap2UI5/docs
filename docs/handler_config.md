## HTTP Handler Configuration

abap2UI5 can be run with various custom configurations. This is the call for the default setup:
```abap
  METHOD if_http_extension~handle_request.

    z2ui5_cl_http_handler=>factory( server )->main( ).

  ENDMETHOD.
```
For custom configurations, simply create a config variable and import it to the main method:

#### Theme
eg. for changing the theme the source code looks like this:
```abap
  METHOD if_http_extension~handle_request.

    DATA(lo_server) = z2ui5_cl_http_handler=>factory( server ).
    lo_server->main( VALUE #(
        theme = `sap_belize`
    ) ).

  ENDMETHOD.
``` 

#### UI5 Bootstrapping

```abap
    DATA(lo_server) = z2ui5_cl_http_handler=>factory( server ).
    lo_server->main( VALUE #(
        src = `https://ui5.sap.com/1.116.0/resources/sap-ui-core.js`
    ) ).
```

#### Custom JS
Use this for example to install additional custom controls [here:](https://github.com/abap2UI5-addons/custom-controls)
```abap
    DATA(lo_server) = z2ui5_cl_http_handler=>factory( server ).
    lo_server->main( VALUE #(
        custom_js = z2ui5add_cl_cc_websocket=>get_js( ) 
    ) ).
```

#### Content-Security-Policy

```abap
    DATA(lo_server) = z2ui5_cl_http_handler=>factory( server ).
    lo_server->main( VALUE #(
        content_security_policy = ` <meta http-equiv="Content-Security-Policy" content="default-src 'self' 'unsafe-inline' 'unsafe-eval' data: ` && |\n|  &&
                                  `   ui5.sap.com *.ui5.sap.com sapui5.hana.ondemand.com *.sapui5.hana.ondemand.com ` && |\n|  &&
                                  `   sdk.openui5.org *.sdk.openui5.org "/>`
    ) ).
```

#### Title

```abap
    DATA(lo_server) = z2ui5_cl_http_handler=>factory( server ).
    lo_server->main( VALUE #(
        title = `My Title`
    ) ).
```

#### Style / CSS

```abap
    DATA(lo_server) = z2ui5_cl_http_handler=>factory( server ).
    lo_server->main( VALUE #(
        styles_css =  `<< style definiiton here>>`
    ) ).
```

#### URL Parameter
Further configuration can be done by changing the URL [**here.**](https://help.sap.com/doc/saphelp_nw75/7.5.5/de-DE/8b/46468c433b40c3b87b2e07f34dea1b/content.htm?no_cache=true)
