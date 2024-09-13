## HTTP Handler Configuration

abap2UI5 can be run with various custom configurations. This is the call for the default setup:
```abap
  METHOD if_http_extension~handle_request.

    server->response->set_cdata( z2ui5_cl_http_handler=>main( server->request->get_cdata( ) ) ).
    server->response->set_header_field( name = `cache-control` value = `no-cache` ).
    server->response->set_status( code = 200 reason = `success` ).

  ENDMETHOD.
```
For custom configurations, simply create a config variable and import it to the main method:

#### Theme
eg. for changing the theme the source code looks like this:
```abap
  METHOD if_http_extension~handle_request.

    DATA(s_config) = VALUE z2ui5_if_types=>ty_s_http_request_get(
        t_config = VALUE #(
            (  n = `data-sap-ui-theme` v = `sap_belize` ) ) ).

   server->response->set_cdata( z2ui5_cl_http_handler=>main(
        body   = server->request->get_cdata( )
        config = s_config ) ).

    server->response->set_header_field( name = `cache-control` value = `no-cache` ).
    server->response->set_status( code = 200 reason = `success` ).

  ENDMETHOD.
``` 

#### UI5 Bootstrapping

```abap
    DATA(s_config) = VALUE z2ui5_if_types=>ty_s_http_request_get(
        t_config = VALUE #(
            (  n = `src` v = `https://ui5.sap.com/1.116.0/resources/sap-ui-core.js` ) ) ).

```

#### Custom JS
Use this for example to install additional custom controls [here:](https://github.com/abap2UI5-addons/custom-controls)
```abap
    DATA(s_config) = VALUE z2ui5_if_types=>ty_s_http_request_get(
        custom_js = z2ui5add_cl_cc_websocket=>get_js( ) ).

```

#### Content-Security-Policy

```abap
    DATA(s_config) = VALUE z2ui5_if_types=>ty_s_http_request_get(
        content_security_policy = ` <meta http-equiv="Content-Security-Policy" content="default-src 'self' 'unsafe-inline' 'unsafe-eval' data: ` && |\n|  &&
                                  `   ui5.sap.com *.ui5.sap.com sapui5.hana.ondemand.com *.sapui5.hana.ondemand.com ` && |\n|  &&
                                  `   sdk.openui5.org *.sdk.openui5.org "/>` ).

```

#### Title

```abap
    DATA(s_config) = VALUE z2ui5_if_types=>ty_s_http_request_get(
        t_param = VALUE #(
            (  n = `TITLE` v = `My custom Title` ) ) ).

```

#### Style / CSS

```abap
    DATA(s_config) = VALUE z2ui5_if_types=>ty_s_http_request_get(
        t_param = VALUE #(
            (  n = `STYLE` v = `<< style definiiton here>>` ) ) ).

```

#### Class of the HTML Body

```abap
    DATA(s_config) = VALUE z2ui5_if_types=>ty_s_http_request_get(
        t_param = VALUE #(
           (  n = `BODY_CLASS`   v = `sapUiBody`   )

```

#### Default
If nothing is imported the following default values are used:
```abap
    DATA(lv_csp)  = `<meta http-equiv="Content-Security-Policy" content="default-src 'self' 'unsafe-inline' 'unsafe-eval' data: ` &&
   `ui5.sap.com *.ui5.sap.com sapui5.hana.ondemand.com *.sapui5.hana.ondemand.com openui5.hana.ondemand.com *.openui5.hana.ondemand.com ` &&
   `sdk.openui5.org *.sdk.openui5.org cdn.jsdelivr.net *.cdn.jsdelivr.net cdnjs.cloudflare.com *.cdnjs.cloudflare.com schemas *.schemas"/>`.

    data(lv_style) =  `        html, body, body > div, #container, #container-uiarea {` && |\n| &&
               `            height: 100%;` && |\n| &&
               `        }` && |\n| &&
               `        .dbg-ltr {` && |\n| &&
               `            direction: ltr !important;` && |\n| &&
               `        }`.

    result = VALUE #(
        t_param = VALUE #(
            (  n = `TITLE`                   v = `abap2UI5` )
            (  n = `STYLE`                   v =  lv_style )
            (  n = `BODY_CLASS`              v = `sapUiBody sapUiSizeCompact`   )
            )
        t_config = VALUE #(
            (  n = `src`                       v = `https://sdk.openui5.org/resources/sap-ui-cachebuster/sap-ui-core.js` )
            (  n = `data-sap-ui-theme`         v = `sap_horizon` )
            (  n = `data-sap-ui-async`         v = `true` )
            (  n = `id`                        v = `sap-ui-bootstrap` )
            (  n = `data-sap-ui-bindingSyntax` v = `complex` )
            (   n = `data-sap-ui-frameOptions`  v = `trusted` )
            (  n = `data-sap-ui-compatVersion` v = `edge` ) )
        content_security_policy = lv_csp ).

```

#### URL Parameter
Further configuration can be done by changing the URL [**here.**](https://help.sap.com/doc/saphelp_nw75/7.5.5/de-DE/8b/46468c433b40c3b87b2e07f34dea1b/content.htm?no_cache=true)
