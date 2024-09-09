## HTTP Handler Configuration

abap2UI5 can be run with various custom configurations. This is the call for the default setup:
```abap
  METHOD if_http_extension~handle_request.

    server->response->set_cdata( z2ui5_cl_http_handler=>main( server->request->get_cdata( ) ) ).
    server->response->set_header_field( name = `cache-control` value = `no-cache` ).
    server->response->set_status( code = 200 reason = `success` ).

  ENDMETHOD.
```
For custom configurations, simply modify the method call as follow:

#### Theme
```abap
    DATA(s_config) = VALUE z2ui5_if_types=>ty_s_http_request_get(
        t_config = VALUE #(
            (  n = `data-sap-ui-theme` v = `sap_belize` ) ) ).

    DATA(response) = z2ui5_cl_http_handler=>main(
        body   = server->request->get_cdata( )
        config = s_config ).

``` 

#### UI5 Bootstrapping

```abap
    DATA(s_config) = VALUE z2ui5_if_types=>ty_s_http_request_get(
        t_config = VALUE #(
            (  n = `src` v = `https://ui5.sap.com/1.116.0/resources/sap-ui-core.js` ) ) ).

    DATA(response) = z2ui5_cl_http_handler=>main(
        body   = server->request->get_cdata( )
        config = s_config ).

```

#### Custom JS

```abap
    DATA(s_config) = VALUE z2ui5_if_types=>ty_s_http_request_get(
        custom_js = z2ui5add_cl_cc_websocket=>get_js( ) ).

    DATA(response) = z2ui5_cl_http_handler=>main(
        body   = server->request->get_cdata( )
        config = s_config ).

```

#### Content-Security-Policy

```abap
    DATA(s_config) = VALUE z2ui5_if_types=>ty_s_http_request_get(
        content_security_policy = ```<meta http-equiv="Content-Security-Policy" content="default-src 'self' 'unsafe-inline' 'unsafe-eval' data: ` && |\n|  &&
                                  `   ``ui5.sap.com *.ui5.sap.com sapui5.hana.ondemand.com *.sapui5.hana.ondemand.com ` && |\n|  &&
                                  `   ``sdk.openui5.org *.sdk.openui5.org "/>` ).

    DATA(response) = z2ui5_cl_http_handler=>main(
        body   = server->request->get_cdata( )
        config = s_config ).

```
#### URL Parameter
Further configuration can be done by changing the URL [**here.**](https://help.sap.com/doc/saphelp_nw75/7.5.5/de-DE/8b/46468c433b40c3b87b2e07f34dea1b/content.htm?no_cache=true)
