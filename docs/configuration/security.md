# Security & Authorization



PICTURE with 

## Content-Security-Policy

```abap
    DATA(lo_server) = z2ui5_cl_http_handler=>factory( server ).
    lo_server->main( VALUE #(
        content_security_policy = ` <meta http-equiv="Content-Security-Policy" content="default-src 'self' 'unsafe-inline' 'unsafe-eval' data: ` && |\n|  &&
                                  `   ui5.sap.com *.ui5.sap.com sapui5.hana.ondemand.com *.sapui5.hana.ondemand.com ` && |\n|  &&
                                  `   sdk.openui5.org *.sdk.openui5.org "/>`
    ) ).
```


