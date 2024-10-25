# Security

### HTTP Endpoint
The framework is implemented as an HTTP handler. You create the HTTP handler and invoke the abap2UI5 API within it. End users can access abap2UI5 by calling the endpoint defined in your HTTP handler. Security is managed in the same way as with any other UI5 application.

### Authorization
Authorization is managed at the ICF (Internet Communication Framework) node level. You have all options to maintain the of your icf node, changing isibilty , login procedure etc.

### Authentication
Authentication is managed by the app developer. You have various options, on app level, node level etc. chout out more information here. You can configure which users or systems have access to your endpoint, ensuring that only authorized parties can interact with the framework [here.](/configuration/authorization)

### Backend Code
abap2UI5 is delivered as Z-Code. Once installed, you "own" the code and can either use it as-is or modify it as necessary. However, to benefit from future updates, it is recommended not to alter the main code line directly.

### Frontend Code
The frontend of the application is a Single Page Application (SPA) built using SAPUI5 or OpenUI5. The application is delivered from the HTTP endpoint upon the first request, following standard practices for modern web apps.

### Content-Security-Policy
To enhance security, abap2UI5 uses a Content Security Policy (CSP) by default. CSP helps prevent attacks like cross-site scripting (XSS) and data injection by restricting which resources the browser is allowed to load.

#### Default CSP:
By default, abap2UI5's CSP is configured to allow only trusted sources such as ui5.sap.com, sapui5.hana.ondemand.com, and sdk.openui5.org, among others.

Per default abap2UI5 uses the following CSP:
```xml
 <meta http-equiv="Content-Security-Policy" content="default-src 'self' 'unsafe-inline' 'unsafe-eval' data:
     ui5.sap.com *.ui5.sap.com sapui5.hana.ondemand.com *.sapui5.hana.ondemand.com openui5.hana.ondemand.com *.openui5.hana.ondemand.com
     sdk.openui5.org *.sdk.openui5.org cdn.jsdelivr.net *.cdn.jsdelivr.net cdnjs.cloudflare.com *.cdnjs.cloudflare.com schemas *.schemas"/>
```

#### Customizing the CSP:
You have the option to adjust the CSP if needed. This can be done by modifying the HTTP handler call as shown below:

```abap
    DATA(lo_server) = z2ui5_cl_http_handler=>run( VALUE #(
        content_security_policy = ` <meta http-equiv="Content-Security-Policy" content="default-src 'self' 'unsafe-inline' 'unsafe-eval' data: ` && |\n|  &&
                                  `   ui5.sap.com *.ui5.sap.com sapui5.hana.ondemand.com *.sapui5.hana.ondemand.com ` && |\n|  &&
                                  `   sdk.openui5.org *.sdk.openui5.org "/>`
    ) ).
```