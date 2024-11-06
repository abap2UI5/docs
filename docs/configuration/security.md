---
outline: [2, 3]
---

# Security

### HTTP Endpoint
The abap2UI5 framework operates as an HTTP handler. You create this HTTP handler and call the abap2UI5 API within it. End users can access abap2UI5 by calling the endpoint defined in your HTTP handler, with security managed similarly to any other UI5 application.

### Authorization
Authorization is managed at the ICF (Internet Communication Framework) node level. You have full control to configure your ICF node by adjusting visibility, login procedures, and other authorization settings.

### Authentication
Authentication settings are customizable by the app developer and can be managed at the application level or node level. For more information on configuring authentication for your endpoint, including user and system access control, refer to the details [here.](/configuration/authorization)

### Backend Code
abap2UI5 is delivered as Z-Code. Once installed, you "own" the code, allowing you to use or modify it as necessary. However, to benefit from future updates, it is recommended not to directly alter the main code line.

### Frontend Code
The frontend of the application is a Single Page Application (SPA) built using SAPUI5 or OpenUI5. It is delivered from the HTTP endpoint upon the first request, following best practices for modern web applications.

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