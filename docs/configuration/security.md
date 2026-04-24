---
outline: [2, 4]
---
# Security
abap2UI5 is a backend-centric framework. All logic and business data remain on the server; the frontend receives only the data it needs to render the view.

### HTTP Endpoint
The abap2UI5 framework runs as an HTTP handler. You create the HTTP handler and call the abap2UI5 API inside it. End users access abap2UI5 by calling the endpoint externally, with security managed as in any other UI5 application.

### Authentication
The ICF (Internet Communication Framework) node level handles authentication. You have full control over configuring the ICF node, including visibility settings, login procedures, and other security parameters.

### Authorization
As an app developer, you have full flexibility over authorization settings. Configure them at either the application level or the service node level. For details on setting up authorization for your endpoint, see the [Authorization](/configuration/authorization) page.

### Backend Code
abap2UI5 ships as custom code. Once installed, you own the code in full and can modify it to suit your needs. To preserve compatibility with future updates, however, avoid direct modifications to the core codebase.

### Frontend Code
The frontend is a Single-Page Application (SPA) built with SAPUI5 or OpenUI5. The HTTP endpoint delivers it on the first request, following best practices for modern web applications.

### Business Logic
abap2UI5 never sends the app's business logic to the client. All business processes remain securely on the server, so sensitive data never reaches the frontend.

### Content-Security-Policy
To strengthen security, abap2UI5 uses a Content Security Policy (CSP) by default. CSP blocks attacks like cross-site scripting (XSS) and data injection by restricting which resources the browser can load. By default, CSP allows only trusted sources such as ui5.sap.com, sapui5.hana.ondemand.com, and sdk.openui5.org, among others.

#### Default CSP
By default, abap2UI5 uses the CSP below:
```xml
<meta http-equiv="Content-Security-Policy" content="default-src 'self' 'unsafe-inline' 'unsafe-eval' data:
    ui5.sap.com *.ui5.sap.com sapui5.hana.ondemand.com *.sapui5.hana.ondemand.com openui5.hana.ondemand.com *.openui5.hana.ondemand.com
    sdk.openui5.org *.sdk.openui5.org cdn.jsdelivr.net *.cdn.jsdelivr.net cdnjs.cloudflare.com *.cdnjs.cloudflare.com schemas *.schemas"/>
```

#### Customizing the CSP
If needed, adjust the CSP by modifying the HTTP handler call:

```abap
METHOD z2ui5_if_exit~set_config_http_get.

    cs_config-content_security_policy = `<meta http-equiv="Content-Security-Policy" content="default-src 'self' 'unsafe-inline' 'unsafe-eval' ui5.sap.com *.ui5.sap.com sdk.openui5.org *.sdk.openui5.org cdn.jsdelivr.net *.cdn.jsdelivr.net"/>`.

ENDMETHOD.
```
