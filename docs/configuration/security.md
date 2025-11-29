---
outline: [2, 3]
---

# Security
abap2UI5 is a backend-centric framework. All logic and business data remain securely on the backend, and the frontend only receives the data it needs to render the view. This approach helps maintain security while providing efficient functionality.

### HTTP Endpoint
The abap2UI5 framework operates as an HTTP handler. You create this HTTP handler and call the abap2UI5 API within it. End users can access abap2UI5 by calling the endpoint externally, with security managed similarly to any other UI5 application.

### Authentication 
Authorization is handled at the ICF (Internet Communication Framework) node level. You have complete control over configuring the ICF node, including visibility settings, login procedures, and other security parameters.

### Authorization
As an app developer, you have full flexibility in managing authorization settings. You can configure these either at the application level or service node level. For detailed guidance on setting up authentication for your endpoint, check out [this page.](/configuration/authorization)

### Backend Code
abap2UI5 is delivered as custom code. Once installed, you have full ownership of the code, giving you the flexibility to modify it to suit your needs. However, to ensure compatibility with future updates, we recommend avoiding direct modifications to the core code base.

### Frontend Code
The frontend of the application is a Single Page Application (SPA) built using SAPUI5 or OpenUI5. It's delivered from the HTTP endpoint upon the first request, adhering to best practices for modern web applications. 

### Business Logic
The business logic of the abap2UI5 app is not sent to the client. All business-related processes remain securely on the server, ensuring sensitive data is never exposed on the frontend.

### Content-Security-Policy
To enhance security, abap2UI5 uses a Content Security Policy (CSP) by default. CSP helps prevent attacks like cross-site scripting (XSS) and data injection by restricting which resources the browser can load. CSP is configured to allow only trusted sources such as ui5.sap.com, sapui5.hana.ondemand.com, and sdk.openui5.org, among others.

#### Default CSP:
By default, abap2UI5 uses the following CSP:
```xml
 <meta http-equiv="Content-Security-Policy" content="default-src 'self' 'unsafe-inline' 'unsafe-eval' data:
     ui5.sap.com *.ui5.sap.com sapui5.hana.ondemand.com *.sapui5.hana.ondemand.com openui5.hana.ondemand.com *.openui5.hana.ondemand.com
     sdk.openui5.org *.sdk.openui5.org cdn.jsdelivr.net *.cdn.jsdelivr.net cdnjs.cloudflare.com *.cdnjs.cloudflare.com schemas *.schemas"/>
```

#### Customizing the CSP:
You have the option to adjust the CSP if needed. This can be done by modifying the HTTP handler call as shown below:

```abap
METHOD z2ui5_if_exit~set_config_http_get.

    cs_config-content_security_policy = `<meta http-equiv="Content-Security-Policy" content="default-src 'self' 'unsafe-inline' 'unsafe-eval' ui5.sap.com *.ui5.sap.com sdk.openui5.org *.sdk.openui5.org cdn.jsdelivr.net *.cdn.jsdelivr.net"/>`.

ENDMETHOD.
```
