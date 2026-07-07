---
outline: [2, 4]
---
# Security
abap2UI5 is a backend-centric framework. All logic and business data stay on the server; the frontend gets only the data it needs to render the view.

### HTTP Endpoint
The abap2UI5 framework runs as an HTTP handler. You build the HTTP handler and call the abap2UI5 API inside it. Users access abap2UI5 by calling the endpoint externally, with security managed like any other UI5 app.

### Authentication
The ICF (Internet Communication Framework) node level handles authentication. You get full control over the ICF node configuration, including visibility settings, login procedures, and other security settings.

### Authorization
As an app developer, you have full flexibility over authorization. Set it up at either the app level or the service node level. For details on setting up authorization for your endpoint, see the [Authorization](/configuration/authorization) page.

### Backend Code
abap2UI5 ships as custom code. Once installed, you own the code in full and can change it as needed. To stay compatible with future updates, avoid direct changes to the core codebase.

### Frontend Code
The frontend is a Single-Page Application (SPA) built with SAPUI5 or OpenUI5. The HTTP endpoint delivers it on the first request, in line with standard practices for modern web apps.

### Business Logic
abap2UI5 never sends the app's business logic to the client. All business processes stay safely on the server, and sensitive data never reaches the frontend.

### Content-Security-Policy
To strengthen security, abap2UI5 uses a Content Security Policy (CSP) by default. CSP blocks attacks like cross-site scripting (XSS) and data injection by restricting which resources the browser can load. The default policy allows a fixed set of trusted sources — the SAP and OpenUI5 CDNs plus jsDelivr and cdnjs; the complete policy is shown below. It deliberately does **not** contain `'unsafe-eval'`: current UI5 releases load all modules in a CSP-compliant way, so allowing `eval()` is unnecessary. If you bootstrap an older UI5 release, read [Bootstrapping Older UI5 Releases](#bootstrapping-older-ui5-releases) below.

#### Default CSP
By default, abap2UI5 uses the CSP below (defined in `z2ui5_cl_exit`):
```xml
<meta http-equiv="Content-Security-Policy" content="default-src 'self' 'unsafe-inline' data:
    ui5.sap.com *.ui5.sap.com sapui5.hana.ondemand.com *.sapui5.hana.ondemand.com openui5.hana.ondemand.com *.openui5.hana.ondemand.com
    sdk.openui5.org *.sdk.openui5.org cdn.jsdelivr.net *.cdn.jsdelivr.net cdnjs.cloudflare.com *.cdnjs.cloudflare.com schemas *.schemas;
    connect-src 'self' ui5.sap.com *.ui5.sap.com sapui5.hana.ondemand.com *.sapui5.hana.ondemand.com
    openui5.hana.ondemand.com *.openui5.hana.ondemand.com sdk.openui5.org *.sdk.openui5.org
    cdn.jsdelivr.net *.cdn.jsdelivr.net cdnjs.cloudflare.com *.cdnjs.cloudflare.com;
    worker-src 'self' blob:;"/>
```

#### Customizing the CSP
If needed, adjust the CSP in the [user exit](/advanced/extensibility/user_exits). The exit runs after the framework fills in the defaults, so whatever you set there overrides the default policy:

```abap
METHOD z2ui5_if_exit~set_config_http_get.

    cs_config-content_security_policy = `<meta http-equiv="Content-Security-Policy" content="default-src 'self' 'unsafe-inline' ui5.sap.com *.ui5.sap.com sdk.openui5.org *.sdk.openui5.org cdn.jsdelivr.net *.cdn.jsdelivr.net"/>`.

ENDMETHOD.
```

#### Bootstrapping Older UI5 Releases
Older UI5 releases (for example `1.71`, the oldest supported version) still execute fetched modules via `eval()` in their module loader. The default CSP blocks `eval()`, so bootstrapping such a release fails: the page loads, but the component cannot start and the browser console shows an error like

```
Failed to load component for container container. Reason: EvalError: Evaluating a string as
JavaScript violates the following Content Security Policy directive because 'unsafe-eval' is
not an allowed source of script: default-src 'self' 'unsafe-inline' data: ui5.sap.com ...
```

Newer UI5 releases load modules without `eval()` and work with the default CSP out of the box — this error only appears with old releases.

To bootstrap an older release, override the CSP in the same exit where you set the bootstrap source and re-add `'unsafe-eval'` to `default-src` (or `script-src`). The example below is the default policy with only `'unsafe-eval'` added:

```abap
METHOD z2ui5_if_exit~set_config_http_get.

    cs_config-src   = `https://ui5.sap.com/1.71/resources/sap-ui-core.js`.
    cs_config-theme = `sap_fiori_3`.

    " old UI5 releases still execute modules via eval() - re-add 'unsafe-eval'
    cs_config-content_security_policy =
      |<meta http-equiv="Content-Security-Policy" | &&
      |content="default-src 'self' 'unsafe-inline' 'unsafe-eval' data: | &&
      |ui5.sap.com *.ui5.sap.com | &&
      |sapui5.hana.ondemand.com *.sapui5.hana.ondemand.com | &&
      |openui5.hana.ondemand.com *.openui5.hana.ondemand.com | &&
      |sdk.openui5.org *.sdk.openui5.org | &&
      |cdn.jsdelivr.net *.cdn.jsdelivr.net | &&
      |cdnjs.cloudflare.com *.cdnjs.cloudflare.com schemas *.schemas; | &&
      |connect-src 'self' | &&
      |  ui5.sap.com *.ui5.sap.com | &&
      |  sapui5.hana.ondemand.com *.sapui5.hana.ondemand.com | &&
      |  openui5.hana.ondemand.com *.openui5.hana.ondemand.com | &&
      |  sdk.openui5.org *.sdk.openui5.org | &&
      |  cdn.jsdelivr.net *.cdn.jsdelivr.net | &&
      |  cdnjs.cloudflare.com *.cdnjs.cloudflare.com; | &&
      |worker-src 'self' blob:; "/>|.

ENDMETHOD.
```

::: warning
`'unsafe-eval'` weakens the protection CSP provides against cross-site scripting. Only add it when you must run a UI5 release that needs it, and remove it again once you upgrade to a newer release.
:::
