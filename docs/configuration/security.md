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
To strengthen security, abap2UI5 uses a Content Security Policy (CSP) by default. CSP blocks attacks like cross-site scripting (XSS) and data injection by restricting which resources the browser can load. The default policy allows a fixed set of trusted sources — the SAP and OpenUI5 CDNs plus jsDelivr and cdnjs; the complete policy is shown below. It also carries hardening directives (`object-src 'none'`, `base-uri 'self'`, `frame-ancestors 'self'`) that block plugin content, pin `<base>` to the app origin and forbid cross-origin framing.

The default **does** contain `'unsafe-eval'`: the ui5loader of OpenUI5 `1.71` — the oldest supported release — still evaluates module source as a string, and without `'unsafe-eval'` a `1.71` bootstrap fails with a CSP `EvalError`. Modern UI5 releases load all modules without `eval()`, so if you pin a modern release you can tighten the policy — see [Hardening: Dropping `'unsafe-eval'`](#hardening-dropping-unsafe-eval) below.

#### Default CSP
By default, abap2UI5 uses the CSP below (defined in `z2ui5_cl_exit`):
```xml
<meta http-equiv="Content-Security-Policy" content="default-src 'self' 'unsafe-inline' 'unsafe-eval' data:
    ui5.sap.com *.ui5.sap.com sapui5.hana.ondemand.com *.sapui5.hana.ondemand.com openui5.hana.ondemand.com *.openui5.hana.ondemand.com
    sdk.openui5.org *.sdk.openui5.org cdn.jsdelivr.net *.cdn.jsdelivr.net cdnjs.cloudflare.com *.cdnjs.cloudflare.com schemas *.schemas;
    connect-src 'self' ui5.sap.com *.ui5.sap.com sapui5.hana.ondemand.com *.sapui5.hana.ondemand.com
    openui5.hana.ondemand.com *.openui5.hana.ondemand.com sdk.openui5.org *.sdk.openui5.org
    cdn.jsdelivr.net *.cdn.jsdelivr.net cdnjs.cloudflare.com *.cdnjs.cloudflare.com;
    worker-src 'self' blob:;
    object-src 'none'; base-uri 'self'; frame-ancestors 'self';"/>
```

#### Customizing the CSP
If needed, adjust the CSP in the [user exit](/advanced/extensibility/user_exits). The exit runs after the framework fills in the defaults, so whatever you set there overrides the default policy:

```abap
METHOD z2ui5_if_exit~set_config_http_get.

    cs_config-content_security_policy = `<meta http-equiv="Content-Security-Policy" content="default-src 'self' 'unsafe-inline' ui5.sap.com *.ui5.sap.com sdk.openui5.org *.sdk.openui5.org cdn.jsdelivr.net *.cdn.jsdelivr.net"/>`.

ENDMETHOD.
```

#### Hardening: Dropping `'unsafe-eval'`
`'unsafe-eval'` weakens the protection CSP provides against script injection. The default keeps it only because OpenUI5 `1.71` — the oldest supported release — still executes fetched modules via `eval()` in its module loader. If you pin a modern UI5 release, no `eval()` is involved and you can remove `'unsafe-eval'` in the same exit where you set the bootstrap source. The example below is the default policy without `'unsafe-eval'`:

```abap
METHOD z2ui5_if_exit~set_config_http_get.

    cs_config-src   = `https://ui5.sap.com/resources/sap-ui-core.js`.
    cs_config-theme = `sap_horizon`.

    " modern UI5 loads modules without eval() - drop 'unsafe-eval'
    cs_config-content_security_policy =
      |<meta http-equiv="Content-Security-Policy" | &&
      |content="default-src 'self' 'unsafe-inline' data: | &&
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
      |worker-src 'self' blob:; | &&
      |object-src 'none'; base-uri 'self'; frame-ancestors 'self'; "/>|.

ENDMETHOD.
```

::: warning
With `'unsafe-eval'` removed, bootstrapping an old release such as `1.71` fails: the page loads, but the component cannot start and the browser console shows an error like

```
Failed to load component for container container. Reason: EvalError: Evaluating a string as
JavaScript violates the following Content Security Policy directive because 'unsafe-eval' is
not an allowed source of script: default-src 'self' 'unsafe-inline' data: ui5.sap.com ...
```

Only tighten the policy when every system you deploy to bootstraps a modern release.
:::

### Cross-Site Request Forgery (CSRF)
Every state-changing request in abap2UI5 is a POST, so the framework ships its own CSRF defense instead of relying on a fronting SAP ICF/CSRF layer that may or may not be there. The check compares the host authority of the request's `Origin` (or `Referer`) header against the `Host` header — a cross-origin POST is rejected with an error response before any app logic runs.

**CSRF protection is active by default.** A fresh install rejects cross-origin POSTs without any configuration. If your endpoint must accept cross-origin POSTs (for example, behind a proxy setup where the origin legitimately differs), opt out in the [user exit](/advanced/extensibility/user_exits):

```abap
METHOD z2ui5_if_exit~set_config_http_post.

    " escape hatch - only disable this if your endpoint must accept cross-origin POSTs
    cs_config-check_csrf_active = abap_false.

ENDMETHOD.
```
