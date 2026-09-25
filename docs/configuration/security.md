---
outline: [2, 4]
description: How abap2UI5 keeps data on the server - one HTTP endpoint, SAP authentication and authorization, the Content-Security-Policy, response headers and CSRF.
---
# Security
abap2UI5 is a backend-centric framework. All logic and business data stay on the server; the frontend gets only the data it needs to render the view.

## HTTP Endpoint
The abap2UI5 framework runs as an HTTP handler. You build the HTTP handler and call the abap2UI5 API inside it. Users access abap2UI5 by calling the endpoint externally, with security managed like any other UI5 app.

## Authentication
The ICF (Internet Communication Framework) node level handles authentication. You get full control over the ICF node configuration, including visibility settings, login procedures, and other security settings.

## Authorization
As an app developer, you have full flexibility over authorization. Set it up at either the app level or the service node level. For details on setting up authorization for your endpoint, see the [Authorization](/configuration/authorization) page.

## Backend Code
abap2UI5 ships as custom code. Once installed, you own the code in full and can change it as needed. To stay compatible with future updates, avoid direct changes to the core codebase.

## Frontend Code
The frontend is a Single-Page Application (SPA) built with SAPUI5 or OpenUI5. The HTTP endpoint delivers it on the first request, in line with standard practices for modern web apps.

## Business Logic
abap2UI5 never sends the app's business logic to the client. All business processes stay safely on the server, and sensitive data never reaches the frontend.

## Content-Security-Policy
To strengthen security, abap2UI5 uses a Content Security Policy (CSP) by default. CSP blocks attacks like cross-site scripting (XSS) and data injection by restricting which resources the browser can load. The default policy allows a fixed set of trusted sources — the SAP and OpenUI5 CDN hosts, and nothing else; an installation that loads from another host adds it in the user exit, to the one directive that needs it. The complete policy is shown below. It also carries two hardening directives — `object-src 'none'` and `base-uri 'self'` — that block plugin content and pin `<base>` to the app origin. It deliberately carries **no** `frame-ancestors`: browsers ignore that directive in a `<meta>` CSP (and log a console warning about it), so cross-origin framing is forbidden by the real `X-Frame-Options` response header instead — see [Response headers](#response-headers) below.

The default carries **no** `'unsafe-eval'` and no `'unsafe-inline'` for scripts: nothing abap2UI5 ships evaluates code, and UI5 from `1.84` on loads its modules without `eval()`. The page's one inline script is allowed by its SHA-256 hash, which the framework appends to `script-src` after the exit ran, so an injected `<script>`, an `onerror=` attribute or a `javascript:` URL is refused by the browser. Only a popup on UI5 `1.71` to `1.82` can still need `'unsafe-eval'` — see [Older releases](#older-releases-switching-unsafe-eval-on) below.

### Default CSP
By default, abap2UI5 uses the CSP below (defined in `z2ui5_cl_ui5_user_exit`; the framework appends the hash of the page's inline script to `script-src` afterwards):
```xml
<meta http-equiv="Content-Security-Policy" content="default-src 'self' data: blob:
    ui5.sap.com *.ui5.sap.com sapui5.hana.ondemand.com *.sapui5.hana.ondemand.com
    openui5.hana.ondemand.com *.openui5.hana.ondemand.com sdk.openui5.org *.sdk.openui5.org schemas *.schemas;
    script-src 'self' ui5.sap.com *.ui5.sap.com sapui5.hana.ondemand.com *.sapui5.hana.ondemand.com
    openui5.hana.ondemand.com *.openui5.hana.ondemand.com sdk.openui5.org *.sdk.openui5.org;
    style-src 'self' 'unsafe-inline' ui5.sap.com *.ui5.sap.com sapui5.hana.ondemand.com *.sapui5.hana.ondemand.com
    openui5.hana.ondemand.com *.openui5.hana.ondemand.com sdk.openui5.org *.sdk.openui5.org;
    connect-src 'self' ui5.sap.com *.ui5.sap.com sapui5.hana.ondemand.com *.sapui5.hana.ondemand.com
    openui5.hana.ondemand.com *.openui5.hana.ondemand.com sdk.openui5.org *.sdk.openui5.org;
    worker-src 'self' blob:;
    object-src 'none'; base-uri 'self';"/>
```

`script-src` and `style-src` are written out rather than left to the `default-src` fallback on purpose: `default-src` carries `data:` and `blob:` for images, fonts and media, and a `data:` that falls through to `script-src` is a textbook CSP bypass. `style-src` keeps `'unsafe-inline'` because UI5 renders style attributes itself.

### Customizing the CSP
If needed, adjust the CSP in the [user exit](/advanced/extensibility/user_exits). The exit runs after the framework fills in the default, so edit the directive you need rather than replacing the whole tag — a copy of the tag goes stale with the next release, an edit does not:

```abap
METHOD z2ui5_if_ui5_exit~set_config_http_get.

    " an installation that loads a library from another host
    REPLACE `script-src 'self'` IN cs_config-content_security_policy
       WITH `script-src 'self' cdn.example.com`.
    REPLACE `connect-src 'self'` IN cs_config-content_security_policy
       WITH `connect-src 'self' cdn.example.com`.

ENDMETHOD.
```

A `script-src` that names `'unsafe-inline'` itself is left without the hash of the page's inline script — `'unsafe-inline'` would be ignored beside a hash anyway.

### Older releases: switching `'unsafe-eval'` on
On UI5 `1.71` to `1.82` a popup is processed synchronously, and a module that the popup's XML only names in a binding type or a `core:require` is fetched and evaluated as a string on the spot. The frontend loads the popup's controls asynchronously first, so the shipped popups stay clean — but a module it cannot see from the XML still needs `'unsafe-eval'` on those releases. Such an installation switches it on in the same exit, for `script-src` only:

```abap
METHOD z2ui5_if_ui5_exit~set_config_http_get.

    cs_config-src   = `https://ui5.sap.com/1.71/resources/sap-ui-core.js`.

    " UI5 1.71 to 1.82: a popup that names a module it has not loaded
    " evaluates it as a string
    REPLACE `script-src 'self'` IN cs_config-content_security_policy
       WITH `script-src 'self' 'unsafe-eval'`.

ENDMETHOD.
```

::: warning
The symptom without it is a popup that does not open, and in the browser console an error like

```
EvalError: Evaluating a string as JavaScript violates the following Content
Security Policy directive because 'unsafe-eval' is not an allowed source of
script: script-src 'self' ui5.sap.com ...
```

From `1.84` on nothing needs it; leave the default alone there.
:::

## Response headers
Not everything hardening can be done in a `<meta>` CSP is done there — a page
served over HTTP carries headers too, and some directives only work as one.
abap2UI5 sets these on every response, out of the box:

| Header | Value | What it does |
|---|---|---|
| `X-Frame-Options` | `SAMEORIGIN` | forbids cross-origin framing. This, not the CSP, is where clickjacking protection lives: `frame-ancestors` in a `<meta>` CSP is ignored by browsers |
| `X-Content-Type-Options` | `nosniff` | the browser honors the declared content type instead of guessing one |
| `Referrer-Policy` | `strict-origin-when-cross-origin` | a cross-origin request leaks the origin, never the path or query |
| `Permissions-Policy` | `geolocation=(self), microphone=(self), camera=(self), payment=(), usb=()` | the device APIs abap2UI5 offers stay available to the app itself; payment and USB are off |
| `Cross-Origin-Resource-Policy` | `same-origin` | no other site may embed a response of the app as a resource |

They live in `cs_config-t_security_header` and are set in the same
[user exit](/advanced/extensibility/user_exits) as the CSP, so an installation
behind a proxy that already sets one of them can drop or change it. Caching is
not among them: the handler decides it per verb — the page is checked against
its `ETag` and answered with a `304`, every other response is `no-store` — and
an exit entry named `cache-control` still wins over that:

```abap
METHOD z2ui5_if_ui5_exit~set_config_http_get.

    " keep everything the framework set, override one entry
    DELETE cs_config-t_security_header WHERE n = `Referrer-Policy`.
    APPEND VALUE #( n = `Referrer-Policy` v = `no-referrer` )
        TO cs_config-t_security_header.

ENDMETHOD.
```

### Two headers only an HTTPS installation gets
`Strict-Transport-Security` and `Cross-Origin-Opener-Policy` are **not** in
that list, both for the same reason: they belong to a deployment served over
TLS, and abap2UI5 cannot see from inside the ICF node whether TLS terminated in
front of it — the request hands out the path, the parameters and the `Host`,
never the scheme.

COOP is the one with a visible symptom. A browser honors it on a
**trustworthy origin** only (`https://…`, or a `localhost` host), so on a
plain-HTTP system it is dropped and the console shows a red entry on every app
start:

```
The Cross-Origin-Opener-Policy header has been ignored, because the URL's
origin was untrustworthy.
```

Nothing is broken there — the header simply did nothing — which is why it is no
longer sent by default. Behind HTTPS both are worth having, and the exit is
where they go:

```abap
METHOD z2ui5_if_ui5_exit~set_config_http_get.

    " an installation served over HTTPS - sever cross-origin window
    " references and keep the browser on TLS
    APPEND VALUE #( n = `Cross-Origin-Opener-Policy` v = `same-origin` )
        TO cs_config-t_security_header.
    APPEND VALUE #( n = `Strict-Transport-Security` v = `max-age=31536000` )
        TO cs_config-t_security_header.

ENDMETHOD.
```

## Cross-Site Request Forgery (CSRF)
Every state-changing request in abap2UI5 is a POST, so the framework ships its own CSRF defense instead of relying on a fronting SAP ICF/CSRF layer that may or may not be there. The check compares the host authority of the request's `Origin` (or `Referer`) header against the `Host` header — or, by default, against the first `X-Forwarded-Host` when a proxy sent one (`cs_config-check_trust_forwarded_host`; an installation without a proxy hardens the gate by switching it to `abap_false`) — a cross-origin POST is rejected with an error response before any app logic runs.

**CSRF protection is active by default.** A fresh install rejects cross-origin POSTs without any configuration. If your endpoint must accept cross-origin POSTs (for example, behind a proxy setup where the origin legitimately differs), opt out in the [user exit](/advanced/extensibility/user_exits):

```abap
METHOD z2ui5_if_ui5_exit~set_config_http_post.

    " escape hatch - only disable this if your endpoint must accept cross-origin POSTs
    cs_config-check_csrf_active = abap_false.

ENDMETHOD.
```
