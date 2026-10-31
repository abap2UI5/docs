---
outline: [2, 4]
description: The user exit z2ui5_if_ui5_exit - when set_config_http_get and set_config_http_post run, what is_context carries, and every field of the two configuration structures with its default, generated from the source.
---
# User Exits

The framework has one extension point for an installation: the interface
[`z2ui5_if_ui5_exit`](https://github.com/abap2UI5/abap2UI5/blob/main/src/02/z2ui5_if_ui5_exit.intf.abap).
A class that implements it, in a package of your own and **not** in the
abap2UI5 packages, is found by the framework and called on every request. The
shipped class
[`z2ui5_cl_ui5_user_exit`](https://github.com/abap2UI5/abap2UI5/blob/main/src/01/04/z2ui5_cl_ui5_user_exit.clas.abap)
looks the class up, fills every field of the configuration with its default,
and only then calls yours — so an exit sets the fields it cares about and
leaves the rest alone.

Only one exit can be active. When more than one class implements the
interface, the lookup sorts them by name and takes the first, so a second
class added by a transport silently replaces the first; keep it to one. A
class the lookup names but cannot instantiate — a constructor that raises, a
class left inactive — is a visible 500 on the first request, not a silent
fall-back to the defaults.

## The two methods

Both methods take an `is_context` importing parameter describing the request
and a `cs_config` changing parameter holding the configuration, and both are
called more than once per request. Neither may keep state or write to the
database: read configuration, set fields, return.

**`set_config_http_get`** runs at most once per request. On the page request —
the GET that fetches the HTML shell — the result builds the page: UI5 source
and theme, the Content-Security-Policy, the installation's CSS, extra
bootstrap attributes. On every other request, the POST roundtrips included,
the same result is read once more for its `t_security_header`, which every
response of the handler carries. The handler caches the structure per
request, so the method is not called a second time for the headers.

**`set_config_http_post`** is what the backend decides per roundtrip, and the
framework asks for it wherever it needs one of its fields, with a fresh
empty structure each time: the CSRF gate on every POST and HEAD, before any
app code runs; the roundtrip monitor, to see whether it is switched on; the
draft cleanup on an app's first start; the error response, to decide how
much of an exception the browser may see; and the start page's check of the
same flag. That is several calls in one request, so the method has to be
cheap and idempotent — the same input gives the same output, and nothing
changes because it ran.

The example sets the theme and keeps drafts twice as long as the default:

```abap
CLASS zcl_sample_user_exit DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES  z2ui5_if_ui5_exit.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_sample_user_exit IMPLEMENTATION.

  METHOD z2ui5_if_ui5_exit~set_config_http_get.

    cs_config-theme = `sap_horizon_dark`.

  ENDMETHOD.

  METHOD z2ui5_if_ui5_exit~set_config_http_post.

    cs_config-draft_exp_time_in_hours = 8.

  ENDMETHOD.

ENDCLASS.
```

The interface has no default implementation for either method, so a class
implements both, even when one of them stays empty.

## The request context

`is_context` says which request the exit is answering for:

- `path` — the path of the request, as the ICF handler received it.
- `app_start` — the `app_start` query parameter of this request, read the
  way the framework reads it: trimmed, upper-cased, a percent-encoded
  namespace unpacked. It is empty on every POST (the frontend posts to the
  manifest URI, not to the app's URL) and when the app is named by the hash
  route, which never reaches the server. Treat it as a hint for the page
  request — a tighter CSP for one app, hidden error details for another —
  not as the authority on which app runs.
- `t_params` — every URL parameter of the request as name/value pairs.

The shipped class passes the context on every call. An exit written before
the parameter existed compiles unchanged: it is optional in the interface.

## What the fields control

The reference below lists every field with its type and default. Where a
field is explained on another page, that page is the place to read:

- `src`, `theme`, `t_add_config`, `styles_css` — the start page the
  framework generates: [Setup](/configuration/setup) and its pages on the
  [UI5 source](/configuration/setup/ui5_bootstrapping),
  the [theme](/configuration/setup/theme),
  [bootstrap attributes](/configuration/setup/bootstrap_attributes) and
  [CSS](/configuration/setup/style_css).
- `content_security_policy`, `t_security_header`, `check_csrf_active`,
  `check_trust_forwarded_host` — [Security](/configuration/security): the
  CSP meta tag and how to edit one directive of it rather than replace the
  tag, the response headers, and the CSRF gate with its two switches. Both
  switches default to `abap_true`; an installation that is not behind a
  proxy hardens the gate by setting `check_trust_forwarded_host` to
  `abap_false`, and only an endpoint that must accept cross-origin POSTs
  sets `check_csrf_active` to `abap_false`.
- `check_hide_error_details` — off by default: an exception nobody catches is
  answered with a 500 whose body carries the whole exception chain, class
  names and source positions included. A production system sets it to
  `abap_true` and the browser gets a bare `Internal Server Error`; see
  [Exception](/cookbook/event_navigation/exception).
- `check_monitor_active` — off by default, and nothing seeds it: the
  roundtrip monitor `z2ui5_if_ui5_monitor` is only called when the exit sets
  it to `abap_true`. The monitor is a second interface found the same way as
  the exit, and unlike the exit it fails open — a monitor that raises costs
  its log entry, never the app. Available from abap2UI5 1.147.0 on; see
  [Monitoring](/configuration/monitoring).
- `draft_exp_time_in_hours` — how long the backend keeps a draft before the
  cleanup on an app's first start deletes it. The default is 4 hours, and a
  value of 0 or less falls back to it, so the expiry is always positive.

## Reference

Everything below is generated from the interface and the shipped exit on
the framework's `main` branch by `npm run generate:api`, the same run that
writes the [Client API](/resources/api); `check:api-reference` fails when
either source changes under it. Edit the prose above, never the block.

<!-- api:start (generated by scripts/generate-api-reference.mjs — do not edit) -->

Generated from [`z2ui5_if_ui5_exit`](https://github.com/abap2UI5/abap2UI5/blob/main/src/02/z2ui5_if_ui5_exit.intf.abap) on **main** — 2 methods,
3 structures — with the defaults [`z2ui5_cl_ui5_user_exit`](https://github.com/abap2UI5/abap2UI5/blob/main/src/01/04/z2ui5_cl_ui5_user_exit.clas.abap)
seeds before an exit of your own runs. A field your exit leaves alone keeps the default.

### `set_config_http_get`

| Parameter | Type | |
|---|---|---|
| `is_context` | `ty_s_http_context` | importing, optional |
| `cs_config` | `ty_s_http_config` | changing |

### `set_config_http_post`

| Parameter | Type | |
|---|---|---|
| `is_context` | `ty_s_http_context` | importing, optional |
| `cs_config` | `ty_s_http_config_post` | changing |

### `ty_s_http_context`

What the request the exit is answering for is - path, the app named by the URL, and the URL parameters. app_start is the app_start query parameter of THIS request, trimmed, upper-cased and with a percent-encoded namespace unpacked - the way the framework reads it. It is empty on every POST (the SPA posts to the manifest URI) and when the app is named by the hash route, which never reaches the server: a hint for the page request, not the authority on which app runs.

| Field | Type |
|---|---|
| `path` | `string` |
| `app_start` | `string` |
| `t_params` | `z2ui5_if_client=>ty_t_name_value` |

### `ty_s_http_config`

Everything the generated page is built from - the changing parameter of set_config_http_get.

| Field | Type | Default |
|---|---|---|
| `src` | `string` | `https://sdk.openui5.org/resources/sap-ui-cachebuster/sap-ui-core.js` |
| `theme` | `string` | `sap_horizon` |
| `content_security_policy` | `string` | built by [the shipped exit](https://github.com/abap2UI5/abap2UI5/blob/main/src/01/04/z2ui5_cl_ui5_user_exit.clas.abap) (`gv_csp_default`) |
| `styles_css` | `string` | *(empty)* |
| `t_add_config` | `z2ui5_if_client=>ty_t_name_value` | *(empty)* |
| `t_security_header` | `z2ui5_if_client=>ty_t_name_value` | `X-Content-Type-Options: nosniff`, `X-Frame-Options: SAMEORIGIN`, `Referrer-Policy: strict-origin-when-cross-origin`, `Permissions-Policy: geolocation=(self), microphone=(self), camera=(self), payment=(), usb=()`, `Cross-Origin-Resource-Policy: same-origin` |

**`content_security_policy`** — the CSP meta tag of the page, pre-filled with the default before the exit runs. The default carries no 'unsafe-eval'. Only on UI5 1.71 to 1.82 can a popup still need it - one whose XML names a module in a binding type or a core:require that is not loaded yet. Such an installation switches it on here - `` REPLACE `script-src 'self'` IN cs_config-content_security_policy WITH `script-src 'self' 'unsafe-eval'`. `` - or the exit replaces the whole tag. It does carry 'wasm-unsafe-eval' in script-src, which lets a script compile WebAssembly and nothing else: the camera scanner of sap.ndc and the BarcodeScanner of the custom controls need it. A tag an exit writes itself keeps it for them. Its script-src carries no 'unsafe-inline' either: after the exit ran, the framework appends the hash of the page's one inline script to every script-src (and script-src-elem) - here and in a policy sent through t_security_header - so nothing else inline runs. A script-src that names 'unsafe-inline' itself is left without the hash, which is how an installation that needs inline script of its own opts back in `` REPLACE `script-src 'self'` IN cs_config-content_security_policy WITH `script-src 'self' 'unsafe-inline'`. ``

**`styles_css`** — CSS of the installation's own, written into the page head as a &lt;style> element of its own (a &lt; is escaped as \3c )

### `ty_s_http_config_post`

What the backend decides per roundtrip - the changing parameter of set_config_http_post.

| Field | Type | Default |
|---|---|---|
| `draft_exp_time_in_hours` | `i` | `4` |
| `check_hide_error_details` | `abap_bool` | `abap_false` |
| `check_csrf_active` | `abap_bool` | `abap_true` |
| `check_trust_forwarded_host` | `abap_bool` | `abap_true` |
| `check_monitor_active` | `abap_bool` | `abap_false` |

**`check_hide_error_details`** — when set via the exit, framework errors answer with a generic 500 message instead of the raw exception text (avoids leaking internal details to the client in hardened installations)

**`check_csrf_active`** — a state-changing POST whose Origin/Referer header names a different site than the app's own host is rejected with 403 (CSRF defense). On by default: z2ui5_cl_ui5_user_exit=>set_config_http_post seeds abap_true before the user exit runs, so an app that must accept cross-origin POSTs opts out by setting it back to abap_false in its own exit. Lenient: a request without an Origin/Referer header (older clients, some proxies) is allowed, so only an explicit cross-origin marker is blocked - the app's own roundtrips are always same-origin (the SPA fetches its own backend).

**`check_trust_forwarded_host`** — the CSRF gate compares Origin/Referer against the app's own host, and behind a reverse proxy / web dispatcher that host is the X-Forwarded-Host the proxy wrote - so it is trusted by default (seeded abap_true like check_csrf_active), or every request behind a proxy would 403 against the internal Host. The header is client-suppliable, though: an installation that is NOT behind a proxy that sets it hardens the gate by switching this to abap_false in its exit, so only the transport-level Host header is compared. Of a comma-separated list the FIRST entry is compared, on purpose: that is the header's meaning (each proxy appends the Host it saw, so the first is the one the browser sent, which is what Origin carries), and the last entry behind two proxies is an internal name. A proxy that APPENDS instead of replacing lets a client put its own entry first - a non-browser client, since a browser cannot send the header without a CORS preflight the ICF node answers 405, and a non-browser client is not a CSRF victim. Do not "fix" this by taking the last entry; the switch above is the hardening.

**`check_monitor_active`** — the roundtrip monitor (z2ui5_if_ui5_monitor) is called only when this is abap_true - off by default, nothing seeds it. A monitor is code of another package running inside every roundtrip of every app, and a syntax error in it or in anything it uses is the runtime error SYNTAX_ERROR, which no CATCH stops: installing a monitor must not be able to break abap2UI5. An installation switches it on here, once the monitor's package is active and checked: `cs_config-check_monitor_active = abap_true.` Read on every roundtrip, so switching it off again takes effect on the next one.

<!-- api:end -->
