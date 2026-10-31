---
outline: [2, 4]
description: The Basis view of an abap2UI5 installation - the ICF node, ICM and compression, the draft table and its cleanup, the monitor tables, where to look when something breaks, static checks, SSO, and a housekeeping checklist.
---
# Basis Runbook

What the person who runs the system needs to know about an abap2UI5
installation, object by object. Every statement here is either verified
against the framework's source or says "check your system" - the framework
sets no profile parameter and knows nothing about your ICM, so the SAP side
is yours to look up.

## The ICF Node

One node, one handler class, one framework call:

| | |
|---|---|
| Path | whatever you created in `SICF` - the Quickstart's example is `/sap/bc/<your_service>` |
| Handler | your class implementing `if_http_extension`, whose `handle_request` calls `z2ui5_cl_ui5_http_handler=>run( server )`; on ABAP Cloud `if_http_service_extension` and `run( req = request res = response )` |
| Verbs | `GET` answers the page (the whole embedded frontend, checked against its `ETag` and answered with `304` when unchanged); `POST` is a roundtrip; `HEAD` is sent by the frontend only - the session-terminate ping and the CSRF token fetch |
| Logon | the node's logon procedure is standard ICF: whatever the *Logon Data* tab says applies, SSO included. A request that reaches the handler has passed SAP logon, and `sy-uname` is the user |
| Authorization | the node's authorization field is checked against `S_ICF`; a check of your own goes into the handler before the framework call - [Authorization](/configuration/authorization) |
| Error pages | `/sap/public/bc/icf/logoff`, where the logout event sends the browser, is the system's; its target is configured under *Error Pages → Logoff Page* of that node - [Logout](/configuration/logout#sicf-configuration) |

A node that is not active answers the ICF error page or a plain 404 before
any abap2UI5 code runs; a node that is active but whose handler class is
inactive or missing answers a 500 from ICF, not the framework's error page.

## ICM, HTTP

- **Compression.** Since 1.143.0 the handler asks the ICF runtime to gzip
  every response the client accepts compressed: the page (several hundred
  KB of embedded frontend) and the roundtrip JSON. On an ICM profile that
  compresses already this is a no-op; where it does not, it is a 70 to 85
  percent transfer cut. On ABAP Cloud the platform router compresses and the
  call is not made.
- **Caching.** The page is `ETag`-validated and may be stored; every other
  response is `no-store`. An exit entry named `cache-control` overrides the
  verb's default - [Security](/configuration/security#response-headers).
- **Request size and timeouts.** The framework sets neither. A roundtrip is
  one POST whose body carries the event and the model delta, uploaded file
  contents included. The ICM limits on request size and on the time a
  request may run are profile parameters of your system - check them with
  Basis when an upload or a long `main( )` fails with an ICM error rather
  than an abap2UI5 one.
- **Behind a proxy.** The CSRF check compares `Origin` against the first
  `X-Forwarded-Host` when a proxy sent one; without a proxy, switch
  `check_trust_forwarded_host` off in the exit. TLS termination in front of
  the node is invisible to the handler, which is why `Strict-Transport-Security`
  and `Cross-Origin-Opener-Policy` are not sent by default -
  [Security](/configuration/security#two-headers-only-an-https-installation-gets).

## The Draft Table `Z2UI5_T_01`

The one table of the framework: the serialized app state between two
roundtrips. Transparent, delivery class A, client-dependent, not buffered,
and deliberately without a secondary index.

| Field | Content |
|---|---|
| `MANDT`, `ID` | the client and the draft id, a 32-character UUID; the id is what the browser holds between two clicks |
| `ID_PREV`, `ID_PREV_APP`, `ID_PREV_APP_STACK` | the previous draft of the same app, the draft of the calling app, and the app stack - what Back and `nav_app_leave( )` follow |
| `UNAME` | the user who wrote the row; every read is checked against it and fails closed, so a leaked or guessed id loads nothing for anyone else |
| `TIMESTAMPL` | when the row was written, UTC |
| `DATA` | the app instance as XML - every attribute of the class, whatever its visibility, bound or not |

**One `INSERT` per roundtrip.** Every roundtrip of a stateless app writes a
new row with a fresh id and leaves the previous one in place (it is what
Back restores). A stateful app writes a row only on the paths that need a
bookmarkable draft - [Statefulness](/cookbook/expert_more/statefulness).

**Cleanup runs on every app start**, not on every roundtrip: the first request
of an app - a start from the page, a launchpad tile, a bookmark or a route
restore - runs one `DELETE` of every row older than the expiry across the whole
table, every user's rows, followed by a `COMMIT WORK`. The expiry is
`draft_exp_time_in_hours` of the user exit's `set_config_http_post`, **4 hours
by default**; a value of zero or below falls back to the default. To shorten it:

```abap
METHOD z2ui5_if_ui5_exit~set_config_http_post.

    cs_config-draft_exp_time_in_hours = 1.

ENDMETHOD.
```

Shortening it also shortens how long a bookmark, an app-state link and a
Back step keep working: a bookmark or app-state link whose draft expired
starts the app fresh with a toast saying so; an ordinary click on an expired
draft is answered with the error page, and the next start is a fresh app.

**What to watch.** The table's row count: on a system where every start
cleans up, it settles at roughly the roundtrips of the last expiry window
([Sizing](/configuration/sizing#draft-table-growth)). It grows past that
when nobody starts an app for a long time while users keep clicking - the
cleanup has not run - or when the instances are large. The framework's
start page shows the own and the total count; the Admin Cockpit's
*Drafts & Housekeeping* tab shows rows, age, owners and expiry, deletes the
expired rows on demand, and ships that deletion as a class for a background
job ([Monitoring](/configuration/monitoring#admin-cockpit)). Without the
cockpit, a job of your own that runs the same `DELETE` by the timestamp is the
whole housekeeping.

Do not add a secondary index on `TIMESTAMPL` or `UNAME`: the table takes one
`INSERT` per click, and the maintainers decided against paying for an index
on every write to speed up a delete that runs once per app start.

## The Monitor Tables

The framework itself persists nothing but drafts. What gets logged about
roundtrips is the monitor's business - a class of yours implementing
`z2ui5_if_ui5_monitor`, or the Admin Cockpit, which keeps its own tables
`Z2UI5_T_CK_*` (aggregates per day, hour, app and event; a raw row for errors
and slow roundtrips; users; settings; administrators; an audit log; alerts)
and purges them itself by a retention setting. The monitor runs only when the
exit sets `check_monitor_active`, and a syntax error in a monitor class is a
short dump on every request - switch it on after the package is active and
checked. Details and the field list: [Monitoring](/configuration/monitoring).

## When Something Breaks

| Symptom | Where to look |
|---|---|
| ICF error page or a bare 404 | `SICF`: is the node active, is the handler class in its handler list and active |
| The framework's error overlay in the browser | the 500 body: the exception chain, one block per exception with class, text, source position and kernel error id, plus the app, event and draft id. The user sees the messages, *Details* and *Copy* hold the rest. With `check_hide_error_details` on, the body is `Internal Server Error` and the chain is gone - read it from the monitor's `error_text` instead, or switch the details on for one user in the exit |
| A short dump | `ST22`. The framework catches what `main( )` raises and turns it into the 500; what reaches `ST22` is what no `CATCH` stops - a syntax error in an app, an exit or a monitor class, or a kernel error |
| The system log | `SM21`, for ICF and work process messages around the time of the error |
| The request itself | the ICF recorder of `SICF` for the raw request and response on the server; the frontend's developer tools (`Ctrl+F12`) show the same from the browser's side, roundtrip by roundtrip, with an export for a bug report - [Debugging](/configuration/debugging) |
| `NO_DRAFT_ENTRY_OF_PREVIOUS_REQUEST_FOUND` | the draft the browser named is gone - expired, cleaned up, written by another user, or never written because the previous roundtrip failed. The user gets a fresh start; a pattern of it points at an expiry shorter than a session, or at a rollback |
| A 403 `CSRF validation failed` | the `Origin` of the POST does not match the host the handler sees - a proxy that does not set `X-Forwarded-Host`, or a page that frames the app from another origin - [Security](/configuration/security#cross-site-request-forgery-csrf) |

The monitor records every failed roundtrip with its exception chain and
class when it is switched on, which is the record that outlives the browser
tab.

## Static Checks

The framework's own CI lints the package with abaplint under three rule sets
- Standard ABAP, ABAP Cloud (released APIs only, no classic statements) and
7.02 - over `src/01`, `src/02` and `src/99`; `src/00`, the external
libraries, is excluded from the rules. An ATC or Code Inspector run of your own
over the package will report findings against your variant that the
project's rules do not share - naming conventions above all, and the
unavoidable ones of a framework that creates objects and calls methods
dynamically. Treat them as your variant's opinion about code you do not own;
a finding that is a real defect belongs in an issue of the framework, with
the object name.

The objects are in the `Z2UI5_` namespace, which is a customer namespace by
SAP's rules and reserved for abap2UI5 by the project's. A system whose ATC
variant requires another prefix, or that needs two installations side by
side, installs the renamed build - [Namespaces, Renaming](/advanced/renaming).
Never add an object of yours under `Z2UI5_`: the next pull may ship one of
that name.

## SSO, SAML, Kerberos

Authentication is the node's, not the framework's: abap2UI5 adds no user
store, no token and no session concept beyond the ICF session. Whatever the
node's logon procedure accepts - logon ticket, SAML assertion, SPNEGO,
X.509, basic authentication - the handler sees as `sy-uname`. Configure it on
the node like on any other ICF service. The one thing to know on the way
out: a user of an abap2UI5 app may hold up to three sessions at once (the
launchpad shell, the ICF session, a BSP context), and ending one does not end
the others - [Logout](/configuration/logout) lists which startup context owns
which, and what the logout event ends.

## Housekeeping Checklist

- [ ] The node is active on this system, with the authorization field set
- [ ] The exit names a controlled, pinned UI5 bootstrap source, hides error
      details, sends the CSP as a header, and trusts `X-Forwarded-Host` only
      behind a proxy
- [ ] `draft_exp_time_in_hours` fits the longest session a user keeps open
- [ ] The row count of `Z2UI5_T_01` is watched, and a background job deletes
      expired rows on a system where app starts are rare
- [ ] The monitor is switched on only where its package is active and checked
- [ ] The release and the UI5 version are recorded, and the changelog is read
      before the next pull - [Versioning, Upgrade](/configuration/versioning)
- [ ] The cockpit's security traffic light is green, or every yellow has a
      reason - [Monitoring](/configuration/monitoring#admin-cockpit)
