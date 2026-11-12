---
outline: [2, 4]
description: The go-live checklist of an abap2UI5 installation - the six things to set before business users reach an app, where each is set and how to verify it. With a production user exit to copy, and one row per operational topic.
---
# Go-Live Checklist

This is the go-live checklist. Technically, abap2UI5 is an HTTP handler: a
package of ABAP classes and interfaces and one table, behind an ICF node. Run
it the way any other HTTP service of the system is run.

A fresh installation starts without any configuration. Some of its defaults
are chosen for that first start and not for production. Work through the list
below once per system, then use the table under
[Operating It](#operating-it) to find the page for each later task.

## Before Go-Live {#four-defaults-to-change-before-go-live}

Every row names what to do, where to do it and how to see that it worked.
Rows 2 to 5 are fields of the
[user exit](/advanced/extensibility/user_exits), a class of your own; the
complete class is [below](#a-production-user-exit).

| # | What | Where | How to verify |
|---|---|---|---|
| 1 | Restrict who may call the endpoint | `SICF`: a value in *SAP Authorization* on the node; `PFCG`: a role with `S_ICF` - [Authorization](/configuration/authorization) | a user without the role gets the *not authorized* page of the ICF, a user with it gets the app |
| 2 | Load UI5 from a source you control, with a fixed version | `cs_config-src` in `set_config_http_get` - [Bootstrapping](/configuration/setup/ui5_bootstrapping) | the page source (`Ctrl+U`) shows your URL in the `<script id="sap-ui-bootstrap">` tag |
| 3 | Hide error details from the browser | `cs_config-check_hide_error_details = abap_true` in `set_config_http_post` | an app that raises an exception answers `Internal Server Error`, without the exception chain |
| 4 | Send the Content Security Policy as a response header | an entry `Content-Security-Policy` in `cs_config-t_security_header` - [Security](/configuration/security#sending-the-csp-as-a-response-header) | the network tab of the browser (`F12`) shows the header on the page request |
| 5 | Decide whether to trust `X-Forwarded-Host` | `cs_config-check_trust_forwarded_host` in `set_config_http_post`: keep `abap_true` behind a web dispatcher or reverse proxy, set `abap_false` without one - [Security](/configuration/security#cross-site-request-forgery-csrf) | an app still answers a click; a 403 `CSRF validation failed` means the setting does not fit your proxy |
| 6 | Install a release, not `main`, and write the version down | abapGit: pull a [release tag](https://github.com/abap2UI5/abap2UI5/releases/) - [Versioning, Upgrade](/configuration/versioning) | the system information of the start page shows the abap2UI5 version |

Why each row matters:

- **Row 1.** By default everyone with a logon reaches the node. The node is
  the outer check; the app's `main( )` is the inner one.
- **Row 2.** The default is the public CDN with the newest UI5. A CDN serves
  the whole JavaScript runtime into a logged-on session, and the newest
  version can change without notice.
- **Row 3.** By default the error response shows the full exception chain,
  including the public attributes of the exception classes.
- **Row 4.** By default the policy is a `<meta>` tag. The browser enforces a
  header earlier, before it reads the page.
- **Row 5.** The setting is on by default so that a proxy works at once. Any
  client can send that header, so an installation without a proxy switches
  the setting off.
- **Row 6.** The security policy supports the latest release only. Small,
  regular upgrades keep each step easy.

With the [Admin Cockpit](/configuration/monitoring#admin-cockpit) installed,
its security traffic light checks rows 3 to 5 for you: green means done.

### A Production User Exit

Copy this class into a package of your own, not into the abap2UI5 packages,
and adjust the three numbered values. The framework finds the class by its
interface; nothing has to be registered.

```abap
CLASS zcl_my_production_exit DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_ui5_exit.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_my_production_exit IMPLEMENTATION.

  METHOD z2ui5_if_ui5_exit~set_config_http_get.

    " row 2 - (1) UI5 from this system; or a CDN URL with a version in it
    cs_config-src = `/sap/public/bc/ui5_ui5/resources/sap-ui-core.js`.

    " row 4 - (2) the policy as a header; add the host UI5 comes from
    "         when it is not this system
    APPEND VALUE #(
        n = `Content-Security-Policy`
        v = `default-src 'self' data: blob:; `             &&
            `script-src 'self' 'wasm-unsafe-eval'; `       &&
            `style-src 'self' 'unsafe-inline'; `           &&
            `img-src 'self' data: blob:; `                 &&
            `connect-src 'self'; worker-src 'self' blob:; ` &&
            `object-src 'none'; base-uri 'self'; `         &&
            `frame-ancestors 'self'` )
        TO cs_config-t_security_header.

  ENDMETHOD.

  METHOD z2ui5_if_ui5_exit~set_config_http_post.

    " row 3
    cs_config-check_hide_error_details = abap_true.

    " row 5 - (3) abap_true behind a web dispatcher or reverse proxy
    cs_config-check_trust_forwarded_host = abap_false.

  ENDMETHOD.

ENDCLASS.
```

After you activate the class, reload the page and check rows 2 to 5 as the
table says. If the page stays white, open the browser console (`F12`): a
blocked script names the directive of the policy that refused it.

## What an Installation Consists Of

| Object | What it is | Where it comes from |
|---|---|---|
| The framework package and its sub-packages | `src/00` external libraries, `src/01` the engine and the embedded frontend, `src/02` the released API, `src/99` the frozen compatibility package | an abapGit pull of a [release tag](https://github.com/abap2UI5/abap2UI5/releases/) |
| Table `Z2UI5_T_01` | the table of the app drafts: the saved app instance between two roundtrips, client-dependent, one row per roundtrip | part of the package (`src/01/01`) |
| An HTTP handler class and an ICF node | a class of yours calling `z2ui5_cl_ui5_http_handler=>run( )`, entered in the handler list of a node in `SICF` | created on each system - see the [Quickstart](/get_started/quickstart#_2-set-up-http-handler-and-service) |
| A user exit class | a class of yours implementing `z2ui5_if_ui5_exit`, in a package of your own; it carries the production settings above | your package - see [User Exits](/advanced/extensibility/user_exits) |
| BSP `Z2UI5` | the frontend as an app in the UI5 ABAP repository, pulled from the [frontend repository](https://github.com/abap2UI5/frontend) | only when apps run as [launchpad](/configuration/launchpad) tiles or in a [Fiori elements host](/advanced/fiori) |
| The monitor tables `Z2UI5_T_CK_*` | the Admin Cockpit's own tables | only when the [cockpit](/configuration/monitoring#admin-cockpit) is installed |

The ICF node serves the frontend itself: the page request carries the whole
embedded UI5 app, and UI5 is loaded from the bootstrap source the exit names.
An installation without a launchpad has no frontend deployment of its own.
The words *roundtrip* and *app draft* are explained in the
[Glossary](/get_started/glossary).

## How a Release Is Tested

Every release is checked on NetWeaver 7.02, on Standard ABAP and on ABAP
Cloud, and in the browser on UI5 1.71 and on the current UI5 release. These
checks run without an SAP system. What they cannot prove is your system: that
a transport activates on your kernel, that your ICF node is configured, that
a browser behind your proxy loads the page. Reports from real systems are
collected on the framework's
[Verified on](https://github.com/abap2UI5/abap2UI5/blob/main/docs/verified-on.md)
page. There is no fixed release cadence; the
[changelog](/resources/changelog) shows when each release shipped.

## Operating It

| Task | Page |
|---|---|
| Move the installation from development to production: which objects travel in which order, what is set per system | [Transport, Landscape](/configuration/transport) |
| Upgrade, stay on a release, roll back; what is stable API | [Versioning, Upgrade](/configuration/versioning) |
| Find the cause when something breaks; recurring housekeeping | [Basis Runbook](/configuration/runbook) |
| Estimate work processes and the growth of the draft table | [Sizing](/configuration/sizing) |
| Understand the security model: data flow, CSP, response headers, CSRF | [Security](/configuration/security) |
| Decide who may start which app | [Authorization](/configuration/authorization) |
| See whether abap2UI5 is used, fast and safely configured | [Monitoring](/configuration/monitoring) |
| Make a slow roundtrip faster | [Performance](/configuration/performance) |
| Set breakpoints, read the developer tools and the error overlay | [Debugging](/configuration/debugging) |
| End every session of a user at logout | [Logout](/configuration/logout) |
| Run a second installation beside the first, under another prefix | [Namespaces, Renaming](/advanced/renaming) |

## Transport

abap2UI5 ships as ABAP objects and goes to production through the transport
process you already have. The order of the transports and a picture of the
landscape are on [Transport, Landscape](/configuration/transport).
