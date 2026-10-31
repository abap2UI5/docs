---
outline: [2, 4]
description: What a productive abap2UI5 installation consists of, the four defaults to change before go-live, how a release is tested, and where each operational topic is covered.
---
# Productive Usage

Technically, abap2UI5 is an HTTP handler implementation: a package of ABAP
classes and interfaces and one table, behind an ICF node. Run it the way any
other HTTP service of the system is run. This page is the entry point of the
operational chapter: what an installation consists of, what to change before
it goes live, how a release is tested, and one row per topic linking the page
that covers it.

## What an Installation Consists Of

| Object | What it is | Where it comes from |
|---|---|---|
| The framework package and its sub-packages | `src/00` external libraries, `src/01` the engine and the embedded frontend, `src/02` the released API, `src/99` the frozen compatibility package | an abapGit pull of a [release tag](https://github.com/abap2UI5/abap2UI5/releases/) |
| Table `Z2UI5_T_01` | the draft table: the serialized app state between two roundtrips, client-dependent, one row per roundtrip | part of the package (`src/01/01`) |
| An HTTP handler class and an ICF node | a class of yours calling `z2ui5_cl_ui5_http_handler=>run( )`, entered in the handler list of a node in `SICF` | created on each system - see the [Quickstart](/get_started/quickstart#_2-set-up-http-handler-and-service) |
| A user exit class | a class of yours implementing `z2ui5_if_ui5_exit`, in a package of your own; it carries the production settings below | your package - see [User Exits](/advanced/extensibility/user_exits) |
| BSP `Z2UI5` | the frontend as an app in the UI5 ABAP repository, pulled from the [frontend repository](https://github.com/abap2UI5/frontend) | only when apps run as [launchpad](/configuration/launchpad) tiles or in a [Fiori elements host](/advanced/fiori) |
| The monitor tables `Z2UI5_T_CK_*` | the Admin Cockpit's own tables | only when the [cockpit](/configuration/monitoring#admin-cockpit) is installed |

The ICF node serves the frontend itself: the page request carries the whole
embedded UI5 app, and UI5 is loaded from the bootstrap source the exit names.
An installation without a launchpad has no frontend deployment of its own.

## Four Defaults to Change Before Go-Live

A fresh installation runs with zero configuration, and four of its defaults
are chosen for that first start rather than for production. All four are set
through the [user exit](/advanced/extensibility/user_exits):

| Default | Productive setting | Why |
|---|---|---|
| UI5 is loaded from the public CDN (`sdk.openui5.org`, cache-buster URL) | point `cs_config-src` at an on-stack or otherwise controlled UI5, pinned to a version - [Bootstrapping](/configuration/setup/ui5_bootstrapping) | the CDN serves the entire JavaScript runtime into an authenticated session, and a cache-buster URL cannot be integrity-pinned |
| Error details are shown | `cs_config-check_hide_error_details = abap_true` in `set_config_http_post` | the 500 body renders the full exception chain, including the public attributes of SAP's and your own exception classes |
| The CSP is a `<meta>` policy | a real `Content-Security-Policy` response header through `t_security_header` - [Security](/configuration/security#content-security-policy) | a header is enforced before the first byte of the body is parsed |
| The ICF node is reachable by everyone with a logon | an authorization object on the node, as for any service - [Authorization](/configuration/authorization) | whether the node can be reached is the outer gate; the app's `main( )` is the inner one |

A fifth switch is worth knowing: `check_trust_forwarded_host` is on by
default so that a web dispatcher or reverse proxy works out of the box. An
installation without such a proxy hardens the CSRF check by switching it off -
[Security](/configuration/security#cross-site-request-forgery-csrf).

## How a Release Is Tested

A release is a tag on `main` (`X.Y.Z`) plus the same tag on the downported
branch (`X.Y.Z-702`). Nothing merges into `main` before all of this is green,
and none of it needs an SAP system - which is what makes it run on every
change, and what it cannot prove: that a transport activates on a given
kernel, that an ICF node is configured, that a browser behind a corporate
proxy loads the page.

| Claim | How it is verified |
|---|---|
| Syntax on NW 7.02 | every merge is downported by abaplint and linted against the 7.02 language version; the result is the `702` branch and the `-702` tag |
| Syntax on Standard ABAP (7.50 and later) | abaplint with the standard-ABAP rule set |
| Syntax and API scope on ABAP Cloud | abaplint with the cloud rule set: released APIs only, no classic statements |
| The backend behaves as specified | the framework transpiled to JavaScript runs its ABAP unit tests on every merge |
| Apps render and roundtrip on UI5 1.71 and on the current release | browser tests drive the shell, the roundtrip, popups, navigation, the launchpad split and the error overlay on both UI5 builds |
| Nothing in the frontend uses an API UI5 2.x removes | the legacy-free 2.x bootstrap runs the frontend checks, and a `[FUTURE FATAL]` log entry on the current 1.x build fails the run |
| Every icon, control and module exists on 1.71 | the shipped views and modules are compared against the 1.71 inventory |
| abapGit imports the result byte for byte | a gate on file format, sidecars and package structure under `src/` |
| The public API did not change by accident | a normalized snapshot of `src/02` is compared on every pull request |

What is reported from real systems - release, ABAP release, UI5 version,
green or red - is collected on the framework's
[Verified on](https://github.com/abap2UI5/abap2UI5/blob/main/docs/verified-on.md)
page, and the system report that feeds it takes two minutes. There is no fixed
release cadence; the [changelog](/resources/changelog) shows when each release
shipped.

## Operating It

| Topic | Page |
|---|---|
| Release tags, what is stable API, the frozen package, how to upgrade, pin and roll back | [Versioning, Upgrade](/configuration/versioning) |
| Which objects travel in which order, the per-system settings, ABAP Cloud and 7.02 | [Transport, Landscape](/configuration/transport) |
| The ICF node, ICM, the draft table, the monitor tables, what to look at when something breaks, housekeeping | [Basis Runbook](/configuration/runbook) |
| Work processes per roundtrip, draft table growth, stateful sessions, model size | [Sizing](/configuration/sizing) |
| The data-flow rule, the threat model, CSP, response headers, CSRF | [Security](/configuration/security) |
| Service-level and app-level authorization | [Authorization](/configuration/authorization) |
| The roundtrip monitor and the Admin Cockpit | [Monitoring](/configuration/monitoring) |
| What costs time in a roundtrip and how to avoid it | [Performance](/configuration/performance) |
| Breakpoints, the developer tools, the error overlay | [Debugging](/configuration/debugging) |
| The session layers a logout has to end | [Logout](/configuration/logout) |

## Stable Version

Pin a [release](https://github.com/abap2UI5/abap2UI5/releases/) rather than
tracking `main`, and upgrade regularly to keep the step small. The security
policy supports the latest release only. What is API and what is not, and how
a deprecation is announced, is on [Versioning, Upgrade](/configuration/versioning).

Apps embedded with the embed control load their frontend from the backend, so
an upgrade reaches all of them with the same transport - see
[Fiori Elements Integration](/advanced/fiori#upgrades).

## Monitoring

Whether abap2UI5 is used, fast and safely configured is what the
[Monitoring](/configuration/monitoring) page covers: a monitor interface the
framework calls after every roundtrip, and the Admin Cockpit add-on with a
security traffic light that works without any logging. *From abap2UI5 1.147.0
on.*

## Renaming

To keep apps in production untouched while new development moves to a newer
framework, install abap2UI5 a second time under another prefix with the
[renaming feature](/advanced/renaming). The two installations share nothing:
not the package, not the draft table, not the ICF node.

## Transport

abap2UI5 ships as ABAP objects and goes to production through the transport
process you already have. The order - framework first, then the node and the
exit, a Hello World check, then your apps - and what differs on ABAP Cloud and
on 7.02 are on [Transport, Landscape](/configuration/transport).

![Transport process from development to production via abapGit](/configuration/image-3.webp){ width=80% }
