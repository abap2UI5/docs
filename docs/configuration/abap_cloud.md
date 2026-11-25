---
outline: [2, 4]
description: abap2UI5 on ABAP Cloud - BTP ABAP Environment and S/4 Public Cloud - what differs from Standard ABAP, the installation in short, the handler, the UI5 bootstrap the platform's CSP allows, outbound HTTP, what is not available, and where the detailed pages are.
---
# ABAP Cloud

ABAP Cloud is the development model of the **BTP ABAP Environment** and of
**S/4 Public Cloud**: code in the language version *ABAP for Cloud
Development*, using released APIs only. abap2UI5 runs there as an ordinary
ABAP Cloud development - classes, interfaces and one table, no BSP, no
function group. This page collects what is different there and links the
pages that cover each step in detail.

| I want to… | Section |
| --- | --- |
| know what is different | [What Differs](#what-differs) |
| install it | [Installation](#installation) |
| write the handler | [The Handler](#the-handler) |
| know why the page loads UI5 from ui5.sap.com | [The UI5 Bootstrap](#the-ui5-bootstrap) |
| call an external service, a language model | [Outbound HTTP](#outbound-http) |
| know what does not work there | [What Is Not Available](#what-is-not-available) |
| put an app in front of business users | [Launchpad and Work Zone](#launchpad-and-work-zone) |

## What Differs

| | Standard ABAP | ABAP Cloud |
| --- | --- | --- |
| install | abapGit, the report or ADT | abapGit for Eclipse (ADT) only, then a mass activation |
| endpoint | an ICF node in `SICF`, handler on `if_http_extension` | an HTTP service, a development object in ADT, handler on `if_http_service_extension` |
| who can open the endpoint | every user with the authorization you give the node | developers: the service needs `S_DEVELOP`; business users come through a tile |
| default UI5 bootstrap | `sdk.openui5.org` (OpenUI5) | `ui5.sap.com` (SAPUI5) |
| outbound HTTP | `CL_HTTP_CLIENT`, an SM59 destination or a URL | `CL_WEB_HTTP_CLIENT_MANAGER`, a communication arrangement, a BTP destination or a URL |
| stateful sessions | available | not available: `set_session_stateful( )` compiles and does nothing |
| transport | the package travels in a transport request | the package belongs to a software component, which travels |

Your apps are a separate question. The framework runs on both, and an app runs
on ABAP Cloud when the app itself uses released APIs only: a `SELECT` on a
released CDS view, not on a classic table.

### What the framework decides at run time

The framework is one code base for both. Where the two differ it asks
`z2ui5_cl_ui5_util_context=>check_abap_cloud( )`, which answers `abap_true`
when the classic table `T100` cannot be described by RTTI - which is the case
under ABAP for Cloud Development. Four things depend on the answer:

- **The default bootstrap** of the user exit: `ui5.sap.com` instead of
  `sdk.openui5.org` ([below](#the-ui5-bootstrap)).
- **The outbound HTTP client**: `CL_WEB_HTTP_CLIENT_MANAGER` instead of
  `CL_HTTP_CLIENT` ([below](#outbound-http)).
- **Finding the classes of an installation**: the user exit and the
  [roundtrip monitor](/configuration/monitoring#the-roundtrip-monitor) are
  found as implementations of an interface, through the XCO library
  (`XCO_CP_ABAP`) instead of the classic class repository.
- **The start page** shows the answer in its *ABAP for Cloud* checkbox.

Every class the framework needs on one platform and that is missing on the
other is named dynamically, so the framework activates on both.

## Installation

The steps, in order - each one is shown with screenshots on
[S/4 Public Cloud](/configuration/s4_public_cloud), which covers the BTP ABAP
Environment as well:

1. **Pull with abapGit for Eclipse.** Pull the newest release tag without a
   suffix, `X.Y.Z`, into a package of your own. Folder logic `PREFIX`, and a
   short package name: the sub-packages are derived from it, and a package
   name has 30 characters at most
   ([S/4 Public Cloud](/configuration/s4_public_cloud#_1-install-abap2ui5-with-abapgit)).
2. **Mass-activate.** ADT abapGit pulls every object inactive. Activate them
   all, and repeat until nothing is left inactive.
3. **Create the handler class and the HTTP service** in ADT
   ([below](#the-handler)) - or pull the branch `cloud` of abap2UI5 in step
   1, which brings both. Open the service URL: the start page of abap2UI5
   appears, with *ABAP for Cloud* ticked.
4. **For business users:** deploy the app and put a tile in front
   of it ([Launchpad and Work Zone](#launchpad-and-work-zone)).

What travels to the next system, and in which order, is on
[Transport, Landscape](/configuration/transport#abap-cloud).

## The Handler

The handler implements `if_http_service_extension` and hands the request and
the response to the framework:

```abap
CLASS zcl_my_abap2UI5_http_handler DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_http_service_extension.
ENDCLASS.

CLASS zcl_my_abap2UI5_http_handler IMPLEMENTATION.
  METHOD if_http_service_extension~handle_request.
    z2ui5_cl_ui5_http_handler=>run( req = request res = response ).
  ENDMETHOD.
ENDCLASS.
```

`run( )` with `req` and `res` takes the ABAP Cloud path:
`z2ui5_cl_ui5_http_handler=>factory_cloud( )` wraps the two objects, which is
also the call for a handler that needs the instance itself. The HTTP service
that names this class is created in ADT as a development object of the
package; SAP's tutorial
[Create an HTTP Service](https://developers.sap.com/tutorials/abap-environment-create-http-service.html)
shows the dialog. Everything else about the endpoint - the user exit, the CSP,
the authorization of apps - is the same as on Standard ABAP
([Security](/configuration/security), [Authorization](/configuration/authorization)).

## The UI5 Bootstrap

ABAP Cloud answers every HTTP service with a Content Security Policy header of
its own, next to the one abap2UI5 writes, and the browser enforces both. The
platform's `script-src` trusts the SAP hosts only. The OpenUI5 CDN, the
default everywhere else, is refused there, and the page stays blank.

So from abap2UI5 1.149.0 on, the default bootstrap on ABAP Cloud is
`https://ui5.sap.com/resources/sap-ui-cachebuster/sap-ui-core.js` - SAPUI5,
the latest release. An exit that sets `cs_config-src` itself is not touched.
To bootstrap from another host, set it in the exit **and** add it to the
trusted sites of the system: app `Maintain Protection Allowlists`, tab
*Content Security Policy*
([Bootstrapping](/configuration/setup/ui5_bootstrapping)). The same holds for
a custom theme on another host ([Theme](/configuration/setup/theme)).

## Outbound HTTP

The framework's outbound HTTP - `z2ui5_cl_ui5_util_http` and through it the
language model providers of `z2ui5_cl_ui5_llm` (`create_claude( )`,
`create_openai( )`) - uses `CL_WEB_HTTP_CLIENT_MANAGER` on ABAP Cloud. The
`destination` parameter is read in three ways:

| `destination` | What the framework creates |
| --- | --- |
| empty | a destination from the URL itself - the host then needs an outbound communication arrangement |
| `SCENARIO/SERVICE_ID` or `SCENARIO/SERVICE_ID/COMM_SYSTEM` | a destination from a communication arrangement - the slash tells it apart |
| any other name | a destination of the BTP destination service, used as the system, not as the user in front of the screen |

The key, the model and the endpoint come from the app; nothing is written
into the framework. A request that could not be sent, or got no answer in
time, can be retried on ABAP Cloud as on Standard ABAP; a missing or forbidden
destination cannot ([Release Notes](/resources/changelog#_1-149-0)).

## What Is Not Available

What ABAP Cloud does not release is not there for an app either. The cases
the pages of this site meet:

- **Stateful sessions.** The released HTTP interface has no stateful mode, so
  `set_session_stateful( )` does nothing and every roundtrip lands in a fresh
  session. An enqueue lock does not survive to the next click
  ([Statefulness](/cookbook/expert_more/statefulness),
  [Lock](/cookbook/expert_more/lock)).
- **Classic APIs.** `cl_bcs` and its kin are not released: mail goes through
  `cl_bcs_mail_message` ([Send by Mail](/tutorials/everyday/send_by_mail)).
  Spreadsheets go through `XCO_CP_XLSX`
  ([Spreadsheet](/cookbook/device_capabilities/spreadsheet)), data element
  texts through `XCO_CP_ABAP_DICTIONARY` instead of the classic DDIC reads
  ([Translation](/cookbook/translation_messages/translation_i18n)), and the
  classic form runtime is not released for PDF forms
  ([Print Forms as PDF](/tutorials/everyday/print_forms)).
- **Selection screens, `WRITE` lists, `CL_SALV_TABLE`.** None exists. The
  add-on behind [Apps Written Like Reports](/advanced/report_style) gives the
  same shape as an abap2UI5 app.
- **Parts of add-ons.** An add-on is ABAP Cloud ready where its README says so,
  and some say *partly*. The [popups](https://github.com/abap2UI5-addons/popups)
  add-on: transport requests work on-premise only, and the search help does not
  run on ABAP Cloud yet. The
  [table content loader](https://github.com/abap2UI5-addons/table-content-loader):
  XLSX upload and download are not ready there. The
  [SQL console](https://github.com/abap2UI5-addons/sql-console): the native SQL
  console is Standard ABAP; the Open SQL console runs on ABAP Cloud. The full
  list is on [Add-ons](/resources/addons).
- **BSP applications in the Excel add-in.** The host page is served from an
  approuter instead ([Excel Add-in](/integration/excel)).

## Launchpad and Work Zone

The HTTP service needs `S_DEVELOP`, so a business user reaches an app through
a tile. That takes two things:

- **The app**, deployed from VS Code with the SAP Fiori tools - the folder
  `app` of the branch `cloud` of abap2UI5
  ([Deployment Scenarios](/configuration/installation#frontend-branches),
  [S/4 Public Cloud](/configuration/s4_public_cloud#_4-deploy-the-ui5-app-optional)).
- **The chain of objects behind a tile**: launchpad app descriptor item, IAM
  app, business catalog, business role, then space and page
  ([S/4 Public Cloud](/configuration/s4_public_cloud#_5-give-business-users-access-optional)).

To show the apps in SAP Build Work Zone instead, a BTP destination points at
the HTTP service and a connector app forwards to it
([Build Work Zone](/configuration/btp)). What an app can do inside a launchpad
- title, parameters, navigation to other apps - is on
[Fiori Launchpad](/configuration/launchpad).

## Operations

- **Draft cleanup and the Admin Cockpit.** The draft table cleans itself on
  every app start, as everywhere. The Admin Cockpit runs on ABAP Cloud, and its
  housekeeping is an application job there
  ([Monitoring](/configuration/monitoring#housekeeping-as-a-job)).
- **Compression.** On ABAP Cloud the platform compresses the responses
  ([Basis Runbook](/configuration/runbook#icm-http)).
- **Upgrades.** Pull the next tag with abapGit for Eclipse and activate what
  arrives inactive; the deployed app loads the frontend from the HTTP service
  and needs no new deployment
  ([Versioning, Upgrade](/configuration/versioning#how-to-upgrade)).
