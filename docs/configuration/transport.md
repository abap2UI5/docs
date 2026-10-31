---
outline: [2, 4]
description: Which objects of an abap2UI5 installation travel from development to production, in which order, what is set per system, and what differs on ABAP Cloud and on a 7.02 system.
---
# Transport, Landscape

abap2UI5 is installed once, with abapGit, on the development system. From
there everything reaches quality and production through the transport
process the landscape already has - the framework is a package of workbench
objects like any other. This page is about what travels, in which order, and
what does not travel and is set per system instead.

## What Travels

| Object | Type | Travels? |
|---|---|---|
| The framework package with its sub-packages (`src/00` to `src/02` and `src/99`) | classes, interfaces, one table | yes - one transport, the whole package tree |
| `Z2UI5_T_01` | transparent table, delivery class A, client-dependent | the definition travels; its rows do not and must not - they are the drafts of the system they were written on |
| Your HTTP handler class | class in your package | yes |
| The ICF node | `SICF` service | see below |
| Your user exit class | class in your package | yes - its settings are code, and that is the point: the same exit runs on every system |
| BSP `Z2UI5` (launchpad only) | BSP application in the UI5 ABAP repository | yes, like any UI5 app deployed to the repository |
| The Admin Cockpit (optional) | package of its own, tables `Z2UI5_T_CK_*` | yes; its administrators and settings are table rows and are set per system |
| Your apps | one class per app, plus the texts, tables and other objects it uses | yes |

Pull the framework with the folder logic `PREFIX` that its `.abapgit.xml`
declares; the sub-package names are derived from the root package you pick,
and a long root name leaves the deeper ones no room (the package name is
capped at 30 characters) - [S/4 Public Cloud](/configuration/s4_public_cloud)
shows the dialog.

## The Order

1. **The framework.** The package tree, in one request. It has no
   dependency on anything of yours.
2. **The handler class and the exit class.** They depend on the framework
   (`z2ui5_cl_ui5_http_handler`, `z2ui5_if_ui5_exit`), so they follow it -
   in the same request, or a later one.
3. **The ICF node.** Create or import it, and activate it: an imported node
   is not active until somebody activates it on that system.
4. **The frontend BSP**, when the launchpad is used, and the tile
   configuration - [Fiori Launchpad](/configuration/launchpad).
5. **A Hello World check** on the target system before any app arrives:
   open the node, press *check* on the start page, start the bundled test
   app. That verifies pull, handler, node, exit and table at once - the same
   check as the Quickstart's, on the production system.
6. **Your apps.** One class per app; whatever else the app uses - message
   classes, tables, CDS views, function modules - travels with it as it would
   for a report.

An upgrade is the same list again, minus the node:
[Versioning, Upgrade](/configuration/versioning).

## The ICF Node

The node is where the two kinds of settings meet. Its handler list names your
class, which is workbench code and the same everywhere. Everything else on
the node - whether it is active, the logon procedure, the authorization
field, SSL requirements - is set on the system it runs on:

- A service is a workbench object of its own (abapGit serializes one, and
  `SICF` can put one into a transport request), and many landscapes create
  the node by hand on every system instead - check your landscape's
  convention. Either way, **activation is per system**: after an import the
  node is inactive until it is activated there.
- The authorization field of the node is checked against `S_ICF` and is the
  outer gate of the installation - [Authorization](/configuration/authorization).
- The logon procedure and the SSO setup are standard ICF and belong to the
  Basis team's landscape conventions - [Basis Runbook](/configuration/runbook#the-icf-node).

The launchpad frontend branch pulls more than the BSP: next to it come an
ICF node `z2ui5` and its handler class `z2ui5_cl_lp_handler`, as abapGit
objects of the same package, so they travel with the BSP; the cloud branches
ship the HTTP service and the IAM artifacts the same way. Activation of that
node is per system as well.

## Per-System Settings

The user exit is one class, so its values are the same on every system
unless the exit decides otherwise. What legitimately differs between
development, quality and production:

| Setting | Where | Why it may differ |
|---|---|---|
| `cs_config-src`, the UI5 bootstrap source | `set_config_http_get` | production loads UI5 from the system itself or a controlled host, a sandbox may use the CDN - [Bootstrapping](/configuration/setup/ui5_bootstrapping) |
| `cs_config-check_hide_error_details` | `set_config_http_post` | on in production; developers want the exception chain on the development system |
| the CSP and `t_security_header` | `set_config_http_get` | a host that only production loads from, a header a proxy already sets |
| `cs_config-check_trust_forwarded_host` | `set_config_http_post` | off on a system without a reverse proxy, on behind the web dispatcher - [Security](/configuration/security#cross-site-request-forgery-csrf) |
| `cs_config-check_monitor_active` | `set_config_http_post` | on where the cockpit's package is installed and active |

An exit that needs different values per system reads `sy-sysid` or a
customizing table of your own; the framework offers no per-system switch of
its own. Keep the branch small - an exit that is wrong on one system is a
production-only bug, and the cockpit's *Installation & Security* tab shows
what the exit produced on the system it runs on.

## Parallel Installations

Two installations of abap2UI5 cannot share a package name or an object name:
every object abapGit pulls is global in the system. To keep production apps
on one release while new development moves to another, install the framework
a second time under another prefix with the renaming build - each copy has
its own package, draft table, node and exit, and they travel separately -
[Namespaces, Renaming](/advanced/renaming).

## ABAP Cloud

On S/4 Public Cloud and BTP ABAP Environment the same objects exist, the
mechanics differ:

- abapGit for Eclipse (ADT) pulls the repository into a package that belongs
  to a **software component**; the software component is what moves through
  the landscape, and abap2UI5 travels inside it with everything else in that
  package - [S/4 Public Cloud](/configuration/s4_public_cloud).
- The HTTP service is a development object of the package, created in ADT
  (the handler class implements `if_http_service_extension`), so it travels
  with the component rather than being created per system.
- On S/4 Public Cloud the endpoint itself needs `S_DEVELOP`, which rules it
  out for business users; the way to them is the frontend app and a tile -
  [Deployment Scenarios](/configuration/installation#s-4-public-cloud).
- Stateful sessions do not exist there; nothing of
  [Statefulness](/cookbook/expert_more/statefulness) applies.

## A 7.02 System

Below 7.50 the installation is the `702` branch - the same framework, its
syntax rewritten by abaplint on every push - and the tag to pin is
`X.Y.Z-702` ([Downporting](/advanced/downporting)). What differs in the
transport:

- The handler class is written in 7.02 syntax; the Quickstart's snippet uses
  constructs the release does not have.
- The frontend repository lists no branch for a release below 7.50; apps
  run from the node, or are reached from a newer system through the
  [RFC](/advanced/rfc) or [HTTP](/advanced/http) connector.
- The draft table and the user exit are the same; so is the order above.
