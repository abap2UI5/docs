---
outline: [2, 4]
description: Which abap2UI5 tag or branch belongs to which system - S/4 Public Cloud, BTP ABAP Environment, S/4 Private Cloud, on-premise, NetWeaver from 7.02 - and whether business users can use the plain HTTP endpoint there.
---
# Deployment Scenarios
To install the framework, start with the [Quickstart](/get_started/quickstart). This page answers three questions per system: which release tag to pull, whether business users can use the HTTP endpoint directly, and whether you need the launchpad app as well.

abap2UI5 apps run directly behind an HTTP service. That is the simplest setup, and it works on every ABAP release and with both ABAP language versions (Standard ABAP and ABAP for Cloud). Only two things add work: an S/4 Public Cloud or BTP system, and the Fiori Launchpad.

## Which System, Which Setup

| System | What to pull | Can business users use the HTTP endpoint? | Launchpad app needed? | Guide |
|---|---|---|---|---|
| S/4 Public Cloud | branch `cloud`, with abapGit for Eclipse | no - the endpoint needs the `S_DEVELOP` authorization | yes, plus a tile | [S/4 Public Cloud](/configuration/s4_public_cloud) |
| BTP ABAP Environment | branch `cloud`, with abapGit for Eclipse | no - same as S/4 Public Cloud | yes, plus a tile | [ABAP Cloud](/configuration/abap_cloud) |
| S/4 Private Cloud | tag `X.Y.Z` | yes | only for the Fiori Launchpad | [Fiori Launchpad](/configuration/launchpad) |
| S/4 On-Premise | tag `X.Y.Z` | yes | only for the Fiori Launchpad | [Fiori Launchpad](/configuration/launchpad) |
| R/3 NetWeaver 7.50 and later | tag `X.Y.Z` | yes | only for the Fiori Launchpad, where the release has one | [Fiori Launchpad](/configuration/launchpad) |
| R/3 NetWeaver 7.02 to 7.4x | tag `X.Y.Z-702`, the downported build of the same release | yes | not available - the branch brings no launchpad app | [Downporting](/advanced/downporting) |

`X.Y.Z` is the number of the newest release; the [Quickstart](/get_started/quickstart#_1-install-the-framework-via-abapgit) says how to find it. The branch `cloud` has no release tags of its own ([below](#frontend-branches)).

## What a Pull Brings {#frontend-branches}

One pull of abap2UI5 is a running installation. From 1.151.0 (2026-11-25) on, the ICF node `/sap/bc/z2ui5`, its handler `z2ui5_cl_ui5_icf_handler` and the BSP `Z2UI5` for the launchpad are part of abap2UI5 itself (its package `src/03`). There is no second repository to pull.

The repository has three branches to install from. This table is the one place that says what each carries; the other pages link here:

| | `main` | `702` | `cloud` |
|---|---|---|---|
| System | S/4 Private Cloud, S/4 On-Premise, R/3 NetWeaver 7.50 and later | R/3 NetWeaver 7.02 to 7.4x | S/4 Public Cloud, BTP ABAP Environment |
| What to pull | tag `X.Y.Z` | tag `X.Y.Z-702` | the branch itself - it has no release tags |
| What abapGit pulls | `src/`: the framework (`src/00` to `src/02`) and the service package `src/03` | `src/`, downported to 7.02 syntax | `src/`: the same framework, with the ABAP Cloud service package in place of `src/03` |
| The service in `src/03` | the ICF node `/sap/bc/z2ui5`, its handler on `if_http_extension`, the BSP `Z2UI5` | the ICF node and its handler - no BSP: the launchpad app needs a newer UI5 than these releases ship | the HTTP service `Z2UI5` (`/sap/bc/http/sap/z2ui5`), its handler on `if_http_service_extension`, the IAM app and the business catalog |
| The launchpad app | the BSP, pulled with the rest | none | the Fiori project in `app/`, which you deploy ([S/4 Public Cloud](/configuration/s4_public_cloud#_4-deploy-the-ui5-app-optional)) |
| How it is published | the source: every change lands here | generated from `main` whenever the ABAP sources or the downport tooling change | generated from `main` on every push |
| After an abap2UI5 upgrade | nothing else to pull | nothing else to pull | nothing to deploy again |

Both generated branches carry the whole repository, not only what a system imports: each is the tree of `main` with the generated parts laid over it. abapGit reads `src/` and nothing else, so an installation does not notice. The branch `cloud` is always exactly one commit ahead of `main`. On it, two folders hold the app: `app/` is the one to deploy, with its backend URL set to the HTTP service. `app_source/` beside it is the source the BSP of `main` is built from and points at the ICF node - do not deploy that one to an ABAP Cloud system.

The BSP and the deployed cloud app carry no copy of the frontend: each is one view with the embed control, which loads the frontend from the node or the HTTP service at run time (`?z2ui5-bundle`), so they always run the backend's version. A tag `X.Y.Z` is no installation source for ABAP Cloud: its `src/03` holds the ICF node, a handler on `if_http_extension` and the BSP, none of which exists there.

::: tip Coming from the frontend repository
Until 1.151.0 (2026-11-25) the BSP and the cloud HTTP service came from a second repository, abap2UI5/frontend (branches `standard`, `standard_v2`, `standard_new`, `cloud`, `cloud_v2`, `cloud_new`). It is retired. Those branches stay available, frozen at 1.150.0. If an abapGit repository on your system still points at one of them, remove that repository from abapGit **without deleting its objects**, then pull abap2UI5 (`main`, `702` or `cloud`). The node, the BSP, the HTTP service and the IAM objects keep their names, so a target mapping or a tile stays as it is; the old handler `Z2UI5_CL_LP_HANDLER` is named by nothing after the pull and can be deleted. If it held the legacy objects instead - the view builder, popups and utility classes the core no longer ships, which were there from 2026-11-17 to 2026-11-25 - change its URL to [abap2UI5-ext](https://github.com/abap2UI5/abap2UI5-ext) and pull; see [Deprecations](/resources/deprecations#utility-classes).
:::

## S/4 Public Cloud
In `S/4 Public Cloud`, the HTTP endpoint needs `S_DEVELOP` authorization, so only developers can use it. To make an app reachable for business users, deploy the app of the branch `cloud` and add a tile. See [S/4 Public Cloud](/configuration/s4_public_cloud) for the steps, and [ABAP Cloud](/configuration/abap_cloud) for what else differs there.

## S/4 Private Cloud
In `S/4 Private Cloud`, business users can use the HTTP endpoint, and you manage its authorizations as for any service. To also use the SAP Fiori Launchpad, use the BSP `Z2UI5` the pull brings. See [Fiori Launchpad](/configuration/launchpad).

## S/4 On-Premise
The same as `S/4 Private Cloud`. Follow the [Fiori Launchpad](/configuration/launchpad) guide.

## R/3 NetWeaver
The same as `S/4 Private Cloud`. Whether you can add the apps to a Launchpad depends on your release. Follow the [Fiori Launchpad](/configuration/launchpad) guide.

## R/3 NetWeaver (<7.50)
Below 7.50, install the downported version: the same release with its syntax rewritten for older systems. abap2UI5 runs on releases as far back as 7.02. The tag to pull is `X.Y.Z-702`. It brings the ICF node `/sap/bc/z2ui5` with its handler, but no BSP: the launchpad app needs a newer UI5 than these releases ship. See [Downporting](/advanced/downporting).

## BTP ABAP Environment
The same as `S/4 Public Cloud`: both are ABAP Cloud. [ABAP Cloud](/configuration/abap_cloud) says what differs from Standard ABAP and links each step; the screenshots of the installation are on [S/4 Public Cloud](/configuration/s4_public_cloud).
