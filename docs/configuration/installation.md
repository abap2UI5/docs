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
| S/4 Public Cloud | tag `X.Y.Z`, with abapGit for Eclipse | no - the endpoint needs the `S_DEVELOP` authorization | yes, plus a tile | [S/4 Public Cloud](/configuration/s4_public_cloud) |
| BTP ABAP Environment | tag `X.Y.Z`, with abapGit for Eclipse | no - same as S/4 Public Cloud | yes, plus a tile | [ABAP Cloud](/configuration/abap_cloud) |
| S/4 Private Cloud | tag `X.Y.Z` | yes | only for the Fiori Launchpad | [Fiori Launchpad](/configuration/launchpad) |
| S/4 On-Premise | tag `X.Y.Z` | yes | only for the Fiori Launchpad | [Fiori Launchpad](/configuration/launchpad) |
| R/3 NetWeaver 7.50 and later | tag `X.Y.Z` | yes | only for the Fiori Launchpad, where the release has one | [Fiori Launchpad](/configuration/launchpad) |
| R/3 NetWeaver 7.02 to 7.4x | tag `X.Y.Z-702`, the downported build of the same release | yes | only for the Fiori Launchpad, where the release has one | [Downporting](/advanced/downporting) |

`X.Y.Z` is the number of the newest release; the [Quickstart](/get_started/quickstart#_1-install-the-framework-via-abapgit) says how to find it.

## What a Pull Brings {#frontend-branches}

One pull of abap2UI5 is a running installation. Since 2026-11-17 the ICF node `/sap/bc/z2ui5`, its handler `z2ui5_cl_ui5_icf_handler` and the BSP `Z2UI5` for the launchpad are part of abap2UI5 itself (its package `src/03`); a release tag carries them from 1.151.0 on. There is no second repository to pull:

| Branch | System | What it brings besides the framework | After an abap2UI5 upgrade |
|---|---|---|---|
| `main` (tag `X.Y.Z`) | S/4 Private Cloud, S/4 On-Premise, R/3 NetWeaver 7.50 and later | the ICF node, its handler and the BSP `Z2UI5` | nothing else to pull |
| `702` (tag `X.Y.Z-702`) | R/3 NetWeaver 7.02 to 7.4x | the same, downported | nothing else to pull |
| `cloud` | S/4 Public Cloud, BTP ABAP Environment | the HTTP service `Z2UI5` with its handler and the IAM objects, plus the app as a Fiori project in `app/` to deploy | nothing to deploy again |

The BSP and the deployed cloud app carry no copy of the frontend: each is one view with the embed control, which loads the frontend from the node or the HTTP service at run time, so they always run the backend's version. The branch `cloud` is generated from `main` on every push; it has no release tags of its own.

::: tip Coming from the frontend repository
Until 2026-11-17 the BSP and the cloud HTTP service came from a second repository, abap2UI5/frontend (branches `standard`, `standard_v2`, `standard_new`, `cloud`, `cloud_v2`, `cloud_new`). It is retired. If an abapGit repository on your system still points at one of its branches, remove that repository from abapGit **without deleting its objects**, then pull abap2UI5 (`main`, `702` or `cloud`). The node, the BSP and the HTTP service keep their names, so a target mapping or a tile stays as it is; the old handler `Z2UI5_CL_LP_HANDLER` is named by nothing after the pull and can be deleted. If it held the legacy objects instead - the view builder, popups and utility classes the core no longer ships, which were there from 2026-11-17 to 2026-11-25 - change its URL to [abap2UI5-ext](https://github.com/abap2UI5/abap2UI5-ext) and pull; see [Deprecations](/resources/deprecations#utility-classes).
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
Below 7.50, install the downported version: the same release with its syntax rewritten for older systems. abap2UI5 runs on releases as far back as 7.02. The tag to pull is `X.Y.Z-702`. See [Downporting](/advanced/downporting).

## BTP ABAP Environment
The same as `S/4 Public Cloud`: both are ABAP Cloud. [ABAP Cloud](/configuration/abap_cloud) says what differs from Standard ABAP and links each step; the screenshots of the installation are on [S/4 Public Cloud](/configuration/s4_public_cloud).
