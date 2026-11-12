---
outline: [2, 4]
description: Which abap2UI5 tag and which frontend branch belong to which system - S/4 Public Cloud, BTP ABAP Environment, S/4 Private Cloud, on-premise, NetWeaver from 7.02 - and whether business users can use the plain HTTP endpoint there.
---
# Deployment Scenarios
To install the framework, start with the [Quickstart](/get_started/quickstart). This page answers three questions per system: which release tag to pull, whether business users can use the HTTP endpoint directly, and whether you need a frontend app as well.

abap2UI5 apps run directly behind an HTTP service. That is the simplest setup, and it works on every ABAP release and with both ABAP language versions (Standard ABAP and ABAP for Cloud). Only two things add work: an S/4 Public Cloud or BTP system, and the Fiori Launchpad.

## Which System, Which Setup

| System | What to pull | Can business users use the HTTP endpoint? | Frontend app needed? | Guide |
|---|---|---|---|---|
| S/4 Public Cloud | tag `X.Y.Z`, with abapGit for Eclipse | no - the endpoint needs the `S_DEVELOP` authorization | yes, plus a tile | [S/4 Public Cloud](/configuration/s4_public_cloud) |
| BTP ABAP Environment | tag `X.Y.Z`, with abapGit for Eclipse | no - same as S/4 Public Cloud | yes, plus a tile | [S/4 Public Cloud](/configuration/s4_public_cloud) |
| S/4 Private Cloud | tag `X.Y.Z` | yes | only for the Fiori Launchpad | [Fiori Launchpad](/configuration/launchpad) |
| S/4 On-Premise | tag `X.Y.Z` | yes | only for the Fiori Launchpad | [Fiori Launchpad](/configuration/launchpad) |
| R/3 NetWeaver 7.50 and later | tag `X.Y.Z` | yes | only for the Fiori Launchpad, where the release has one | [Fiori Launchpad](/configuration/launchpad) |
| R/3 NetWeaver 7.02 to 7.4x | tag `X.Y.Z-702`, the downported build of the same release | yes | only for the Fiori Launchpad, where the release has one | [Downporting](/advanced/downporting) |

`X.Y.Z` is the number of the newest release; the [Quickstart](/get_started/quickstart#_1-install-the-framework-via-abapgit) says how to find it.

## Which Frontend Branch {#frontend-branches}

Skip this section when the answer in the column *Frontend app needed?* is no for you: the HTTP endpoint serves the frontend itself.

A frontend app is the abap2UI5 frontend as an app in the UI5 ABAP repository. Pull one branch of the [frontend repository](https://github.com/abap2UI5/frontend) with abapGit:

| Branch | System | What it carries | After an abap2UI5 upgrade |
|---|---|---|---|
| `standard` | S/4 Private Cloud, S/4 On-Premise, R/3 NetWeaver 7.50 and later | a copy of the frontend | pull this branch again |
| `standard_v2` | S/4 Private Cloud, S/4 On-Premise | a copy of the [legacy-free](/advanced/legacy_free) frontend (UI5 2.x) | pull this branch again |
| `standard_new` | S/4 Private Cloud, S/4 On-Premise, R/3 NetWeaver 7.50 and later | no copy: one view with the embed control, which loads the frontend from the backend | nothing to do |
| `cloud` | S/4 Public Cloud, BTP ABAP Environment | a copy of the frontend | pull this branch again |
| `cloud_v2` | S/4 Public Cloud, BTP ABAP Environment | a copy of the legacy-free frontend (UI5 2.x) | pull this branch again |
| `cloud_new` | S/4 Public Cloud, BTP ABAP Environment | no copy: the embed control | nothing to do |

Every branch keeps the same objects, names and endpoints, so a system switches between them by pulling another branch into the same abapGit repository. The `_new` branches need an abap2UI5 newer than 1.146.0.

## S/4 Public Cloud
In `S/4 Public Cloud`, the HTTP endpoint needs `S_DEVELOP` authorization, so only developers can use it. To make an app reachable for business users, install a frontend app and add a tile. See [S/4 Public Cloud](/configuration/s4_public_cloud) for the steps.

## S/4 Private Cloud
In `S/4 Private Cloud`, business users can use the HTTP endpoint, and you manage its authorizations as for any service. To also use the SAP Fiori Launchpad, install a frontend app. See [Fiori Launchpad](/configuration/launchpad).

## S/4 On-Premise
The same as `S/4 Private Cloud`. Follow the [Fiori Launchpad](/configuration/launchpad) guide.

## R/3 NetWeaver
The same as `S/4 Private Cloud`. Whether you can add the apps to a Launchpad depends on your release. Follow the [Fiori Launchpad](/configuration/launchpad) guide.

## R/3 NetWeaver (<7.50)
Below 7.50, install the downported version: the same release with its syntax rewritten for older systems. abap2UI5 runs on releases as far back as 7.02. The tag to pull is `X.Y.Z-702`. See [Downporting](/advanced/downporting).

## BTP ABAP Environment
The same as `S/4 Public Cloud`. Follow the [S/4 Public Cloud](/configuration/s4_public_cloud) guide.
