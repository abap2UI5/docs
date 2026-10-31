---
outline: [2, 4]
description: The legacy-free variant of the abap2UI5 frontend - the v2 branches of the frontend repository bootstrapped from OpenUI5's legacy-free distribution, what that distribution leaves out, and how to install it next to the classic frontend.
---
# UI5 Legacy-Free

The abap2UI5 frontend is also available as a **legacy-free** variant,
bootstrapped from the legacy-free distribution of OpenUI5 — the build without
`jQuery.sap.*`, synchronous APIs and the other deprecated globals. Which
version it loads is pinned in the frontend's bootstrap
(`1.142.0-legacy-free` in `tools/app2app_v2/patch-v2.mjs` of the framework
repository today); the branches below carry it.

## What is UI5 Legacy-Free?

`*-legacy-free` is a parallel distribution of OpenUI5/SAPUI5 (starting with the
1.136.x line) that ships the same controls and APIs as the regular 1.x build,
but with everything deprecated removed up front. In practice that means no
`jQuery.sap.*`, no `sap.ui.getCore()`, no synchronous module loading
(`sap.ui.requireSync`, sync XHR), no global view/controller factories, no jQuery
bundled into the core, and none of the compatibility shims that 1.x kept around
for backwards compatibility.

UI5 2.x enforces exactly this surface, so the legacy-free build is a preview
of it on a 1.x release: what runs on it runs on 2.x, and what breaks on it
would break on 2.x. The abap2UI5 frontend and its custom controls are held
to that surface by the `UI5_2X` check of the framework's CI, and the v2
branches are how an installation runs on it today.

## Installation

The legacy-free frontend ships as the `v2` branches of the
[frontend repository](https://github.com/abap2UI5/frontend). Pull the branch
that matches your system into your ABAP system with
[abapGit](https://abapgit.org) and activate — there is no separate build step:

| Branch | System | UI5 |
|--------------|-------------------------------------------------------|-----------------------------|
| `cloud_v2` | S/4 Public Cloud, BTP ABAP Environment | legacy-free (UI5 2.x preview) |
| `standard_v2` | S/4 Private Cloud, S/4 On-Premise | legacy-free (UI5 2.x preview) |

The v2 branches install the frontend under the same `z2ui5` name as the classic
branches. To evaluate the legacy-free frontend side by side with the classic one
in the same system, run abap2UI5's
[`frontend_deploy`](https://github.com/abap2UI5/abap2UI5/actions/workflows/frontend_deploy.yaml)
workflow with a branch name of the form `standard_v2_<name>`: it generates that
branch of the frontend repository with the whole deployment identity (BSP, ICF
nodes, handler class) under a different name. This is for the BSP of
`standard_v2`; the [frontend repository](https://github.com/abap2UI5/frontend)
has the details.

## Learn More

- [Available OpenUI5 versions](https://sdk.openui5.org/versionoverview.html) (incl. `*-legacy-free` builds)
- [Explore SAPUI5/OpenUI5 1.136.x-legacy-free (SAP Community)](https://community.sap.com/t5/technology-blog-posts-by-sap/explore-sapui5-openui5-1-136-x-legacy-free/ba-p/14103271)
- [Adapt your app to SAPUI5 2.x (SAP Community)](https://community.sap.com/t5/technology-blog-posts-by-sap/adapt-your-app-to-sapui5-2-x-make-an-impact-by-joining-our-cei-project/ba-p/13722775)
