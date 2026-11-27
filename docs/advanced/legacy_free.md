---
outline: [2, 4]
description: The legacy-free variant of the abap2UI5 frontend - bootstrapped from OpenUI5's legacy-free distribution, what that distribution leaves out, and why there is no build of it to install at the moment.
---
# UI5 Legacy-Free

The abap2UI5 frontend had a **legacy-free** variant until 2026-11,
bootstrapped from the legacy-free distribution of OpenUI5 — the build without
`jQuery.sap.*`, synchronous APIs and the other deprecated globals. Which
version it loads is pinned in its bootstrap patch (`1.142.0-legacy-free` in
`tools/app2app_v2/patch-v2.mjs` of the framework repository).

## What is UI5 Legacy-Free?

`*-legacy-free` is a parallel distribution of OpenUI5/SAPUI5 (starting with the
1.136.x line) that ships the same controls and APIs as the regular 1.x build,
but with everything deprecated removed up front. In practice these are gone:

- `jQuery.sap.*` and `sap.ui.getCore()`
- synchronous module loading (`sap.ui.requireSync`, sync XHR)
- the global view and controller factories
- the jQuery bundled into the core
- the compatibility shims that 1.x kept for backwards compatibility

UI5 2.x enforces exactly this surface, so the legacy-free build is a preview
of it on a 1.x release: what runs on it runs on 2.x, and what breaks on it
would break on 2.x. The abap2UI5 frontend and its custom controls are held
to that surface by the `UI5_2X` check of the framework's CI.

## Installation

There is no legacy-free build to install at the moment. It shipped as the
branches `standard_v2` and `cloud_v2` of abap2UI5/frontend, a second
repository that is retired: from 1.151.0 (2026-11-25) on the BSP and the cloud
app are part of abap2UI5 itself
([Deployment Scenarios](/configuration/installation#frontend-branches)), and
the v2 branches are no longer built. The bootstrap patch stays in the
framework repository until it is decided whether the variant comes back.

An installation that pulled `standard_v2` or `cloud_v2` switches the way every
frontend branch does
([Coming from the frontend repository](/configuration/installation#frontend-branches)).
It then runs the classic UI5 build again.

## Learn More

- [Available OpenUI5 versions](https://sdk.openui5.org/versionoverview.html) (incl. `*-legacy-free` builds)
- [Explore SAPUI5/OpenUI5 1.136.x-legacy-free (SAP Community)](https://community.sap.com/t5/technology-blog-posts-by-sap/explore-sapui5-openui5-1-136-x-legacy-free/ba-p/14103271)
- [Adapt your app to SAPUI5 2.x (SAP Community)](https://community.sap.com/t5/technology-blog-posts-by-sap/adapt-your-app-to-sapui5-2-x-make-an-impact-by-joining-our-cei-project/ba-p/13722775)
