---
outline: [2, 4]
---
# UI5 Legacy-Free

The abap2UI5 frontend is also available as a **legacy-free** variant, bootstrapped from the new legacy-free distribution of OpenUI5 (`1.142.0-legacy-free`) — the build without `jQuery.sap.*`, synchronous APIs, and other deprecated globals. It is essentially a preview of the API surface that UI5 2.x will enforce.

### What is UI5 Legacy-Free?

`*-legacy-free` is a parallel distribution of OpenUI5/SAPUI5 (starting with the 1.136.x line) that ships the same controls and APIs as the regular 1.x build, but with everything deprecated removed up front. In practice that means no `jQuery.sap.*`, no `sap.ui.getCore()`, no synchronous module loading (`sap.ui.requireSync`, sync XHR), no global view/controller factories, no jQuery bundled into the core, and none of the compatibility shims that 1.x kept around for backwards compatibility.

### Why It Matters for the Future

UI5 2.x — the next major release line — will enforce exactly this surface. By developing against the legacy-free build today, the project gets several things at once:

- **A real-world preview of UI5 2.x.** Anything that still works here will keep working on 2.x; anything that breaks is something that would have broken on 2.x anyway, just discovered earlier and on our terms.
- **No future migration cliff.** The painful "rip out jQuery.sap and sync APIs" step that most UI5 1.x apps will eventually face is done incrementally as part of normal development, not as a one-shot upgrade project.
- **A leaner, faster runtime.** Dropping jQuery and the compatibility layers shrinks the bootstrap, speeds up startup, and reduces memory footprint — particularly noticeable on mobile and constrained networks.
- **Modern, async-first patterns.** Async module loading, async component and view creation, and standard `Promise`-based APIs are the only option, which keeps the codebase aligned with the current web platform instead of with patterns from the early 2010s.
- **Cleaner extension points for abap2UI5.** Custom controls and the frontend glue code can rely on stable, documented APIs only, without reaching into internals that are scheduled for removal.

In short: the legacy-free build is the bridge between today's UI5 1.x ecosystem and tomorrow's UI5 2.x. Building the abap2UI5 frontend on it now means the framework is ready for 2.x the day it lands, instead of chasing it afterwards.

### Installation

The legacy-free frontend ships as the `v2` branches of the [frontend repository](https://github.com/abap2UI5/frontend). Pull the branch that matches your system into your ABAP system with [abapGit](https://abapgit.org) and activate — there is no separate build step:

| Branch | System | UI5 |
|--------------|-------------------------------------------------------|-----------------------------|
| `cloud_v2` | S/4 Public Cloud, BTP ABAP Environment | legacy-free (UI5 2.x preview) |
| `standard_v2` | S/4 Private Cloud, S/4 On-Premise | legacy-free (UI5 2.x preview) |

The v2 branches install the frontend under the same `z2ui5` name as the classic branches. To evaluate the legacy-free frontend side by side with the classic one in the same system, use the frontend repository's `build_rename` workflow to generate a branch with the whole deployment identity (BSP, ICF nodes, handler class) under a different name — see the [frontend repository](https://github.com/abap2UI5/frontend) for details.

::: warning Obsolete Repository
The former separate repository `abap2UI5/frontend-legacy-free` is obsolete. The legacy-free frontend is now maintained on the `*_v2` branches of the [frontend repository](https://github.com/abap2UI5/frontend), generated from the same single source as the classic variant.
:::

### Why Try the v2 Version?

- **Future-proof.** Apps already run on what UI5 2.x will require — no big-bang migration later.
- **Smaller and faster.** No jQuery and no compatibility layers means a leaner core and quicker startup.
- **Cleaner foundation.** Modern async patterns, no hidden globals, better alignment with current web standards.

### Learn More

- [Available OpenUI5 versions](https://sdk.openui5.org/versionoverview.html) (incl. `*-legacy-free` builds)
- [Explore SAPUI5/OpenUI5 1.136.x-legacy-free (SAP Community)](https://community.sap.com/t5/technology-blog-posts-by-sap/explore-sapui5-openui5-1-136-x-legacy-free/ba-p/14103271)
- [Adapt your app to SAPUI5 2.x (SAP Community)](https://community.sap.com/t5/technology-blog-posts-by-sap/adapt-your-app-to-sapui5-2-x-make-an-impact-by-joining-our-cei-project/ba-p/13722775)
