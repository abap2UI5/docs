---
outline: [2, 4]
description: ajson, the open-source JSON library abap2UI5 ships under its own namespace for every request and response, and why it replaced /UI2/CL_JSON.
---
# ajson

abap2UI5 handles all frontend-backend communication with JSON. Initially, the framework used SAP's `/UI2/CL_JSON`, but that approach turned out to be problematic: key behavior varied across releases, documentation was sparse, and compatibility issues came up often.

The solution: [ajson](https://github.com/sbcgua/ajson), a solid open-source JSON library that greatly improved abap2UI5's functionality.

**Why ajson?**
- **Zero SAP API Dependencies** — No reliance on release-specific SAP classes
- **Broad Compatibility** — Works smoothly from NW 7.02 to ABAP Cloud
- **Developer-Friendly** — Clear API with thorough documentation
- **Active Maintenance** — Responsive issue resolution and ongoing improvement

## It is the framework's engine, not an API for your app

ajson is what serializes the model on every roundtrip and what reads the delta back — `z2ui5_cl_ui5_srv_model` is built on it, and so is `_bind( json = abap_true )`. That is where it belongs.

What ships under `z2ui5_cl_ui5_ajson` is a **vendored copy** of an external project (`src/00/01`), not a contract abap2UI5 owns. It sits outside the released API, it is resynced from upstream automatically (see below), and a resync is free to change it. An app that calls it binds itself to whatever the mirror looks like today — which is why the abap2UI5 linter reports such a call as `non-released-api`, correctly.

::: warning Do not call it from app code
Nothing in an app class should name `z2ui5_cl_ui5_ajson`, `z2ui5_cx_ui5_ajson_error`, `z2ui5_if_ui5_ajson_map` or `z2ui5_if_ui5_ajson_filter` - nor their names before November 2026, `z2ui5_cl_ajson` and the rest. There is no released JSON parser to use instead, and that is deliberate — see [JSON is built and read by hand](/resources/deprecations#json-is-built-and-read-by-hand) for what to write in both directions.
:::

Nor is there a substitute to reach for: `/ui2/cl_json` is not released for ABAP Cloud and `xco_cp_json` is missing on 7.02 — the two reasons the framework vendored a library in the first place.

## Automatic Updates

ajson updates and bug fixes reach abap2UI5 through the [mirror-ajson](https://github.com/abap-util/mirror-ajson) repository of [`abap-util`](https://github.com/abap-util/abap-util), which renames ajson to the `zabaputil_` prefix and applies a few local patches. Once a month a GitHub Actions workflow of the framework takes the objects abap2UI5 uses from there, renames them to its own names (`zabaputil_cl_ajson` becomes `z2ui5_cl_ui5_ajson`) and opens a pull request with the difference. `abap-util` vendors the same mirror, so the two copies differ only in their names. A release carries the copy that was merged before it, with no manual steps on your side.

That automation is the second reason for the rule above. The copy in your system changes without anybody in this project reviewing the diff for its effect on apps, because no app is supposed to depend on it.
