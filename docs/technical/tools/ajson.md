---
outline: [2, 4]
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

What ships under `z2ui5_cl_ajson` is a **mirrored copy** of an external project (`src/00/01`), not a contract abap2UI5 owns. It sits outside the released API, it is resynced from upstream automatically (see below), and a resync is free to change it. An app that calls it binds itself to whatever the mirror looks like today — which is why the abap2UI5 linter reports such a call as `non-released-api`, correctly.

::: warning Do not call it from app code
Nothing in an app class should name `z2ui5_cl_ajson`, `z2ui5_cx_ajson_error`, `z2ui5_if_ajson_mapping` or `z2ui5_if_ajson_filter`. There is no released JSON parser to use instead, and that is deliberate — see [JSON is built and read by hand](/resources/deprecations#json-is-built-and-read-by-hand) for what to write in both directions.
:::

Nor is there a substitute to reach for: `/ui2/cl_json` is not released for ABAP Cloud and `xco_cp_json` is missing on 7.02 — the two reasons the framework vendored a library in the first place.

## Automatic Updates

Every ajson update and bug fix flows into abap2UI5 automatically via GitHub Actions and the [mirror-ajson](https://github.com/abap2UI5/mirror-ajson) repository. You always run the latest stable version — no manual steps needed.

That automation is the other half of the argument above: the copy in your system moves without anybody in this project reviewing the diff for app-facing impact, because nothing app-facing is supposed to depend on it.
