---
outline: [2, 4]
---
# Bootstrapping

Bootstrapping is the process of loading the UI5 runtime into the browser. abap2UI5 generates a `<script src="…/sap-ui-core.js">` tag in the page `<head>`; the URL decides which UI5 version is loaded, from which delivery channel (CDN or your own server) and whether the cache-buster is active.

## Pick a Bootstrap Source

```abap
METHOD z2ui5_if_exit~set_config_http_get.

    cs_config-src = `https://ui5.sap.com/1.116.0/resources/sap-ui-core.js`.

ENDMETHOD.
```

Without an override, abap2UI5 uses the OpenUI5 cache-buster URL — i.e. the current latest OpenUI5 build (see [Pinning a Version](#pinning-a-version) below before relying on this in production).

## Common Sources

| URL                                                                       | When to use it |
|---------------------------------------------------------------------------|----------------|
| `https://sdk.openui5.org/resources/sap-ui-cachebuster/sap-ui-core.js`     | OpenUI5 latest, cache-buster enabled — the abap2UI5 default |
| `https://sdk.openui5.org/1.120.0/resources/sap-ui-core.js`                | OpenUI5, pinned version |
| `https://ui5.sap.com/resources/sap-ui-core.js`                            | SAPUI5 latest |
| `https://ui5.sap.com/1.120.0/resources/sap-ui-core.js`                    | SAPUI5, pinned version |
| `https://sapui5.hana.ondemand.com/1.120.0/resources/sap-ui-core.js`       | SAPUI5 legacy host name (still works, but `ui5.sap.com` is the preferred alias) |
| `/sap/public/bc/ui5_ui5/resources/sap-ui-core.js`                         | Locally hosted UI5 on the same SAP system |
| `https://ui5.sap.com/1.71/resources/sap-ui-core.js`                         | Oldest supported version |

::: warning Older releases need a CSP adjustment
The default Content Security Policy does not allow `eval()`, but the module loader of older UI5 releases (such as `1.71`) still relies on it. Bootstrapping such a release with the default CSP fails with an `EvalError` in the browser console ("Failed to load component for container container"). Re-add `'unsafe-eval'` to the CSP in the same exit where you set the bootstrap source — see [Bootstrapping Older UI5 Releases](/configuration/security#bootstrapping-older-ui5-releases).
:::

### OpenUI5 vs SAPUI5

- **OpenUI5** (`sdk.openui5.org`) is the Apache-licensed open-source subset of UI5. It contains all libraries a typical abap2UI5 app uses: `sap.m`, `sap.ui.core`, `sap.ui.layout`, `sap.ui.table`, `sap.ui.unified`, `sap.uxap`, `sap.f`, `sap.tnt`, `sap.viz`.
- **SAPUI5** (`ui5.sap.com`) is the SAP-licensed superset. It adds e.g. `sap.ui.comp` (Smart Controls, `ValueHelpDialog`), `sap.suite.ui.commons`, `sap.ui.generic.app` and the Fiori Elements floorplans.

If you only need controls available in OpenUI5 — which covers the vast majority of abap2UI5 apps — stick with `sdk.openui5.org`. Switch to `ui5.sap.com` only when you genuinely need a `sap.ui.comp.*` control or a SAPUI5-only library.

### Pinning a Version

Hard-coding a version (`/1.116.0/`) keeps your app stable when SAP ships a new patch. Without a version the CDN serves the current "evergreen" release, which is convenient but can break the app the next time SAP rolls out a change. For productive apps, pin the version explicitly.

### Cache-Buster

URLs containing `sap-ui-cachebuster` append a versioned token to every asset URL, so the browser cache is invalidated automatically whenever SAP releases a new build. Use it when you intentionally stay on the latest release.

### Local Hosting

If your gateway has the UI5 ABAP repository (`/sap/public/bc/ui5_ui5/resources/`) installed, you can serve UI5 from your own SAP system instead of a CDN. This is faster on the intranet, works offline and avoids any external dependency, but you need a basis admin to keep the UI5 version current. See [UI5 Version](/configuration/ui5_versions) for the available approaches.

## See Also

- Official SAP documentation on [bootstrapping variants](https://sapui5.hana.ondemand.com/#/topic/2d3eb2f322ea4a82983c1c62a33ec4ae) and [the supported tags](https://sapui5.hana.ondemand.com/#/topic/91f2cebe7c8e4d289fd80a4f0c0bd2ca).
- [Bootstrap Attributes](/configuration/setup/bootstrap_attributes) — how to add extra `data-sap-ui-*` parameters next to `src`.
