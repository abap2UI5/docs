---
outline: [2, 4]
description: abaplint in abap2UI5 - the static analysis on every pull request, the automatic 7.02 downport and the namespace renaming, with the configuration files each of them reads.
---
# abaplint

This project uses [abaplint](https://abaplint.app/) for three jobs: the static analysis on every pull request, the 7.02 downport and the namespace renaming. Each job reads a configuration file of its own, all in the [framework repository](https://github.com/abap2UI5/abap2UI5).

## Static Code Analysis

On every pull request, abaplint checks the ABAP code several times over, once per platform the framework runs on. No single profile can do it: the released APIs of ABAP Cloud, the classic ICF interface and the 7.02 syntax do not fit into one set of dependencies.

| Configuration | What it lints | As |
|---|---|---|
| [`abaplint.jsonc`](https://github.com/abap2UI5/abap2UI5/blob/main/abaplint.jsonc) | `src/00` to `src/02`, the framework | Standard ABAP 7.50; the file an editor plugin and the [dashboard](https://abaplint.app/stats/abap2UI5/abap2UI5) read |
| `.github/abaplint/abap_standard.jsonc` | all of `src/` except the ICF handler | Standard ABAP 7.50 |
| `.github/abaplint/abap_cloud.jsonc` | `src/00` to `src/02` | ABAP Cloud: released APIs only, no classic statements |
| `.github/abaplint/abap_service.jsonc` | `src/03`, the ICF node, its handler and the BSP | Standard ABAP 7.50, against a dependency that holds `if_http_extension` |
| `.github/abaplint/abap_service_cloud.jsonc` | `cloud/src/03`, the service package of the branch `cloud` | ABAP Cloud |
| `.github/abaplint/abap_702.jsonc` | the downported tree | 7.02 |

Two folders are exempt from findings everywhere: `src/00/01` and `src/00/02`, the mirrored copies of [ajson](/technical/tools/ajson) and [S-RTTI](/technical/tools/srtti). They come from upstream projects and are not edited here. The rest of `src/00` is the framework's own code and is linted like everything else.

Run the same checks in a clone with `npm run check:standard`, `npm run check:cloud` and `npm run check:service`.

## Downporting

The abaplint downport rule rewrites the 7.50 syntax of `main` into syntax that compiles on 7.02. One command builds the downported tree, into `node/downport`:

```sh
npm run downport
```

It runs `abaplint --fix` with a generated downport configuration, applies a few fix-ups abaplint does not do, and checks the result. `abap_702.jsonc` from the table above is the profile that lints what comes out; it is not the one that produces it. The branch `702` is this build, published by the workflow `auto_downport` ([Downporting](/advanced/downporting)).

## Renaming

All artifacts are compatible with the abaplint renaming feature, so abap2UI5 can be installed under a namespace of your own:

```sh
abaplint .github/abaplint/rename.jsonc --rename
```

That is the core of it. The renamed installation, with the service package and a 7.02 variant, comes from a workflow: [Renaming](/advanced/renaming#step-by-step-guide).
