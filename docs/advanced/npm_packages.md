---
outline: [2, 3]
description: The npm packages around abap2UI5 - what each one is for, which ones depend on each other, and the version rules that keep them compatible.
---
# npm Packages

abap2UI5 itself is ABAP and needs no npm at all. The packages below are the
tooling around it and the ways to run it outside an SAP system. This page is
the map: what each one is for, which ones build on each other, and which
versions go together. The versions themselves are on npm; the rules that
decide which ones fit are here.

## The packages

| Package | What it is | Source |
|---|---|---|
| [`@abap2ui5/linter`](https://www.npmjs.com/package/@abap2ui5/linter) | checks an app class and the view it builds, without a system | [abap2UI5/linter](https://github.com/abap2UI5/linter) |
| [`@abap2ui5/linter-render`](https://www.npmjs.com/package/@abap2ui5/linter-render) | the UI5 runtime the linter's render gate uses | [abap2UI5/linter](https://github.com/abap2UI5/linter) |
| [`@abap2ui5/mcp-server`](https://www.npmjs.com/package/@abap2ui5/mcp-server) | the development loop for AI agents | [abap2UI5/mcp-server](https://github.com/abap2UI5/mcp-server) |
| [`@abap2ui5/node-runtime`](https://www.npmjs.com/package/@abap2ui5/node-runtime) | the framework transpiled to JavaScript, with an HTTP handler | [abap2UI5/abap2UI5](https://github.com/abap2UI5/abap2UI5) |
| [`@abap2ui5/embed-control`](https://www.npmjs.com/package/@abap2ui5/embed-control) | a UI5 control that runs an abap2UI5 app inside another UI5 app | [abap2UI5/embed-control](https://github.com/abap2UI5/embed-control) |
| [`@cap2ui5/cds-plugin`](https://www.npmjs.com/package/@cap2ui5/cds-plugin) | abap2UI5 in a CAP project, apps as JavaScript classes | [cap2UI5/cap2UI5](https://github.com/cap2UI5/cap2UI5) |
| [`@cap2ui5/samples`](https://www.npmjs.com/package/@cap2ui5/samples) | the samples as apps for the CAP plugin | [cap2UI5/samples](https://github.com/cap2UI5/samples) |

The [VS Code extension](/advanced/vscode) is not on npm but on the
Marketplace. It bundles the linter and starts the MCP server.

## How they depend on each other

```
@abap2ui5/linter ──optional peer──▶ @abap2ui5/linter-render
       ▲
       └── read by @abap2ui5/mcp-server and the VS Code extension

@abap2ui5/node-runtime ◀──exact pin── @cap2ui5/cds-plugin ◀──peer── @cap2ui5/samples

@abap2ui5/embed-control ──talks HTTP to──▶ abap2UI5 on ABAP, node-runtime or the CAP plugin
```

## Which versions go together

- **node-runtime and the framework.** The version of
  `@abap2ui5/node-runtime` is the framework's version, and it is built from
  that release's tag. Its `@abaplint/runtime` is pinned to the version the
  transpile ran with, because transpiled code is tied to its runtime.
- **The CAP plugin pins one node-runtime.** `@cap2ui5/cds-plugin` depends on
  exactly one `@abap2ui5/node-runtime` version, the one its tests ran
  against. A project that wants another one says so with npm `overrides`;
  the log line at startup names the version that was loaded.
- **The samples follow the plugin's minor.** While the plugin is `0.x`, a
  minor release may change how apps are written. `@cap2ui5/samples` therefore
  names the plugin's minor as its peer range and is released together with
  it.
- **The linter and its render runtime share a minor.** `@abap2ui5/linter`
  accepts the `@abap2ui5/linter-render` versions of its own minor. The render
  runtime pins the OpenUI5 version the linter's metadata was generated from.
  Its old name, `@abap2ui5/render-runtime`, is deprecated.
- **The embed control needs abap2UI5 1.145.0 or later** on the server, on
  ABAP as well as through node-runtime or the CAP plugin. Its README lists
  what 1.145.0 does not do yet.
- **The MCP server has no version coupling of its own.** It finds the linter
  next to it (a sibling checkout, `AI_VIEW_CHECK_HOME`, or the project's
  `node_modules`) and takes the framework from the newest plain release. The
  VS Code extension starts it without a version pin for the same reason.

## Starting the command-line tools

Every command-line tool here lives in a scoped package, so its command has a
different name than its package. Run it through the scoped name, so `npx`
can never fetch an unrelated package that happens to carry the command's
name:

| Tool | Without an install | Installed in the project |
|---|---|---|
| Linter | `npx @abap2ui5/linter src` | `npx --no-install abap2ui5lint src` |
| MCP server | `npx -y -p @abap2ui5/mcp-server abap2ui5-mcp` | `npx --no-install abap2ui5-mcp` |
| ABAP Unit runner | `npx -y -p @abap2ui5/mcp-server abap2ui5-unit` | `npx --no-install abap2ui5-unit` |
| cap2UI5 translator | `npx -y -p @cap2ui5/cds-plugin -p @abaplint/core cap2ui5 abap2js` | `npx --no-install cap2ui5 abap2js` |

The translator reads ABAP with `@abaplint/core`, which is an optional peer of
the plugin: a project that translates adds it with `npm add -D @abaplint/core`.
