---
outline: [2, 4]
---
# MCP Server

An AI coding agent asked to write an abap2UI5 app can write ABAP. What it
cannot do is find out whether the app **works** — that has always needed a
system: activate the class, launch the app, look at the screen. So the agent
writes something plausible, and you are the one who finds out.

[**abap2UI5/mcp-server**](https://github.com/abap2UI5/mcp-server) closes that
loop. It is an [MCP](https://modelcontextprotocol.io) server: a small program
your AI client starts in the background, which gives the agent a set of tools
it can call by itself. Those tools let it check a view, **look at a picture of
it**, deploy the class into a local sandbox, transpile the whole framework to
Node, boot the app in a headless browser and read the errors — all on your
machine, with **no SAP system involved**.

```
examples -> app_guide -> validate_view + screenshot_view -> deploy_app -> build_backend -> run_app -> pitfalls
(has somebody  (how an app  (SECONDS, no system:        (write ABAP,  (transpile     (boot headless,  (what a green
 built it       is built)    is the view legal,          lint)         to Node)       errors +         run still
 already?)                   and what does it LOOK like)                              SCREENSHOT)      does not prove)
```

Everything it runs locally is infrastructure that already guards the abap2UI5
ecosystem in CI: the abaplint transpiler and the open-abap runtime, the
framework's express shim, the samples-controls build and boot gates, and the
[linter](/advanced/linter)'s validation core.

## What "MCP" means here

MCP — Model Context Protocol — is a standard way for an AI client to talk to a
tool server. You register the server once with your client; from then on the
agent sees a list of tools with their descriptions and decides when to call
one. You do not call these tools yourself, and you do not paste their output
anywhere: the agent asks, the server answers, and you read the conversation.

It works with any MCP client — Claude Code, Cursor, VS Code (Copilot agent
mode), Claude Desktop.

## Setting it up

The tools need different things, so the setup is **levelled**: stop at the
level whose tools you want. Each level adds the ones below it, and a tool whose
prerequisites are missing answers with a message naming what it needs rather
than failing — the server starts either way.

### Level 1 — check and see views (about 3 MB, a minute)

This is the level most work happens at: `validate_view` and `screenshot_view`
reconstruct the view your ABAP builds, check it against the UI5 API and
photograph it. Seconds per answer, no backend, no transpile.

```sh
git clone https://github.com/abap2UI5/linter        # AI_VIEW_CHECK_HOME
cd linter && npm ci
```

The server itself is published as `@abap2ui5/mcp-server`, so it needs no
checkout — the `npx` command below fetches and runs it. Clone it as well only
if you want to work *on* the server:

```sh
git clone https://github.com/abap2UI5/mcp-server && cd mcp-server && npm ci
```

That install is about 45 MB, and 19 MB of it is a Playwright driver only
`run_app` uses — paid on the first start, cached afterwards.

`screenshot_view` additionally needs the linter's render runtime and a browser
— `npm i -D @abap2ui5/render-runtime && npx playwright install chromium` in the
linter checkout. `validate_view`'s property gate needs neither.

### Level 2 — the sample catalogues and deploying (about 110 MB)

Adds `examples`, `capabilities`, `app_guide`, `scaffold_app`,
`generation_rules`, `pitfalls`, `scope_of` and `deploy_app`.

```sh
git clone https://github.com/abap2UI5/abap2UI5          # A2UI5_HOME
git clone https://github.com/abap2UI5/samples-controls  # SAMPLES_CONTROLS_HOME
git clone https://github.com/abap2UI5/samples           # SAMPLES_HOME
git clone https://github.com/abap2UI5/samples-stack     # SAMPLES_STACK_HOME
cd abap2UI5 && npm ci && cd ../samples-controls && npm ci
```

`examples` needs only the clones — no install — and it is the cheapest useful
thing here. It answers *"has somebody already built a value help, a tree,
navigation between two apps?"* out of **614 working apps in three
repositories**, and hands back a class to read rather than a snippet to trust.
Any one of the three catalogues is enough to start; a missing clone is reported
in the answer, not fatal. Each catalogue says what it covers on its own page —
[Learn](https://abap2ui5.github.io/samples/),
[Controls](https://abap2ui5.github.io/samples-controls/),
[Stack](https://abap2ui5.github.io/samples-stack/).

### Level 3 — see the running app (a browser, and time)

Adds `build_backend` and `run_app`: the screenshot loop.

```sh
npx playwright install chromium
```

Then one `build_backend { mode: "full" }`, which transpiles the framework and
the corpus to Node. **Budget tens of minutes for that first build** — every
later one is incremental, one to two minutes. It is the slowest thing here by
far, and it is what buys an agent the ability to look at what it built.

### Registering it with your client

**Claude Code:**

```sh
claude mcp add abap2ui5 -- npx --yes @abap2ui5/mcp-server
```

From a checkout instead: `claude mcp add abap2ui5 -- node
/path/to/mcp-server/server.mjs`.

**Cursor** (`.cursor/mcp.json`), **VS Code** (`.vscode/mcp.json`), **Claude
Desktop** (`claude_desktop_config.json`) and anything else reading the standard
stdio shape:

```json
{
  "mcpServers": {
    "abap2ui5": {
      "command": "npx",
      "args": ["--yes", "@abap2ui5/mcp-server"],
      "env": {
        "AI_VIEW_CHECK_HOME": "/path/to/linter",
        "A2UI5_HOME": "/path/to/abap2UI5",
        "SAMPLES_CONTROLS_HOME": "/path/to/samples-controls"
      }
    }
  }
}
```

To run a checkout instead, swap those two lines for `"command": "node"` and
`"args": ["/path/to/mcp-server/server.mjs"]`.

The three `env` entries are only needed when the checkouts are not siblings of
the server — which they cannot be when it runs from npx, so state them there.
Drop the ones whose level you stopped short of. VS Code wants the
same object under a top-level `"servers"` key instead of `"mcpServers"`.

::: tip Using VS Code?
The [abap2UI5 extension](/advanced/vscode) registers this server for you — no
JSON to write — and adds a second one of its own for the half this server
deliberately does not have: your real systems.
:::

## The tools

| Tool | What the agent gets |
| --- | --- |
| `capabilities` | Whether abap2UI5 can express a UI5 feature **at all**, from the verified capability map. The question to ask before writing a line of ABAP |
| `app_guide` | **How to build an app**, live from the framework checkout: the app class template, lifecycle, the view-builder chain, binding, events, popups, navigation, portability |
| `scaffold_app` | The files a new project starts from, live from app-template — both gate configs, the CI workflow, the abapGit metadata, an `AGENTS.md` and a working app class. `{ class: "zcl_my_app" }` renames it throughout, including the sidecar's `CLSNAME`, which is what decides whether the object activates |
| `examples` | Search the three sample catalogues for a working use of a control or a pattern. Answers with a class to read, never with a snippet to trust |
| `generation_rules` | The rulebook for porting a UI5 demo-kit sample into the samples-controls corpus — a different job from `app_guide` |
| `pitfalls` | The catalogues of defects **a green run does not catch**: `{ area: "abap" }` for abapGit import, activation, extended check, downport and runtime; `{ area: "view" }` for the oldest UI5 release. Every entry is a defect that actually shipped |
| `scope_of` | In/out-of-scope verdict for a UI5 control (since ≤ 1.71, not deprecated) |
| `validate_view` | **Seconds, not minutes**: the [linter](/advanced/linter)'s gates, from ABAP source or raw XML, judged by your project's own `abap2ui5lint.jsonc`. Findings come with severity, message, line and column — and what each rule that fired *means*, so interpreting one needs no web search |
| `screenshot_view` | **See the view in seconds**, with no build and no backend: reconstructed, rendered and returned as an image. Several viewports in one session, any theme, and preview data for the tables a `SELECT` would fill |
| `deploy_app` | Write the class plus its abapGit sidecar into a gitignored sandbox and abaplint it against the full framework context |
| `build_backend` | Rebuild the transpiled Node backend — incremental after the first full build |
| `run_app` | Boot any app class headless, return boot status, real page errors (benign UI5 noise filtered) and a full-page **screenshot as an image**. The *running* app, so it needs a `build_backend` first |
| `backend` | `status` / `start` / `stop` / `restart` of the local express backend |
| `remove_app` | Delete a dev app from the sandbox, or list what is deployed |

Two of these look similar and are not: `screenshot_view` photographs the
**view** (seconds, no backend, mock data), `run_app` photographs the **running
app** (a build, a real roundtrip, real behaviour). They cost three orders of
magnitude apart, and most iterations should end at the first.

## The intended loop

1. `capabilities` — check the feature is expressible, and how, before writing
   any ABAP.
2. `app_guide` — once per session, before writing any ABAP.
3. `scaffold_app` — when the user wants a project of their own rather than a
   class to paste into one that exists.
4. Write the class, then `validate_view` **and** `screenshot_view` — the
   findings and the picture, both in seconds. **Most iterations should end
   here.**
5. `deploy_app` — abaplint against the full framework context.
6. `build_backend` — incremental after the first full build.
7. `run_app` — read the errors, look at the screenshot. Then edit, validate,
   deploy, build, run again.
8. `pitfalls` before calling it done — the defects no gate here can see: what
   the class does on a *real* system, and what the view does on the *oldest*
   one. A green loop is not the same as a shipped app.

## Good to know

- **The sandbox is gitignored.** Deployed apps land in the samples-controls
  checkout's `src/zz_dev/`, so nothing an agent deploys can leak into a commit.
  Promote a finished app by moving it into a real package deliberately.
- **Port:** the local backend listens on 3000 (`A2UI5_MCP_PORT` overrides).
- **Timeouts:** every spawned child is killed with its whole process tree when
  it exceeds its limit — lint and scope five minutes, build thirty by default.
- **Offline:** UI5 modules are served from the local `@openui5` packages, so
  booting needs no network. Theme CSS is the exception: with network access it
  loads from the CDN and screenshots come out styled; without, apps render
  unstyled but structurally complete. `A2UI5_MCP_OFFLINE=1` forces the
  hermetic behaviour.
- **Deployment to a real system** stays what it always was: abapGit. This
  server is the inner development loop.

## Next Steps

- [Building with AI](/get_started/ai) — the whole AI setup in rising order of
  effort; this server is the top rung
- [abap2UI5 linter](/advanced/linter) — the gates behind `validate_view` and
  `screenshot_view`
- [VS Code Extension](/advanced/vscode) — registers this server for you, and
  adds the real-system tools
