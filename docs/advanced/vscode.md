---
outline: [2, 4]
---
# VS Code Extension

Developing an abap2UI5 app the plain way means three windows: the editor with
the class, a browser with the app, and the activation step between them. Change
a line, save, activate, switch to the browser, reload, look, switch back.

The [**abap2UI5 extension for VS Code**](https://github.com/abap2UI5/vscode-extension)
collapses that into one window. **F9** launches the app next to the source,
**Ctrl+F3** activates the class and reloads the preview, and the whole
[linter](/advanced/linter) runs in the editor while you type. It works with any
system running abap2UI5, on-premise or cloud — the only thing tying it to a
system is the launch URL you configure once.

## Installing

Install **abap2UI5** from the
[VS Code Marketplace](https://marketplace.visualstudio.com/items?itemName=abap2ui5.abap2ui5):
Extensions panel (`Ctrl/Cmd + Shift + X`) → search for *abap2UI5* → **Install**.
From the terminal:

```sh
code --install-extension abap2ui5.abap2ui5
```

It is on [Open VSX](https://open-vsx.org/extension/abap2ui5/abap2ui5) as well,
for VSCodium, Eclipse Theia and SAP Business Application Studio. Without
Marketplace access, every
[release](https://github.com/abap2UI5/vscode-extension/releases/latest) carries
the `.vsix` for *Install from VSIX…*.

For the parts that talk to a system you also need an ABAP connection — the
[ABAP remote filesystem](https://marketplace.visualstudio.com/items?itemName=murbani.vscode-abap-remote-fs)
extension is what opens ABAP objects from the system and what the activation
step hands over to.

## Running the app: F9

Put the cursor in a class implementing `z2ui5_if_app` and press **F9**. The app
opens in an embedded browser beside the source, and the cursor goes back to
where it was — even when the loading app tries to grab focus. In a class that
is *not* an abap2UI5 app, F9 keeps its usual meaning (toggle breakpoint), so
you do not lose the key.

On the first F9 the extension asks for the **launch URL**, with `{class}` as
the placeholder:

```
https://host:44300/sap/bc/z2ui5?app_start={class}&sap-client=100
```

Working against several systems is the normal case, so name them instead:

```jsonc [settings.json]
"abap2ui5.systems": [
  { "name": "DEV",     "url": "https://dev:44300/sap/bc/z2ui5?app_start={class}&sap-client=100" },
  { "name": "Sandbox", "url": "https://box:44300/sap/bc/z2ui5?app_start={class}" }
]
```

*"abap2UI5: Select System"* switches between them and can add one, so you never
have to find the JSON. The active system is remembered **per window** — two
windows can work against two systems at once — and credentials are stored per
host, so switching back and forth does not ask again.

**Where the app opens** is `abap2ui5.openMode`: `tab` (default, an editor tab
next to the code), `panel` (down with Terminal and Output) or `external` (your
normal browser, reusing your existing SAP session). Two commands move a running
app between tab and panel without restarting it.

### Why there is a local proxy

An embedded iframe has **no** SAP session, so a direct call would end in a
**401**. In `tab` and `panel` mode the extension therefore starts a small auth
proxy on `127.0.0.1`: it asks once for your SAP user and password (the same
ones you use in ADT, kept in VS Code's SecretStorage), attaches them to every
request, and rewrites `Origin` and `Referer` so origin-validating CSRF checks
accept the app's POSTs. To make embedding possible at all it strips
`X-Frame-Options` and the `frame-ancestors` CSP directive from the responses,
and it accepts self-signed certificates.

::: warning Basic auth required
The system must accept basic auth. Pure SSO/SAML without a basic-auth fallback
is not supported — use `external` mode there. *"abap2UI5: Clear Stored SAP
Credentials"* forgets the password again.
:::

### When the preview stays white: the connection check

The most common first-run failure is a launch URL that is slightly wrong, and
its symptom in the preview is a white rectangle that says nothing. *"abap2UI5:
Check System Connection"* diagnoses it: the command walks the exact route F9
takes — the same URL expansion, the same stored credentials, the same proxy —
and reports step by step where a launch would end: the URL's shape, the host,
the logon, the ICF path, the page itself, each with the fix next to the
failing step. The full report lands in the **abap2UI5** output channel. This
also works for a system installed five minutes ago, which makes it the
diagnosis step of the [Quickstart](/get_started/quickstart#_3-first-launch).

### Reload on activation, not on save

Saving an ABAP class does not change what the server runs — only **activation**
does. So the preview reloads on activation:

| `abap2ui5.reloadOn` | Behaviour |
| --- | --- |
| `activation` (default) | **Ctrl+F3** saves, activates through your ABAP tooling and reloads. Activations done any other way are noticed on the server and reload too. A plain save only marks the preview *not activated* |
| `save` | Reload on every save — for setups where saving already publishes the change |
| `never` | Only F9, the reload button or the status bar |

Activating from somewhere else works as well: while the preview shows the *not
activated* badge, the extension watches the class's ADT metadata on the server
and reloads as soon as it is active again — whether you used Ctrl+F3, the ABAP
remote filesystem's own button, or even Eclipse.

### What the preview gives you beyond a browser tab

- **Device widths, theme and language** — desktop, tablet (834px) and phone
  (414px), UI5 themes and logon languages, to check a responsive app without
  leaving the editor.
- **Runtime errors land in the editor.** A thrown error, a failed assertion or
  a rejected promise inside an iframe is normally visible only in browser
  devtools — exactly the context switch the preview exists to avoid. They are
  forwarded to the **abap2UI5** output channel and counted in a toolbar badge.
  When the text names a binding path or identifier that appears in the class,
  the log adds the file and line under it.
- **Inspect (🎯)** — a one-shot element picker like the one in devtools: the
  hovered control is outlined, and a click jumps to the `ele( )` / `tag( )`
  call in the class that wrote it. A row inside a bound list lands on its
  template; an `id` written in the class settles the match outright.
- **Model (`{ }`)** — the running app's JSON model as a document beside the
  code: live values next to the statically known shape.
- **Traffic log with roundtrip timings** — the proxy sees every request, which
  makes it a free network tab. Every abap2UI5 event is one POST, and the
  toolbar shows the last one's duration as a badge. *"Is the backend slow or
  the UI?"* stops being a devtools trip.
- **Take App Screenshot (📷)** — the running app as a PNG for a bug report,
  rendered headless through the same proxy.
- **Stateful reload (📌)** — a reload is normally a fresh start, and the three
  clicks that reproduced the bug have to be clicked again on every activation.
  With the pin on, the model is captured before the reload and the class's own
  paths are restored into the fresh page.

## Checking views while you type

The extension runs the [abap2UI5 linter](/advanced/linter) in the editor, so
the findings land in the Problems panel before the app ever reaches a system:

- **The property gate** is bundled, needs no setup and runs **while you type**,
  shortly after each pause. It resolves every control and property against the
  UI5 metadata snapshot: a control that does not exist is an error, anything
  newer than your floor (`abap2ui5.viewCheck.minUi5`, default 1.71) or
  deprecated is a warning.
- **The abap2UI5 rules** come with it — the defects that stay silent at
  runtime, from a hand-written binding path the model does not have to an
  ABAP boolean written straight into the view.
- **The render gate** is the expensive one and stays on save and on demand.
  Install it once with *"abap2UI5: Install Render Gate"*: the command downloads
  a self-contained checker bundle and Chromium into the extension's storage and
  runs everything with VS Code's own runtime — no node, npm or PATH setup on
  the machine.
- **The system can answer the settings.** After the first F9 the extension
  reads the system's `sap-ui-version.json` and offers, once per system, to
  align `viewCheck.minUi5` and `.distribution` with what the system actually
  runs. The detected version stays visible in the status bar.

**Quick fixes.** Every finding whose correction is mechanical carries it, and
the lightbulb offers it — plus *fix all in this file*, as a command, as a
CodeLens above the class, as *Fix All View Findings in the Workspace* in one
undo step, and as `source.fixAll.abap2ui5` for `editor.codeActionsOnSave`. The
other quick fix on any finding is **suppress on this line**, which writes the
linter's own directive — so waiving something here waives it in CI too, and a
line waived in CI no longer squiggles here.

**The repository config wins.** An `abap2ui5lint.jsonc` in the repo is what the
CLI and the Action check against, so it overrides the VS Code settings wherever
it says something; the settings fill in the rest. The output channel names the
file the current values came from — the first place to look when the editor and
CI disagree. The config file itself is schema-checked as you edit it, and a
`baseline` it names is applied here as well, so the Problems panel shows only
what is *new*.

**The findings view.** The Problems panel answers *what is wrong in this file*.
The **abap2UI5 Findings** view in the Explorer answers the other question —
*what is wrong in this repository* — by grouping the same findings under the
**rule** that produced them, worst first. Twelve `unknown-binding-path` across
three classes are one decision; per file they look like twelve unrelated
problems.

## Writing the view

The extension knows the whole UI5 API and the model your class derives, so it
can help while the chain is being written rather than after it:

- **Completion** for control names in `ele( )` / `tag( )` (resolved through the
  namespace in play), for the members of exactly that control in the `a( )`
  chained to it, and for the values an enum property accepts.
- **Binding paths complete too.** Typing `{` offers the paths the derived model
  actually has — the same model the `unknown-binding-path` rule checks against,
  so what is offered is exactly what will not squiggle afterwards. Inside an
  aggregation template the fields of the bound row come first.
- **`client->` completes and explains itself** — every `z2ui5_if_client` method,
  with the full ABAP signature and documentation on hover.
- **Hover** adds a member's type, the UI5 version it appeared in, its
  deprecation and a link to the UI5 API reference. On a binding path it says
  what the model resolves it to — a field, a structure, a table, or **missing**.
- **Format Document** repairs a builder chain: a child one step under its
  parent, an attribute one step under its element, an `end( )` in the column of
  the `ele( )` it closes. Deliberately conservative — only builder-verb lines
  inside a chain are touched.
- **Emmet for chains** — `Page>content>Button*3`, then *"Expand Abbreviation to
  a Chain"*, and the chain that builds it is written in the house layout.
  `#id`, `[attr=value]` and `{text}` work as in Emmet.
- **Extract to View Method** — a real screen is one very long statement. Put
  the cursor on the `)->ele( )` that should start a new method, name it, and
  the tail becomes a helper method taking the builder handle, with the
  declaration written into the class and the call left behind. It extracts a
  *tail* and refuses rather than guesses.
- **Convert XML View to Builder Chain** — paste a UI5 demo kit sample (or any
  view XML) and get the builder chain out, in the corpus style. What the
  builder cannot express is listed as `TODO` comments rather than dropped
  silently. Porting a sample starts with paste instead of transcription.
- **Snippets** for the everyday shapes: `z2ui5app`, `z2ui5main`, `z2ui5ele`,
  `z2ui5button`, `z2ui5table`, `z2ui5event`, `z2ui5popup`, `z2ui5navto` and
  more.
- **Inline annotations** — the finding at the end of its line, the UI5 `@since`
  of what you are writing (warned when it is above your floor), and what a
  `PUBLIC` attribute costs per roundtrip. Each can be switched off.

### Seeing the view and the structure

- **Show Reconstructed XML View** opens the XML the builder calls actually
  produce, live beside the class and following the editor the way the Markdown
  preview does. The findings are mirrored onto the XML lines they concern, and
  **Go to Definition** on any line jumps back to the builder call that produced
  it.
- **Preview View (No System)** renders the view and shows the **picture** —
  no system, no activation, no launch URL. It is the render gate turned around:
  the view is kept standing and photographed instead of thrown away. A
  `<class>.mock.json` next to the source fills the tables a `SELECT` would fill,
  and the caption always says which model was used. A comma-separated viewport
  list renders a device matrix in one browser session, and *Compare with HEAD*
  puts the committed version beside the working tree — answering what no linter
  can: *did my change do what I meant to the view?*
- **Outline and navigation** — the `ele( )`/`tag( )` hierarchy as a tree in the
  Outline pane; Go to Definition between `client->_event( 'GO' )` and the
  `WHEN 'GO'` that handles it, in both directions; and on a binding path, to
  the `TYPES` field that declares it.
- **F2 renames the strings an app is wired together with.** An abap2UI5 app
  ties its two halves together with literals and nothing connects the ends, so
  renaming one is normally a grep and missing one is silent. F2 takes an event,
  a control id (with every wire addressing it) or a bound attribute
  (`mv_title` and `{/MV_TITLE}` together) all at once. Position decides what a
  literal is, never its text.
- **App navigation map** — every `z2ui5_if_app` class in the workspace and each
  `nav_app_call( )` between them, as a clickable graph.
- **The apps of this workspace** — the Explorer view listing every app class
  with run, preview and check on it: the list that says which thirty apps a
  repository has.
- **Show Examples for this Control** — put the cursor on an `ele( )` call and
  the [sample catalogues](https://abap2ui5.github.io/samples/) are searched for
  working uses of that control, richest first, opening at the line. It reads the catalogues
  from `abap2ui5.mcp.reposRoot`, so it needs those checkouts.

### Starting from a template

*"New App from Template"* is a gallery rather than one skeleton — empty view,
list, form, master & detail, popup — and every template ships linter-clean.
*"New Project from Template"* puts the same into an empty folder together with
everything that makes it a project: app-template's `abaplint.jsonc` and
`abap2ui5lint.jsonc`, its `AGENTS.md`, the CI workflow running both gates, and
the abapGit files. See [Working Off-Stack](/advanced/working_off_stack) for
what that project then looks like.

## For AI agents

The extension registers the [abap2UI5 MCP server](/advanced/mcp_server) for
every MCP client in the window — Copilot agent mode, Claude Code, anything else
speaking MCP — so an agent working in your editor has the systemless dev loop
without any separate configuration. Point `abap2ui5.mcp.reposRoot` at the folder
holding the checkouts and the extension passes the paths through.

It adds a **second** server of its own for the half the MCP server deliberately
does not have: your configured **systems**.

| MCP tool | What the agent gets |
| --- | --- |
| `list_systems` | The configured launch systems and which one is active |
| `search_apps` | Class names on the system, via the ADT quick search |
| `run_app_on_system` | The app rendered **on the real system**, headless, as a screenshot |

Every prompt — system pick, credentials — stays an ordinary VS Code dialog the
agent never sees. The names keep the two apart: the abap2UI5 server's `run_app`
builds and boots the transpiled sandbox, this one runs a class on a system.

## In the browser

The extension ships a web bundle, so it also runs in
[vscode.dev](https://vscode.dev), github.dev and browser-based SAP Business
Application Studio. Everything that needs no process and no socket works there:
completion and hover, the property gate live while typing, the reconstructed
XML view, the outline and event navigation, *Convert XML View to Builder
Chain*, the snippets and the whole template gallery. The repository's
`abap2ui5lint.jsonc` is honoured there too, so vscode.dev agrees with CI.

Desktop-only, and hidden from the palette on the web: the embedded preview with
its proxy, traffic log, screenshot and pin; Ctrl+F3 activation and the ADT
integration; the render gate; the workspace-wide check and quick fixes; the
navigation map; the Control Properties view; and the MCP servers.

## The settings worth knowing

| Setting | Default | Meaning |
| --- | --- | --- |
| `abap2ui5.launchUrlTemplate` | – | URL template used to launch an app, `{class}` as the placeholder |
| `abap2ui5.systems` | `[]` | Named launch profiles, for more than one system |
| `abap2ui5.openMode` | `tab` | `tab`, `panel` or `external` |
| `abap2ui5.reloadOn` | `activation` | When the preview reloads: `activation`, `save` or `never` |
| `abap2ui5.viewCheck.minUi5` | `1.71` | The UI5 version your system runs |
| `abap2ui5.viewCheck.distribution` | `sapui5` | Which distribution the system serves |
| `abap2ui5.viewCheck.live` | `true` | Run the property gate while typing |
| `abap2ui5.viewCheck.render` | `false` | Also run the headless render gate |
| `abap2ui5.viewPreview.theme` | `sap_horizon` | Theme the systemless preview renders in |
| `abap2ui5.viewPreview.viewport` | `1280x900` | Viewport(s); a comma-separated list is a device matrix |
| `abap2ui5.inlineFindings` | `problems` | The finding at the end of its line: `problems`, `all` or `off` |
| `abap2ui5.mcp.reposRoot` | – | Folder holding the `abap2UI5` / `samples-controls` / `linter` / `mcp-server` checkouts |

Every command is in the Command Palette (`Ctrl/Cmd + Shift + P`) under
*abap2UI5*; the full settings and command tables are in the
[repository README](https://github.com/abap2UI5/vscode-extension).

## Next Steps

- [abap2UI5 linter](/advanced/linter) — the gates behind the editor diagnostics
- [MCP Server](/advanced/mcp_server) — the systemless loop the extension
  registers for agents
- [Working Off-Stack](/advanced/working_off_stack) — the project the template
  gallery writes
