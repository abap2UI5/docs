---
outline: [2, 4]
description: The optional tooling around abap2UI5, in the order a developer meets it - the playground, the layout editor, the Claude Code plugin and MCP server, the VS Code extension, the linter in CI, the headless frontend and the agent runtime - each with what it does, when to use it and how to start it.
---
# Tooling

Everything on this page is optional — abap2UI5 is one ABAP class and needs no
tools at all. What the seven below add is the loop that ABAP does not give you
by itself: trying an idea before a system is involved, catching a broken view
**before** it reaches one, seeing the app without leaving the editor, and
testing it without a browser.

They are listed in the order a developer meets them, from the first sketch to
the app in production, and each is independent of the rest. Take the first
one and stop, or take all seven. Every one of them is a repository of its own
under [abap2UI5](https://github.com/abap2UI5) on GitHub and installs with
one command. A project that wants the checks wired in from the first commit
starts from the [app-template](/advanced/working_off_stack); which npm package
does what, and which versions go together, is on
[npm Packages](/advanced/npm_packages).

```
playground -> layout editor -> plugin + MCP server -> VS Code -> linter in CI -> headless frontend -> agent runtime
(try it in    (sketch the      (an agent writes,      (write,     (every push,   (test a session   (agents use the
 the browser)  view, get a      checks and runs it,    lint, F9)   annotated)     in ABAP Unit)     app in the system)
               class)           no system)
```

## 1. Playground: ABAP in the browser

[**abap2ui5.github.io/playground**](https://abap2ui5.github.io/playground/)
— ABAP on the left, the running app on the right, and nothing in between but
the browser. The framework is transpiled to JavaScript at build time; your
class is compiled the moment you press *Run*, in about 20 ms, and starts
against an in-memory database. No server, no SAP system, no account.

Two checkers read what you type. [abaplint](/technical/tools/abaplint) says
whether the ABAP compiles, against the real framework sources; the
[abap2UI5 linter](/advanced/linter) reconstructs the view your builder chain
produces and says whether *that* works on UI5 1.71 — a control the release
does not have, an icon that is in no icon font. Both configurations are
editable in a tab, so "why is it not warning here?" has an answer you can try.

The playground is also the front door to the samples: the
[sample catalog](https://abap2ui5.github.io/samples-site/) lists every
sample of all three sample repositories — searchable by what an app does, by
the control it builds and by the release your system runs — and each one
opens in the editor. The three sources are one click each:
[**Learn**](https://abap2ui5.github.io/samples-site/#learn) for
*“Where do I start?”*,
[**Controls**](https://abap2ui5.github.io/samples-site/)
for *“Which control does what?”* and
[**Stack**](https://abap2ui5.github.io/samples-site/) for
*“Will my system run it?”*.

**Use it** to try a control, to reproduce a problem in a form somebody else
can open, and to read a sample next to its running app. **Share** puts every
open file into the URL fragment — a 2,500-character class becomes a link of
about 700 characters, and because it is a fragment it never leaves the
browser. The same dialog has *Download for abapGit*, a zip laid out as a
repository, which is the way from the sketch into a system.

Nothing to install: open the link. The source is
[abap2UI5/playground](https://github.com/abap2UI5/playground), and every
fenced example on this site with a **Run** button runs in it.

## 2. Layout editor: drag and drop, one class out

[**abap2ui5.github.io/layout-editor**](https://abap2ui5.github.io/layout-editor/)
— lay out a UI5 view by drag and drop, set its properties, bind its fields,
name its events, and get **one ABAP class**: an abap2UI5 app with the view,
its data and its events, in the house layout of every sample. Controls and
outline on the left, the canvas in the middle, properties on the right — the
familiar pattern of the SAP Web IDE layout editor, with one addition: the
class is never hidden. Canvas and code sit side by side, selecting a control
marks the lines that build it, and clicking a line selects the control.

There is no runtime of the editor's own, no metadata and no lock-in. The
class follows the app template exactly — `z2ui5_if_app`, `main( )` as the
dispatcher, `view_display`, `on_event`, `model_init` — so from the export on
it is your code and the editor is out of the picture. Every control and
property the palette offers exists in UI5 1.71; a control inserted by its
class name is whatever the canvas's UI5 says it is, and *Checks* says so.

**Use it** for the first version of a screen — a form, a table with a
search, a set of tabs — when the shape is clearer in your head than the chain
that builds it. *Import XML* brings in an existing UI5 view, *Export*
downloads the class with its abapGit files, and *Copy share link* puts the
whole design in a URL.

```
Export → abapGit: New Offline → Import ZIP → pull → ?app_start=zcl_my_app
```

Nothing to install: open the link. The source is
[abap2UI5/layout-editor](https://github.com/abap2UI5/layout-editor).

## 3. Claude Code plugin and MCP server: the loop for an agent

[**abap2UI5/mcp-server**](https://github.com/abap2UI5/mcp-server) — an AI
coding agent can write an abap2UI5 class; what it cannot do on its own is find
out whether the class **works**. The MCP server gives it the tools for that,
and none of them needs an SAP system: check the view the ABAP builds and look
at a picture of it in seconds, deploy the class into a local sandbox,
transpile it to Node beside the framework, boot the app in a headless browser
and read the errors and the screenshot. The agent writes, checks, runs, looks
and iterates — on your machine. It works with any MCP client: Claude Code,
Cursor, VS Code in agent mode, Claude Desktop.

**Use it** whenever an assistant writes the code. The cheaper rungs — the
paragraph to paste and the index to point at — are on
[Developing with AI](/get_started/ai); this is the top one.

```sh
claude mcp add abap2ui5 -- npx --yes -p @abap2ui5/mcp-server abap2ui5-mcp
```

That is the view checks out of the box; the screenshot, sandbox and run tools
are a leveled setup, and `setup_status` tells the agent what else works on
this machine. For Claude Code the **plugin** bundles the server with the
framework's agent skills — building an app, the ABAP and UI5 pitfall
catalogs, the view-chain layout — in one install:

```text
/plugin marketplace add abap2UI5/abap2UI5
/plugin install abap2ui5@abap2ui5
```

[MCP Server](/advanced/mcp_server) has the whole of it: what MCP means here,
the three setup levels, every tool and the loop they are meant to be used in.
[Agent Setup](/advanced/agent_setup) is the rest of an agent's setup, in
rising order of effort.

## 4. VS Code extension: lint while you type, F9 against a system

[**abap2UI5/vscode-extension**](https://github.com/abap2UI5/vscode-extension)
— developing the plain way is three windows: the editor, a browser with the
app, and the activation step between them. The extension collapses that into
one. **F9** on an app class starts it in a preview beside the source, against
your real system; **Ctrl+F3** activates the class and reloads the preview; and
the whole linter runs in the editor while you type, so a broken view is a
squiggle rather than a blank screen. Plus completion and hover for the UI5
API inside the builder chain, a click-a-control → jump-to-the-line inspector,
a traffic log of every roundtrip, and a view preview that needs no system at
all.

**Use it** for the day-to-day loop against a system: the first tool on this
page that touches one.

```sh
code --install-extension abap2ui5.abap2ui5
```

It is on the VS Code Marketplace and on Open VSX, for VSCodium, Theia and SAP
Business Application Studio; every release also carries the `.vsix`. The
parts that talk to a system need the
[ABAP remote filesystem](https://marketplace.visualstudio.com/items?itemName=murbani.vscode-abap-remote-fs)
extension for the connection. [VS Code Extension](/advanced/vscode) has every
command and setting.

For an agent working in your editor, the extension registers the MCP server
above and adds a second one of its own for the half that server deliberately
does not have: your configured systems — list them, search app classes, run a
class on the real system as a screenshot.

Since 0.31.0 the same system server carries the **app tools** — an agent
operating the app on your system through its JSON protocol, answering with
the agent snapshot — behind the setting `abap2ui5.agent.enableAppTools`.
[Agent-Operable Apps](/advanced/agents) describes them.

## 5. Linter in CI: every push, annotated

[**abap2UI5/linter**](https://github.com/abap2UI5/linter) — the view your app
builds only exists at runtime, so no UI5 tooling can see it and the ABAP
compiler has no opinion about it. The linter reconstructs the view from the
builder chain and judges the two together: controls, properties, aggregations
and enum values that UI5 does not have, or does not have **yet** on the
release you target; bindings that point at nothing; events nothing handles;
deprecated controls. With its render package installed it also loads every
view in a headless browser, which is the only way to find a view that does
not merely render wrongly but fails to load at all.

The playground, the MCP server and the editor above all run this same linter.
**Use it** in CI so that nothing reaches a system that the three did not
catch — and so that a project nobody opens in those tools is held to the same
floor. The GitHub Action is the whole setup:

```yaml
steps:
  - uses: actions/checkout@v4
  - uses: abap2UI5/linter@v0
    with:
      paths: src
      min-ui5: '1.71'
```

Findings are annotated onto the pull-request diff. On your machine the same
run is one line, with no install:

```sh
npx @abap2ui5/linter src
```

The [linter page](/advanced/linter) has the rest: reading a finding, the two
gates, `--fix`, waiving a rule, and the baseline for switching it on over a
codebase that already exists.

## 6. Headless frontend: a user session as an ABAP Unit test

[**abap2UI5/headless-frontend**](https://github.com/abap2UI5/headless-frontend)
— an abap2UI5 app's whole conversation with the outside world is one JSON
request in and one JSON response out. `z2ui5_cl_frontend_simulator` plays the
browser's side of that conversation inside ABAP: it builds the request the
UI5 frontend builds, hands it to the framework and replays the response — in
the same session, in the same call stack, in a plain ABAP Unit test.

```abap
DATA(sim) = z2ui5_cl_frontend_simulator=>start( `Z2UI5_CL_FRONTEND_SIM_EXAMPLE` ).

sim->set_value( name = `MV_NAME` value = `World` ).
sim->click( `GREET` ).

cl_abap_unit_assert=>assert_equals( exp = `Hello World!`
                                    act = sim->get_message( ) ).
```

That is a full user session — typed a name, pressed a button, read the result
— in milliseconds, with no browser, no driver and no network, failing with an
ABAP stack trace that points at your app. The app is addressed by the names
its author already wrote: `MV_NAME` is the bound attribute, `GREET` the
event. A test does not break when a `VBox` becomes a `Grid`, because a user
pressing *Greet* does not care either; the rendered XML is still there through
`get_view( )` when you want to assert on it. Popups, popovers, nested views,
typed edits, table rows and a session resumed across requests are all
covered.

**Use it** for the tests between the unit test of a method and a browser
driven by Playwright: the whole roundtrip, with only the browser missing —
which was never the part with your bugs in it. Install it with abapGit next
to the framework; [Step 12](/tutorials/step-12#testing-the-roundtrip) of the
walkthrough shows it in a project.

## 7. Agent runtime: apps operated by agents in the system

[**abap2UI5-addons/abap-agent-runtime**](https://github.com/abap2UI5-addons/abap-agent-runtime)
— an MCP endpoint inside the SAP system itself, written in ABAP. Any MCP
client that connects to a remote server over HTTP calls
`/sap/bc/z2ui5_agent`, reads the screen of an abap2UI5 app as structured
data, fills its fields and fires its events — as the SAP user who logged on,
through the app's own `main( )`, with the app's own validation and
`AUTHORITY-CHECK`s. No Node, no browser, no technical user, and no second API
next to the app: whatever the app lets a user do on a screen, it lets an agent
do, and nothing more. Underneath it is the headless frontend above, playing
the browser's half of the protocol.

::: info Not in a release yet
The add-on is on `main` of abap-agent-runtime and its generated `standard` /
`cloud` branches, pulled into a 7.5x on-premise system but not yet run in
production, and its API may still change before 1.0. Its CHANGELOG says
when it does.
:::

**Use it** when the users of an app are AI agents — any MCP client —
acting for the people who log on. The endpoint is
disabled after installation, only apps that opt in are reachable, events that
need a human are never fired by an agent, and every call is audited.

Install with abapGit, in this order: the framework, the headless frontend,
then the add-on from the branch of your platform — `standard` for ABAP
Standard from 7.50, `cloud` for ABAP Cloud. Then connect a client:

```sh
claude mcp add --transport http abap2ui5-agent https://host:44300/sap/bc/z2ui5_agent \
  --header "Authorization: Basic $(printf '%s' 'USER:PASSWORD' | base64)"
```

[Agent-Operable Apps](/advanced/agents) has the snapshot an agent reads, the
four tools, the opt-in interface, the policies and the security model.

## Next Steps

- [Developing with AI](/get_started/ai) — the cheapest steps for an assistant,
  before any of the tools above
- [Agent Setup](/advanced/agent_setup) — the conventions file, the gates, the
  MCP loop and the editor, in rising order of effort
- [Working Off-Stack](/advanced/working_off_stack) — the app-template that has
  the linter and the agent setup wired in
- [Agent-Operable Apps](/advanced/agents) — the other direction: an agent
  operating a finished app instead of writing one
