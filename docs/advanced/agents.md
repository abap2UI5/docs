---
outline: [2, 4]
description: AI agents operating abap2UI5 apps - four tools that read an app's screen as data, fill its fields and fire its events over the app's own JSON protocol, in the MCP server, in the VS Code extension and as an ABAP-native endpoint in the system.
---
# Agent-Operable Apps

::: info Preview — coming with the next release
Everything on this page is finished on development branches of the MCP server,
the VS Code extension, abap2UI5/headless-frontend and the agent add-on, and is
not in a release of any of them yet. Details can still change until then.
:::

An abap2UI5 app is one ABAP class, and its whole conversation with the outside
world is one JSON request in and one JSON response out: the browser sends what
the user typed and the name of the event, the backend sends back a view, a
model and some messages. The browser is not the only thing that can hold up its
end of that conversation.

Four tools — `app_list`, `app_start`, `app_describe` and `app_act` — let an AI
agent operate **any** abap2UI5 app that way. The agent reads the screen as
structured data, fills fields by model path or label and fires events by the
name the app gave them. No browser, no CSS selector, no screenshot to
interpret, and no second API next to the app: whatever the app lets a user do
on a screen, it lets an agent do, through the app's own `main( )` — and nothing
more.

The same four tools exist in three places, for three different jobs:

| Where | Runs against | What it is for |
| --- | --- | --- |
| [MCP server](#while-developing-the-mcp-server) | the local, transpiled sandbox — no SAP system | developing: does the event branch do what I meant? |
| [VS Code extension](#on-your-system-from-vs-code) | your configured system, through the extension's proxy, as you | trying the app on real data from the editor |
| [Agent add-on](#in-production-the-agent-add-on) | an MCP endpoint inside the SAP system, as the logged-on user | agents operating apps for their users |

## What an agent sees: the snapshot

Every tool answers with an **agent snapshot**, one JSON document describing
the current screen. All three implementations produce the same shape (version
1), derived from the view XML of every open view and its model:

```json
{
  "snapshotVersion": 1,
  "session": "B9D27C42CDAB45558097FD6F95E77BAD",
  "app": "Z2UI5_CL_SMP_APP_009",
  "title": "abap2UI5 - Value Help",
  "layer": "popup",
  "fields": [],
  "actions": [
    { "id": "a1", "event": "POPUP_TABLE_VALUE_CONTINUE", "args": [], "label": "continue",
      "control": "sap.m.Button", "trigger": "press", "enabled": true, "scope": "screen", "layer": "popup" }
  ],
  "tables": [
    { "id": "t1", "path": "/T_SUGGESTION_SEL", "name": "T_SUGGESTION_SEL", "label": "T_SUGGESTION_SEL",
      "control": "sap.m.Table", "columns": [ { "name": "VALUE", "label": "Color" }, { "name": "DESCR", "label": "Description" } ],
      "rowCount": 6, "rows": [ { "VALUE": "GREEN", "DESCR": "this is the color Green", "SELKZ": false } ],
      "truncated": true, "selectionMode": "Single", "editableCells": ["SELKZ"], "layer": "popup", "selectionField": "SELKZ" }
  ],
  "messages": [],
  "texts": [],
  "unsupported": []
}
```

| Key | What it holds |
| --- | --- |
| `session` | the draft id to continue with — the argument of the next call |
| `layer` | the topmost open layer: `main`, `popup` or `popover` |
| `fields` | every input-like control bound to the model: path, label, kind (`text`, `number`, `date`, `boolean`, `choice`, …), current value, the allowed values of a choice, `required`, `editable` |
| `actions` | every event wire on a visible control: the event name, its arguments, a label, the UI5 trigger, `enabled`, and whether it belongs to a table row |
| `tables` | columns, the first rows, the selection mode, the editable cells — and the row property a selection writes to |
| `messages` | toasts, message boxes, message strips, the value states of fields and the app's message table |
| `texts` | some static text on the screen, for context |
| `unsupported` | what the snapshot saw and cannot describe, such as a custom control |
| `pending` | values set without an event, not sent yet |

A dynamic event argument is written as a descriptor the client fills in at act
time: `$row:NAME` is the field of the row the action is fired on, `$model:/PATH`
a value of the model. The ids (`f1`, `a1`, `t1`) are stable within one snapshot
only. A popup is modal, so while one is open only the popup is described — the
page behind it is not reachable in the browser either; a popover is not, and
the page stays described beside it.

## The four tools

| Tool | Input | Answer |
| --- | --- | --- |
| `app_list` | `filter` | the app classes that can be started |
| `app_start` | `app`, `values`, `max_rows` | the first snapshot of the app |
| `app_describe` | `session`, `max_rows` | the current snapshot, from memory — nothing is sent |
| `app_act` | `session`, `values`, `event`, `args`, `row`, `max_rows` | the next snapshot |

`app_act` fills `values` — keyed by field id, model path or name, or a table
cell as `"<table>/<row>/<COLUMN>"` — and fires `event`, by name or by action id.
Selecting a table row is setting its selection field. Without an event the
values stay **pending**, the way typing does in a browser; with one they go out
as the same model delta the UI5 frontend would send.

**Everything is validated against the snapshot before anything is sent.** An
event that is not on the screen, a field that is not editable, a choice
outside its values, the page's controls while a popup is open — each is
refused with a sentence naming what is wrong and what *is* allowed, and a
refused act changes nothing. `@CLOSE_POPUP` and `@CLOSE_POPOVER` close a dialog
the way the browser closes it, without a roundtrip. Only the current draft id
of a session is accepted.

A short session against the value-help sample of abap2UI5/samples:

```text
app_start { app: "z2ui5_cl_smp_app_009" }
  -> fields f1..f5 (f3 "Input with value", /S_SCREEN/COLOR_02, text, ""),
     actions a1 POPUP_TABLE_VALUE (valueHelpRequest of f3), ..., a5 BUTTON_SEND
app_act { session, values: { f4: "Smith" }, event: "POPUP_TABLE_VALUE" }
  -> layer "popup", table t1 /T_SUGGESTION_SEL (6 rows, Single, editableCells [SELKZ]),
     action a1 POPUP_TABLE_VALUE_CONTINUE
app_act { session, values: { "/T_SUGGESTION_SEL/2/SELKZ": true }, event: "POPUP_TABLE_VALUE_CONTINUE" }
  -> layer "main", f3 = "BLACK", f4 = "Smith", message { toast, "value selected" }
```

The full reference — every derivation rule, the descriptors, the refusals —
is [`docs/agent-snapshot.md`](https://github.com/abap2UI5/mcp-server/blob/9ca6cdf220acab2db938bcce123c81d6640c27ee/docs/agent-snapshot.md)
in the MCP server's repository, at the commit this page was written against.

## While developing: the MCP server

The [MCP server](/advanced/mcp_server) runs the four tools against its local
backend — the framework transpiled to Node, with your deployed apps on top —
so an agent can operate the app it just wrote with no SAP system at all. They
need what `run_app` needs: a `build_backend` first (Level 3 of the setup).
`app_list` lists the deployed apps and the framework's own; an app deployed
after the last build is not listed until the next one.

The tools speak the protocol and answer with data; `interact_app` boots a
headless browser, drives it with CSS selectors and answers with a picture. Use
`app_start` and `app_act` to check what an event *does* — the model, the
messages, the next screen — and `interact_app` for what only a rendered page
shows.

## On your system from VS Code

The [VS Code extension](/advanced/vscode#for-ai-agents) adds the same four
tools to its own MCP server, the one that knows your configured systems. They
take the same input, answer with the same snapshot and refuse the same
things — but on a real system they **act for real, as you**: an event may
save, post or delete data.

That is why they do nothing until you allow them:
`abap2ui5.agent.enableAppTools` is off by default and can only be switched on in
your **User** settings — a workspace cannot. While it is off, the tools stay
listed and answer with how to switch them on, without contacting the system.

- They work on the system *Select System* made active, through the same proxy
  and credentials as the preview; the requests show in its traffic log. The
  optional `system` argument must name the active system — the tools never
  switch, and a session refuses to continue once another system is active.
- The launch URL has to carry the class as a query parameter
  (`…/sap/bc/z2ui5?app_start={class}&sap-client=100`).
- `app_list` is a name search over ADT; whether a class implements
  `z2ui5_if_app` is not checked. A class in a namespace (`/ns/cl_app`) starts
  too.

## In production: the agent add-on

[**abap2UI5-addons/agent**](https://github.com/abap2UI5-addons/agent) puts an
MCP endpoint into the SAP system itself, written in ABAP: any MCP client that
connects to a remote server over HTTP calls `/sap/bc/z2ui5_agent`, and the
endpoint operates the app inside the system as the user who logged on. No
Node, no browser, no technical user.

Underneath it is the [headless frontend](#testing-the-roundtrip-in-abap) that
plays the browser's half of the protocol in ABAP. Every MCP call is one HTTP
request, so a session lives between calls as the abap2UI5 draft plus one row
holding what a browser would remember — the views of the open layers and the
pending values.

### Installing and enabling it

Install, each with abapGit: the framework, then
[abap2UI5/headless-frontend](https://github.com/abap2UI5/headless-frontend),
then the add-on from the branch of your platform — `standard` for Standard ABAP
from 7.50 (it brings the ICF node), `cloud` for ABAP Cloud (you create an HTTP
service and a communication arrangement in ADT).

The endpoint is **disabled** after installation. To switch it on:

1. Add the first agent administrator once, as a developer in the system:
   `z2ui5_cl_agent_settings=>admin_add( '<USER>' )`, from a console class or
   the class test in SE24.
2. Open the settings app `?app_start=z2ui5_cl_agent_app_admin` and switch
   *Agents may operate apps* on. Here you also allow or deny app classes,
   classify events, mark sensitive fields, set the handover page and clean up
   the audit log, which `?app_start=z2ui5_cl_agent_app_audit` shows.
3. Activate the ICF node `/sap/bc/z2ui5_agent` in SICF and check its logon
   procedure (on ABAP Cloud: expose the HTTP service).
4. Opt your apps in — nothing is reachable before that.

### Opting an app in

An app opts in by implementing `z2ui5_if_agent_app` next to `z2ui5_if_app`.
Its one method, `describe( )`, says what the app is and which events an agent
must not fire on its own:

<!-- playground: no Run button — z2ui5_if_agent_app lives in abap2UI5-addons, which the playground does not carry -->
```abap
CLASS zcl_sales_order_app DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    INTERFACES z2ui5_if_agent_app.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sales_order_app IMPLEMENTATION.

  METHOD z2ui5_if_agent_app~describe.

    result-description = `Create and change sales orders`.
    result-t_event = VALUE #( ( event = `DELETE*` policy = z2ui5_if_agent_app=>cs_policy-forbidden )
                              ( event = `POST`    policy = z2ui5_if_agent_app=>cs_policy-confirm ) ).
    result-t_sensitive = VALUE #( ( `/MS_PARTNER/IBAN` ) ).

  ENDMETHOD.

  METHOD z2ui5_if_app~main.

    " the app itself, unchanged

  ENDMETHOD.

ENDCLASS.
```

An empty `describe( )` is fine: every event is allowed then. Event names may
be patterns (`DELETE*`, `*_POST`), the first matching rule decides, and
`default_policy` covers the events no rule names. `t_sensitive` lists model
paths or names whose values the audit log masks. `describe( )` runs on a fresh
instance of the class, never on the running app, so it answers for the class
and not for a state of it.

An administrator can allow classes that do not implement the interface, deny
classes that do, and classify events on top of what the app says — the
stricter verdict wins. The add-on's own apps are never reachable by an agent.

### Allowed, confirm, forbidden

| Policy | What an agent may do |
| --- | --- |
| `allowed` | fire the event like a user pressing the button |
| `confirm` | nothing — only a human may fire it, and the screen is handed over |
| `forbidden` | nothing — the event is refused |

**The handover.** When an agent tries a `confirm` event, `app_act` refuses and
answers with the abap2UI5 URL of the session's draft, such as
`/sap/bc/z2ui5#/app/ZCL_SALES_ORDER_APP/8F3A…`. The agent passes it on to its
user, who opens it in the browser: abap2UI5 restores the very state the agent
prepared — same user, same draft — and the human checks it and presses the
button. Values still pending are listed in the refusal; a dialog that was open
is not part of the draft, so the app shows its main view.

### Security model

- **Disabled by default**, and only opted-in or explicitly allowed apps can be
  listed or started.
- **The real SAP user, always.** Authentication is the logon of the HTTP
  request — Basic, OAuth, certificates, principal propagation. The app runs as
  that user, with its own validation and `AUTHORITY-CHECK`s, so an agent can
  never do what its user could not do in the browser.
- **Sessions belong to their user** and expire with their draft.
- **Every call is audited**: user, session, app, operation, event, arguments
  (masked for password inputs and sensitive fields), outcome, and the MCP
  client's name and version. Users see their own entries, administrators
  everybody's.
- **A web page cannot drive it**: a request with an `Origin` of another host
  is refused, and only `Content-Type: application/json` is accepted, so the
  user's single sign-on cookies are of no use to a foreign page.

### Connecting a client

The endpoint speaks MCP over HTTP, the transport for remote servers. With
Claude Code:

```sh
claude mcp add --transport http abap2ui5-agent https://host:44300/sap/bc/z2ui5_agent \
  --header "Authorization: Basic $(printf '%s' 'USER:PASSWORD' | base64)"
```

Prefer a token over a password where your system issues one —
`--header "Authorization: Bearer <token>"`. SAP systems do not implement MCP's
own authorization discovery, so the credential goes in as a header rather than
through the client's OAuth flow. Then ask the agent to call `app_list`.

### Limits of the add-on

- **Stateful apps** (`client->set_session_stateful( )`) cannot be operated:
  a stateful session lives within one HTTP request, and an MCP call is one
  request. `app_start` refuses them.
- Values travel as text, as the browser sends them. A multi-choice value and
  an edit inside a structure that also holds a table cell cannot be sent yet
  and are refused before anything is sent.
- Labels, texts and messages are what the app wrote, in the user's logon
  language.

## What the snapshot cannot see

Common to all three: anything computed in the browser — formatters, composite
bindings, complex expression bindings; client-only state such as the open tab
of an `IconTabBar` or a selection without a `selected` binding; named models
and XML templating; custom controls (listed under `unsupported`, not
described); frontend actions such as opening a new tab or copying to the
clipboard (listed, never performed); nested tables, file uploads, drag and
drop. An app built from standard controls bound to its own attributes is
operable as it stands.

## Testing the roundtrip in ABAP

The engine under the add-on is useful on its own, and older than the add-on:
[abap2UI5/headless-frontend](https://github.com/abap2UI5/headless-frontend)
plays the browser's side of the protocol inside ABAP, which makes a whole user
session an ABAP Unit test. [Step 12](/tutorials/walkthrough/step-12#testing-the-roundtrip)
of the walkthrough shows it; the add-on uses its session API — `resume( )`,
`get_state( )` and `get_layers( )` — to continue a session in the next HTTP
request.

## Next Steps

- [MCP Server](/advanced/mcp_server) — the sandbox the development tools run in
- [VS Code Extension](/advanced/vscode) — the system tools and the setting that
  allows them
- [Agent Setup](/advanced/agent_setup) — the rest of an agent's setup, in rising
  order of effort
- [Monitoring](/configuration/monitoring) — the same roundtrips, logged
