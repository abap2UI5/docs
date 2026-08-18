---
outline: [2, 4]
---
# abap2UI5 linter

The view an abap2UI5 app shows does not exist until the app runs. It is built
by a chain of `z2ui5_cl_ui5_view_builder` calls and handed to the browser as a
string, so the ABAP compiler never sees a control name, and no UI5 tooling ever
sees the class. Between the two sits a whole class of defect nothing catches: a
binding path the model has no field for, an event argument the control never
delivers, a control that exists in the newest UI5 and not on the release your
users are on. The app activates, the class compiles, and the screen comes up
empty.

[`@abap2ui5/linter`](https://github.com/abap2UI5/linter) closes that gap. It
reconstructs the view from the builder chain and judges the class and the view
**together** — with no SAP system, no deployment and no UI5 project.

## Try it in one line

```sh
npx @abap2ui5/linter src
```

That is the whole of it: no install, no configuration, no system. The package
is about 240 kB and pulls in nothing else, so the line above is a fast one.

```
src/zcl_my_app.clas.abap
  36:14  error  sap.m.Page has no aggregation contentt - typo?                       unknown-aggregation
  40:22  error  sap.m.Button type="Emphasised" is not a valid value (allowed: ...)   invalid-property-value
```

Files with nothing to report are not printed.

::: tip Name the package, not the command
The executable the package installs is called `abap2ui5lint` — and so are the
config file, the baseline and the source waivers further down. But
`npx abap2ui5lint` would go looking for a *package* of that name, which is not
this one. With `npx`, write `@abap2ui5/linter`; once it is installed in a
project, `npx abap2ui5lint` is the command.
:::

Then, when you want it to stay green:

```sh
npm install -D @abap2ui5/linter          # pin it for CI and for your machine
npx abap2ui5lint --init                  # write a commented abap2ui5lint.jsonc
```

Starting a new app repository? [Working Off-Stack](/advanced/working_off_stack)
walks through **app-template**, which ships with all of this already wired up.

## Reading a finding

Every finding carries four things, and all four matter:

```
src/zcl_my_app.clas.abap
  20:9   error    a( n = `title` ) without an element to attach it to …            attribute-without-element
  31:18  error    text is set twice on the same control …                          duplicate-property
  44:22  warning  sap.m.GenericTile systemInfo is @since 1.92.0 …                  member-too-new
  51:35  hint     event NO_HANDLER is raised but never handled …                   event-without-handler

4 problems (2 errors, 1 warning, 1 hint)
abap2ui5-linter: 12 file(s), 1 failing, 0 skipped (target SAPUI5 1.71, metadata from 1.151.0, failing on warning)
```

**Line and column** point into the ABAP source you wrote, not into the
generated view. **The message** is ready to act on. **The severity** says how
much it matters:

| Severity | Meaning |
| --- | --- |
| `error` | the app breaks: a dump, a control that will not load, a value UI5 rejects, or a defect that silently destroys the view |
| `warning` | it works where it was written, but not necessarily on the target system (version floor, deprecation) — or the data behind it is not what the author thinks it is |
| `hint` | worth knowing, never wrong by itself |

**The rule id** at the end of the line is the key to everything else: it is what
you name in the config to change or switch a rule off, what a source waiver
names, and the anchor of that rule's page in the full reference at
[abap2ui5.github.io/linter](https://abap2ui5.github.io/linter/) — every rule is
documented one page away, with what it means and why it exists.

Which severities break the build is a separate decision from what is reported:
`--fail-on error|warning|hint|never` sets the exit code (default `warning`,
`--advisory` is the same as `--fail-on never`). Everything is always *reported*.

| Exit code | |
| --- | --- |
| `0` | clean, or nothing above `--fail-on` |
| `1` | a finding at or above `--fail-on`, or a render error |
| `2` | bad usage or a broken config file |

## What it checks

Two gates run over every file, and they answer different questions.

### The property gate

Everything the view writes is resolved against a **UI5 metadata snapshot** —
988 controls with their full member lists and types, 219 enums, generated from
the OpenUI5 sources. It is instant, needs no browser, and catches the whole
family of *this name does not exist* defects:

| | |
| --- | --- |
| a control, property, aggregation or enum value UI5 does not have | `sap.m.Shell2`, `Button typ=`, `Page contentt`, `type="Emphasised"` |
| a name UI5 does not have **yet** on your target release | a member `@since 1.92` on a 1.71 system — the permanent abap2UI5 question |
| an icon that is wrong, too new, or gone again | `sap-icon://textFormatting` matches nothing ever (it is `text-formatting`), `information` arrived in 1.80 |
| structural defects in the view | two controls in a 0..1 aggregation, a duplicate `id`, an undeclared namespace prefix, an aggregation inside an aggregation |
| layout that breaks on older releases | a `ToolbarSpacer` inside a `sap.m.Bar` deletes every sibling after it before 1.76 |

Bindings and expressions are never value-checked — their value is a runtime
matter — custom namespaces stay out of scope, and a control whose inheritance
chain leaves the snapshot is never reported as missing a member. The gate does
not guess.

### The render gate

The view is then loaded with a real `XMLView.create` in headless Chromium
against a local OpenUI5 runtime, with UI5 **future mode** active. This is the
only way to catch a view that does not merely render wrongly but **fails to
load at all** — a broken expression binding, a strict property-type violation,
a control that resolves to a 404.

It needs a real UI5 runtime, so it ships as a second package — one deliberate
install rather than a surprise attached to the first:

```sh
npm install -D @abap2ui5/render-runtime   # the UI5 runtime, once
npx playwright install chromium           # and its browser
```

Without it the property gate still runs in full, and says so
(`--no-render` asks for that explicitly). In CI write `--render` instead: that
turns a missing runtime into an error rather than a silent fallback, so a gate
your config promised cannot quietly disappear from a green pipeline.

### The abap2UI5 rules

On top of the UI5 rules sit the ones that are the reason this tool exists
rather than a UI5 linter: the defects that live *between* the ABAP class and
the view it builds, and stay **silent** at runtime. A few, to give the shape of
them:

| Rule | What it catches |
| --- | --- |
| `unknown-binding-path` | a hand-written `{/TYPO}` the derived model has no path for — the field just stays empty. Inside a bound aggregation a relative `{TYPO}` is resolved against the **row**, so a misspelled column is caught too |
| `frozen-view-builder` | the class still builds its view with `z2ui5_cl_xml_view`, the frozen predecessor. It matters more than it looks: that API is what nearly all public abap2UI5 material shows, and therefore what a language model writes when asked for an app |
| `binding-to-nonpublic` | a `PROTECTED`/`PRIVATE` attribute bound — only `PUBLIC` attributes are serialized into the model, so the first roundtrip fails with `BINDING_ERROR` |
| `binding-to-local` | a local variable bound — the instance is serialized across the roundtrip, the method stack is not, so the value is lost |
| `unconverted-abap-boolean` | an ABAP boolean written straight into the view: it arrives as `'X'`/`' '` and UI5 reads any non-empty string as true, so `visible = abap_false` makes the control **visible** |
| `missing-on-navigated-branch` | a lifecycle dispatcher with no `check_on_navigated( )` branch — the app works standalone and goes blank the first time another app hands control back |
| `view-never-displayed` | a view is built and never handed to the client: an empty page, no error |
| `source-line-too-long` | a source line over 255 characters — the class does not fail to lint, it fails to **import**, and abapGit leaves an empty class stub behind |
| `chain-indentation` | a builder call whose indentation contradicts the tree it builds. The chain is the only picture of the view's structure there is, and nothing else in the toolchain formats it |

There are more than eighty rules in total. The full list, each with the
paragraph explaining why it exists, is the
[rule reference](https://abap2ui5.github.io/linter/) — one page, searchable,
one anchor per rule id.

### What it cannot do, by design

- **Event roundtrips and visual fidelity** stay with a live run — that is what
  the [MCP server](/advanced/mcp_server)'s `run_app` and the
  [VS Code extension](/advanced/vscode)'s `F9` are for.
- A class that builds view parts in helper methods **without the handle idiom**
  cannot be statically reconstructed. The render gate is then skipped with a
  notice rather than validating the wrong view; the property gate still runs on
  what was reconstructed.
- A model field the class fills **in code** (a `LOOP` in `model_init`) has no
  static value, so the render gate only ever sees what a literal seed sets.

## Seeing the view without a system

The render gate loads the view to find out whether it survives creation, then
throws it away. `--screenshot` keeps it standing and photographs it:

```sh
abap2ui5lint zcl_my_app.clas.abap --screenshot app.png
abap2ui5lint zcl_my_app.clas.abap --screenshot app.png --screenshot-size 390x844,1280x900
abap2ui5lint zcl_my_app.clas.abap --screenshot app.png --screenshot-theme sap_horizon_dark
```

Looking at an abap2UI5 view has always meant activating the class on a system
and launching the app. Here it is reconstructed from the builder calls, seeded
with a model derived from the class's own `TYPES`/`DATA`, and rendered against
the local OpenUI5 runtime in the theme and viewport you name. Several viewports
render in **one** browser session — the launch and the UI5 boot cost more than
every render together, so a phone-and-desktop matrix is barely more expensive
than one picture.

**The empty-table problem.** The model is derived from what the class seeds
*literally*, because that is all a static reconstruction can know. A table
filled by a `SELECT` is therefore empty, and a list view — most real apps —
photographs as *No data*. So a JSON file next to the source is picked up as
preview data, by convention and without a flag:

```
src/zcl_travel_list.clas.abap
src/zcl_travel_list.mock.json     ->  { "MT_ROWS": [ { "NAME": "Berlin - Rome" } ] }
```

It is **merged over** the derived model rather than replacing it, so the file
only has to name the table you want filled; every other binding keeps
resolving. `--screenshot-model <file.json>` does the same without a file lying
next to the source.

What this is not: a preview of the *app*. Nothing round-trips, no event reaches
ABAP, and the data is a mock model rather than what a system would serve. It is
the view, rendered.

## Fixing what is mechanical

Some rules carry an exact correction and are rewritten in place:

```sh
npx @abap2ui5/linter src --fix          # or --fix-dry-run to see it first
```

Among them: `client->_bind_edit( )` rewritten to `client->_bind( )`, the empty
`*_model_update( )` calls deleted, `client->_event_client( )` rewritten to
`client->follow_up_action( )`, a missing `$` inserted in an event argument, a
missing `xmlns:` declaration added at the view root.

Nothing else is touched. A correction that would have to **guess** — which of
two duplicate attributes survives, what event a `_bind` on an event slot meant
to raise — is worse than the finding it replaces, so it is reported instead.

## Waiving a rule

Three scopes, from narrow to wide.

**One line** — a comment in the source, carried by whatever comment syntax the
file has:

```abap
" abap2ui5lint-disable-next-line unknown-binding-path -- filled in a LOOP
```

`-disable-line` waives the line the comment sits on, `-disable` … `-enable`
waives a block, and naming no rule waives every rule. Everything after `--` is
a reason and is ignored.

**One repository** — the `rules` block of the config file: switch a rule off,
give it a different severity, or exclude files from it.

**One member** — `--allow sap.m.Avatar.displaySize` keeps using a control or
member newer than your floor, without touching the rule itself.

### Adopting it on a codebase that already exists

Switching a linter on over a grown codebase reports everything at once, which
is the moment most adoptions stop. The **baseline** freezes that debt instead
of hiding it:

```sh
npx abap2ui5lint src --update-baseline     # writes abap2ui5lint-baseline.json
```

Commit that file and name it in the config. From then on the frozen findings
are counted but not listed, **new** findings fail normally, and an entry whose
finding is gone is stale and fails too — so the baseline only ever shrinks.
Its keys are line-free, so moving code around does not invalidate them.

## Which UI5 does it check against?

Two settings decide, and both describe **your system**, not your wish.

`--ui5 <version>` is the UI5 release your system runs (default `1.71`, which is
what most systems serve). It drives both directions: a control or member
introduced *after* it is a finding, and a deprecation is only reported once it
is *in effect* at that version.

`--distribution sapui5|openui5` says which distribution the system serves.
SAPUI5 ships libraries OpenUI5 does not — `sap.ui.comp` (Smart controls),
`sap.suite.*`, `sap.ushell`, `sap.fe` — so a SmartTable is perfectly fine on
SAPUI5 and a guaranteed runtime error on OpenUI5. With `openui5` those controls
are reported as `sapui5-only-control`.

## The configuration file

Pin the settings in the repository instead of repeating CLI flags — the same
idea as `abaplint.jsonc`. `abap2ui5lint --init` writes a commented starter
version; discovery is eslint-style, and precedence is CLI flag > config file >
built-in default.

```jsonc
{
  "$schema": "./node_modules/@abap2ui5/linter/data/abap2ui5lint.schema.json",
  "paths": ["src"],          // used when the CLI got no positional paths
  "ui5": "1.71",             // the UI5 floor for the property gate
  "distribution": "sapui5",  // or "openui5"
  "failOn": "warning",       // error | warning | hint | never
  "render": true,            // true is a PROMISE: a missing runtime fails
  "allow": [],               // e.g. ["sap.m.Avatar.displaySize"]
  "baseline": "abap2ui5lint-baseline.json",
  "rules": {
    "missing-accessibility": false,        // off
    "member-deprecated": "hint",           // another severity
    "event-without-handler": {
      "severity": "warning",
      "exclude": ["/test/"]                // file regex, case insensitive
    }
  }
}
```

The `$schema` line gives editors completion and validation for every key and
every rule id — the schema is generated from the rule registry, so it cannot
drift from the linter. Unknown keys and unknown rule ids fail loudly, which is
typo protection rather than strictness for its own sake.

## In your pipeline

The GitHub Action is the whole setup:

```yaml
jobs:
  lint-views:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: abap2UI5/linter@v0
        with:
          paths: src
          min-ui5: '1.71'
          fail-on: warning
```

Findings are annotated onto the pull-request diff by default. Ask for
`screenshots: build/screenshots` and the job also **photographs every checked
view** — the review artefact CI could not produce before, and it runs whether
the check passed or failed, because the failing run is where a reviewer most
wants to see the view.

The render gate runs by default, which costs the job a UI5 runtime and a
Chromium download; `render: false` skips both and still runs every static rule.
`@v0` is a moving tag that follows the newest `0.x` release — which is the
point of a linter, and the reason to pin `@v0.1.0` where a build has to stay
reproducible.

Other output formats fit other places: `--format markdown` for a job summary,
`--format json` for a tool, and `--format sarif` for GitHub code scanning, which
puts findings in the Security tab and as native PR annotations.

### Badges

A run can write two [shields.io](https://shields.io/badges/endpoint-badge)
endpoint files, because a repository has two different things to say:
`--badge-corpus` says what the repository **is** (`148 apps · 172 views · 2,176
controls`, blue, a fact), and `--badge` says what the gate **said**
(`83 rules passed`, green/yellow/red, a verdict). Both are written on every
run, the failing one included.

### What a clean run still tells you

"148 files, no findings" reads identically whether two thousand controls were
judged or the reconstruction quietly produced empty views. So a run over more
than one file closes with what it **looked at**:

```
sources    148 app classes
views      172 documents reconstructed, nested 11 deep, 7 classes produced none
judged     2,176 controls of 106 types, 548 bindings, 69 icons, 4,164 attributes
gates      properties 148 files, render 172 documents
baselined  476 findings suppressed by abap2ui5lint-baseline.json
```

`7 classes produced none` and a `judged` line of zeroes are the two readings
that say the gate is not seeing your corpus — the failure mode a green run
otherwise hides.

## As a library

The package is ESM, ships `types.d.ts` and has no dependencies:

```js
import { checkFiles, checkAbapSource, checkXmlSource } from '@abap2ui5/linter';

const results = await checkFiles(['src/zcl_my_app.clas.abap']);
// -> [{ file, findings: [...], renderErrors, docs, model }]
```

`screenshotFiles` is the same runtime taking pictures instead of a verdict,
returning PNGs as buffers — which is what an editor holding an unsaved buffer
needs. The `findings` subpath exposes the rule registry, the severity/message
annotation and the directive handling, so a consumer never reinvents them; the
`report` subpath holds the formatters, the run summary and the badge builders.

## Where else it runs

The same gates reach you through three other doors, and all four agree because
they read the same `abap2ui5lint.jsonc`:

| | |
| --- | --- |
| the **CLI** and the **GitHub Action** | this page |
| the [VS Code extension](/advanced/vscode) | as diagnostics while you type, with the quick fixes and the baseline |
| the [MCP server](/advanced/mcp_server) | as the `validate_view` tool, so an AI agent can check its own work |
| [app-template](/advanced/working_off_stack) | the CLI and the workflow, already wired up |
