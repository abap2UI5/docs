---
outline: [2, 4]
---
# abap2UI5 linter

The view an abap2UI5 app shows does not exist until the app runs. It is built
by a chain of `z2ui5_cl_ui5_view_builder` calls and handed to the browser as a
string, so the ABAP compiler never sees a control name, and no UI5 tooling ever
sees the class. That gap is what
[`@abap2ui5/linter`](https://github.com/abap2UI5/linter) closes: it
reconstructs the view from the builder chain and judges the class and the view
**together**.

```sh
npx @abap2ui5/linter src
```

No SAP system, no install, no configuration. The package is small and pulls in
nothing else, so that line is a fast one.

Name the **package** there, not the command: the executable it installs is
called `abap2ui5lint` — and so are the config file, the baseline and the
in-source waivers below — but `npx abap2ui5lint` would go looking for a package
of that name, which is not this one.

## What it checks

Two gates.

**The property gate** resolves everything the view writes against a UI5
metadata snapshot generated from the OpenUI5 sources: controls, properties,
aggregations, enum values, icons. It reports a name UI5 does not have, and — the
part that matters on a real landscape — a name UI5 does not have **yet** on the
release you target. The floor is `1.71` by default, which is what most systems
serve.

**The render gate** then loads every view with a real `XMLView.create` in
headless Chromium, with UI5 future mode on. That is the only way to catch a
view that does not merely render wrongly but fails to load at all. It needs a
UI5 runtime, which ships as a separate package:

```sh
npm install -D @abap2ui5/render-runtime
npx playwright install chromium
```

Without it the property gate still runs in full — `--no-render` says so
explicitly. In CI, write `--render` instead, so a missing runtime is an error
rather than a silently skipped gate.

On top of the UI5 rules sit the abap2UI5-specific ones, which are the reason
this exists rather than a UI5 linter: the defects that live *between* the class
and its view and stay silent at runtime. A binding path the model has no field
for. A `PROTECTED` attribute bound, which only `PUBLIC` attributes survive. An
ABAP boolean written into the view raw, where UI5 reads `'X'` and `' '` both as
strings. A `check_on_navigated( )` branch that never re-displays. And
`frozen-view-builder` — a class still built with `z2ui5_cl_xml_view`, the
frozen predecessor, which is what most public abap2UI5 material still shows.

Every finding carries a severity (`error`, `warning`, `hint`), a message, the
line and column, and a **rule id**. The id is the key everywhere else: in the
`rules` block of the config, in a source directive that waives it, and as the
anchor of its page in the rule reference at
[abap2ui5.github.io/linter](https://abap2ui5.github.io/linter/).

## Fixing and adopting

Some rules carry an exact correction and are rewritten in place:

```sh
npx @abap2ui5/linter src --fix    # or --fix-dry-run to see it first
```

Only corrections that need no guessing — an obsolete call renamed, a missing
`$` in an event argument, a missing `xmlns:` declaration. Anything that would
have to choose between two plausible outcomes is reported instead.

Switching a linter on over a codebase that already exists reports everything at
once, which is the moment most adoptions stop. The **baseline** freezes that
debt instead of hiding it:

```sh
npx @abap2ui5/linter src --update-baseline
```

Commit the resulting `abap2ui5lint-baseline.json` and name it in the config.
The frozen findings are then counted but not listed, **new** findings fail
normally, and an entry whose finding is gone fails too — so the baseline only
ever shrinks.

## Where it runs

The CLI, a GitHub Action (`abap2UI5/linter@v0`), a library, inside the
[VS Code extension](/get_started/tooling#run-the-app-from-the-editor) as
diagnostics while you type, and as the `validate_view` tool of the
[MCP server](/get_started/ai). The
[app-template](/get_started/project_setup) has the CLI and the workflow wired
up already.
