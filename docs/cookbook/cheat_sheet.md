---
outline: [2, 4]
---
# Cheat Sheet

A one-page recap of the rules that decide whether an abap2UI5 app works or misbehaves in a way that is hard to debug. Each row links to the recipe that explains it in full — read this page as a checklist, not as an introduction.

| Rule | Why it matters |
|---|---|
| Implement `z2ui5_if_app` and put everything in the single `main` method | It is the only entry point the framework calls — on the initial load *and* on every user interaction → [Life Cycle](/cookbook/event_navigation/life_cycle) |
| Dispatch with one `IF` / `ELSEIF` chain over `check_on_init( )`, `check_on_navigated( )` and `check_on_event( )` | Each check answers for its own phase only; separate `IF` blocks let two branches run in the same roundtrip → [Life Cycle](/cookbook/event_navigation/life_cycle) |
| Always call `view_display( )` in the `check_on_navigated( )` branch | After a called app returns via `nav_app_leave( )`, the browser still shows *its* view — without a re-display the user is left on a stale or blank screen → [Navigation](/cookbook/event_navigation/navigation/inner_app) |
| Declare every attribute you bind in the `PUBLIC SECTION` | Binding works via dynamic `ASSIGN` and cannot reach `PROTECTED` / `PRIVATE`; the roundtrip fails with `BINDING_ERROR` → [Binding](/cookbook/model/binding) |
| Keep state in attributes, not in local variables | Between two events the app instance is serialized into a draft on the SERVER and read back — attributes survive at any visibility; locals, `DATA(...)` declarations, open cursors and locks do not → [Statefulness](/cookbook/expert_more/statefulness) |
| Respect the UI5 aggregation rules even though the builder does not enforce them | The builder lets you nest anything inside anything; UI5 does not, and the mismatch surfaces as broken rendering rather than a syntax error → [Definition](/cookbook/view/definition) |
| Never use a deprecated UI5 control | It renders today and vanishes on the next UI5 upgrade — and the XML is passed through unchanged, so nothing in the framework stops you → [linter](/advanced/linter), which reports it against the release your system runs |
| Take a ready-made dialog from the popups add-on before building your own | Confirm, select, file up/download, ranges, PDF and about a dozen more, versioned on their own → [popups add-on](https://github.com/abap2UI5-addons/popups) |
| Use backtick string literals (`` ` ``) | Project-wide convention in the framework, the samples and this documentation; keeps ABAP string handling consistent |

::: warning An ABAP flag passed as `v` does not reach the view as a boolean
`a( )` takes **either** `v` — any string expression — **or** `b`, an ABAP boolean. Only `b` converts, and it is the form to use whenever the value comes out of ABAP:

```abap
    )->tag( `Button`
        )->a( n = `text`    v = `Save`
        )->a( n = `enabled` b = abap_false )   " → enabled="false"
```

Through `v` the flag is written verbatim: `abap_true` arrives in the view as `enabled="X"` and `abap_false` as an empty value. Neither is the `true` / `false` UI5 expects, and neither is a syntax error — the view renders, with the control in the wrong state.

A **literal** is a string and belongs in `v`, unquoted by any flag variable:

```abap
    )->a( n = `enabled` v = `false` )
```

Any expression that yields a flag works in `b`, so there is no reason to convert by hand: `` )->a( n = `visible` b = xsdbool( lines( mt_item ) > 0 ) ) ``.
:::

## Next Steps

- [Common Failures](/cookbook/troubleshooting/common_failures) — symptoms and their usual causes
