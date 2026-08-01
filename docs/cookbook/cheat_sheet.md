---
outline: [2, 4]
---
# Cheat Sheet

A one-page recap of the rules that decide whether an abap2UI5 app works or misbehaves in a way that is hard to debug. Each row links to the recipe that explains it in full — read this page as a checklist, not as an introduction.

| Rule | Why it matters |
|---|---|
| Implement `z2ui5_if_app` and put everything in the single `main` method | It is the only entry point the framework calls — on the initial load *and* on every user interaction → [Life Cycle](/cookbook/event_navigation/life_cycle) |
| Dispatch with one `IF` / `ELSEIF` chain over `check_on_init( )`, `check_on_navigated( )` and `check_on_event( )` | Each check answers for its own phase only; separate `IF` blocks let two branches run in the same roundtrip → [Life Cycle](/cookbook/event_navigation/life_cycle) |
| Always call `view_display( )` in the `check_on_navigated( )` branch | After a called app returns via `nav_app_leave( )`, the browser still shows *its* view — without a re-display the user is left on a stale or blank screen → [Navigation](/cookbook/event_navigation/navigation) |
| Declare every attribute you bind in the `PUBLIC SECTION` | Binding works via dynamic `ASSIGN`; `PROTECTED` and `PRIVATE` attributes are silently ignored → [Binding](/cookbook/model/binding) |
| Keep state in public attributes, not in local variables | Between two events the controller is serialized to the client and back — locals, `DATA(...)` declarations, open cursors and locks do not survive → [Statefulness](/cookbook/expert_more/statefulness) |
| Respect the UI5 aggregation rules even though the builder does not enforce them | `z2ui5_cl_xml_view` lets you nest anything inside anything; UI5 does not, and the mismatch surfaces as broken rendering rather than a syntax error → [Definition](/cookbook/view/definition) |
| Never use a deprecated UI5 control | It renders today and vanishes on the next UI5 upgrade → [Deprecated Controls](/cookbook/view/deprecated_controls) |
| Check the built-in popups before building a custom dialog | Roughly twenty ready-made dialogs ship with the framework — confirm, select, file up/download, ranges, PDF, … → [Built-In](/cookbook/popup_popover/built_in) |
| Use backtick string literals (`` ` ``) | Project-wide convention in the framework, the samples and this documentation; keeps ABAP string handling consistent |

::: warning `abap_false` in `_generic( )` disappears from the view
In the **fluent API** both flags work as expected — the builder inspects the type of the value it receives and writes `true` or `false`:

```abap
view->button( text = `Save` enabled = abap_false ).   " → enabled="false"
```

In **`_generic( t_prop = ... )`** they do not. The property table stores values as `string`, so the boolean type is lost on the way in:

- `abap_true` still ends up correct — the serializer renders a value of `X` as `true`.
- `abap_false` is a blank and becomes an empty string, and properties with an empty value are dropped from the XML entirely. The attribute is never written, so a control whose UI5 default is `true` (`enabled`, `visible`, …) silently stays enabled.

Write the literal instead — it is unambiguous in both directions:

```abap
view->_generic( name = `Button` ns = `sap.m`
    t_prop = VALUE #( ( n = `text`    v = `Save` )
                      ( n = `enabled` v = `false` ) ) ).
```
:::

#### Next Steps

- [Overview](/cookbook/overview) — the full map of cookbook topics
- [Common Failures](/cookbook/troubleshooting/common_failures) — symptoms and their usual causes
