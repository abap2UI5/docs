---
outline: [2, 4]
---
# Controller

abap2UI5 gives you great flexibility in how you structure apps. Most sample apps follow the pattern below. Use it as a starting point, and tweak it or build a wrapper around abap2UI5 for more specific behavior.

The idea: every request enters the `main` method, and you use `CASE` to dispatch between initialization, navigation returns, and user events.

The following sections walk through each phase:

- [Life Cycle](/development/controller/life_cycle) — the overall dispatch pattern in `main`
- [Init](/development/controller/init) — first-time setup and rendering the view
- [Event](/development/controller/event) — handling user actions
- [Navigated](/development/controller/navigated) — returning from another app
