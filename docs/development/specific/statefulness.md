---
outline: [2, 4]
---
# Statefulness

By default, abap2UI5 runs **stateless** like any other UI5 freestyle app, with only REST calls to the ABAP backend. Each roundtrip starts a fresh ABAP session, executes the controller, serializes the app state back into the client, and tears the work process down again. Nothing on the server survives between two clicks — and that is exactly what makes the runtime scale.

For the small set of cases where a sticky backend session is needed — classic GUI-style locking, heavy session-bound resources, RFC connections that must stay open — abap2UI5 can also run in **stateful** mode.

#### Stateless (default)
| Phase | What happens |
|---|---|
| **Request** | Browser sends the serialized app state to the backend |
| **Process** | A free work process picks it up, runs the controller |
| **Response** | New view + state are returned, the work process is released |

No work process is pinned, no enqueue is held, no `SET/GET` parameters survive. This is the recommended default for productive apps.

#### Stateful Sessions
For private and on-premise systems, you can switch a running app to stateful mode. The same work process then handles every subsequent roundtrip of *this* user, and ABAP globals, locks, and open RFC connections survive between events:

```abap
IF client->check_on_init( ).
  client->set_session_stateful( ).
ENDIF.
```

Release it explicitly on exit:

```abap
client->set_session_stateful( abap_false ).
```

See `Z2UI5_CL_DEMO_APP_135` and `Z2UI5_CL_DEMO_APP_137` for complete examples.

#### When to Use It
Stateful sessions are useful when:

- You need a **classic SAP-GUI-style enqueue** that survives between user interactions — see [Locks → Stateful Session](./locks.md#stateful-session).
- You hold a **resource that is expensive to set up** on every roundtrip (e.g. an open RFC destination, a long-running selection, a cached internal table that cannot be re-derived cheaply).
- You are migrating a legacy dynpro flow one-to-one and want the same session semantics during the transition.

For everything else — and especially anything user-facing on a busy system — stick with the stateless default and use [optimistic checks](./locks.md#optimistic-check) or [soft locks](./locks.md#soft-lock) instead.

#### Trade-offs
::: warning
A stateful session **pins one work process per active user** for the lifetime of the app. On a system with a handful of dialog work processes, a few dozen idle stateful sessions are enough to starve everyone else. Use it sparingly, on internal low-traffic apps, and always pair `set_session_stateful( )` with an explicit `set_session_stateful( abap_false )` on every exit path — otherwise a leaked enqueue or a pinned work process blocks future users until the session times out.
:::

A few rules of thumb:

- **Always release on exit.** Wire `set_session_stateful( abap_false )` into your back / cancel / save handlers, not just the happy path.
- **Keep sessions short.** A stateful session is a held resource, not a place to park work indefinitely.
- **Public cloud is stateless only.** S/4 Public Cloud and BTP-hosted scenarios do not support stateful sessions — design for the stateless model from day one if cloud-readiness matters.

#### Related
- [Locks](./locks.md) — strategies for locking business objects, including stateless and stateful patterns
- [Statefulness, Locks (Advanced)](../../advanced/stateful.md) — deeper background on the underlying concept
- [Performance](../../configuration/performance.md) — sizing considerations for productive deployments
