---
outline: [2, 4]
description: abap2UI5 apps run stateless by default, one fresh ABAP session per roundtrip. Stateful sessions exist for the few cases that need them, with their trade-offs. The experimental offline queue keeps events in the browser while the network is away.
samples:
  - z2ui5_cl_smps_app_486
  - z2ui5_cl_smps_app_485
  - z2ui5_cl_smps_app_490
---
# Statefulness

By default, abap2UI5 runs **stateless** like any other UI5 freestyle app, with only REST calls to the ABAP backend. Each roundtrip starts a fresh ABAP session, runs the app class, serializes the app state into a server-side draft (table `Z2UI5_T_01`) whose id is all the browser keeps, and tears the work process down again. No session survives between two clicks — and that is exactly what makes the runtime scale.

For the small set of cases where a sticky backend session is needed — classic GUI-style locking, heavy session-bound resources, RFC connections that must stay open — abap2UI5 can also run in **stateful** mode.

![Stateless: three roundtrips with nothing but an app draft between them. Stateful: the same three roundtrips inside one session that stays with the user](/cookbook/expert_more/stateless-stateful.svg)

## Stateless (default)
| Phase | What happens |
|---|---|
| **Request** | Browser sends the id of the app draft and the model delta — not the app state, which never leaves the server |
| **Process** | A free work process picks it up, reads the app instance back from its app draft, runs the app class |
| **Response** | View, model and a NEW id are returned; the app state is written to a new app draft under that id and the work process is released |

No work process is pinned, no enqueue is held, no `SET/GET` parameters survive. This is the recommended default for productive apps.

## Stateful Sessions
For private and on-premise systems, you can switch a running app to stateful mode. The same work process then handles every subsequent roundtrip of *this* user, and ABAP globals, locks, and open RFC connections survive between events:

```abap
IF client->check_on_navigated( ).
  client->set_session_stateful( ).
ENDIF.
```

Release it explicitly on exit:

```abap
client->set_session_stateful( abap_false ).
```

## When to Use It
Stateful sessions are useful when:

- You need a **classic SAP-GUI-style enqueue** that survives between user interactions — see [Locks → Stateful Session](/cookbook/expert_more/lock#_5-stateful-session).
- You hold a **resource that is expensive to set up** on every roundtrip (e.g. an open RFC destination, a long-running selection, a cached internal table that cannot be re-derived cheaply).
- You are migrating a legacy dynpro flow one-to-one and want the same session semantics during the transition.

For everything else — and especially anything user-facing on a busy system — stick with the stateless default and use [optimistic checks](/cookbook/expert_more/lock#_3-optimistic-check) or [soft locks](/cookbook/expert_more/lock#_6-soft-lock) instead.

## Trade-offs
::: warning
A stateful session **pins one work process per active user** for the lifetime of the app. On a system with a handful of dialog work processes, a few dozen idle stateful sessions are enough to starve everyone else. Use it sparingly, on internal low-traffic apps. Always pair `set_session_stateful( )` with an explicit `set_session_stateful( abap_false )` on every exit path. Otherwise a leaked enqueue or a pinned work process blocks future users until the session times out.
:::

A few rules of thumb:

- **Always release on exit.** Call `set_session_stateful( abap_false )` in your back / cancel / save handlers, not just the happy path.
- **Keep sessions short.** A stateful session is a held resource, not a place to park work indefinitely.
- **Public cloud is stateless only.** S/4 Public Cloud and the BTP ABAP Environment do not support stateful sessions. The released HTTP server interface of ABAP for Cloud has no `set_session_stateful`, so the framework's HTTP wrapper does nothing there. `set_session_stateful( )` compiles and runs, and the next roundtrip still lands in a fresh session. Design for the stateless model from day one if cloud-readiness matters.

## Offline Queue

Experimental. Sometimes the request of an event cannot reach the backend: the network dropped, or a phone left the building. By default the event then ends in the error dialog with a *Retry* button. `cs_event-offline_queue` names the events of the app that should wait instead:

```abap
client->follow_up_action( val   = client->cs_event-offline_queue
                          t_arg = VALUE #( ( `Saved offline - &1 waiting` ) ( `SAVE` ) ( `COUNT` ) ) ).
```

The first argument is the toast shown when an event was kept, with `&1` the number waiting; leave it empty for no toast. The names after it are the events.

A kept event is stored in the browser's `localStorage` with its arguments and its model changes, so it survives a reload and a closed tab. Once the network is back, the queue is sent in order. Each event runs on the app draft the previous one left, and the last response is rendered. Every other event fails as before, and a call with no event name switches the queue off.

The queue has two limits: it cannot load the page without a network, and it cannot render a view the backend has not sent. The backend sees each event as if it had arrived late, in order, and decides there.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Stateful Sessions — Basics — counts up while the session is stateful, starts over once it is not | [`Z2UI5_CL_SMPS_APP_486`](https://github.com/abap2UI5/samples-stack/blob/main/src/06/z2ui5_cl_smps_app_486.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `06-stateful-locks` |
| Stateful Sessions — Locks — ENQUEUE_E_TABLE and ENQUEUE_READ, end and restart the session | [`Z2UI5_CL_SMPS_APP_485`](https://github.com/abap2UI5/samples-stack/blob/main/src/06/z2ui5_cl_smps_app_485.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `06-stateful-locks` |
| Stateful Sessions — Navigation and Locks — every Next Lock View takes the next VARKEY, going back releases it | [`Z2UI5_CL_SMPS_APP_490`](https://github.com/abap2UI5/samples-stack/blob/main/src/06/z2ui5_cl_smps_app_490.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `06-stateful-locks` |

<!-- samples:end -->
