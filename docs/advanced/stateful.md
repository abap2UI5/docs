---
outline: [2, 3]
---
# Statefulness, Locks

By default, abap2UI5 runs stateless like any other UI5 freestyle app: every user interaction triggers a fresh REST call into the ABAP backend, the framework reconstructs the application state from the persisted session, and the work process is released right after.

That model is simple, scales well, and works on every supported system. Some scenarios, however, need backend state that survives between roundtrips — long-running locks, expensive setup work, or stateful APIs that don't tolerate re-initialization. abap2UI5 offers two options for this.

## Choosing an Approach

| Need                                            | Stateless (default) | Stateful Session | Infinite Transaction |
| ----------------------------------------------- | ------------------- | ---------------- | -------------------- |
| Cloud-ready (BTP, Public Cloud, Private Cloud)  | ✅                  | ❌               | ❌                   |
| Hold a SAP lock object across user interactions | ❌ (re-acquire)     | ✅               | ✅                   |
| Keep an open ABAP context (e.g., OPEN CURSOR)   | ❌                  | ✅               | ✅                   |
| Scale to many concurrent users                  | ✅                  | ⚠️ work process per session | ❌ |

In short: stay stateless if you can. Use a stateful session when you need backend state but can scope it; reach for an infinite transaction only when nothing else fits.

## Stateful Sessions

On private cloud and on-premise systems, abap2UI5 can run in stateful mode. The work process stays attached to the session, so locks acquired with `ENQUEUE_*` and other state held in memory survive across user interactions.

See `Z2UI5_CL_DEMO_APP_135` and `Z2UI5_CL_DEMO_APP_137` in the [samples repository](https://github.com/abap2UI5/samples) for end-to-end examples.

::: warning Resource usage
A stateful session pins one work process per active user. Plan capacity accordingly and make sure sessions are released — either explicitly when the user is done, or via a session timeout.
:::

## Locks

If you only need to lock business objects, stateful sessions are the most direct route: acquire the lock at the start of the session, release it when the user navigates away.

For lighter-weight cases, consider:
- **Optimistic locking** — re-read the object on save and compare a version field; reject the change if it has been modified meanwhile.
- **Short-lived locks** — acquire the lock only during the actual save, not for the duration of the edit.

## Infinite Transactions

Another option, mostly relevant for legacy stacks: create an infinite session in the backend that holds locks while the UI5 app still communicates statelessly. The session lives in a separate work process and the abap2UI5 frontend talks to it via RFC.

See the [SAP documentation on infinite sessions](https://help.sap.com/docs/ABAP_PLATFORM_NEW/6568469cf5a1460a8d85c58b83d21ec2/47db6c68e4282972e10000000a42189b.html?locale=en-US) and the discussions in [issue #2003](https://github.com/abap2UI5/abap2UI5/issues/2003) and [issue #1971](https://github.com/abap2UI5/abap2UI5/issues/1971) for context.

## See Also

- **[Inside an App → Drafts](/technical/deep_dive/lifecycle#drafts-stateful-feel-stateless-backend)** — how abap2UI5 normally simulates a stateful feel without a stateful backend.
- **[Glossary → Stateful Session](/technical/glossary#stateful-session)** — quick reference.
