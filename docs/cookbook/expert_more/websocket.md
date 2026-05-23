---
outline: [2, 4]
---
# WebSocket

Stateless roundtrips are fine for click-driven UIs, but some scenarios need the server to *push* — a news feed, a job-status indicator, a chat. abap2UI5 has no special API for this; you use SAP's standard WebSocket stack and feed the messages into the rendered view via custom JavaScript.

The building blocks:

| Layer | Purpose |
|---|---|
| **APC** (Application Push Channel, `SAPC`) | Exposes the WebSocket endpoint over HTTP/S |
| **AMC** (Application Messaging Channel, `SAMC`) | In-system pub/sub — any ABAP code can broadcast into a channel |
| **Custom JS** in the view | Opens the socket, dispatches incoming messages |

Once both channels are configured, any `COMMIT WORK` that fires an AMC publish reaches every connected browser within milliseconds — no polling, no timer.

#### Server Side

An APC class extending `cl_apc_wsp_ext_stateless_base` binds an AMC consumer when a client connects, so AMC messages are forwarded to that socket. Broadcasting is then a one-liner from anywhere in the system:

```abap
DATA(lo_producer) = cl_amc_channel_manager=>create_message_producer(
    i_application_id = `Z2UI5_SAMPLE`
    i_channel_id     = `/news_feed` ).
lo_producer->send( i_message = `New order arrived` ).
```

A full reference implementation lives in the samples repo — `Z2UI5_CL_DEMO_APP_S_05_WS` for the APC handler and `Z2UI5_CL_DEMO_APP_S_05` for the consuming app.

#### Client Side

The browser opens the socket via [Custom JavaScript](./custom_js.md) embedded in the view. The connection stays open across normal abap2UI5 roundtrips — incoming messages can update a model, trigger a toast, or fire an abap2UI5 event to pull fresh data from the backend:

```js
const ws = new WebSocket("wss://" + window.location.host + "/sap/bc/apc/sap/z2ui5_sample");
ws.onmessage = (e) => {
  sap.m.MessageToast.show(e.data);
};
```

#### When to Reach For It

WebSockets cost a permanent connection per user — comparable to a stateful session in resource terms. Use them when push really matters:

- live dashboards, monitoring screens
- multi-user collaboration (chat, shared editing)
- long-running background jobs reporting status

For *"refresh every few seconds"* the [Timer](../browser_interaction/timer.md) is cheaper and simpler.

::: warning
APC/AMC are not available on every ABAP platform — check release notes for your system (ABAP Cloud, S/4 Public Cloud, BTP ABAP Environment) before designing around them.
:::
