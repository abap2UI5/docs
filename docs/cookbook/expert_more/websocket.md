---
outline: [2, 4]
samples:
  - z2ui5_cl_smps_app_489
description: Push from the server into an abap2UI5 app with an ABAP Push Channel - the APC and AMC setup on the server, and the WebSocket custom control that hands every message to the backend as an event.
---
# WebSocket

Stateless roundtrips are fine for click-driven UIs, but some scenarios need the server to *push* — a news feed, a job-status indicator, a chat. abap2UI5 has no special API for this; you use SAP's standard WebSocket stack, and the frontend's `z2ui5:Websocket` control turns every message the channel delivers into an abap2UI5 event — no JavaScript in the app.

The building blocks:

| Layer | Purpose |
|---|---|
| **APC** (Application Push Channel, `SAPC`) | Exposes the WebSocket endpoint over HTTP/S |
| **AMC** (Application Messaging Channel, `SAMC`) | In-system pub/sub — any ABAP code can broadcast into a channel |
| **`z2ui5:Websocket`** in the view | Opens the socket, hands every message to the backend as an event |

Once both channels are configured, any `COMMIT WORK` that fires an AMC publish reaches every connected browser within milliseconds — no polling, no timer.

## Server Side

An APC class extending `cl_apc_wsp_ext_stateless_base` binds an AMC consumer when a client connects, so AMC messages are forwarded to that socket. Broadcasting is then a one-liner from anywhere in the system:

```abap
DATA(lo_producer) = CAST if_amc_message_producer_text(
    cl_amc_channel_manager=>create_message_producer(
        i_application_id = `Z2UI5_SAMPLE`
        i_channel_id     = `/news_feed` ) ).
lo_producer->send( i_message = `New order arrived` ).
```

The APC handler class belongs to the APC application in `SAPC`; `Z2UI5_CL_SMPS_APP_489_WS` in the samples-stack repository is one, next to the consuming app listed below.

## Client Side

The frontend ships the invisible custom control `z2ui5:Websocket`. It keeps the connection to the APC endpoint open across roundtrips; every text message the channel delivers lands in the bound `value` and fires `received`, which is an ordinary backend event. Sending is deliberately not part of the control — an app publishes from ABAP through AMC, as above — so consuming a push channel needs no JavaScript at all:

```abap
CLASS zcl_sample_websocket DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mv_message  TYPE string.
    DATA mv_error    TYPE string.
    DATA mt_messages TYPE string_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_sample_websocket IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`       v = `sap.m`
              )->a( n = `xmlns:mvc`   v = `sap.ui.core.mvc`
              )->a( n = `xmlns:z2ui5` v = `z2ui5.cc`

              )->ele( `Page`
                  )->a( n = `title` v = `News Feed`

                  )->tag( n = `Websocket` ns = `z2ui5`
                      )->a( n = `path`     v = `/sap/bc/apc/sap/z2ui5_sample`
                      )->a( n = `value`    v = client->_bind( mv_message )
                      )->a( n = `received` v = client->_event( `RECEIVED` )
                      )->a( n = `error`    v = client->_event( val   = `WS_ERROR`
                                                               t_arg = VALUE #( ( `${$parameters>/code}` )
                                                                                ( `${$parameters>/message}` ) ) )
                  )->tag( `MessageStrip`
                      )->a( n = `text`    v = client->_bind( mv_error )
                      )->a( n = `type`    v = `Error`
                      )->a( n = `visible` v = |\{= ${ client->_bind_path( mv_error ) } !== '' \}|
                  )->ele( `List`
                      )->a( n = `items` v = client->_bind( mt_messages )
                      )->ele( `items`
                          )->tag( `StandardListItem`
                              )->a( n = `title` v = `{TABLE_LINE}` ).

      client->view_display( view->stringify( ) ).

    ELSEIF client->check_on_event( `RECEIVED` ).
      INSERT mv_message INTO mt_messages INDEX 1.

    ELSEIF client->check_on_event( `WS_ERROR` ).
      mv_error = |Push channel { client->get_event_arg( 1 ) }: { client->get_event_arg( 2 ) }|.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

The `RECEIVED` handler only changes bound data, so the view stays as it is and the list updates with the model — the connection is never touched by a roundtrip that leaves the view alone. A `view_display( )` builds a new control, which opens a new connection.

| Property | |
|---|---|
| `path` | the APC path, resolved against the page's origin (`/sap/bc/apc/sap/<application>`); a full `ws://` or `wss://` URL is taken as it is |
| `value` | the text of the message currently reported to the backend |
| `checkActive` | `false` closes the connection; `true` again opens it — the way to pause a feed from ABAP. Default `true` |
| `checkRepeat` | `false` closes the connection after the first message. Default `true` |

| Event | |
|---|---|
| `received` | one message arrived and `value` holds it |
| `error` | the connection could not be opened or was closed without the app asking; `code` is the WebSocket close code (`1006` for a handshake that never completed — an inactive ICF node, a rejected logon, an unknown APC application) or `CONSTRUCT` when the browser refused the URL, `message` the text |

Messages and errors are queued in the browser and handed over one per roundtrip, in the order they happened: a burst of messages while a roundtrip is in flight is not lost, and the queue holds 100 items before it drops new ones and logs it. After a connection the app did not close, the control reconnects with an increasing delay, from half a second up to thirty, and after five failed attempts in a row it stops until `path` or `checkActive` changes — each failure has fired `error`, so the backend heard about every one.

## When to Reach For It

WebSockets cost a permanent connection per user — comparable to a stateful session in resource terms. Use them when push really matters:

- live dashboards, monitoring screens
- multi-user collaboration (chat, shared editing)
- long-running background jobs reporting status

For *"refresh every few seconds"* the [Timer](/cookbook/browser_interaction/timer) is cheaper and simpler.

::: warning
ABAP Push Channels and ABAP Messaging Channels exist from ABAP Platform 7.40 SP05 on. In ABAP Cloud (BTP ABAP Environment, S/4 Public Cloud) only the released APIs of the two are available — the [samples-stack](https://github.com/abap2UI5/samples-stack) README says what its AMC/APC package needs from the system before you design around them.
:::

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Websocket — News Feed — connect, publish, list the active connections - no JavaScript | [`Z2UI5_CL_SMPS_APP_489`](https://github.com/abap2UI5/samples-stack/blob/main/src/07/z2ui5_cl_smps_app_489.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `07-amc-apc` |

<!-- samples:end -->
