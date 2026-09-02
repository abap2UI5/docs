---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_498
---
# App State

A link that restores an app **exactly as it stands** — every value the user has
typed, the row they selected, the tab they opened — not just the app it was.
That is the app state, and it is the same thing a standard UI5 app does with
`sap-xapp-state`: `client->app_state_set_active( )` puts the id of the current
state in the URL, and it is advanced on every roundtrip.

```abap
METHOD z2ui5_if_app~main.

  IF client->check_on_navigated( ).
    client->app_state_set_active( ).
    view_display( ).
  ENDIF.

ENDMETHOD.
```

From then on the URL carries the id of the current state, and a reload, a
bookmark, or that URL pasted into somebody else's browser comes back to this
app with that state. `client->app_state_set_active( abap_false )` switches the
URL tracking off again.

An example URL: <br>
`.../sap/bc/z2ui5?sap-client=001&app_start=z2ui5_cl_smp_app_004#/z2ui5-xapp-state=024251849E5A1EDFB1DAE2C97C8CE8C2`

**Nothing extra is stored for this.** The draft the framework already persists
between two roundtrips — the same one
[Statefulness](/cookbook/expert_more/statefulness) describes — *is* the state
container; the app state only puts its id in the URL, where a browser can keep
it. The hash value is a server-side key pointing at that draft, so the cost is
a hash that changes, and the limit is the draft's own lifetime: once it expires
(`draft_exp_time_in_hours` in the
[User Exits](/advanced/extensibility/user_exits), four hours by default), the
link no longer restores anything.

## A Complete App

```abap
CLASS z2ui5_cl_sample_app_state DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mv_quantity TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS z2ui5_cl_sample_app_state IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).
      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Page`
                  )->tag( `Label`
                      )->a( n = `text` v = `quantity`
                  )->tag( `Input`
                      )->a( n = `value` v = client->_bind( mv_quantity )
                  )->tag( `Button`
                      )->a( n = `text`  v = `post with state`
                      )->a( n = `press` v = client->_event( `BUTTON_POST` ) ).

      client->view_display( view->stringify( ) ).

    ENDIF.

    CASE client->get( )-event.
      WHEN `BUTTON_POST`.
        client->message_toast_display( `data updated and url adjusted` ).
        client->app_state_set_active( ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

Type something, press the button, and watch the address bar: the id in the hash
advances with every roundtrip, and each one restores what was on screen when it
was written.

## Handing the Link to Somebody

`client->app_state_get_href( )` returns the absolute link to the current state,
composed in the backend. The app owns that string, so it can do anything with
it — copy it, show it in an `Input` the user can select, mail it, render it as
a QR code:

```abap
CLASS z2ui5_cl_sample_share DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mv_quantity TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS z2ui5_cl_sample_share IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    CASE abap_true.

      WHEN client->check_on_navigated( ).

        DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
            )->ele( n = `View` ns = `mvc`
                )->a( n = `xmlns`     v = `sap.m`
                )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

                )->ele( `Shell`
                    )->ele( `Page`

                        )->tag( `Label`
                            )->a( n = `text` v = `quantity`
                        )->tag( `Input`
                            )->a( n = `value` v = client->_bind( mv_quantity )
                        )->tag( `Button`
                            )->a( n = `text`  v = `share`
                            )->a( n = `press` v = client->_event( `BUTTON_POST` ) ).

        client->view_display( view->stringify( ) ).

      WHEN client->check_on_event( `BUTTON_POST` ).

        client->follow_up_action( val   = z2ui5_if_client=>cs_event-clipboard_copy
                                  t_arg = VALUE #( ( client->app_state_get_href( ) ) ) ).
        client->message_toast_display( `clipboard copied` ).

    ENDCASE.
  ENDMETHOD.
ENDCLASS.
```

The link is **launchpad-safe**: the shell hash of the page survives in it, so a
recipient opening it from a Fiori Launchpad lands in this app with this state
instead of on the launchpad home page.

::: tip It costs a roundtrip, and that is usually what you want
`app_state_get_href( )` is a backend method, so a *Share* button that calls it
is a normal event. That is the useful shape: whatever has to be saved before
the link is worth sharing is saved in the same roundtrip, and the composed link
can be shown as well as copied. The older fire-and-forget
`cs_event-clipboard_app_state` — which composed the link in the browser and
could only ever put it on the clipboard — is obsolete; see
[Deprecations](/resources/deprecations).
:::

## What the Link Restores

The recipient's browser sends the state id back with its first request, and the
framework loads the app from that draft. They see the screen the sender saw —
the same input values, the same selection — because that is what the draft
holds.

What it does **not** carry is anything the app read from outside itself. A
`SELECT` runs again on the restore, so a list is refreshed rather than frozen,
and authorizations are the recipient's own: an app-state link is a link to a
*screen*, not a copy of the data on it, and it grants nothing the recipient did
not already have.

::: warning A restored state enters through `check_on_navigated`
Opening an app-state link loads the app from its draft and runs `main( )` with
`client->check_on_navigated( )` true — `check_on_init( )` stays false, because
the instance already existed when the link was made. Display the view in that
branch, or the link opens to whatever the browser was showing before.
`check_on_navigated( )` covers the first start as well, so it is the complete
display condition on its own. Same rule as everywhere else:
[Life Cycle](/cookbook/event_navigation/life_cycle#returning-from-a-sub-app-hits-check-on-navigated-not-check-on-init).
:::

::: warning It claims the app hash
The app state writes the URL, and so do framework routing and an app that owns
its own hash — all three want the same string. Use one of them per app; see
[Hash](/cookbook/event_navigation/navigation/hash).
:::

## Bookmarking

The same URL works as a bookmark, with the same limit: the server keeps the
draft behind it for a configurable time, four hours by default. A bookmark
older than that opens the app, not the state — see the
[draft service](https://github.com/abap2UI5/abap2UI5/blob/main/src/01/01/z2ui5_cl_ui5_srv_draft.clas.abap)
source and the `draft_exp_time_in_hours`
[user exit](/advanced/extensibility/user_exits).

## App State or a Route?

They answer different questions, and the difference is what the link restores:

| | restores |
|---|---|
| [Hash](/cookbook/event_navigation/navigation/hash), routing mode `fresh` | the app, started clean |
| [Hash](/cookbook/event_navigation/navigation/hash), routing mode `keep` | the app on the stack, as the user left it |
| [Hash](/cookbook/event_navigation/navigation/hash), app-owned | the screen *inside* the app the hash names |
| **App state** | the exact roundtrip — this app, this input, this selection |

Routing is about *where the user is* and follows the app stack. The app state is
about *what is on the screen*, and is the one to reach for when a link is meant
to be handed to somebody who has to see the same thing.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalogue](https://github.com/abap2UI5/samples/blob/main/SAMPLES.md)
that use what this page describes. Each is a single class — pull the repository with
[abapGit](https://abapgit.org) and start it with `?app_start=<class>`.

| Sample | Class |
|---|---|
| App State, Bookmark and Share | [`Z2UI5_CL_SMP_APP_498`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_498.clas.abap) |

<!-- samples:end -->
