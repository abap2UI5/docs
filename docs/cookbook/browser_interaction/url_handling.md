---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_073
  - z2ui5_cl_smp_app_316
  - z2ui5_cl_smp_app_492
description: Read the query string and the launchpad startup parameters, open a same-origin URL in a new tab, redirect, trigger a mail, SMS or phone call with the URL helper action, push a history entry, and log out.
---
# URL Handling

Working with URLs is common — reading parameters from the current URL, opening
links in new tabs, or managing browser history.

## Read URL Parameters

Read query parameters from the current URL via the config object:
```abap
DATA(lv_search) = client->get( )-s_config-search.
```

`s_config-search` is the raw query string of the page
(`?app_start=…&sap-client=…`), and `s_config-origin`, `-pathname` and `-hash`
are the other parts of the browser location. Inside the Fiori Launchpad the
startup parameters of the target mapping do not arrive in the query string: they
are parsed once per roundtrip into `client->get( )-t_comp_params`, a name/value
table, and the same table carries the `--param` entries of the
[Excel add-in's manifest](/integration/excel#the-manifest):
```abap
DATA(lv_order) = VALUE #( client->get( )-t_comp_params[ n = `SalesOrder` ]-v OPTIONAL ).
```

## Open a New Tab
Open a URL in a new browser tab via a frontend event:
```abap
DATA(lv_url) = `/sap/bc/z2ui5?app_start=z2ui5_cl_my_report`.
client->follow_up_action(
    val   = client->cs_event-open_new_tab
    t_arg = VALUE #( ( lv_url ) ) ).
```

The URL has to be on the app's own origin: the frontend refuses any other with
an error message, and so does `location_reload`. An external site goes through
`urlhelper` with `REDIRECT`, which accepts any `http` or `https` address:
```abap
client->follow_up_action(
    val   = client->cs_event-urlhelper
    t_arg = VALUE #( ( `REDIRECT` )
                     ( `{"URL":"https://www.abap2UI5.org","NEW_WINDOW":true}` ) ) ).
```

## Mail, SMS and Phone Links

`urlhelper` is `sap.m.URLHelper`, and `REDIRECT` is one of its four methods.
The other three open the device's mail client, messaging app or phone app with
the fields filled in. The second argument is always one structure whose
upper-case components name the fields — as JSON, or as the binding of a bound
structure, which works from a view wire and from a handler alike:

| Method | Fields |
|---|---|
| `TRIGGER_EMAIL` | `EMAIL`, `SUBJECT`, `BODY`, `CC`, `BCC`, `NEW_WINDOW` |
| `TRIGGER_SMS` | `TEL`, `TEXT`, `NEW_WINDOW` |
| `TRIGGER_TEL` | `TEL` |

```abap
" PUBLIC SECTION
TYPES: BEGIN OF ty_s_mail,
         email   TYPE string,
         subject TYPE string,
         body    TYPE string,
       END OF ty_s_mail.
DATA ms_mail TYPE ty_s_mail.

" in the handler
ms_mail = VALUE #( email = `service@example.com` subject = |Order { mv_order }| body = `Please check.` ).
client->follow_up_action(
    val   = client->cs_event-urlhelper
    t_arg = VALUE #( ( `TRIGGER_EMAIL` )
                     ( |${ client->_bind( ms_mail ) }| ) ) ).
```

A field carrying a line break refuses the whole call — a CR or LF in a
recipient or a subject can inject headers in some mail clients. Nothing is
sent from the server here; to send a mail from ABAP, see
[E-Mail](/cookbook/expert_more/email).

## Browser History

`client->hash_set( )` (UI5's `HashChanger#setHash`) writes a pushed entry into
the browser history from the backend, so a state inside your app becomes
bookmarkable and the Back button steps through the states you pushed:

```abap
client->hash_set( `&my-app-state=detail` ).
```

That is one of three things that can own the URL hash, and which one an app
should use is a decision of its own — see
[Navigation → Hash](/cookbook/event_navigation/navigation/hash), which also
covers framework routing, the twin `hash_replace( )` that writes *without* a
history entry, and `cs_event-hash_back` for stepping back from ABAP.

::: tip Register a listener and the value becomes the whole hash
Used on its own, as above, `hash_set( )` appends its value to the hash. An app
that registers
`cs_event-hash_attach_changed` owns the hash outright, and the same call then
writes `#/detail` the way a UI5 router does, with browser Back/Forward
round-tripping back into the running app. That is the
[app-owned routing](/cookbook/event_navigation/navigation/hash#app-owned-routing)
section.
:::

## All Three in One App

```abap
CLASS zcl_sample_url DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA mv_search TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_sample_url IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      mv_search = client->get( )-s_config-search.

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Page`
                  )->a( n = `title` v = `URL Handling`

                  )->tag( `Label`
                      )->a( n = `text` v = `the query string this app was started with:`
                  )->tag( `Text`
                      )->a( n = `text` v = client->_bind( mv_search )
                  )->tag( `Button`
                      )->a( n = `text`  v = `open the report in a new tab`
                      )->a( n = `press` v = client->follow_up_action(
                                               val   = client->cs_event-open_new_tab
                                               t_arg = VALUE #( ( `/sap/bc/z2ui5?app_start=z2ui5_cl_my_report` ) ) )
                  )->tag( `Button`
                      )->a( n = `text`  v = `push a history entry`
                      )->a( n = `press` v = client->_event( `PUSH` ) ).

      client->view_display( view->stringify( ) ).

    ELSEIF client->check_on_event( `PUSH` ).
      client->hash_set( `&my-app-state=detail` ).
      client->message_toast_display( `pushed - the browser Back button now has a step to take` ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

The *open a new tab* button carries no roundtrip at all — the call sits in the
view attribute, so the browser runs it on the press. The *push* button does,
because the backend decides what to push.

## Logout

`cs_event-system_logout` ends the sessions the app holds and navigates to the
logoff page — the launchpad's own logout inside the shell, the ICF logoff
otherwise, or a same-origin URL passed as the first argument. Which sessions
exist for a given start and what the argument replaces is on
[Logout](/configuration/logout).

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Open a URL in a New Tab (A) | [`Z2UI5_CL_SMP_APP_073`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_073.clas.abap) |
| Open Mail, Phone and SMS Links (A) | [`Z2UI5_CL_SMP_APP_316`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_316.clas.abap) |
| Reload the Page (A) | [`Z2UI5_CL_SMP_APP_492`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_492.clas.abap) |

<!-- samples:end -->
