---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_073
  - z2ui5_cl_smp_app_316
  - z2ui5_cl_smp_app_492
description: Read a start parameter, the query string and the launchpad startup parameters, open a URL in a new tab, redirect, start a mail or a phone call, add a history entry, and log out.
---
# URL Handling

Working with URLs is common — reading parameters from the current URL, opening
links in new tabs, or managing browser history.

## Read URL Parameters

`client->get_start_param( )` answers one start parameter by name, whichever
way the app was started:
```abap
DATA(lv_order) = client->get_start_param( `SalesOrder` ).
```

It asks three sources in order and the first that has the parameter with a
value answers: the query string of the page (`?SalesOrder=4711`), the startup
parameters a launchpad target mapping hands over, and the query of the
launchpad intent in the hash (`#SalesOrder-display?SalesOrder=4711`). The
name is matched without regard to case; the value comes back as the source
carries it, in the URL not percent-decoded, and empty when no source has the
parameter. The parameters belong to the page, so the call answers the same on
every later roundtrip and in every app the user navigates to.

The sources themselves are in the config object. `s_config-search` is the raw
query string of the page (`?app_start=…&sap-client=…`), and
`s_config-origin`, `-pathname` and `-hash` are the other parts of the browser
location. The startup parameters are parsed once per roundtrip into
`client->get( )-t_comp_params`, a name/value table, which is where a
parameter with several values has all of them. The same table carries the
`--param` entries of the
[Excel add-in's manifest](/integration/excel#the-manifest):
```abap
DATA(lv_search) = client->get( )-s_config-search.
DATA(lt_params) = client->get( )-t_comp_params.
```

## Open a New Tab
Open a URL in a new browser tab via a frontend action:
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
structure, which works in a view attribute and in a handler alike:

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

`client->hash_set( )` (UI5's `HashChanger#setHash`) writes the app's URL hash
as a pushed entry into the browser history, so a state inside your app becomes
bookmarkable and the Back button steps through the states you pushed. The app
has to own the hash first: it registers a listener with
`cs_event-hash_attach_changed`, and the value is the whole app hash:

```abap
client->follow_up_action( val   = client->cs_event-hash_attach_changed
                          t_arg = VALUE #( ( `HASH_CHANGED` ) ) ).
client->hash_set( `/detail` ).
```

Without the listener nothing is written, and the frontend says so in its log.
The listener, `hash_replace( )`, which writes *without* a history entry,
`cs_event-hash_back` for stepping back from ABAP, and the two other things
that can own the hash are all on
[URL Hash and Routing](/cookbook/event_navigation/navigation/hash#app-owned-routing).

## All Three in One App

```abap
CLASS zcl_sample_url DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA mv_search TYPE string.
    DATA mv_hash   TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_sample_url IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      mv_search = client->get( )-s_config-search.
      mv_hash   = client->get( )-s_config-hash.

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
                  )->tag( `Label`
                      )->a( n = `text` v = `the hash the browser stands on:`
                  )->tag( `Text`
                      )->a( n = `text` v = client->_bind( mv_hash )
                  )->tag( `Button`
                      )->a( n = `text`  v = `open the report in a new tab`
                      )->a( n = `press` v = client->follow_up_action(
                                               val   = client->cs_event-open_new_tab
                                               t_arg = VALUE #( ( `/sap/bc/z2ui5?app_start=z2ui5_cl_my_report` ) ) )
                  )->tag( `Button`
                      )->a( n = `text`  v = `push a history entry`
                      )->a( n = `press` v = client->_event( `PUSH` ) ).

      client->view_display( view->stringify( ) ).

      " the app owns the hash from here on; registered with every render
      client->follow_up_action( val   = client->cs_event-hash_attach_changed
                                t_arg = VALUE #( ( `HASH_CHANGED` ) ) ).

    ELSEIF client->check_on_event( `PUSH` ).
      client->hash_set( `/detail` ).
      mv_hash = `#/detail`.
      client->message_toast_display( `pushed - the browser Back button now has a step to take` ).

    ELSEIF client->check_on_event( `HASH_CHANGED` ).
      " browser Back or Forward: the hash the browser now stands on
      mv_hash = client->get( )-s_config-hash.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

The *open a new tab* button carries no roundtrip at all — the call sits in the
view attribute, so the browser runs it on the press. The *push* button does,
because the backend decides what to push. A browser Back after it fires
`HASH_CHANGED`, and the app shows the hash it is back on.

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
