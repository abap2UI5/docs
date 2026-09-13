---
outline: [2, 5]
samples:
  - z2ui5_cl_smp_app_381
  - z2ui5_cl_smp_app_382
  - z2ui5_cl_smp_app_008
  - z2ui5_cl_smp_app_452
  - z2ui5_cl_smp_app_467
  - z2ui5_cl_smp_app_474
  - z2ui5_cl_smp_app_502
  - z2ui5_cl_smp_app_512
---
# Message

Showing messages is an everyday task for ABAP developers. The functions below cover the most common cases.

## Message Toast

For brief notifications like success confirmations, use the message toast:

```abap
METHOD z2ui5_if_app~main.
  client->message_toast_display( `this is a message` ).
ENDMETHOD.
```

## Message Box

When the user needs to acknowledge the message, show a message box they must close:

```abap
METHOD z2ui5_if_app~main.
  client->message_box_display( `this is a message` ).
ENDMETHOD.
```

For error messages, change the type:

```abap
METHOD z2ui5_if_app~main.
  client->message_box_display(
    text = `This is an error message`
    type = `error` ).
ENDMETHOD.
```

## SY, BAPIRET, CX_ROOT
You can pass common message structures, objects, and variables straight to these functions:

### SY
```abap
METHOD z2ui5_if_app~main.

  MESSAGE ID `NET` TYPE `I` NUMBER `001` INTO DATA(lv_dummy).
  client->message_box_display( sy ).

ENDMETHOD.
```
### BAPIRET
```abap
METHOD z2ui5_if_app~main.

  DATA lt_bapiret TYPE STANDARD TABLE OF bapiret2.
  CALL FUNCTION `BAPI_USER_GET_DETAIL`
    EXPORTING
      username = sy-uname
    TABLES
      return   = lt_bapiret.
  client->message_box_display( lt_bapiret ).

ENDMETHOD.
```
### CX_ROOT
```abap
METHOD z2ui5_if_app~main.

  TRY.
    DATA(lv_val) = 1 / 0.
  CATCH cx_root INTO DATA(lx).
    client->message_box_display( lx ).
  ENDTRY.

ENDMETHOD.
```
The framework accepts other inputs too — pass your message structure and the message box shows it.

## Client Method or Global Object?

Both of the calls above have a second form, and the two answer different
questions.

`client->message_toast_display( )` and `client->message_box_display( )` are the
**ABAP side**. Hand them what the app already holds — a text, a BAPIRET2 table,
a log, an exception, a structure, a whole table — and they work out the
severity, the title and the rendering themselves, as the section above shows.
What they take as parameters is what an ABAP app decides: the data, the kind of
box, the buttons as a `string_table`, and the backend event raised when the box
closes.

A plain **UI5 option** — where a toast docks, how wide the box is, which icon it
carries — is not an ABAP decision, and it is set on the control instead. The
whitelisted global call reaches `sap.m.MessageToast` and `sap.m.MessageBox`
directly, and its last argument is the option object of that API 1:1:

```abap
METHOD z2ui5_if_app~main.

  " the display method IS the box type
  client->follow_up_action(
    val   = client->cs_event-control_global
    t_arg = VALUE #( ( `MESSAGE_BOX` )
                     ( `error` )
                     ( `Not saved.` )
                     ( `{"contentWidth":"30rem","icon":"WARNING"}` ) ) ).

ENDMETHOD.
```

An argument that starts with a brace travels as **real JSON**, so the options
arrive as an object rather than as a string that looks like one. Both ways end
in the same frontend code: `onClose` stays a backend event name on either path,
and the details of a box are expanded on either path.

These options left the two client methods in September 2026 and are set through
the global object now:

| Control | Options on the control |
|---|---|
| `MESSAGE_TOAST` | `width`, `my`, `at`, `of`, `offset`, `collision`, `autoClose`, `animationTimingFunction`, `animationDuration`, `closeOnBrowserNavigation`, and the abap2UI5-own `class` |
| `MESSAGE_BOX` | `icon`, `textDirection`, `closeOnNavigation`, `dependentOn`, `contentWidth` |

The global call has one more property the client method cannot have: it can be
**wired into the view**, where it runs without a round-trip at all. Extra
arguments fill the `{0}`, `{1}` placeholders of the text, so a button can say
what was pressed without asking the backend:

```abap
)->tag( `Button`
    )->a( n = `text`  v = `Save`
    )->a( n = `press` v = client->follow_up_action(
                              val   = client->cs_event-control_global
                              t_arg = VALUE #( ( `MESSAGE_TOAST` )
                                               ( `show` )
                                               ( `{0} was pressed` )
                                               ( `${$source>/text}` ) ) ) )
```

See [Frontend Actions](/cookbook/event_navigation/frontend) for the other global
objects this call reaches.

## All of Them in One App

The fragments above are each one call. Press **Run** to see the whole family in
a single app:

```abap
CLASS z2ui5_cl_sample_message DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS z2ui5_cl_sample_message IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).
      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Page`
                  )->a( n = `title` v = `Messages`

                  )->tag( `Button`
                      )->a( n = `text`  v = `toast`
                      )->a( n = `press` v = client->_event( `TOAST` )
                  )->tag( `Button`
                      )->a( n = `text`  v = `box`
                      )->a( n = `press` v = client->_event( `BOX` )
                  )->tag( `Button`
                      )->a( n = `text`  v = `from an exception`
                      )->a( n = `press` v = client->_event( `EXC` ) ).

      client->view_display( view->stringify( ) ).

    ELSEIF client->check_on_event( `TOAST` ).
      client->message_toast_display( `this is a message` ).

    ELSEIF client->check_on_event( `BOX` ).
      client->message_box_display( text = `This is an error message`
                                   type = `error` ).

    ELSEIF client->check_on_event( `EXC` ).
      TRY.
          DATA(lv_val) = 1 / 0.
        CATCH cx_root INTO DATA(lx).
          client->message_box_display( lx ).
      ENDTRY.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

## More Than One Message at a Time

The message box shows one message — but it also takes a whole set of them. Pass
a BAPI return table, a message log or the result of a validation run and the
framework flattens it into the lines the box shows; see
[Logging](/cookbook/translation_messages/logging) for the sources it reads.

For something richer than a list of lines — a sortable table with severities
and long texts — build the view from the same data, or take the ready-made
message dialog from the
[popups add-on](https://github.com/abap2UI5-addons/popups).

::: tip **Improvements**
These message functions evolve all the time. Open an issue if you hit errors or incompatibilities, or submit a PR to extend them.
:::

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/playground/samples/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Message — MessageToast via the Global Object | [`Z2UI5_CL_SMP_APP_381`](https://github.com/abap2UI5/samples/blob/main/src/00/z2ui5_cl_smp_app_381.clas.abap) |
| MessageBox, Types and Custom Actions | [`Z2UI5_CL_SMP_APP_382`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_382.clas.abap) |
| MessageBox from SY, BAPIRET2 or Exception | [`Z2UI5_CL_SMP_APP_008`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_008.clas.abap) |
| MessageView and MessagePopover (A) | [`Z2UI5_CL_SMP_APP_452`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_452.clas.abap) |
| Message Model and MessageManager (C) | [`Z2UI5_CL_SMP_APP_467`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_467.clas.abap) |
| MessagePopover URL Policy (A) | [`Z2UI5_CL_SMP_APP_474`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_474.clas.abap) |
| MessageBox for Any Data | [`Z2UI5_CL_SMP_APP_502`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_502.clas.abap) |
| MessageBox via the Global Object | [`Z2UI5_CL_SMP_APP_512`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_512.clas.abap) |

<!-- samples:end -->
