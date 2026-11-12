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
description: Messages in abap2UI5 - message_toast_display( ) and message_box_display( ) with their parameters, the message shapes they accept (sy, BAPIRET2, a log, an exception), and the global call for plain UI5 options.
---
# Message

Two calls show a message to the user. Both are called in an event branch of
`main`, like any other reaction to an event.

| I want to… | Call |
| --- | --- |
| confirm something briefly, without a click | `client->message_toast_display( )` |
| show a message the user must close | `client->message_box_display( )` |
| show an error | ``message_box_display( text = … type = `error` )`` |
| show `sy`, a BAPIRET table or an exception | pass it as `text`, see [SY, BAPIRET, CX_ROOT](#sy-bapiret-cx-root) |
| ask a question and get the answer | `message_box_display( )` with `actions` and `onclose`, see [the parameters](#the-parameters-of-message-box-display) |

## Message Toast

For brief notifications like success confirmations, use the message toast:

```abap
client->message_toast_display( `this is a message` ).
```

## Message Box

When the user needs to acknowledge the message, show a message box they must
close:

```abap
client->message_box_display( `this is a message` ).
```

For error messages, change the type:

```abap
client->message_box_display(
  text = `This is an error message`
  type = `error` ).
```

## SY, BAPIRET, CX_ROOT
You can pass common message structures, objects, and variables straight to these
functions:

### SY
```abap
MESSAGE ID `NET` TYPE `I` NUMBER `001` INTO DATA(lv_dummy).
client->message_box_display( sy ).
```
### BAPIRET
```abap
DATA lt_bapiret TYPE STANDARD TABLE OF bapiret2.
CALL FUNCTION `BAPI_USER_GET_DETAIL`
  EXPORTING
    username = sy-uname
  TABLES
    return   = lt_bapiret.
client->message_box_display( lt_bapiret ).
```
### CX_ROOT
```abap
TRY.
  DATA(lv_val) = 1 / 0.
CATCH cx_root INTO DATA(lx).
  client->message_box_display( lx ).
ENDTRY.
```
The framework accepts other inputs too — pass your message structure and the
message box shows it.

## Client Method or Global Object?

Both of the calls above have a second form, and the two answer different
questions.

`client->message_toast_display( )` and `client->message_box_display( )` are the
**ABAP side**. Hand them what the app already holds: a text, a BAPIRET2 table,
a log, an exception, a structure, a whole table. They work out the severity,
the title and the rendering themselves, as the section above shows. Their
parameters are the things an ABAP app decides: the data, the kind of box, the
buttons as a `string_table`, and the backend event raised when the box
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

The UI5 options (where a toast docks, a box's icon, width or text direction)
are set this way and only this way. When an older app does not compile, check
the parameter list on
[Deprecations](/resources/deprecations#the-ui5-options-of-message-toast-display-message-box-display).

The global call has one more property the client method cannot have: it can be
**written into a view attribute**, where it runs without a roundtrip at all.
Extra arguments fill the `{0}`, `{1}` placeholders of the text, so a button can
say what was pressed without asking the backend:

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

## The Parameters of `message_box_display( )`

| Parameter | What it sets |
|---|---|
| `text` | what to show: a text, or a message structure, table or object |
| `type` | the kind of box, which decides icon and default title: `information` (the default), `warning`, `error`, `success`, `confirm`, `alert` or `show` |
| `title` | the text of the title bar; empty means the title of the type |
| `actions` | the buttons |
| `onclose` | the backend event raised when the box closes |
| `emphasizedaction` | which of the actions is the emphasized button |
| `initialfocus` | the action (or control id) that has the focus when the box opens |
| `details` | a further text (or JSON) shown below the message |
| `styleclass` | one or more CSS classes added to the box |

### `text`

`text` is `TYPE any`. It takes a text, a message structure or table
(`BAPIRET2`, `T100`, RAP, `symsg`, a log object, an exception), an HTML
string, a business table, a nested structure, an object or a number.
Messages are recognized first and set severity and title themselves. Several
messages become one box, typed by the most severe. One case shows nothing:
complex data that is initial, so an empty message table stays silent.

### `details`

The text is shown already unfolded, and the *Show details* link of the box is
hidden.

### `actions` and `onclose`

`actions` are the buttons, as `sap.m.MessageBox.Action` names (`OK`,
`CANCEL`, `YES`, `NO`, `ABORT`, `RETRY`, `IGNORE`, `CLOSE`, `DELETE`) or as
free texts. Without `actions`, UI5's default for the type applies: `OK` for
information and success, `CLOSE` for error, `OK` and `CANCEL` for confirm. An
`onclose` handler should therefore not expect `OK` alone.

`onclose` is the name of a **backend** event that is raised when the box
closes. The action the user pressed arrives as the first event argument,
`get_event_arg( )`, so one handler tells `DELETE` from `CANCEL`.

A confirmation is therefore one call and one branch:

```abap
ELSEIF client->check_on_event( `DELETE` ).
  client->message_box_display(
      text             = `Delete this entry?`
      type             = `confirm`
      actions          = VALUE #( ( `DELETE` ) ( `CANCEL` ) )
      emphasizedaction = `DELETE`
      onclose          = `DELETE_CONFIRMED` ).

ELSEIF client->check_on_event( `DELETE_CONFIRMED` ).
  IF client->get_event_arg( ) = `DELETE`.
    delete_entry( ).
  ENDIF.
```

`Z2UI5_CL_SMP_APP_382` opens a box of every type with the same `onclose`
and shows the answer in a toast.

### `message_toast_display( )`

Three parameters: `text`, `duration` — the milliseconds the toast stays,
UI5's default is 3000 — and `onclose`, a backend event name raised when it
has gone. Everything else about the toast is the control's and goes through
the global call above, which also composes its text on the client.

## All of Them in One App

The fragments above are each one call. Press **Run** to see the whole family in
a single app:

```abap
CLASS zcl_sample_message DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_sample_message IMPLEMENTATION.
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
These message functions evolve all the time. Open an issue if you hit errors or
incompatibilities, or submit a PR to extend them.
:::

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| MessageToast via the Global Object (A) | [`Z2UI5_CL_SMP_APP_381`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_381.clas.abap) |
| MessageBox, Types and Custom Actions | [`Z2UI5_CL_SMP_APP_382`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_382.clas.abap) |
| MessageBox from SY, BAPIRET2 or Exception | [`Z2UI5_CL_SMP_APP_008`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_008.clas.abap) |
| MessageView and MessagePopover (A) | [`Z2UI5_CL_SMP_APP_452`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_452.clas.abap) |
| Message Model and MessageManager (C) | [`Z2UI5_CL_SMP_APP_467`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_467.clas.abap) |
| MessagePopover URL Policy (A) | [`Z2UI5_CL_SMP_APP_474`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_474.clas.abap) |
| MessageBox for Any Data | [`Z2UI5_CL_SMP_APP_502`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_502.clas.abap) |
| MessageBox via the Global Object (A) | [`Z2UI5_CL_SMP_APP_512`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_512.clas.abap) |

<!-- samples:end -->
