---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_493
description: Your first abap2UI5 app - one ABAP class with a view, two-way data binding and an event, explained line by line.
---
# Hello World

This is the class from [In a Nutshell](/get_started/about), now in your own
system. Create a class named `ZCL_APP_HELLO_WORLD` and copy this into it:

```abap
CLASS zcl_app_hello_world DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_app_hello_world IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
    client->message_box_display( `Hello World` ).
  ENDMETHOD.
ENDCLASS.
```

Open the abap2UI5 startup page in your browser — the page the
[Quickstart](/get_started/quickstart#_3-first-launch) ends on — enter the class
name `ZCL_APP_HELLO_WORLD` in the input field, press **Check**, and click the
link that appears.

That is a complete abap2UI5 app: one class, one method, no frontend project and
no OData service.

## Starting an App by URL

The startup page is a convenience around a URL parameter you will use from now
on. `?app_start=<class>` appended to your service URL starts that app directly:

```
https://<host>:<port>/sap/bc/<your_service>?app_start=zcl_app_hello_world
```

That is the form a browser bookmark, a Launchpad tile and every sample
catalog use — the sample pages all say "start it with `?app_start=<class>`"
and mean exactly this.

If launching reports `Class ZCL_APP_HELLO_WORLD does not exist or does not
implement z2ui5_if_app`, the framework could not instantiate the class: a typo
in the name, the class is not activated yet, or `INTERFACES z2ui5_if_app` is
missing. Anything that goes wrong *after* this point —
an app that renders empty, a binding that does not update, an error view on a
roundtrip — is cataloged with symptom, cause and fix in
[Common Failures](/cookbook/troubleshooting/common_failures).

::: tip **Naming**
Name your own apps in your customer namespace (`Z...`/`Y...`). The `Z2UI5_`
prefix is reserved for the framework and its samples.
:::

## A Real Screen, an Event and Data Exchange

A message box is not an app. A real app has three more things: a **view** (the
screen), **data** that travels to the screen and back, and an **event** (what
the user did). This second class has all three and is still one class. You do
not need to understand every line yet: the [Tutorial](/tutorials/) builds
exactly this, one line at a time, in its first four steps.

```abap
CLASS zcl_app_hello_screen DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA name TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_app_hello_screen IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      name = `World`.

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Shell`
                  )->ele( `Page`
                      )->a( n = `title` v = `abap2UI5 - Hello World`

                      )->tag( `Input`
                          )->a( n = `value` v = client->_bind( name )
                      )->tag( `Button`
                          )->a( n = `text`  v = `Say Hello`
                          )->a( n = `press` v = client->_event( `SAY_HELLO` ) ).

      client->view_display( view->stringify( ) ).

    ELSEIF client->check_on_event( `SAY_HELLO` ).

      client->message_toast_display( |Hello { name }!| ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

Type a name, press the button, and the toast greets it. Four things are
happening, and you wrote no JavaScript and no data transfer for any of them:

- `check_on_navigated( )` is true when the screen has to be drawn: on the
  first call, and whenever the user comes back to this app
- the **view** is a UI5 XML view, built in ABAP with `z2ui5_cl_ui5_view_builder`
- `client->_bind( name )` ties the input to the public attribute, so what the
  user types is in `name` on the next roundtrip
- `client->_event( )` sends the button press back to `main`, and
  `check_on_event( )` catches it

## Jump into the Code

Press `Ctrl+F12` in any running app to open the **Developer Tools** — the
problems, every roundtrip with its request and response, the rendered view XML
and the model data of the view and of every popup. It is the first place to look
when something does not render the way you expected:

![The Developer Tools opened with Ctrl+F12 over the Hello World app: the Overview tab with app class, draft id, last event and the view slots](/get_started/image-2.webp)

## Where Next

**Next: the [Tutorial](/tutorials/).** It takes these two classes apart and
grows them into a complete app, one concept per step. Every step is a complete
class you can run in the browser.

### Later

- **Something does not work?**
  [Common Failures](/cookbook/troubleshooting/common_failures) lists symptom,
  cause and fix.
- **Read an app that already does it.** The
  [sample catalog](https://abap2ui5.github.io/samples-site/){target="_self"}
  holds hundreds of working apps with their ABAP printed in full.
- **Look a pattern up.** The [Cookbook](/cookbook/) is the reference for
  everyday work.
- **Go live.** [Go-Live Checklist](/configuration/productive_usage) is the page
  to read before the first go-live.
- **Need somebody's approval?** Send them
  [the case for abap2UI5](https://abap2ui5.github.io/why/){target="_self"}.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Basics I — Hello World, the Smallest App | [`Z2UI5_CL_SMP_APP_493`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_493.clas.abap) |

<!-- samples:end -->
