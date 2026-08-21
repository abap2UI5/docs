---
outline: [2, 4]
description: The smallest possible abap2UI5 app — one class, one method, one message.
---
# Step 1: Hello World

Every abap2UI5 app is one ABAP class implementing the interface
`z2ui5_if_app`. That interface has a single method, `main`, and the framework
calls it with one parameter: `client`, your only API. This is the smallest app
that can exist:

```abap
CLASS zcl_app_walkthrough DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS zcl_app_walkthrough IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
    client->message_box_display( `Hello World` ).
  ENDMETHOD.
ENDCLASS.
```

Press **Run this example** under the code — the class starts right here in
the browser. To run it in your own system instead, copy it in, open the
abap2UI5 startup page (see the [Quickstart](/get_started/quickstart)), and
enter the class name.

## What Just Happened

- **No app project, no OData service, no frontend artifact.** The class *is*
  the app. abap2UI5 follows a thin-frontend model: the browser only renders,
  while all logic, state and data stay in ABAP on the server.
- **`main` runs on every roundtrip.** The framework calls it when the app
  starts and again after every user interaction. Right now every call shows
  the same message box; from [Step 3](/tutorials/walkthrough/step-3) on we
  will tell the calls apart.
- **`client` is the whole API.** Displaying views and messages, reacting to
  events, binding data — everything in this tutorial goes through this one
  object.

Next, we replace the message box with a real UI5 view.
