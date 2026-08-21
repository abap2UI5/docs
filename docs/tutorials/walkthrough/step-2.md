---
outline: [2, 4]
description: Render a UI5 XML view built entirely in ABAP with the view builder.
---
# Step 2: A First View

A message box is not much of a UI. UI5 apps describe their screens as XML
views — and in abap2UI5 you write that view in ABAP, with
`z2ui5_cl_ui5_view_builder`:

```abap
CLASS zcl_app_walkthrough DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS zcl_app_walkthrough IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Shell`
                )->ele( `Page`
                    )->a( n = `title` v = `Walkthrough - Step 2`

                    )->tag( `Text`
                        )->a( n = `text` v = `Hello World` ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```

## The Four Verbs

You are writing a UI5 XML view, one control per call. The builder has four
verbs and no list of controls to look up — every UI5 control, property and
aggregation is available, because the builder never knew any of them by name:

| | |
| --- | --- |
| `ele( )` | add a control and **descend** into it — for a container |
| `tag( )` | add a control and **stay** — for a leaf |
| `a( )` | set **one** attribute on the control the chain points at |
| `end( )` | ascend to the parent |

The single rule: `a( )` applies to the control the chain currently points at,
so attributes follow their control — and a control gets them *before* its
first child. The root `mvc:View` and its `xmlns` declarations are written by
hand, exactly as in a real UI5 view. `stringify( )` renders the XML from the
root, and `view_display( )` sends it to the browser.

The indentation mirrors the XML tree — `Text` sits inside `Page` inside
`Shell` — which is what makes the chain readable as the view it builds. The
full layout rules are on the [View → Definition](/cookbook/view/definition)
page.

Want to see the XML this chain produced? Press `Ctrl+F12` in the running app
and open the view tab — the [developer tools](/get_started/hello_world#jump-into-the-code)
show the generated view, the model and the payload of every roundtrip.

Next, the app gets its first button — and with it, the app lifecycle.
