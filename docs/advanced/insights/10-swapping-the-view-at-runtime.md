# Swapping the View at Runtime

The view is a string the app produced for this request. Which raises an
uncomfortable question: what stops the next request from producing a different
one?

Nothing does.

```abap
  METHOD z2ui5_if_app~main.

    me->client = client.
    IF client->get_event( ) = `TOGGLE`.
      as_list = xsdbool( as_list = abap_false ).
    ENDIF.
    set_view( ).

  ENDMETHOD.

  METHOD set_view.

    DATA(page) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Page`
                )->a( n = `title` v = `Products`

                )->tag( `Switch`
                    )->a( n = `state`  v = client->_bind( as_list )
                    )->a( n = `change` v = client->_event( `TOGGLE` ) ).

    IF as_list = abap_true.
      render_list( page ).
    ELSE.
      render_table( page ).
    ENDIF.

    client->view_display( page->stringify( ) ).

  ENDMETHOD.
```

![Same data, same class — the control is chosen in an IF.](/insights/10-view-swap.svg)

*Same data, same class — the control is chosen in an IF.*

Same data, same class, same request handler. One click and the table is a list —
not a table with its columns hidden, and not a second app behind a navigation
step. A different control, chosen in ABAP, in an `IF`.

This is ordinary in the way it is written and unusual in what it implies. A
screen assembled at design time can vary only where somebody anticipated
variation and left a switch. A screen assembled per request varies wherever the
code branches, which is everywhere.

It is the same freedom the first article in this series claimed for the model — a structure
described at runtime rather than declared up front — arriving now on the view
side. The two together are the whole argument: if both the shape of the data
and the shape of the screen are decided while the request is running, then the
things a screen can adapt to are no longer fixed on the day it was designed.

**A view that is built is a view that can be built differently.**

Happy ABAPing! 🦖🦕🦣
