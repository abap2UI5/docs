# #17 What the Client Can Do

The app implements one interface with one method. The one parameter of that
method is the other interface — `z2ui5_if_client` — and it is the whole of what
an app can ask the framework for. It is worth knowing by shape before knowing
it by name, because the shape is small:

| The app wants to… | …and calls |
|---|---|
| show a screen | `view_display( )`, `popup_display( )`, `popover_display( )`, `nest_view_display( )` |
| put ABAP data on that screen | `_bind( )` — the attribute, by reference, both directions |
| find out what the user did | `check_on_event( )`, `get_event( )`, `get_event_arg( )` |
| find out why `main( )` was called | `check_on_init( )`, `check_on_navigated( )` |
| talk to the user | `message_toast_display( )`, `message_box_display( )` |
| move to another app and back | `nav_app_call( )`, `nav_app_leave( )`, `get_app_prev( )` |
| ask the browser to do something | `follow_up_action( )` with a `cs_event` constant |
| know where it runs | `get( )` — device, launchpad, URL parameters, the raw event |

The two that carry the most weight are the smallest. `_bind( )` takes an ABAP
variable and returns the binding path the view needs; the framework builds the
model around whatever was bound, ships it, and writes the user's changes back
into the same variable:

```abap
    )->tag( `Input`
        )->a( n = `value` v = client->_bind( customer )
```

And `_event( )` names what the frontend should send when a control fires:

```abap
    )->tag( `Button`
        )->a( n = `text`  v = `Save`
        )->a( n = `press` v = client->_event( `SAVE` ) ).
```

Both are string generators. Nothing is registered, nothing is declared: the
attribute name and the event name are written into the view, and read back
out of the request.

The last row is the escape hatch for everything that is a browser matter rather
than an ABAP matter — focus a field, scroll, copy to the clipboard, download a
file, set the tab title, raise the model size limit. Each is a constant in
`cs_event`, and each is one call:

```abap
    client->follow_up_action( val   = client->cs_event-set_focus
                              t_arg = VALUE #( ( `inputCustomer` ) ) ).
```

That is the whole API surface an app ever touches. The full list with every
parameter is on the [Client API](/resources/api) page, generated from the
interface itself.

One method in, one interface out. Everything an app can do is a method on
`client`.

Happy ABAPing! 🦖🦕🦣
