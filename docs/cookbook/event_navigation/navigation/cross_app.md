---
outline: [2, 4]
---
# Cross App

Navigation *out of* your app and into another Fiori app, and back. This is the
launchpad's own navigation, not abap2UI5's: it goes through the FLP shell, so
the launchpad's history, its back button and its app-to-app contract all keep
working. [Inner App](/cookbook/event_navigation/navigation/inner_app) is the other direction — calling one of your
own abap2UI5 classes, which never leaves the page and which the launchpad
never hears about.

It only applies inside a launchpad. An abap2UI5 app served on its own has no
shell to navigate through; see [Fiori Launchpad](/configuration/launchpad) for
how an app gets there in the first place.

## Am I in a Launchpad?

`client->get( )-check_launchpad_active` answers it, which is what guards
everything on this page:

```abap
IF client->get( )-check_launchpad_active = abap_true.
  " the shell is there - cross-app navigation is available
ENDIF.
```

## Navigating to Another App

Fire `cross_app_nav_to_ext` with the target intent — a semantic object and an
action, exactly as a Fiori app would — and, optionally, parameters. Do **not**
route this through a backend roundtrip: the shell has to do the navigating, or
the launchpad's history and back button lose track of where the user is.

```abap
)->tag( `Button`
    )->a( n = `text`  v = `go to app 128`
    )->a( n = `press` v = client->follow_up_action(
              val   = client->cs_event-cross_app_nav_to_ext
              t_arg = VALUE #(
                  ( `{ semanticObject: "Z2UI5_CL_LP_SAMPLE_04", action: "display" }` )
                  ( `$` && client->_bind( nav_params ) ) ) )
```

The second argument carries the parameters. Binding a structure to it (rather
than writing the values into the string) means the parameters are read from the
model at press time, so they are whatever the user has just entered.

To go back to the launchpad app the user came from:

```abap
)->tag( `Button`
    )->a( n = `text`  v = `BACK`
    )->a( n = `press` v = client->follow_up_action( client->cs_event-cross_app_nav_to_prev_app )
```

Both are ordinary [frontend events](/cookbook/event_navigation/frontend), so
they work in the two positions every frontend event does: wired to a control as
above, or as a statement in `main` when the navigation should follow some
backend work.

## Arriving From Another App

Parameters from the target mapping — or from the start URL — arrive as
name/value pairs:

```abap
DATA(lt_params) = client->get( )-t_comp_params.
DATA(lv_product) = VALUE #( lt_params[ n = `PRODUCT` ]-v OPTIONAL ).
```

Read them in the `check_on_init( )` branch: they describe the start of the app
instance, and they do not change over its life.

## Setting the Shell Title

The launchpad shell has its own title bar, and the app can write it at any
point in its life:

```abap
client->follow_up_action(
    val   = z2ui5_if_client=>cs_event-set_title_launchpad
    t_arg = VALUE #( ( `Invoice 4711` ) ) ).
```

`set_title` writes the browser tab instead — see
[Title](/cookbook/browser_interaction/title). Inside a launchpad you usually
want both, and they are two calls.
