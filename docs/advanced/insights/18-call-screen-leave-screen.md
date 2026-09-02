# #18 CALL SCREEN, LEAVE SCREEN

Module pools had a call stack of screens. `CALL SCREEN 200` pushed one,
`LEAVE TO SCREEN 0` popped it, and the screen underneath came back with its
fields intact. abap2UI5 has the same stack — the elements on it are app
instances.

**Within one class** there is no stack to speak of. A flag or a step number
decides which view `view_display( )` builds, and the event handlers move it
along. Two views over the same internal table are two branches of one class,
and that is the right shape whenever the screens share their data.

**Between classes** the framework keeps the stack:

```abap
    " push: the called app takes the screen, this instance waits underneath
    client->nav_app_call( NEW zcl_app_detail( order_id = order_id ) ).
```

```abap
    " pop: back to the caller, which is called again with check_on_navigated( )
    client->nav_app_leave( ).
```

The caller gets `main( )` called again, `check_on_navigated( )` is true, and
its own attributes are still there — the instance was serialized while it
waited. What it has to do is show its view again, because the browser is still
displaying the sub-app's. And it can read what the sub-app left behind:

```abap
    WHEN client->check_on_navigated( ).
      DATA(detail) = CAST zcl_app_detail( client->get_app_prev( ) ).
      IF detail IS BOUND.
        message = detail->result.
      ENDIF.
      view_display( ).
```

`nav_app_leave( )` called *with* an app instance is `LEAVE TO TRANSACTION`:
it starts the given app without pushing the current one, so there is nothing to
return to.

**A popup is the same thing in a smaller slot.** `popup_display( )` takes a
fragment instead of a view, `popup_destroy( )` closes it, and everything in
between is ordinary binding — the popup's inputs are attributes of the same
class. For a popup that is used from several apps, make it a class of its own:
an ordinary `z2ui5_if_app` that displays into the popup slot, and ends with
`nav_app_leave( )`. The caller reads the result through `get_app_prev( )`, as
above.

What is not on the stack: the Fiori Launchpad. Navigating between *Fiori* apps
goes through the launchpad's cross-app navigation, so that the shell's history
and back button keep working — that is a `cs_event` constant rather than a
`nav_app_call( )`, and [its own page](/cookbook/event_navigation/navigation/cross_app).

**The stack came along. Its elements are instances now, not screen numbers.**

Happy ABAPing! 🦖🦕🦣
