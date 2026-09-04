# #16 One Click, One Request

The mental model that everything else in the framework rests on fits in one
sentence: **every click is one HTTP request, and every request is one call to
`main( )` on a fresh instance.**

That is PBO and PAI with the names removed. A request arrives carrying the
event and what the user changed. `main( )` runs, decides what happens, and
returns. Whatever it built travels back, the work process is released, and the
next click starts the same way. Nothing runs between two clicks, because nothing
is there to run.

So `main( )` is a dispatcher, and it dispatches on three questions the client
can answer:

```abap
  METHOD z2ui5_if_app~main.

    me->client = client.

    CASE abap_true.
      WHEN client->check_on_init( ).
        data_read( ).          " once - this instance has never run
        view_display( ).
      WHEN client->check_on_navigated( ).
        view_display( ).       " back from a sub-app or a popup: hand a view back
      WHEN client->check_on_event( `SAVE` ).
        on_save( ).            " the model changed - the view stands
    ENDCASE.

  ENDMETHOD.
```

![Three questions, in this order, and what each branch owes the browser.](/insights/16-dispatch.svg)

*Three questions, in this order, and what each branch owes the browser.*

Three things about that shape are worth knowing.

**Init before navigated.** `check_on_init( )` is true exactly once, on the very
first call. Every path to a first call raises `check_on_navigated( )` as well,
so in a `CASE abap_true` the first match wins and the init branch has to stand
first, or the one-time setup never runs.

**A return owes a view.** Coming back from a sub-app or a value help does not
fire `check_on_init( )` again. It fires `check_on_navigated( )`, and the browser
is still showing whatever the sub-app left. An app that builds its view only
under init comes back to a blank screen, and nothing reports it — the response
simply carries no view.

**An event owes nothing.** The handler changes attributes and returns. Bound
controls update from the model, focus and scroll stay where they were, and
`view_display( )` is called only when the structure of the screen changes.
Article [#8](/advanced/insights/08-only-the-changed-part) is about exactly that.

The variable that survives between the three is the instance itself,
serialized after every request — [#9](/advanced/insights/09-a-new-instance-on-every-request).
Local variables, an open cursor, a lock: gone with the work process, every time.

PBO builds, PAI decides, and the dialog step is a POST.

Happy ABAPing! 🦖🦕🦣
