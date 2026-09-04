# #20 MESSAGE Still Works

An ABAP developer has thirty years of habits around messages, and most of them
carry over unchanged. What changes is where the message ends up.

**A toast for what needs no acknowledgement**, a box for what does:

```abap
    client->message_toast_display( `Saved` ).

    client->message_box_display( text = `Order is locked by another user`
                                 type = `error` ).
```

**The old structures go straight in.** `sy` after a `MESSAGE ... INTO`, a
`BAPIRET2` table from a BAPI, an exception object from a `CATCH` — the
framework reads the text, the type and the details out of each:

```abap
    MESSAGE ID `NET` TYPE `I` NUMBER `001` INTO DATA(dummy).
    client->message_box_display( sy ).

    client->message_box_display( t_bapiret ).

    TRY.
        DATA(x) = 1 / 0.
      CATCH cx_root INTO DATA(lx).
        client->message_box_display( lx ).
    ENDTRY.
```

Which means the message class, the T100 text and the translation tooling
around them are exactly as useful as before. A text element or a message class
is still the way a string gets translated, because the app is an ABAP class
and there is no i18n file beside it.

**What is different is the exception that gets away.** There is one catch in
the framework, in the HTTP handler, and an exception that escapes `main( )`
travels all the way up to it. The roundtrip ends with HTTP 500, the database
work of that roundtrip is rolled back, and the browser shows an *Application
Error, please restart* overlay with the exception chain behind a *Details*
button. There is no *continue*: the roundtrip that would have carried the
app's next state is the one that failed, so the user restarts, and the draft
brings them back to where they were.

That is the fallback, and reaching it means the user has lost the screen.
Anything predictable — a failed conversion, a locked object, a service that is
not there — is caught where it happens and shown as a box, which costs the
user one click.

One setting belongs in every production system. The 500 body carries source
positions, RTTI names and system context — everything a developer wants and
more than a browser should get. `check_hide_error_details` in the
[user exit](/advanced/extensibility/user_exits) turns the body into a bare
*Internal Server Error* and leaves everything else as it is.

The message classes, the texts and the translation stay. Only the dump
looks different.

Happy ABAPing! 🦖🦕🦣
