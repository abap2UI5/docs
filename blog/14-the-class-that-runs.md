# The Class That Runs

*abap2UI5 Know-How #14 — draft*

Every ABAP developer already knows the smallest program that does something:

```abap
CLASS zcl_app_adt DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
ENDCLASS.

CLASS zcl_app_adt IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    out->write( `Hello World` ).
  ENDMETHOD.
ENDCLASS.
```

One interface, one method, F9. No transaction, no program, no repository object
beside it. It runs where it was written.

That is the shape abap2UI5 copied:

```abap
CLASS zcl_app_ui5 DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS zcl_app_ui5 IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
    client->message_toast_display( `Hello World` ).
  ENDMETHOD.
ENDCLASS.
```

Same silhouette, different `out`. What the second one adds is only the
destination: it runs in a browser instead of the console, follows the Fiori
design guidelines, and can be sent to a colleague as a URL rather than as an
instruction to open ADT and press F9.

There is a property here that has quietly become valuable. The entire
application is one file. Not a class plus a service plus a binding plus a
frontend project — one artefact, read from top to bottom, with its state, its
screen and its logic in the same place.

Anything that has to reason about the app — a reviewer, a colleague inheriting
it, a search across the system, an agent asked to change something — can hold it
in full. Code that is understood in one piece is code that is changed
confidently, and there is not much of that around.

**The unit of an abap2UI5 application is a file somebody can read.**

Happy ABAPing! 🦖🦕🦣

---

## LinkedIn teaser post

Plain text — LinkedIn renders no markdown.

> The smallest ABAP program that does something is a class with
> if_oo_adt_classrun: one interface, one method, F9. No transaction, no program,
> nothing beside it.
>
> abap2UI5 copied that silhouette exactly. Same shape, different destination —
> it renders in a browser, follows the Fiori guidelines, and goes to a colleague
> as a URL instead of "open ADT and press F9".
>
> Which leaves a property that has quietly become valuable: the whole app is one
> file. Anything that has to reason about it — a reviewer, a successor, a search,
> an agent — can hold all of it at once.
>
> New article 🎉
>
> What is the smallest complete app in your system, measured in files?
>
> #ABAP #SAP #UI5
