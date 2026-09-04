# #14 A Classrun for the Browser

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

  PROTECTED SECTION.
  PRIVATE SECTION.
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

There is a property here worth mentioning. A small application is one
class, and a class is one thing to read: state, screen and logic in the same
place, top to bottom. A reviewer, a colleague inheriting it, a search across the
system, an agent asked to change something — each can hold the whole thing.
Code that is understood in one piece is code that is changed confidently.

That holds for the small screens this series keeps coming back to. A larger app
splits into several classes like any other ABAP program does, and how is
[its own article](/advanced/insights/25-when-one-class-is-not-enough). The
point is where it starts: one class, one method, and it runs.

Happy ABAPing! 🦖🦕🦣
