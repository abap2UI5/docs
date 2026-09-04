# #9 A New Instance on Every Request

Every request lands on a fresh instance of the app class. No session holds the
previous one, and the next request may not even reach the same application
server.

So how does anything the user typed three clicks ago still exist?

`z2ui5_if_app` inherits `if_serializable_object`. After each request the
framework serializes the app instance into `z2ui5_t_01` — one generic table
for every app, not a typed draft table per data model — and reads it back on the
next one. The browser gets a draft id, and sends the draft id back. The state
itself never leaves the server.

![Every request is a new instance; z2ui5_t_01 is what carries the last one forward.](/insights/09-draft.svg)

*Every request is a new instance; z2ui5_t_01 is what carries the last one forward.*

What survives is the whole instance, whatever the visibility of an attribute.
`PUBLIC` is not what keeps a value alive — it is what `_bind( )` needs in order
to reach it, because the framework resolves a bound attribute by name from
outside the class. So the two sections have two different jobs:

```abap
  PUBLIC SECTION.
    " bound to the view - the framework reads and writes these by name
    DATA customer TYPE string.
    DATA items    TYPE ty_t_item.

  PROTECTED SECTION.
    " never bound - but serialized all the same
    DATA client   TYPE REF TO z2ui5_if_client.
    DATA t_cache  TYPE ty_t_catalog.
```

What does *not* survive is what the serializer cannot write: a local variable,
an open cursor, an acquired lock, a reference to a class that is not
serializable. The `client` reference above is set again on every call, which
is why `me->client = client` is the first line of every `main( )`.

And that single fact is the convention that catches people: a catalogue read
once and parked in an attribute is not a cached catalogue. It is a payload that
is written to the database and read back after every single click, in either
section, and it makes every click slower for as long as the app runs. The fix
is not a keyword. Anything large is re-read per request instead of carried, and
the instance stays the size of what the screen needs.

What that discipline buys is worth the rule. The app feels stateful —
PBO, PAI, cancel, back — while every request is genuinely independent. Any
server can answer any click. Nothing has to be sticky, drained before a restart,
or replicated between nodes.

Stateful for the user, stateless for the system — and the price is keeping
the instance small.

Happy ABAPing! 🦖🦕🦣
