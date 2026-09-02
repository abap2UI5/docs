# PUBLIC Means Persisted

Every request lands on a fresh instance of the app class. No session holds the
previous one, and the next request may not even reach the same application
server.

So how does anything the user typed three clicks ago still exist?

`z2ui5_if_app` inherits `if_serializable_object`. After each request the
framework serializes the app instance into `z2ui5_t_01` — one generic table
for every app, not a typed draft table per data model — and reads it back on the
next one. What survives is exactly the `PUBLIC SECTION`; nothing else is part of
the serialized state.

![Every request is a new instance; z2ui5_t_01 is what carries the last one forward.](/insights/09-draft.svg)

*Every request is a new instance; z2ui5_t_01 is what carries the last one forward.*

That single fact is the convention that catches people:

```abap
  PUBLIC SECTION.
    " state - serialized after every request, sent to the browser, read back
    DATA customer TYPE string.
    DATA items    TYPE ty_t_item.

  PROTECTED SECTION.
    " working data - rebuilt per request, never travels
    DATA client   TYPE REF TO z2ui5_if_client.
    DATA t_cache  TYPE ty_t_catalog.
```

A catalogue read once and parked in a public attribute is not a cached
catalogue. It is a payload that is written to the database and shipped to the
browser after every single click, and it makes every click slower for as long
as the app runs. The fix is one keyword: state stays public, working data goes
protected, and anything large is re-read per request instead of carried.

What is bought with that discipline is worth the rule. The app feels stateful —
PBO, PAI, cancel, back — while every request is genuinely independent. Any
server can answer any click. Nothing has to be sticky, drained before a restart,
or replicated between nodes.

**Stateful for the user, stateless for the system — and the price is knowing
which section an attribute belongs in.**

Happy ABAPing! 🦖🦕🦣
