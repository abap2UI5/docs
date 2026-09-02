# #23 100 Rows, Four Hours, One Request

Three numbers decide whether an abap2UI5 app feels fast, and none of them is
in the app's code. They are worth knowing before the first table is bound.

**100 rows.** Every UI5 JSON model exposes at most 100 items to a list binding
by default. Bind a table with 300 entries and the first 100 render — the rest
are silently dropped, with no error anywhere. It is a UI5 setting, not a
framework one, and it is raised per view from ABAP:

```abap
    client->follow_up_action( val   = client->cs_event-set_size_limit
                              t_arg = VALUE #( ( `1000` )
                                               ( client->cs_view-main ) ) ).
```

Raising it is the fix for a table of a thousand rows. It is not the fix for a
table of fifty thousand: every bound row travels to the browser with the
response, and back to the database with the draft. A screen that needs that
many pages in ABAP — a `SELECT` with `UP TO`, a growing table, a filter bar —
the way a selection screen always did.

**Four hours.** The serialized instance from [#9](/advanced/insights/09-a-new-instance-on-every-request)
lives in `z2ui5_t_01` until it expires, four hours by default. A user who comes
back after lunch continues where they were; one who comes back tomorrow starts
fresh. The number is `draft_exp_time_in_hours` in the user exit, and the table
is the one place in the framework that grows — it is cleaned up on its own
schedule, and a system with many users and long-running screens sizes it like
any other table.

**One request.** Every click is one HTTP roundtrip plus one read and one write
of the draft. The cost of a click is therefore the size of the instance and the
size of the model, and the two are the same attributes. An app that carries a
catalogue of ten thousand rows in an attribute pays for it on every click, in
either section. An app that re-reads what it needs pays a `SELECT`, which the
database was built for.

![What a click costs: the instance to the database, the model to the browser.](/insights/23-what-a-click-costs.svg)

*What a click costs: the instance to the database, the model to the browser.*

The rule that follows is the same as in a dynpro program with global variables:
keep in the instance what the screen shows, and nothing that can be read
again.

**Small instance, bounded model, one request per click. Everything else the
system already does well.**

Happy ABAPing! 🦖🦕🦣
