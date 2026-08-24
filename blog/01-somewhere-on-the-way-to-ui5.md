# Somewhere on the Way to UI5, We Lost RTTI

*abap2UI5 Know-How #1 — scheduled for Tuesday, 25 August 2026*

![The dinosaur at a classic ALV grid, the sheep and the sloth at the same table
rendered in UI5, with cl_abap_structdescr->get_components( ) bridging the
two](assets/01-rtti-header.png)

ABAP developers have always built screens for tables whose structure is unknown
when the code is written. With ALV you either assemble the field catalog
yourself and hand it to `REUSE_ALV_GRID_DISPLAY`, or you let `CL_SALV_TABLE` do
it for you:

```abap
cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_alv)
                        CHANGING  t_table      = <lt_any> ).
lo_alv->display( ).
```

Two statements, any internal table, no type known at design time. RTTI reads
the structure at runtime, the field catalog follows from it, DDIC labels come
along for free.

Data browsers, table maintenance, migration cockpits, generic reports — a whole
category of tooling was built that way, and the generality was the requirement,
not a trick.

## Model definition at design time — OData

When you build a UI5 app today, the path from backend to frontend goes through
a typed OData service. That holds for freestyle UI5, for Fiori Elements, and
for a fully backend-driven RAP application with a consumption view. The shape
of the model is decided when the code is written and baked into the contract.

And that is powerful. The point of OData is that a client can trust the API and
discover everything it needs from the metadata document
([odata.org](https://www.odata.org)) — it does not have to know SAP at all.
Whenever you do not know the client, or the client does not know your backend,
that contract is exactly what you want.

## Model definition at runtime — RTTI

But some use cases do not know the model at design time either. Think of SE16
or SE16N: the whole point is to display any table.

There the contract buys nothing. Backend and frontend are tightly coupled, one
team builds both, and the client knows the system intimately — anything resting
on design-time metadata is aimed at a different problem.

What this needs is a model assembled at runtime, and a freestyle UI5 app does
not actually require OData to get one: a `JSONModel` can be filled from any HTTP
endpoint, so nothing stops a request from carrying a different model shape every
time.

That is one of the use cases abap2UI5 is built for.

## Binding at runtime with abap2UI5

An abap2UI5 view is a string the application builds, and the model is bound
from ABAP data — including data whose type only exists at runtime:

```abap
" tab is TYPE STANDARD TABLE - filled however you like, a SELECT, a function
" module, an EML read. Only its type matters from here on.
METHOD render_any.

  DATA(comps) = CAST cl_abap_structdescr(
                    CAST cl_abap_tabledescr(
                        cl_abap_typedescr=>describe_by_data( tab )
                      )->get_table_line_type( ) )->get_components( ).

  DATA(ui_table) = parent->ele( `Table`
                       )->a( n = `items` v = client->_bind( tab ) ).

  " one column per component - discovered, not declared
  DATA(columns) = ui_table->ele( `columns` ).
  LOOP AT comps INTO DATA(comp).
    columns->ele( `Column`
        )->ele( `header`
            )->tag( `Text`
                )->a( n = `text` v = comp-name ).
  ENDLOOP.

  " one cell per component, bound by field name
  DATA(cells) = ui_table->ele( `items`
      )->ele( `ColumnListItem`
          )->ele( `cells` ).
  LOOP AT comps INTO comp.
    cells->tag( `Text`
        )->a( n = `text` v = |\{{ comp-name }\}| ).
  ENDLOOP.

ENDMETHOD.
```

RTTI, precisely — the read half of RTTS. The write half, RTTC
(`cl_abap_structdescr=>create( )`), never appears: nothing here builds a type,
the code only asks what a type already is.

No entity type, no CDS view, no service binding — and no field name anywhere
in the view. The columns are whatever the table happens to have, the binding
paths are the component names RTTI just handed back, and `comp-type` will tell
you whether a component is a DDIC type, which is where the real labels come
from. The field catalog, rebuilt from the same source it always came from.

The signature carries as much of the point as the body. `TYPE STANDARD TABLE`
is the shape `cl_salv_table=>factory( )` has taken since forever — and it is
the reason the caller stays ordinary. Fill the table with a plain typed
`SELECT`, a function module, an EML read on a RAP business object; the
renderer is handed an internal table and asks it what it is.

Worth being precise about, because the demo everyone reaches for first is
SE16N — a DDIC name from an input field, `CREATE DATA` on it, a dynamic
`SELECT`. It shows well and it is a poor thing to copy: an app that reads
whatever table it is handed. The genericity worth having is in the view, not
in the data access. Whole class:
[`Z2UI5_CL_SMP_APP_497`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_497.clas.abap),
one of the abap2UI5 samples, so it is compiled and linted on every commit. If
you do want the browser, take the
[se16n addon](https://github.com/abap2UI5-addons/se16n).

## Nothing here is exotic

Both halves stay inside the conventions you already work in, which is what
makes this cheap to adopt and cheap to hand over.

The frontend is a freestyle UI5 app using `sap.m` controls, XML views, a
`JSONModel` and two-way binding. The backend is a global ABAP class: one
interface, in a package, travelling in a transport.

## What it costs

A generic table has no contract, and that is a real price. Nothing external can
depend on it, nothing announces that the underlying structure changed, and a
locally defined structure gets technical names instead of labels.

So this is not an upgrade over a typed service. One answers what a foreign
system can depend on for five years; the other, what an internal tool should
put on screen without being told twice.

## What actually changed

None of the runtime machinery ever went away. `cl_abap_structdescr` is still
there, still released in ABAP Cloud. Only the screen in front of it did.

abap2UI5 gives runtime-typed ABAP a UI5 face again by binding ABAP data
directly, so data typed at runtime is not a special case. It is just data.

It is open source, runs on-premise and in the cloud, and sits beside what you
already operate rather than in place of it. Put an abap2UI5 app into the
launchpad and no user can tell it from the RAP and freestyle UI5 tiles next to
it.

So next time you reach for RTTI, give abap2UI5 a try. It may be a good
complement to the UI5 solutions you already run.

Happy ABAPing! 🦖🦕🦣

---

## LinkedIn teaser post

> Before Fiori, screens were routinely built for tables nobody had seen.
> `cl_salv_table=>factory( )` took any internal table and drew it — RTTI
> answered what the columns were, the DDIC supplied the labels, and a whole
> category of ABAP tooling was built that way.
>
> Then the path to a screen started going through a typed service, and the
> entity type had to exist first. Exactly right when the client does not know
> your backend; beside the point when not knowing the structure is what the
> tool is for.
>
> A new article on building UI5 views from RTTI at runtime — and on how it sits
> next to the RAP and OData services a system already runs, rather than in
> place of them: [link]
>
> Where do you still use RTTI-driven tooling today?
>
> #ABAP #SAP #UI5
