# #1 Somewhere on the Way to UI5, We Lost RTTS

Open SE16 and type a table name. Any table name. A screen appears with the
right columns, the right labels and the right types, and nobody wrote that
screen for that table. Nobody could have — the table was named a second ago.

ABAP developers have been building screens like that forever. With ALV you
either assemble the field catalog yourself and hand it to the iconic
`CL_GUI_ALV_GRID`, or you let `CL_SALV_TABLE` do it for you:

```abap
cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_alv)
                        CHANGING  t_table      = <lt_any> ).
lo_alv->display( ).
```

Two statements, any internal table, no type known at design time: RTTS reads
the structure at runtime, the field catalog follows, DDIC labels come along for
free.

Data browsers, table maintenance, migration cockpits, generic reports — a
whole category of tooling was built that way, and the generality was the
requirement, not a trick. Then the screens moved to the browser.

## Model Definition at Design Time — OData

Build a UI5 app today and the path from backend to frontend goes through a
typed OData service — in freestyle UI5, in Fiori Elements, in a RAP
application with a consumption view. The shape of the model is decided when
the code is written, and baked into the contract.

And that is powerful: a client can trust the API and discover everything it
needs from the metadata document, without knowing SAP at all. Whenever you do
not know the client, that is exactly what you want.

It does mean the entity type has to exist before the data does — which is
awkward when the entity type *is* the question.

## Model Definition at Runtime — RTTS

SE16 is exactly that case: the whole point is to display any table, so the
contract buys nothing. Backend and frontend are tightly coupled, one team
builds both, and anything resting on design-time metadata is aimed at a
different problem.

The model is assembled at runtime instead, and in ABAP that has always been
RTTS — a service nearly every ABAP developer already uses without necessarily
knowing the acronym (the reading half is RTTI):

![Design time: the entity type is declared before any data exists. Runtime: RTTS reads the shape from the data that is there.](/insights/01-runtime-model.svg)

*Design time: the entity type is declared before any data exists. Runtime: RTTS reads the shape from the data that is there.*

So the structure is there — and reaching a UI5 app with it is easier than it
looks, because UI5 does not require OData at all. A `JSONModel` can be filled
from any plain HTTP endpoint, so nothing stops a request from carrying a
different model shape every time.

## Data Binding at Runtime with abap2UI5

In abap2UI5 the view is a string the application builds, and the model is
bound from ABAP data — including data whose type only exists at runtime. A view
that draws any table looks like this:

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

Notice what is missing: no entity type, no CDS view, no service binding — and
not a single field name anywhere in the view. The columns are whatever the
table happens to have, and the binding paths are the component names RTTI
handed back. `comp-type` says whether a component is a DDIC type, which is
where the real labels live — the field catalog, rebuilt from its own source.

Full source:
[`Z2UI5_CL_SMP_APP_497`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_497.clas.abap),
one of the abap2UI5 samples, so it is compiled and linted on every commit.

A table nobody described, drawn from whatever the data turned out to be.
Doesn't that look a bit like `cl_salv_table` in a UI5 view? 😉 How far it goes
is the [se16n addon](https://github.com/abap2UI5-addons/se16n), a full
SE16-flavored app built on exactly this.

Nothing in either half is exotic, which is what makes it cheap to hand over:
the frontend is a freestyle UI5 app with `sap.m` controls and a `JSONModel`,
and the backend is a global ABAP class in a package, traveling in a transport.

## What It Costs

A generic table has no contract, and that is a real price. Nothing external can
depend on it, nothing announces that the underlying structure changed, and a
locally defined structure gets technical names instead of labels.

So this is not an upgrade over a typed service. One answers what a foreign
system can depend on for the next five years; the other, what an internal tool
should show right now, given a structure it was handed a millisecond ago.

## Conclusion

RTTS never went away. `cl_abap_structdescr` is still there, and it is released
in ABAP Cloud too. Only the screen in front of it went missing.

abap2UI5 gives runtime-typed ABAP a UI5 face again by binding the data
directly, so a structure that exists only at runtime stops being a special
case. It is just data — the way it was in the old SALV and field-catalog days.

So next time you reach for RTTS and need a screen, give abap2UI5 a try.
