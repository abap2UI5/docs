---
outline: [2, 4]
description: What one abap2UI5 roundtrip costs - draft, main( ), serialization, model delta - the figures the framework documents, and what an app does about it - view_display( ) once, small instances, windowed tables, events without a roundtrip, keystroke events, stateful sessions, the draft table - and how to measure with the roundtrip monitor.
---
# Performance

Almost all processing of an abap2UI5 app runs in ABAP; the browser renders
what the backend sends. So the time a user waits is mostly the time of a
[roundtrip](/get_started/glossary), and an app decides most of what one
costs. This page goes through the parts of a roundtrip, the figures the
framework documents for them, and what an app does to keep each part small.
What a roundtrip costs the system - work processes and table rows - is on
[Sizing](/configuration/sizing).

| I want to… | Section |
| --- | --- |
| know where the time goes | [One Roundtrip](#one-roundtrip) |
| stop rebuilding the view | [Call `view_display( )` Once](#call-view-display-once) |
| keep the draft small | [Keep the Instance Small](#keep-the-instance-small) |
| show a table of thousands of rows | [Large Tables](#large-tables) |
| react without the backend | [Events Without a Roundtrip](#events-without-a-roundtrip) |
| handle an event on every keystroke | [Keystroke Events](#keystroke-events) |
| measure | [Measuring It](#measuring-it) |

## One Roundtrip

A roundtrip is one `POST` that occupies one dialog work process for three
phases. The [roundtrip monitor](/configuration/monitoring#what-a-roundtrip-reports)
reports each of them:

| Phase | What happens | What makes it expensive | Monitor field |
| --- | --- | --- | --- |
| load | the request is parsed, the draft row is read and the app instance deserialized from it | a large instance: every attribute is in the draft | `ms_load` |
| main | your `main( )` runs | your code - a `SELECT` per click, a loop over a large table | `ms_main` |
| render | the view and the model are serialized, the new draft row is written | a large view sent again, a large bound model, a large instance | `ms_render` |

Between the phases the framework keeps the model small on the wire, in both
directions:

- **Browser to backend.** The browser sends only the paths the user changed
  (a delta), so read-only and untouched fields cost nothing on the way back.
- **Backend to browser.** The framework takes a copy of the bound values
  before `main( )` and compares after it. An event that changes nothing bound
  sends no model; an event that changes a bound value sends the model of the
  open view slots. From 1.149.0 on the copy is a copy of the values, not a
  digest of the serialized model, so a roundtrip no longer serializes the
  whole model before `main( )` runs, and an event without a display
  serializes it only when it is sent.
- **Compression.** The handler asks the ICF to gzip the page and the JSON
  where the browser accepts it ([Sizing](/configuration/sizing#model-size)).

The release notes document measured effects of this work:

| Release | Change | Measured |
| --- | --- | --- |
| 1.147.0 | a bound table of flat rows and a flat structure are written to JSON directly | a start roundtrip with 4000 bound rows: 1.1 s to 0.6 s |
| 1.147.0 | the developer tools are a second bundle, loaded the first time they open | the page the browser loads first is about a fifth smaller |
| 1.151.0 | the frontend is a cached, smaller bundle that loads while UI5 boots | the first view is on screen about 0.5 s earlier |
| 1.151.0 | the custom controls are a bundle of their own, loaded when a view uses one | not measured |
| 1.151.0 | a draft holds only the rows it needs and no type descriptors, and a chain is pruned on every save | not measured |

The figures are the framework's own, each from one setup. Your system has
others; the monitor below gives them.

## Call `view_display( )` Once

The largest saving an app controls is **not** rebuilding the view on every
event:

- **`client->view_display( )`** sends a new XML view and its model. UI5
  destroys the current view and creates a new one from scratch. Call it on
  initialization and navigation, and when the view's *structure* changes.
- **Change your bound attribute and return.** The framework pushes what
  changed by itself. UI5 updates the existing view through data binding and
  re-renders only the affected controls. The UI keeps its state - scroll
  position, focus, what the user typed elsewhere.

```abap
METHOD z2ui5_if_app~main.

  IF client->check_on_navigated( ).
    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Page`
                )->a( n = `title` v = `My App`

                )->tag( `Text`
                    )->a( n = `text` v = client->_bind( mv_text )
                )->tag( `Button`
                    )->a( n = `text`  v = `update`
                    )->a( n = `press` v = client->_event( `UPDATE` ) ).

    client->view_display( view->stringify( ) ).
  ELSEIF client->check_on_event( `UPDATE` ).
    " no view_display( ), and nothing else either - the changed value is
    " pushed with the response
    mv_text = `new value`.
  ENDIF.

ENDMETHOD.
```

There is no call that pushes the model: bound data travels by itself. An older
app may still call one of the model-update methods. They do nothing and can be
deleted ([Deprecations](/resources/deprecations)).

A value the view shows belongs in a bound attribute, not written into the
view. A button text built with `COND #( )` while the view is built is fixed
until the next `view_display( )`; the same text bound to an attribute changes
with the attribute.

## Keep the Instance Small

The draft is the app instance, serialized: **every attribute of the class,
whatever its visibility, bound or not**. It is read on every click and written
on every click ([Sizing](/configuration/sizing#what-a-draft-row-holds)). The
model sent to the browser holds only what is bound. So:

- **Declare public attributes for what the view shows.** `_bind( )` needs an
  attribute to be public, and each public attribute is draft state like any
  other. Data the view does not show has no reason to be public.
- **Keep large data out of the instance; making it private is not enough.**
  Select it in the event that needs it, hold a key instead of a result set,
  or bind the part the user sees and keep the rest in the database.
- **A bound table is paid twice:** in the draft row and in the model that
  crosses the wire. The Admin Cockpit flags a model over 1 MB
  ([Monitoring](/configuration/monitoring#admin-cockpit)).
- **Follow the usual ABAP rules** - no `SELECT` in a loop, a sorted or hashed
  table for reads by key - as in any other ABAP program.

## Large Tables

A table bound with `_bind( )` travels whole and is serialized into the draft
on every roundtrip. For a few hundred rows that is fine. For thousands, two
other tools exist:

- **The size limit.** A UI5 JSON model exposes 100 rows per list binding.
  Raising it with `cs_event-set_size_limit`
  ([More than 100 Rows](/cookbook/model/size_limit)) shows more, but every row
  is still sent, parsed and held by the browser.
- **A windowed table.** `client->window_bind_tab( )` binds only the rows the
  app has loaded, plus the total, and the browser asks the app for more as
  the user scrolls, sorts or filters
  ([Windowed Tables](/cookbook/model/windowed_tables)). The app reads a window
  from the database the way an OData service does, so neither the model nor
  the draft holds more than the rows the user has seen.

For a list the user has to search rather than scroll, a filtered read per
search is still the cheapest table of all: the rows that match, and no
others.

## Events Without a Roundtrip

Not every click needs the backend. Three things run in the browser alone:

- **A frontend action in the view.** `client->follow_up_action( )` as the
  value of an event attribute runs when the user clicks, and `main( )` is not
  called - closing a popup, moving the focus, copying a text, filtering a
  table bound with `_bind( )` through `cs_event-binding_call`
  ([Frontend Actions](/cookbook/event_navigation/frontend)).
- **An expression binding.** `visible`, `enabled` or a text computed from
  other bound values in the browser - a field that shows when a checkbox is
  ticked needs no event at all
  ([Expression Binding](/cookbook/model/expression_binding)).
- **A formatter.** Formatting a value for display happens in the browser
  ([Formatter](/cookbook/model/formatter)).

## Keystroke Events

`liveChange`, `liveSearch` and similar events fire on every keystroke. Each is
a roundtrip, and while one runs the busy guard drops the next. Two flags of
`_event( )` fix that
([Handling Events in ABAP](/cookbook/event_navigation/backend#the-per-wire-options-s-ctrl)):

```abap
)->tag( `SearchField`
    )->a( n = `value`      v = client->_bind( mv_search )
    )->a( n = `liveChange` v = client->_event(
                                 val    = `SEARCH`
                                 s_ctrl = VALUE #( check_queue_last = abap_true
                                                   check_no_busy    = abap_true ) ) )
```

`check_queue_last` keeps the **last** event fired while a roundtrip is in
flight and sends it once the response has landed, so the backend ends on the
field's current value and the roundtrips do not pile up. `check_no_busy` keeps
the busy overlay down while the user types. A search on `search` (Enter)
instead of `liveChange` needs neither and costs one roundtrip per search.

## Stateful Sessions

`client->set_session_stateful( )` keeps the app in one ABAP session, so what
it does not serialize - an enqueue lock, an open RFC connection - survives
between clicks. It is not a speed-up for ordinary apps: the price is one
reserved dialog work process per active user for as long as the session lives
([Sizing](/configuration/sizing#stateful-sessions)). Use it when a resource is
expensive to rebuild on every request or has to stay open, for a few internal
apps with few users, and switch it off on every exit path. On ABAP Cloud it
does not exist ([Statefulness](/cookbook/expert_more/statefulness)).

## The Draft Table

Every roundtrip of a stateless app that changed something inserts a row into
`Z2UI5_T_01`. A chain of roundtrips keeps its two newest rows, and rows older
than `draft_exp_time_in_hours` (4 by default) are deleted on every app start.
How large the table gets from that is on
[Sizing](/configuration/sizing#draft-table-growth). The size of a row is the
size of the instance, which is why the section above matters twice. The table
has no secondary index, on purpose: an index would cost time on every write. The
expiry, the cleanup and what to watch are on the
[Basis Runbook](/configuration/runbook#the-draft-table-z2ui5-t-01).

## Measuring It

Measure before changing anything. The
[roundtrip monitor](/configuration/monitoring#the-roundtrip-monitor) is the
interface `z2ui5_if_ui5_monitor`: implement it in a class of your own and
switch it on with `check_monitor_active` in the user exit. abap2UI5 then calls
it once per roundtrip with the app, the event, the three phases in
milliseconds, the sizes of the request, the response and the model, and the
browser's own measure of the previous roundtrip. *From abap2UI5 1.147.0 on.*

Read the phases against the table at the top: a long `ms_load` with a long
`ms_render` points at the instance, a long `ms_main` at your code, a large
`bytes_model` at bound data. The Admin Cockpit add-on is an implementation of
the monitor that turns this into p95 times per app and hints such as a model
that has grown too large ([Monitoring](/configuration/monitoring#admin-cockpit)).

## Still Slow?

Build a small sample that shows the problem and send it as a pull request to
the [samples repository](https://github.com/abap2UI5/samples). The
maintainers analyze it and check whether the framework can be made faster.
