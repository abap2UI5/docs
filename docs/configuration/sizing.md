---
outline: [2, 4]
description: What one abap2UI5 roundtrip costs the system - a dialog work process for its duration, one draft row - how the draft table grows, what a stateful session pins, and the model size thresholds to watch.
---
# Sizing

abap2UI5 adds no server of its own: a roundtrip is one HTTP request on the
ABAP application server, and what it costs is what any ICF request costs,
plus one row in the draft table. This page gives the two numbers that follow
from that - work processes and draft table growth - with the formula and a
worked example. The example figures are placeholders; the measurement that
replaces them is the [roundtrip monitor](/configuration/monitoring).

## One Roundtrip

Every click that reaches the backend is one `POST` to the node, and one POST
occupies one dialog work process for as long as it runs:

| Phase | What happens | Monitor field |
|---|---|---|
| load | the request is parsed, the draft row is read and the app instance deserialized from it | `ms_load` |
| main | your `main( )` runs, including a hop to another app in the same roundtrip | `ms_main` |
| render | the view and the model are serialized, the new draft row is written and committed | `ms_render` |

The work process is released when the response is sent. Nothing is kept:
no session, no enqueue, no open connection. The next click of the same user
may land on any work process of any application server, because the whole
state travels as the draft id in the request and the row in the database.
No server affinity is needed, and none is set - the only call that binds
anything is `set_session_stateful( )`, below.

An event that changes nothing bound sends no model; an event that changes a
bound value sends the model of the open view slots. The framework compares
a copy of the bound values before and after `main( )`, so an unchanged
attribute costs a compare, not a serialization.

So for dialog work processes the question is the one asked for any web
request: concurrent clicks divided by the time a click takes. The monitor
gives `ms_total` per roundtrip; the Admin Cockpit turns it into a p95 per
app ([Monitoring](/configuration/monitoring#admin-cockpit)).

## What a Draft Row Holds

The draft is the app instance, serialized as XML: **every attribute of the
class, whatever its visibility, bound or not**. A private internal table of
ten thousand rows that `main( )` filled for convenience is in every draft
row from then on, read and deserialized on every click, and written back
again. The model sent to the browser, by contrast, holds only what is bound
with `_bind( )`. Two consequences:

- Keep large data out of the instance; making it private is not enough. Select
  it in the event that needs it, hold a key instead of a result set, or bind the
  page the user sees and keep the rest in the database.
- A bound table is in both: in the draft row and in the model that crosses
  the wire. The cockpit raises a hint at a model over 1 MB.

The row is larger than the model it contains: XML carries a tag per field.
Measure one instance on your system by reading its `DATA` field in a
roundtrip of the app, or let the cockpit show the size per app.

## Draft Table Growth

This section is the one place that says how the table grows; the runbook, the
performance page and the upgrade page link here.

Every roundtrip of a stateless app that changed something inserts a row. Two
rules then take rows away again:

- **Pruning, on every save.** The roundtrips of one app in one browser tab form
  a chain, each draft naming the one before it. A save deletes the draft at
  depth `draft_keep_per_chain` + 1 of its chain, so a chain keeps that many
  drafts: 2 by default, the newest and the one a retry of a lost response
  continues. The exit field takes 2 to 50; 0 switches pruning off.
- **Expiry, on every app start.** Rows older than `draft_exp_time_in_hours`
  (4 by default) are deleted, across all users. This is what removes a chain
  nobody continues: a closed tab, a finished session.

Pruning never deletes a draft something still points to: the drafts of the app
stack (the app Back returns to, the app under a popup), the draft of an app
state link and the draft a route restore names. An app whose app state hash is
active is not pruned at all, because its URL names every draft
([App State](/cookbook/event_navigation/navigation/app_state)).

So with the defaults the table settles at the chains that were continued or
started within one expiry window, at two rows each:

```
rows   ≈ app sessions within the expiry window × draft_keep_per_chain
         + the protected drafts
bytes  ≈ rows × average row size
```

An app session is one start of one app in one tab; every navigation to another
app starts a chain of its own and protects the draft it came from. A worked
example, with placeholder figures - replace every one of them with your own:

| | Example value |
|---|---|
| users of abap2UI5 apps | 200 |
| app sessions per user within the expiry window | 5 |
| `draft_keep_per_chain` | 2 (the default) |
| average row size | 40 KB |
| rows in the table | 200 × 5 × 2 = 2,000, plus the protected drafts |
| size of the table | 2,000 × 40 KB ≈ 80 MB |

The number of roundtrips is not in that estimate any more: an app that polls
every ten seconds keeps two drafts, not one per tick. It comes back in two
cases, and then the table settles at everything written within one expiry
window, which is also the upper bound for any installation:

```
rows   ≈ users × roundtrips per user per hour × expiry in hours
```

- `draft_keep_per_chain = 0` in the exit, which keeps every draft until it
  expires - the behavior of every release before 1.151.0;
- apps with an active app state hash.

With 30 roundtrips per user per hour, the same 200 users write 200 × 30 × 4 =
24,000 rows in a four-hour window, 960 MB at 40 KB a row.

Row size is the lever the developer holds; expiry and the number of drafts
kept are the ones the exit holds
([User Exits](/advanced/extensibility/user_exits#set-config-http-post)). In
operation, watch the row count against this estimate. A count well above it
has one of three causes: the cleanup is not running (no app has been started
for a while), pruning is switched off, or an app writes much larger rows than
assumed. The cockpit's *Drafts & Housekeeping* tab shows the count and the
sizes ([Basis Runbook](/configuration/runbook#the-draft-table-z2ui5-t-01)).

The estimate ignores two things on purpose. A stateful app writes rows only on
the paths that keep a bookmarkable draft. And the cleanup is a `DELETE` across
the table at every app start, which on a busy system is many deletes of few rows
each.

## Stateful Sessions

`client->set_session_stateful( )` switches the ABAP session of this app to
stateful from that roundtrip on. The same work process then answers every later
roundtrip of this user. Enqueue locks, open RFC connections and whatever the app
does not serialize survive between clicks. The price is **one reserved dialog
work process per active user of that app** for as long as the session lives,
which is why it is meant for a few internal, low-traffic apps. The session ends
in one of three ways:

- the app calls `set_session_stateful( abap_false )`
- the frontend's session-terminate ping arrives: a `HEAD` the frontend sends
  when the app ends (a restart or the component's exit)
- the ICF session times out; the framework sets no timeout, so check the
  system's value with Basis


Pair every switch on with a switch off on every exit path. Stateful sessions do
not exist on ABAP Cloud. The trade-offs are on
[Statefulness](/cookbook/expert_more/statefulness).

## Model Size

What crosses the wire is the model of the bound attributes, and three
limits apply to it:

- **The binding size limit.** A UI5 JSON model exposes 100 items per list
  binding by default; a bound table with more rows shows the first 100 until
  the app raises the limit with `cs_event-set_size_limit` -
  [Size Limit](/cookbook/model/size_limit). Raising it to ten thousand means
  ten thousand rows rendered, so pair a large limit with a control that pages
  or grows.
- **The cockpit's hints.** The Performance tab flags a model over 1 MB, a
  large response, a slow p95 and app state that grows from roundtrip to
  roundtrip.
- **Compression.** The handler asks ICF to gzip the page and the JSON where the
  client accepts it. On a system whose ICM does not compress already, that cuts
  the transfer by 70 to 85 percent. It does not shrink what the browser has to
  parse.

What the monitor records per roundtrip - `bytes_request`, `bytes_response`,
`bytes_model`, the three phases and the browser's own measure of the
previous roundtrip - is the data a sizing is made from after go-live:
[Monitoring](/configuration/monitoring#what-a-roundtrip-reports).
