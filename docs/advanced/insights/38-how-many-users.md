---
description: Sizing an abap2UI5 installation - one dialog work process per roundtrip, what a draft row holds, how z2ui5_t_01 grows and cleans itself, no server affinity, what a stateful session changes, what the roundtrip monitor measures.
---
# #38 How Many Users

How many users can an abap2UI5 installation carry? The honest answer is a
formula with the installation's own numbers in it, and this article is the
formula. Every term in it comes from one of the earlier articles.

**One roundtrip, one dialog work process.** A click is one POST
([#16](/advanced/insights/16-one-click-one-request)). A free dialog work
process picks it up, reads the draft, runs `main( )`, builds the response,
writes the new draft and is released. Nothing is pinned between two clicks, so
the work processes an installation needs are decided by *concurrent
roundtrips*, not by logged-on users: a hundred users who click every thirty
seconds and whose roundtrips take 300 ms occupy one work process on average.
The number that matters is the duration of a roundtrip, and the monitor below
measures it.

**One draft row per roundtrip.** After each request the framework serializes
the app instance into one row of `z2ui5_t_01`
([#9](/advanced/insights/09-a-new-instance-on-every-request)) — the whole
instance, every attribute of every section, whatever is bound or not, as one
string column. A roundtrip writes a new row with a new id rather than updating
the old one, so the table holds one row per click until the rows expire. The
size of a row is the size of the instance, which is why a catalog parked in
an attribute costs a `SELECT` on every click
([#23](/advanced/insights/23-100-rows-four-hours-one-request)).

The model that travels to the browser is a different, smaller thing: only the
public attributes the view binds. An attribute in the protected section is in
the draft and not in the model; a bound one is in both
([Performance](/configuration/performance)).

**How the table grows.** Rows expire after `draft_exp_time_in_hours`, four
hours by default, and the framework deletes the expired rows on every first
start of an app — one `DELETE` on the table's timestamp and a `COMMIT WORK`,
whichever user starts whichever app. So the table never holds more than the
last four hours of clicks:

```text
rows   ≈ users × clicks per user per hour × 4 h
bytes  ≈ rows × size of a serialized instance
```

Two hundred users at sixty clicks an hour are 48,000 rows, and at 20 kB per
instance a gigabyte. The same users with instances of 2 kB are a tenth of
that. The table has no secondary index by decision — the cleanup and the
counts scan it, and every click would pay for an index on every write — which
is one more reason to keep the instance small rather than the expiry long. A
lower expiry in the user exit trades a user who returns from a long lunch to
a fresh start against a smaller table.

**No server affinity.** The draft is in the database, the id is in the browser,
and nothing is in a work process between two clicks. Any application server
answers any roundtrip, so nothing in front of the servers needs sticky sessions,
a rolling restart drains nothing, and the capacity is the sum of the servers.

**What a stateful session changes.** `client->set_session_stateful( )` pins
one work process to this user for the rest of the app, so that a classic
enqueue or an open RFC connection survives between clicks
([#21](/advanced/insights/21-the-lock-is-gone-by-the-next-click)). From that
call on the formula is the GUI's: one work process per *active user*, held
while they think, released only when the app switches back with
`set_session_stateful( abap_false )` or the session times out. A few dozen
idle stateful sessions on a system with a handful of dialog work processes
starve everyone else, which is why it is for a few internal, low-traffic apps
and for nothing on public cloud
([Statefulness](/cookbook/expert_more/statefulness)). The draft is still
written on every `nav_app_call( )` and `nav_app_leave( )` and when the URL
carries the app, so the table formula above does not go to zero.

**The size limit of a binding.** A UI5 JSON model exposes at most 100 rows of
a bound table unless the app raises the limit with `cs_event-set_size_limit`
([#23](/advanced/insights/23-100-rows-four-hours-one-request)). Rows beyond
the limit are still in the model, in the draft and on the wire — the limit is
what renders, not what travels.

**What to measure instead of estimating.** From 1.147.0 the framework calls
`z2ui5_if_ui5_monitor` after every roundtrip, once an installation switches
it on in the user exit and implements it
([Monitoring](/configuration/monitoring)). Per roundtrip it reports the app,
the event, the user, three times — reading the request and the draft, the
app's `main( )`, building the response and saving the draft — the sizes of
the request, the response and the model in characters, and how long the
previous roundtrip took as the browser measured it. Those are the terms of
the formulas above, taken from the installation rather than assumed: the
roundtrip duration for the work-process count, the model size for what the
browser carries, the draft phase for what the table costs. The Admin Cockpit
add-on turns them into p95 times per app.
