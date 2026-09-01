---
outline: [2, 4]
---
# App State

A link that restores an app **exactly as it stands** — every value the user has
typed, the row they selected, the tab they opened — not just the app it was.
That is what the app state is: `client->set_app_state_active( )` puts the id of
the current state in the URL, and it is advanced on every roundtrip.

```abap
METHOD z2ui5_if_app~main.

  IF client->check_on_init( ).
    client->set_app_state_active( ).
    view_display( ).
  ENDIF.

ENDMETHOD.
```

From then on the URL carries the id of the current state, and a reload, a
bookmark, or that URL pasted into somebody else's browser comes back to this
app with that state. `client->set_app_state_active( abap_false )` switches the
URL tracking off again.

**Nothing extra is stored for this.** The draft the framework already persists
between two roundtrips — the same one
[Statefulness](/cookbook/expert_more/statefulness) describes — *is* the state
container; the app state only puts its id in the URL, where a browser can keep
it. So the cost is a hash that changes, and the limit is the draft's own
lifetime: once it expires (`draft_exp_time_in_hours` in the
[User Exits](/advanced/extensibility/user_exits)), the link no longer restores
anything.

## Handing the Link to Somebody

The `clipboard_app_state` frontend event puts the link to the current state on
the user's clipboard, so a *Share* button is one call and no ABAP string
handling:

```abap
)->tag( `Button`
    )->a( n = `text`  v = `share this screen`
    )->a( n = `press` v = client->follow_up_action( client->cs_event-clipboard_app_state ) )
```

Wired in the view like this it costs no roundtrip at all — the browser composes
the link from the URL it is standing on and copies it. As a statement in `main`
it does the same after your backend work, which is the form to use when
something has to be saved before the link is worth sharing.

## What the Link Restores

The recipient's browser sends the state id back with its first request, and the
framework loads the app from that draft. They see the screen the sender saw —
the same input values, the same selection — because that is what the draft
holds.

What it does **not** carry is anything the app read from outside itself. A
`SELECT` runs again on the restore, so a list is refreshed rather than frozen,
and authorizations are the recipient's own: an app-state link is a link to a
*screen*, not a copy of the data on it, and it grants nothing the recipient did
not already have.

::: warning A restored state enters through `check_on_navigated`
Opening an app-state link loads the app from its draft and runs `main( )` with
`client->check_on_navigated( )` true — `check_on_init( )` stays false, because
the instance already existed when the link was made. Display the view in that
branch, or the link opens to whatever the browser was showing before. Same rule
as everywhere else: [Life Cycle](/cookbook/event_navigation/life_cycle#returning-from-a-sub-app-hits-check-on-navigated-not-check-on-init).
:::

::: warning It claims the app hash
The app state writes the URL, and so do framework routing and a manual push
state — all three want the same string. Use one of them per app; see
[Hash](/cookbook/event_navigation/navigation/hash).
:::

## App State or a Route?

They answer different questions, and the difference is what the link restores:

| | restores |
|---|---|
| [Hash](/cookbook/event_navigation/navigation/hash), routing mode `fresh` | the app, started clean |
| [Hash](/cookbook/event_navigation/navigation/hash), routing mode `keep` | the app on the stack, as the user left it |
| **App state** | the exact roundtrip — this app, this input, this selection |

Routing is about *where the user is* and follows the app stack. The app state is
about *what is on the screen*, and is the one to reach for when a link is meant
to be handed to somebody who has to see the same thing.
