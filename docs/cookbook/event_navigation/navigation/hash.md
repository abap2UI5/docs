---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_468
  - z2ui5_cl_smp_app_480
  - z2ui5_cl_smp_app_499
---
# Hash

The URL hash is one string, and in abap2UI5 exactly one thing may own it at a
time. There are three candidates, and picking between them is most of what this
page is about:

| Owner | Turned on with | The hash then reads |
|---|---|---|
| **the framework's router** | `cs_event-hash_routing` | `#/app/<CLASS>` — one route per app on the stack |
| **your app** | `cs_event-hash_attach_changed` | whatever you write with `hash_set( )` — `#/detail`, a state of your own naming |
| **the app state** | `client->app_state_set_active( )` | the id of the current state — see [App State](/cookbook/event_navigation/navigation/app_state) |

They are not additive: each writes the same hash, so a second one turned on
takes the first one's meaning away. Decide once, per app.

::: tip The names follow UI5's
Everything on this page that touches the URL fragment is named after
`sap/ui/core/routing/HashChanger`: `hash_set( )` is `setHash`,
`hash_replace( )` is `replaceHash`, `cs_event-hash_attach_changed` is
`attachHashChanged`, and `cs_event-hash_back` is the `onNavBack` pattern.
`nav_*` keeps meaning real navigation between apps, `app_state_*` is the state
the URL carries. The older spellings — `set_push_state( )`,
`cs_event-set_nav_routing`, `set_app_state_active( )` — still compile and reach
the same code; see [Deprecations](/resources/deprecations).
:::

## Framework routing

By default, abap2UI5 leaves the URL hash untouched — the browser's Back and Forward buttons navigate away from the abap2UI5 page instead of moving between your apps. With **hash-based app routing** (UI5 Router style), the URL hash mirrors the running app as a bookmarkable route, and the browser Back/Forward buttons drive the server-side app stack.

### Enabling routing

Enable routing with
`client->follow_up_action( client->cs_event-hash_routing )` — typically in
the launcher app's `check_on_init` branch:

```abap
METHOD z2ui5_if_app~main.

  IF client->check_on_init( ).
    " an empty t_arg means cs_nav_mode-keep
    client->follow_up_action( client->cs_event-hash_routing ).
    view_display( ).
  ENDIF.

ENDMETHOD.
```

**The mode belongs to the app, like a manifest.** The framework remembers it
and re-sends it whenever the frontend may not still hold it — a page load, a
Back/Forward restore, a navigation hop — so one call per app is enough, and an
app called with `nav_app_call( )` inherits the caller's mode. What it does
*not* do is leak: a navigation hop that lands on an app which never asked for
routing turns the hash tracking off again, so leaving a routed app for a plain
one no longer leaves a stale `#/app/<CLASS>/<DRAFT>` in the address bar.

Once enabled, a forward navigation to another app (`client->nav_app_call`) pushes a new route history entry — the routing equivalent of a UI5 `navTo`. The browser Back button then returns to the calling app instead of leaving the page.

### Routing modes

The mode (see `cs_nav_mode`) decides how much of the running app the URL hash carries — and therefore what Back/Forward, a reload, or a bookmark restores:

| Mode      | Route                     | Back / Forward / reload / bookmark restore                                                                                     |
| --------- | ------------------------- | ------------------------------------------------------------------------------------------------------------------------------ |
| `default` | *(hash untouched)*        | No routing — framework behavior as before this feature.                                                                        |
| `fresh`   | `#/app/<CLASS>`           | The app starts **fresh**: a clean instance, no preserved input.                                                                |
| `keep`    | `#/app/<CLASS>/<DRAFT>`   | The exact preserved state is restored (all user input), falling back to a fresh start once the draft has expired.              |

```abap
" route by class only - Back/reload/bookmark always restart the app fresh
client->follow_up_action( val   = client->cs_event-hash_routing
                          t_arg = VALUE #( ( client->cs_nav_mode-fresh ) ) ).
```

::: tip Back restores the calling app as the user left it
In `keep` mode, the calling app's route entry is advanced to the draft saved for it during the `nav_app_call`. Pressing Back therefore restores the calling app **as the user left it** — including every bound value that changed on the client since the last render and traveled to the backend with the triggering event.
:::

::: warning Draft expiry
`keep` routes carry a server draft id. Once the draft has expired (see `draft_exp_time_in_hours` in the [User Exits](/advanced/extensibility/user_exits)), the route falls back to a fresh start of the app class.
:::

::: warning A `keep` restore enters through `check_on_navigated`, not `check_on_init`
Restoring a `keep` route (browser Back/Forward, a reload, a bookmark) loads the app from its draft and runs `main( )` with `client->check_on_navigated( )` true — `check_on_init( )` stays false, the instance already exists. As everywhere else, render the view in that branch (see [Life Cycle](/cookbook/event_navigation/life_cycle#returning-from-a-sub-app-hits-check-on-navigated-not-check-on-init)). An app that renders only on `check_on_init( )` answers without a view and the browser keeps showing the previous screen — a Forward press onto such an app appears to do nothing. This applies to **every** app reachable through routing, including detail pages that are only ever entered via `nav_app_call( )`.
:::

### Navigating by route

There is no separate route-navigation event: `client->nav_app_call( )` **is**
the navigation, and with routing enabled it pushes the same route history
entry, so Back returns to the calling app.

```abap
client->nav_app_call( NEW z2ui5_cl_new_app( ) ).
```

## App-owned routing

The router gives you one route per *app*. An app with several screens of its
own — a list and a detail, a wizard, a FlexibleColumnLayout — wants the URL to
follow **those** instead: no hash on start, `#/detail` while the user is on the
detail page, and the browser's Back button switching pages inside the *running*
instance rather than restarting anything. That is what a UI5 router does, and
`cs_event-hash_attach_changed` is how an app gets the same semantics.

Register it with the name of a backend event, in `view_display( )`:

```abap
" the registration dies with an app switch, so re-assert it on every render
client->follow_up_action( val   = client->cs_event-hash_attach_changed
                          t_arg = VALUE #( ( `HASH_CHANGED` ) ) ).
```

From then on the app owns the whole hash and the framework leaves it alone:

| Call | UI5 equivalent | What it does |
|---|---|---|
| <code>client-&gt;hash_set( `/detail` )</code> | `HashChanger#setHash`, the router's `navTo` | writes `#/detail` as a **pushed** history entry — Back has a step to take |
| <code>client-&gt;hash_replace( `/detail` )</code> | `HashChanger#replaceHash`, `navTo( …, abap_true )` | writes the same hash with **no** new entry — Back skips it |
| `cs_event-hash_back` | the `onNavBack` pattern | one real, **consumed** step back in the browser history |

A hash change the app did **not** write itself — browser Back/Forward, a manual
edit of the address bar, a deep link — fires the registered event. The hash the
browser now stands on rides along with that request, and with every other one:

```abap
CASE client->get_event( ).

  WHEN `HASH_CHANGED`.
    " the router's routeMatched: show the page the hash now names
    CASE client->get( )-s_config-hash.
      WHEN `/detail`.
        check_detail = abap_true.
      WHEN OTHERS.
        check_detail = abap_false.
    ENDCASE.

ENDCASE.
```

Because `s_config-hash` arrives on *every* request, a cold start on `#/detail`
— a reload, a bookmark, a shared link — is the same read in `view_display( )`:
the render that follows simply starts on the detail page. That is the
`routeMatched` of a cold boot, and it is why a deep link works without any
extra branch.

### Stepping back

`cs_event-hash_back` is the in-app back button, and it guards the cold deep
link the way UI5's recommended `onNavBack` does — hand it a fallback hash:

```abap
)->a( n = `navButtonPress` v = client->follow_up_action(
          val   = client->cs_event-hash_back
          t_arg = VALUE #( ( `/` ) ) ) )
```

Normally that is one real `window.history.go(-1)`, and the resulting hash
change round-trips as the registered event. But when *this page load* never
pushed a hash of its own — somebody opened `#/detail` directly — there is no
in-app step to take, and a plain Back would fall out of the app entirely. The
fallback is then written as a **replace** instead, so the user lands on the
start page rather than on whatever was in the browser before.

::: warning Without a listener, `hash_set( )` writes a suffix
`hash_set( )` predates the listener (it was called `set_push_state( )` then) and
keeps its old behavior when no listener is registered: the value is appended to
the hash rather than being the whole of it, which is what
<code>hash_set( `&amp;my-app-state=detail` )</code> in older code is doing. Registering the
listener is what makes the value the *whole* app hash — and what makes browser
Back reach your app at all.
:::

For a complete example, see sample `Z2UI5_CL_SMP_APP_499` in the table below —
it uses `hash_set`, `hash_replace`, `hash_back` and the deep-link restore in
one app.

## Which One to Pick

All three write the same hash, so the question is only who should own it:

- **routes for your apps** — framework routing. One `hash_routing` call, and
  `nav_app_call( )` / `nav_app_leave( )` become the navigation; nothing else to
  write, and Back/Forward walk the app stack.
- **screens inside one app** — `cs_event-hash_attach_changed` plus
  `hash_set( )` / `hash_replace( )` / `cs_event-hash_back`. The framework stays
  out of the way and the hash means whatever you decide it means.
- **one link that restores exactly this screen** — neither; that is
  [App State](/cookbook/event_navigation/navigation/app_state), and it claims the hash too.

Prefer routing for app-to-app navigation, and the app-owned hash for
fine-grained in-app states.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalogue](https://github.com/abap2UI5/samples/blob/main/SAMPLES.md)
that use what this page describes. Each is a single class — pull the repository with
[abapGit](https://abapgit.org) and start it with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Routing mode fresh | [`Z2UI5_CL_SMP_APP_468`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_468.clas.abap) |
| Routing mode keep | [`Z2UI5_CL_SMP_APP_480`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_480.clas.abap) |
| App-Owned Routing (#/detail) | [`Z2UI5_CL_SMP_APP_499`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_499.clas.abap) |

<!-- samples:end -->
