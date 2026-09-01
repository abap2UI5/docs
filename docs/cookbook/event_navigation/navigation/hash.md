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
| **the framework's router** | `cs_event-set_nav_routing` | `#/app/<CLASS>` — one route per app on the stack |
| **your app** | `client->set_push_state( )` | whatever you push — a state of your own naming |
| **the app state** | `client->set_app_state_active( )` | the id of the current state — see [App State](/cookbook/event_navigation/navigation/app_state) |

They are not additive: each writes the same hash, so a second one turned on
takes the first one's meaning away. Decide once, per app.

## Framework routing

By default, abap2UI5 leaves the URL hash untouched — the browser's Back and Forward buttons navigate away from the abap2UI5 page instead of moving between your apps. With **hash-based app routing** (UI5 Router style), the URL hash mirrors the running app as a bookmarkable route, and the browser Back/Forward buttons drive the server-side app stack.

### Enabling routing

Enable routing once per session with
`client->follow_up_action( client->cs_event-set_nav_routing )` — typically in
the launcher app's `check_on_init` branch:

```abap
METHOD z2ui5_if_app~main.

  IF client->check_on_init( ).
    " an empty t_arg means cs_nav_mode-keep
    client->follow_up_action( client->cs_event-set_nav_routing ).
    view_display( ).
  ENDIF.

ENDMETHOD.
```

The framework remembers the mode on the app and re-sends it whenever the
frontend may not still hold it (page load, a Back/Forward restore, a
navigation hop), so one call per session is enough.

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
client->follow_up_action( val   = client->cs_event-set_nav_routing
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

## Pushing your own states

The router gives you one route per *app*. An app with several screens of its
own — a list and a detail, a wizard, a FlexibleColumnLayout — may want the URL
to follow those instead, and `set_push_state( )` is what writes it:

```abap
client->set_push_state( `&my-app-state=detail` ).
```

The value is appended to the URL hash as a **pushed** history entry, so the
browser's Back button has a step to take and the screen is bookmarkable. There
is no method that presses Back for you — the pushed states are what Back walks
through, and leaving an app is [`nav_app_leave( )`](/cookbook/event_navigation/navigation/inner_app), which returns
to the calling app rather than to the previous URL. To step back from ABAP, hand
the raw expression to `follow_up_action( )`:

```abap
client->follow_up_action( |history.back()| ).
```

That is raw JavaScript, with everything
[that implies](/cookbook/event_navigation/frontend#raw-javascript) — it is the
one place in this area where there is no built-in event yet.

For a complete example, see sample `Z2UI5_CL_SMP_APP_322`.

## Which One to Pick

All three write the same hash, so the question is only who should own it:

- **routes for your apps** — framework routing. One `set_nav_routing` call, and
  `nav_app_call( )` / `nav_app_leave( )` become the navigation; nothing else to
  write, and Back/Forward walk the app stack.
- **states inside one app** — `set_push_state( )`. The framework stays out of
  the way and the hash means whatever you decide it means.
- **one link that restores exactly this screen** — neither; that is
  [App State](/cookbook/event_navigation/navigation/app_state), and it claims the hash too.

Prefer routing for app-to-app navigation, and reach for manual push states only
for fine-grained in-app states.

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
