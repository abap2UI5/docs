---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_468
  - z2ui5_cl_smp_app_480
  - z2ui5_cl_smp_app_499
---
# Routing

By default, abap2UI5 leaves the URL hash untouched — the browser's Back and Forward buttons navigate away from the abap2UI5 page instead of moving between your apps. With **hash-based app routing** (UI5 Router style), the URL hash mirrors the running app as a bookmarkable route, and the browser Back/Forward buttons drive the server-side app stack.

## Enabling Routing

Enable routing once per session with
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

The framework remembers the mode on the app and re-sends it whenever the
frontend may not still hold it (page load, a Back/Forward restore, a
navigation hop), so one call per session is enough.

Once enabled, a forward navigation to another app (`client->nav_app_call`) pushes a new route history entry — the routing equivalent of a UI5 `navTo`. The browser Back button then returns to the calling app instead of leaving the page.

## Routing Modes

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

## Navigating by Route

There is no separate route-navigation event: `client->nav_app_call( )` **is**
the navigation, and with routing enabled it pushes the same route history
entry, so Back returns to the calling app.

```abap
client->nav_app_call( NEW z2ui5_cl_new_app( ) ).
```

(The frontend-side `nav_to_route` event that used to do this was removed in
1.143.0 — see [Deprecations](/resources/deprecations).)

## Relation to Manual History Control

Routing coexists with the app-owned hash described in [URL Handling](/cookbook/browser_interaction/url_handling) — the `hash_*` family (`hash_set`, `hash_replace`, `hash_back`, `cs_event-hash_attach_changed`), named after UI5's `HashChanger`. There the APP owns the hash and reproduces a UI5 router's URLs 1:1; here the FRAMEWORK owns it and maps it to apps. For app-to-app navigation, prefer routing — reach for the app-owned hash when a single app wants its own routes (`#/detail/0`), like a ported router app. The two are mutually exclusive per app. (The obsolete spelling `cs_event-set_nav_routing` keeps compiling.)

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
