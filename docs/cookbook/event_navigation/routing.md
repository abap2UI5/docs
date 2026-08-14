---
outline: [2, 4]
---
# Routing

By default, abap2UI5 leaves the URL hash untouched — the browser's Back and Forward buttons navigate away from the abap2UI5 page instead of moving between your apps. With **hash-based app routing** (UI5 Router style), the URL hash mirrors the running app as a bookmarkable route, and the browser Back/Forward buttons drive the server-side app stack.

## Enabling Routing

Enable routing once per session with `client->set_nav_routing( )` — typically in the launcher app's `check_on_init` branch:

```abap
METHOD z2ui5_if_app~main.

  IF client->check_on_init( ).
    client->set_nav_routing( ).   " mode defaults to cs_nav_mode-keep
    view_display( ).
  ENDIF.

ENDMETHOD.
```

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
client->set_nav_routing( client->cs_nav_mode-fresh ).
```

::: tip Back restores the calling app as the user left it
In `keep` mode, the calling app's route entry is advanced to the draft saved for it during the `nav_app_call`. Pressing Back therefore restores the calling app **as the user left it** — including every bound value that changed on the client since the last render and traveled to the backend with the triggering event.
:::

::: warning Draft expiry
`keep` routes carry a server draft id. Once the draft has expired (see `draft_exp_time_in_hours` in the [User Exits](/advanced/extensibility/user_exits)), the route falls back to a fresh start of the app class.
:::

::: warning A `keep` restore enters through `check_on_navigated`, not `check_on_init`
Restoring a `keep` route (browser Back/Forward, a reload, a bookmark) loads the app from its draft and runs `main( )` with `client->check_on_navigated( )` true — `check_on_init( )` stays false, the instance already exists. As everywhere else, render the view in that branch (see [Life Cycle](/cookbook/event_navigation/life_cycle#returning-from-a-sub-app-hits-check_on_navigated-not-check_on_init)). An app that renders only on `check_on_init( )` answers without a view and the browser keeps showing the previous screen — a Forward press onto such an app appears to do nothing. This applies to **every** app reachable through routing, including detail pages that are only ever entered via `nav_app_call( )`.
:::

## Navigating by Route

With routing enabled, the `nav_to_route` frontend event navigates to another app by setting the hash route — without a backend roundtrip. It adds a browser history entry, so Back returns to the current app:

```abap
press = client->_event_client(
    val   = client->cs_event-nav_to_route
    t_arg = VALUE #( ( `Z2UI5_CL_NEW_APP` ) ) )
```

The argument is the target app class (or a full `app/<CLASS>` route). The event is a no-op unless the session has enabled routing.

## Relation to Manual History Control

Routing coexists with the manual history methods described in [URL Handling](/cookbook/browser_interaction/url_handling) (`set_push_state`, the `history_back` event). Those manipulate the hash yourself; routing lets the framework own the hash and map it to apps. For app-to-app navigation, prefer routing — reach for manual push states only for fine-grained in-app states.
