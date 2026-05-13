---
outline: [2, 4]
---
# Logout

A logout button isn't a single action. Depending on how an abap2UI5 app was started, the user may have one, two, or three SAP sessions open at the same time, and each lives in a different place. abap2UI5 ships a built-in client event, `system_logout`, that terminates whichever sessions exist for the current app. See sample `Z2UI5_CL_DEMO_APP_361` for a working example.

### The Logout Event
Fire the event from any controller method to log the user off:
```abap
client->_event_client( client->cs_event-system_logout ).
```

Optionally pass a same-origin URL as the first argument to control where the user lands afterwards. The default is `/sap/public/bc/icf/logoff`:
```abap
client->_event_client(
  val   = client->cs_event-system_logout
  t_arg = VALUE #( ( `/sap/public/bsp/sap/system/logoff.htm` ) ) ).
```

### How It Works
abap2UI5 can be launched in three ways. Each one creates a different combination of SAP sessions, so the same logout event has to do different things in each case.

#### Startup Contexts
| Context | Typical URL | Fiori Launchpad shell? |
|---|---|---|
| Fiori Launchpad tile | `…/sap/bc/ui2/flp#Z2UI5-display` | yes |
| ICF HTTP handler (e.g. `Z2UI5_CL_HTTP_HANDLER`) | `…/sap/bc/<your-service>` | no |
| BSP application (`Z2UI5`, `Z2UI5_V2`) | `…/sap/bc/bsp/sap/z2ui5/index.html` | no |

#### Session Layers
Up to three independent SAP sessions can exist at once. Different cookies, different lifetimes, different ways to end them:

- **Fiori Launchpad shell** — the launchpad UI running in the browser, accessed through `sap.ushell.Container`. Only exists if the app was opened from a tile.
- **ICF / SSO session** — the authenticated AS ABAP session. Cookies `MYSAPSSO2` and `SAP_SESSIONID_*`. Killed by the standard endpoint `/sap/public/bc/icf/logoff`.
- **BSP stateful context** — the per-app server-side state that BSP applications keep alive by default. Cookie `sap-contextid`, scoped to `/sap/bc/bsp/sap/<app>/`. Visible in `SM04` as a Plugin HTTP session. Killed by appending `?sap-sessioncmd=logoff` to a URL inside the BSP app.

The sessions are independent. Ending one does not end the others, and the client can only end sessions that the current page actually loaded.

#### What `system_logout` Does
The event terminates whichever layers the current startup context owns:

| Started from | Launchpad shell | ICF / SSO | BSP context |
|---|---|---|---|
| Fiori Launchpad tile | calls `Container.logout()` | handled by the shell | — |
| ICF handler URL | — | redirect to `/sap/public/bc/icf/logoff` | — |
| BSP URL | — | redirect to `/sap/public/bc/icf/logoff` | hidden iframe to `?sap-sessioncmd=logoff` first |

### Customization
There are three ways to influence what happens on logout. Pick the one that matches your goal.

#### Custom Post-Logoff URL
Pass any same-origin URL as `t_arg`. The browser navigates there once the SAP sessions are ended (see the second example in [The Logout Event](#the-logout-event)). Useful when you want a specific landing page — for instance, the standard BSP logoff confirmation page — without changing any system settings.


::: warning
The URL must be same-origin. Cross-origin redirects are rejected.
:::

#### SICF Configuration
The endpoint `/sap/public/bc/icf/logoff` is part of the SAP system, not abap2UI5. What it shows or redirects to is configured in transaction `SICF` under **Service → Error Pages → Logoff Page**. Common patterns:

| Goal | SICF setting |
|---|---|
| Show SAP's "you have logged off" page | leave default |
| Redirect to the Fiori Launchpad | Redirect to URL = `…/sap/bc/ui2/flp` |
| Redirect to a custom company page | Redirect to URL = your page |
| Force a fresh login prompt | combine with disabling SSO for that path |

This is a Basis task. If "I logged out and came right back into the launchpad" is the symptom, this setting is almost always the cause.

#### Run Inside the Fiori Launchpad
If the goal is the cleanest user experience — going through `Container.logout()` and the FLP's own logoff flow — launch the app as an FLP tile, not from a BSP or handler URL. There is no shell to log out from on a non-FLP URL.

### Single Sign-On
On any system with SSO (SAML, Kerberos, X.509, OIDC, …), the logout event ends the SAP sessions in the browser, but the Identity Provider session stays untouched. The next request to a protected SAP URL silently re-authenticates, and the user perceives "the logout didn't work."

This is SAP NetWeaver behavior, not abap2UI5 behavior. A true "log out everywhere" needs one of the following:

- Trigger the Identity Provider's own Single Logout (SLO) endpoint
- Close the browser tab so the IdP cookies are dropped
- Build a server-side logout-everywhere flow specific to your landscape
