---
outline: [2, 4]
---
# abap2UI5 — Logoff guide for Launchpad, ICF handler, and BSP

> **TL;DR** &nbsp; The `SYSTEM_LOGOUT` client event behaves differently depending on **how the app was started**, because SAP exposes three independent session layers (FLP shell, ICF/SSO, BSP context) and no single browser-side action terminates all three. This guide explains each layer, what `SYSTEM_LOGOUT` does in each startup context, and which options you have to customize the behavior.

See sample `Z2UI5_CL_DEMO_APP_361` for a working example that triggers the `system_logout` client event.

---

## 1. The three startup contexts

abap2UI5 can be launched in three ways. The JavaScript that runs on the client is the **same** in all three — the difference is which session layers exist around it.

| Context | Typical URL | Server runtime | FLP shell loaded? |
|---|---|---|---|
| **Fiori Launchpad tile** | `…/sap/bc/ui2/flp#Z2UI5-display` | ICF + UI2 services | ✅ yes |
| **ICF HTTP handler** (e.g. `Z2UI5_CL_HTTP_HANDLER`, `Z2UI5_CL_LP_HANDLER`) | `…/sap/bc/<your-service>` | Plain ICF | ❌ no |
| **BSP application** (`Z2UI5`, `Z2UI5_V2`) | `…/sap/bc/bsp/sap/z2ui5/index.html` | BSP runtime, **stateful** by default | ❌ no |

---

## 2. The three SAP session layers

Different cookies, different lifetimes, different ways to terminate.

| Layer | What it represents | Cookie / identifier | Lifetime / scope |
|---|---|---|---|
| **A. FLP shell session** | The Fiori Launchpad UI runtime in the browser | In-memory `sap.ushell.Container` state, plus underlying ICF session | Tab-bound, only exists if FLP was loaded |
| **B. ICF / SSO logon** | AS ABAP authenticated session | `MYSAPSSO2`, `SAP_SESSIONID_<sid>_<client>` | Path-scoped to `/sap/`, killed by `/sap/public/bc/icf/logoff` |
| **C. BSP stateful context** | BSP runtime app state on the application server (visible in `SM04` as Plugin HTTP) | `sap-contextid` scoped to `/sap/bc/bsp/sap/<app>/` | Per-app, killed by `?sap-sessioncmd=logoff` on a URL inside that BSP app |

**Crucial:** these are independent. Killing one does **not** kill the others. And in any **SSO / IdP** setup (SAML, Kerberos, X.509, custom OIDC) the next request to a protected SAP URL silently re-authenticates regardless — see §6.

---

## 3. What `SYSTEM_LOGOUT` actually does

The button on the demo `Z2UI5_CL_DEMO_APP_361` triggers the client event `cs_event-system_logout`. The JS handler is identical across all three runtimes; pseudocode:

```
SYSTEM_LOGOUT(args):
  logoutUrl = args[1] || '/sap/public/bc/icf/logoff'

  try:
    if z2ui5.oLaunchpad?.Container?.logout:        # only inside FLP shell
        z2ui5.oLaunchpad.Container.logout()        # terminates layer A
    else:
        redirectToLogoff()                         # fallback path
  catch: redirectToLogoff()


redirectToLogoff():
  # NEW (since the BSP fix): also handle layer C when running inside BSP
  if window.location.pathname.startsWith('/sap/bc/bsp/'):
      # Hit the BSP path with sap-sessioncmd=logoff via a hidden iframe.
      # BSP runtime calls server->session->terminate( ).
      <fire iframe to "<bsp-path>?sap-sessioncmd=logoff">
      <on iframe load, OR after 1.5s safety timeout:>
          window.location.href = logoutUrl        # then go drop layer B
  else:
      window.location.href = logoutUrl            # ICF/handler stays as before
```

### What this terminates per startup context

| Context | A – FLP shell | B – ICF/SSO | C – BSP context |
|---|---|---|---|
| Fiori Launchpad tile | ✅ via `Container.logout()` | depends on FLP shell wiring | n/a |
| ICF HTTP handler | n/a | ✅ via `/sap/public/bc/icf/logoff` | n/a |
| BSP application | n/a | ✅ via `/sap/public/bc/icf/logoff` | ✅ via `?sap-sessioncmd=logoff` *(new)* |

**This is the whole feature** — there is no client-side mechanism that can terminate a layer that wasn't loaded into the page. A BSP page cannot log out the FLP shell, because no FLP shell is mounted on it. The same constraint holds in reverse.

---

## 4. Why the BSP fix was needed

`/sap/public/bc/icf/logoff` invalidates layer B only. The BSP runtime keeps its **layer C** stateful context alive on the server (you'd see it in `SM04` as a Plugin HTTP session) and the cookie `sap-contextid` stays scoped to `/sap/bc/bsp/sap/<app>/`. Returning to the BSP URL (or any cached state) re-attaches to that context.

The canonical SAP-standard mechanism for terminating a BSP context is `?sap-sessioncmd=logoff` on a request inside the BSP namespace. SAP itself uses exactly this pattern in `CL_BSP_LOGIN_APPLICATION`, `CL_BSP_LOGOFF_APPL_TAG`, the CRM IC framework, and several `*.wapa.leave.htm` BSP pages — typically delivered via a hidden 1×1 element so the redirect happens silently.

The patched JS does the same: a hidden iframe → `?sap-sessioncmd=logoff` → on load, redirect to `logoutUrl`. The non-BSP branch is unchanged.

---

## 5. Where this lives in the code

When you customize, edit only one of these — pick the one that matches your runtime.

| Object | Type / Package | Role |
|---|---|---|
| `Z2UI5_CL_APP_VIEW1_JS` | CLAS / `$ZLK_00_01_03` | Generates the JS for the **ICF handler** runtime |
| `Z2UI5` (`controller/View1.controller.js`) | WAPA / `$ZLK_01_02` | JS served from MIME repository for **BSP V1** |
| `Z2UI5_V2` (`cc/Actions.js`) | WAPA / `$ZLK_02` | JS served from MIME repository for **BSP V2** |
| `Z2UI5_CL_DEMO_APP_361` | CLAS / `$ZLK_06_02` | Demo that fires the `system_logout` client event |
| `Z2UI5_IF_CLIENT` | INTF | Defines the `cs_event-system_logout` constant |

> **Important:** the JS is duplicated in three places. If you change the logoff behavior, change it in all three so the runtime you're not testing today doesn't drift.

---

## 6. Your customization options

There are exactly four levers. Pick based on what you want, not all of them at once.

### Lever 1 — Override the post-logoff URL per call

Pass any same-origin URL as `args[1]` of the `system_logout` event. The JS uses it instead of `/sap/public/bc/icf/logoff`.

```abap
client->_event_client(
  val   = client->cs_event-system_logout
  t_arg = VALUE #( ( `/sap/public/bsp/sap/system/logoff.htm` ) ) ).
```

Use this when you want, e.g., the SAP standard BSP logoff confirmation page rather than wherever your customized ICF logoff redirects.

> Constraint: same origin only. The JS rejects cross-origin redirects.

### Lever 2 — Customize what `/sap/public/bc/icf/logoff` does (SICF)

Default behavior of this endpoint is system-level customizing. Common patterns:

| What you want | SICF setup |
|---|---|
| Show SAP's standard "you have logged off" page | leave default (ships out of the box) |
| Redirect to the FLP login | Service → Error Pages → Logoff Page → Redirect to URL = `…/sap/bc/ui2/flp` |
| Redirect to a custom company logoff page | Same path, set Redirect to URL = your page |
| Land on a fresh login dialog (force re-prompt) | Combine with disabling SSO for that path |

This is a **Basis / system-administrator** task. abap2UI5 cannot influence it from the app. If your symptom is "I logged out and came back into the launchpad," this lever is almost certainly the cause.

### Lever 3 — Run inside the FLP shell when you want shell logout

If your goal is to always go through `Container.logout()` (the cleanest user experience), run the app **as an FLP tile**, not via a BSP URL. From a BSP URL there is no shell to log out from — that's not a bug, that's by design of how FLP works.

### Lever 4 — Replace the JS entirely

If none of the above fits, edit the `_evSystemLogout` / `SYSTEM_LOGOUT` handler in the file matching your runtime (table in §5) and add whatever sequence you need: extra `fetch()` calls, custom IdP single-logout, post-logoff messages, etc. Keep the default branch intact so the existing demos keep working.

---

## 7. The SSO truth

In any SAP landscape with single sign-on, "logoff" from a single SAP app is inherently **soft**:

- Killing layers A + B + C in the browser deletes the SAP-side session
- The IdP session (Active Directory, SAML provider, OIDC, certificate-based auth) is **untouched**
- The next request to any protected SAP URL re-authenticates silently
- The user experience is "I clicked logoff, I came right back in"

This is not abap2UI5 behavior, it's SAP NetWeaver behavior. Real "log out everywhere" requires either:

1. **IdP-initiated single logout (SLO)** — your IdP's logout endpoint, not SAP's
2. **Closing the browser tab/window** so the IdP session cookies are lost
3. **A server-side logout-everywhere flow** that you build per landscape

If users complain "logoff doesn't really log me off," explain this constraint. The fix is at the IdP, not in the app.

---

## 8. Decision tree

```
User clicks Logout
│
├─ App is in FLP shell?            ───YES──▶  Container.logout()              [layer A killed]
│                                              FLP-managed redirect afterward
│                                              (typically to a logoff page)
│
└─ NO  (ICF handler or BSP)
   │
   ├─ App is on /sap/bc/bsp/…?     ───YES──▶  hidden iframe to ?sap-sessioncmd=logoff
   │                                          [layer C killed]
   │                                          THEN ↓
   │
   └─ window.location.href = logoutUrl       (default /sap/public/bc/icf/logoff)
                                              [layer B killed by ICF logoff service]
                                              afterward: SICF redirect target
                                              (your system administrator decides)
```

---

## 9. Common scenarios and what to expect

| Scenario | A | B | C | Where you land |
|---|---|---|---|---|
| Started from FLP, click Logout | ✅ | depends on FLP wiring | n/a | FLP "you have signed out" page |
| Started from ICF handler URL, click Logout | n/a | ✅ | n/a | Whatever `/sap/public/bc/icf/logoff` redirects to |
| Started from BSP URL, click Logout *(after fix)* | n/a | ✅ | ✅ | Whatever `/sap/public/bc/icf/logoff` redirects to — could be FLP if your SICF says so |
| Started from BSP URL, click Logout *(before fix)* | n/a | ✅ | ❌ context lingered in `SM04` | Same as above, but BSP context lived on |
| Any of the above, on an SSO-enabled system | as above | as above | as above | If the redirect target is itself protected, SSO re-authenticates — looks like "didn't log out" |

---

## 10. Recommended config matrix

| You want… | Lever 1 (`args[1]`) | Lever 2 (SICF) | Lever 3 (start in FLP) | Lever 4 (edit JS) |
|---|---|---|---|---|
| Standard "you have logged off" page after logout | — | leave default | — | — |
| Redirect to your company logoff page | optionally `args[1]` to that page | OR set in SICF | — | — |
| Always go through FLP shell logout | — | — | ✅ run as FLP tile | — |
| Hard logout including the IdP | — | combine with IdP SLO | — | possibly extra calls |
| Replace mechanism entirely (e.g. mobile app) | — | — | — | ✅ |

---

## 11. Glossary

- **FLP / Fiori Launchpad** — the SAP shell UI that hosts Fiori tiles; provides `sap.ushell.Container`.
- **ICF** — Internet Communication Framework, the AS ABAP HTTP server (transactions `SICF`, `SMICM`).
- **BSP** — Business Server Pages, an older SAP web technology that runs **stateful** by default and has its own session layer on top of ICF.
- **SSO / IdP** — Single Sign-On / Identity Provider; the external auth system (AD, SAML, OIDC, etc.). Logging out of SAP locally does **not** log you out of the IdP.
- **`sap-contextid`** — the BSP runtime's stateful-session cookie, scoped to `/sap/bc/bsp/sap/<app>/`.
- **`sap-sessioncmd=logoff`** — URL parameter that instructs the BSP runtime to call `server->session->terminate( )`.
