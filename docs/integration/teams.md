---
outline: [2, 4]
description: Show an abap2UI5 app in Microsoft Teams as a personal tab - why it is not an Adaptive Card, the user exit that lets Teams frame the app, a wrapper page with the Teams JavaScript library, the app manifest, the upload, and the sign-in.
---
# Microsoft Teams

An abap2UI5 app can open inside Microsoft Teams as a tab of its own, in the
left rail of the client, next to Chat and Calendar. It is the full app, with
every roundtrip going to your ABAP system as it does in a browser. What it
takes is a Teams app of your own: a manifest, two icons and one small HTML
page, plus a user exit that lets Teams put the app in a frame.

::: info A recipe, not a shipped feature
Nothing on this page is part of the framework: it combines the
[user exit](/advanced/extensibility/user_exits) with what Microsoft documents
for Teams tabs. If you run it on your system and something differs, an
[issue](https://github.com/abap2UI5/docs/issues) helps the next reader.
:::

## What Fits Where

Teams has three places a web app could go, and only one of them can carry an
abap2UI5 app:

| | |
|---|---|
| A card in a chat | An **Adaptive Card** is JSON that Teams draws itself, with no HTML and no JavaScript in it. A UI5 app cannot run there. A card can carry a button that opens the app (`Action.OpenUrl`), which needs nothing on the ABAP side |
| The built-in Website tab | Since July 2024 the new Teams client no longer shows a website inside this tab: it opens the address in the browser. It is a bookmark, not an integration |
| A tab of your own Teams app | Teams loads the page in a frame inside the client. **This is the one that works**, and the rest of this page sets it up |

## How It Fits Together

```
Teams client
└── tab: wrapper page  (loads the Teams JavaScript library)
    └── frame: https://sap.example.com/sap/bc/z2ui5?app_start=Z2UI5_CL_MY_APP
```

The wrapper page is there because Teams shows a tab only after the page in it
has called `app.initialize( )` of the Teams JavaScript library, and abap2UI5
does not load that library. The wrapper loads it, calls it, and puts the app
into a frame of its own. Nothing in your app class changes.

The [Excel add-in](/integration/excel) goes one step further: its page embeds
the app as a UI5 component with the embed control instead of a frame, so
abap2UI5's own node never has to be framed. A Teams page could be built the
same way; none exists yet, so this page uses the frame.

## Step 1: Let Teams Frame the App

Every abap2UI5 response carries `X-Frame-Options: SAMEORIGIN`, and with it a
browser refuses to show the app inside Teams: the tab stays blank. Replace it
in the method `set_config_http_get` of your user exit with a
`frame-ancestors` policy naming the hosts that may frame the app:

```abap
CLASS zcl_a2ui5_user_exit DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_ui5_exit.

ENDCLASS.

CLASS zcl_a2ui5_user_exit IMPLEMENTATION.

  METHOD z2ui5_if_ui5_exit~set_config_http_get.

    " SAMEORIGIN refuses every frame that Teams puts around the app
    DELETE cs_config-t_security_header WHERE n = `X-Frame-Options`.

    " frame-ancestors decides instead - as a header: browsers ignore
    " it in a <meta> policy
    APPEND VALUE #(
        n = `Content-Security-Policy`
        v = `frame-ancestors 'self'`
         && ` https://teams.microsoft.com https://*.teams.microsoft.com`
         && ` https://*.cloud.microsoft`
         && ` https://*.microsoft365.com https://*.office.com` )
        TO cs_config-t_security_header.

  ENDMETHOD.

  METHOD z2ui5_if_ui5_exit~set_config_http_post.

  ENDMETHOD.

ENDCLASS.
```

The hosts are the ones Microsoft lists for Teams and the other Microsoft 365
apps that can show a tab. `'self'` is the wrapper page when it is served by
the same SAP system; if it lives on another host, add that origin to the list,
because a browser checks every frame around the app, not only the outermost.

The header only adds `frame-ancestors`. The rest of the policy abap2UI5 sends
stays as it is, so scripts and styles are as restricted as before.

## Step 2: The Wrapper Page

One static HTML page per app, served over HTTPS. Teams does not load a page
from a server with a self-signed certificate, and it does not load plain HTTP
at all.

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>Orders</title>
  <script src="https://cdn.jsdelivr.net/npm/@microsoft/teams-js@2.57.0/dist/umd/MicrosoftTeams.min.js"></script>
  <style>
    html, body { margin: 0; height: 100%; }
    iframe { display: block; width: 100%; height: 100%; border: 0; }
  </style>
</head>
<body>
  <iframe id="app" title="Orders"></iframe>
  <script>
    // Teams shows the tab once the page has called app.initialize( )
    microsoftTeams.app.initialize().then(function () {
      document.getElementById("app").src =
        "https://sap.example.com/sap/bc/z2ui5?app_start=Z2UI5_CL_MY_APP";
    });
  </script>
</body>
</html>
```

The Teams JavaScript library is the npm package `@microsoft/teams-js`; take
it from a CDN as above, from Microsoft's own CDN, or serve the file yourself
next to the page. Where the page lives is up to you. On the SAP system itself
(a BSP application, for example) it is on the same origin as the app, which
keeps the frame policy short: `'self'` covers it.

## Step 3: The App Manifest

A Teams app is a zip file with three files in it: `manifest.json`, a color
icon of 192 × 192 pixels and an outline icon of 32 × 32 pixels (white on a
transparent background). A personal tab needs no more than this:

```json
{
  "$schema": "https://developer.microsoft.com/json-schemas/teams/v1.17/MicrosoftTeams.schema.json",
  "manifestVersion": "1.17",
  "version": "1.0.0",
  "id": "8b2f5a3e-1c4d-4e8a-9f6b-2d7c0e1a5b93",
  "developer": {
    "name": "Contoso IT",
    "websiteUrl": "https://sap.example.com",
    "privacyUrl": "https://sap.example.com/privacy",
    "termsOfUseUrl": "https://sap.example.com/terms"
  },
  "name": {
    "short": "Orders",
    "full": "Orders from SAP"
  },
  "description": {
    "short": "The open orders of the SAP system",
    "full": "Shows the abap2UI5 app Z2UI5_CL_MY_APP of the SAP system in a Teams tab."
  },
  "icons": {
    "color": "color.png",
    "outline": "outline.png"
  },
  "accentColor": "#0A6ED1",
  "staticTabs": [
    {
      "entityId": "orders",
      "name": "Orders",
      "contentUrl": "https://sap.example.com/teams/orders.html",
      "websiteUrl": "https://sap.example.com/sap/bc/z2ui5?app_start=Z2UI5_CL_MY_APP",
      "scopes": ["personal"]
    }
  ],
  "validDomains": ["sap.example.com"]
}
```

| Field | |
|---|---|
| `id` | A GUID of your own, generated once and kept for every later version of the app |
| `version` | Raise it with every upload, or Teams keeps the previous package |
| `contentUrl` | The wrapper page from step 2 |
| `websiteUrl` | Where **Open in browser** in the tab's menu goes: the app itself, without the wrapper |
| `validDomains` | Every host the tab loads a page from - the wrapper's and the SAP system's, without `https://` |

Version 1.17 of the schema is enough for a static tab; a newer one works the
same way. One manifest can carry several entries under `staticTabs`, one per
app.

## Step 4: Upload the App

Zip the three files - the files themselves, not a folder around them - and
upload the zip:

- **For yourself, to try it:** in Teams, **Apps** › **Manage your apps** ›
  **Upload an app** › **Upload a custom app**. Your tenant has to allow custom
  apps for this; many do not.
- **For your organization:** the Teams admin center, **Teams apps** ›
  **Manage apps** › **Upload new app**, and a setup policy that pins the app
  for the users who need it.

The app then appears under **Apps**, and **Pin** puts it in the left rail.

## Sign-In

The tab shows whatever the SAP system answers, including its sign-in, and
that is the part to plan for:

- **Single sign-on is the setup that works.** Users are signed in to
  Microsoft Entra ID in Teams already; with SAML or OpenID Connect between
  the SAP system and Entra ID they arrive signed in, and nobody sees a login
  form in the tab.
- **A login page in a frame often fails.** Many identity providers refuse to
  be framed, for the same clickjacking reason abap2UI5 does, and a basic
  authentication prompt inside a frame is up to the client to show or not.
- **The session cookie has to be allowed in a frame.** The tab is a frame
  inside a Teams page, so the SAP session cookie is a third-party cookie
  there, wherever the wrapper page lives, and a browser sends it only with
  `SameSite=None; Secure`. On the ABAP side that is the profile parameter
  `icf/set_SameSiteAttribute`. A browser that blocks third-party cookies
  altogether loses the session.

Which cookie rules apply depends on the client: the Teams desktop client on
Windows runs the tab in the Edge webview, the web client in whatever browser
it is open in.

## Troubleshooting

| What you see | Why |
|---|---|
| The tab stays white, the browser console says `Refused to display ... in a frame` | The exit from step 1 is not active, or the console names a host that is missing from `frame-ancestors` |
| A spinner, then "There was a problem reaching this app" | The wrapper page did not call `app.initialize( )`, or could not load the Teams JavaScript library |
| A login form in a loop, or a timeout after the first click | The session cookie does not survive the frame - see [Sign-In](#sign-in) |

To see the console of the desktop client, open the tab in the web client
(`teams.microsoft.com`) with the browser's developer tools, which shows the
same messages.
