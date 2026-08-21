---
outline: [2, 4]
description: Take the finished walkthrough app to a real system — real data, a pinned release, the transport order, authorization, and the URL users start it from.
---
# Step 11: From Playground to Production

The app from [Step 10](/tutorials/walkthrough/step-10) is finished — and so
far it has run in the playground, or in your development system against demo
data. This step takes it to the system your users work on. Unlike the ten
steps before it, there is nothing new to build here: everything this step
needs already has a configuration chapter, and what follows is those chapters
in the order the invoice app meets them.

If your framework installation is not done yet, that comes first — the
[Quickstart](/get_started/quickstart) is the end-to-end path, including the
verify step at its end.

## Make the Data Real

Step 10 separated the app into seams on purpose, and this is the step where
that pays off the first time: `data_read` and `data_update` are the **only**
two methods that touch data. Each carries the real statement as a comment —
replace the `VALUE #( … )` demo table with the `SELECT` printed above it, and
the in-memory update with the `UPDATE`, and no other method changes. View,
events and popup never knew the data was fake.

One caution before the class leaves your machine: only `SELECT` from tables
that exist on every system the app will reach. A dependency on an SD table,
for example, compiles fine on your system and is a syntax error on a system
without SD.

## Pin the Framework

Your app class travels through the landscape; the framework underneath it
should hold still while it does. There is no fixed "stable" version — instead,
pin the installation to a [release](https://github.com/abap2UI5/abap2UI5/releases/)
rather than tracking `main`, and update deliberately:
[Productive Usage](/configuration/productive_usage) explains the policy, the
[Release Notes](/resources/changelog) list every change, and the
[Deprecations](/resources/deprecations) page names what is on its way out and
what replaces it. If other abap2UI5 apps already run in production and you
want new development decoupled from them, that page also points to the
[renaming feature](/advanced/renaming) — a second, independently versioned
installation of the framework.

## Transport, in Order

abap2UI5 ships as ABAP objects, so the way to production is the transport
process you already have — with one ordering rule, spelled out on the
[Transport](/configuration/transport) page:

1. Transport the framework and the HTTP service first.
2. On the target system, activate the service if needed, and adjust the
   [UI5 bootstrap source](/configuration/setup/ui5_bootstrapping) if
   production should load UI5 from somewhere else — a system without internet
   access serves UI5 itself.
3. Confirm the installation with a Hello World class before your app arrives —
   the same check as the Quickstart's verify step, on the production system.
4. Then transport the invoice app.

That order exists so that when something fails, you know which layer failed:
a broken step 3 is installation, not your app.

## Decide Who May Do What

Two different questions, answered in two different places — the
[Security](/configuration/security) page is the map:

**Authentication** — who gets in at all — is the ICF node's job, exactly as
for any other service on your system: logon procedure, visibility, all of it
on the service node you created in the Quickstart. The framework's own
defaults (a Content-Security-Policy, security response headers, CSRF
protection on every POST) are already on; nothing to configure.

**Authorization** — who may do *what* — stays yours, and the invoice app
makes it concrete: whoever reaches the service can press *Read Invoices*, and
whoever can open the edit dialog can press *Save*. The framework runs no
check in between — the [Authorization](/configuration/authorization) page
shows both places to put one: in the HTTP handler, gating which app classes a
user may start at all, and in the app class, where an `AUTHORITY-CHECK` at
the top of `main` — or one in `data_update`, if reading and writing separate —
protects the operation itself. If you check only at the service level, make
sure users cannot reach the app through navigation from another one.

## Give Users a Way In

Your users do not open `SICF`. What they need is the URL from the Quickstart:

```
https://<host>:<port>/sap/bc/<your_service>?app_start=zcl_app_walkthrough
```

That URL is a bookmark, an intranet link — or a tile: the
[Installation](/configuration/installation) page says what launching looks
like per system, from the [Fiori Launchpad](/configuration/launchpad) on
S/4 Private Cloud and On-Premise to the tile chain on
[S/4 Public Cloud](/configuration/s4_public_cloud). On a phone, the app can
be [added to the home screen](/configuration/mobile_start) like any web app.

## What to Take Away

- The seams from Step 10 are production seams: making the data real touched
  two methods and nothing else
- Pin a release, and read the changelog when you move it
- Transport bottom-up — framework, service, Hello World, then the app — so a
  failure names its layer
- Authentication is configuration on the ICF node; authorization is code you
  write, at the service level, in the app, or both
- Users start the app from a URL; everything else — tile, bookmark, home
  screen — is a wrapper around `?app_start=`

The app is live. What keeps it safe from the *next* change is
[Step 12](/tutorials/walkthrough/step-12) — unit tests against the app class,
which the structure from Step 10 makes plain ABAP.
