---
# https://vitepress.dev/reference/default-theme-home-page
layout: home
title: Home

# THIS PAGE IS THE PROJECT'S FRONT DOOR, NOT THE MANUAL'S.
#
# It used to be the documentation's home page, reachable by clicking the mark
# and by nothing else, and it read like one: a hero, then three ways into three
# sections of this site. The bar names four places now — Home, Documentation,
# Samples, Playground — and Home is the one that has to answer "what IS this"
# for somebody who arrived from a conference talk, a LinkedIn post or a
# colleague's link, and who has not decided to read a manual yet.
#
# So the page answers, in this order: what it is, what one app looks like (with
# a button that RUNS it, right here), where to go next, what it runs on, and
# what is built around it. A reader who wants the manual is one word away in
# the bar; this page does not compete with it.
hero:
  name: abap2UI5
  text: Build UI5 Apps Purely in ABAP
  tagline: "One ABAP class is one UI5 app. No JavaScript, no OData service, no RAP, no frontend project.\nInstall it with abapGit and run it on anything from NetWeaver 7.02 to ABAP Cloud."
  image:
    src: /logo.png
    alt: abap2UI5 Logo
    width: 200px
    height: 200px
  # Three buttons, in the order a stranger needs them: try it without
  # installing anything, install it, understand it. The playground is first on
  # purpose — it is the one claim on this page a reader can check in ten
  # seconds, and it costs them nothing. It is an absolute URL, so VitePress
  # draws it as an external link and gives it a `target` of its own, which is
  # also what keeps this site's router off a neighbouring deployment
  # (scripts/lib/cross-site.mjs).
  actions:
    - theme: brand
      text: Try it in the browser
      link: https://abap2ui5.github.io/playground/
    - theme: alt
      text: Install with abapGit
      link: /get_started/quickstart
    - theme: alt
      text: What it is
      link: /get_started/about

# The other three places the bar names, in the order the bar names them. Not
# three sections of this site any more: a reader on this page is choosing
# between reading, browsing and trying, and two of those are somewhere else.
features:
  - title: Documentation
    icon: <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><path fill="none" stroke="currentColor" stroke-width="1.7" stroke-linejoin="round" d="M12 6.9C10.4 5.5 8.2 4.8 5.4 4.8H2.4v12.6h3c2.8 0 5 .7 6.6 2.1 1.6-1.4 3.8-2.1 6.6-2.1h3V4.8h-3c-2.8 0-5 .7-6.6 2.1z"/><path fill="none" stroke="currentColor" stroke-width="1.7" d="M12 6.9v12.6"/></svg>
    details: The manual — a tutorial you build along with, a cookbook of tables, popups, navigation and uploads, and everything between your first app and real users.
    link: /get_started/about
  - title: Samples
    icon: <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><rect x="2.6" y="4.2" width="18.8" height="15.6" rx="2" fill="none" stroke="currentColor" stroke-width="1.7"/><path fill="none" stroke="currentColor" stroke-width="1.7" stroke-linecap="round" d="M2.6 9.1h18.8M8.2 9.1v10.7"/></svg>
    details: Over 700 working apps, searchable by control, by library and by what your system can run — the UI5 demo kit rebuilt in ABAP, plus everything that needs OData, RAP or a launchpad.
    link: https://abap2ui5.github.io/playground/samples/
    target: _self
  - title: Playground
    icon: <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><circle cx="12" cy="12" r="9.75" fill="none" stroke="currentColor" stroke-width="1.7"/><path d="M9.6 7.9v8.2a.5.5 0 0 0 .76.43l6.6-4.1a.5.5 0 0 0 0-.86l-6.6-4.1a.5.5 0 0 0-.76.43z" fill="currentColor"/></svg>
    details: Write ABAP in the browser and watch the app run beside it. The whole framework compiled into a page — no server, no system, nothing to install.
    link: https://abap2ui5.github.io/playground/
    target: _self
---

## One class, one app

This is a complete abap2UI5 application. It has a public attribute the view
binds to, a view built in ABAP, and an event handler — nothing else. Press
**Run this example** and it starts in your browser, on the real framework.

```abap
CLASS zcl_app_hello DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA recipient TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_app_hello IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      recipient = `World`.

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Shell`
                  )->ele( `Page`
                      )->a( n = `title` v = `Hello abap2UI5`

                      )->tag( `Input`
                          )->a( n = `value` v = client->_bind( recipient )
                      )->tag( `Button`
                          )->a( n = `text`  v = `Say Hello`
                          )->a( n = `press` v = client->_event( `SAY_HELLO` ) ).

      client->view_display( view->stringify( ) ).

    ELSEIF client->check_on_event( `SAY_HELLO` ).

      client->message_toast_display( |Hello { recipient }!| ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

- **The attribute is the model.** `recipient` travels to the browser, comes
  back edited, and is a plain ABAP string on both sides. The framework
  serializes your instance between roundtrips, so the class keeps its state
  without any session handling of your own.
- **The view is ABAP.** No XML file in a repository, no frontend project, no
  build step — the view is built at runtime and sent as data.
- **Events come back as ABAP.** `check_on_event( )` is where the button press
  arrives, in the same class, with the model already updated.

## What it needs, and what it does not

| | |
|---|---|
| **Runs on** | NetWeaver 7.02 and up, S/4HANA, ABAP Cloud, on-premise, private and public cloud, and the trial systems |
| **Installed with** | [abapGit](/get_started/quickstart) — one repository, no transport of frontend artefacts, no BSP application to maintain |
| **Needs no** | JavaScript, OData service, RAP business object, CDS view, frontend project or Node toolchain |
| **Speaks** | UI5, over stateless HTTP roundtrips against the framework's own service |
| **Licence** | MIT, and the code is [on GitHub](https://github.com/abap2UI5/abap2UI5) |

Old releases matter here: apps written on 7.02 use the same API as apps on ABAP
Cloud, and [Downporting](/advanced/downporting) explains what the framework
does so that they can.

## Around the framework

<div class="a2ui5-out">
  <a class="a2ui5-out-card is-inside" href="/docs/resources/addons">
    <span class="a2ui5-out-title">Add-ons</span>
    <span class="a2ui5-out-details">Optional repositories for the things not every app needs: popups, HTTP and RFC connectors, a lock manager, table maintenance, launchpad KPIs.</span>
  </a>
  <a class="a2ui5-out-card" href="https://abap2ui5.github.io/linter/" target="_self">
    <span class="a2ui5-out-title">Linter</span>
    <span class="a2ui5-out-details">Rules that read abap2UI5 code as abap2UI5 — view chains, bindings, event wiring — with a rule reference you can read on its own.</span>
  </a>
  <a class="a2ui5-out-card is-inside" href="/docs/advanced/mcp_server">
    <span class="a2ui5-out-title">Tooling</span>
    <span class="a2ui5-out-details">A VS Code extension, an MCP server so an AI assistant answers from the real API, and an app template to start from.</span>
  </a>
</div>

## Built in the open

<div class="a2ui5-out">
  <a class="a2ui5-out-card" href="https://github.com/abap2UI5/abap2UI5/" target="_blank" rel="noreferrer">
    <span class="a2ui5-out-icon"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><path fill="currentColor" d="M12 .297c-6.63 0-12 5.373-12 12 0 5.303 3.438 9.8 8.205 11.385.6.113.82-.258.82-.577 0-.285-.01-1.04-.015-2.04-3.338.724-4.042-1.61-4.042-1.61C4.422 18.07 3.633 17.7 3.633 17.7c-1.087-.744.084-.729.084-.729 1.205.084 1.838 1.236 1.838 1.236 1.07 1.835 2.809 1.305 3.495.998.108-.776.417-1.305.76-1.605-2.665-.3-5.466-1.332-5.466-5.93 0-1.31.465-2.38 1.235-3.22-.135-.303-.54-1.523.105-3.176 0 0 1.005-.322 3.3 1.23.96-.267 1.98-.399 3-.405 1.02.006 2.04.138 3 .405 2.28-1.552 3.285-1.23 3.285-1.23.645 1.653.24 2.873.12 3.176.765.84 1.23 1.91 1.23 3.22 0 4.61-2.805 5.625-5.475 5.92.42.36.81 1.096.81 2.22 0 1.606-.015 2.896-.015 3.286 0 .315.21.69.825.57C20.565 22.092 24 17.592 24 12.297c0-6.627-5.373-12-12-12"/></svg></span>
    <span class="a2ui5-out-title">Community</span>
    <span class="a2ui5-out-details">Read the code, open an issue, send a pull request — abap2UI5 is built in the open, and contributions are welcome.</span>
  </a>
  <a class="a2ui5-out-card" href="https://www.linkedin.com/company/abap2ui5/" target="_blank" rel="noreferrer">
    <span class="a2ui5-out-icon"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><path fill="currentColor" d="M20.447 20.452h-3.554v-5.569c0-1.328-.027-3.037-1.852-3.037-1.853 0-2.136 1.445-2.136 2.939v5.667H9.351V9h3.414v1.561h.046c.477-.9 1.637-1.85 3.37-1.85 3.601 0 4.267 2.37 4.267 5.455v6.286zM5.337 7.433c-1.144 0-2.063-.926-2.063-2.065 0-1.138.92-2.063 2.063-2.063 1.14 0 2.064.925 2.064 2.063 0 1.139-.925 2.065-2.064 2.065zm1.782 13.019H3.555V9h3.564v11.452zM22.225 0H1.771C.792 0 0 .774 0 1.729v20.542C0 23.227.792 24 1.771 24h20.451C23.2 24 24 23.227 24 22.271V1.729C24 .774 23.2 0 22.225 0z"/></svg></span>
    <span class="a2ui5-out-title">LinkedIn</span>
    <span class="a2ui5-out-details">Follow along — new releases, articles, and what people are building with it right now.</span>
  </a>
  <a class="a2ui5-out-card is-inside" href="/docs/resources/sponsor">
    <span class="a2ui5-out-icon"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><path fill="currentColor" d="M12 21s-7.6-4.9-9.5-9.2C1.1 8.4 3 5 6.4 5c2 0 3.4 1.1 4.3 2.3l1.3 1.7 1.3-1.7C14.2 6.1 15.6 5 17.6 5c3.4 0 5.3 3.4 3.9 6.8C19.6 16.1 12 21 12 21z"/></svg></span>
    <span class="a2ui5-out-title">Sponsor</span>
    <span class="a2ui5-out-details">The framework is free and maintained by volunteers. If it saved your project time, there is a way to give some back.</span>
  </a>
</div>
