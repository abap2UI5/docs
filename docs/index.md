---
# https://vitepress.dev/reference/default-theme-home-page
layout: home
title: Home

hero:
  name: abap2UI5
  text: Build UI5 Apps Purely in ABAP
  tagline: "No JavaScript, OData, or RAP needed.\nKeep it simple. Keep it ABAP."
  image:
    src: /logo.png
    alt: abap2UI5 Logo
    width: 200px
    height: 200px
  actions:
    - theme: brand
      text: Get Started
      link: /get_started/quickstart
    - theme: alt
      text: What's New?
      link: /resources/changelog
    - theme: alt
      text: Playground
      link: https://abap2ui5.github.io/playground/

# One card per reader journey, in the order a newcomer meets them: install,
# look things up, copy from working apps, take it to production, understand
# it, join in. GitHub and LinkedIn are NOT cards — both already sit in the
# nav bar as social icons, and a card spent on a link that is always visible
# is a card not spent on a journey.
features:
  - title: Quickstart
    icon: <i class="fa-solid fa-rocket"></i>
    details: Install with abapGit and launch your first app in minutes.
    link: /get_started/quickstart
  - title: Cookbook
    icon: <i class="fa-solid fa-book"></i>
    details: Recipes for everyday tasks — views, binding, tables, events, popups, files.
    link: /cookbook/overview
  - title: Samples
    icon: <i class="fa-solid fa-shapes"></i>
    details: Hundreds of small working apps to copy from — one per control or pattern.
    link: /resources/samples
  - title: Configuration
    icon: <i class="fa-solid fa-gear"></i>
    details: Setup, security, performance, launchpad — the road to production use.
    link: /configuration/setup
  - title: Technical Insight
    icon: <i class="fa-solid fa-lightbulb"></i>
    details: How UI5 over the wire works, and the open-source tools it builds on.
    link: /technical/concept
  - title: Community
    icon: <i class="fa-brands fa-github"></i>
    details: Browse the code, report issues, contribute — the project is built in the open.
    link: https://github.com/abap2UI5/abap2UI5/
---

## A complete app in nine lines

```abap
CLASS zcl_app_hello_world DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS zcl_app_hello_world IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
    client->message_box_display( `Hello World` ).
  ENDMETHOD.
ENDCLASS.
```

That's it — no JavaScript, no OData service, no frontend deployment. Copy the
class into your system and it runs anywhere abapGit reaches, from legacy R/3
releases to S/4HANA Cloud and BTP. The [Hello World](/get_started/hello_world)
page explains what happens here — or press **Run this example** above and watch
this very class start in the [playground](https://abap2ui5.github.io/playground/):
the whole framework compiled into a browser page, no SAP system needed.
