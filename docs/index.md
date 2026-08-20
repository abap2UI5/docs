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

# Three cards, one per thing a reader comes here to do: look a task up, take
# it to production, join in. Quickstart is not a card — the hero's first
# button is already that jump, and a card repeating it is a card spent twice.
# Technical Insight is not one either: it is what you read after the thing
# runs, not a way in. GitHub and LinkedIn are not cards — both sit in the nav
# bar as social icons, and a card spent on a link that is always visible is a
# card not spent on a journey.
#
# Samples are NOT a card either, and that is the one deliberate gap: the
# catalogue is a reading destination of its own, and a card that looks like
# the three around it and then hands you a corpus is the card people click by
# accident. It sits below the grid instead, as a single button, set apart,
# where leaving the grid — and this site — is the obvious thing to be doing.
features:
  - title: Cookbook
    icon: <i class="fa-solid fa-book"></i>
    details: Recipes for everyday tasks — views, binding, tables, events, popups, files.
    link: /cookbook/overview
  - title: Configuration
    icon: <i class="fa-solid fa-gear"></i>
    details: Setup, security, performance, launchpad — the road to production use.
    link: /configuration/setup
  - title: Community
    icon: <i class="fa-brands fa-github"></i>
    details: Browse the code, report issues, contribute — the project is built in the open.
    link: https://github.com/abap2UI5/abap2UI5/
---

<!-- Below the feature grid, off on its own: one button, straight to the
     sample page itself rather than to a page here describing it. That page
     names its own corpus, carries the search and the filters, and links the
     other two catalogues in the bar at its top — everything the page here
     used to say, said where the samples are. No figure on purpose: the count
     is on that page, kept by the repository that owns it, and a second copy
     here is one nothing would check. -->
<div class="a2ui5-catalogues">

Looking for a working app to copy? Hundreds of them are searchable in the browser — nothing to install.
{.a2ui5-catalogues-lead}

[Browse the Samples](https://abap2ui5.github.io/samples/)
{.a2ui5-catalogues-links}

</div>
