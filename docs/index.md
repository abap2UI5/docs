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
# look things up, take it to production, understand it, join in. GitHub and
# LinkedIn are NOT cards — both already sit in the nav bar as social icons,
# and a card spent on a link that is always visible is a card not spent on a
# journey.
#
# Samples are NOT a card either, and that is the one deliberate gap: the three
# catalogues are somewhere else entirely — three published pages, not a page of
# this site — and a card that looks like the five around it and then leaves the
# site is the card people click by accident. They sit below the grid instead,
# set apart, where leaving is the obvious thing to be doing.
features:
  - title: Quickstart
    icon: <i class="fa-solid fa-rocket"></i>
    details: Install with abapGit and launch your first app in minutes.
    link: /get_started/quickstart
  - title: Cookbook
    icon: <i class="fa-solid fa-book"></i>
    details: Recipes for everyday tasks — views, binding, tables, events, popups, files.
    link: /cookbook/overview
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

<!-- Below the feature grid, off on its own: the three sample catalogues, each
     a page published by its own repository. No figure here on purpose —
     check:counts verifies the counts on resources/samples.md against the
     catalogues themselves, and a second copy on this page is one nothing
     would check. -->
<div class="a2ui5-catalogues">
  <p class="a2ui5-catalogues-lead">Looking for a working app to copy? The sample catalogues are searchable in the browser — nothing to install.</p>
  <p class="a2ui5-catalogues-links">
    <a href="https://abap2ui5.github.io/samples/" target="_blank" rel="noreferrer">Learn</a>
    <a href="https://abap2ui5.github.io/samples-controls/" target="_blank" rel="noreferrer">Controls</a>
    <a href="https://abap2ui5.github.io/samples-stack/" target="_blank" rel="noreferrer">Stack</a>
  </p>
</div>
