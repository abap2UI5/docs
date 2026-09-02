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
      text: Quickstart
      link: /get_started/quickstart
    - theme: alt
      text: What's New?
      link: /resources/changelog

# Three cards, one per thing a reader comes here to do: learn it, take it to
# production, join in. The Cookbook is not one of them any more: it answers a
# question you already have, and someone on the home page does not have it
# yet — the Tutorial is what a first visit is for, and the Cookbook is a click
# away in the nav the moment it is wanted. Quickstart is not a card either —
# the hero's first button is already that jump, and a card repeating it is a
# card spent twice.
# Technical Insight is not one either: it is what you read after the thing
# runs, not a way in. GitHub and LinkedIn are not cards — both sit in the nav
# bar as social icons, and a card spent on a link that is always visible is a
# card not spent on a journey.
#
# The playground is not a card, and that is the one deliberate gap: it is
# not a page to read but a thing to try, and a card that looks like the three
# around it and then opens an editor is the card people click by accident.
# It sits below the grid instead, as a single button, set apart, where
# leaving the grid — and this site — is the obvious thing to be doing. It
# used to be the hero's third button, "Live Demo", next to Quickstart and
# What's New; three buttons in a row read as three equal choices, and the
# one that lets you try the thing before reading a word deserves a line of
# its own.
features:
  - title: Tutorial
    icon: <i class="fa-solid fa-graduation-cap"></i>
    details: Learn by building — steps that grow one runnable app, from a message box to a tested app in production.
    link: /tutorials/walkthrough/
  - title: Configuration
    icon: <i class="fa-solid fa-gear"></i>
    details: Setup, security, performance, launchpad — the road to production use.
    link: /configuration/setup
  - title: Community
    icon: <i class="fa-brands fa-github"></i>
    details: Browse the code, report issues, contribute — the project is built in the open.
    link: https://github.com/abap2UI5/abap2UI5/
---

<!-- Below the feature grid, off on its own: one button, straight into the
     playground. The invitation carries what the hero cannot in five words:
     nothing to install, nothing to sign up for, the app runs in the browser
     next to the code. The samples are a click away from there, and from the
     cookbook chapters, so they need no button here. -->
<div class="a2ui5-catalogues">

Curious what an app looks like? Write ABAP in the browser and watch it run — nothing to install, nothing to sign up for.
{.a2ui5-catalogues-lead}

[Try it in the Playground](https://abap2ui5.github.io/playground/)
{.a2ui5-catalogues-links}

</div>
