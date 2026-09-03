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

# Two rows of cards. This row, the theme's own, is the two things a reader
# comes here to READ: learn it, take it to production. Two cards, on purpose —
# VPFeatures picks its column count from the number of cards, and two gives
# a row of two, which is the line break before the second row below. The
# Cookbook is not one of them: it answers a question you already have, and
# someone on the home page does not have it yet — the Tutorial is what a
# first visit is for, and the Cookbook is a click away in the nav the moment
# it is wanted. Quickstart is not a card either — the hero's first button is
# already that jump, and a card repeating it is a card spent twice. Technical
# Insight is not one either: it is what you read after the thing runs, not a
# way in.
#
# The second row, in the markdown below the frontmatter, is the three places
# that are not pages of this site: the repository, LinkedIn and the
# playground. They used to be scattered — Community was a third card up here,
# the playground a lone button under a rule, LinkedIn only an icon in the nav
# bar — and a reader who had finished the two cards above had nowhere
# obvious to go next. Now they are a row of their own, drawn differently
# from the two above (a tinted fill where these are outlined), so that a
# card that leaves this site is recognisable as one before it is clicked,
# and looks like something to click rather than something to read.
features:
  - title: Tutorial
    icon: <i class="fa-solid fa-graduation-cap"></i>
    details: Learn by building — steps that grow one runnable app, from a message box to a tested app in production.
    link: /tutorials/walkthrough/
  - title: Configuration
    icon: <i class="fa-solid fa-gear"></i>
    details: Setup, security, performance, launchpad — the road to production use.
    link: /configuration/setup
---

<!-- The second row of cards - see the note above the features. Every card is
     one link, opened in a new tab: all three are somewhere else. The marks
     are inline rather than Font Awesome's, the same three the nav bar
     carries: an icon that is an empty square until a stylesheet from a CDN
     arrives is worse than one that never needed it. -->
<div class="a2ui5-out">
  <a class="a2ui5-out-card" href="https://github.com/abap2UI5/abap2UI5/" target="_blank" rel="noreferrer">
    <span class="a2ui5-out-icon"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><path fill="currentColor" d="M12 .297c-6.63 0-12 5.373-12 12 0 5.303 3.438 9.8 8.205 11.385.6.113.82-.258.82-.577 0-.285-.01-1.04-.015-2.04-3.338.724-4.042-1.61-4.042-1.61C4.422 18.07 3.633 17.7 3.633 17.7c-1.087-.744.084-.729.084-.729 1.205.084 1.838 1.236 1.838 1.236 1.07 1.835 2.809 1.305 3.495.998.108-.776.417-1.305.76-1.605-2.665-.3-5.466-1.332-5.466-5.93 0-1.31.465-2.38 1.235-3.22-.135-.303-.54-1.523.105-3.176 0 0 1.005-.322 3.3 1.23.96-.267 1.98-.399 3-.405 1.02.006 2.04.138 3 .405 2.28-1.552 3.285-1.23 3.285-1.23.645 1.653.24 2.873.12 3.176.765.84 1.23 1.91 1.23 3.22 0 4.61-2.805 5.625-5.475 5.92.42.36.81 1.096.81 2.22 0 1.606-.015 2.896-.015 3.286 0 .315.21.69.825.57C20.565 22.092 24 17.592 24 12.297c0-6.627-5.373-12-12-12"/></svg></span>
    <span class="a2ui5-out-title">Community</span>
    <span class="a2ui5-out-details">Browse the code, report issues, contribute — the project is built in the open on GitHub.</span>
  </a>
  <a class="a2ui5-out-card" href="https://www.linkedin.com/company/abap2ui5/" target="_blank" rel="noreferrer">
    <span class="a2ui5-out-icon"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><path fill="currentColor" d="M20.447 20.452h-3.554v-5.569c0-1.328-.027-3.037-1.852-3.037-1.853 0-2.136 1.445-2.136 2.939v5.667H9.351V9h3.414v1.561h.046c.477-.9 1.637-1.85 3.37-1.85 3.601 0 4.267 2.37 4.267 5.455v6.286zM5.337 7.433c-1.144 0-2.063-.926-2.063-2.065 0-1.138.92-2.063 2.063-2.063 1.14 0 2.064.925 2.064 2.063 0 1.139-.925 2.065-2.064 2.065zm1.782 13.019H3.555V9h3.564v11.452zM22.225 0H1.771C.792 0 0 .774 0 1.729v20.542C0 23.227.792 24 1.771 24h20.451C23.2 24 24 23.227 24 22.271V1.729C24 .774 23.2 0 22.225 0z"/></svg></span>
    <span class="a2ui5-out-title">LinkedIn</span>
    <span class="a2ui5-out-details">Follow along — releases, articles and what people are building with it.</span>
  </a>
  <a class="a2ui5-out-card" href="https://abap2ui5.github.io/playground/" target="_blank" rel="noreferrer">
    <span class="a2ui5-out-icon"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><circle cx="12" cy="12" r="9.75" fill="none" stroke="currentColor" stroke-width="1.7"/><path d="M9.6 7.9v8.2a.5.5 0 0 0 .76.43l6.6-4.1a.5.5 0 0 0 0-.86l-6.6-4.1a.5.5 0 0 0-.76.43z" fill="currentColor"/></svg></span>
    <span class="a2ui5-out-title">Playground</span>
    <span class="a2ui5-out-details">Write ABAP in the browser and watch it run — nothing to install, nothing to sign up for.</span>
  </a>
</div>
