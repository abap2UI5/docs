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

# Two rows of three. The first, the theme's own, is the three ways INTO the
# documentation, in the order a reader meets them: learn it, look things up
# while building, take it to production. The Cookbook is the middle one on
# purpose — it used to be left off on the argument that it answers a question
# a first-time visitor does not have yet, but most visits here are not first
# visits, and the reader who arrives already stuck was being sent through a
# nav dropdown to find the page written for them. Quickstart is still not a
# card: the hero's first button is that jump, and a card repeating it is a
# card spent twice. Technical Insight is not one either — it is what you read
# after the thing runs, not a way in.
#
# Three cards give VPFeatures `grid-3`, which is why the second row below —
# the three places that are NOT pages of this site: the repository, LinkedIn
# and the playground — is set to the same three columns at the same
# breakpoint. Two rows of three, one grid. They are drawn differently (a
# tinted fill where these are outlined) so that a card which leaves this site
# is recognisable as one before it is clicked.
#
# Every `details` line is written to be clicked rather than read: it opens on
# what the reader gets to DO there, and names the concrete things they came
# looking for. A card that only describes its section is a card that loses to
# the search box.
features:
  - title: Tutorial
    icon: <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><path fill="currentColor" d="M12 3 1.4 7.6 12 12.2l10.6-4.6z"/><path fill="none" stroke="currentColor" stroke-width="1.7" stroke-linecap="round" d="M5.6 10.3v4.8c0 1.8 2.9 3.2 6.4 3.2s6.4-1.4 6.4-3.2v-4.8"/><path fill="none" stroke="currentColor" stroke-width="1.7" stroke-linecap="round" d="M21.4 8.7v5.6"/></svg>
    details: Start here — one app you build step by step, from a first message box to a tested app running in production.
    link: /tutorials/walkthrough/
  - title: Cookbook
    icon: <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><path fill="none" stroke="currentColor" stroke-width="1.7" stroke-linejoin="round" d="M12 6.9C10.4 5.5 8.2 4.8 5.4 4.8H2.4v12.6h3c2.8 0 5 .7 6.6 2.1 1.6-1.4 3.8-2.1 6.6-2.1h3V4.8h-3c-2.8 0-5 .7-6.6 2.1z"/><path fill="none" stroke="currentColor" stroke-width="1.7" d="M12 6.9v12.6"/></svg>
    details: Look it up — tables, popups, navigation, file upload and download. One page per problem, each with code to copy.
    link: /cookbook/view/definition
  - title: Configuration
    icon: <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><path fill="none" stroke="currentColor" stroke-width="1.7" stroke-linejoin="round" d="M9.89 4.65 10.11 1.82h3.78l.22 2.83 1.6.66 2.15-1.84 2.67 2.67-1.84 2.15.66 1.6 2.83.22v3.78l-2.83.22-.66 1.6 1.84 2.15-2.67 2.67-2.15-1.84-1.6.66-.22 2.83h-3.78l-.22-2.83-1.6-.66-2.15 1.84-2.67-2.67 1.84-2.15-.66-1.6-2.83-.22v-3.78l2.83-.22.66-1.6-1.84-2.15 2.67-2.67 2.15 1.84z"/><circle cx="12" cy="12" r="3.2" fill="none" stroke="currentColor" stroke-width="1.7"/></svg>
    details: Go live — setup, security, performance, transport and the launchpad. Everything between your first app and real users.
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
    <span class="a2ui5-out-details">Read the code, open an issue, send a pull request — abap2UI5 is built in the open, and contributions are welcome.</span>
  </a>
  <a class="a2ui5-out-card" href="https://www.linkedin.com/company/abap2ui5/" target="_blank" rel="noreferrer">
    <span class="a2ui5-out-icon"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><path fill="currentColor" d="M20.447 20.452h-3.554v-5.569c0-1.328-.027-3.037-1.852-3.037-1.853 0-2.136 1.445-2.136 2.939v5.667H9.351V9h3.414v1.561h.046c.477-.9 1.637-1.85 3.37-1.85 3.601 0 4.267 2.37 4.267 5.455v6.286zM5.337 7.433c-1.144 0-2.063-.926-2.063-2.065 0-1.138.92-2.063 2.063-2.063 1.14 0 2.064.925 2.064 2.063 0 1.139-.925 2.065-2.064 2.065zm1.782 13.019H3.555V9h3.564v11.452zM22.225 0H1.771C.792 0 0 .774 0 1.729v20.542C0 23.227.792 24 1.771 24h20.451C23.2 24 24 23.227 24 22.271V1.729C24 .774 23.2 0 22.225 0z"/></svg></span>
    <span class="a2ui5-out-title">LinkedIn</span>
    <span class="a2ui5-out-details">Follow along — new releases, articles, and what people are building with it right now.</span>
  </a>
  <a class="a2ui5-out-card" href="https://abap2ui5.github.io/playground/" target="_blank" rel="noreferrer">
    <span class="a2ui5-out-icon"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><circle cx="12" cy="12" r="9.75" fill="none" stroke="currentColor" stroke-width="1.7"/><path d="M9.6 7.9v8.2a.5.5 0 0 0 .76.43l6.6-4.1a.5.5 0 0 0 0-.86l-6.6-4.1a.5.5 0 0 0-.76.43z" fill="currentColor"/></svg></span>
    <span class="a2ui5-out-title">Playground</span>
    <span class="a2ui5-out-details">Try it right now — write ABAP in the browser and watch it run. Nothing to install, nothing to sign up for.</span>
  </a>
</div>
