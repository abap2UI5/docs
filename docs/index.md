---
# https://vitepress.dev/reference/default-theme-home-page
layout: home
title: Home
# The one sentence a search result shows under the title, and the one a
# preview card carries. It used to be the slogan alone - four words for the
# most important page on the site, where every chapter has a sentence of its
# own. What it is, what it needs, what it costs, where it runs.
description: One ABAP class is one UI5 app - no JavaScript, no OData, no frontend project. Free and MIT licensed, installed with abapGit, from NetWeaver 7.02 to ABAP Cloud.

# THIS PAGE IS THE PROJECT'S FRONT DOOR, NOT THE MANUAL'S.
#
# It used to be the documentation's home page, reachable by clicking the mark
# and by nothing else, and it read like one: a hero, then three ways into three
# sections of this site. The bar names four places now — Home, Documentation,
# Samples, Playground — and Home is the one that has to answer "what IS this"
# for somebody who arrived from a conference talk, a LinkedIn post or a
# colleague's link, and who has not decided to read a manual yet.
#
# So the page answers, in this order: what it is (the hero), where to go next
# (the three tiles), the four answers a decision needs — security, integration,
# cost, AI — what one app looks like, with a button that RUNS it right here,
# and what is built around it. A reader who wants the manual is one word away
# in the bar; this page does not compete with it.
#
# KEEP IT SHORT. Every claim on this page is made once. The tagline, the fact
# table, the example's bullets and the AI section each used to carry their own
# copy of "no JavaScript, no OData service, no frontend project" - four times on
# one page, and the reader who needed it had it after the first. It lives in the
# tagline now, and nowhere else. abapGit is the same story: the tagline and the
# second button say it, so no card says it again.
#
# AND EVERY CARD MAKES ONE BOLD CLAIM, with plain sentences around it. The
# four used to share a formula - bold lead, explanation, "**And it ...**",
# explanation - which reads as a template by the third card. The headings
# are claims too, not labels: "Plays well with what you have" promises
# something, where "Integration" only named a topic. The last card asks
# instead, and its bold lead is the one-word answer - so the reader who
# scans only the headings still leaves with the price.
#
# THE ORDER IS AN ARGUMENT: it is safe here, it fits what you run, an agent
# can write it - and only then, to somebody already persuaded, what it costs.
# The price is the last card because it is the last question, not the first.
hero:
  # The greeting, because this is the front door and a reader who arrives from
  # a talk or a colleague's link should be met rather than pitched at. The
  # headline under it still says what the project IS, in the same words as
  # before - the two lines are a welcome and an answer, in that order.
  name: Welcome to abap2UI5
  text: Build UI5 Apps Purely in ABAP
  # ONE PARAGRAPH, NO HARD BREAK. The `\n` that used to stand after "no
  # frontend project." broke the tagline at a point the line length had nothing
  # to do with, so the first line stopped two words short of the measure and
  # the second started under a ragged edge. Two sentences that belong together
  # wrap where the column says they wrap.
  # THREE "no"s. There were four - "no RAP" stood between them - and a list of
  # negatives stops being read at the third. RAP has its own sentence in the
  # Integration card, where the point is that the two work side by side.
  tagline: "One ABAP class is one UI5 app — no JavaScript, no OData, no frontend project. Install it with abapGit and run it on anything from NetWeaver 7.02 to ABAP Cloud."
  image:
    src: /logo-hero.png
    alt: abap2UI5 Logo
    width: 200px
    height: 200px
  # Two buttons, in the order a stranger needs them: try it without installing
  # anything, then install it. There was a third, "What it is", and it opened
  # the same page as the Documentation tile directly under it - two doors into
  # one room, and a reader cannot tell them apart. The playground is first on
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

# The other three places the bar names, in the order the bar names them. Not
# three sections of this site any more: a reader on this page is choosing
# between reading, browsing and trying, and two of those are somewhere else.
features:
  - title: Documentation
    icon: <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><path fill="none" stroke="currentColor" stroke-width="1.7" stroke-linejoin="round" d="M12 6.9C10.4 5.5 8.2 4.8 5.4 4.8H2.4v12.6h3c2.8 0 5 .7 6.6 2.1 1.6-1.4 3.8-2.1 6.6-2.1h3V4.8h-3c-2.8 0-5 .7-6.6 2.1z"/><path fill="none" stroke="currentColor" stroke-width="1.7" d="M12 6.9v12.6"/></svg>
    details: Tutorial, cookbook and reference — everything from your first app to production.
    link: /get_started/about
  - title: Samples
    icon: <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><rect x="2.6" y="4.2" width="18.8" height="15.6" rx="2" fill="none" stroke="currentColor" stroke-width="1.7"/><path fill="none" stroke="currentColor" stroke-width="1.7" stroke-linecap="round" d="M2.6 9.1h18.8M8.2 9.1v10.7"/></svg>
    details: Over 700 working apps, searchable by control, by library and by what your system can run.
    link: https://abap2ui5.github.io/playground/samples/
    target: _self
  - title: Playground
    icon: <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><circle cx="12" cy="12" r="9.75" fill="none" stroke="currentColor" stroke-width="1.7"/><path d="M9.6 7.9v8.2a.5.5 0 0 0 .76.43l6.6-4.1a.5.5 0 0 0 0-.86l-6.6-4.1a.5.5 0 0 0-.76.43z" fill="currentColor"/></svg>
    details: Write ABAP in the browser and watch the app run beside it.
    link: https://abap2ui5.github.io/playground/
    target: _self
---

## Ready for Your Enterprise

**Runs inside the security you already have.** One HTTP endpoint, standard SAP
logon — your [authorizations](/configuration/authorization) and
[session handling](/configuration/security) apply unchanged. Every merge is
tested against Standard ABAP and ABAP Cloud. [Support](/resources/support) is on
GitHub and Slack.

## Plays well with what you have

**Complements UI5 freestyle and RAP — it does not replace them.** Your RAP
business objects and OData services stay where they are; abap2UI5 covers the app
that would otherwise need a frontend project of its own.

It runs where your users already are: a browser tab, a
[Fiori launchpad](/configuration/launchpad) tile, [SAP Build Work
Zone](/configuration/btp) or [SAP Mobile Start](/configuration/mobile_start) —
rendering with the UI5 your system already ships.

## Made for AI agents

**One class is one file — the whole app, for an agent to write.** It can check
its own work without an SAP system: the [linter](/advanced/linter) validates the
view, the [MCP server](/advanced/mcp_server) boots the app headless and returns
the errors and a screenshot. [The AI guide](/get_started/ai) starts with one
paragraph you paste ahead of a task.

## And what does it cost?

**Nothing.** [MIT licensed](/resources/license), commercial use included — no
licence key, no subscription, no per-user fee.

Nobody counts your users, because nothing is counting: it is a standard UI5 app
served by your own ABAP stack — ten users or ten thousand, the SAP licence you
have is the one you keep.

## Try it out now

```abap edit
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

## Around the project

<div class="a2ui5-out">
  <a class="a2ui5-out-card is-inside" href="/docs/resources/addons">
    <span class="a2ui5-out-title">Add-ons</span>
    <span class="a2ui5-out-details">Optional repositories for the things not every app needs: popups, HTTP and RFC connectors, a lock manager, table maintenance, launchpad KPIs.</span>
  </a>
  <a class="a2ui5-out-card" href="https://abap2ui5.github.io/linter/" target="_self">
    <span class="a2ui5-out-title">Linter</span>
    <span class="a2ui5-out-details">Rules that understand abap2UI5 code — view chains, bindings, events — with a rule reference you can read on its own.</span>
  </a>
  <a class="a2ui5-out-card is-inside" href="/docs/advanced/mcp_server">
    <span class="a2ui5-out-title">Tooling</span>
    <span class="a2ui5-out-details">A VS Code extension, an MCP server for AI assistants, and an app template to start from.</span>
  </a>

  <a class="a2ui5-out-card" href="https://github.com/abap2UI5/abap2UI5/" target="_blank" rel="noreferrer">
    <span class="a2ui5-out-icon"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><path fill="currentColor" d="M12 .297c-6.63 0-12 5.373-12 12 0 5.303 3.438 9.8 8.205 11.385.6.113.82-.258.82-.577 0-.285-.01-1.04-.015-2.04-3.338.724-4.042-1.61-4.042-1.61C4.422 18.07 3.633 17.7 3.633 17.7c-1.087-.744.084-.729.084-.729 1.205.084 1.838 1.236 1.838 1.236 1.07 1.835 2.809 1.305 3.495.998.108-.776.417-1.305.76-1.605-2.665-.3-5.466-1.332-5.466-5.93 0-1.31.465-2.38 1.235-3.22-.135-.303-.54-1.523.105-3.176 0 0 1.005-.322 3.3 1.23.96-.267 1.98-.399 3-.405 1.02.006 2.04.138 3 .405 2.28-1.552 3.285-1.23 3.285-1.23.645 1.653.24 2.873.12 3.176.765.84 1.23 1.91 1.23 3.22 0 4.61-2.805 5.625-5.475 5.92.42.36.81 1.096.81 2.22 0 1.606-.015 2.896-.015 3.286 0 .315.21.69.825.57C20.565 22.092 24 17.592 24 12.297c0-6.627-5.373-12-12-12"/></svg></span>
    <span class="a2ui5-out-title">Community</span>
    <span class="a2ui5-out-details">Built in the open — read the code, open an issue, send a pull request.</span>
  </a>
  <a class="a2ui5-out-card" href="https://www.linkedin.com/company/abap2ui5/" target="_blank" rel="noreferrer">
    <span class="a2ui5-out-icon"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><path fill="currentColor" d="M20.447 20.452h-3.554v-5.569c0-1.328-.027-3.037-1.852-3.037-1.853 0-2.136 1.445-2.136 2.939v5.667H9.351V9h3.414v1.561h.046c.477-.9 1.637-1.85 3.37-1.85 3.601 0 4.267 2.37 4.267 5.455v6.286zM5.337 7.433c-1.144 0-2.063-.926-2.063-2.065 0-1.138.92-2.063 2.063-2.063 1.14 0 2.064.925 2.064 2.063 0 1.139-.925 2.065-2.064 2.065zm1.782 13.019H3.555V9h3.564v11.452zM22.225 0H1.771C.792 0 0 .774 0 1.729v20.542C0 23.227.792 24 1.771 24h20.451C23.2 24 24 23.227 24 22.271V1.729C24 .774 23.2 0 22.225 0z"/></svg></span>
    <span class="a2ui5-out-title">LinkedIn</span>
    <span class="a2ui5-out-details">New releases, articles, and what people are building with it.</span>
  </a>
  <a class="a2ui5-out-card is-inside" href="/docs/resources/sponsor">
    <span class="a2ui5-out-icon"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><path fill="currentColor" d="M12 21s-7.6-4.9-9.5-9.2C1.1 8.4 3 5 6.4 5c2 0 3.4 1.1 4.3 2.3l1.3 1.7 1.3-1.7C14.2 6.1 15.6 5 17.6 5c3.4 0 5.3 3.4 3.9 6.8C19.6 16.1 12 21 12 21z"/></svg></span>
    <span class="a2ui5-out-title">Sponsor</span>
    <span class="a2ui5-out-details">Free and maintained by volunteers. If it saved your project time, here is a way to give some back.</span>
  </a>
</div>
