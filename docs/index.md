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
# So the page answers, in this order: what it is (the hero), the four answers
# a decision needs — security, integration, AI, cost — what one app looks
# like, running right here, and then where to go next (the three tiles).
# Under it, one line of where the project lives. A reader who wants the
# manual is one word away in the bar; this page does not compete with it.
#
# KEEP IT SHORT. Every claim on this page is made once. The tagline, the fact
# table, the example's bullets and the AI section each used to carry their own
# copy of "no JavaScript, no OData service, no frontend project" - four times on
# one page, and the reader who needed it had it after the first. It lives in the
# tagline now, and nowhere else. abapGit is the same story: the tagline and the
# second button say it, so no card says it again.
#
# AND EVERY CARD IS ONE BOLD CLAIM AND ONE PLAIN SENTENCE. They used to be
# two or three paragraphs each - the enterprise card alone ran to 120 words -
# and four panels of prose under three tiles read as a page that starts
# over. What a card says now is the claim, one sentence that backs it, and
# the link to the page that argues it in full: the About page carries the
# lifecycle, the exit path and the support model, and carried them before.
# The headings are claims too, not labels: "Plays well with what you have"
# promises something, where "Integration" only named a topic. The last card
# asks instead, and its bold lead is the one-word answer - so the reader who
# scans only the headings still leaves with the price.
#
# THE ORDER IS AN ARGUMENT: it is safe here, and it fits what you run - then
# an agent can write it, and it costs nothing. The price is the last card
# because it is the closing line: a reader who has taken the first three
# reaches "Nothing." as the answer to the question the cards have built up
# to. It is not hidden for being last - the tagline says "free, MIT
# licensed" in the first screen, and the cards are one sentence each now,
# so all four stand on one desk screen.
#
# THE CARDS ANSWER A BUYER, not only a developer - somebody who would otherwise
# license a low-code platform, and who asks what a developer does not: who is
# behind this, what happens when nobody is, how an app is governed, what it is
# not for. The answers - lifecycle (transports, ATC, ABAP Unit), exit path (a
# handful of classes and one table), versioned releases, support as it is
# (the community's, no vendor SLA) - stand on the About page, and each card
# links the section that carries them. References by name and a list of what
# this is NOT for were tried here and taken out again by the maintainer: the
# limits stay on the About page, the references on Who Uses abap2UI5.
#
# THIS PAGE IS FOR THE MANAGER. A developer has usually found the project by
# another path - a talk, a sample, the playground - and the person this page
# has to convince is the one who decides whether it may be used. So nothing
# here is arranged for a developer's reading order: the cards come before the
# code. What a developer gets anyway, because it costs the manager nothing,
# is the example itself - there is no paragraph in front of it any more; the
# heading and the running app say what it is - one way out behind it (the
# tutorial, through to transport and unit tests), and the UI5 releases in
# the integration card. The daily tools
# - ADT, the ABAP debugger, ABAP Unit, the linter, the MCP server - are on
# the Tooling page, one click into the manual.
#
# THE THREE TILES ARE TASKS, NOT PLACES. They used to say Documentation,
# Samples, Playground - the same three words, in the same order, with the
# same marks, 44px under the bar that already says them. A tile that repeats
# the row above it is a second copy to keep in step, which is the reason the
# old Guide dropdown was taken out of the bar. What a reader on this page is
# choosing between is what to DO next: build a first app, find a sample for
# the use case in front of them, take an app to production. Each tile names
# the task and lands on the page for it; the bar goes on naming the places.
#
# AND THEY STAND UNDER THE EXAMPLE, not under the hero. Under the hero they
# were the second row of the first screen - two buttons, then three tiles,
# five calls to action before a single claim was made - and "what to do
# next" is a question a reader asks after the case, not before it. So the
# published page draws them after the running app: the argument, the proof,
# then the three doors. The frontmatter is where they are written because
# VitePress, the second opinion, knows no other place for them.
#
# THE EXAMPLE IS A REAL APP, not a greeting: the tutorial's finished class - a
# table of invoices with an edit dialog, a date picker, save and a toast - so
# that the first thing that runs on this page looks like the thing a reader
# would build. There is deliberately NO screenshot: the app itself runs here,
# and a picture of one beside it would be the same thing twice.
#
# TWO COLUMNS, not the tutorial's four. Product, supplier, quantity and
# delivery date fit a page; the panel the app runs in on THIS page is half of
# one, and at that width every heading wrapped to two lines and each row read
# as a grid of fragments - the example that is here to look like a real app
# looked like a broken one. What the paragraph above asks the reader to do
# needs the product and the date it edits, so the other two are out of the
# table and out of the class with it: a field a reader sees typed and filled
# but never displayed is a question this example is not here to answer.
#
# EVERY CARD ENDS WITH A WAY OUT, IN ITS LAST SENTENCE. Each card used to
# close on its own "→ More on …" line in italics - four extra lines on the
# page, every one of them saying what the sentence above it could say. The
# example keeps its line: the tutorial is the way out of the whole page, not
# of a card. The link sits on the topic, never on the word
# "here" - a reader scanning the links has to be able to tell where each one
# goes. The cost card's is the calculator - a page of sliders for users,
# systems, apps and support tiers whose every line comes to zero - and not
# the license, which is already linked where "MIT licensed" stands. A second
# link to the same page inside one card is the "every claim once" rule broken
# with a hyperlink.
#
# THE LAST LINE IS A LINE, NOT A ROW OF CARDS. "Around the project" used to
# be five cards - Add-ons, Tooling, Community, LinkedIn, Sponsor - carrying
# more words between them than the cost card, a whole screen under the
# example. Add-ons and Tooling are pages of the manual and stood twice (the
# AI card already names the linter and the MCP server); the repositories are
# in the bar's menu. What is left is where the project lives outside this
# site - the code, the news, the way to give back - and that is one line.
hero:
  # No greeting line. "Welcome to abap2UI5" stood over the headline in the
  # accent at 20px, and read as a second headline - two lines to take in
  # before the one that says what the project is, and a welcome tells a
  # stranger nothing they came for. The tab and the preview card still carry
  # the name; the first thing on the page is the claim.
  text: Build UI5 Apps Purely in ABAP
  # ONE PARAGRAPH, NO HARD BREAK. The `\n` that used to stand after "no
  # frontend project." broke the tagline at a point the line length had nothing
  # to do with, so the first line stopped two words short of the measure and
  # the second started under a ragged edge. Two sentences that belong together
  # wrap where the column says they wrap.
  # THREE "no"s. There were four - "no RAP" stood between them - and a list of
  # negatives stops being read at the third. RAP has its own sentence in the
  # Integration card, where the point is that the two work side by side.
  # THE PRICE IS IN THE FIRST SCREEN. "Free, MIT licensed" replaced "Install it
  # with abapGit" here: abapGit stood three times in the first screen (the
  # tagline, the second button, the Documentation tile), and the price stood
  # nowhere above the fold. The install path is the first tile now.
  tagline: "One ABAP class is one UI5 app — no JavaScript, no OData, no frontend project. Free and MIT licensed, and it runs on anything from NetWeaver 7.02 to ABAP Cloud."
  image:
    src: /logo-hero.webp
    alt: abap2UI5 Logo
    width: 200px
    height: 200px
  # Two buttons, in the order a stranger needs them: try it without installing
  # anything, then read how it works. The second used to be "Install with
  # abapGit" - the third mention of abapGit in the first screen, and a step
  # the manager this page addresses is not taking; installing is the first
  # tile below. The playground is first on purpose — it is the one claim on
  # this page a reader can check in ten seconds, and it costs them nothing.
  # It is an absolute URL, so VitePress draws it as an external link and
  # gives it a `target` of its own, which is also what keeps this site's
  # router off a neighbouring deployment (scripts/lib/cross-site.mjs).
  actions:
    - theme: brand
      text: Try it in the browser
      link: https://abap2ui5.github.io/playground/
    - theme: alt
      text: How it works
      link: /get_started/about#how-it-works

# Three things to do next, each on the page for it. The bar names the
# places; these name the tasks (see THE THREE TILES ARE TASKS above). They
# are frontmatter, so VitePress draws them under the hero; the published page
# draws them UNDER THE EXAMPLE (build-site.mjs), where "what next" belongs.
features:
  - title: Build your first app
    icon: <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><path fill="none" stroke="currentColor" stroke-width="1.7" stroke-linejoin="round" stroke-linecap="round" d="M8 7.5 3.5 12 8 16.5M16 7.5l4.5 4.5-4.5 4.5M13.6 4.8 10.4 19.2"/></svg>
    details: Install with abapGit, run Hello World, then the twelve-step tutorial — through to transport and unit tests.
    link: /get_started/quickstart
  - title: Find a sample for your use case
    icon: <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><rect x="2.6" y="4.2" width="18.8" height="15.6" rx="2" fill="none" stroke="currentColor" stroke-width="1.7"/><path fill="none" stroke="currentColor" stroke-width="1.7" stroke-linecap="round" d="M2.6 9.1h18.8M8.2 9.1v10.7"/></svg>
    details: Over 700 working apps, searchable by control, by library and by the UI5 release your system runs.
    link: https://abap2ui5.github.io/playground/samples/
    target: _self
  - title: Take an app to production
    icon: <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><path fill="none" stroke="currentColor" stroke-width="1.7" stroke-linejoin="round" stroke-linecap="round" d="M4 17.5h16M6.5 17.5V9.8l5.5-4.3 5.5 4.3v7.7M10 17.5v-4.2h4v4.2"/></svg>
    details: Security, authorizations, the launchpad tile, the transport — what a go-live needs, on one page.
    link: /configuration/productive_usage
---

## Ready for your enterprise

**Runs inside the security you already have.** One HTTP endpoint, standard SAP
logon, your own [authorizations](/configuration/authorization) — and an app is
an ABAP class, so transports, ATC and ABAP Unit apply as to everything else
you ship. Support is the community's, on GitHub and Slack; more on
[Enterprise Readiness](/get_started/about#enterprise-ready).

## Plays well with what you have

**Complements UI5 freestyle and RAP — it does not replace them, and lives
right next to your existing UI5 solutions.** It runs in a
browser tab, a [Fiori launchpad](/configuration/launchpad) tile or SAP Build
Work Zone, with the UI5 your system ships, 1.71 to 2.x, and without internet
access; more on [Integration](/get_started/about#where-it-fits).

## Made for AI agents

**One class is one file — the whole app, for an agent to write.** The
[linter](/advanced/linter) and the [MCP server](/advanced/mcp_server) let it
check its own work without an SAP system; more on
[Developing with AI](/get_started/ai).

## And what does it cost?

**Nothing.** [MIT licensed](/resources/license), commercial use included — no
license key, no subscription, no per-user fee, and no BTP required. Ten users
or ten thousand, the SAP license you have is the one you keep; run your own
numbers through the [Cost Calculator](/resources/cost_calculator).

## Try it out now

```abap edit
CLASS zcl_app_invoices DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_invoice,
        product       TYPE string,
        delivery_date TYPE string,
      END OF ty_s_invoice.

    DATA t_invoices TYPE STANDARD TABLE OF ty_s_invoice WITH EMPTY KEY.
    DATA s_edit     TYPE ty_s_invoice.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_app_invoices IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      t_invoices = VALUE #(
          ( product = `Pineapple`    delivery_date = `2026-07-15` )
          ( product = `Milk`         delivery_date = `2026-07-20` )
          ( product = `Canned Beans` delivery_date = `2026-08-01` )
          ( product = `Salad`        delivery_date = `2026-08-10` )
          ( product = `Bread`        delivery_date = `2026-08-12` ) ).

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc` ).

      DATA(tab) = view->ele( `Shell`
          )->ele( `Page`
              )->a( n = `title` v = `Invoices`

              )->ele( `Table`
                  )->a( n = `headerText` v = `Invoices`
                  )->a( n = `items`      v = client->_bind( t_invoices ) ).

      tab->ele( `columns`

          )->ele( `Column`
              )->tag( `Text`
                  )->a( n = `text` v = `Product`

          )->end(
          )->ele( `Column`
              )->tag( `Text`
                  )->a( n = `text` v = `Delivery Date`

          )->end(
          )->ele( `Column`
              )->a( n = `width` v = `10%` ).

      tab->ele( `items`
          )->ele( `ColumnListItem`
              )->ele( `cells`

                  )->tag( `Text`
                      )->a( n = `text` v = `{PRODUCT}`
                  )->tag( `Text`
                      )->a( n = `text` v = `{DELIVERY_DATE}`
                  )->tag( `Button`
                      )->a( n = `icon`    v = `sap-icon://edit`
                      )->a( n = `tooltip` v = `Edit delivery date`
                      )->a( n = `press`   v = client->_event( val   = `EDIT`
                                                              t_arg = VALUE #( ( `${PRODUCT}` ) ) ) ).

      client->view_display( view->stringify( ) ).

    ELSEIF client->check_on_event( `EDIT` ).

      s_edit = VALUE #( t_invoices[ product = client->get_event_arg( ) ] OPTIONAL ).

      DATA(popup) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `FragmentDefinition` ns = `core`
              )->a( n = `xmlns`      v = `sap.m`
              )->a( n = `xmlns:core` v = `sap.ui.core`

              )->ele( `Dialog`
                  )->a( n = `title` v = |Edit { s_edit-product }|

                  )->ele( `content`

                      )->tag( `Label`
                          )->a( n = `text` v = `Delivery Date`
                      )->tag( `DatePicker`
                          )->a( n = `value`       v = client->_bind( s_edit-delivery_date )
                          )->a( n = `valueFormat` v = `yyyy-MM-dd`

                  )->end(

                  )->ele( `buttons`

                      )->tag( `Button`
                          )->a( n = `text`  v = `Cancel`
                          )->a( n = `press` v = client->_event( `CANCEL` )
                      )->tag( `Button`
                          )->a( n = `text`  v = `Save`
                          )->a( n = `press` v = client->_event( `SAVE` )
                          )->a( n = `type`  v = `Emphasized` ).

      client->popup_display( popup->stringify( ) ).

    ELSEIF client->check_on_event( `SAVE` ).

      t_invoices[ product = s_edit-product ]-delivery_date = s_edit-delivery_date.
      client->popup_destroy( ).
      client->message_toast_display( |{ s_edit-product } updated.| ).

    ELSEIF client->check_on_event( `CANCEL` ).

      client->popup_destroy( ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

→ *The [Tutorial](/tutorials/walkthrough/) builds this app in twelve steps, through to transport and unit tests. It runs with the UI5 and the ABAP stack you already have.*

<p class="a2ui5-links">
  <a href="https://github.com/abap2UI5/abap2UI5/" target="_blank" rel="noreferrer"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><path fill="currentColor" d="M12 .297c-6.63 0-12 5.373-12 12 0 5.303 3.438 9.8 8.205 11.385.6.113.82-.258.82-.577 0-.285-.01-1.04-.015-2.04-3.338.724-4.042-1.61-4.042-1.61C4.422 18.07 3.633 17.7 3.633 17.7c-1.087-.744.084-.729.084-.729 1.205.084 1.838 1.236 1.838 1.236 1.07 1.835 2.809 1.305 3.495.998.108-.776.417-1.305.76-1.605-2.665-.3-5.466-1.332-5.466-5.93 0-1.31.465-2.38 1.235-3.22-.135-.303-.54-1.523.105-3.176 0 0 1.005-.322 3.3 1.23.96-.267 1.98-.399 3-.405 1.02.006 2.04.138 3 .405 2.28-1.552 3.285-1.23 3.285-1.23.645 1.653.24 2.873.12 3.176.765.84 1.23 1.91 1.23 3.22 0 4.61-2.805 5.625-5.475 5.92.42.36.81 1.096.81 2.22 0 1.606-.015 2.896-.015 3.286 0 .315.21.69.825.57C20.565 22.092 24 17.592 24 12.297c0-6.627-5.373-12-12-12"/></svg>GitHub</a>
  <a href="https://www.linkedin.com/company/abap2ui5/" target="_blank" rel="noreferrer"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><path fill="currentColor" d="M20.447 20.452h-3.554v-5.569c0-1.328-.027-3.037-1.852-3.037-1.853 0-2.136 1.445-2.136 2.939v5.667H9.351V9h3.414v1.561h.046c.477-.9 1.637-1.85 3.37-1.85 3.601 0 4.267 2.37 4.267 5.455v6.286zM5.337 7.433c-1.144 0-2.063-.926-2.063-2.065 0-1.138.92-2.063 2.063-2.063 1.14 0 2.064.925 2.064 2.063 0 1.139-.925 2.065-2.064 2.065zm1.782 13.019H3.555V9h3.564v11.452zM22.225 0H1.771C.792 0 0 .774 0 1.729v20.542C0 23.227.792 24 1.771 24h20.451C23.2 24 24 23.227 24 22.271V1.729C24 .774 23.2 0 22.225 0z"/></svg>LinkedIn</a>
  <a href="/docs/resources/sponsor"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><path fill="currentColor" d="M12 21s-7.6-4.9-9.5-9.2C1.1 8.4 3 5 6.4 5c2 0 3.4 1.1 4.3 2.3l1.3 1.7 1.3-1.7C14.2 6.1 15.6 5 17.6 5c3.4 0 5.3 3.4 3.9 6.8C19.6 16.1 12 21 12 21z"/></svg>Sponsor</a>
</p>
