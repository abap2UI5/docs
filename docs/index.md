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
# So the page answers, in this order: what it is (the hero), what one app
# looks like, running right here, and then where to go next (the tiles).
# The case for the enterprise, the integration and the AI story used to be
# three cards here as well - and the same three sections on the site for
# managers and in the README. They are made once now, over there; the
# tiles under the example hand the reader on.
#
# KEEP IT SHORT. Every claim on this page is made once. The tagline, the fact
# table, the example's bullets and the AI section each used to carry their own
# copy of "no JavaScript, no OData service, no frontend project" - four times on
# one page, and the reader who needed it had it after the first. It lives in the
# tagline now, and nowhere else. abapGit is the same story: the tagline and the
# second button say it, so no card says it again.
#
# AND EVERY CARD IS ONE SHORT PARAGRAPH OF PLAIN TEXT. They used to be
# two or three paragraphs each - the enterprise card alone ran to 120 words -
# and four panels of prose under three tiles read as a page that starts
# over. What a card says now is the claim, one sentence that backs it, and
# the link to the page that argues it in full.
# Nothing in the paragraph is bold: the heading carries the claim, the text
# reads as one run. The headings are claims too, not labels: "Plays well with what you have"
# promises something, where "Integration" only named a topic. The last card
# asks instead, and its first word is the one-word answer.
#
# THE ORDER IS AN ARGUMENT: it is safe here, and it fits what you run - then
# an agent can write it. The price card that closed the row is gone with the
# rest of the case for the buyer (THIS PAGE IS FOR THE DEVELOPER, below); the
# description and the license page still say MIT.
#
# THIS PAGE IS FOR THE DEVELOPER. The manual is technical; the case for the
# person who approves abap2UI5 - what it can build, how it compares with RAP,
# freestyle UI5 and commercial platforms, what it costs, who runs it - is a
# site of its own (https://abap2ui5.github.io/why/, abap2UI5/why).
# This page argues none of that a second time: the hero's second button and
# the last tile hand that reader over, once each, and every claim on a card
# links a page of the manual.
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
# EVERY CARD ENDS WITH A WAY OUT, ON A LINE OF ITS OWN: "→ More on …" in
# italics, the same shape on all three, so the cards read as one structure
# and the eye finds the link in the same place on each. The example's line
# under the code is the same shape again. The link sits on the topic, never on the word
# "here" - a reader scanning the links has to be able to tell where each one
# goes, in the same "More on …" words on all three, so the lines read as
# one. A second
# link to the same page inside one card is the "every claim once" rule broken
# with a hyperlink.
#
# NO LAST LINE OF LINKS. GitHub and LinkedIn are the two marks at the end of
# the bar on every page, as on every VitePress site.
hero:
  # The default theme's hero: the name in the accent, the claim under it.
  name: abap2UI5
  text: Build UI5 Apps Purely in ABAP
  # ONE PARAGRAPH, NO HARD BREAK. The `\n` that used to stand after "no
  # frontend project." broke the tagline at a point the line length had nothing
  # to do with, so the first line stopped two words short of the measure and
  # the second started under a ragged edge. Two sentences that belong together
  # wrap where the column says they wrap.
  # THREE "no"s. There were four - "no RAP" stood between them - and a list of
  # negatives stops being read at the third. RAP has its own sentence in the
  # Integration card, where the point is that the two work side by side.
  # THE INSTALL PATH IS IN THE FIRST SCREEN. "Install with abapGit" is the
  # second sentence: one tool, no project to set up, and the reader knows how
  # it gets onto a system before reading anything else. The price has its own
  # card below, and the tiles that used to repeat abapGit sit under the
  # example now, so the word stands once above the fold.
  tagline: "One ABAP class is one UI5 app — no JavaScript, no OData, no frontend project. Install with abapGit, and it runs on anything from NetWeaver 7.02 to ABAP Cloud."
  image:
    src: /logo-hero.webp
    alt: abap2UI5 Logo
    width: 200px
    height: 200px
  # Two buttons. The accent one opens the playground: the one thing a
  # developer who arrived here can DO in the next ten seconds, with nothing
  # installed - the bar names the place and the example below runs on this
  # page, but neither is a call to action in the first screen, and a first
  # screen with no primary action reads as a brochure. Same tab (target
  # _self), like every other way into the neighbouring site. The outlined
  # one opens "In a Nutshell", the technical picture of how the framework
  # works. The page for the person who approves abap2UI5 is the last tile,
  # not a button: this page is the developer's.
  actions:
    - theme: brand
      text: Try it in the playground
      link: https://abap2ui5.github.io/playground/
      target: _self
    - theme: alt
      text: See how it works
      link: /get_started/about

# Four things to do next, each on the page for it. The bar names the
# places; these name the tasks (see THE THREE TILES ARE TASKS above). The
# fourth hands the page over to whoever approves abap2UI5 - a site of its own,
# so the manual does not argue the case a second time. They
# are frontmatter, so VitePress draws them under the hero; the published page
# draws them UNDER THE EXAMPLE (build-site.mjs), where "what next" belongs.
features:
  - title: Build your first app
    icon: <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" aria-hidden="true"><path fill="none" stroke="currentColor" stroke-width="1.7" stroke-linejoin="round" stroke-linecap="round" d="M8 7.5 3.5 12 8 16.5M16 7.5l4.5 4.5-4.5 4.5M13.6 4.8 10.4 19.2"/></svg>
    details: Install with abapGit, run Hello World, then the twelve-step tutorial — through to transport and unit tests.
    link: /get_started/quickstart
  - title: Find a sample for your use case
    icon: <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" aria-hidden="true"><rect x="2.6" y="4.2" width="18.8" height="15.6" rx="2" fill="none" stroke="currentColor" stroke-width="1.7"/><path fill="none" stroke="currentColor" stroke-width="1.7" stroke-linecap="round" d="M2.6 9.1h18.8M8.2 9.1v10.7"/></svg>
    details: Hundreds of working apps, searchable by control, by library and by the UI5 release your system runs.
    link: https://abap2ui5.github.io/samples-site/
    target: _self
  - title: Take an app to production
    icon: <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" aria-hidden="true"><path fill="none" stroke="currentColor" stroke-width="1.7" stroke-linejoin="round" stroke-linecap="round" d="M3.5 19.5V9l5.5 3.5V9l5.5 3.5V9l6 3.5v7zM3.5 19.5h17M6 4.5h3v7.5"/></svg>
    details: Security, authorizations, the launchpad tile, the transport — what a go-live needs, on one page.
    link: /configuration/productive_usage
  - title: Send it to whoever decides
    icon: <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" aria-hidden="true"><path fill="none" stroke="currentColor" stroke-width="1.7" stroke-linejoin="round" stroke-linecap="round" d="M4 12.5 9 17.5 20 6.5"/></svg>
    details: What abap2UI5 means for an organization — what it can build, how it compares, what it costs — for the person who approves it.
    link: https://abap2ui5.github.io/why/
    target: _self
---

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
                      )->a( n = `press`   v = client->_event( val = `EDIT` arg = `${PRODUCT}` ) ).

      client->view_display( view->stringify( ) ).

    ELSEIF client->check_on_event( `EDIT` ).

      s_edit = VALUE #( t_invoices[ product = client->get_event_arg( ) ] OPTIONAL ).

      DATA(popup) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `FragmentDefinition` ns = `core`
              )->a( n = `xmlns`      v = `sap.m`
              )->a( n = `xmlns:core` v = `sap.ui.core`

              )->ele( `Dialog`
                  )->a( n = `title` t = |Edit { s_edit-product }|

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

→ *The [Tutorial](/tutorials/) builds this app in twelve steps, through to
transport and unit tests.*

