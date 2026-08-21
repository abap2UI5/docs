---
outline: [2, 4]
description: Build a small invoice app from scratch in eight steps — every step a complete, runnable abap2UI5 class.
---
# Walkthrough

In this tutorial we build a small invoice app from scratch and meet every
paradigm an abap2UI5 app is made of along the way: the app class and its
lifecycle, views written in ABAP, events, data binding, lists and popups.

## Preview

![The finished walkthrough app: a list of invoices with supplier and quantity, and a dialog editing the quantity of one row](/tutorials/walkthrough-preview.png)

This is the app after the last step — about a hundred lines of ABAP, no
frontend project, no OData service.

Each step is a **complete, runnable class** — the whole app as it stands at
that point, not a fragment. Press the Run button under the code to start it in
the browser, or copy the class into your system and launch it like any
abap2UI5 app (see the [Quickstart](/get_started/quickstart)). No step depends
on anything outside its own code, so you can also jump straight to the step
you are interested in.

## Steps

- **[Step 1: Hello World](/tutorials/walkthrough/step-1)** — the smallest possible app: one class, one method, one message.
- **[Step 2: A First View](/tutorials/walkthrough/step-2)** — render a UI5 view built entirely in ABAP.
- **[Step 3: Events](/tutorials/walkthrough/step-3)** — a button, a press event, and the lifecycle behind them.
- **[Step 4: Data Binding](/tutorials/walkthrough/step-4)** — an input field whose value reaches the server by itself.
- **[Step 5: List Binding](/tutorials/walkthrough/step-5)** — show an internal table as a UI5 list.
- **[Step 6: Row Events](/tutorials/walkthrough/step-6)** — react to a click on a row, and know which row it was.
- **[Step 7: Popups](/tutorials/walkthrough/step-7)** — edit a row in a dialog.
- **[Step 8: App Structure](/tutorials/walkthrough/step-8)** — refactor into the structure real apps use.

## What You Should Know

No prior abap2UI5 or UI5 experience is needed — the tutorial introduces every
concept as it appears. Basic ABAP (classes, methods, internal tables) is
assumed. If you want the framework installed in your own system first, do the
[Quickstart](/get_started/quickstart) — but the Run button works without any
installation at all.
