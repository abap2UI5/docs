---
outline: [2, 3]
---
# Behind the Scenes

_A technical deep dive into abap2UI5._

This is the entry point for developers who want to understand how abap2UI5 works under the hood: runtime view rendering, generic HTTP handling, decoupling from OData, draft-based state, and how the architecture stays small.

The deep dive is split into four parts. Each part is independent — read in order for a guided tour, or jump to the part you care about.

## The Path

### 1. Foundations

Where the architecture comes from: HTML Over the Wire, hypermedia-driven applications, the sweet spot between SPA and MPA, and the trait of UI5 that makes the whole thing possible.

→ Read **[Foundations](/technical/deep_dive/foundations)**.

### 2. The abap2UI5 Architecture

How the backend ships both the View and the data, why a single generic HTTP service serves every app, how Model and View are defined at runtime, and how this compares to RAP.

→ Read **[The abap2UI5 Architecture](/technical/deep_dive/architecture)**.

### 3. Inside an App

The single ABAP class that holds an app, the draft mechanism that survives stateless roundtrips, the initial GET request, and the small `_bind` / `_event` API that keeps view-building loosely coupled.

→ Read **[Inside an App](/technical/deep_dive/lifecycle)**.

### 4. Trade-offs and Compatibility

What abap2UI5 doesn't do well, where RAP and UI5 freestyle still win, the system footprint by the numbers, and how a single codebase covers ABAP releases from 7.02 up to ABAP Cloud.

→ Read **[Trade-offs and Compatibility](/technical/deep_dive/tradeoffs)**.

## Background

Originally published on the [SAP Community](https://community.sap.com/t5/technology-blog-posts-by-members/abap2ui5-7-technical-background-under-the-hood-of-abap2ui5/ba-p/13566459) as a single article. The deep dive has since been split into four parts here for easier navigation, but the substance is the same.

If you prefer the lighter introduction first, see the **[architecture overview](/technical/concept)** — it covers the same ideas without the implementation detail.
