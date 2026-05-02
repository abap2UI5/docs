---
outline: [2, 3]
---
# Foundations

_Part 1 of the architectural deep dive._

Before looking at how abap2UI5 works internally, this page covers the ideas that inspired its architecture: HTML Over the Wire, hypermedia-driven applications, and how UI5 itself is structured.

The pattern is older than the framework. Frameworks like Phoenix LiveView (2018) and Laravel Livewire (2019) reintroduced server-driven UI updates years before abap2UI5 carried the same idea into UI5. Understanding that lineage makes the rest of the deep dive easier to follow.

## HTML Over the Wire

The concept of **"HTML Over the Wire"** inspired one of the core ideas behind abap2UI5. This approach renders HTML directly on the server and sends it to the browser — with no need for JSON, client-side MVC frameworks, bundling, or transpiling pipelines.

> You can write fast, modern, responsive web applications by generating your HTML on the server, and delivering that directly to the browser. You don't need JSON as an in-between format. You don't need client-side MVC frameworks. You don't need complicated bundling and transpiling pipelines. But you do need to think different. [...]

> This is what HTML Over The Wire is all about. It's a celebration of the simplicity in HTML as the format for carrying data and presentation together, whether this is delivered on the first load or with subsequent dynamic updates.

The SAP community introduced this idea with examples that use the JavaScript library **htmx** to build Fiori-like apps. Unlike typical SPAs where state and logic live on the frontend, the **HTML Over the Wire** principle keeps all app logic and state on the server.

After the initial page load, the server pushes small HTML fragments asynchronously over AJAX to update parts of the page — avoiding full reloads.

<img width="400" alt="HTML 'Over the Wire' Lifecycle" src="/img/a9fde24a-c572-4e5c-b203-59a0667b9931.png" />

_HTML "Over the Wire" Lifecycle [(Source)](https://community.sap.com/t5/technology-blog-posts-by-members/fiori-like-web-app-development-in-pure-abap-with-htmx-and-fundamental/ba-p/13500763)_

This approach contrasts with the common separation of concerns, where HTML, CSS, and JavaScript are handled separately on the frontend while the backend only delivers data.

## Hypermedia-Driven Apps

This concept evolves into a **Hypermedia-Driven Application (HDA)**. In HDAs, the browser focuses only on rendering HTML, CSS, and JavaScript without knowing the app's state. All logic lives on the server.

By contrast, SPAs define all routes and actions up front on the frontend, needing a full rebuild for any change. The illustration below compares MPAs, SPAs, and HDAs:

<img width="600" alt="MPA vs. SPA vs. HDA" src="/img/8117dc10-f0ba-4c52-9d1d-6b9d0986401d.png" />

_MPA vs. SPA vs. HDA [(Source)](https://craftcms.com/events/dot-all-2022/sessions/a-practical-guide-to-html-over-the-wire)_

## Rethinking Separation of Concerns

Unlike traditional architectures, HDAs don't enforce strict separation of CSS, JavaScript, and HTML. The backend builds the UI and handles program flow — much like SAP GUI apps did in the past. This unified approach simplifies customization and maintenance.

## The Sweet Spot Between SPA and MPA

Frameworks like Phoenix LiveView (2018) and Laravel Livewire (2019) were among the first to adopt this principle. Tools like htmx, Hotwire, and Unpoly followed, aiming to cut complexity while keeping high UI fidelity. These frameworks aim for a "sweet spot" between SPA and MPA architectures:

<img width="600" alt="'Sweet Spot' between SPA and MPA" src="/img/41af4a41-829e-4289-82f5-18ee7408054b.png" />

_"Sweet Spot" between SPA and MPA_

A recommended video offers a good introduction to these ideas.

## How UI5 Fits In

UI5 apps usually follow an SPA architecture: the backend delivers data via OData, while all logic and UI rendering run on the frontend. But one trait is worth a closer look — **how UI5 creates views**. UI5 renders each HTML output from an XML View (ignoring the older HTML/JS/JSON Views), with its matching data from the server. The frontend keeps the view as part of the app:

<img width="600" alt="UI5 normally - ABAP delivers only Data" src="/img/3b2a884e-e899-4b60-8a95-79b418f33657.png" />

_UI5 normally — ABAP delivers only Data._

This is the hook abap2UI5 uses. If the XML View is just a string, it doesn't have to live on the frontend — it can be sent from the backend on every request.

## What's Next

That's exactly what abap2UI5 does. The next part of the deep dive shows how the framework moves the view to the backend, how a single generic HTTP service serves every app, and why the View and Model can be defined entirely at runtime.

→ Continue with **[The abap2UI5 Architecture](/technical/deep_dive/architecture)**.
