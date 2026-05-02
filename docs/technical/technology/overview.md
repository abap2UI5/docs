---
outline: [2, 3]
---
# Technology Overview
_From ITS to abap2UI5_

This page traces the evolution of web development in the ABAP ecosystem — from early server-rendered interfaces like ITS and Web Dynpro to modern client-side SPAs with UI5 Freestyle and RAP — and ends by comparing them to abap2UI5.

The pattern is cyclical: server-rendered (ITS, BSP, Web Dynpro) → client-side SPA (UI5 freestyle, RAP/Fiori Elements) → server-driven again (abap2UI5, "Over the Wire"). Each shift solves real problems with the previous generation. Understanding the cycle makes it easier to decide which tool fits which use case today.

For head-to-head comparisons, see **[RAP vs. abap2UI5](/technical/technology/rap)** and **[UI5 Freestyle vs. abap2UI5](/technical/technology/ui5)**.

## Internet Transaction Server
_Bringing SAP GUI to the Web (2000+) (SAP)_

The **Internet Transaction Server (ITS)** was SAP's first step toward bringing SAP GUI screens (Dynpro) to the web. ITS Mobile converts classic Dynpro screens into basic HTML pages.

- Pure **Server-Side Rendering (SSR)**: the server builds HTML for every interaction
- Optimized for basic mobile devices (e.g., warehouse scanners)
- Reuses existing Dynpro logic for web and mobile use cases
- Still used today for specific legacy use cases

## Business Server Pages
_Embedded HTML in ABAP (2001+) (SAP)_

Business Server Pages (BSP) introduced HTML-based web development inside the ABAP stack.

- Developers write HTML and JavaScript, with embedded ABAP for logic
- Also server-side rendering, but with more control over layout and styling
- Used widely in early CRM and SRM web applications
- Technically obsolete, but still present in some legacy environments

## Web Dynpro
_Structured Web Applications (2003+) (SAP)_

**Web Dynpro** introduced a component-based UI framework for web applications.

- Developers create UI definitions in ABAP (Views, Context, Controllers)
- Still **Server-Side Rendering**: UI is built on the server, rendered as HTML in the browser
- Fits transactional business applications
- More structured and modular than ITS, but limited for modern UX

## UI5 Freestyle
_Full Client-Side SPAs (2010+) (SAP)_

With the rise of smartphones and richer web experiences, SAP introduced **UI5 Freestyle**.

- Based on **JavaScript, XML Views, and UI5 Controls**
- Follows a **Single-Page Application (SPA)** architecture
- Full control over frontend behavior and look and feel
- Enables highly customized, interactive applications
- Needs dedicated frontend expertise

## RAP / Fiori Elements
_Standardized Backend-Driven SPA (2019+) (SAP)_

The **RESTful Application Programming Model (RAP)** and **Fiori Elements** standardize application development.

- Developers define **CDS annotations** in ABAP that describe UI behavior
- The UI5 Fiori Elements runtime in the browser renders the app as a **SPA**
- Simplifies UI development with predefined floorplans and templates
- Still SPA complexity (OData metadata handling, UI5 runtime in browser)

## abap2UI5
_UI5 Over-the-Wire (2023+) (Open Source)_

**abap2UI5** brings the simplicity of **Over-the-Wire** to SAP UI5 development.

- The ABAP backend defines **UI5 XML Views** and **JSON ViewModels**
- A static UI5 frontend dynamically renders these definitions
- No custom JavaScript needed
- Combines backend-driven development with UI5 flexibility
- Lines up with SAP's cloud strategy

## Direct Comparisons

- **[RAP vs. abap2UI5](/technical/technology/rap)** — architecture, communication, developer workflow.
- **[UI5 Freestyle vs. abap2UI5](/technical/technology/ui5)** — architecture, state, developer experience.

## See Also

- **[UI5 Over-the-Wire](/technical/concept)** — the architectural pattern that makes abap2UI5 possible.
- **[Behind the Scenes](/technical/how_it_all_works)** — full deep dive into the implementation.
