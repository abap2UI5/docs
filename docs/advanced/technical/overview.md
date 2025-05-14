# Technology Overview: From ITS to abap2UI5

This page explains how web development in the SAP environment has evolved over the years — from classic server-side rendering approaches like ITS and Web Dynpro to client-side SPAs with UI5 Freestyle and RAP, while contextualizing the open-source framework abap2UI5 and its Over-the-Wire paradigm.

### ITS Mobile (2000+) (SAP)
_Bringing SAP GUI to the Web_

The **Internet Transaction Server (ITS)** was SAP’s first step to bring SAP GUI screens (Dynpro) to the web. ITS Mobile converts classical Dynpro screens into basic HTML pages.

- Pure **Server-Side Rendering (SSR)**: HTML is generated on the server for every interaction.
- Optimized for simple mobile devices (e.g., warehouse scanners).
- Focus on reusing existing Dynpro logic for web & mobile use cases.
- Still used today for specific legacy scenarios.

### Web Dynpro ABAP (2003+) (SAP)
_Structured Web Applications_

**Web Dynpro ABAP** introduced a component-based UI framework for web applications.

- UI definitions are created in ABAP (Views, Context, Controllers).
- Still **Server-Side Rendering**: UI is built on the server, rendered as HTML in the browser.
- Suitable for transactional business applications.
- More structured and modular than ITS, but limited for modern UX expectations.

### UI5 Freestyle (2010+) (SAP)
_Full Client-Side SPAs_

With the advent of smartphones and richer web experiences, **UI5 Freestyle** was introduced.

- Based on **JavaScript, XML Views, and UI5 Controls**.
- Follows a **Single Page Application (SPA)** architecture.
- Full control over frontend behavior and look & feel.
- Enables highly customized, interactive applications.
- Requires dedicated frontend development expertise.

### RAP / Fiori Elements (2019+) (SAP)
_Standardized Backend-Driven SPA_ 

The **RESTful Application Programming Model (RAP)** and **Fiori Elements** aim to standardize application development.

- Developers define **CDS Annotations** in ABAP to describe UI behavior.
- The UI5 Fiori Elements runtime in the browser renders the app as a **SPA**.
- Simplifies UI development by using predefined floorplans and templates.
- Still SPA complexity (OData Metadata handling, UI5 runtime in browser).

### abap2UI5 (2023+) (Open Soure)
_UI5 Over-the-Wire_ 

**abap2UI5** brings the simplicity of the **Over-the-Wire** concept to SAP UI5 development.

- The ABAP backend defines **UI5 XML Views** and **JSON ViewModels**.
- A static UI5 frontend dynamically renders these definitions.
- No custom JavaScript development required.
- Combines backend-driven development with UI5 flexibility.
- Aligns with SAP's "Keep the Core Clean" strategy.

### Summary

Why abap2UI5?
- **ITS & Web Dynpro**: Full SSR, but limited UX flexibility
- **UI5 Freestyle**: Richer UIs, but introduce SPA complexity
- **RAP**: Limited Flexibility, UI based on CDS and UI Annotations
- **abap2UI5**: Bridges the gap — backend-driven like ITS/Web Dynpro, but with modern UI5 rendering & Over-the-Wire efficiency.

For typical business applications (forms, tables, dashboards), abap2UI5 offers a pragmatic and maintainable alternative to SPA-heavy approaches — fully in line with SAP UI5 and clean core principles.
