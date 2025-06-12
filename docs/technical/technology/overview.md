# Technology Overview
_From ITS Mobile to abap2UI5_

This page outlines the evolution of web development in the SAP ecosystem — from early server-rendered interfaces like ITS and Web Dynpro to modern client-side SPAs with UI5 Freestyle and RAP. It also introduces the open-source framework abap2UI5, which brings a fresh take on UI5 through an Over-the-Wire approach.


### ITS & ITS Mobile 
_Bringing SAP GUI to the Web (2000+) (SAP)_

The **Internet Transaction Server (ITS)** was SAP’s first step to bring SAP GUI screens (Dynpro) to the web. ITS Mobile converts classical Dynpro screens into basic HTML pages.

- Pure **Server-Side Rendering (SSR)**: HTML is generated on the server for every interaction.
- Optimized for simple mobile devices (e.g., warehouse scanners).
- Focus on reusing existing Dynpro logic for web & mobile use cases.
- Still used today for specific legacy scenarios.

### Business Server Pages
_Embedded HTML in ABAP (2001+) (SAP)_
Business Server Pages (BSP) introduced HTML-based web development within the ABAP stack.

- Developers write HTML and JavaScript, with embedded ABAP for logic.
- Also server-side rendering, but with greater control over layout and styling.
- Used extensively in early CRM and SRM web applications.
- Technically obsolete, but still present in some legacy environments.

### Web Dynpro
_Structured Web Applications (2003+) (SAP)_

**Web Dynpro** introduced a component-based UI framework for web applications.

- UI definitions are created in ABAP (Views, Context, Controllers).
- Still **Server-Side Rendering**: UI is built on the server, rendered as HTML in the browser.
- Suitable for transactional business applications.
- More structured and modular than ITS, but limited for modern UX expectations.

### UI5 Freestyle 
_Full Client-Side SPAs (2010+) (SAP)_

With the advent of smartphones and richer web experiences, **UI5 Freestyle** was introduced.

- Based on **JavaScript, XML Views, and UI5 Controls**.
- Follows a **Single Page Application (SPA)** architecture.
- Full control over frontend behavior and look & feel.
- Enables highly customized, interactive applications.
- Requires dedicated frontend development expertise.

### RAP / Fiori Elements 
_Standardized Backend-Driven SPA (2019+) (SAP)_ 

The **RESTful Application Programming Model (RAP)** and **Fiori Elements** aim to standardize application development.

- Developers define **CDS Annotations** in ABAP to describe UI behavior.
- The UI5 Fiori Elements runtime in the browser renders the app as a **SPA**.
- Simplifies UI development by using predefined floorplans and templates.
- Still SPA complexity (OData Metadata handling, UI5 runtime in browser).

### abap2UI5 
_UI5 Over-the-Wire (2023+) (Open Source)_ 

**abap2UI5** brings the simplicity of the **Over-the-Wire** concept to SAP UI5 development.

- The ABAP backend defines **UI5 XML Views** and **JSON ViewModels**.
- A static UI5 frontend dynamically renders these definitions.
- No custom JavaScript development required.
- Combines backend-driven development with UI5 flexibility.
- Aligns with SAP's "Keep the Core Clean" strategy.
