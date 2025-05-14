## Technology Overview

### abap2UI5: Over-the-Wire for SAP (2023+)
abap2UI5 applies the Over-the-Wire principle to SAP UI5 apps. The backend (ABAP) defines UI5 XML Views and ViewModels, which are sent to the browser. A generic, static UI5 app dynamically renders this content.

- **Frontend stays generic & static**
- **No custom JavaScript development needed**
- Perfect for CRUD-heavy business applications
- Inspired by modern Over-the-Wire frameworks like Hotwire, Livewire
- Addresses SAP's "Clean Core" approach by keeping custom code backend-driven

### RAP / Fiori Elements: Backend-driven SPA (2019+)
The **RESTful Application Programming Model (RAP)** standardizes how SAP applications expose data and services via OData V4. **Fiori Elements** apps use these services to render UIs client-side in the browser.

- Developers define CDS Annotations in ABAP
- UI is built dynamically by the UI5 Fiori Elements runtime (SPA)
- Suitable for standard apps with CRUD patterns
- Reduces custom UI development, but still SPA complexity (OData Metadata handling, UI5 runtime)

### UI5 Freestyle: Custom SPA (2010+)
UI5 Freestyle apps offer full control over the frontend. Developers build custom UI5 applications using JavaScript, XML Views, and UI5 Controls.

- Allows highly customized, interactive UIs
- Full SPA architecture in the browser
- Requires frontend expertise (JavaScript, UI5 skills)
- More flexible, but also more complex in development & maintenance

### Web Dynpro ABAP: Server-Side Rendering (2003+)
Web Dynpro ABAP is SAP's traditional web UI framework. It renders HTML on the server and sends complete pages to the browser for each interaction.

- Component-based UI framework fully implemented in ABAP
- Classic Server-Side Rendering paradigm (SSR)
- Suitable for transactional SAP applications
- Limited flexibility for modern UI patterns, but robust and widely used

### ITS Mobile: Legacy SSR for Mobile Devices (2000+)
**Internet Transaction Server (ITS) Mobile** converts classical SAP Dynpro (SAP GUI) screens into HTML pages for mobile browsers.

- Simplifies legacy app mobilization (e.g., warehouse scanners)
- Pure server-side HTML rendering (SSR)
- Very basic UI, optimized for keyboard-based navigation
- Still relevant for niche use cases with rugged devices

