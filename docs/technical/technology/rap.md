---
outline: [2, 4]
---
# RAP vs. abap2UI5
_Architecture, Communication & Developer Experience_

This page provides a technical comparison between **RAP** and **abap2UI5**, focusing on architecture, state management, developer workflow, and communication models.

### 1. Architectural Paradigms

| Aspect            | RAP (Fiori Elements)                                             | abap2UI5                                                   |
|-------------------|------------------------------------------------------------------|------------------------------------------------------------|
| **Backend Stack** | CDS Views, Behavior Definitions, OData V4 services               | ABAP Classes generating XML Views and JSON ViewModels     |
| **Frontend Stack**| UI5 Fiori Elements SPA                                           | Static UI5 Freestyle App                            |
| **Rendering**     | Client interprets metadata and builds UI dynamically             | XML View sent by backend, rendered in frontend      |
| **UI Definition** | Annotations in CDS & metadata                                    | XML Views created directly in ABAP                         |
| **Communication** | OData V4 (metadata, data, actions)                               | Simple HTTP requests (Over-the-Wire)                      |
| **Runtime Control**| Logic partly changeable via RAP Implementation                  | Full control over UI and logic in backend                 |
| **Model**         | Defined at design time via CDS                                   | Defined at design time or runtime via internal tables     |
| **Drafts**        | Managed via RAP drafts on model level                            | Managed via serialization on app level                    |

### 2. Developer Workflow

| Aspect                     | RAP (Fiori Elements)                                 | abap2UI5                                               |
|----------------------------|------------------------------------------------------|--------------------------------------------------------|
| **Languages/Artifacts**    | CDS, BDEF, UI annotations, UI5 app                    | ABAP class for both View and logic                     |
| **Frontend Deployment**    | UI5 Fiori Elements runtime app deployed separately    | Shared static UI5 Shell (no app-specific deployment)   |
| **Tooling Requirements**   | ADT, Fiori Tools, metadata layers                     | Any ABAP IDE (including SE80), no additional tools     |
| **Transport**              | Separate transport for backend and frontend           | Single backend deployment via transport or abapGit     |
| **Development Style**      | Declarative, metadata-driven                          | Programmatic, ABAP-centric                             |
| **Complexity**             | High: multiple layers & technologies                  | Low: one language, one layer                           |

### 3. Client–Server Communication

#### RAP

```plaintext
Browser (Fiori Elements SPA)
  ├──> OData $metadata request
  ├──> OData entity requests
  ├──> Renders UI from metadata
  ├──> Triggers OData actions (CRUD, validation)
Backend (RAP Services)
  └──> Processes requests, returns data/actions
```

#### abap2UI5

```plaintext
Browser (Static UI5 Shell)
  ├──> HTTP request: Load XML View + ViewModel
  ├──> Renders UI5 controls as defined by backend
  ├──> Sends event requests on interaction via HTTP
Backend (ABAP Class)
  └──> Processes event, updates ViewModel, returns changes
```

### 4. Flexibility & Runtime Capabilities

| Aspect                    | RAP (Fiori Elements)                        | abap2UI5                                 |
|---------------------------|---------------------------------------------|------------------------------------------|
| **UI Customization**      | Limited to what annotations support         | Fully flexible via ABAP logic            |
| **Runtime Model Dynamics**| Static, metadata-bound                      | Dynamic via RTTI and runtime logic       |
| **Use Case Fit**          | Ideal for standard CRUD applications        | Suitable for dynamic, backend-driven UIs |
| **Learning Curve**        | Steep (new concepts, multiple layers)       | Flat (ABAP-only, no metadata tooling)    |

### 5. Cloud Readiness & Compliance

| Feature                    | RAP                        | abap2UI5                     |
|----------------------------|-----------------------------|-------------------------------|
| **ABAP Cloud Compliant**   | ✅ Yes                      | ✅ Yes                        |
| **CDS/OData Dependency**   | ✅ Required                 | ❌ Not used                   |
| **Clean Core Compliance**  | ✅ Yes                      | ✅ Yes                        |
| **Runtime Flexibility**    | ❌ Rigid, design-time focus | ✅ Fully runtime-capable      |

> 🔒 **Both frameworks are cloud-ready and clean-core compliant.** abap2UI5 achieves this **without CDS or OData**, relying solely on released ABAP APIs.

### Conclusion

- **RAP (Fiori Elements)** is best suited for standardized, metadata-driven applications using CDS, OData, and annotations.
- **abap2UI5** provides runtime flexibility, backend control, and lower complexity — ideal for dynamic UIs and fast iterations.

### Summary Table

| Category                 | RAP (Fiori Elements)           | abap2UI5                          |
|--------------------------|-------------------------------|-----------------------------------|
| UI Architecture          | Metadata-based SPA            | Backend-driven Over-the-Wire     |
| Data & Actions           | OData V4                      | Simple HTTP                      |
| UI Customization         | Limited by annotations        | Fully dynamic                     |
| Tooling                  | ADT, Fiori Tools              | Any ABAP IDE                     |
| Cloud Readiness          | ✅ Yes                        | ✅ Yes                            |
| Clean Core               | ✅ Yes                        | ✅ Yes                            |
| Use Case Fit             | Standardized CRUD Apps        | Dynamic, backend-controlled UIs  |
| Learning Curve           | High                          | Low                              |
| Deployment               | Split backend/frontend        | Pure ABAP backend class        |
