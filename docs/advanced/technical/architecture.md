# Architecture: UI-Driven-Design?


abap2UI5 introduces a pivotal change: the backend also sends the view. This shifts the frontend’s role towards an HDA, displaying views and data received from the server:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/9717f500-c0de-4428-a996-11fc131c073c" />

"UI5 Over the Wire" - ABAP delivers Data & View together

Despite still relying on frontend HTML rendering, all necessary information (view & data) is now retrieved via AJAX from the backend. As a result, the UI5 app remains a SPA, but its role is now reduced to that of a HDA, which is responsible solely for displaying the view and its data:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/17a3a301-b698-4704-9cbc-43798c5bd600" />

UI5 app downgraded to an HDA - Displaying Data & View received from the server

This means that the frontend app is not aware of what it is currently displaying (whether it's a table, list or input) and neither is it aware of what actions will be taken next. The app logic remains completely on the server and the frontend app is just a small HDA transmitted with the first request:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/2c9f8dc1-c6d8-4e93-80a2-b50bfc1d5ec1" />

"UI5 Over the Wire" - Server to Client Communication

The HDA displays the view with its data and sends back each event to the server for determination of the next action and output. This process is somewhat similar to the PAI/PBO process used in former SAP GUI apps:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/3b464d0b-19fd-400c-a7e4-3eec893f7724" />

UI5 vs. "UI5 Over the Wire" - Communication

We use an AJAX roundtrip logic similar to "HTML Over the Wire" approaches, but in this case, we cannot send HTML directly. Instead, we send a View combined with its Data. This results in a concept that we could refer to as "UI5-View Over the Wire".

##### 7. Merging Data & Presentation

A typical "UI5-View Over the Wire" response looks like this:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/d52112e6-b9b7-4e7f-ac7f-825c20620240" />

"UI5 Over the Wire" - Response with View & Data together

But is this maybe just the same like RAP, but in a different format?






















### new

Domain-Driven Architecture with RAP (and contrast to UI-Driven)

**Domain-Driven Design (DDD)** is an architectural approach that models software systems based on real-world business domains. It promotes aligning code with business terminology and processes.

In SAP, the **ABAP RESTful Application Programming Model (RAP)** applies domain-driven principles to build enterprise applications on S/4HANA.

This page explains how RAP embodies Domain-Driven Architecture — and how **abap2UI5 takes a fundamentally different approach**.

---

## What is Domain-Driven Architecture?

Key principles of Domain-Driven Design:
- **Domain Models**: Represent business entities & relationships.
- **Bounded Contexts**: Isolated domain-specific models with clear interfaces.
- **Ubiquitous Language**: Shared business terminology in models & code.
- **Separation of Concerns**: Domain logic separated from UI & infrastructure.

In SAP, this typically translates to:
- CDS Views & Entities as Domain Models.
- Behaviors for domain-specific processes.
- Typed OData Services for structured interfaces.

---

## Domain-Driven Design in RAP

### Domain Models
- Modeled using **CDS Views** (Root & Composite Entities).
- Reflect business concepts like `SalesOrder`, `BusinessPartner`.
- Data model is defined **at design-time** via CDS.

### Behaviors & Processes
- Modeled through **Behavior Definitions** (Managed/Unmanaged).
- Actions like `Submit`, `Approve`, `Cancel` represent domain logic.
- Draft handling, validations, side effects handled by RAP runtime.

### Bounded Contexts & Service Projections
- Service Projections define clear boundaries per domain.
- Typed OData V4 APIs expose domain models & behaviors.

### UI Integration
- Fiori Elements renders UIs based on OData metadata & annotations.
- UI structure is derived from the domain model (Domain → UI).

---

## Limitations of RAP's Domain-Driven Approach
- **Data models are static**: Defined via CDS at design-time.
- **UI reflects the domain model structure**: Flexible adjustments at runtime are limited.
- Dynamic use cases (e.g., changing data schemas at runtime) are not feasible.
- Strong governance ensures stability but reduces runtime flexibility.

---

## Contrast: abap2UI5's UI-driven, Dynamic Approach

In contrast to RAP's domain-first architecture, **abap2UI5 follows a UI-driven pattern** with high runtime flexibility:

### Key Differences:
- **UI5 View Definitions are central**: The UI is designed first, independent of the backend data model.
- **Backend supplies XML Views & ViewModels dynamically**.
- Data models can be **swapped or constructed dynamically at runtime**.
- Technologies like **RTTI (Runtime Type Information)** enable dynamic data handling.
- No dependency on design-time CDS models.
- UI logic and structure are controlled in ABAP at runtime.

### Why is this different from RAP?
| Aspect | RAP (Domain-Driven) | abap2UI5 (UI-Driven, Dynamic) |
|--------|---------------------|------------------------------|
| **Data Model Definition** | CDS Views (design-time) | ViewModel built dynamically in ABAP |
| **UI Definition** | Based on domain model (annotations) | XML View controlled directly in ABAP |
| **Runtime Flexibility** | Limited, strictly typed | High: dynamic ViewModel, RTTI possible |
| **Frontend Rendering** | Fiori Elements SPA builds UI from metadata | Static UI5 shell renders backend-provided View |
| **Typical Use Cases** | Stable, transactional apps | Dynamic UIs, prototypes, tools, flexible CRUD apps |

---

## Summary

- **RAP** applies Domain-Driven principles by strictly defining data models, behaviors, and services at design-time.
- This ensures robust, structured applications aligned with business domains.
- However, it limits runtime flexibility and dynamic UI adaptations.

- **abap2UI5**, in contrast, prioritizes UI-driven development with flexible View & ViewModel handling at runtime.
- Data models can be built or replaced dynamically, allowing agile, backend-controlled UI composition without CDS dependencies.

Both approaches serve different purposes:
- **RAP**: Structured, enterprise-grade transactional apps.
- **abap2UI5**: Flexible, backend-driven UI development for scenarios where dynamic behavior & rapid iterations are key.

---

## Further Reading
- [Domain-Driven Design Quickly (DDD Summary)](https://dddcommunity.org/resources/)
- [ABAP RESTful Application Programming Model (RAP)](https://help.sap.com/docs/abap-cloud/abap-restful-application-programming-model)
- [abap2UI5 Documentation](https://abap2ui5.github.io/docs/)
- [Ubiquitous Language in DDD](https://martinfowler.com/bliki/UbiquitousLanguage.html)
- [SAP Clean Core Approach](https://community.sap.com/topics/clean-core)
