# Runtime


##### 10. Define Model at Runtime

This enables the possibility to define models not only at design time, but also at runtime. The user doesn't have to do any extra work because abap2UI5 handles the entire process in the background during every AJAX request:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/e9f1bf8c-6d8c-44ad-ba89-c3648b638335" />

abap2UI5 - Dynamic Data Binding & Model Creation

In apps we can use RTTI now again in a way that is similar to how it was used with ALVs. This means that there is no need to create separated apps for each model. In this demo, you can see an abap2UI5 app with a view including a table output that displays a generic table and its type is created and modified at runtime (similar to SE16):

![gif_se16_2](https://github.com/user-attachments/assets/20b4a140-7954-45b0-8d0e-8aa1e8a6f1f5)

Replacing the Model (metadata) at Runtime


##### 11. Define View at Runtime

Same for the view: In RAP, only certain predefined control attributes can be modified at runtime, while the view is defined in CDS artifacts with UI annotations previously. However, in an abap2UI5 app, it is possible to replace entire view controls. For example, in the following app, a table control is replaced with a list control and vice versa:

![gif_ui_change2-1](https://github.com/user-attachments/assets/b6e081e4-2eae-4175-aca8-fc761b145762)
Replacing the View at Runtime

##### 12. View & Model independent from the HTTP-Service

In the end, the View & Model are defined independent from the HTTP-Service and we are no longer forced to deliver a predefined static OData-Service for every app, as is the case in RAP. The number of backend artifacts is significantly reduced:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/6fb61790-87bc-47fa-855e-83d5292b70f3" />

RAP vs. Model & View decoupled from the (single & generic) HTTP-Service

Let's take a look to the HTTP-Handler that provides us with this flexibility.

















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
