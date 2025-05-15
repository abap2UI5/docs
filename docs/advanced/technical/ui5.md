---
title: UI5 Freestyle vs abap2UI5: Architecture, State, Workflow & Communication
description: Compare UI5 Freestyle with abap2UI5, focusing on architecture, state handling, developer workflow, and client-server communication.
---

# UI5 Freestyle vs abap2UI5: Architecture, State, Workflow & Communication

This page compares **UI5 Freestyle applications** with **abap2UI5**, focusing on architecture, state handling, developer workflow, and client-server communication patterns.

---

## Architecture Comparison

### UI5 Freestyle
- **Frontend-centric SPA** built with SAPUI5/OpenUI5.
- Developers create XML Views, JavaScript Controllers, and Models in the frontend.
- Application logic, navigation, and UI state are managed client-side.
- Backend provides data via OData services (SEGW, CAP).
- Tight coupling between frontend development and backend data provisioning.

### abap2UI5
- Static UI5 shell in the browser, no app-specific frontend code.
- Backend defines UI5 XML Views & JSON ViewModels in ABAP.
- Frontend simply renders the backend-provided View definitions.
- User interactions are handled by backend logic via Over-the-Wire communication.
- Simplified, backend-driven UI rendering with minimal frontend complexity.

---

## State Handling

| Aspect | UI5 Freestyle | abap2UI5 |
|--------|---------------|----------|
| **State Definition** | Managed in frontend models (JSONModel, ODataModel) | Managed in ABAP ViewModels |
| **Frontend State Management** | Handled via controllers & bindings | Frontend has no state logic, purely renders backend ViewModels |
| **Persistence** | OData services persist data | Backend updates ViewModels and persists state |
| **Interactivity** | Events handled in frontend controllers | Events trigger backend logic, which returns updated ViewModels |

---

## Developer Workflow

| Aspect | UI5 Freestyle | abap2UI5 |
|--------|---------------|----------|
| **UI Definition** | XML Views & JS Controllers developed in frontend | XML View & ViewModel defined in ABAP |
| **APIs** | OData services required (SEGW, CAP) | Generic HTTP event handler, no OData needed |
| **Frontend Artifacts** | BSP or UI5 repository deployment required | Static UI5 Shell, no app-specific frontend deployment |
| **Development Scope** | Separate frontend and backend development | Pure ABAP development |
| **Deployment** | Frontend & backend deployed separately | Single ABAP transport / abapGit project |
| **Complexity** | High: UI5 Views, Controllers, Models + OData coordination | Low: Backend-driven simplicity |

---

## Client-Server Communication Flow

### UI5 Freestyle Flow
1. **Frontend loads custom UI5 application** (BSP, repository app).
2. **UI5 Views, JS Controllers, Models** are loaded and executed in browser.
3. **Frontend handles UI logic, state, and user interactions**.
4. **OData services are called for data retrieval & persistence**.
5. Backend returns data but does not influence UI structure or flow.
6. Frontend updates UI models and bindings accordingly.

```plaintext
Browser (UI5 Freestyle App)
  ├──> Load XML Views, Controllers, Models
  ├──> Handle events, state, navigation in frontend
  ├──> OData Calls to backend for data operations
Backend (OData Services / CAP)
  └──> Provides data, no involvement in UI rendering or logic
```

## abap2UI5 Flow
- Frontend loads static UI5 Shell (index.html).
- Requests View & ViewModel from backend (ABAP Class).
- Frontend renders UI5 controls from backend definitions.
- User events trigger HTTP requests to backend.
- Backend processes events, updates ViewModel.
- Backend returns updated ViewModel.
- Frontend re-binds UI, updating only affected controls.

```plaintext
Browser (Static UI5 Shell)
  ├──> HTTP Request: Load View & ViewModel
  ├──> Render UI5 controls from backend definitions
  ├──> User events → AJAX Event Request to backend
Backend (ABAP Class)
  └──> Processes events, updates ViewModel, returns changes
```

## Side-by-Side Comparison

| Aspect | UI5 Freestyle | abap2UI5 |
|--------|---------------|----------|
| **UI Rendering** | Client builds UI with XML Views & JS Controllers | Frontend renders backend-defined XML View & ViewModel |
| **Communication** | OData services (SEGW, CAP) for data operations | Simple HTTP event requests & ViewModel updates |
| **State Handling** | Managed in frontend models & controllers | Fully backend-driven ViewModel state |
| **Developer Workflow** | Separate frontend (UI5) and backend (OData) development | Pure ABAP class development |
| **Frontend Artifacts** | Requires BSP or UI5 repository deployment | Static UI5 Shell, no per-app deployment |
| **Flexibility** | High flexibility in frontend, but complex coordination with backend | Full backend control, runtime flexibility without frontend overhead |
| **Complexity** | High: UI5 Views, Controllers, Models + OData + deployment | Low: ABAP-only, Over-the-Wire simplicity |

## Conclusion

Both UI5 Freestyle and abap2UI5 enable UI5 application development, but follow fundamentally different approaches:

- **UI5 Freestyle** offers maximum frontend freedom, suitable for highly customized, interactive applications, but comes with significant complexity in development, state handling, and deployment.
- **abap2UI5** shifts UI control back to the ABAP backend, simplifying development by eliminating the need for separate frontend apps, OData services, and JavaScript controllers.

For projects where simplicity, backend-driven control, and rapid iterations are key, **abap2UI5 offers a pragmatic alternative** to the more frontend-heavy Freestyle approach.
