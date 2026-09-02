---
outline: [2, 4]
---
# Use Cases

abap2UI5 fits many contexts, whether you work in a cloud-ready environment or with classic ABAP.

The sections below follow SAP's extensibility model. **On-stack** means the app runs inside your SAP system; **side-by-side** means it runs on a separate system (for example the SAP BTP ABAP Environment) and calls your SAP system remotely. Within each approach, SAP grades an extension by how "clean core" it is. Until 2025 that grade was the three-tier model; the [Clean Core Extensibility white paper](https://community.sap.com/t5/technology-blog-posts-by-sap/why-clean-core-matters-get-some-insights-into-our-brand-new-extensibility/ba-p/14163750) of August 2025 replaced the tiers with four **clean core levels**, and SAP S/4HANA 2025 ships with them:

| Level | Language version | What the extension uses | Clean core |
| --- | --- | --- | --- |
| **A** | ABAP for Cloud Development | released APIs only — on-stack, side-by-side or both | yes, and upgrade-stable |
| **B** | Standard ABAP | classic APIs and the frameworks SAP recommends for them: BAdIs, user exits, ALV, SAP GUI | yes, with less upgrade stability |
| **C** | Standard ABAP | SAP-internal objects that are neither released nor classified | conditionally — re-check before every upgrade |
| **D** | Standard ABAP | modifications, implicit enhancements, objects SAP marks as no API | no |

The framework itself is Level A: it is written in ABAP for Cloud Development and uses released APIs only. Which level an *app* reaches depends on what the app calls, not on abap2UI5.

::: tip Coming from the three-tier model
Tier 1 is Level A. Tier 2 was never a layer of its own — its wrappers still exist, written in Standard ABAP and released for ABAP Cloud, and an app that calls one stays Level A. Tier 3 is now split into Levels B, C and D by what it touches. Code written under the tier model does not need to be migrated; the ABAP Test Cockpit and the Cloudification Repository Viewer tell you which level an object lands on.
:::

## On-Stack Extension

### Level A
Build apps in ABAP for Cloud Development, using released APIs only. They are clean core, cloud-ready and upgrade-stable, and they run unchanged on S/4HANA Public Cloud, Private Cloud and on-premise:
![Level A on-stack extension: abap2UI5 and the apps run inside S/4HANA and use released APIs only](/advanced/use_cases/on_stack_level_a.svg){ width=60% }

### Level A with a Wrapper
If an API you need is not released, wrap it: a class in Standard ABAP that calls the classic API and is released for ABAP for Cloud Development. The wrapper is graded on its own — Level B as long as it sticks to classic APIs — and the abap2UI5 app that calls it stays Level A:
![Level A on-stack extension with a wrapper: the apps stay in ABAP for Cloud Development and reach a classic API through a wrapper written in Standard ABAP](/advanced/use_cases/on_stack_wrapper.svg){ width=60% }

### Levels B to D
On S/4HANA Private Cloud and on-premise you can also write the app itself in Standard ABAP, and on releases that do not know ABAP for Cloud Development yet it is the only option. Level B uses classic APIs and frameworks; Level C reaches SAP-internal objects and has to be re-checked before every upgrade; Level D — modifications and implicit enhancements — is not clean core. The framework stays Level A either way, so moving an app up a level later means changing what it calls, not how it renders:
![On-stack extension on Levels B to D: apps written in Standard ABAP call classic APIs or SAP-internal objects, while abap2UI5 itself stays Level A](/advanced/use_cases/on_stack_level_b.svg){ width=60% }

## Side-by-Side Extension

### Level A
Build apps on the SAP BTP ABAP Environment, separate from your S/4 system's lifecycle, with remote calls to released APIs only — OData, released RFC or SOAP services. Everything on the BTP side is Level A by construction, and it works with S/4HANA Public Cloud too:
![Level A side-by-side extension: abap2UI5 and the apps run on the SAP BTP ABAP Environment and call released remote APIs of S/4HANA](/advanced/use_cases/side_by_side_level_a.svg){ width=60% }

### With a Custom Service
When the released remote APIs do not cover what you need, expose your own service on the S/4 system. The BTP side stays Level A; the service on the S/4 side is graded by what it uses — Level B if it stays with classic APIs:
![Side-by-side extension with a custom service: the apps on SAP BTP stay Level A and call a service on S/4HANA that wraps classic APIs in Standard ABAP](/advanced/use_cases/side_by_side_custom_service.svg){ width=60% }

## Software as a Service (SaaS)
With a Level A side-by-side extension, you can connect a single abap2UI5 app to multiple S/4 systems. Use the same abap2UI5 code across customer tenants and remote systems for a real SaaS setup:
![SaaS architecture: one abap2UI5 app on the SAP BTP ABAP Environment serves several S/4HANA systems through their released APIs](/advanced/use_cases/saas.svg){ width=60% }

## Further Reading
- [Why Clean Core matters — the Clean Core Extensibility white paper](https://community.sap.com/t5/technology-blog-posts-by-sap/why-clean-core-matters-get-some-insights-into-our-brand-new-extensibility/ba-p/14163750), where SAP introduces the four levels
- [ABAP Extensibility Guide — Clean Core for SAP S/4HANA Cloud, August 2025 update](https://community.sap.com/t5/technology-blog-posts-by-sap/abap-extensibility-guide-clean-core-for-sap-s-4hana-cloud-august-2025/ba-p/14175399), the technical rules behind each level
- [Clean Core maturity and the new extensibility levels](https://community.sap.com/t5/technology-blog-posts-by-sap/clean-core-maturity-and-the-new-extensibility-levels/ba-p/14293974), on grading existing custom code
- [Use Cases of abap2UI5 — an Overview](https://www.linkedin.com/pulse/use-cases-abap2ui5-overview-abap2ui5-udbde/), the background article with additional scenarios, still in the vocabulary of the tier model
