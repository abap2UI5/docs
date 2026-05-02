---
outline: [2, 3]
---
# Trade-offs and Compatibility

_Part 4 of the architectural deep dive._

The previous parts covered what abap2UI5 enables: backend-defined Views, runtime models, draft-based state, and a tiny architectural surface. This page is about the **costs** — what abap2UI5 doesn't do well, where RAP and UI5 freestyle still win, why the system footprint stays small on purpose, and how a single codebase covers ABAP releases from 7.02 up to ABAP Cloud.

## As Simple as Possible

Apps can be built very complex, but the opposite also works — apps can be very simple. One minimal yet elegant approach uses `if_oo_adt_classrun`. By implementing a single method, you get output with one click (`F9`). This is very efficient and inspired abap2UI5. A comparison of both approaches:

<img width="600" alt="if_oo_adt_classrun vs. abap2UI5" src="/img/28a09830-ba3a-4608-aab9-5f4af8028a18.png" />

_`if_oo_adt_classrun` vs. abap2UI5._

To recap the architecture: abap2UI5 is built in a very generic way, placing most of the responsibility on the user's apps. The result: plenty of flexibility and freedom in the app implementation, but also full responsibility for view building and program flow. These trade-offs deserve attention.

## Downsides vs. UI5 and RAP

Most notably, compared to UI5, **abap2UI5 can't build offline capabilities** — in those cases, the app can't keep asking the server after every event to decide what happens next.

Also, using HANA DB capabilities directly on the frontend causes issues. Since every app uses the same generic HTTP service, the UI is decoupled from the rest of the stack. RAP, by contrast, uses a typed OData and can reach HANA capabilities via a CDS View directly (and skip the ABAP layer). This makes it easy to add pagination or fuzzy searches to UI5 freestyle or RAP apps. An OData service calling a CDS View of HANA directly works very well here.

You can still SELECT from CDS Views in an abap2UI5 app and send the result to the frontend. But doing it manually takes more effort, and a fuzzy search help can't be rendered on the frontend — this approach replaces the entire view on every request. As always, every advantage abap2UI5 brings (like flexibility in creating models) comes with a trade-off elsewhere.

And **Fiori Elements**, with all its floorplans and templates, is simple and will keep getting updates. The wide range of UI5 use cases makes comparing the approaches hard — and there's no fully fair single comparison.

::: tip Choosing the right tool
Use RAP and Fiori Elements when your app fits a standard floorplan and the data model is stable. Use UI5 freestyle when you need rich client-side interactivity or offline support. Use abap2UI5 when you want runtime flexibility, minimal frontend overhead, and full ABAP control over the UI.
:::

## System Footprint

The system footprint stays deliberately small — abap2UI5 ships only ABAP classes, with no CDS or RAP artifacts. Most of the code lives in user apps outside the framework. Overall, the framework comes to about **2,300 lines of code** spread across one HTTP handler, two interfaces, and one database table:

<img width="600" alt="System footprint of abap2UI5" src="/img/981ab684-d2cf-4f56-b25c-c333db3c6dcc.png" />

_System footprint of abap2UI5._

The HTTP handler class holds all the framework logic:

<img width="600" alt="This is all that abap2UI5 does" src="/img/9c54e6b9-18a0-4582-a8cf-345d41d61a00.png" />

_This is all that abap2UI5 does._

The framework focuses only on communication between backend and frontend, controlling the app flow, and building the view model. The initial GET request was covered earlier in the deep dive. This implementation handles the POST request:

<img width="600" alt="AJAX POST Handler" src="/img/d8276aed-f339-4084-97aa-b769a55d73c8.png" />

_AJAX POST Handler._

The result is a pure source-code-based framework, which makes the next part possible.

## Running Everywhere

At its core, abap2UI5 produces two strings — one holding an XML view and the other the JSON view model. The framework sends these strings to and from the frontend. As a result, you don't need a recent ABAP release — this works even on older releases. This approach runs on the latest ABAP Cloud stack, on-premise systems, and legacy systems alike, making it release-independent. And UI5 features aren't lost — the UI5 framework can be bootstrapped on the frontend from a Content Delivery Network (CDN), so the latest UI5 version is available even on older releases:

<img width="600" alt="Local Bootstrapping - UI5 version depends on the SAP release" src="/img/ff4e308f-4b34-4981-9d8a-ecda39039720.png" />

_Local Bootstrapping — UI5 version depends on the SAP release._

<img width="600" alt="CDN Bootstrapping - UI5 version independent of the SAP release" src="/img/a8477539-45c4-4a43-a53b-c4078d5057aa.png" />

_CDN Bootstrapping — UI5 version independent of the SAP release._

As a result, you can build abap2UI5 apps that are portable across SAP systems, releases, and environments. An app built once on ABAP Cloud 2305 can also run on earlier releases. Apps built on older NetWeaver releases can run on BTP ABAP Environment or S/4 Public Cloud ABAP Environment. For this compatibility, abap2UI5 and its apps have to work with both language versions — ABAP Cloud and Standard ABAP. To avoid duplication, abap2UI5 maintains a single codebase ("one code line").

## One Code Line

With this approach, dependencies stay limited to APIs and functions that are both cloud-released and available on earlier NetWeaver releases. abap2UI5 uses SAP dependencies only when truly needed — for example, for GUID creation:

<img width="600" alt="GUID creation compatible with ABAP Cloud and Standard ABAP" src="/img/8039f152-1f39-4428-93a3-6cf80b38da5d.png" />

_GUID creation compatible with ABAP Cloud and Standard ABAP._

Building methods compatible with both ABAP Cloud and Standard ABAP is more complex than picking either one. Fortunately, abap2UI5 needs only GUIDs as a dependency. When you build apps, keep this in mind. The key advantage: **abap2UI5 runs on ABAP 2305 and stays portable down to NetWeaver 7.02**.

## Compatibility and Downporting

Downporting abap2UI5 code by hand would produce a release that's hard to maintain and debug. To avoid this, abap2UI5 splits into two repositories: a main repository (compatible from NW 7.50 to ABAP 2305) and a downport repository (compatible down to NW 7.02).

[abaplint](/technical/tools/abaplint) produces the low-syntax branch automatically. This separate branch supports development with all new ABAP expressions available since ABAP 7.50 while still making sure that all future abap2UI5 features get downported and made available for ABAP 7.02 automatically.

Automated ABAP downporting greatly boosts efficiency. Beyond abaplint, the framework uses [abapGit](/technical/tools/abapgit) and benefits from the work of those who built it:

<img width="600" alt="abaplint dashboard and abapGit - tools powering abap2UI5 downporting" src="/img/b0ae2acd-9446-48ca-9459-13d4bffa8f72.png" />

## Summary

In short: inspired by HTML Over the Wire, abap2UI5 combined UI and Data and created a "UI5 Over the Wire" approach by sending the XML View from the server. A single generic HTTP service serves all apps, independent of the View and Data Model. This gives great flexibility — Data Models and Views can be created dynamically at runtime — which greatly cuts the number of backend artifacts.

The framework keeps its complexity small by avoiding frontend artifacts, removing extra customizing layers, and separating the view from the framework, along with app-specific JS or HTML. The result is a pure source-code approach with only one database table, two interfaces, one class, and about 2,300 lines of code. It builds in a single code line, making it cloud- and on-premise-ready and downportable to older releases. Paired with abapGit, the apps run on nearly every release.

Overall, with abap2UI5 you set aside some common rules: no separation between View and Model in the HTTP communication, HTML and JavaScript live directly in the source code, no OData or RAP — and there are other trade-offs to weigh. But if you accept all of this, you get a very minimal approach where you only need to write a single method to build standalone UI5 apps.

Happy ABAPing!

## Related

- **[The abap2UI5 Architecture](/technical/deep_dive/architecture)** — how Views and Models reach the frontend.
- **[Inside an App](/technical/deep_dive/lifecycle)** — drafts, lifecycle, `_bind` / `_event`.
- **[abaplint](/technical/tools/abaplint)** — the tool that produces the downport branch automatically.
- **[abapGit](/technical/tools/abapgit)** — distribution, transports, and contribution.

← Back to **[Inside an App](/technical/deep_dive/lifecycle)** or jump back to the **[Deep Dive Index](/technical/how_it_all_works)**.
