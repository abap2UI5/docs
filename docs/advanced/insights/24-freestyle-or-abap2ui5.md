# UI5 Freestyle or abap2UI5 — When to Use Which

Both give full access to the UI5 control library. Neither limits what a screen
can contain. So the choice is not about what is possible — it is about where the
work happens and who does it.

| | UI5 Freestyle | abap2UI5 |
|---|---|---|
| **Backend** | OData or REST services, loosely coupled | ABAP classes producing an XML view and a JSON model |
| **Frontend** | your own UI5 app: JavaScript, XML views, controllers | one static UI5 shell, shared by every app |
| **UI definition** | XML views in the frontend project | XML views written in ABAP |
| **Where the logic runs** | split between controller and backend | backend only |
| **Client state** | held in the browser between roundtrips | travels with the request |
| **Tooling** | Business Application Studio, a frontend build | any ABAP IDE, no extra toolchain |
| **Deployment** | build and deploy the frontend, plus the backend | activating the class |

**Reach for UI5 Freestyle when the browser has to be smart.** Anything that must
keep working offline, anything where interesting state lives on the client
between roundtrips, anything genuinely interactive — drag and drop across a
board, a canvas, live collaboration. A request per event is the wrong shape for
those, and no amount of tuning changes it. The same goes for an app that has to
ship as its own deployable artefact, and for a dedicated frontend team with a
release cycle of its own: there the contract between frontend and backend is a
feature, and giving it up costs more than it saves.

**Reach for abap2UI5 when the backend already knows everything.** The data, the
rules and the decisions are in ABAP, the screen mostly shows them and sends
events back, and the team writing it writes ABAP. Then a separate frontend
project is a second place to maintain, a second thing to deploy and a second
release to coordinate, for a screen whose logic never left the server.

The practical tiebreaker is usually iteration speed against client richness.
Change a class, activate, refresh — no build, no cache, no deployment — is worth
a great deal for internal applications, and worth nothing for an app that has to
work on a tablet with no signal.

**Nothing about picking one rules out the other later. The view is a string
either way, and the controls are the same controls.**

Happy ABAPing! 🦖🦕🦣
