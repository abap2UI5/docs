# #34 UI5 Freestyle or abap2UI5?

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

**Reach for UI5 Freestyle when the browser has to be smart.** This is where the
edges of abap2UI5 are, so they are worth listing plainly:

- **Offline is out.** Every event asks the server what happens next. Take the
  server away and there is no app left — not a degraded one, none. An app for
  a warehouse with no signal is a different architecture.
- **Real-time and heavily interactive UIs are the wrong shape.** Drag and drop
  across a board, a canvas, live collaboration, anything where the interesting
  state lives in the browser between roundtrips. A request per event does not
  turn into that with tuning.
- **Pushdown to HANA is indirect.** A typed OData service can put a CDS view in
  front of the database and let the frontend page, filter and fuzzy-search
  straight against it. Here the app selects in ABAP and sends the result, and
  a fuzzy search help rendered on the frontend is not available.
- **Separate frontend teams lose.** The whole benefit assumes one developer
  holds the screen and the logic together. Where a frontend team ships on its
  own release cycle by design, the contract between the two is a feature, and
  giving it up costs more than it saves.

**Reach for abap2UI5 when the backend already knows everything.** The data, the
rules and the decisions are in ABAP, the screen mostly shows them and sends
events back, and the team writing it writes ABAP. Then a separate frontend
project is a second place to maintain, a second thing to deploy and a second
release to coordinate, for a screen whose logic never left the server.

The practical tiebreaker is usually iteration speed against client richness.
Change a class, activate, refresh — no build, no cache, no deployment — is worth
a great deal for internal applications, and worth nothing for an app that has to
work on a tablet with no signal.

What is left after the edges is still most business software: forms, tables,
dashboards, approvals, admin tools, the small screens nobody funds a project
for.

Nothing about picking one rules out the other later. The view is a string
either way, and the controls are the same controls.

Happy ABAPing! 🦖🦕🦣
