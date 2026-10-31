---
description: The technical criteria for choosing UI5 Freestyle or abap2UI5 - the four edges where the browser has to be smart, and the case where the backend already knows everything.
---
# #34 UI5 Freestyle or abap2UI5?

Both give full access to the UI5 control library. Neither limits what a screen
can contain. So the choice is not about what is possible — it is about where
the state lives between two clicks, and who writes the code. The comparison
of the two as a whole is on
[When to use it, and when not](https://abap2ui5.github.io/why/#when-to-use-it-and-when-not){target="_self"};
this article is the four technical edges.

**Reach for UI5 Freestyle when the browser has to be smart.** Each of these
is a consequence of [#16](/advanced/insights/16-one-click-one-request): every
event is a request to the server, and nothing runs between two of them.

- **No offline-first app.** Every event asks the server what happens next, and
  the page itself needs the network to load. What exists is an experimental
  event queue, `cs_event-offline_queue`: the events it names wait in the
  browser while the backend cannot be reached, survive a reload, and are sent
  in order once the network is back — a form that saves later, not an app
  that works without a server. A warehouse app for a device with no signal is
  a different architecture.
- **Real-time and heavily interactive UIs are the wrong shape.** Drag and drop
  across a board, a canvas, live collaboration, anything where the interesting
  state lives in the browser between roundtrips. A request per event does not
  turn into that with tuning.
- **Pushdown to HANA is indirect.** A typed OData service can put a CDS view in
  front of the database and let the frontend page, filter and fuzzy-search
  straight against it. Here the app selects in ABAP and sends the result, and
  a bound table is capped by the model's size limit
  ([#23](/advanced/insights/23-100-rows-four-hours-one-request)) — paging and
  filtering are the app's `SELECT`.
- **Separate frontend teams lose.** The whole benefit assumes one developer
  holds the screen and the logic together. Where a frontend team ships on its
  own release cycle by design, the OData contract between the two is a
  feature, and giving it up costs more than it saves.

**Reach for abap2UI5 when the backend already knows everything.** The data,
the rules and the decisions are in ABAP, the screen shows them and sends
events back, and the team writing it writes ABAP. Then a separate frontend
project is a second place to maintain, a second thing to deploy and a second
release to coordinate — for a screen whose logic never left the server. The
loop is change, activate, refresh
([#11](/advanced/insights/11-no-build-no-deploy-no-cache)), and the view is
tested in ABAP Unit and the linter
([#24](/advanced/insights/24-abap-unit-for-a-screen)).

Nothing about picking one rules out the other later. The view is a string
either way, the controls are the same controls, and an abap2UI5 app can be
embedded into a Fiori elements page as a section
([Fiori Elements Integration](/advanced/fiori)).
