# Where the Line Is

Eighteen articles is enough credit to say what this does not do.

**Offline is out.** Every event asks the server what happens next. Take the
server away and there is no app left — not a degraded one, none. An app that has
to work in a warehouse with no signal is a different architecture, and no amount
of tuning here turns into one.

**Pushdown to HANA is indirect.** A typed OData service can put a CDS view in
front of the database and let the frontend page, filter and fuzzy-search
straight against it. Here the generic service decoupled the UI from all of that
deliberately, and the price is paid exactly there: selecting from the CDS view
in ABAP and sending the result works, but it is written by hand, and a fuzzy
search help rendered on the frontend is not available.

**Real-time and heavily interactive UIs are not the target.** Collaborative
editing, live cursors, anything where the interesting state lives in the browser
between roundtrips — a request-per-event model is the wrong shape for it.

**Separate frontend teams lose.** The whole benefit assumes one developer holds
the screen and the logic together. Where a frontend team and a backend team work
independently by design, a contract between them is a feature, and this removes
it.

**And Fiori Elements is not a worse tool.** A list report or an object page
described by annotations is less work than building the same screen control by
control, it stays consistent by construction, and it keeps getting updates.
Where a floorplan fits, it fits.

What is left after all of that is still most business software: forms, tables,
dashboards, approvals, admin tools, the small screens nobody funds a project
for.

**A framework that fits everything fits nothing in particular. This one has a
shape, and the shape has edges.**

Happy ABAPing! 🦖🦕🦣
