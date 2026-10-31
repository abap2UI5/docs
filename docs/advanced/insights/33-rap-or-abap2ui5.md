---
description: The technical criteria for choosing RAP with Fiori Elements or abap2UI5 for a given screen - behavior against screen, EML from an abap2UI5 app, a runtime-typed model, the release floor.
---
# #33 RAP or abap2UI5?

Most systems end up with both, and the question is never "which framework"
but "which one for this screen". The criteria below are the technical ones;
the comparison of the two as products, with their costs and their reach, is on
[When to use it, and when not](https://abap2ui5.github.io/why/#when-to-use-it-and-when-not){target="_self"}.

**Behavior or screen.** RAP models the behavior of a business object —
validations, determinations, authorizations, draft handling — in a place that
is not a UI, and every consumer of the object gets it. Fiori Elements then
generates the screen from annotations. abap2UI5 models nothing: the class
builds a view and reacts to events, and whatever it reads or writes is an
ordinary ABAP statement in a method
([#3](/advanced/insights/03-one-class-no-programming-model)). So the first
question is where the logic has to live. A transactional object with more than
one consumer — a second UI, an API, a batch job — needs the behavior outside
the screen, and RAP is where SAP put it. A screen with one consumer and the
logic of that screen alone has nothing to put there.

**The two compose.** An abap2UI5 app calls a RAP business object through EML
like any other consumer. `MODIFY ENTITIES`, `COMMIT ENTITIES`, the `FAILED`
and `REPORTED` structures — the validations run, the draft works, the
authorization control applies, and the business object does not notice that
the caller is a screen. A screen that Fiori Elements cannot shape the way it
needs to be shaped is not a reason to abandon the business object, only a
reason to put a different UI in front of it.

**Where the model is decided.** A RAP service fixes the entity type at design
time, in CDS, and the frontend discovers it from the metadata. An abap2UI5
view binds internal tables, and an internal table may have a type that exists
only at runtime ([#1](/advanced/insights/01-somewhere-on-the-way-to-ui5)). A
screen whose columns come out of RTTI, or whose layout branches on the data
([#10](/advanced/insights/10-swapping-the-view-at-runtime)), has no
annotation to write.

**Where the vocabulary ends.** Fiori Elements renders what the annotation
vocabulary describes — list report, object page, their facets and actions —
and everything outside it is an extension point. abap2UI5 names any control
in the UI5 library ([#4](/advanced/insights/04-no-annotation-in-between)),
and writes the layout by hand in return. A list report over a stable model is
cheapest generated; a wizard, a tree, a custom control is cheapest written.

**The release floor.** RAP needs a release that has it. abap2UI5 runs from
NetWeaver 7.02 up ([#27](/advanced/insights/27-one-codebase-702-to-abap-cloud)),
and the same class runs on every release in between, which decides the
question on a system that has no RAP at all.

**Drafts.** RAP keeps a draft of the business object, in its draft table, for
as long as the draft lives. abap2UI5 keeps a draft of the app instance, in
`z2ui5_t_01`, for four hours by default
([#23](/advanced/insights/23-100-rows-four-hours-one-request)) — a different
thing with the same name. An abap2UI5 screen over a RAP object uses both, and
the one that holds the business data is RAP's.

Rule of thumb: model the behavior once, in RAP, if more than one thing will
use it. Build the screen with whichever of the two reaches the controls and
the data it needs with the least code.
