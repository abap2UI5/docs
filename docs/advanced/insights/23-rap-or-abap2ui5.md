# RAP or abap2UI5 — When to Use Which

The honest answer is that most systems end up with both, and the question is
never "which framework" but "which one for this screen".

Here is the split that holds up in practice.

**Reach for RAP when the behaviour matters more than the screen.** A
transactional object with validations, determinations, authorizations and draft
handling — and more than one consumer for it. The moment a second client exists,
or is likely to, the behaviour needs to live somewhere that is not a UI, and RAP
is where SAP put it. Standard CRUD over a stable model, close to what a list
report or an object page already does, is the case it was built for and the case
where it costs the least.

**Reach for abap2UI5 when the screen is the deliverable.** One consumer, one
purpose, and often a short life: an operations tool, a correction screen, a form
somebody needs by Thursday, a dashboard for one team. Also whenever the shape is
not known until runtime — a table whose columns come from RTTI — or when the
screen needs a control the annotation vocabulary does not reach. And on an older
release, where RAP is not available at all.

**The two are not exclusive, and this is the part worth remembering.** An
abap2UI5 app calls a RAP business object through EML like any other consumer.
The validations still run, the draft still works, the authorizations still
apply. So a screen that RAP cannot shape the way it needs to be shaped is not a
reason to abandon the business object — only a reason to put a different UI in
front of it.

**Rule of thumb: model the behaviour once, in RAP, if more than one thing will
use it. Build the screen wherever it is cheapest.**

Happy ABAPing! 🦖🦕🦣
