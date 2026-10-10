# #37 What the AI Leaves Behind

The commercial low-code platforms have AI assistants now. Describe an app in
chat and the assistant assembles it in the visual designer, out of the same
components, bindings and event logic a hand on the mouse would have used —
their own material says so, and it is the right way to build such an
assistant: the platform's building blocks are what the platform's runtime can
run.

So "an AI builds the app" is now true on both sides of
[#35](/advanced/insights/35-low-code-or-abap2ui5), and as a demo the two
worlds look identical — a prompt goes in, an app comes out. The question that
separates them has moved one step later: what is left behind when the
assistant is done?

On a platform, what is left behind is what was always left behind: a designer
artifact in the platform's repository, run by the licensed runtime. The AI
changed who operates the designer, not what exists afterwards. Every line of
the table in #35 still reads the same — the per-user license, the platform's
own versioning and testing, the exit path that ends where the contract does.
The assistant makes the artifact cheaper to produce, and no cheaper to own.

Here, what is left behind is an ABAP class
([#36](/advanced/insights/36-written-for-agents)), and that difference does
the work. A class lands in a pull request and shows its diff. It goes through
the transport that has always decided what reaches production. It stays in
your system when any contract ends, because there is no runtime to license
under it.

The same split answers the governance question the platforms now lead with:
who checks what an AI built? A platform assistant answers with the platform —
it plans before it writes, asks before it acts, and the person who prompted
approves what they see. Code answers with the controls your shop has run for
twenty years: review on a readable diff, [the linter](/advanced/linter)
judging the view without a system, unit tests, the transport route, and an
authorization concept that never asked who wrote the code. Governing
AI-written apps is not a new layer to buy. It is the old layer, doing its job
on one more author.

And because the artifact is text, the claim can be measured instead of
demonstrated: every portable sample of the official UI5 demo kit has been
ported this way — written by agents, judged by the same gates, held green by
CI in [samples-controls](https://github.com/abap2UI5/samples-controls). A
number like that exists only where the AI's output is something a gate can
read.

The demo is the same everywhere now: a prompt becomes an app, in under a
minute, to applause. The artifact is what you own, review, transport and pay
for from that minute on. Judge the tool by what it leaves behind.
