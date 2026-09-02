# #36 Written for Agents

The property this series kept returning to — an app is one class, and the class
is text — has a consequence that was not on anybody's mind when the framework
was designed. It is the shape an AI coding agent is best at.

An agent writing a Fiori Elements app has to keep a CDS view, its annotations,
a behavior definition, a service binding and a generated frontend in step, and
can verify none of it without a system. An agent writing a freestyle app has
to hold an ABAP backend and a JavaScript frontend in its head at once and keep
the contract between them true.

An agent writing an abap2UI5 app writes one file, in one language, and the
thing it writes is the thing that runs.

Three things around the framework turn that into a working setup.

**A map for the reader with no checkout.** The site publishes
[`llms.txt`](https://abap2ui5.github.io/docs/llms.txt), every page with one
line of what it covers, and the framework repository publishes the same for its
interfaces. An agent that has read either one works from what abap2UI5 is
today instead of from what it recalls — where the view builder still has a
method per control.

**A check that needs no system.** The [linter](/advanced/linter) reconstructs
the UI5 view out of the ABAP that builds it and reports what UI5 does not have.
An agent that can verify its own work stops handing over apps that do not
render, and the same linter gates the sample repositories.

**Several hundred worked examples.** The sample catalogues hold a complete,
tested app per pattern — value help, tree, navigation, upload — so *has
somebody already built this?* has an answer. Several hundred of those samples
were themselves ported by agents that way: write the class, lint the view, run
it, read the screenshot, and CI keeps them true.

None of it is a feature of abap2UI5. It is what falls out when an application
is a file somebody can read — and an agent is only the newest somebody.

The setup, from *paste this* to an MCP server, is on
[Developing with AI](/get_started/ai).

**Code-first was the design. Agent-friendly was the consequence.**

Happy ABAPing! 🦖🦕🦣
