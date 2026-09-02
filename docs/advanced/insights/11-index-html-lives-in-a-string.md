# index.html Lives in a String

Somebody has to deliver the first HTML page. In the SAP world that normally
means a BSP application: a frontend artefact, built somewhere, deployed to the
ABAP stack, transported on its own path, and invalidated from its own caches
when it changes.

abap2UI5 does not have one. The initial GET is answered from ABAP source code —
the page and the frontend files it needs are strings inside the handler, and
serving them is a method call.

![Four steps, or a method that returns a string.](/insights/11-initial-request.svg)

*Four steps, or a method that returns a string.*

The consequence is a project made of nothing but ABAP.

That is what makes the whole thing installable with abapGit and nothing else.
No npm install, no bundler, no dist folder, no separate deployment path that
has to succeed for the app to exist. Pull the repository, activate, call the
ICF node.

It is also the reason a change is visible immediately. There is no build output
to go stale, so there is no cache to invalidate — the loop that costs the most
patience in frontend development on the ABAP stack simply is not present.

And there is a quieter benefit that has become harder to ignore. A frontend
build pipeline is a dependency tree, and a dependency tree is a supply chain
that somebody now has to audit, pin, renew and answer for. A project whose
frontend ships as ABAP source has one thing to review: the source. Every file
that reaches the browser is in the repository, readable, diffable, and
transported by the system that already governs everything else.

**The cheapest build pipeline is the one that was never introduced.**

Happy ABAPing! 🦖🦕🦣
