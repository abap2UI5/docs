# #11 No Build, No Deploy, No Cache

The loop is: change the class, activate, refresh the browser.

That is the whole article, and it is worth spelling out what is missing from it,
starting with the first page. Somebody has to deliver the initial HTML. In the
SAP world that normally means a BSP application: a frontend artefact, built
somewhere, deployed to the ABAP stack, transported on its own path, and
invalidated from its own caches when it changes.

abap2UI5 does not have one. The initial GET is answered from ABAP source code —
the page and the frontend files it needs are strings inside the handler, and
serving them is a method call.

![Four steps, or a method that returns a string.](/insights/11-initial-request.svg)

*Four steps, or a method that returns a string.*

The consequence is a project made of nothing but ABAP, and everything below
follows from it.

**No build.** Pull the repository with abapGit, activate, call the ICF node. No
npm install, no bundler, no dist folder, no separate deployment path that has
to succeed for the app to exist.

**No deployment.** The app is a class. Activating it is the deployment, and the
standard transport system moves it to production like any other ABAP object.
There is no state in which the backend is live and the frontend is not.

**No cache to invalidate.** The UI is built on every request, so there is no
build output that can be stale. Nobody runs a cache transaction, nobody asks a
colleague to hard-refresh, and a change that does not appear is a change that
was not activated.

**No IDE agreement.** It is ABAP. ADT, SE80, or a VS Code setup — that stays a
personal preference rather than a project decision, and nobody has to install a
frontend toolchain to fix a label.

**No context switch to debug.** A breakpoint in the method that built the view
stops in the method that built the view. The browser dev tools stay closed,
because the logic that produced the screen never left the backend.

**No supply chain to answer for.** A frontend build pipeline is a dependency
tree, and a dependency tree is something somebody has to audit, pin and renew.
A project whose frontend ships as ABAP source has one thing to review: the
source. Every file that reaches the browser is in the repository, readable,
diffable, and transported by the system that already governs everything else.

Individually these are conveniences. Together they are the reason a screen gets
tried at all: when an experiment costs a class and a refresh, the answer to
"could we just show this on a screen?" stops being a project.

Iteration speed is not a nice-to-have. It decides which ideas get built.

Happy ABAPing! 🦖🦕🦣
