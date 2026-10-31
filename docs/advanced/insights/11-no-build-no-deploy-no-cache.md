---
description: What is missing from change, activate, refresh - no frontend build, no deployment path, no cache to invalidate - because the initial page and the frontend modules are answered from ABAP source.
---
# #11 No Build, No Deploy, No Cache

The loop is: change the class, activate, refresh the browser.

That is the whole article. What is interesting is everything that is *missing*
from that sentence, so let us go looking for it, starting with the first page.
Somebody has to deliver the initial HTML. In the SAP world that normally means
a BSP application: a frontend artifact, built somewhere, deployed to the ABAP
stack, transported on its own path, and invalidated from its own caches when it
changes.

The standalone ICF node needs none. The initial GET is answered from ABAP
source code — the page and the frontend modules it needs are strings in
generated classes, and serving them is a method call. A BSP enters in two
cases only, and both are system-wide rather than per app: the launchpad loads
the shell from the UI5 repository, so that installation pulls one BSP for the
whole system ([#26](/advanced/insights/26-a-tile-like-any-other)); and a
custom control of your own lives in a BSP of its own
([#12](/advanced/insights/12-where-your-own-javascript-goes)).

![Four steps, or a method that returns a string.](/insights/11-initial-request.svg)

*Four steps, or a method that returns a string.*

The consequence is a project made of nothing but ABAP, and everything below
follows from it.

**No build.** Pull the repository with abapGit, activate, call the ICF node. No
npm install, no bundler, no dist folder, no separate deployment path that has
to succeed for the app to exist. Nothing that can be broken by a transitive
dependency you have never heard of releasing a patch on a Friday.

**No deployment.** The app is a class. Activating it is the deployment, and the
standard transport system moves it to production like any other ABAP object.
There is no state in which the backend is live and the frontend is not — which
is the state most Fiori incidents are actually about.

**No cache to invalidate.** The UI is built on every request
([#5](/advanced/insights/05-ui5-over-the-wire)), so there is no build output
that can be stale. Nobody runs a cache transaction, nobody asks a
colleague to hard-refresh, and a change that does not appear is a change that
was not activated.

**No IDE agreement.** It is ABAP. ADT, SE80, or a VS Code setup — that stays a
personal preference rather than a project decision, and nobody has to install a
frontend toolchain to fix a label.

**No context switch to debug.** A breakpoint in the method that built the view
stops in the method that built the view. The browser dev tools stay closed,
because the logic that produced the screen never left the backend.

Every file that reaches the browser is in the repository, readable, diffable,
and transported by the system that already governs everything else — what
that is worth to a review is on
[What you keep](https://abap2ui5.github.io/why/#what-you-keep){target="_self"}.
