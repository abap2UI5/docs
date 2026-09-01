# No Cache, No Deploy, Any IDE

*abap2UI5 Know-How #16 — draft*

The loop is: change the class, activate, refresh the browser.

That is the whole article, and it is worth spelling out what is missing from it.

**No deployment.** The app is a class. Activating it is the deployment, and the
standard transport system moves it to production like any other ABAP object.
There is no second path that also has to succeed, and no state in which the
backend is live and the frontend is not.

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

**No handover to share.** Sending someone an app means sending them a class —
abapGit, a transport, or paste. Reviewing one means reading ABAP.

Individually these are conveniences. Together they are the reason a screen gets
tried at all: when an experiment costs a class and a refresh, the answer to
"could we just show this on a screen?" stops being a project.

**Iteration speed is not a nice-to-have. It decides which ideas get built.**

Happy ABAPing! 🦖🦕🦣

---

## LinkedIn teaser post

Plain text — LinkedIn renders no markdown.

> The abap2UI5 loop is: change the class, activate, refresh the browser.
>
> What is missing from that sentence is the point. No deployment — activating is
> the deployment. No cache to invalidate — the UI is built per request, so
> nothing can be stale. No IDE agreement, because it is ABAP. No context switch
> to debug: a breakpoint in the method that built the view stops there.
>
> Individually, conveniences. Together, the reason a screen gets tried at all —
> when an experiment costs a class and a refresh, "could we just show this?"
> stops being a project.
>
> New article 🎉
>
> What is your current edit-to-see-it time on a Fiori change?
>
> #ABAP #SAP #UI5
