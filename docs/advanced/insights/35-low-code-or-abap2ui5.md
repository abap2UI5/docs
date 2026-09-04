# #35 Low-Code or abap2UI5?

A commercial low-code platform and abap2UI5 answer the same question: modern
UIs for SAP systems without a full frontend stack per app. The models could
hardly be further apart — a visual designer on a licensed platform, or plain
ABAP in an open-source framework — so the choice is usually clear once the
right question is asked.

| | Low-code platform | abap2UI5 |
|---|---|---|
| **App definition** | visual designer, platform-specific format | ABAP classes, written in any ABAP IDE |
| **App storage** | the platform's repository | your system, like any other development object |
| **Versioning, transport** | platform mechanisms | CTS or abapGit |
| **Testing** | platform tooling | ABAP Unit, plus the project's linter |
| **License** | commercial, typically per end user | MIT, unlimited users |
| **Installation** | platform or add-on, plus client components | one abapGit pull |
| **Release coverage** | what the vendor supports | NW 7.02 to ABAP Cloud |
| **Exit path** | apps exist in the platform's format | apps stay plain ABAP classes in your system |

**Reach for a low-code platform when the requirement is outside code.**
Offline-capable native mobile apps, where local storage and synchronisation
are the whole point. A bundled suite — workflow, API management, a portal —
rather than a UI layer alone. Contractual support with an SLA and somebody to
hold accountable. Or app building by people who are not developers: a designer
exists precisely so that a non-developer can produce something, and no
code-first framework replaces that.

**Reach for abap2UI5 when apps should be code in your own system.** Every app
is an ABAP class: diffable, transportable, unit-testable, reviewable in a pull
request. There is no second format to govern and no designer artefact drifting
from the system it describes. Nothing recurs per seat — ten users and ten
thousand cost the same. And if the project vanished tomorrow, the apps would
keep running from your own repository, because MIT means the code is
permanently yours to fork.

One asymmetry is worth naming because it is new. A visual designer needs a
human in front of it. Code-first is what AI coding agents are actually good at
— write the class, validate the view, run it, read the screenshot — and several
hundred UI5 sample ports were produced that way and are guarded by CI.

Neither is a migration. abap2UI5 is adopted one app at a time, and the first
one costs an abapGit pull and an afternoon.

Happy ABAPing! 🦖🦕🦣
