---
description: Why an ENQUEUE set while a screen opens is gone before the next click, and the patterns that replace it - lock at save, an optimistic check, a soft lock, a stateful session.
---
# #21 The Lock Is Gone by the Next Click

`VA02` calls `ENQUEUE_EVVBAK` when the order opens and holds the lock for as
long as the dialog session lives. That works because the session lives.

In abap2UI5 every click is a fresh session — [#16](/advanced/insights/16-one-click-one-request)
— so a lock set while the order opens is released before the user has finished
reading the screen. The ABAP is still valid. The assumption underneath it is
not, and this is the single habit that most reliably surprises a developer
coming from dialog programming.

It is also not a framework limitation to be worked around. It is the same
question every stateless web application has answered since roughly 1999, and
the answers are known. Locking splits into two questions:

| Phase | Question |
|---|---|
| **Edit** | What happens while the user is thinking and typing? |
| **Save** | What happens the moment they hit save? |

**Lock at save.** Nothing is held while the user thinks. The save handler
acquires the enqueue, writes, commits and releases, all inside one roundtrip —
the lock exists for milliseconds, and a foreign lock is a message box instead
of a write.

**Plus an optimistic check.** Two users can still read the same order, edit for
ten minutes each, and save one after the other — the second silently wins, and
the first user's afternoon disappears without a message. So the app remembers
the change timestamp it read, and compares before it writes: if the row changed
underneath, the save is refused and the user is told what happened. Together,
lock at save and the optimistic check are the production default for a
stateless app. Every pattern on this page is written out as a complete class,
with the edge cases, on [Lock](/cookbook/expert_more/lock).

![The lock lives inside one roundtrip; the timestamp guards the minutes between.](/insights/21-lock-at-save.svg)

*The lock lives inside one roundtrip; the timestamp guards the minutes between.*

**A soft lock** — a row in a table of your own saying who is editing what,
with a timestamp that expires — gives the *this order is being edited by
Müller* warning back, without a work process holding anything. It is advisory,
it can go stale, and users like it anyway.

**And the GUI way still exists.** `client->set_session_stateful( )` pins one
work process to this user for the rest of the app, and a classic enqueue
survives between clicks again. It is the right tool for a one-to-one migration
of a dialog flow and for a resource that is expensive to rebuild per request.
It is also one pinned work process per active user, has to be released on
every exit path, and is not available on public cloud. Use it knowingly, not
out of habit.
