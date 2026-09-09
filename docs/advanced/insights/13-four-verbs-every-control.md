# #13 Four Verbs, Every Control

The first view builder in abap2UI5 had one method per UI5 control. `button( )`,
`input( )`, `table( )` — hundreds of them, each with the control's properties as
named parameters.

It was a good idea, for a real reason. ADT code completion listed them. A
developer who did not know the UI5 API could press Ctrl-Space in the backend and
be shown what a control accepted, in ABAP, without leaving the editor. The
frontend API had been imported into the backend tooling, which felt like
cheating in the best way.

It also had a hard edge. A control the class had no method for could not be
written at all. Every new UI5 control was a pull request, and a wrapper is a
translation — it can be behind, and it can be wrong, in a way the browser only
discovers at runtime.

![The four verbs, and where each one leaves the cursor in the tree.](/insights/13-four-verbs.svg)

*The four verbs, and where each one leaves the cursor in the tree.*

Its successor `z2ui5_cl_ui5_view_builder` has four verbs instead. `ele` opens an
element and descends into it, `tag` adds a child and stays, `a` sets an
attribute, `end` goes back up. Four methods, and you now know the entire API.

Nothing in there names a control, which is exactly why every control is
reachable — including the ones released last month and the ones nobody has
wrapped. The builder cannot be behind UI5, because it never knew what UI5
contains.

The trade is honest: the completion list is gone. What replaced it is not
nothing, though. The chain is still ABAP, so the compiler still checks it, and
the linter checks the view against the real UI5 metadata before it ever reaches
a browser — an unknown control, a misspelled property, a member that does not
exist in the oldest supported release. That last one catches the bug that used
to be found by a colleague on a 2019 system, three weeks later.

The completion list was a way to avoid reading the SDK. Coverage of the whole
API is worth more than a shortcut around part of it.

Four verbs that name no control can build every control.

Happy ABAPing! 🦖🦕🦣
