# When the API Is Not Released

SAP grades an extension, and since August 2025 it does so on four **clean core
levels**: A uses released APIs only, in ABAP for Cloud Development, and is
upgrade-stable. B uses classic APIs and the frameworks SAP recommends for them.
C reaches SAP-internal objects that are neither released nor classified, and has
to be re-checked before every upgrade. D — modifications, implicit enhancements
— is not clean core at all.

abap2UI5 is Level A. It is written in ABAP for Cloud Development and calls
released APIs only.

That says nothing about the app. Which level an app reaches is decided by what
the app calls, and a screen built on a released API is Level A whether it
renders through abap2UI5 or anything else.

Which leaves the case that actually comes up: the API you need is not released.

The move is a wrapper. A class in Standard ABAP that calls the classic API and
is itself released for ABAP for Cloud Development. It is graded on its own — B,
as long as it stays with classic APIs — and the app calling it stays A.

![The app stays in ABAP for Cloud Development and reaches a classic API through a wrapper written in Standard ABAP](/advanced/use_cases/on_stack_wrapper.svg){ width=60% }

The value of that is not the grade. It is that the part of the system which is
not upgrade-stable has a name, a size and a boundary. One class to re-check when
SAP changes the classic API underneath it, instead of a search through every app
that ever touched it.

**A wrapper does not make the dependency clean. It makes it findable.**

Happy ABAPing! 🦖🦕🦣
