# #25 When One Class Is Not Enough

Every example in this series is one class, and for the screens it is about
that is the right size. A real application grows past it, and the question is
what the seams are. There are four, and each one is ordinary ABAP.

**One class per screen.** The unit of an abap2UI5 app is a screen with the
logic behind it. A master list and a detail screen over different data are two
classes, connected by the stack from [#18](/advanced/insights/18-call-screen-leave-screen):
the list calls the detail with `nav_app_call( )`, the detail returns with
`nav_app_leave( )`, and the list reads the result through `get_app_prev( )`.
Each class is small enough to read, and each has a `main( )` of its own.

**Popups and value helps as classes.** A confirmation dialog, a value help, a
settings popup used from several screens — each is a `z2ui5_if_app` that
displays into the popup slot. Written once, called from anywhere, tested on its
own.

**The logic in a class that has no screen.** A `zcl_invoice_service` with
`read( )`, `validate( )` and `post( )` is where the business logic goes when a
second screen needs it, or a batch job does, or a test does. The app class
becomes what a dynpro program's PAI modules were: it takes the event, calls the
service, updates the attributes. One thing to know: a reference held in an
attribute is serialized with the instance, so a helper either implements
`if_serializable_object` and stays small, or is created fresh in every
`main( )`, which is usually the better answer anyway.

**Parts of one screen as nested views.** A master-detail layout, a tab strip,
a side panel — `nest_view_display( )` puts a view built in ABAP into a named
slot of the main view, and rebuilds only that slot later. The page stands, the
part changes.

![One app, several classes: a screen per class, a service without a screen, a popup on the stack.](/insights/25-more-than-one-class.svg)

*One app, several classes: a screen per class, a service without a screen, a popup on the stack.*

What does not appear in that list is a framework concept. No component, no
manifest, no router, no controller hierarchy — the seams are classes, method
calls and one stack, which is exactly what a larger ABAP program has been made
of for thirty years.

A larger app is more classes. The unit stayed the same size.

Happy ABAPing! 🦖🦕🦣
