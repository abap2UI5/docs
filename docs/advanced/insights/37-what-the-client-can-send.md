---
description: The threat model of an abap2UI5 app - what the browser chooses (the class, a draft id, the event, every bound value, startup parameters) and what the framework checks on each; the page a security review starts from.
---
# #37 What the Client Can Send

Every roundtrip is a POST the browser assembles, and the framework trusts
nothing in it that it cannot verify. This is the list of what the browser
chooses, what the framework checks on each item, and what is left to the app —
the page a security review starts from, with the setting that decides each
item named.

**The class.** The first request names the app: as `?app_start=` on the page
URL, as a launchpad startup parameter, as a hash route, or in the body of the
first POST — after the start, no roundtrip carries a class name any more, and
`nav_app_call( )` changes the app without the browser naming it. Before the
framework instantiates the class it checks one thing through RTTI: that the
class implements `z2ui5_if_app`. A class that does not is refused before it is
loaded. A class that does is started — for anybody who reaches the ICF node.
So every implementer of the interface on the system can be started by every
user of the node, and the decision which user may run which app is the app's own
`AUTHORITY-CHECK` in the branch that shows the first view
([#22](/advanced/insights/22-who-may-start-which-app),
[Authorization](/configuration/authorization)). A check on the node sees only
who reaches the framework, not which app runs.

**The draft id.** Every response carries the id of the row in `z2ui5_t_01`
that holds the serialized instance, and the next request sends it back. The
row carries `sy-uname`, and the read is scoped to it: an id of another user's
draft is answered with `NO_DRAFT_ENTRY_OF_PREVIOUS_REQUEST_FOUND`, not with
that user's instance. An expired id — older than `draft_exp_time_in_hours`,
four hours by default — is gone, because every first start of an app deletes
every row past the expiry. What a draft id therefore gives its holder is the
ability to continue *their own* session from a bookmark or a copied link, and
nothing about anybody else's.

**The event, and every bound value.** The event name arrives from the browser,
so `get_event( )` returns whatever the browser sent — a button that the view
did not render, or rendered disabled, is no guarantee that its event never
arrives. A handler that must not run for a given state checks the state, not
the button. The same holds for the model: every attribute the view binds with
`_bind( )` is a path the browser may send a value for, and the framework
writes what arrives into the attribute, a `Text` as much as an `Input`. A
value that must not change on the way through the browser — a key, a price,
a user name — is not bound, or is re-read on the server before it is used.

**Startup parameters.** `client->get( )-t_comp_params` holds the parameters
of the target mapping or the start URL, and the user can start the app with any
other value. An app that shows one sales order checks that the user may see
*that* order, not only that they may run the app
([Startup parameters are user input](/configuration/authorization#startup-parameters-are-user-input)).

**The headers.** The CSRF check compares the host of the `Origin` header — or
`Referer`, when there is no `Origin` — against the request's `Host`, and a
cross-origin POST is rejected before any app logic runs. It is on by default
(`check_csrf_active`), and it is lenient by design about a request that carries
neither header. Behind a reverse proxy or a web dispatcher the host the
browser saw is the first `X-Forwarded-Host`, so that header is trusted by
default (`check_trust_forwarded_host`). A client can set that header too: an
installation that is *not* behind a proxy switches the trust off in its user
exit, and only the transport-level `Host` is compared from then on
([CSRF](/configuration/security#cross-site-request-forgery-csrf)).

**The error body.** An exception that escapes `main( )` ends the roundtrip
with HTTP 500, and the body carries the exception chain, source positions and
system context. `check_hide_error_details` in the user exit reduces it to a
bare *Internal Server Error*
([#20](/advanced/insights/20-message-still-works)); a production system sets
it.

**And the other direction.** `follow_up_action( )` with
`cs_event-control_by_id` lets the *server* call a method on a control in the
browser, and the frontend refuses a list of methods that would break its own
invariants — `destroy`, `fireEvent`, `clone`, the `bind…`, `attach…` and
`setModel` families, the generic `setAggregation` and its kin — and casts each
argument to the kind the method declares, dropping arguments beyond them. That
list is a guard for the framework, not a boundary against the client: the
actions flow from the server to the browser, and the browser is the untrusted
side. A security review of the client reads the six items above; this one
belongs to a review of the app's own follow-up actions.

What the browser never gets is the business logic. It receives a view and the
data bound to it ([#5](/advanced/insights/05-ui5-over-the-wire)), and every
decision about either is taken in `main( )` on the server, on the next
request.
