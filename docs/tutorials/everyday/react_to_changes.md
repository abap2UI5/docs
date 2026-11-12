---
outline: [2, 4]
samples:
  - z2ui5_cl_smps_app_011
  - z2ui5_cl_smps_app_012
description: Do something whenever a RAP business object changes - a log, a mail, a follow-up document. With business events raised in the save sequence and consumed by a handler class, and how the result reaches an abap2UI5 screen.
---
# React to Changes

Every new ticket should be logged, a manager informed when a status changes,
a follow-up document created when an order is released. The tempting place
for that is the app's Save handler: after the COMMIT, write the log, send the
mail. It works until the second app, the Fiori elements UI or the background
job changes the same data and nothing happens.

The reaction belongs to the business object, not to one of its screens. RAP
business events are the mechanism: the business object announces what
happened, a handler reacts, and neither knows the other.

The worked example is package `05` of
[abap2UI5/samples-stack](https://github.com/abap2UI5/samples-stack):
`Z2UI5_CL_SMPS_APP_011` creates and changes tickets, `Z2UI5_CL_SMPS_APP_012`
shows the log the handler writes. Business events need ABAP Platform 2021 or
ABAP Cloud.

To install it, pull the branch of that package with abapGit: see
[Getting the Sample](/tutorials/everyday/#getting-the-sample).

## The Shape of the Solution

| Part | Where it lives | What it does |
|---|---|---|
| **The event** | The behavior definition | Declares what the business object announces |
| **Raising** | The additional save of the behavior pool | Raises the event for what this transaction created or changed |
| **Consuming** | A handler class of its own | Reacts - here, writes a log row |
| **The screens** | Two abap2UI5 apps | One changes data, one shows what the handler did |

## Step 1: Declare What Is Announced

Two kinds of event, and the choice between them is the main decision:

```abap
" notification event: the key, nothing else
event TicketCreated;

" data event: the state at the time of the change, typed by an abstract entity
event StatusChanged parameter Z2UI5_R_SMPS_TCK_STAT;
```

A **notification** event says "this instance changed" and leaves it to the
handler to read the current state. A **data** event carries the state along,
so the handler needs no read - at the price of data that may be outdated by
the time the handler runs. Take the notification when the handler needs the
latest state anyway; take the data event when it needs to know what the
state was at the moment of the change, as an audit log does.

## Step 2: Raise in the Save Sequence

Events are raised where the business object knows the transaction went
through - in the additional save of the behavior pool, which sees what was
created and updated:

```abap
METHOD save_modified.
  IF create-ticket IS NOT INITIAL.
    RAISE ENTITY EVENT z2ui5_r_smps_tck~TicketCreated
      FROM VALUE #( FOR c IN create-ticket ( %key = VALUE #( TicketUUID = c-TicketUUID ) ) ).
  ENDIF.
  ...
ENDMETHOD.
```

Raised here, an event is raised only for data that is actually saved. A
validation that refuses the save also prevents the event, which a log
written in the app's handler would not get right.

## Step 3: Consume in a Handler Class

The handler is a class of its own that inherits from
`cl_abap_behavior_event_handler` and names the events it wants:

```abap
CLASS lhe_ticket DEFINITION INHERITING FROM cl_abap_behavior_event_handler.
  PRIVATE SECTION.
    METHODS on_ticket_created FOR ENTITY EVENT
      ticketcreated FOR ticket~TicketCreated.
ENDCLASS.
```

That declaration is all it takes to listen; nothing is registered anywhere else.
A second handler for the same event runs as well, and deleting one leaves
the business object unchanged. The handler receives all instances of the
transaction as one table, so it writes its log rows with one `INSERT`.

The handler runs **after** the transaction that raised the event, in a
session of its own. Two consequences:

- An error in the handler does not undo the change. Handle failures in the
  handler - log them, retry them - instead of expecting the save to fail.
- The app that saved does not see the result in the same roundtrip. The log
  row exists a moment later.

## Step 4: Show the Result on Screen

The abap2UI5 apps take no part in the event mechanism: one changes tickets
through EML as any app would, the other reads the log table. Because the
handler runs after the save, the log screen shows a new row on its next
read - the sample has a refresh button for it.

For a screen that should update by itself:

- **Polling.** A timer that reads again every few seconds - simple, and fine
  for a monitor page. See [Timer](/cookbook/browser_interaction/timer).
- **Push.** The handler publishes a message through ABAP Messaging Channels,
  and the open screens receive it over a WebSocket without asking - see
  [WebSocket](/cookbook/expert_more/websocket). On-premise only.

## What Fits Here, and What Does Not

| Reaction | Business event? |
|---|---|
| A log or audit trail of changes | Yes - a data event and an `INSERT` |
| A mail or a notification to someone | Yes - the handler sends it, see [Send by Mail](/tutorials/everyday/send_by_mail) |
| A follow-up document in another business object | Yes - the handler calls that business object with EML |
| A check that may refuse the save | No - that is a validation of the business object |
| A value that must be correct right after the save | No - that is a determination |

## Related

- [RAP](/cookbook/eml_cds_sql/rap) and [EML](/cookbook/eml_cds_sql/eml) -
  the business object and how the apps call it.
- [Timer](/cookbook/browser_interaction/timer) and
  [WebSocket](/cookbook/expert_more/websocket) - getting the result onto the
  screen without a click.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Ticket App — every create and update raises an entity event | [`Z2UI5_CL_SMPS_APP_011`](https://github.com/abap2UI5/samples-stack/blob/main/src/05/z2ui5_cl_smps_app_011.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `05-business-events` |
| Event Log App — what the handler wrote, newest first | [`Z2UI5_CL_SMPS_APP_012`](https://github.com/abap2UI5/samples-stack/blob/main/src/05/z2ui5_cl_smps_app_012.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `05-business-events` |

<!-- samples:end -->
