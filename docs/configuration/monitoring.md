---
outline: [2, 4]
description: Logging what abap2UI5 apps do in production - the roundtrip monitor z2ui5_if_ui5_monitor an installation implements, and the Admin Cockpit add-on that shows whether abap2UI5 is used, fast and safely configured.
---
# Monitoring

::: info New in 1.147.0
The monitor interface ships with abap2UI5 1.147.0 and later. The Admin Cockpit
add-on is installed from its own repository with abapGit, like every add-on.
:::

abap2UI5 serves every roundtrip and keeps no record of any of them. That is on
purpose: what to log, where, for how long and under which privacy rules is a
decision of the installation, not of the framework. What the framework offers
instead is a **seam** — one interface it calls after every roundtrip, with
everything it knows about it — and an add-on that builds the usual answers on
top of it.

## The roundtrip monitor

`z2ui5_if_ui5_monitor` is an interface in the released API. Implement it in a
class of your own, in a package of your own, and abap2UI5 finds the class the
way it finds the [user exit](/advanced/extensibility/user_exits): the first
class implementing the interface, sorted by name, looked up once per roll area.
Its one method, `on_roundtrip`, is called once per POST roundtrip, after the
response is built — on success and on failure alike.

It is not called for the page request (GET), for HEAD, or for a POST the
[CSRF check](/configuration/security#cross-site-request-forgery-csrf) rejected:
none of them runs an app.

### What a roundtrip reports

`on_roundtrip` receives one structure, `is_roundtrip`:

| Field | Content |
| --- | --- |
| `app` | the app class that answered, upper case — or the one that failed; empty when the request failed before an app was resolved |
| `event` | the event of this roundtrip, empty on a first start or a navigation |
| `draft_id`, `draft_id_prev` | the draft id the response carries and the one the request came with; `draft_id` is empty on failure, `draft_id_prev` on a first start |
| `uname` | the user |
| `check_sticky` | the app runs in a stateful session — this decides the LUW, below |
| `check_start` | the first request of an app start |
| `timestampl` | when the roundtrip started, UTC |
| `ms_total`, `ms_load`, `ms_main`, `ms_render` | milliseconds in total and per phase: reading the request and the draft, the app's `main( )`, building the response and saving the draft |
| `bytes_request`, `bytes_response`, `bytes_model` | the sizes of the JSON strings in characters; the response and the model are 0 on failure |
| `ms_client_prev` | how long the **previous** roundtrip took as the browser measured it, sent along with this request; 0 when unknown |
| `check_error`, `error_text`, `error_class` | whether it failed, the full exception chain as the error response renders it — with the same error id the user sees — and the class of the root cause |

### An implementation

A monitor that keeps the slow and the failed roundtrips in a table of your own:

```abap
CLASS zcl_my_roundtrip_monitor DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_ui5_monitor.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_my_roundtrip_monitor IMPLEMENTATION.

  METHOD z2ui5_if_ui5_monitor~on_roundtrip.

    " only what is worth reading later
    IF is_roundtrip-check_error = abap_false AND is_roundtrip-ms_total < 2000.
      RETURN.
    ENDIF.

    " a stateful app owns the LUW - committing here would commit its work
    IF is_roundtrip-check_sticky = abap_true.
      RETURN.
    ENDIF.

    TRY.
        " zmy_rt_log is a table of your own
        DATA(ls_log) = VALUE zmy_rt_log( app         = is_roundtrip-app
                                         event       = is_roundtrip-event
                                         uname       = is_roundtrip-uname
                                         timestampl  = is_roundtrip-timestampl
                                         ms_total    = is_roundtrip-ms_total
                                         error_class = is_roundtrip-error_class ).
        INSERT zmy_rt_log FROM @ls_log.
        COMMIT WORK.
      CATCH cx_root ##NO_HANDLER.
        " a monitor never breaks an app
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
```

### The rules an implementation lives by

- **It is synchronous.** Whatever the method costs, the user waits for. Keep it
  to a few single-row statements and decide early what to skip.
- **It fails open.** Anything the method raises is caught and ignored, and so
  is a class that cannot be instantiated: a broken monitor costs its log
  entries, never the app. The user exit does the opposite and fails closed,
  because it is a hardening control; a monitor only watches.
- **The LUW depends on `check_sticky`.** For an app that is not sticky the LUW
  is empty when the monitor runs: on success the framework has committed its
  draft save, on failure it rolled the LUW back before the call. A
  `COMMIT WORK` in the monitor commits its own writes and nothing of the app.
  For a sticky app the LUW belongs to the app, which may hold uncommitted work
  and locks across roundtrips — do not commit then. Keep the entry and write it
  on a later roundtrip that is not sticky, or through a database connection of
  its own.
- **Commit yourself.** The framework offers no commit helper; an
  implementation issues `COMMIT WORK` itself.
- **One monitor is called.** With two implementing classes on a system, the
  first by name wins — the same rule as the user exit, so the choice does not
  change after a transport or a system copy. A monitor that has to feed two
  consumers calls the second one itself.

Without an implementation nothing changes, apart from the one lookup per roll
area. A host without a class repository — the framework running on Node — and
a unit test install a monitor directly with
`z2ui5_cl_ui5_srv_monitor=>set_monitor( )`.

## Admin Cockpit

[**abap2UI5-addons/admin-cockpit**](https://github.com/abap2UI5-addons/admin-cockpit)
answers the three questions asked before abap2UI5 goes to production — *is it
used, is it fast, is it safely configured?* — as an abap2UI5 app, installed
with abapGit next to the framework and started like any other app:

```
?app_start=z2ui5_cl_cockpit_app
```

| Tab | What it shows | Needs the monitor |
| --- | --- | --- |
| Overview | active users and roundtrips today, p95 response time, error rate, draft table size, and the last 30 days | yes |
| Apps | per app class: users, sessions, roundtrips, average and p95 time, response and model size, errors, last use — plus the apps nobody has started in N days | yes |
| Errors | grouped by app, event, exception class and first line, with every occurrence and its full exception chain — and **Reproduce** for an occurrence | yes; Reproduce also needs the headless frontend |
| Performance | the slowest roundtrips with their phases, and hints: a model over 1 MB, large responses, a slow p95, growing app state | yes |
| Drafts & Housekeeping | rows, age, owners and expiry of the draft table; delete expired drafts, purge the cockpit's own log | no |
| Installation & Security | version, platform, user exit, UI5 bootstrap and theme, installed add-ons — and a **security traffic light**: CSRF origin check, hidden error details, a CSP without `'unsafe-eval'` and `'unsafe-inline'`, security headers, reachable developer add-ons, the cockpit's own access | no |
| Live | who is active now | partly |
| Agents | what AI agents did through the agent add-on's endpoint — shown when that add-on is installed | no; the agent add-on |
| Settings | monitor mode, slow threshold, retention, privacy mode, administrators, and the change log | no |

The tabs that need no monitor work on any installation: install, open, read
the traffic light. What most of its security checks look at — the CSRF check,
the CSP, the response headers — is explained on the
[Security](/configuration/security) page.

### Installation

Install the framework first, then the cockpit with abapGit into a package of
its own. Pick the branch by your abap2UI5 release:

| Your abap2UI5 | Branch | What you get |
| --- | --- | --- |
| has `z2ui5_if_ui5_monitor` | `main` | everything |
| has no monitor interface yet | `standalone` | Installation & Security, Drafts & Housekeeping, Settings — the other tabs say what they need |

`standalone` is `main` without the one class that implements the monitor
interface, because abapGit always pulls a whole repository and that class
cannot activate without the interface. Once your abap2UI5 has it, switch the
repository to `main` and pull.

Then start the cockpit right away: until somebody has claimed it, it shows
nothing but a screen for claiming the administrator role (below).

### Who may open it

The cockpit shows usage and configuration of the whole system and can delete
drafts. Like every abap2UI5 app it can be started by anybody who reaches the
abap2UI5 ICF node, so it checks access itself, at the top of its `main( )`, in
this order:

1. **A class of your own implementing `z2ui5_if_cockpit_auth`** decides, when
   the installation has one — the first implementing class by name, found the
   way the user exit is found. A class that cannot be created denies.
2. **Otherwise the administrator list** on the Settings tab: only the users on
   it get in, everybody else sees "No authorization".
3. **As long as that list is empty, the cockpit shows only the claim screen**
   — one button, *Claim the administrator role* for the user in front of it,
   and nothing of the system. The first user who presses it becomes the
   administrator. The claim is a row with a fixed key, so of two users
   claiming at the same moment exactly one wins, and it is written to the
   change log. Until it is claimed, the security traffic light shows the
   cockpit's own access in red.

Claim it right after the installation, so the administrator is you. More
administrators are added on the Settings tab, and the last one cannot be
removed there. A class of your own looks like this:

```abap
CLASS zcl_my_cockpit_auth DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_cockpit_auth.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_my_cockpit_auth IMPLEMENTATION.

  METHOD z2ui5_if_cockpit_auth~check.

    " basis administrators only
    AUTHORITY-CHECK OBJECT 'S_ADMI_FCD' ID 'S_ADMI_FCD' FIELD 'ST0R'.
    result = xsdbool( sy-subrc = 0 ).

  ENDMETHOD.

ENDCLASS.
```

`action` is `DISPLAY` (open the cockpit) or `CHANGE` (delete drafts, purge
logs, change settings and administrators, reproduce errors).

When the administrator has left, one class method starts over — from a
two-line report, a console class (`if_oo_adt_classrun`) on ABAP Cloud or the
class test environment in SE24 or ADT:

```abap
" hand the cockpit to a named user ...
z2ui5_cl_cockpit_auth=>reset_admins( 'NEW_ADMIN' ).
" ... or empty the list, so the next user who opens the cockpit can claim it
z2ui5_cl_cockpit_auth=>reset_admins( ).
```

Both commit, and both are written to the change log.

### The change log

Every change made through the cockpit is recorded with time, user, action and
details: the claim and a reset, administrators added and removed, settings
saved, drafts deleted, logs purged, the housekeeping job, and every
reproduced error. The Settings tab shows it, newest first. It lives in the
cockpit's table `Z2UI5_T_CK_AUD` and is kept as long as the aggregates.

### What it records

The add-on's monitor class maps the roundtrip structure by name and hands it
to a recorder, which writes aggregates per day, hour, app and event — counts,
errors, the time of each phase, sizes and a latency histogram the p95 is read
from — plus one raw row for each failed or slow roundtrip. The mode decides how
much: `ALL`, `SAMPLE` (every error and a share of the rest), `ERRORS` or `OFF`.
Its own tables purge themselves by retention once a day.

The aggregates are added up without row locks: a roundtrip lands in the row of
its hour with one `UPDATE … SET cnt = cnt + 1, …`, which is atomic on every
database, so two work processes never lose each other's counts and nobody
waits. The sums — milliseconds and kilobytes — are `DEC 15` columns: an
`INT4` overflows at 2.1 billion, which the milliseconds of one busy app can
reach within an hour, `INT8` does not exist below 7.50, and `DEC 15` exists on
every release and on ABAP Cloud. Increments are capped at the largest value
the column holds rather than overflowing, and totals over several days are
shown capped the same way.

It follows the LUW rule above: for a sticky app it writes nothing and keeps
the entry in the roll area until the first roundtrip there that is not sticky.
A session that ends while still sticky loses those entries — the price of
never touching the app's LUW.

### Privacy

No user names are stored by default. Users are counted under a pseudonym made
from the user name and a salt that is replaced every day, so a pseudonym can
neither be traced back to a name nor linked to the same user on another day —
hence "users per day", never "users per month". Counting can be switched off
entirely, or switched to plain user names.

Recording which employee used which application, when and how fast can be
subject to co-determination by a works council — in Germany under § 87 (1)
no. 6 BetrVG, whether or not anybody intends to evaluate individuals. The
default and the "no counting" mode are designed to stay on the side of
aggregate system monitoring; agree plain user names with your works council and
your data protection officer before switching them on.

### Reproducing an error

When the headless frontend from [Step 12](/tutorials/step-12#testing-the-roundtrip)
of the walkthrough is installed, the detail of an error group offers
**Reproduce…** for the selected occurrence. It resumes the draft the failed
request came with and fires the same event again through the simulator, so the
app runs exactly as it ran for the user; the cockpit shows the exception
chain, the messages the app showed and the view the roundtrip displayed.

::: warning It re-runs the app logic for real
Whatever the event writes, posts or sends happens again — as the
administrator, with the administrator's authorizations, and committed if the
app commits. That is why the cockpit offers it to administrators only (the
`CHANGE` action), asks before it starts and writes every replay to the change
log first.
:::

What can be reproduced, all of it stated in the dialog:

- **Draft-based apps only** — a sticky (stateful) app keeps no draft to resume.
- **Only drafts that still exist** — drafts expire, after 4 hours by default.
- **Only your own drafts** — abap2UI5 binds a draft to its owner. The cockpit
  knows the owner from the user name (user tracking with plain names) or from
  today's pseudonym (the default, same UTC day only); otherwise it tries, and
  the simulator reports that there is no draft when it is somebody else's.
- **The event, not the values** — what the user typed in that roundtrip and
  the event's arguments are not recorded, so they are not replayed.

The cockpit names the simulator only at run time: it activates without it and
simply does not offer the button.

### Agents

When the [agent add-on](/advanced/agents#in-production-the-agent-add-on) is
installed, the Agents tab reads its audit log and settings — at run time, with
no dependency in either direction:

- whether the endpoint is enabled, the apps opted in, the app classes allowed
  or denied, and the agent administrators;
- calls per day, per app and per MCP client, each with the calls refused by
  **policy** (endpoint disabled, app not enabled for agents, event forbidden
  or reserved for a human) and those refused or failed otherwise
  (**validation**: a wrong field or value, an unknown or expired session, an
  exception in the app);
- the last calls with operation, event, outcome and text — the user only with
  user tracking set to plain names.

The security traffic light gets a line for it: a disabled endpoint is green,
an enabled one without an agent administrator is red, and one that allows
every app class or reaches no app at all is yellow.

### Housekeeping as a job

What the Drafts & Housekeeping tab does — delete expired drafts through the
framework's own draft store, purge the cockpit's rows past their retention —
is one call for a background job, `z2ui5_cl_cockpit_job=>run( )`: a two-line
report scheduled in SM36 on Standard ABAP, an application job on ABAP Cloud.

The cockpit runs on ABAP Cloud and on Standard ABAP from 7.50, with a 7.02
downport checked in its CI, and its UI runs on UI5 1.71 and later.

## Next Steps

- [User Exits](/advanced/extensibility/user_exits) — the other interface the
  framework finds on its own, and the one that fails closed
- [Security](/configuration/security) — what the cockpit's traffic light checks
- [Performance](/configuration/performance) — what to do about a slow p95
- [Agent-Operable Apps](/advanced/agents) — the same roundtrip, driven by an AI
  agent instead of a browser
