# Cloud-Ready Is a Property of Your App

The framework is ABAP Cloud compliant. It is written in the ABAP for Cloud
language version, it calls released APIs only, it modifies no standard SAP
code, and it installs on BTP ABAP Environment and S/4HANA Public Cloud.

None of that says anything about the app somebody builds with it.

ABAP Cloud is a restricted language version, and the restriction that matters
most on a screen is this one: no direct access to SAP database tables. The
Virtual Data Model — the CDS views SAP publishes and maintains as an API — is
the way in, and that is exactly what keeps an app upgrade-stable when the tables
beneath it change.

![The released view is the shape SAP keeps stable across the upgrade.](/insights/20-vdm.svg)

*The released view is the shape SAP keeps stable across the upgrade.*

Two lines decide it:

```abap
    " cloud-ready: a released CDS view
    DATA t_orders TYPE STANDARD TABLE OF i_salesorder WITH EMPTY KEY.

    SELECT FROM i_salesorder
      FIELDS salesorder, salesorganization
      INTO TABLE @t_orders
      UP TO 10 ROWS.
```

```abap
    " not cloud-ready: a database table read directly
    DATA t_orders TYPE STANDARD TABLE OF vbak WITH EMPTY KEY.

    SELECT FROM vbak
      FIELDS vbeln, vkorg
      INTO TABLE @t_orders
      UP TO 10 ROWS.
```

Same screen. Same framework. One of them survives the next upgrade and can move
to a cloud system, and the other cannot — and nothing about the UI is involved
in the difference.

That is worth saying plainly because a framework's badge is easy to mistake for
a guarantee. Clean core is not a property a dependency grants an application. It
is a property of what the application reads and writes, and it is decided in the
`SELECT`.

**A cloud-ready framework does not make a cloud-ready app. It just stops being
the reason one is not.**

Happy ABAPing! 🦖🦕🦣
