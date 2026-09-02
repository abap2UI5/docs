# Cloud-Ready Is a Property of Your App

*abap2UI5 Know-How #20 — draft*

The framework is ABAP Cloud compliant. It is written in the ABAP for Cloud
language version, it calls released APIs only, it modifies no standard SAP
code, and it installs on BTP ABAP Environment and S/4HANA Public Cloud.

None of that says anything about the app somebody builds with it.

ABAP Cloud is a restricted language version, and the restriction that matters
most on a screen is this one: no direct access to SAP database tables. The
released data model — the CDS views SAP publishes as an API — is the way in,
and that is exactly what keeps an app upgrade-stable when the tables beneath it
change.

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

---

## LinkedIn teaser post

Plain text — LinkedIn renders no markdown.

> abap2UI5 is ABAP Cloud compliant: written in ABAP for Cloud, released APIs
> only, no modifications, installs on BTP and S/4HANA Public Cloud.
>
> None of that says anything about the app you build with it.
>
> SELECT FROM i_salesorder, or SELECT FROM vbak. Same screen, same framework —
> one survives the next upgrade and can move to a cloud system, the other
> cannot. Nothing about the UI is involved in the difference.
>
> Clean core is not a property a dependency grants your application. It is
> decided in the SELECT.
>
> New article 🎉
>
> Where does your team actually check this — review, ATC, or after the upgrade?
>
> #ABAP #SAP #UI5
