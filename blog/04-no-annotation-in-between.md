# No Annotation in Between

*abap2UI5 Know-How #4 — draft*

In a metadata-driven UI a screen is described rather than written. The
description has a vocabulary — line items, selection fields, facets,
identification — and a generator turns it into UI5 at runtime. Whatever the
vocabulary covers arrives in an afternoon. Whatever it does not becomes an
extension point, a custom control, or a conversation about whether the screen
really needs that.

abap2UI5 has no vocabulary of its own to run out of. What it sends to the
browser is a UI5 XML view, so the vocabulary is the UI5 control library itself —
all of it, at the version the launchpad already serves.

Which is why a control nobody generates is not a special case. A three-step
wizard, for instance:

```abap
CLASS zcl_onboarding DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA name  TYPE string.
    DATA email TYPE string.
    DATA plant TYPE string.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.
    METHODS set_view.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_onboarding IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.
    IF client->get_event( ) = `COMPLETE`.
      client->message_toast_display( |{ name } assigned to { plant }| ).
    ENDIF.
    set_view( ).

  ENDMETHOD.

  METHOD set_view.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Page`
                )->a( n = `title` v = `Onboarding`

                )->ele( `Wizard`
                    )->a( n = `finishButtonText` v = `Create`
                    )->a( n = `complete`         v = client->_event( `COMPLETE` )

                    )->ele( `steps`
                        )->ele( `WizardStep`
                            )->a( n = `title` v = `Person`

                            )->tag( `Input`
                                )->a( n = `value`       v = client->_bind( name )
                                )->a( n = `placeholder` v = `Full name`

                        )->end(
                        )->ele( `WizardStep`
                            )->a( n = `title` v = `Contact`

                            )->tag( `Input`
                                )->a( n = `value` v = client->_bind( email )
                                )->a( n = `type`  v = `Email`

                        )->end(
                        )->ele( `WizardStep`
                            )->a( n = `title` v = `Assignment`

                            )->tag( `Input`
                                )->a( n = `value`       v = client->_bind( plant )
                                )->a( n = `placeholder` v = `Plant` ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

ENDCLASS.
```

Every name in that chain is a UI5 name. `Wizard`, `steps`, `WizardStep`,
`finishButtonText`, `complete` — not one of them is invented by abap2UI5, and
not one is translated on the way. The chain builds XML, the browser gets that
XML, and the reference for the view is the UI5 SDK. There is no second place to
look and no mapping to learn.

Two things follow, and the second is worth more than the first.

Anything the SDK documents is reachable the day the frontend serves it. A
control added in a newer UI5 release needs no framework release behind it,
because nothing inside abap2UI5 enumerates controls.

And what a developer learns building these screens is UI5 — the same knowledge a
freestyle app needs, the same one an extension to a generated app needs. It
outlives whichever ABAP-side framework it was learned through.

The price is on the same page. Nothing writes the layout: a list report a few
annotations would have described is here a table built control by control.
Reaching every control is not the same as being handed one.

**A vocabulary is a promise about what will be needed. An API makes no such
promise, and takes nothing off the table either.**

Happy ABAPing! 🦖🦕🦣

---

## LinkedIn teaser post

Plain text — LinkedIn renders no markdown.

> A metadata-driven screen is described, not written — and the description has a
> vocabulary. What the vocabulary covers arrives in an afternoon. What it does
> not becomes an extension point.
>
> abap2UI5 has no vocabulary of its own to run out of. It sends a UI5 XML view,
> so the vocabulary is the UI5 control library itself, all of it, at whatever
> version the launchpad already serves. The new article builds a three-step
> sap.m.Wizard in one ABAP class to show what that means in practice.
>
> The price is on the same page: nothing writes the layout for you.
>
> New article 🎉
>
> Which control did you last want, and not have a way to ask for?
>
> #ABAP #SAP #UI5
