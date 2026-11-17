---
outline: [2, 4]
description: Accessibility of abap2UI5 apps - the UI5 controls bring keyboard, screen reader and high-contrast support; the view decides labels, accessible names, landmarks and focus; what the framework itself does; and what nobody has tested.
---
# Accessibility

An abap2UI5 app is a UI5 app. The view an app builds is an XML view of
standard UI5 controls, and the browser renders it with the UI5 runtime the
page bootstraps. So the accessibility features of UI5 apply to an abap2UI5 app
as they apply to any other: keyboard navigation, screen reader support through
ARIA attributes, high-contrast themes. SAP documents them in
[Accessibility](https://sapui5.hana.ondemand.com/#/topic/03b914b46e624b138a6fb1b7cf2049ae)
of the UI5 documentation.

What a control cannot know is what the view means. A label belongs to a field
only if the view says so, an icon button has a name only if the view gives it
one. That part is the app's, and it is the same work it is in a hand-written
UI5 app.

| I want to… | Section |
| --- | --- |
| know what UI5 does for me | [What the Controls Bring](#what-the-controls-bring) |
| write an accessible view | [What the View Has to Say](#what-the-view-has-to-say) |
| move the focus or announce a change | [Focus and Announcements](#focus-and-announcements) |
| offer a high-contrast theme | [Themes](#themes) |
| know what the framework does itself | [What the Framework Does](#what-the-framework-does) |
| know what is not verified | [What Is Not Verified](#what-is-not-verified) |

## What the Controls Bring

Every control in an abap2UI5 view is a UI5 control, rendered by UI5. Nothing
in between replaces or wraps it. What a control does for accessibility it does
here as well:

- **Keyboard.** The controls can be reached and used with the keyboard: Tab
  between them, the arrow keys inside a list, a table or a segmented button,
  Enter and Space to press. UI5 documents the keys per control.
- **Screen readers.** The controls render the ARIA roles and attributes UI5
  gives them, and read their own state - selected, expanded, required,
  invalid - to assistive technology.
- **Themes.** The high-contrast themes of UI5 restyle every control
  ([Themes](#themes) below).
- **The language of the page.** UI5 sets the `lang` attribute of the page to
  the language it runs in, which a screen reader uses to pick its voice. That
  language follows the logon language
  ([Logon Language](/configuration/setup/logon_language)).

Custom controls are the exception. A control under
[Custom Controls](/advanced/extensibility/custom_control) - your own or one the
framework ships - has exactly the accessibility its author wrote into it.

## What the View Has to Say

The rules below are the ones of any UI5 view. In abap2UI5 they are attributes
in the view chain:

- **Every input has a label.** Give the `Label` the `labelFor` of the field,
  or put both into a `SimpleForm` or `Form`, which link a label to its field.
  A placeholder is not a label: it disappears as soon as the user types.
- **Every icon-only button has a name.** A `Button` with an `icon` and no
  `text` reads as "button" and nothing else. Give it a `tooltip`, or an
  `ariaLabelledBy` pointing at a text. The same holds for a meaningful `Image`
  (`decorative` set to `false`): it needs an `alt`.
- **A button text says what it does.** *Save order* reads well out of
  context; *OK* and *Click here* do not.
- **Color is not the only signal.** An `ObjectStatus` with `state` `Error`
  is red; give it a `text` and an `icon` as well, so the state reads without
  the color. The same goes for a `valueState` on an input: add a
  `valueStateText`.
- **A control names its context.** `ariaLabelledBy` and `ariaDescribedBy`
  point at other controls by id. A `core:InvisibleText` is a text that is
  read but not shown - the way to name a table, a list or a toolbar that has
  no visible title.
- **The page has landmarks.** `sap.m.Page` takes a `landmarkInfo`
  aggregation (`PageAccessibleLandmarkInfo`) whose labels a screen reader
  uses to jump between header, content and footer.
- **The browser tab has a title.** The page abap2UI5 delivers is called
  *abap2UI5* until the app sets its own with `cs_event-set_title`
  ([Title](/cookbook/browser_interaction/title)). The title is the first thing
  a screen reader announces after a switch of tabs.

An app that does all of it:

```abap
CLASS zcl_sample_accessible DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA mv_customer TYPE string.
    DATA mv_quantity TYPE i.
    DATA mv_status   TYPE string VALUE `Not saved`.
    DATA mv_state    TYPE string VALUE `Warning`.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_sample_accessible IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      client->follow_up_action( val   = client->cs_event-set_title
                                t_arg = VALUE #( ( `Order entry` ) ) ).

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`      v = `sap.m`
              )->a( n = `xmlns:mvc`  v = `sap.ui.core.mvc`
              )->a( n = `xmlns:core` v = `sap.ui.core`

              )->ele( `Page`
                  )->a( n = `title` v = `Order Entry`

                  )->ele( `landmarkInfo`
                      )->tag( `PageAccessibleLandmarkInfo`
                          )->a( n = `rootLabel`    v = `Order entry`
                          )->a( n = `contentLabel` v = `Order data`
                          )->a( n = `footerLabel`  v = `Order actions`
                  )->end(

                  )->ele( `content`
                      )->tag( n = `InvisibleText` ns = `core`
                          )->a( n = `id`   v = `helpText`
                          )->a( n = `text` v = `Opens the help for this screen`

                      )->tag( `Label`
                          )->a( n = `text`     v = `Customer`
                          )->a( n = `labelFor` v = `customer`
                          )->a( n = `required` v = `true`
                      )->tag( `Input`
                          )->a( n = `id`    v = `customer`
                          )->a( n = `value` v = client->_bind( mv_customer )

                      )->tag( `Label`
                          )->a( n = `text`     v = `Quantity`
                          )->a( n = `labelFor` v = `quantity`
                      )->tag( `StepInput`
                          )->a( n = `id`    v = `quantity`
                          )->a( n = `value` v = client->_bind( mv_quantity )

                      )->tag( `ObjectStatus`
                          )->a( n = `text`  v = client->_bind( mv_status )
                          )->a( n = `state` v = client->_bind( mv_state )
                          )->a( n = `icon`  v = `sap-icon://status-inactive`
                  )->end(

                  )->ele( `footer`
                      )->ele( `OverflowToolbar`
                          )->tag( `Button`
                              )->a( n = `icon`           v = `sap-icon://sys-help`
                              )->a( n = `tooltip`        v = `Help`
                              )->a( n = `ariaDescribedBy` v = `helpText`
                              )->a( n = `press`          v = client->_event( `HELP` )
                          )->tag( `ToolbarSpacer`
                          )->tag( `Button`
                              )->a( n = `text`  v = `Save order`
                              )->a( n = `type`  v = `Emphasized`
                              )->a( n = `press` v = client->_event( `SAVE` ) ).

      client->view_display( view->stringify( ) ).

    ELSEIF client->check_on_event( `SAVE` ).
      IF mv_customer IS INITIAL.
        mv_status = `Customer missing`.
        mv_state  = `Error`.
        client->follow_up_action( val   = client->cs_event-set_focus
                                  t_arg = VALUE #( ( `customer` ) ) ).
        RETURN.
      ENDIF.
      mv_status = `Saved`.
      mv_state  = `Success`.

    ELSEIF client->check_on_event( `HELP` ).
      client->message_box_display( `Enter the customer and the quantity, then save.` ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

The status changes from *Not saved* to *Saved* or *Customer missing* with its
state, and the text says it as well as the color. The help button has no text,
so it gets a tooltip for its name and a hidden description. A save without a
customer puts the focus back into the field that is missing.

The [abap2UI5 linter](/advanced/linter) checks part of this: its rule
`missing-accessibility` reports an icon-only `Button` without `text`,
`tooltip` or `ariaLabelledBy`, and an `Image` declared meaningful without
`alt`. It is a hint, because neither is wrong in every view. Everything
else in the list above no tool decides for you.

## Focus and Announcements

**The focus stays where it is when the view stays.** An event that only
changes bound values does not replace the view: UI5 updates the controls in
place, and the focus, the caret and the scroll position stay where the user
left them. That is one more reason to call `view_display( )` only when the
structure changes ([Performance](/configuration/performance#call-view-display-once)).

**After `view_display( )` the app decides.** A new view is new controls, and
the framework does not put the focus back by itself. It sends what had the
focus with every request - `client->get( )-s_focus`, the control id and the
caret - and the app moves it with `cs_event-set_focus`, which also takes a
selection range ([Focus](/cookbook/browser_interaction/focus)). Move the focus
where the user's next step is: the first field of a new screen, the field an
error message is about.

**Announce what changes out of sight.** A text that appears somewhere the
user is not looking - a count that changed, a background job that finished -
is not read by a screen reader by itself. The frontend action
`cs_event-control_global` with `INVISIBLE_MESSAGE` and `announce` reads a text
out without showing it (UI5 1.78 and later;
[Frontend Actions](/cookbook/event_navigation/frontend#calling-control-methods-on-the-frontend)):

```abap
client->follow_up_action( val   = client->cs_event-control_global
                          t_arg = VALUE #( ( `INVISIBLE_MESSAGE` )
                                           ( `announce` )
                                           ( `3 orders released` )
                                           ( `Polite` ) ) ).
```

A message toast is shown and gone again after a few seconds; for anything the
user must not miss, use a message box, which keeps the focus until it is
closed ([Message](/cookbook/translation_messages/message)).

## Themes

The theme is set in the user exit, `cs_config-theme`, or for one start with the
URL parameter `sap-ui-theme` ([Theme](/configuration/setup/theme)). The
high-contrast themes are `sap_horizon_hcb` (black) and `sap_horizon_hcw`
(white), and `sap_fiori_3_hcb` / `sap_fiori_3_hcw` on releases before UI5 1.102.
A user who needs one can start an app with `?sap-ui-theme=sap_horizon_hcb` in
the URL, or an installation can make it the default in the exit.

Custom CSS does not follow the theme. A color written into
[Style / CSS](/configuration/setup/style_css) stays that color in a
high-contrast theme; use the theme parameters of UI5 or leave the colors to
the controls.

## What the Framework Does

Most of what the user sees is the app's view. A few parts are the
framework's own:

- **The busy indicator.** While a roundtrip runs, the framework shows UI5's
  global `sap.ui.core.BusyIndicator` (or sets the component busy when it is
  embedded in a page it does not own). The flag `check_no_busy` keeps it down
  for one event ([Handling Events in ABAP](/cookbook/event_navigation/backend#the-per-wire-options-s-ctrl)).
- **Message boxes and toasts.** `message_box_display( )` and
  `message_toast_display( )` open `sap.m.MessageBox` and `sap.m.MessageToast`,
  UI5's own controls with UI5's own keyboard and screen reader behavior.
- **The error dialog.** An error the app does not catch opens a
  `sap.m.Dialog` with its focus on *Restart*. It does not close with Escape,
  on purpose: the app behind it is broken. When UI5 itself cannot render, a
  plain HTML overlay takes its place; it carries `role="alertdialog"`,
  `aria-modal` and a label, moves the focus into itself and keeps it there.
  Its texts are English, whatever the logon language.
- **The focus of an embedded app.** An app embedded in another page does not
  take the focus from the host page.
- **What it does not do.** It moves no focus after a `view_display( )`, adds
  no ARIA attribute to the controls of a view, and does not check a view for
  accessibility at run time.

## What Is Not Verified

**No accessibility test suite exists for abap2UI5, and no audit has been
made.** Nobody has measured an abap2UI5 app against WCAG or a national
standard, and the framework's automated tests do not run an accessibility
checker. What this page says the controls do is what UI5 says they do. What
it says the framework does is read from the frontend's source, not
measured with a screen reader.

An app that has to meet a standard has to be tested as such: with the
keyboard alone, with a screen reader, in a high-contrast theme, and against
the checklist your organization uses. SAP's
[accessibility documentation for UI5](https://sapui5.hana.ondemand.com/#/topic/03b914b46e624b138a6fb1b7cf2049ae)
has a chapter on testing each aspect: screen reader support, keyboard
handling, colors and theming, text size.
