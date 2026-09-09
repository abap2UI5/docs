# #22 Who May Start Which App

One ICF node serves every abap2UI5 app in the system, and the URL names the
class: `?app_start=zcl_job_monitor`. Show that to a Basis colleague and you get
the right question within about four seconds — so anybody with the node can
start any class?

Only if nobody decided otherwise, and the deciding happens in two familiar
places.

**Authentication is the ICF node's.** Logon procedure, SSO, certificate,
visibility — the node is configured like the node of any other UI5 app, and
abap2UI5 adds nothing to it and takes nothing away.

**Authorization is the app's, the way a report checks before it selects.** An
app is one class, `main( )` is its entry, and the branch that shows the first
view is where an ordinary `AUTHORITY-CHECK` goes — before anything is selected
and before anything is displayed. When it fails, the user gets a message
instead of a view:

```abap
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      AUTHORITY-CHECK OBJECT 'Z_APP_AUTH'
                      ID 'APP' FIELD 'ZCL_JOB_MONITOR'.
      IF sy-subrc <> 0.
        client->message_box_display( text  = `You are not authorized to use this app.`
                                     type  = `error`
                                     title = `Not authorized` ).
        RETURN.
      ENDIF.

      " build and display the view as usual

    ENDIF.

  ENDMETHOD.
```

One authorization object with one field, the app class as the value, roles as
usual in PFCG. Nothing here is new to anybody in the room, and that is the
point: the check sits in the class it protects, so a transport carries the app and its
guard together, and nothing on the node has to know which classes exist.

It also holds on the way into an app that the URL never names: a
`nav_app_call( )` from another app arrives as an ordinary roundtrip with no
`app_start` on it, and the check in `main( )` is the one that runs for it. A
guard on the node alone would have missed that entirely. The complete example,
with the refusal branch and what it should say, is on
[Authorization](/configuration/authorization).

What the framework itself brings to the table is the part a web application
needs and a report never did. A CSRF token on every POST, on by default. A
Content Security Policy on the first page, restricting what the browser will
load, tightened in the user exit when an old UI5 release no longer has to be
served. Error details off in production, as
[#20](/advanced/insights/20-message-still-works) described. And the business
logic never leaves the server: the browser gets a view and the data bound to
it, and nothing else — there is no client-side rule to read out of a bundle,
because there is no bundle.

One node, one class, one AUTHORITY-CHECK. The rest is PFCG.

Happy ABAPing! 🦖🦕🦣
