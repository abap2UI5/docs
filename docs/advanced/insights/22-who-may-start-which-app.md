# #22 Who May Start Which App

One ICF node serves every abap2UI5 app in the system, and the URL names the
class: `?app_start=zcl_job_monitor`. The first question a Basis colleague asks
is the right one — so anybody with the node can start any class?

Only if nobody decided otherwise, and the deciding happens in two familiar
places.

**Authentication is the ICF node's.** Logon procedure, SSO, certificate,
visibility — the node is configured like the node of any other UI5 app, and
abap2UI5 adds nothing to it and takes nothing away.

**Authorization is yours, and the natural place is the handler.** The handler
class the node points to is a class you write, and `run( )` is one line in it.
Everything before that line is an ordinary ABAP authorization check:

```abap
  METHOD if_http_extension~handle_request.

    DATA(app) = to_upper( server->request->get_form_field( `app_start` ) ).

    AUTHORITY-CHECK OBJECT 'Z_APP_AUTH'
                    ID 'APP' FIELD app.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    z2ui5_cl_ui5_http_handler=>run( server ).

  ENDMETHOD.
```

One authorization object with one field, the app class as the value, roles as
usual in PFCG. A department that needs its own set of apps gets its own node
and its own handler — the framework does not mind how many there are.

**Or inside the app**, the way a report checks before it selects. An
`AUTHORITY-CHECK` in the `check_on_navigated( )` branch, and a message instead
of a view when it fails. The two combine: the handler decides who may start
what, the app decides what they may do inside.

What the framework itself brings to the table is the part a web application
needs and a report never did. A CSRF token on every POST, on by default. A
Content Security Policy on the first page, restricting what the browser will
load, tightened in the user exit when an old UI5 release no longer has to be
served. Error details off in production, as [#20](/advanced/insights/20-message-still-works)
described. And the business logic never leaves the server: the browser gets a
view and the data bound to it, and nothing else.

**One node, one handler, one AUTHORITY-CHECK. The rest is PFCG.**

Happy ABAPing! 🦖🦕🦣
