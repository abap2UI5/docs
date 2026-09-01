/*
 * Which fenced example gets a Run button.
 *
 * The decision is made in `docs/.vitepress/playground.mjs`, and the thing that
 * could really decide it — the playground itself — is not in this repository.
 * So the rules there are an approximation, and this file is what keeps the
 * approximation honest: one fixture per shape that was actually watched failing
 * in a playground, and one per shape that was watched SUCCEEDING and would be
 * lost to a rule written one word too wide — a SELECT in a comment, the word
 * FROM inside a string, an INSERT into an internal table.
 *
 * The failure this guards against is not a red page. It is a Run button on an
 * example that cannot run: the reader clicks it, waits several seconds for an
 * ABAP runtime to boot, and is shown an error message about the documentation
 * they were reading. Every rule here therefore fails towards NO button.
 *
 *   node --test test/
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { abapOnly, playgroundExample } from '../docs/.vitepress/playground.mjs';

/** A complete app class, with `body` as the whole of `main`. */
const app = (name, body) => `CLASS ${name} DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS ${name} IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
${body}
  ENDMETHOD.
ENDCLASS.`;

const DISPLAY = '    client->message_box_display( `Hello` ).';

const runs = (code) => playgroundExample(code).name;
const refused = (code) => playgroundExample(code).why;

test('a complete app class that displays something gets a button', () => {
  assert.equal(runs(app('zcl_app_hello', DISPLAY)), 'zcl_app_hello');
});

test('the name comes from the class, because the file is named after it', () => {
  // The playground puts the code in a file called after the class and refuses
  // the pair when they disagree - abapGit's rule, and abaplint's.
  assert.equal(runs(app('z2ui5_cl_sample_tab', DISPLAY)), 'z2ui5_cl_sample_tab');
});

test('a fragment of a method gets nothing - there is no object to compile', () => {
  assert.match(refused('view->ele( `Page` )->a( n = `title` v = `x` ).'), /not a complete class/);
});

test('a class that is not an app gets nothing', () => {
  const helper = `CLASS zcl_helper DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS do.
ENDCLASS.
CLASS zcl_helper IMPLEMENTATION.
  METHOD do.
  ENDMETHOD.
ENDCLASS.`;
  assert.match(refused(helper), /z2ui5_if_app/);
});

test('a class name longer than 30 characters gets nothing, and says why', () => {
  // Not a playground limit: an ABAP object name is 30 characters, so this is a
  // class nobody can create anywhere. Two of them were printed here for years,
  // invisible to check:examples because it renames every example before
  // compiling it.
  const why = refused(app('z2ui5_cl_sample_nested_structures', DISPLAY));
  assert.match(why, /33 characters/);
});

test('an app that displays nothing gets nothing', () => {
  // An app that computes and keeps it to itself. It starts perfectly well and
  // shows an empty frame, which demonstrates nothing.
  //
  // This used to be pinned with configuration/authorization.md's authority
  // check, which is the shape the rule was written for - that example is now
  // refused one rule earlier, for having no user to check, so the two are
  // pinned apart.
  const code = app('z2ui5_cl_app', `    DATA(lv_total) = 1 + 1.
    IF lv_total <> 2.
      RETURN.
    ENDIF.`);
  assert.match(refused(code), /displays nothing/);
});

test('a SELECT from a business table gets nothing', () => {
  const code = app('z2ui5_cl_sample_sql', `    SELECT FROM sflight FIELDS * INTO TABLE @DATA(rows).
${DISPLAY}`);
  assert.match(refused(code), /sflight/);
});

test('a SELECT from a table the page does have keeps its button', () => {
  // technical/dx.md reads T100, which open-abap ships and which therefore is
  // there. A rule that refused every SELECT would have taken this one too.
  assert.equal(runs(app('zcl_app_alv', `    SELECT FROM t100 FIELDS * INTO TABLE @DATA(rows).
${DISPLAY}`)), 'zcl_app_alv');
});

test('a SELECT in a comment is not a SELECT', () => {
  // get_started/full_example.md prints the SELECT a reader would write, as a
  // comment, above the demo data it uses instead - and it runs.
  const code = app('zcl_app_full_example', `    " in your system, replace this with a SELECT, e.g.:
    " SELECT order_id FROM zsd_order INTO TABLE @DATA(rows)
${DISPLAY}`);
  assert.equal(runs(code), 'zcl_app_full_example');
});

test('the word FROM inside a string is not a table', () => {
  // cookbook/browser_interaction/clipboard.md says `Hello from abap2UI5`.
  const code = app('z2ui5_cl_sample_clipboard', `    DATA(text) = \`Hello from abap2UI5\`.
${DISPLAY}`);
  assert.equal(runs(code), 'z2ui5_cl_sample_clipboard');
});

test('INSERT VALUE #( ) into an internal table is not a database write', () => {
  const code = app('z2ui5_cl_app_table_basic', `    DATA rows TYPE string_table.
    INSERT VALUE #( ) INTO TABLE rows.
${DISPLAY}`);
  assert.equal(runs(code), 'z2ui5_cl_app_table_basic');
});

test('EML gets nothing', () => {
  const code = app('z2ui5_cl_sample_eml', `    READ ENTITIES OF i_salesorder IN LOCAL MODE ENTITY salesorder ALL FIELDS WITH VALUE #( ) RESULT DATA(rows).
${DISPLAY}`);
  assert.match(refused(code), /CDS entity|behavior definition/);
});

test('an add-on interface gets nothing', () => {
  const code = `CLASS z2ui5_cl_lp_kpi_hello DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    INTERFACES z2ui5_if_lp_kpi.
ENDCLASS.
CLASS z2ui5_cl_lp_kpi_hello IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
${DISPLAY}
  ENDMETHOD.
  METHOD z2ui5_if_lp_kpi~count.
  ENDMETHOD.
ENDCLASS.`;
  assert.match(refused(code), /add-on/);
});

test('an on-premise SAP class gets nothing', () => {
  for (const call of ['cl_bcs_message=>create( )', 'cl_demo_output=>get( )',
    'cl_abap_structdescr=>describe_by_name( `USR01` )']) {
    assert.match(refused(app('z2ui5_cl_sample', `    DATA(x) = ${call}.
${DISPLAY}`)), /on-premise/, call);
  }
});

test('an authority check gets nothing', () => {
  const code = app('z2ui5_cl_sample', `    AUTHORITY-CHECK OBJECT \`Z_APP_AUTH\` ID \`APP\` FIELD \`X\`.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
${DISPLAY}`);
  assert.match(refused(code), /no user and no roles/);
});

// and the words inside a literal are not one - abapOnly blanks them, the same
// guard the SELECT rules needed
test('the words of an authority check inside a literal are not one', () => {
  assert.ok(runs(app('z2ui5_cl_sample', `    DATA(x) = \`AUTHORITY-CHECK is explained below\`.
${DISPLAY}`)));
});

test('a method that is declared and never implemented gets nothing', () => {
  // Watched failing on cookbook/event_navigation/life_cycle.md, whose skeleton
  // declared render_main and on_post and implemented neither; the page has
  // since been completed and runs. The shape stays guarded because it is not a
  // playground limit - such a class does not activate in any system.
  const code = `CLASS z2ui5_cl_demo_app_001 DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
  PROTECTED SECTION.
    METHODS render_main.
ENDCLASS.
CLASS z2ui5_cl_demo_app_001 IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
${DISPLAY}
  ENDMETHOD.
ENDCLASS.`;
  assert.match(refused(code), /render_main/);
});

test('a local class gets nothing, defined here or not', () => {
  // cookbook/device_capabilities/spreadsheet.md calls lcl_help=>itab_get_by_xlsx.
  // Defining it in the same fence would not help: a playground file is one
  // abapGit object, and a class-local class is not one - abapGit puts it in a
  // .clas.locals_imp.abap beside the class, which there is nowhere to put here.
  const calls = app('z2ui5_cl_sample_upload', `    DATA(rows) = lcl_help=>itab_get_by_xlsx( \`x\` ).
${DISPLAY}`);
  assert.match(refused(calls), /lcl_help/);

  const defines = `CLASS lcl_help DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get RETURNING VALUE(r) TYPE string.
ENDCLASS.
CLASS lcl_help IMPLEMENTATION.
  METHOD get.
  ENDMETHOD.
ENDCLASS.
${app('z2ui5_cl_sample_upload', `    DATA(text) = lcl_help=>get( ).
${DISPLAY}`)}`;
  assert.match(refused(defines), /lcl_help/);
});

test('comments go, string templates keep their braces', () => {
  const stripped = abapOnly([
    '* a full-line comment',
    'DATA(x) = 1. " a trailing one',
    'DATA(y) = |a { x } b|.',
    'DATA(z) = `a " b`.',
  ].join('\n'));
  assert.equal(stripped.includes('full-line'), false);
  assert.equal(stripped.includes('trailing'), false);
  assert.match(stripped, /DATA\(x\) = 1\./);
  // The delimiters stay so nothing shifts; a double quote inside a literal is
  // not the start of a comment.
  assert.match(stripped, /DATA\(z\) = ` {5}`\./);
});
