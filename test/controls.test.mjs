/*
 * The parser behind the generated custom-controls reference
 * (resources/controls.md), on one control fixture written the way the
 * frontend writes them: a `sap.ui.define` whose factory extends a base class
 * with a metadata object - a default that is an EXPRESSION, a definition
 * object handed to extend( ) by name, a hidden aggregation, an event with
 * and without parameters. A regex over the source would have printed
 * `String(_DEFAULT_TIMEOUT_MS)` as the default; this runs the module.
 *
 *   npm test
 */
import test from 'node:test';
import assert from 'node:assert/strict';

import { definedControls, describeControl, moduleSummary, usesControl } from '../scripts/lib/controls.mjs';

const fixture = `// Invisible control that reads the device position once after rendering
// into its bindable properties and fires \`finished\`.
//
// A second paragraph the summary leaves out.
sap.ui.define(
  ["sap/ui/core/Control", "z2ui5/core/Lib", "z2ui5/core/Env"],
  (Control, Lib, Env) => {
    "use strict";

    const _DEFAULT_TIMEOUT_MS = 5000;

    const definition = {
      metadata: {
        properties: {
          latitude: { type: "string", defaultValue: "" },
          timeout: { type: "string", defaultValue: String(_DEFAULT_TIMEOUT_MS) },
          enableHighAccuracy: { type: "boolean", defaultValue: false },
          rows: { type: "any", defaultValue: null },
          mode: { type: "string", defaultValue: Lib.SOME_MODE },
        },
        aggregations: {
          _content: { type: "sap.m.HBox", multiple: false, visibility: "hidden" },
        },
        events: {
          finished: { allowPreventDefault: true, parameters: {} },
          error: { parameters: { code: { type: "string" }, message: { type: "string" } } },
        },
      },
      init() {
        navigator.mediaDevices.addEventListener("devicechange", () => this.invalidate());
        window.setTimeout(() => Lib.logError("never runs"), 0);
      },
      renderer: { apiVersion: 2, render: Lib.renderInvisibleSpan },
    };
    for (const name of Object.keys(definition.metadata.properties)) {
      definition["set" + name] = function (v) { return this.setProperty(name, v, true); };
    }

    const Geolocation = Control.extend("z2ui5.cc.Geolocation", definition);
    return Env.ownClass(Geolocation);
  },
);
`;

test('the module is run, not matched: an expression default comes out evaluated', () => {
  const [control] = definedControls(fixture);
  assert.equal(control.name, 'z2ui5.cc.Geolocation');
  assert.equal(control.base, 'sap/ui/core/Control');
  assert.equal(control.metadata.properties.timeout.defaultValue, '5000');
});

test('the control as the page describes it', () => {
  const c = describeControl('Geolocation.js', fixture);
  assert.equal(c.tag, 'z2ui5:Geolocation');
  assert.equal(c.fullName, 'z2ui5.cc.Geolocation');
  assert.equal(c.file, 'app/webapp/cc/Geolocation.js');
  assert.deepEqual(c.properties.map((p) => [p.name, p.type, p.defaultValue]), [
    ['latitude', 'string', ''],
    ['timeout', 'string', '5000'],
    ['enableHighAccuracy', 'boolean', false],
    ['rows', 'any', null],
    /* a default read out of a dependency this reader does not have is
     * said to be that, never invented */
    ['mode', 'string', '(from a module)'],
  ]);
  assert.deepEqual(c.events, [
    { name: 'finished', parameters: [], allowPreventDefault: true },
    { name: 'error', parameters: [{ name: 'code', type: 'string' }, { name: 'message', type: 'string' }] },
  ]);
  assert.deepEqual(c.aggregations, [{ name: '_content', type: 'sap.m.HBox', multiple: false, visibility: 'hidden' }]);
});

test('the summary is the first paragraph of the leading comment', () => {
  assert.equal(moduleSummary(fixture), 'Invisible control that reads the device position once after rendering into its bindable properties and fires `finished`.');
});

test('a page uses a control as a tag, as a module name, or the way the builder writes it', () => {
  assert.ok(usesControl('a `<z2ui5:Geolocation>` tag', 'Geolocation'));
  assert.ok(usesControl('the module z2ui5.cc.Geolocation', 'Geolocation'));
  assert.ok(usesControl(')->tag( n = `Geolocation` ns = `z2ui5`', 'Geolocation'));
  assert.ok(!usesControl('the word Geolocation in prose', 'Geolocation'));
  assert.ok(!usesControl(')->tag( n = `Geolocation` ns = `sap`', 'Geolocation'));
});
