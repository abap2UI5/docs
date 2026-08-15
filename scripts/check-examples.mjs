#!/usr/bin/env node
// Checks the ABAP examples in this documentation against the real thing.
//
// Why: this repository taught `z2ui5_cl_xml_view` on 52 of 122 pages long
// after the framework had frozen that class, and nothing noticed - the pages
// build, the links resolve, and prose has no compiler. The code in a fenced
// block is the one part of a documentation site that CAN be checked, so it is.
//
// Two gates, the same two every abap2UI5 project runs:
//
//   abaplint      does the class compile against the framework at all
//   abap2ui5lint  does the view it builds use controls, properties and enum
//                 values that exist on the UI5 floor this documentation
//                 targets - the property gate, which is what catches an
//                 example that drifted away from the API
//
// Scope: a complete class (DEFINITION and IMPLEMENTATION) that builds a view
// with z2ui5_cl_ui5_view_builder. That is where the drift this check exists
// for lives, and those examples are self-contained by nature.
//
// Deliberately out of scope, because failing on them would say nothing about
// the documentation: fragments showing three lines of a method (they cannot
// be compiled alone), ICF handler classes (if_http_extension is on-premise
// and in no API mirror this can clone), partial interface implementations
// used to show one method of z2ui5_if_exit, and examples built on an add-on
// repository such as the launchpad's z2ui5_if_lp_kpi. The limit is real: a
// drifted fragment still passes. Prefer a complete view-building class.
//
// Pages still carrying the "previous view builder" banner are skipped, and
// counted so the number stays visible. That is deliberate and it clears
// itself: migrating a page means deleting its banner, which is exactly what
// puts the page under this gate. The count only ever goes down.
//
// Usage: node scripts/check-examples.mjs [--keep]

import { execFileSync } from 'node:child_process';
import { mkdtempSync, mkdirSync, writeFileSync, readFileSync, rmSync, readdirSync, statSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import os from 'node:os';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');
const DOCS = join(ROOT, 'docs');
const KEEP = process.argv.includes('--keep');

const walk = (dir) =>
  readdirSync(dir).flatMap((e) => {
    const p = join(dir, e);
    if (e === '.vitepress' || e === 'node_modules') return [];
    return statSync(p).isDirectory() ? walk(p) : [p];
  });

/** Every fenced abap block that is a whole class. */
function examples() {
  const out = [];
  for (const file of walk(DOCS).filter((f) => f.endsWith('.md')).sort()) {
    const md = readFileSync(file, 'utf8');
    const pending = md.includes('This page still shows the previous view builder');
    for (const m of md.matchAll(/```abap\n([\s\S]*?)```/g)) {
      const code = m[1];
      if (!/CLASS\s+\S+\s+DEFINITION/i.test(code)) continue;
      if (!/CLASS\s+\S+\s+IMPLEMENTATION/i.test(code)) continue;
      if (!/z2ui5_cl_ui5_view_builder/i.test(code)) continue;
      out.push({ file: file.slice(ROOT.length + 1), code, pending });
    }
  }
  return out;
}

const sidecar = (name) => `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_CLAS" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <VSEOCLASS>
    <CLSNAME>${name.toUpperCase()}</CLSNAME>
    <LANGU>E</LANGU>
    <DESCRIPT>documentation example</DESCRIPT>
    <STATE>1</STATE>
    <CLSCCINCL>X</CLSCCINCL>
    <FIXPT>X</FIXPT>
    <UNICODE>X</UNICODE>
   </VSEOCLASS>
  </asx:values>
 </asx:abap>
</abapGit>`;

const all = examples();
const pending = all.filter((e) => e.pending);
const found = all.filter((e) => !e.pending);
const pendingPages = new Set(pending.map((e) => e.file));
if (found.length === 0) {
  console.error('check-examples: no view-building class example found — has the fence language or the builder name changed?');
  process.exit(1);
}

const dir = mkdtempSync(join(os.tmpdir(), 'a2ui5-docs-'));
mkdirSync(join(dir, 'src'));
const origin = new Map();

found.forEach((ex, i) => {
  const name = `zcl_docs_example_${String(i + 1).padStart(2, '0')}`;
  origin.set(name, ex.file);
  // the class name in the page is whatever the page chose - rename it to the
  // file it is written into, so several examples compile side by side and
  // abaplint's global_class (definition name == filename) holds
  const declared = /CLASS\s+(\S+)\s+DEFINITION/i.exec(ex.code)[1];
  const code = ex.code.replaceAll(declared, name);
  writeFileSync(join(dir, 'src', `${name}.clas.abap`), code);
  writeFileSync(join(dir, 'src', `${name}.clas.xml`), sidecar(name));
});

writeFileSync(join(dir, 'abaplint.json'), JSON.stringify({
  global: { files: '/src/**/*.*' },
  dependencies: [
    { url: 'https://github.com/abap2UI5/abap2UI5', files: '/src/**/*.*' },
    // the SAP standard API an example may implement (if_http_extension and
    // friends); same mirror abap2UI5/samples-stack lints against
    { url: 'https://github.com/abapedia/steampunk-2305-api', folder: '/deps', files: '/src/**/*.*' },
  ],
  syntax: { version: 'v750', errorNamespace: '^(Z|Y)' },
  rules: {
    check_syntax: true, parser_error: true, unknown_types: true,
    global_class: true, implement_methods: true, begin_end_names: true,
  },
}, null, 2));

writeFileSync(join(dir, 'abap2ui5lint.jsonc'), JSON.stringify({
  paths: ['src'],
  // the floor the documentation targets, and abap2UI5's own
  ui5: '1.71',
  distribution: 'openui5',
  // the render gate needs a browser and ~118 MB of UI5 sources; the property
  // gate is what decides whether an example names API that exists, which is
  // the drift this check is for
  render: false,
  failOn: 'warning',
}, null, 2));

console.log(`check-examples: ${found.length} view-building class example(s) from ${new Set(found.map((f) => f.file)).size} page(s)`);
for (const [name, file] of origin) console.log(`  ${name}  <-  ${file}`);
if (pending.length) {
  console.log(`\nnot checked yet: ${pending.length} example(s) on ${pendingPages.size} page(s) still`);
  console.log('carrying the previous-view-builder banner. Migrating a page removes its');
  console.log('banner and brings its examples in here — the number only goes down.');
}

let failed = false;
const run = (label, cmd, args) => {
  console.log(`\n--- ${label}`);
  try {
    execFileSync(cmd, args, { cwd: dir, stdio: 'inherit' });
  } catch {
    failed = true;
  }
};

const bin = (name) => join(ROOT, 'node_modules', '.bin', name);
run('abaplint (does it compile)', bin('abaplint'), ['abaplint.json']);
run('abap2ui5lint (does the view name real API)', bin('abap2ui5lint'), []);

if (KEEP) console.log(`\nkept: ${dir}`);
else rmSync(dir, { recursive: true, force: true });

if (failed) {
  console.error('\ncheck-examples: an example does not hold up. Fix the page, not this script —');
  console.error('a documentation example that does not compile is the defect it looks like.');
  process.exit(1);
}
console.log('\ncheck-examples: every view-building example compiles and names API that exists.');
