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

/* `public` is static assets, and since scripts/generate-llms.mjs it also holds
 * a raw-markdown COPY of every page, written on each build. Walking into it
 * would check every fence twice and, worse, report the copy under its copied
 * path - so a page on an allowlist here would be exempt under one name and
 * reported under the other. */
const walk = (dir) =>
  readdirSync(dir).flatMap((e) => {
    const p = join(dir, e);
    if (e === '.vitepress' || e === 'node_modules' || e === 'public') return [];
    return statSync(p).isDirectory() ? walk(p) : [p];
  });

/* RAP/EML and HANA-only SQL: real ABAP that needs a CDS entity, a behavior
 * definition or a HANA database to compile at all. abaplint has none of them
 * here, so the parse error is about this check's environment, not about the
 * example. */
const NEEDS_A_SYSTEM = /\bREAD\s+ENTITIES\b|\bMODIFY\s+ENTITIES\b|\bCOMMIT\s+ENTITIES\b|TABLE\s+FOR\s+(READ|CREATE|UPDATE)\b|\bFUZZY\b|CONTAINS\s*\(/i;

const skipped = [];
const unmigrated = [];

/* The FRAGMENT gate. Everything above needs a whole class, and most of the
 * view code in this documentation is not one - it is four lines showing how a
 * binding string is written. Those fragments are the majority of what a reader
 * copies, and nothing looked at them: thirty of them were still calling
 * `)->input( )`, `)->column( )`, `)->checkbox( )` on pages whose complete
 * examples had long been migrated.
 *
 * They cannot be compiled, so this is the one thing that can be checked
 * without a system: the CURRENT builder has five verbs, and a chain calling
 * anything else is calling a method of the frozen one. */
const VERBS = new Set(['ele', 'tag', 'a', 'end', 'stringify']);

/* Pages that show the previous builder ON PURPOSE. The deprecation record is
 * a before/after table - "this is what you had, this is what you write now" -
 * so the old call is the content, not a leftover. */
const SHOWS_THE_OLD_API = new Set(['docs/resources/deprecations.md']);

function legacyFragments() {
  const out = [];
  for (const file of walk(DOCS).filter((f) => f.endsWith('.md')).sort()) {
    const page = file.slice(ROOT.length + 1);
    if (SHOWS_THE_OLD_API.has(page)) continue;
    const md = readFileSync(file, 'utf8');
    if (md.includes('This page still shows the previous view builder')) continue;
    for (const m of md.matchAll(/```abap\n([\s\S]*?)```/g)) {
      /* The other fluent API in this documentation. `z2ui5_cl_ajson` chains
       * the same way and its verbs are its own, so a fence building JSON is
       * not a view chain. Listed rather than guessed at from the receiver
       * name: most view fragments here start mid-chain, with no receiver to
       * read, and a heuristic that needs one would skip exactly the fragments
       * this gate exists for. */
      if (/z2ui5_cl_ajson/i.test(m[1])) continue;
      for (const call of m[1].matchAll(/\)->([a-z_][a-z0-9_]*)\(/gi)) {
        if (!VERBS.has(call[1].toLowerCase())) out.push({ page, call: `)->${call[1]}(` });
      }
    }
  }
  return out;
}

const legacy = legacyFragments();
if (legacy.length) {
  const byPage = new Map();
  for (const { page, call } of legacy) byPage.set(page, [...new Set([...(byPage.get(page) || []), call])]);
  console.error(`check-examples: ${legacy.length} view chain step(s) on ${byPage.size} page(s) call a method of the`);
  console.error('frozen builder. The current one has five verbs: ele, tag, a, end, stringify.\n');
  for (const [page, calls] of byPage) console.error(`  - ${page}: ${calls.join(' ')}`);
  console.error('\nA fragment cannot be compiled, so this is the only check it gets — and it is');
  console.error('the code a reader copies most. Rewrite it with the current builder.');
  process.exit(1);
}

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
      /* An example built on the FROZEN builder. Scope alone would let it
       * through - this check only picks up classes naming the current builder,
       * so a page that was never migrated is invisible to it rather than
       * failing it. That is how app_state_share.md kept a `z2ui5_cl_xml_view`
       * chain through a migration that touched every other page. A page
       * carrying the banner is declaring itself unmigrated and is exempt; a
       * page without one is claiming to be done. */
      if (/z2ui5_cl_xml_view\s*=>/i.test(code) && !pending) {
        unmigrated.push(file.slice(ROOT.length + 1));
        continue;
      }
      if (!/z2ui5_cl_ui5_view_builder/i.test(code)) continue;
      /* A class whose ABAP needs artefacts this check cannot clone. abaplint
       * parses against the framework and the released API mirror, and neither
       * carries a CDS entity, a behavior definition or a HANA-only SQL dialect
       * - so the example is refused for what it depends on rather than for
       * anything wrong with it, and refusing it says nothing about the
       * documentation. The VIEW half of these pages is exactly the same chain
       * as everywhere else and is read by the reader, not by this script. */
      if (NEEDS_A_SYSTEM.test(code)) { skipped.push(file.slice(ROOT.length + 1)); continue; }
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

/* DDIC fixtures. An example may read and write a Z table that the READER
 * creates: lock.md's soft lock is a row in a custom table, and the page prints
 * its schema field by field for exactly that reason. abaplint cannot clone a
 * table that exists only in the reader's system, and the alternative - putting
 * the whole example out of scope - would leave its view chain unchecked for
 * the sake of four DDIC fields. So the schema the PAGE documents is
 * materialized here and the example is compiled in full.
 *
 * A fixture is therefore a claim about a page, and `matchesItsPage` holds it
 * to one: every field and type below has to appear on the page, on one line,
 * the way the page prints its schema. Change the table on the page and this
 * fails until the fixture follows. */
const DDIC = [
  {
    name: 'z2ui5_sample_01',
    text: 'soft lock (documentation example)',
    page: 'docs/cookbook/expert_more/lock.md',
    fields: [
      ['MANDT', 'MANDT', true],
      ['VBELN', 'VBELN_VA', true],
      ['USERNAME', 'SYUNAME', false],
      ['LOCKED_AT', 'TIMESTAMPL', false],
    ],
  },
];

const tabl = ({ name, text, fields }) => `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_TABL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD02V>
    <TABNAME>${name.toUpperCase()}</TABNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <TABCLASS>TRANSP</TABCLASS>
    <CLIDEP>X</CLIDEP>
    <DDTEXT>${text}</DDTEXT>
    <MASTERLANG>E</MASTERLANG>
    <CONTFLAG>A</CONTFLAG>
    <EXCLASS>1</EXCLASS>
   </DD02V>
   <DD09L>
    <TABNAME>${name.toUpperCase()}</TABNAME>
    <AS4LOCAL>A</AS4LOCAL>
    <TABKAT>0</TABKAT>
    <TABART>APPL0</TABART>
    <BUFALLOW>N</BUFALLOW>
   </DD09L>
   <DD03P_TABLE>
${fields.map(([field, type, key]) => `    <DD03P>
     <FIELDNAME>${field}</FIELDNAME>${key ? '\n     <KEYFLAG>X</KEYFLAG>' : ''}
     <ROLLNAME>${type}</ROLLNAME>
     <ADMINFIELD>0</ADMINFIELD>
     <NOTNULL>X</NOTNULL>
     <COMPTYPE>E</COMPTYPE>
    </DD03P>`).join('\n')}
   </DD03P_TABLE>
  </asx:values>
 </asx:abap>
</abapGit>`;

/** The page has to still print the schema this fixture supplies. */
function matchesItsPage({ name, page, fields }) {
  const md = readFileSync(join(ROOT, page), 'utf8');
  if (!md.toLowerCase().includes(name)) return `${page} no longer mentions ${name}`;
  const missing = fields
    .filter(([field, type]) => !md.split('\n').some((l) => l.includes(field) && l.includes(type)))
    .map(([field, type]) => `${field} ${type}`);
  return missing.length ? `${page} no longer documents ${name}: ${missing.join(', ')}` : null;
}

const drifted = DDIC.map(matchesItsPage).filter(Boolean);
if (drifted.length) {
  console.error('check-examples: a DDIC fixture no longer matches the page it stands in for:\n');
  for (const d of drifted) console.error(`  - ${d}`);
  console.error('\nThe fixture exists so the example compiles against the schema the page');
  console.error('prints. Update the fixture in this script, or the schema on the page.');
  process.exit(1);
}

const all = examples();
const pending = all.filter((e) => e.pending);
const found = all.filter((e) => !e.pending);
const pendingPages = new Set(pending.map((e) => e.file));
if (unmigrated.length) {
  const pages = [...new Set(unmigrated)];
  console.error(
    `check-examples: ${unmigrated.length} example(s) on ${pages.length} page(s) still build their view with`,
  );
  console.error('z2ui5_cl_xml_view, which is frozen, while the page carries no banner saying so:\n');
  for (const page of pages) console.error(`  - ${page}`);
  console.error('\nMigrate the example to z2ui5_cl_ui5_view_builder — that is what puts it under');
  console.error('this gate. A page that cannot be migrated yet says so with the banner.');
  process.exit(1);
}
if (found.length === 0) {
  console.error('check-examples: no view-building class example found — has the fence language or the builder name changed?');
  process.exit(1);
}

const dir = mkdtempSync(join(os.tmpdir(), 'a2ui5-docs-'));
mkdirSync(join(dir, 'src'));
const origin = new Map();

for (const fixture of DDIC) writeFileSync(join(dir, 'src', `${fixture.name}.tabl.xml`), tabl(fixture));

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
  rules: {
    // 24 examples on 12 pages gate their display on check_on_init( ) alone.
    // That is a real defect and not a documentation-only one: an app reached
    // by navigation - a called app returning, a value help closing, a
    // bookmarked state restored - leaves the previous view on screen with no
    // error anywhere. An example that teaches it is worse than an app that
    // has it, because every one of them is copied.
    //
    // Not silenced, and not fixed here either. The fix is not a line: each of
    // these builds its view INLINE inside the IF branch, so it needs a
    // view_display( ) method extracted and called from both branches - and on
    // several of these pages the prose walks through the code as it stands.
    // That is R-6 in the repository plan, which covers the same defect across
    // 530 apps in the other repositories and is being worked separately. The
    // documentation examples belong to that decision rather than to a
    // different one made here in passing. Held as a hint until then, which is
    // how samples, samples-stack and samples-controls carry it too.
    //
    // Remove this entry when R-6 reaches these pages: `npm run check:examples`
    // then reports them as findings again, and the count has to be zero.
    'missing-on-navigated-branch': 'hint',
  },
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
if (skipped.length) {
  const pages = [...new Set(skipped)];
  console.log(
    `             ${skipped.length} example(s) on ${pages.length} page(s) need a CDS entity, a behavior`
    + ' definition or a HANA database to compile and were not built:',
  );
  for (const page of pages) console.log(`               ${page}`);
}
