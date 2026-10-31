/*
 * controls — the custom controls the frontend ships, read out of their source.
 *
 * Every control under app/webapp/cc/ in abap2UI5/abap2UI5 is a UI5 module:
 * `sap.ui.define([deps], (Control, …) => Control.extend("z2ui5.cc.Name", {
 * metadata: { properties, events, aggregations }, … }))`. An app reaches one
 * from its view as `<z2ui5:Name>` under `xmlns:z2ui5="z2ui5.cc"`, and what it
 * can bind - the properties, the events it can wire to the backend - is
 * exactly that metadata object. Nothing on the site listed it; a reader had
 * to open the JavaScript.
 *
 * The metadata is NOT parsed with a regex. A default is sometimes an
 * expression (`String(_DEFAULT_TIMEOUT_MS)`, a module constant), one control
 * builds its definition object first and hands it to extend( ) by name, and a
 * regex that followed all of that would be a parser. Instead the module is
 * RUN, in a vm context with nothing in it but a `sap.ui.define` that calls
 * the factory with stand-in modules: every stand-in answers any property with
 * another stand-in and records the one call that matters, `extend(name,
 * definition)`. The control's methods never run - nothing instantiates the
 * class - so the browser APIs the file names inside them are never touched.
 *
 * The one thing a module can do at definition time that this cannot follow
 * is reach into a real dependency for a value (`Lib.SOMETHING` as a default):
 * it comes back as a stand-in, and `describeValue( )` prints such a default
 * as "(from a module)" rather than inventing a literal.
 *
 * Pinned by test/controls.test.mjs on one control fixture.
 */
import path from 'node:path';
import vm from 'node:vm';
import { execFileSync } from 'node:child_process';
import { fetchInterface, frameworkHome } from './client-interface.mjs';

/** Where the controls are in a clone of the framework. */
export const controlsDir = 'app/webapp/cc';

/** The namespace a view declares them under, and the prefix the view
 *  builder's `ns` names. */
export const NAMESPACE = 'z2ui5.cc';
export const PREFIX = 'z2ui5';

/* A stand-in for anything: a function so it can be called or constructed,
 * answering every property with another stand-in. `then` is left undefined
 * so an `await` on one does not hang, and toPrimitive answers '' so a
 * template string over one does not throw. */
const STAND_IN_MARK = Symbol('stand-in');
function standIn() {
  return new Proxy(function standIn() {}, {
    get: (target, prop) => {
      if (prop === STAND_IN_MARK) return true;
      if (prop === Symbol.toPrimitive) return () => '';
      if (prop === 'then') return undefined;
      if (prop === 'prototype') return target.prototype;
      return standIn();
    },
    apply: () => standIn(),
    construct: () => standIn(),
  });
}
export const isStandIn = (v) => Boolean(v && (typeof v === 'object' || typeof v === 'function') && v[STAND_IN_MARK]);

/**
 * The controls a module defines: `[{ name, base, metadata }]` - the full
 * name `extend( )` was given, the module it extends (as the dependency path,
 * `sap/m/ComboBox`), and the metadata object as the module wrote it.
 *
 * @param {string} source the module's JavaScript
 */
export function definedControls(source) {
  const found = [];
  const module = (dep) => new Proxy(function module() {}, {
    get: (target, prop) => {
      if (prop === 'extend') {
        return (name, definition) => {
          found.push({ name, base: dep, metadata: definition?.metadata ?? {} });
          return module(name);
        };
      }
      if (prop === Symbol.toPrimitive) return () => '';
      if (prop === 'then') return undefined;
      if (prop === 'prototype') return target.prototype;
      return standIn();
    },
    apply: () => standIn(),
    construct: () => standIn(),
  });
  const sandbox = {
    sap: { ui: { define: (deps, factory) => (typeof deps === 'function' ? deps() : factory(...deps.map(module))) } },
    console: { log() {}, warn() {}, error() {}, info() {}, debug() {} },
    String, Number, Boolean, Object, Array, Symbol, Math, JSON, Promise, Map, Set, WeakMap, WeakSet, Date, RegExp, Error, TypeError,
    window: standIn(), document: standIn(), navigator: standIn(), globalThis: standIn(), self: standIn(),
    setTimeout: () => 0, clearTimeout() {}, URL: standIn(), Blob: standIn(), FileReader: standIn(),
  };
  vm.runInNewContext(source, sandbox, { timeout: 2000 });
  return found;
}

/** The first paragraph of a module's leading `//` comment - what the file
 *  says it is, in its own words. ExcelBridge opens with sixty lines of them,
 *  so a paragraph is the unit, and a paragraph that runs on past the first
 *  sentences is cut at a sentence end before this many characters. */
const SUMMARY_MAX = 320;
export function moduleSummary(source) {
  const out = [];
  for (const line of source.split(/\r?\n/)) {
    const m = /^\s*\/\/\s?(.*)$/.exec(line);
    if (!m) break;
    if (!m[1].trim()) { if (out.length) break; else continue; }
    out.push(m[1].trim());
  }
  const text = out.join(' ').replace(/\s+/g, ' ').trim();
  if (text.length <= SUMMARY_MAX) return text;
  const head = text.slice(0, SUMMARY_MAX);
  const cut = head.lastIndexOf('. ');
  return cut > 40 ? head.slice(0, cut + 1) : `${head.trimEnd()}…`;
}

/** A metadata value as the page prints it, with no literal invented for a
 *  value that came out of a module this reader does not have. */
export function describeValue(v) {
  if (isStandIn(v)) return '(from a module)';
  if (v === undefined) return '';
  if (v === null) return 'null';
  if (typeof v === 'string') return v === '' ? '(empty)' : v;
  if (typeof v === 'function') return '(function)';
  if (Array.isArray(v) || typeof v === 'object') {
    try { return JSON.stringify(v); } catch { return String(v); }
  }
  return String(v);
}

/**
 * One control as the reference describes it:
 *
 *   { name: 'Geolocation', fullName: 'z2ui5.cc.Geolocation', tag: 'z2ui5:Geolocation',
 *     base, summary, file,
 *     properties: [{ name, type, defaultValue, bindable }],
 *     events:     [{ name, parameters: [{ name, type }], allowPreventDefault }],
 *     aggregations: [{ name, type, multiple, visibility }],
 *     defaultAggregation }
 */
export function describeControl(file, source) {
  const defined = definedControls(source);
  if (!defined.length) return null;
  /* the control the file is named after, or the first one defined */
  const stem = path.basename(file, '.js');
  const def = defined.find((d) => d.name.endsWith(`.${stem}`)) ?? defined[0];
  const m = def.metadata;
  const name = def.name.slice(def.name.lastIndexOf('.') + 1);
  const entries = (o) => Object.entries(o ?? {});
  return {
    name,
    fullName: def.name,
    tag: `${PREFIX}:${name}`,
    base: def.base,
    summary: moduleSummary(source),
    file: `${controlsDir}/${path.basename(file)}`,
    properties: entries(m.properties).map(([n, p]) => ({
      name: n,
      type: typeof p === 'string' ? p : describeValue(p?.type),
      defaultValue: typeof p === 'string' ? undefined
        : isStandIn(p?.defaultValue) || typeof p?.defaultValue === 'function' ? describeValue(p.defaultValue)
          : p?.defaultValue,
      ...(p?.bindable !== undefined ? { bindable: p.bindable } : {}),
    })),
    events: entries(m.events).map(([n, e]) => ({
      name: n,
      parameters: entries(e?.parameters).map(([pn, pd]) => ({ name: pn, type: typeof pd === 'string' ? pd : describeValue(pd?.type) })),
      ...(e?.allowPreventDefault ? { allowPreventDefault: true } : {}),
    })),
    aggregations: entries(m.aggregations).map(([n, a]) => ({
      name: n,
      type: typeof a === 'string' ? a : describeValue(a?.type),
      multiple: typeof a === 'string' ? true : a?.multiple !== false,
      ...(a?.visibility ? { visibility: a.visibility } : {}),
    })),
    ...(m.defaultAggregation ? { defaultAggregation: m.defaultAggregation } : {}),
  };
}

/** The control files at `ref`, by name. Out of a clone with `git ls-tree`
 *  at the ref - never its working tree, for the reason lib/client-interface
 *  gives - or, without one, from the repository's contents API, which is the
 *  one way to list a directory of GitHub without a clone. */
export async function listControlFiles(ref, { home = frameworkHome() } = {}) {
  if (home) {
    const tried = [];
    for (const spec of [ref, `origin/${ref}`]) {
      try {
        const out = execFileSync('git', ['-C', home, 'ls-tree', '--name-only', spec, `${controlsDir}/`], {
          encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'],
        });
        return out.split('\n').map((l) => l.trim()).filter((l) => l.endsWith('.js')).map((l) => path.basename(l)).sort();
      } catch (err) {
        tried.push(`${spec}: ${String(err.stderr ?? err.message).trim().split('\n')[0]}`);
      }
    }
    throw new Error(`not in the clone at ${home} - ${tried.join('; ')}`);
  }
  const url = `https://api.github.com/repos/abap2UI5/abap2UI5/contents/${controlsDir}?ref=${encodeURIComponent(ref)}`;
  /* GITHUB_TOKEN when the environment has one (CI does, check:version
   * already uses it): the unauthenticated limit of the contents API is 60
   * calls an hour PER ADDRESS, and a shared egress address spends that on
   * other people. Without a token the call is still made, and a refusal is
   * reported as "not resolved", which --check SKIPS on rather than fails. */
  const headers = process.env.GITHUB_TOKEN ? { authorization: `Bearer ${process.env.GITHUB_TOKEN}` } : {};
  const res = await fetch(url, { signal: AbortSignal.timeout(20000), headers });
  if (!res.ok) throw new Error(`HTTP ${res.status} listing ${controlsDir}`);
  const entries = await res.json();
  return entries.filter((e) => e.type === 'file' && e.name.endsWith('.js')).map((e) => e.name).sort();
}

/** Every control at `ref`, in file order: the listing above, then each file
 *  through the same reader the interfaces go through. */
export async function fetchControls(ref, { home = frameworkHome() } = {}) {
  const out = [];
  for (const file of await listControlFiles(ref, { home })) {
    const source = await fetchInterface(ref, { home, file: `${controlsDir}/${file}` });
    const control = describeControl(file, source);
    if (control) out.push(control);
  }
  return out;
}

/**
 * Where a page of the manual uses a control: a line that names it as a tag
 * (`z2ui5:Name`), by its module name (`z2ui5.cc.Name`), or the way the view
 * builder writes it - the control's name as a literal next to `ns = \`z2ui5\``.
 */
export function usesControl(markdown, name) {
  const tag = new RegExp(`\\b${PREFIX}:${name}\\b`);
  const module = new RegExp(`\\b${NAMESPACE.replace('.', '\\.')}\\.${name}\\b`);
  const chain = new RegExp(`\`${name}\`[^\\n]*\\bns\\s*=\\s*\`${PREFIX}\``);
  return tag.test(markdown) || module.test(markdown) || chain.test(markdown);
}
