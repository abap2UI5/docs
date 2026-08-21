/*
 * client-interface — where z2ui5_if_client comes from, and how to read it.
 *
 * Two gates need the same file and must not drift apart in how they get it:
 *
 *   check-api-names          holds every `client->` name on the site against
 *                            the interface, so a page cannot teach API the
 *                            reader's install does not have
 *   generate-api-reference   renders the interface INTO a page (and a JSON),
 *                            and in --check mode holds the committed copy
 *                            against the interface
 *
 * Both fetch the SAME source at the SAME pin: the raw file at the release the
 * site names (lib/release.mjs), never main - a reader installs a release, and
 * main is ahead of it by definition.
 *
 * The full parser lives here too, used by generate-api-reference. It answers
 * a richer question than check-api-names asks - not "does this name exist"
 * but "what is everything, with its types, defaults, values and the ABAP-Doc
 * written on it". check-api-names keeps its own narrow parse on purpose: its
 * failure mode is pinned by 1543 real names on the pages, and a shared
 * document model would couple the gate that guards the prose to the renderer
 * that produces some of it.
 */

/** The one file this documentation calls "the client API". */
export const interfacePath = 'src/02/z2ui5_if_client.intf.abap';

export const interfaceUrl = (ref) =>
  `https://raw.githubusercontent.com/abap2UI5/abap2UI5/${ref}/${interfacePath}`;

/** The interface source at a pinned ref. Throws on any network or HTTP
 *  failure - the CALLER decides whether that skips or fails, because a gate
 *  must not go red over an unreachable github.com and must not claim to have
 *  verified something it did not. */
export async function fetchInterface(ref) {
  const res = await fetch(interfaceUrl(ref), { signal: AbortSignal.timeout(20000) });
  if (!res.ok) throw new Error(`HTTP ${res.status}`);
  return res.text();
}

/* ---------------------------------------------------------------- parsing */

/** ABAP-Doc escapes, undone. The parsed model carries PLAIN text; whoever
 *  renders it into markdown or HTML escapes for that target again. */
const decode = (s) => s
  .replace(/&lt;/g, '<')
  .replace(/&gt;/g, '>')
  .replace(/&amp;/g, '&')
  .replace(/\\([{}|])/g, '$1');

/** Hard-wrapped ABAP-Doc lines, folded back into paragraphs: an empty
 *  `"!` line is a paragraph break, everything else joins with a space. */
function paragraphs(lines) {
  const out = [];
  let cur = [];
  for (const line of lines) {
    if (line.trim() === '') {
      if (cur.length) { out.push(cur.join(' ')); cur = []; }
      continue;
    }
    cur.push(line.trim());
  }
  if (cur.length) out.push(cur.join(' '));
  return out;
}

/** An ABAP-Doc block, split into the general text and its `@parameter x | …`
 *  entries (each entry runs until the next tag). */
function splitDoc(rawLines) {
  const lines = rawLines.map(decode);
  const general = [];
  const params = new Map();
  let current = null;
  for (const line of lines) {
    const tag = /^@parameter\s+([a-z0-9_]+)\s*\|\s*(.*)$/i.exec(line.trim());
    if (tag) {
      current = tag[1].toLowerCase();
      params.set(current, [tag[2]]);
      continue;
    }
    if (current) params.get(current).push(line.trim());
    else general.push(line);
  }
  return {
    text: paragraphs(general),
    params: new Map([...params].map(([k, v]) => [k, v.join(' ').replace(/\s+/g, ' ').trim()])),
  };
}

/**
 * The whole interface as a document model:
 *
 *   {
 *     constants: [ { name, doc: [para…], members: [member|group…] } ]
 *     types:     [ { name, doc, members } | { name, doc, definition } ]
 *     methods:   [ { name, doc: [para…], preferred,
 *                    params: [ { name, type, default, optional, doc } ],
 *                    returning: { name, type } } ]
 *   }
 *
 * A nested BEGIN OF inside a group is a member with `members` of its own. A
 * plain `"…` comment line inside a constant group is a LABEL for the members
 * after it (cs_event marks its runs `Control` / `experimental` / `obsolet`
 * that way) and lands on each of them as `label`; the same comment before a
 * method parameter is a note on that one parameter and lands as `doc`.
 */
export function parseInterface(text) {
  const model = { constants: [], types: [], methods: [] };
  let doc = [];          // pending "! lines
  let note = [];         // pending plain " lines (method-parameter notes)
  let mode = null;       // 'constants' | 'types' after the introducing keyword
  const stack = [];      // open BEGIN OF groups
  let label = null;      // current run label inside the innermost group
  let method = null;     // the method being read
  let section = null;    // 'importing' | 'returning' | …

  const takeDoc = () => { const d = doc; doc = []; return d; };
  const takeNote = () => { const n = note; note = []; return n.map(decode); };

  const closeMethod = () => {
    note = [];
    if (!method) return;
    const { text: docText, params } = splitDoc(method.rawDoc);
    delete method.rawDoc;
    method.doc = docText;
    for (const p of method.params) {
      const tagged = params.get(p.name);
      if (tagged) p.doc = p.doc ? `${p.doc} ${tagged}` : tagged;
    }
    model.methods.push(method);
    method = null;
    section = null;
  };

  for (const raw of text.split(/\r?\n/)) {
    const abapDoc = /^\s*"!\s?(.*)$/.exec(raw);
    if (abapDoc) { doc.push(abapDoc[1]); continue; }
    const comment = /^\s*"(?!!)\s?(.*)$/.exec(raw);
    if (comment) {
      if (stack.length) label = decode(comment[1].trim());
      else note.push(comment[1].trim());
      continue;
    }

    const line = raw.trimEnd();
    if (!line.trim()) continue;
    const ends = /\.\s*$/.test(line);           // the statement closes here
    const stmt = line.replace(/[,.]\s*$/, '');  // …either way, drop the mark

    const begin = /^\s*BEGIN OF ([a-z0-9_]+)$/i.exec(stmt);
    if (begin) {
      const group = { name: begin[1].toLowerCase(), doc: paragraphs(takeDoc().map(decode)), members: [] };
      if (stack.length) stack.at(-1).members.push(group);
      stack.push(group);
      label = null;
      continue;
    }
    const end = /^\s*END OF ([a-z0-9_]+)$/i.exec(stmt);
    if (end) {
      const group = stack.pop();
      label = null;
      if (!stack.length) (mode === 'types' ? model.types : model.constants).push(group);
      continue;
    }
    if (stack.length) {
      const member = /^\s*([a-z0-9_]+)\s+TYPE\s+(.+?)(?:\s+VALUE\s+`([^`]*)`)?$/i.exec(stmt);
      if (member) {
        stack.at(-1).members.push({
          name: member[1].toLowerCase(),
          type: member[2].trim(),
          ...(member[3] !== undefined ? { value: member[3] } : {}),
          ...(label ? { label } : {}),
        });
      }
      continue;
    }

    if (/^\s*CONSTANTS:?$/i.test(stmt)) { mode = 'constants'; continue; }
    if (/^\s*TYPES:?$/i.test(stmt)) { mode = 'types'; continue; }
    const flatType = /^\s*TYPES\s+([a-z0-9_]+)\s+TYPE\s+(.+)$/i.exec(stmt);
    if (flatType) {
      model.types.push({
        name: flatType[1].toLowerCase(),
        doc: paragraphs(takeDoc().map(decode)),
        definition: flatType[2].trim(),
      });
      continue;
    }

    const decl = /^\s*METHODS\s+([a-z0-9_]+)$/i.exec(stmt);
    if (decl) {
      closeMethod();
      method = { name: decl[1].toLowerCase(), rawDoc: takeDoc(), params: [], returning: null, preferred: null };
      section = null;
      if (ends) closeMethod();
      continue;
    }
    if (method) {
      if (/^\s*IMPORTING$/i.test(stmt)) { section = 'importing'; continue; }
      if (/^\s*(RETURNING|EXPORTING|CHANGING|RAISING)$/i.test(stmt)) { section = stmt.trim().toLowerCase(); if (ends) closeMethod(); continue; }
      const preferred = /^\s*PREFERRED PARAMETER\s+([a-z0-9_]+)$/i.exec(stmt);
      if (preferred) { method.preferred = preferred[1].toLowerCase(); if (ends) closeMethod(); continue; }
      const param = /^\s*(?:VALUE\(([a-z0-9_]+)\)|([a-z0-9_]+))\s+TYPE\s+(.+)$/i.exec(stmt);
      if (param) {
        const name = (param[1] || param[2]).toLowerCase();
        let rest = param[3].trim();
        let optional = false;
        let dflt = null;
        const opt = /^(.*?)\s+OPTIONAL$/i.exec(rest);
        if (opt) { optional = true; rest = opt[1]; }
        const def = /^(.*?)\s+DEFAULT\s+(.+)$/i.exec(rest);
        if (def) { dflt = def[2].trim(); rest = def[1]; }
        // a plain comment standing before a parameter is a note on THAT
        // parameter (`_bind` marks `view` and `custom_mapper` obsolete this way)
        const noteText = takeNote().join(' ').replace(/\s+/g, ' ').trim();
        const entry = {
          name,
          type: rest.trim(),
          ...(dflt !== null ? { default: dflt } : {}),
          ...(optional ? { optional: true } : {}),
          ...(noteText ? { doc: noteText } : {}),
        };
        if (section === 'returning') method.returning = { name: entry.name, type: entry.type };
        else method.params.push(entry);
        if (ends) closeMethod();
        continue;
      }
      if (ends) { closeMethod(); continue; }
    }

    if (/^\s*ENDINTERFACE/i.test(stmt)) closeMethod();
  }
  closeMethod();
  return model;
}
