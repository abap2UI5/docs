/*
 * search-index — one index for the whole project, built here and read by all
 * four bars.
 *
 * The search in the bar used to be VitePress's own local search, which knows
 * exactly one thing: the pages of this site. That is half the project. A
 * reader who types "carousel" wants the cookbook chapter AND the sample that
 * builds one, and until now those were two searches on two sites, one of which
 * they had to know existed.
 *
 * So the index carries both areas:
 *
 *   docs     every page the sidebar declares, with its headings - the
 *            documentation, from lib/pages.mjs, which is also what llms.txt is
 *            built from
 *   samples  every entry in the three sample catalogues, with the title,
 *            summary and keywords the sample repositories maintain for it, and
 *            a link to that sample's own page in the catalogue
 *
 * The playground is not an area. It has no content to find: its URL carries
 * the code in the editor, and a result that opened an empty editor would
 * answer no question anybody typed. It is a destination in the bar, not in the
 * index — the same reasoning that keeps it out of the position memory.
 *
 * WHERE IT IS PUBLISHED, AND WHY THAT IS ENOUGH. `/docs/search-index.json`, on
 * the origin all four documents share, fetched by whichever of them the reader
 * opened - lazily, on the first keystroke, so a reader who never searches
 * never pays for it. One index, one URL, no build-time coupling between the
 * two repositories: the playground does not import this, it fetches it.
 *
 * It is a PROJECTION of pages and catalogues, so it is generated on every
 * build and gitignored, like llms.txt. Never commit it.
 */
import fs from 'node:fs';
import { entriesOf, loadCatalogue } from './catalogue.mjs';
import { sidebarPages, fileOf, describe, title, headings, terms, SITE } from './pages.mjs';

/** The three sample repositories, and where a reader is sent for a hit. */
export const CORPORA = [
  { repo: 'samples', label: 'Samples' },
  { repo: 'samples-controls', label: 'Controls' },
  { repo: 'samples-stack', label: 'Stack' },
];

/* The catalogue publishes one static page per sample, at /samples/<class>/ -
 * tools/sample-pages.mjs in abap2UI5/playground. A hit therefore lands on the
 * sample itself rather than on a catalogue filtered down to it. */
const CATALOGUE = 'https://abap2ui5.github.io/playground/samples';
const samplePage = (cls) => `${CATALOGUE}/${cls.toLowerCase()}/`;

/** The documentation half: one entry per page of this site. */
export function docEntries(pages = sidebarPages(), read = (link) => fs.readFileSync(fileOf(link), 'utf8')) {
  return pages.map((p) => {
    const body = read(p.link);
    return {
      area: 'docs',
      group: p.section,
      title: title(body, p.text),
      text: describe(body),
      /* The headings are the difference between finding a PAGE and finding the
       * paragraph somebody meant. They are matched against and shown as the
       * sub-hits under a page, each with its own anchor. */
      headings: headings(body).map((h) => [h.text, h.anchor]),
      /* And every other distinct word on the page, so a chapter can be found
       * by a term that never made it into a heading. Without this, "carousel"
       * answered with 30 samples and not one of the pages that explain how to
       * build one. */
      terms: terms(body),
      url: `${SITE}${p.link}${p.link.endsWith('/') ? '' : '.html'}`,
    };
  });
}

/** The samples half: one entry per catalogue entry, from whichever of the
 *  three catalogues could be reached. A repository that could not is left out
 *  and reported - it costs its rows, never the build, which is the rule every
 *  other reader of these catalogues here already follows. */
export function sampleEntries(catalogue, label) {
  return entriesOf(catalogue).map((e) => {
    const cls = String(e.class);
    /* The three repositories describe an entry with slightly different fields
     * - `description` and `category` here, `library`/`entity` in
     * samples-controls, `technology` in samples-stack. They are not
     * normalised into one shape: what is taken is what a reader would type,
     * whichever key it arrived under.
     *
     * What is NOT taken is anything already in `title` or `text`: the matcher
     * reads all three, and a field repeated into `terms` is a third of this
     * file's weight bought twice. */
    const keywords = Array.isArray(e.keywords) ? e.keywords.join(' ') : (e.keywords || '');
    return {
      area: 'samples',
      group: label,
      title: [e.title, e.description].filter(Boolean).join(' — ') || cls,
      text: e.summary || '',
      /* The class name is what half the searches here are: somebody has a
       * z2ui5_cl_smpc_app_207 in front of them and wants the sample it came
       * from. It is matched, and shown under the title. */
      code: cls.toLowerCase(),
      terms: [...new Set(
        `${keywords} ${e.entity || ''} ${e.library || ''} ${e.category || ''} ${e.technology || ''}`
          .toLowerCase().split(/[^\p{L}\p{N}._]+/u).filter(Boolean),
      )].join(' '),
      url: samplePage(cls),
    };
  });
}

/**
 * The whole index: `{ built, areas, entries }`.
 *
 * `areas` names what is in it, so the four bars can label a group of results
 * without knowing the three repositories by heart, and so a reader can see at
 * once when a corpus is missing from a build rather than wondering why their
 * sample is not there.
 */
/**
 * The words of a page that are worth carrying: not the ones its title, its
 * description or its headings already carry (the matcher reads all of them),
 * and not the ones that stand on more than a third of all pages.
 *
 * `terms` was 281 kB of a 630 kB index - the largest single field, and most
 * of it words like "client", "view", "class" and "framework", which are on
 * nearly every page and therefore decide nothing: a query for one of them
 * matches the manual wholesale and the ranking falls back on the title
 * anyway. A word on more than a third of the pages is left out; a word on a
 * hundredth of them - "carousel", "geolocation", "websocket" - is exactly
 * what this field exists for and stays. Measured on the pages as they are:
 * 281 kB became 96, and the index over the wire 180 kB gzip became about
 * 110, on the first request every reader who opens the search box makes.
 */
export function trimTerms(entries, { ceiling = 1 / 3 } = {}) {
  const docs = entries.filter((e) => e.area === 'docs' && typeof e.terms === 'string');
  const on = new Map();
  for (const e of docs) for (const w of new Set(e.terms.split(' '))) if (w) on.set(w, (on.get(w) || 0) + 1);
  const limit = Math.ceil(docs.length * ceiling);
  for (const e of docs) {
    const carried = new Set(
      `${e.title} ${e.text} ${(e.headings || []).map((h) => h[0]).join(' ')}`
        .toLowerCase().split(/[^\p{L}\p{N}._]+/u).map((w) => w.replace(/^[._]+|[._]+$/g, '')),
    );
    e.terms = e.terms.split(' ').filter((w) => w && !carried.has(w) && (on.get(w) || 0) <= limit).join(' ');
  }
  return entries;
}

/* ── THE WORD THE READER HAS FOR IT ────────────────────────────────────────
 *
 * "readonly" found nothing, and every page about it says `editable`; "F4" is
 * what half the readers call the value help. A pair is `[what is typed, what
 * the entry says]`: an entry that says the second gets the first, added to
 * `terms` - the long tail, at the long tail's weight, so the alias FINDS the
 * entry and the ranking stays with the words it really carries. One direction
 * each, on purpose: a reader who types "editable" has the project's word
 * already. Data rather than code, so it is not a second thing the four bars
 * have to carry a copy of.
 *
 * What is NOT here is the spelling with the space taken out - "messagebox"
 * for "Message Box": the matcher looks for that itself (`joined( )` in
 * theme/search-engine.js), at the weight of the field it is in.
 */
export const SYNONYMS = [
  ['readonly', 'editable'],
  ['f4', 'value help'],
  ['msg', 'message'],
  ['popup', 'dialog'],
  ['dialog', 'popup'],
  ['flp', 'launchpad'],
  ['launchpad', 'flp'],
  ['i18n', 'translation'],
  ['translation', 'i18n'],
  ['login', 'logon'],
  ['logon', 'login'],
  ['dropdown', 'select'],
  ['excel', 'xlsx'],
  ['xlsx', 'excel'],
];

/** The synonyms, added to every entry's `terms` that says the meaning and
 *  not the alias. */
export function enrich(entries, synonyms = SYNONYMS) {
  for (const e of entries) {
    const said = `${e.title} ${e.text} ${(e.headings || []).map((h) => h[0]).join(' ')} ${e.terms || ''}`.toLowerCase();
    const has = (w) => new RegExp(`(^|[^\\p{L}\\p{N}_])${w}([^\\p{L}\\p{N}_]|$)`, 'u').test(said);
    const extra = synonyms.filter(([alias, meaning]) => has(meaning) && !has(alias)).map(([alias]) => alias);
    if (extra.length) e.terms = [e.terms, ...extra].filter(Boolean).join(' ');
  }
  return entries;
}

export async function buildIndex(root, { fetchFn = globalThis.fetch, log = () => {} } = {}) {
  const entries = trimTerms(docEntries());
  const areas = [{ area: 'docs', label: 'Documentation', count: entries.length }];

  for (const { repo, label } of CORPORA) {
    const found = await loadCatalogue(repo, root, { fetchFn });
    if (!found) {
      log(`  ${repo}: no catalogue reachable — its samples are not in this index`);
      continue;
    }
    const rows = sampleEntries(found.catalogue, label);
    entries.push(...rows);
    areas.push({ area: 'samples', repo, label, count: rows.length });
    log(`  ${repo}: ${rows.length} sample(s) (${found.source})`);
  }

  return { built: new Date().toISOString(), areas, entries: enrich(entries) };
}
