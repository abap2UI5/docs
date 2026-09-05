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
import { sidebarPages, fileOf, summarise, title, headings, terms, SITE } from './pages.mjs';

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
      text: summarise(body),
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
export async function buildIndex(root, { fetchFn = globalThis.fetch, log = () => {} } = {}) {
  const entries = docEntries();
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

  return { built: new Date().toISOString(), areas, entries };
}
