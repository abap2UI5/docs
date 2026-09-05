/*
 * pages — the site's own pages, as the sidebar declares them, plus the two
 * things every generator wants out of one: its title and its first sentence.
 *
 * This was all inside generate-llms.mjs, which was the only program that
 * needed it. The search index is the second, and a second copy of "what is a
 * page of this site" is exactly the kind of copy that answers differently six
 * months later — the sidebar grows a section, one generator learns about it
 * and the other does not. So the walk lives here, both import it, and a page
 * added to the sidebar is added to llms.txt AND to the search in one edit.
 *
 * The sidebar is the source, not the file tree: a page in no sidebar is a page
 * nobody navigates to. Those exist (generate-llms.mjs reports them), and each
 * caller decides what to do with them — `orphanPages( )` is that list.
 */
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import config from '../../docs/.vitepress/config.mjs';

export const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
export const DOCS = path.join(ROOT, 'docs');

/** Where this site is published. */
export const SITE = 'https://abap2ui5.github.io/docs';

/** Every sidebar entry with a local link, in sidebar order, first one wins:
 *  several groups deliberately point their own heading at their first page. */
export function sidebarPages() {
  const seen = new Set();
  const out = [];
  const walk = (nodes, section) => {
    for (const node of nodes || []) {
      // a top-level group names the section everything under it belongs to
      const here = section ?? node.text;
      if (node.link?.startsWith('/') && !seen.has(node.link)) {
        seen.add(node.link);
        out.push({ section: here, text: node.text, link: node.link });
      }
      walk(node.items, here);
    }
  };
  walk(config.themeConfig.sidebar, null);
  return out;
}

export function markdownFiles(dir, out = []) {
  for (const name of fs.readdirSync(dir)) {
    if (name === '.vitepress' || name === 'public' || name === 'node_modules') continue;
    const full = path.join(dir, name);
    if (fs.statSync(full).isDirectory()) markdownFiles(full, out);
    else if (full.endsWith('.md')) out.push(full);
  }
  return out;
}

/* A page link and the file behind it are NOT a plain `.md` suffix apart.
 * VitePress serves `<dir>/index.md` at `<dir>/`, so the trailing slash IS the
 * index page, and `docs/index.md` (the home page) is the same rule at the
 * root - which is why the orphan filter below used to carry a hand-written
 * `/index` exception. Everything that converts between the two goes through
 * mdSuffix, or a directory index resolves to `<dir>/.md` and takes the build
 * down, which is exactly what happened the first time a section grew one. */
export const mdSuffix = (link) => (link.endsWith('/') ? `${link}index.md` : `${link}.md`);
export const linkOf = (file) => {
  const rel = path.relative(DOCS, file).replace(/\\/g, '/');
  if (rel === 'index.md') return '/';
  if (rel.endsWith('/index.md')) return `/${rel.slice(0, -'index.md'.length)}`;
  return `/${rel.replace(/\.md$/, '')}`;
};
export const fileOf = (link) => path.join(DOCS, mdSuffix(link).slice(1));

/** The pages that exist and sit in no sidebar — published, navigated to by
 *  nothing. The home page is not one of them: the brand and the bar's first
 *  item are its navigation. */
export function orphanPages(pages = sidebarPages()) {
  const linked = new Set(pages.map((p) => p.link));
  return markdownFiles(DOCS).map(linkOf).filter((l) => !linked.has(l) && l !== '/').sort();
}

/* ------------------------------------------------------------ the content */

export const stripFrontmatter = (text) => text.replace(/^---\r?\n[\s\S]*?\r?\n---\r?\n/, '');

/** The first real sentence of a page, for the one-line note in the index.
 *
 *  The WHOLE first paragraph is collected before cutting: the sources are hard
 *  wrapped, so stopping at the newline would end half the notes mid-sentence.
 *
 *  Link syntax and `*` emphasis are flattened, and backticks with them - but
 *  NOT the underscore. Almost every identifier this documentation is about
 *  carries one (`_bind`, `check_on_init`, `s_device`), and an index that
 *  advertises `client->bind( )` is worse than no index: it is a plausible,
 *  citable, wrong API name aimed at the one reader least able to notice. */
export function summarise(body) {
  const lines = stripFrontmatter(body).split('\n');
  const para = [];
  let inFence = false;
  for (const line of lines) {
    if (line.startsWith('```')) { inFence = !inFence; continue; }
    if (inFence) continue;
    const t = line.trim();
    if (!para.length) {
      if (!t || t.startsWith('#') || t.startsWith(':::') || t.startsWith('|') || t.startsWith('<')) continue;
      para.push(t);
      continue;
    }
    if (!t) break; // the paragraph ended
    para.push(t);
  }
  const plain = para.join(' ')
    .replace(/\[([^\]]+)\]\([^)]*\)/g, '$1')
    .replace(/[*`]/g, '')
    .replace(/\s+/g, ' ')
    .trim();
  if (!plain) return '';
  // cut at the end of the first sentence, but not so early that the note says
  // nothing; a period inside `sap.m.Table` or `1.71` is not a sentence end
  const stop = plain.search(/\.(?=\s|$)|:\s—|\s—\s/);
  const first = stop > 40 ? plain.slice(0, stop + 1) : plain;
  return first.length > 220 ? `${first.slice(0, 217)}...` : first;
}

export const title = (body, fallback) =>
  (stripFrontmatter(body).match(/^#\s+(.+?)\s*$/m) || [, fallback])[1].trim();

/** Every `##`/`###` heading on a page, flattened — what a reader is actually
 *  looking for when they type two words into a search box. Anchors are
 *  VitePress's own slug: lower case, non-word runs to a dash. */
export function headings(body) {
  const out = [];
  let inFence = false;
  for (const line of stripFrontmatter(body).split('\n')) {
    if (line.startsWith('```')) { inFence = !inFence; continue; }
    if (inFence) continue;
    const m = /^(#{2,3})\s+(.+?)\s*$/.exec(line);
    if (!m) continue;
    const text = m[2].replace(/\[([^\]]+)\]\([^)]*\)/g, '$1').replace(/[*`]/g, '').trim();
    if (!text) continue;
    out.push({
      text,
      anchor: text.toLowerCase().replace(/[^\p{L}\p{N}]+/gu, '-').replace(/^-|-$/g, ''),
    });
  }
  return out;
}

/* Words that are in every page of every documentation and therefore identify
 * none of it. Kept short on purpose: this is a size measure, not a language
 * model, and a word wrongly dropped is a search that finds nothing. */
const EVERYWHERE = new Set(('a an and are as at be by can for from has have in into is it its not of on or that the'
  + ' this to with you your will would there their they them then than so if but do does what when which who how'
  + ' one two also all any each more most other some such only own same too very just about after before both')
  .split(' '));

/** Every distinct word on a page, for a search that has to find a chapter by a
 *  term that appears in its third paragraph.
 *
 *  Titles and headings are matched separately and weigh far more; this is the
 *  long tail - the page that explains carousels without the word in any
 *  heading. Kept as DISTINCT words rather than as the text: the index is
 *  fetched by a reader who is mid-keystroke, and prose repeated verbatim is
 *  weight bought for nothing. Fenced code is deliberately included - a control
 *  or a method name is exactly what somebody types into this box. */
export function terms(body) {
  const seen = new Set();
  /* The same split the query goes through in theme/search-engine.js: the dot
   * and the underscore stay INSIDE a word, because `sap.m.Table` and
   * `nav_app_call` are single things a reader types, and everything else -
   * `->` above all - is a boundary. The two rules have to agree or a word
   * indexed as one token is searched for as two. */
  for (const w of stripFrontmatter(body).toLowerCase().split(/[^\p{L}\p{N}._]+/u)) {
    const word = w.replace(/^[._]+|[._]+$/g, '');
    if (word.length < 3 || word.length > 40 || EVERYWHERE.has(word)) continue;
    if (/^\d+$/.test(word)) continue;
    seen.add(word);
  }
  return [...seen].join(' ');
}
