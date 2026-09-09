/* THE MENU SAYS WHAT THE PAGE SAYS.
 *
 * Three things the sidebar in docs/.vitepress/config.mjs can quietly get
 * wrong, and did:
 *
 *   - An entry's label was not the page's own H1 on fourteen pages: the
 *     menu said "Install with abapGit" and the page said "Quickstart", the
 *     menu said "Production Use" and the page "Productive Usage". A search
 *     result shows the H1 and the menu shows the label, so the same page had
 *     two names a reader could not connect.
 *   - A section's `link` was not the first page under it: Configuration
 *     opened Setup while Installation stood first, Advanced Topics opened
 *     Downporting. A section's words open its link (build-site.mjs), so the
 *     link has to be either the section's own page - the Walkthrough's
 *     index, the Model chapter's Binding - or the first page in the list
 *     under it, never a page from the middle.
 *
 * Two shapes are allowed to differ from the H1 on purpose, and the rule
 * says which: a numbered step ("11. To Production" for "Step 11: From
 * Playground to Production") and a numbered essay ("#3 The Cost of a
 * Screen" for a page whose H1 carries the same words after its number).
 * Everything else has to match to the character. */
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import test from 'node:test';
import config from '../docs/.vitepress/config.mjs';

const DOCS = path.join(process.cwd(), 'docs');
const fileOf = (link) => path.join(DOCS, link.endsWith('/') ? `${link}index.md` : `${link}.md`);
const h1Of = (link) => {
  const src = fs.readFileSync(fileOf(link), 'utf8');
  const fm = src.match(/^---\r?\n([\s\S]*?)\r?\n---/);
  const declared = fm && fm[1].match(/^title:\s*(.+)$/m);
  if (declared) return declared[1].trim().replace(/^["']|["']$/g, '');
  const h1 = src.match(/^#\s+(.+)$/m);
  assert.ok(h1, `${link} has no H1`);
  return h1[1].trim();
};

const leaves = (items, out = []) => {
  for (const i of items) {
    if (i.items) leaves(i.items, out);
    else if (i.link) out.push(i);
  }
  return out;
};
const firstLeaf = (items) => leaves(items)[0];

/* A label that abbreviates a numbered page: "11. To Production" against
   "Step 11: From Playground to Production", "#3 The Cost of a Screen"
   against "#3 The Cost of a Screen" or "The Cost of a Screen". */
const abbreviates = (label, h1) => {
  const step = label.match(/^(\d+)\.\s+(.+)$/);
  if (step) return new RegExp(`^Step ${step[1]}:`).test(h1);
  const essay = label.match(/^#(\d+)\s+(.+)$/);
  if (essay) return h1 === label || h1 === essay[2] || h1.replace(/^#\d+\s+/, '') === essay[2];
  return false;
};

test('every leaf of the sidebar is labelled with its page\'s own title', () => {
  const wrong = [];
  for (const leaf of leaves(config.themeConfig.sidebar)) {
    const h1 = h1Of(leaf.link);
    if (leaf.text !== h1 && !abbreviates(leaf.text, h1)) wrong.push(`${leaf.link}: menu says "${leaf.text}", page says "${h1}"`);
  }
  assert.deepEqual(wrong, [], `the menu and the page disagree on:\n  ${wrong.join('\n  ')}`);
});

test('a section that links a page links the first page under it, and that page is in the list', () => {
  const wrong = [];
  const walk = (items, trail) => {
    for (const i of items) {
      if (!i.items) continue;
      const here = [...trail, i.text].join(' / ');
      if (i.link) {
        /* Either the section's own page - an index no row under it repeats,
           reached through the section's own row - or the first page in the
           list, so that the row and the list agree on where it opens. */
        const under = leaves(i.items).map((l) => l.link);
        const own = !under.includes(i.link);
        if (own && !fs.existsSync(fileOf(i.link))) wrong.push(`${here} links ${i.link}, which is no page`);
        const first = firstLeaf(i.items);
        if (!own && first && first.link !== i.link) wrong.push(`${here} links ${i.link}, but the first page under it is ${first.link}`);
      }
      walk(i.items, [...trail, i.text]);
    }
  };
  walk(config.themeConfig.sidebar, []);
  assert.deepEqual(wrong, [], `section links:\n  ${wrong.join('\n  ')}`);
});

test('every page under docs/ is in the sidebar, except the front door', () => {
  const all = [];
  (function walk(dir) {
    for (const e of fs.readdirSync(path.join(DOCS, dir), { withFileTypes: true })) {
      const rel = dir ? `${dir}/${e.name}` : e.name;
      if (e.isDirectory()) { if (e.name !== '.vitepress' && e.name !== 'public') walk(rel); }
      else if (e.name.endsWith('.md') && rel !== 'index.md') all.push('/' + rel.replace(/\.md$/, '').replace(/\/index$/, '/'));
    }
  })('');
  /* Every link in the tree, a section's own included: its row is a link. */
  const links = (items, out = []) => {
    for (const i of items) { if (i.link) out.push(i.link); if (i.items) links(i.items, out); }
    return out;
  };
  const listed = new Set(links(config.themeConfig.sidebar));
  const orphans = all.filter((p) => !listed.has(p));
  assert.deepEqual(orphans, [], `pages no menu entry names:\n  ${orphans.join('\n  ')}`);
});
