/*
 * The Run button, once it is in a browser.
 *
 * `docs/.vitepress/playground.mjs` decides at build time which fenced example
 * can be started and puts a button under it. This is what happens when it is
 * pressed: the ABAP is read out of the block the reader is looking at, and an
 * embedded playground — the whole framework compiled into a page — is mounted
 * underneath it, running that exact text.
 *
 * Three things are deliberate.
 *
 * **Nothing is fetched until somebody clicks.** Not the loader script, not the
 * playground, not the three megabytes it brings. A reader who never presses a
 * button makes no request to another site and pays nothing, and a page with
 * seven examples on it is still a page of prose.
 *
 * **The code comes from the DOM, not from an attribute.** The button reads the
 * rendered code block, which is the same text the copy button copies. So the
 * example that runs cannot be a different one from the example that is
 * printed — there is only one copy of it, and it is the one on the page.
 *
 * **The app is shown, not the editor — unless the example is one to play
 * with.** The code is already above the frame, in this site's own font and
 * highlighting, so an editor BESIDE it would be the same thing twice. That is
 * still true, and it is why 63 of the 64 examples on this site mount the app
 * on its own. The one that does not is the front door's, marked ```abap edit:
 * there the frame carries the editor and the app, and the printed block is
 * hidden while it runs — so there is still exactly one copy of the code, and
 * that copy is the one you can type in. Close puts the listing back.
 *
 * "Switch to Playground with this code" stays either way, and goes to the full
 * playground — the examples menu, Share, and the Problems / abaplint panel —
 * in this tab, not to another embedded frame.
 */

/* The published playground. Absolute rather than site-relative on purpose: it
 * is a different project with its own deployment, and the code to run travels
 * in the URL fragment, so this works from a local `docs:dev` as well as from
 * the published site. */
const PLAYGROUND = 'https://abap2ui5.github.io/playground/';
const LOADER = `${PLAYGROUND}embed/abap2ui5-embed.js`;

/** The embed loader, fetched once, on the first click anywhere on the site. */
let loading;
function loader() {
  if (window.abap2ui5Embed) return Promise.resolve(window.abap2ui5Embed);
  loading ??= new Promise((resolve, reject) => {
    const script = document.createElement('script');
    script.src = LOADER;
    script.addEventListener('load', () =>
      window.abap2ui5Embed
        ? resolve(window.abap2ui5Embed)
        : reject(new Error('The playground loader did not install itself.')));
    script.addEventListener('error', () => reject(new Error(`${LOADER} could not be loaded.`)));
    document.head.append(script);
  });
  return loading;
}

/** The ABAP in a runnable block: the text the reader sees and copies. */
const sourceOf = (container) => container.querySelector('pre code')?.textContent ?? '';

function fail(container, message) {
  const note = document.createElement('p');
  note.className = 'a2ui5-play-failed';
  note.textContent = `${message} The example still runs at ${PLAYGROUND}.`;
  container.append(note);
}

async function start(button, { unasked = false } = {}) {
  const container = button.closest('.a2ui5-play');
  if (!container || container.dataset.running) return;
  container.dataset.running = '1';
  const label = button.textContent;
  button.disabled = true;
  button.textContent = 'Starting the ABAP runtime…';

  let embed;
  try {
    embed = await loader();
  } catch (e) {
    /* A reader who PRESSED the button is owed an explanation. A reader who did
     * not - the front door's example starts itself - is owed silence and the
     * button back: an error message for something nobody asked for is a broken
     * front door, and the page is not broken, the network is. */
    if (unasked) {
      delete container.dataset.running;
      button.disabled = false;
      button.textContent = label;
      return;
    }
    button.remove();
    fail(container, String(e.message || e));
    return;
  }

  const source = sourceOf(container);
  const editable = container.dataset.play === 'edit';
  const demo = document.createElement('div');
  demo.className = 'abap2ui5-demo';
  /* `view=app` is what takes the editor out of the frame; leaving it off is
   * the embed's own default, which is the editor and the running app side by
   * side with the rest of the playground's furniture tucked away. */
  if (!editable) demo.dataset.view = 'app';
  /* An app alone is content-sized and grows; a split needs room for two panes
   * before either is worth looking at. */
  if (editable) demo.dataset.height = '620';
  /* Already clicked — the loader's own button would be a second one. */
  demo.dataset.auto = '1';
  demo.dataset.code = source;
  container.append(demo);
  embed.setUp(container);
  /* The printed listing steps aside for the editable one: the frame now shows
   * the same source in a place the reader can type. It is hidden rather than
   * removed - `sourceOf` reads it, Close brings it back, and a reader who
   * copies is copying the block that was always there. */
  if (editable) listing(container)?.toggleAttribute('hidden', true);

  button.replaceWith(bar(container, demo, embed, source));
}

/** What replaces the button: close it again, or take it somewhere it can be
 *  edited. Both only make sense once something is running. */
function bar(container, demo, embed, source) {
  const bar = document.createElement('div');
  bar.className = 'a2ui5-play-bar';

  const close = document.createElement('button');
  close.type = 'button';
  close.className = 'a2ui5-play-close';
  close.textContent = 'Close';
  close.addEventListener('click', () => {
    /* The frame is a whole ABAP runtime; removing the element is what frees
     * it. Then the block is back to what it was, button and all. */
    demo.remove();
    listing(container)?.toggleAttribute('hidden', false);
    delete container.dataset.running;
    bar.replaceWith(runButton(container));
  });

  /* In THIS tab, and the label says so: a switch to the playground with the
   * code that is on this page, not a window that opens beside it. The way
   * back is the Documentation item in the playground's bar, which comes back
   * to this page (site-memory.js); a reader who wants a second tab has the
   * middle button. */
  const open = document.createElement('a');
  open.className = 'a2ui5-play-open';
  open.textContent = 'Switch to Playground with this code';
  /* In this tab, and out of VitePress's hands: the playground is same-origin
   * and its path looks like a page, so without a `target` the router takes
   * this link over, has no page of this site to render at it and shows the
   * 404 instead. The reasoning is in scripts/lib/cross-site.mjs. The gate
   * that holds the bar to this cannot see this link - it is built here, in a
   * browser, from a URL the loader returns - so test/cross-site.test.mjs
   * reads this line instead. */
  open.target = '_self';
  /* Built by the loader rather than here: the fragment format is the
   * playground's, and a fragment it cannot read is quietly replaced by its own
   * sample — a wrong link that looks like a working one. An older loader that
   * cannot build one still gets a link, to the playground itself.
   *
   * The loader builds the URL for a FRAME, with `embed=1` in the query — and
   * an embedded playground hides everything but the code and the app: no
   * examples menu, no Share, and the panel under the editor with the Problems,
   * Outline, Log and abaplint tabs stays tucked away. That is right inside the
   * page and wrong for this link, whose whole point is the tooling around the
   * code. So the query is taken off again and only the fragment travels: the
   * reader lands in the playground as it is when they open it themselves. */
  open.href = PLAYGROUND;
  Promise.resolve(embed.url?.({ code: source })).then((href) => {
    if (href) open.href = standalone(href);
  }, () => {});

  bar.append(close, open);
  return bar;
}

/** The same playground, not embedded: the loader's URL without the query
 *  that turns the page into a frame. The code stays where it was, in the
 *  fragment; `view` goes with `embed`, since the app-only layout is the other
 *  half of being furniture in somebody else's page. */
function standalone(href) {
  const url = new URL(href, PLAYGROUND);
  url.searchParams.delete('embed');
  url.searchParams.delete('view');
  return url.href;
}

/** The printed block this container was built around. */
const listing = (container) => container.querySelector('div[class*="language-"]');

const runButton = (container) => {
  const button = document.createElement('button');
  button.type = 'button';
  button.className = 'a2ui5-play-run';
  button.textContent = container?.dataset.play === 'edit'
    ? 'Run and edit this example' : 'Run this example';
  return button;
};

/* Has the reader asked not to be sent three megabytes? Two ways to say it -
 * the media query and the Save-Data header's client-side twin - and either is
 * enough. Neither takes the example away: the button is still there, and it
 * still starts on a press. */
const wantsLessData = () =>
  matchMedia?.('(prefers-reduced-data: reduce)').matches
  || navigator.connection?.saveData === true;

/* The editable example starts ITSELF, once, when it comes into view.
 *
 * "Nothing is fetched until somebody clicks" is why every other example on
 * this site waits, and it still holds for them: a chapter with seven Run
 * buttons must not pull seven runtimes. The front door is one example, and it
 * is the whole argument the page makes - "one ABAP class is one UI5 app" is a
 * claim while it is printed and a fact once it is running beside its own
 * source. A reader who never scrolls past the tiles still fetches nothing, and
 * a reader who asked for less data is never sent it.
 *
 * `rootMargin` starts it a screen early so it is already up by the time it is
 * read, and the observer lets go after the first hit - this happens once. */
function armSelfStart() {
  if (!window.IntersectionObserver || wantsLessData()) return;
  const seen = new WeakSet();
  const watch = new IntersectionObserver((entries) => {
    for (const entry of entries) {
      if (!entry.isIntersecting) continue;
      watch.unobserve(entry.target);
      const button = entry.target.querySelector('.a2ui5-play-run');
      if (button) start(button, { unasked: true });
    }
  }, { rootMargin: '400px 0px' });
  for (const el of document.querySelectorAll('.a2ui5-play[data-play="edit"]')) {
    if (seen.has(el)) continue;
    seen.add(el);
    watch.observe(el);
  }
}

/* One listener for the whole site, installed once. VitePress swaps pages
 * without reloading, so anything bound per page has to be re-bound on every
 * navigation; a delegated listener never notices. The observer does, so it is
 * armed again after a navigation - and on the published site, where every
 * navigation is a load, the second call never happens. */
export function setUpPlayground() {
  document.addEventListener('click', (e) => {
    const button = e.target.closest?.('.a2ui5-play-run');
    if (button) start(button);
  });
  armSelfStart();
  return armSelfStart;
}
