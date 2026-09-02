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
 * **The app is shown, not the editor.** The code is already above it, in this
 * site's own font and highlighting; a second copy in an editor beside it would
 * be the same thing twice. What the page cannot show is the running app, so
 * that is what the frame shows. "Open in the playground" is one link away for
 * a reader who wants to change it — and it opens the full playground, editor
 * and the Problems / abaplint panel included, not another embedded frame.
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

async function start(button) {
  const container = button.closest('.a2ui5-play');
  if (!container || container.dataset.running) return;
  container.dataset.running = '1';
  button.disabled = true;
  button.textContent = 'Starting the ABAP runtime…';

  let embed;
  try {
    embed = await loader();
  } catch (e) {
    button.remove();
    fail(container, String(e.message || e));
    return;
  }

  const source = sourceOf(container);
  const demo = document.createElement('div');
  demo.className = 'abap2ui5-demo';
  demo.dataset.view = 'app';
  /* Already clicked — the loader's own button would be a second one. */
  demo.dataset.auto = '1';
  demo.dataset.code = source;
  container.append(demo);
  embed.setUp(container);

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
    delete container.dataset.running;
    bar.replaceWith(runButton());
  });

  const open = document.createElement('a');
  open.className = 'a2ui5-play-open';
  open.target = '_blank';
  open.rel = 'noopener';
  open.textContent = 'Open in the playground';
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

const runButton = () => {
  const button = document.createElement('button');
  button.type = 'button';
  button.className = 'a2ui5-play-run';
  button.textContent = 'Run this example';
  return button;
};

/* One listener for the whole site, installed once. VitePress swaps pages
 * without reloading, so anything bound per page has to be re-bound on every
 * navigation; a delegated listener never notices. */
export function setUpPlayground() {
  document.addEventListener('click', (e) => {
    const button = e.target.closest?.('.a2ui5-play-run');
    if (button) start(button);
  });
}
