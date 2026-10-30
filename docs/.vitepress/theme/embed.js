/*
 * The playground in a frame, built here - no script is fetched from the
 * playground for it.
 *
 * The documentation and the playground are separate sites with separate
 * builds. What they share is a URL contract, written down by the playground
 * (src/embed/abap2ui5-embed.js and src/shell/share.mjs over there):
 *
 *   https://abap2ui5.github.io/playground/?embed=1[&view=app]#2<base64url>
 *
 * `embed=1` hides everything but the code and the app, `view=app` the code as
 * well, and the fragment is one version character ("2") followed by the
 * base64url of the deflate-raw of a JSON array of `{ name, source }` files.
 * The frame reports its height with `postMessage({ source:
 * "abap2ui5-playground", type: "height", height })`, which is the only
 * message read here.
 *
 * This module is that contract and nothing more. The two sites can change
 * everything else on their own.
 */

export const PLAYGROUND = 'https://abap2ui5.github.io/playground/';
const FROM_PLAYGROUND = 'abap2ui5-playground';

/* abapGit names a file after the object in it, and the playground refuses a
 * file whose name disagrees with the class it declares - so the name is read
 * from the source. */
function fileNameFor(code) {
  const declared = /^[ \t]*(?:CLASS|INTERFACE)\s+([a-zA-Z_]\w*)\s+(?:DEFINITION|PUBLIC)/im.exec(code);
  const kind = /^\s*INTERFACE\s/im.test(code) && !/^\s*CLASS\s/im.test(code) ? 'intf' : 'clas';
  return `${(declared?.[1] || 'zcl_playground').toLowerCase()}.${kind}.abap`;
}

async function fragmentFor(code) {
  const payload = new TextEncoder().encode(JSON.stringify([{ name: fileNameFor(code), source: code }]));
  const deflated = new Uint8Array(
    await new Response(new Blob([payload]).stream().pipeThrough(new CompressionStream('deflate-raw'))).arrayBuffer(),
  );
  const parts = [];
  for (let i = 0; i < deflated.length; i += 0x8000) {
    parts.push(String.fromCharCode.apply(null, deflated.subarray(i, i + 0x8000)));
  }
  return '2' + btoa(parts.join('')).replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
}

/** The playground URL for a frame: `embed=1`, and `view=app` for the app alone. */
export async function url({ code, view } = {}) {
  const href = new URL(PLAYGROUND);
  href.searchParams.set('embed', '1');
  if (view === 'app') href.searchParams.set('view', 'app');
  if (code) href.hash = await fragmentFor(code);
  return href.href;
}

/** Mounts every `.abap2ui5-demo` under `root`: an iframe on the URL above,
 *  520px high (or `data-height`), an app-only one growing with the height the
 *  frame reports. */
export function setUp(root = document) {
  for (const el of root.querySelectorAll('.abap2ui5-demo:not([data-mounted])')) mount(el);
}

async function mount(el) {
  el.dataset.mounted = '1';
  const appOnly = el.dataset.view === 'app';
  const frame = document.createElement('iframe');
  try {
    frame.src = await url({ code: el.dataset.code || '', view: appOnly ? 'app' : undefined });
  } catch (e) {
    delete el.dataset.mounted;
    throw e;
  }
  frame.title = el.dataset.label || 'abap2UI5 example';
  frame.className = 'abap2ui5-demo-frame';
  frame.height = String(Number(el.dataset.height) || 520);
  frame.setAttribute('allow', 'clipboard-write');
  el.replaceChildren(frame);
  if (!appOnly) return;
  const onMessage = (e) => {
    if (!frame.isConnected) { window.removeEventListener('message', onMessage); return; }
    if (e.source !== frame.contentWindow || e.data?.source !== FROM_PLAYGROUND) return;
    if (e.data.type === 'height' && Number.isFinite(e.data.height)) {
      frame.height = String(Math.max(120, Math.min(1200, e.data.height)));
    }
  };
  window.addEventListener('message', onMessage);
}
