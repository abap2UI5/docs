/*
 * "Copy link to selection", once it is in a browser — the DOM half of
 * theme/text-fragment.js, which is where the reasoning for quoting the words
 * rather than numbering the lines is written down.
 *
 * Select anything inside a page of the manual — a sentence, a chain out of a
 * fence, one method of an example — and a small button appears under it. Press
 * it and the clipboard holds this page's URL with the selection quoted in it:
 *
 *   .../development/lifecycle.html#:~:text=client-%3Eview_display
 *
 * Three things are deliberate:
 *
 * **The whole page is the surface.** It is not a button on a code block: a
 * reader quoting a sentence of prose to a colleague has exactly the problem a
 * reader quoting a chain has, and one affordance for both is one thing to
 * learn. What it is scoped to is the ARTICLE (.vp-doc) — a selection in the
 * sidebar or the bar is somebody dragging, not somebody quoting.
 *
 * **Nothing is added to the page until somebody selects something.** No markup
 * on a fence, no ids, no numbers in the gutter: a text fragment is a property
 * of the URL, not of the document, which is what makes this a hundred lines
 * here and no change at all to the 838 examples.
 *
 * **The page it lands on is this page, not a copy of it.** The link is built
 * from `location`, so a link made on a `docs:dev` server names localhost and a
 * link made on the site names the site.
 *
 * The fallback at the bottom is for a browser too old to know what `:~:` means
 * — it would land at the top of the page and look like a broken link. There it
 * scrolls to the block the words are in and marks it for a moment. Browsers
 * that support fragments (`document.fragmentDirective`) do their own, better
 * job and this stays out of their way.
 */
import { fragmentFor, normalise, parseDirective } from './text-fragment.js'

/** The article, and nothing else on the page. */
const ARTICLE = '.vp-doc'
/* The elements a selection's context is taken from: what is quoted has to be
 * disambiguated by the words AROUND it, and a block is where "around" ends. */
const BLOCK = 'p, li, dd, dt, td, th, pre, blockquote, h1, h2, h3, h4, h5, h6'

const LABEL = 'Copy link to selection'
const DONE = 'Link copied'
const FAILED = 'Copy it from the address bar'

/** The selection, if it is a real one inside the article. */
function quoted() {
  const selection = window.getSelection()
  if (!selection || selection.isCollapsed || selection.rangeCount === 0) return undefined
  const range = selection.getRangeAt(0)
  const node = range.commonAncestorContainer
  const element = node.nodeType === Node.ELEMENT_NODE ? node : node.parentElement
  if (!element?.closest(ARTICLE)) return undefined
  return normalise(range.toString()) === '' ? undefined : range
}

/** The text before and after the selection, within the block it starts in. */
function around(range) {
  const start = range.startContainer
  const element = start.nodeType === Node.ELEMENT_NODE ? start : start.parentElement
  const block = element?.closest(BLOCK) ?? element?.closest(ARTICLE)
  if (!block) return { prefix: '', suffix: '' }

  const read = (from, to) => {
    try {
      const part = document.createRange()
      from(part)
      to(part)
      return part.toString()
    } catch {
      /* A selection that started in one block and ended in another: the
       * context is a nicety, and the directive is written without it. */
      return ''
    }
  }
  return {
    prefix: read(
      (part) => part.setStart(block, 0),
      (part) => part.setEnd(range.startContainer, range.startOffset),
    ),
    suffix: read(
      (part) => part.setStart(range.endContainer, range.endOffset),
      (part) => part.setEnd(block, block.childNodes.length),
    ),
  }
}

/** The one button, made when it is first needed and moved about after that. */
let button

function show(range) {
  if (!button) {
    button = document.createElement('button')
    button.type = 'button'
    button.className = 'a2ui5-quote'
    button.textContent = LABEL
    button.addEventListener('mousedown', (event) => event.preventDefault()) // keep the selection
    button.addEventListener('click', copy)
    document.body.append(button)
  }
  button.textContent = LABEL
  button.hidden = false

  /* Under the end of the selection, and never off the right-hand edge. */
  const box = range.getBoundingClientRect()
  const width = button.offsetWidth
  const left = Math.min(box.left + window.scrollX, window.scrollX + document.documentElement.clientWidth - width - 8)
  button.style.left = `${Math.max(window.scrollX + 8, left)}px`
  button.style.top = `${box.bottom + window.scrollY + 8}px`
}

const hide = () => {
  if (button) button.hidden = true
}

function copy() {
  const range = quoted()
  if (!range) return hide()

  const { prefix, suffix } = around(range)
  const directive = fragmentFor({
    text: range.toString(),
    prefix,
    suffix,
    /* Read here rather than on every selection change: this is the whole
     * article as text, and it is only needed to answer whether the quote is
     * ambiguous. */
    pageText: document.querySelector(ARTICLE)?.innerText ?? '',
  })
  if (!directive) return hide()

  const url = `${location.origin}${location.pathname}${location.search}#:~:${directive}`
  const said = (text) => {
    button.textContent = text
    setTimeout(hide, 1600)
  }
  try {
    navigator.clipboard.writeText(url).then(() => said(DONE), () => said(FAILED))
  } catch {
    said(FAILED)
  }
}

/** The button, wired to the selection. Called once, from the theme. */
export function setUpLinkToSelection() {
  let pending
  document.addEventListener('selectionchange', () => {
    clearTimeout(pending)
    /* After the drag, not during it: a selection changes on every pixel of a
     * mouse-down, and a button that moved with it would be impossible to aim
     * at. */
    pending = setTimeout(() => {
      const range = quoted()
      if (range) show(range)
      else hide()
    }, 200)
  })
  document.addEventListener('keydown', (event) => {
    if (event.key === 'Escape') hide()
  })
}

/**
 * What a browser that does not know `:~:` gets instead of a link that appears
 * to do nothing: the block the words are in, scrolled to and marked. Every
 * browser that supports text fragments has `document.fragmentDirective` and is
 * left alone here — it highlights the words themselves, which is better than
 * this can be.
 */
export function markDirective(url = location.href) {
  /* A browser that supports fragments also takes the directive OUT of the
   * URL - `location.href` no longer carries it - so this reads the directive
   * only where it is still there to read, which is exactly where it is
   * needed. The url is a parameter so that this can be exercised at all:
   * nothing else about it can be reached from a browser that strips it. */
  if ('fragmentDirective' in document) return
  const directive = parseDirective(url)
  const article = document.querySelector(ARTICLE)
  if (!directive || !article || directive.start === '') return

  const wanted = normalise(directive.start)
  /* The DEEPEST element carrying the words: every ancestor up to <body>
   * carries them too, and scrolling to the article is not an answer. */
  const carrying = [...article.querySelectorAll('*')].filter(
    (element) => normalise(element.textContent).includes(wanted),
  )
  const target = carrying.find((element) => !carrying.some((other) => other !== element && element.contains(other)))
  if (!target) return

  target.scrollIntoView({ block: 'center' })
  target.classList.add('a2ui5-quoted')
  setTimeout(() => target.classList.remove('a2ui5-quoted'), 4000)
}
