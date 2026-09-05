// https://vitepress.dev/guide/custom-theme
import { h } from 'vue'
import DefaultTheme from 'vitepress/theme'
import './style.css'
import { setUpPlayground } from './playground.js'
import { markDirective, setUpLinkToSelection } from './link-to-selection.js'
import TheBar from './TheBar.vue'
import SiteNav from './SiteNav.vue'
import { rememberHere, rememberScroll, takeHandoff } from './site-memory.js'

/** @type {import('vitepress').Theme} */
export default {
  extends: DefaultTheme,
  // The bar's right-hand end - the three sites, and the menu behind the bar's
  // last button with the light/dark switch in it - injected into the nav bar
  // and, without the menu, into the screen a phone opens instead of it. See
  // SiteNav.vue for what the group is and style.css for where in the row it
  // lands: the slot renders after VitePress's own appearance switch and
  // social links, and CSS orders the sites back in front of the marks and the
  // menu after them.
  // ONE ELEMENT, AND IT IS OURS (TheBar.vue). The theme's own bar keeps
  // exactly two parts, because they are worth keeping and not worth
  // rebuilding: the hamburger, and the screen it opens on a phone - which
  // carries the same four sections as a list.
  Layout() {
    return h(DefaultTheme.Layout, null, {
      'nav-bar-content-before': () => h(TheBar),
      'nav-screen-content-after': () => h(SiteNav),
    })
  },

  enhanceApp({ app, router, siteData }) {
    // The Run button under a runnable ABAP example. One delegated listener for
    // the whole site — the browser half of docs/.vitepress/playground.mjs.
    if (!import.meta.env.SSR) setUpPlayground()

    // "Copy link to selection" — a link to the words a reader marked, as a
    // text fragment (link-to-selection.js, and text-fragment.js for why the
    // link names the words rather than the line they are on). One delegated
    // listener for the whole site, like the Run button above it; the second
    // call is only ever reached by a browser too old for `:~:`, and lands it
    // where the words are instead of at the top of the page.
    if (!import.meta.env.SSR) {
      setUpLinkToSelection()
      if (document.readyState === 'complete') markDirective()
      else window.addEventListener('load', markDirective, { once: true })
    }

    // Where the reader is, for the Docs item on the other three bars to come
    // back to (site-memory.js). On every route change, because this site is a
    // single page application: the first page would otherwise be the only one
    // ever written down. `onAfterRouteChange` rather than `onBeforeRouteChange`
    // - the location is the new one only after the change has happened.
    // THE HOME PAGE IS NOT WRITTEN DOWN. Every other page is, and the
    // Documentation item on all four bars comes back to whatever was written
    // last. If the front door counted as a page of the manual, going Home
    // would overwrite the chapter you were reading with `/docs/`, and
    // Documentation would then open the home page - which is what the Home
    // item is for and not what the word Documentation promises. Home stays a
    // place you go to, never a place you are returned to.
    const rememberUnlessHome = () => {
      if (!location.pathname.replace(/index\.html$/, '').match(/\/docs\/?$/)) rememberHere('docs')
    }

    // How far DOWN the page, which is the other half of coming back to it.
    // The offset is written for every page including the home page: what the
    // rule above is about is which page the Documentation item opens, and
    // that is a different question from where in a page the reader was.
    //
    // Throttled, because scroll fires per frame and this writes to storage.
    // `pagehide` as well, for a reader who leaves inside the window - and
    // SiteNav.vue writes it on the click itself, which is the case that has
    // to be exact.
    let pending = 0
    const noteScroll = () => {
      if (pending) return
      pending = setTimeout(() => { pending = 0; rememberScroll() }, 300)
    }

    // ...and back to it, but ONLY when the bar said so (site-memory.js). The
    // router draws the new page and scrolls it to the top itself, not always
    // in the same frame, so the offset is re-applied for a few frames until
    // it holds. About a tenth of a second, and it stops the moment it works.
    const restore = () => {
      const y = takeHandoff()
      if (y === null) return
      let tries = 0
      const put = () => {
        scrollTo(0, y)
        if (++tries < 6 && Math.abs(scrollY - y) > 2) requestAnimationFrame(put)
      }
      requestAnimationFrame(put)
    }

    if (!import.meta.env.SSR) {
      rememberUnlessHome()
      restore()
      addEventListener('scroll', noteScroll, { passive: true })
      addEventListener('pagehide', () => rememberScroll())
      const onAfter = router.onAfterRouteChange
      router.onAfterRouteChange = (to) => {
        rememberUnlessHome()
        // Home -> Documentation is a route change, not a load: this site is
        // one application and two of the bar's four items are pages of it.
        restore()
        onAfter?.(to)
      }
    }
  }
}
