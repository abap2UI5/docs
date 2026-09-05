// https://vitepress.dev/guide/custom-theme
import { h } from 'vue'
import DefaultTheme from 'vitepress/theme'
import './style.css'
import { setUpPlayground } from './playground.js'
import TheBar from './TheBar.vue'
import SiteNav from './SiteNav.vue'
import { rememberHere } from './site-memory.js'

/** @type {import('vitepress').Theme} */
export default {
  extends: DefaultTheme,
  // The bar's right-hand end - the three sites, and the menu behind the bar's
  // last button with the light/dark switch in it - injected into the nav bar
  // and, without the menu, into the screen a phone opens instead of it. See
  // SiteBar.vue for what the group is and style.css for where in the row it
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
    if (!import.meta.env.SSR) {
      rememberUnlessHome()
      const onAfter = router.onAfterRouteChange
      router.onAfterRouteChange = (to) => {
        rememberUnlessHome()
        onAfter?.(to)
      }
    }
  }
}
