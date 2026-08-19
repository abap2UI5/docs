// https://vitepress.dev/guide/custom-theme
import { h } from 'vue'
import DefaultTheme from 'vitepress/theme'
import './style.css'
import { setUpPlayground } from './playground.js'
import { SupportLink } from './support-link.js'

/** @type {import('vitepress').Theme} */
export default {
  extends: DefaultTheme,
  Layout: () => {
    return h(DefaultTheme.Layout, null, {
      // https://vitepress.dev/guide/extending-default-theme#layout-slots
      // After the LinkedIn and GitHub icons, at the right-hand end of the bar.
      'nav-bar-content-after': () => h(SupportLink),
    })
  },
  enhanceApp({ app, router, siteData }) {
    // The Run button under a runnable ABAP example. One delegated listener for
    // the whole site — the browser half of docs/.vitepress/playground.mjs.
    if (!import.meta.env.SSR) setUpPlayground()
  }
}
