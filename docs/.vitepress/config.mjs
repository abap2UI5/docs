import { defineConfig } from 'vitepress'

// https://vitepress.dev/reference/site-config
export default defineConfig({
  base: '/abap2UI5-documentation/', // Set your base URL here
  title: "abap2UI5",
  description: "Developing Purely in ABAP",
  themeConfig: {
    // https://vitepress.dev/reference/default-theme-config
    nav: [
      { text: 'Get Started', link: '/get_started/quickstart' },
      { text: 'Features', link: '/' },
      { text: 'Setup & Launchpad', link: '/' },
      { text: 'Resources', link: '/resources/blogs' }
    ],
    sidebar: [
      {
        text: 'Get Started',
        collapsed: true,
        items: [
          { text: 'Installation', link: '/get_started/quickstart' },
          { text: 'Hello World', link: '/get_started/quickstart' },
          { text: 'Configuration', link: '/get_started/configuration' },
          { text: 'Sample Apps', link: '/get_started/configuration' }
        ]
      },
      {
        text: 'Features',
        collapsed: true,
        items: [
          { text: 'Feature I', link: '/features/' },
          { text: 'Feature II', link: '/features/' },
          { text: 'Feature III', link: '/features/' },
          { text: 'FAQ', link: '/features/faq' }
        ]
      },
      {
        text: 'Setup & Launchpad',
        collapsed: true,
        items: [
          { text: 'Business Technology Platform', link: '/markdown-examples' },
          { text: 'BTP ABAP Environment', link: '/markdown-examples' },
          { text: 'S/4 Public Cloud', link: '/api-examples' },
          { text: 'S/4 Private or On-Premise', link: '/api-examples' },
          { text: 'R/3 Netweaver', link: '/api-examples' }
        ]
      },
      {
        text: 'Resources',
        collapsed: true,
        items: [
          { text: 'Release Notes', link: '/resources/release_notes' },
          { text: 'Blogs', link: '/resources/blogs' },
          { text: 'Contribution', link: '/resources/release_notes' },
          { text: 'Links', link: '/resources/links' }
        ]
      }
    ],
    socialLinks: [
      { icon: 'github', link: 'https://github.com/abap2UI5-documentation' }
    ]
  },
  markdown: {
    // Aktiviert den "On This Page"-Bereich
    outline: 'deep'
  }
})
