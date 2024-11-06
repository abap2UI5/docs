import { defineConfig } from 'vitepress'

// https://vitepress.dev/reference/site-config
export default defineConfig({
  lastUpdated: {
    text: 'Updated at',
    formatOptions: {
      dateStyle: 'full',
      timeStyle: 'medium'
    }
  },
  base: '/docs/', // Set your base URL here
  head: [
    ['link', { rel: 'shortcut icon', href: '/docs/favicon.ico' }],
    ['link', { rel: 'apple-touch-icon', sizes: '180x180', href: '/docs/favicon.ico' }],
    ['link', { rel: 'stylesheet', href: 'https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css' }],

  ],
  title: "abap2UI5",
  description: "Developing Purely in ABAP",
  themeConfig: {
    logo: '/logo.png',
    footer: {
      message: 'Released under the MIT License',
      copyright: '2023-present abap2UI5'
    },
    editLink: {
      pattern: 'https://github.com/abap2UI5/abap2UI5-documentation/tree/main/docs/:path',
      text: 'Edit this page on GitHub'
    },
    search: {
      provider: 'local'
    },
    // https://vitepress.dev/reference/default-theme-config
    nav: [
      {
        text: 'Guide', items: [
          { text: 'Getting Started', link: '/get_started/get_started' },
          { text: 'Development', link: '/development/development' },
          { text: 'Configuration', link: '/configuration/configuration' },
          { text: 'Advanced', link: '/advanced/advanced' },
          { text: 'Resources', link: '/resources/resources' },
        ]
      },
      {
        text: 'Links', items: [
          {
            text: 'Project', items: [
              { text: 'abap2UI5', link: 'https://github.com/abap2UI5/abap2UI5' },
              { text: 'samples', link: 'https://github.com/abap2UI5/samples' },
              { text: 'frontend', link: 'https://github.com/abap2UI5/frontend' },
              { text: 'docs', link: 'https://github.com/abap2UI5/docs' },
              { text: 'issues', link: 'https://github.com/abap2UI5/abap2UI5/issues' },
            ]
          },
          {
            text: 'More', items: [
              { text: 'addons', link: 'https://github.com/abap2UI5-addons' },
              { text: 'apps', link: 'https://github.com/abap2UI5-apps' },
              { text: 'downported', link: 'https://github.com/abap2UI5-downported' },
              { text: 'renamed', link: 'https://github.com/abap2UI5-renamed' },
            ]
          },
        ]
      },
      {
        text: '1.135.0', items: [
          { text: 'Changelog', link: '/resources/changelog' },
          { text: 'Contribution', link: '/resources/contribution' },
          { text: 'Support', link: '/resources/support' },
        ]
      },
    ],
    sidebar: [
      {
        text: 'Getting Started',
        link: '/get_started/about',
        collapsed: true,
        items: [
          { text: 'About', link: '/get_started/about' },
          { text: 'Quickstart', link: '/get_started/quickstart' },
          { text: 'Hello World', link: '/get_started/hello_world' },
          { text: 'Sample Apps', link: '/get_started/samples' },
          { text: 'Use Cases', link: '/get_started/use_cases' },
          { text: `What's next?`, link: '/get_started/next' }
        ]
      },
      {
        text: 'Development',
        link: '/development/general',
        collapsed: true,
        items: [
          { text: 'General', link: '/development/general' },
          { text: 'View', link: '/development/view' },
          { text: 'Model', link: '/development/model'  },
          { text: 'Events', link: '/development/events' },
          { text: 'Navigation', link: '/development/navigation' },
          { text: 'Messages, Errors', link: '/development/messages' },
          { text: 'Translation, i18n', link: '/development/translation' },
          { text: 'Popups, Popover', link: '/development/popups' },
          { text: 'Troubleshooting', link: '/development/troubleshooting' },
        ]
      },
      {
        text: 'Configuration',
        link: '/configuration/configuration',
        collapsed: true,
        items: [
          { text: 'Installation', link: '/configuration/installation'},
          { text: 'Setup', link: '/configuration/setup' },
          { text: 'Security', link: '/configuration/security' },
          { text: 'Authorization', link: '/configuration/authorization' },
          { text: 'Productive Usage', link: '/configuration/productive_usage' },
          { text: 'Performance', link: '/configuration/performance' },
          { text: 'UI5 Versions', link: '/configuration/ui5_versions' },
          { text: 'S/4 Public Cloud', link: '/configuration/s4_public_cloud' },
          { text: 'Fiori Launchpad', link: '/configuration/launchpad', },
          { text: 'BTP Workzone', link: '/configuration/btp' },
        ]
      },
      {
        text: 'Addons', link: '/addons/addons', collapsed: true, items: [
          { text: 'RTTI', link: '/addons/srtti' },
          { text: 'Layouts, Variants', link: '/addons/layout' },
          { text: 'Popups', link: '/addons/popup' },
          { text: 'Logging, BAL', link: '/addons/logging' },
          { text: 'XLSX', link: '/addons/xlsx' },
          { text: 'JS Libraries', link: '/addons/ext_js' },
          { text: 'Apps, Community', link: '/addons/apps' },
        ]
      },
      {
        text: 'Advanced',
        link: '/advanced/advanced',
        collapsed: true,
        items: [
          { text: 'Stateful Sessions', link: '/advanced/stateful' },
          { text: 'Downporting', link: '/advanced/downporting' },
          { text: 'Renaming', link: '/advanced/renaming' },
          { text: 'Launchpad KPIs', link: '/addons/kpi' },
          { text: 'Remote App Calls', link: '/addons/rfc' },
          { text: 'Extensibility', link: '/advanced/extensibility' },
          { text: 'Technical Background', link: '/advanced/insights/insights' , items: [
          { text: 'General', link: '/advanced/insights/insights' },
          { text: 'open-abap', link: '/advanced/insights/open_abap' },
            // { text: 'CI/CD', link: '/insights/ci' },
            // { text: 'abaplint', link: '/insights/ci' },
          ]
          } ]
      },
      {
        text: 'Resources',
        link: '/resources/resources',
        collapsed: true,
        items: [
          { text: 'Changelog', link: '/resources/changelog' },
          { text: 'Blogs', link: '/resources/blogs' },
          { text: 'References', link: '/resources/references' },
          { text: 'Contribution', link: '/resources/contribution' },
          { text: 'Support', link: '/resources/support' },
          { text: 'Sponsor', link: '/resources/sponsor' },
          { text: 'License', link: '/resources/license' },
        ]
      }
    ],
    socialLinks: [
      { icon: 'linkedin', link: 'https://www.linkedin.com/company/abap2ui5/' },
      { icon: 'github', link: 'https://www.github.com/abap2UI5/abap2UI5' },

    ]
  },
  
    // Aktiviert den "On This Page"-Bereich
    outline: { 
      level: [2,6]
    }
})
