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
      { text: 'Guide',  items: [
        { text: 'Getting Started', link: '/get_started/get_started' },
        { text: 'Development', link: '/development/development' },
        { text: 'Configuration' , link: '/configuration/configuration'  },
        { text: 'Advanced Topics', link: '/advanced/advanced' },
        { text: 'Technical Insights', link: '/insights/insights' },
        { text: 'Resources', link: '/resources/resources' },
      ]},
      { text: 'Jump',  items: [
        { text: 'Project',  items: [
          { text: 'abap2UI5', link: 'https://github.com/abap2UI5/abap2UI5' },
          { text: 'samples', link: 'https://github.com/abap2UI5/samples' },
          { text: 'docs', link: 'https://github.com/abap2UI5/docs' },
          { text: 'issues', link: 'https://github.com/abap2UI5/abap2UI5/issues' },
        ]},
        { text: 'More',  items: [
          { text: 'addons', link: 'https://github.com/abap2UI5-addons' },
          { text: 'connectors', link: 'https://github.com/abap2UI5-connectors' },
          { text: 'apps', link: 'https://github.com/abap2UI5-apps' },
          { text: 'downports', link: 'https://github.com/abap2UI5-downports' },
        ]},
      ]},
      { text: '1.134.0',  items: [
        { text: 'Changelog', link: '/resources/changelog' },
        { text: 'Contribution', link: '/resources/contribution' },
        { text: 'Support', link: '/resources/support' },
      ]},
    ],
    sidebar: [
      {
        text: 'Getting Started',
        link: '/get_started/get_started',
        collapsed: true,
        items: [
          { text: 'Introduction', link: '/get_started/introduction' },
          { text: 'Quickstart', link: '/get_started/quickstart' },
          { text: 'Hello World', link: '/get_started/hello_world' },
          { text: 'Sample Apps', link: '/get_started/sample_apps' },
          { text: `What's next?`, link: '/get_started/next'  }
        ]
      },
      {
        text: 'Development',
        link: '/development/development',
        collapsed: true,
        items: [
          { text: 'General',  link: '/development/general' }, 
        //   { text: 'Messages, Errors', link: '/features/' },
        //   { text: 'Translation, 18n', link: '/features/' },
        //   { text: 'User Interface', link: '/features/' },
        //   { text: 'Data Binding', link: '/features/' },
        //   { text: 'Navigation', link: '/features/' },
        //   { text: 'Runtime Typed Data', link: '/features/' },
        //   { text: 'Debugging', link: '/features/' },
        //   {
        //     text: 'Addons',  items: [
        //       { text: 'Popups', link: '/api-examples' },
        //       { text: 'Layouts & Variants', link: '/api-examples' },
        //       { text: 'External Libraries', link: '/markdown-examples' },
        //     ]
        //   },
        ]
      },
      {
        text: 'Configuration',
        link: '/configuration/configuration',
        collapsed: true,
        items: [
          { text: 'General',  link: '/configuration/general'  },
          // { text: 'Productive Usage', link: '/features/' },
          // { text: 'Performance', link: '/features/faq' },
          // { text: 'Security, Authorization', link: '/features/faq' },
          // { text: 'Fiori Launchpad', link: '/features/' },
          // {
          //   text: 'Installation', 
          //   items: [
          //     { text: 'BTP ABAP Environment', link: '/markdown-examples' },
          //     { text: 'S/4 Public Cloud', link: '/markdown-examples' },
          //     { text: 'S/4 Private Cloud', link: '/markdown-examples' },
          //     { text: 'R/3 Netweaver', link: '/markdown-examples' }
          //   ]
        //   },
        //   {
        //     text: 'More', 
        //     items: [
        //       { text: 'SE80 or ADT', link: '/features/faq' },
        //       { text: 'UI5 Versions', link: '/features/faq' },
        //       { text: 'ABAP Versions', link: '/features/faq' },
        //     ]
        //   }
        ]
      },
     {
         text: 'Advanced Topics',
         link: '/advanced/advanced',
         collapsed: true,
         items: [
        //           { text: 'BTP Workzone', link: '/features/' },
        //           { text: 'Stateful Sessions', link: '/features/' },
        //           { text: 'Remotely App Call', link: '/features/' },
                  { text: 'Downporting', link: '/advanced/downporting/' },
                  { text: 'Renaming', link: '/advanced/renaming/' },
        //           { text: 'JS Transpiling', link: '/features/' },
        //           {
        //             text: 'Extensibility',
        //             items: [
        //               { text: 'Custom JS', link: '/features/' },
        //               { text: 'Custom Controls', link: '/features/' },
        //               { text: 'External Libraries', link: '/features/' },
        //             ]
        //           },
        ]
       },
       {
        text: 'Technical Insights',
        link: '/insights/insights',
        collapsed: true,
        items: [  { text: 'General', link: '/insights/general' }, ]
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
  markdown: {
    // Aktiviert den "On This Page"-Bereich
    outline: "deep"
  }
})
