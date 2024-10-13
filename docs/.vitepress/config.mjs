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
      { text: 'Guide',  items: [
        { text: 'Getting Started', link: '/get_started/get_started' },
        { text: 'Development', link: '/development/development' },
        { text: 'Configuration' , link: '/configuration/configuration'  },
        { text: 'Advanced Topics', link: '/advanced/advanced' },
        { text: 'Technical Insights', link: '/insights/insights' },
        { text: 'Resources', link: '/resources/resources' },
      ]},
      { text: 'Links',  items: [
        { text: 'Project',  items: [
          { text: 'abap2UI5', link: 'https://github.com/abap2UI5/abap2UI5' },
          { text: 'samples', link: 'https://github.com/abap2UI5/samples' },
          { text: 'docs', link: 'https://github.com/abap2UI5/docs' },
          { text: 'frontend', link: 'https://github.com/abap2UI5/frontend' },
          { text: 'issues', link: 'https://github.com/abap2UI5/abap2UI5/issues' },
        ]},
        { text: 'More',  items: [
          { text: 'addons', link: 'https://github.com/abap2UI5-addons' },
          { text: 'apps', link: 'https://github.com/abap2UI5-apps' },
          { text: 'downported', link: 'https://github.com/abap2UI5-downported' },
          { text: 'renamed', link: 'https://github.com/abap2UI5-renamed' },
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
          { text: 'Setup',          link: '/configuration/general'  },
          {
            text: 'Installation', 
            link: '/configuration/installation',
            // items: [
            //   { text: 'BTP ABAP Environment', link: '/configuration/installation' },
            //   { text: 'S/4 Public Cloud', link: '/configuration/installation' },
            //   { text: 'S/4 Private Cloud', link: '/configuration/installation' },
            //   { text: 'R/3 Netweaver', link: '/configuration/installation' },
            // ]
          },
          {
            text: 'Launchpad', 
            link: '/configuration/installation',
            // items: [
            //   { text: 'Standard ABAP',  link: '/configuration/launchpad' },
            //   { text: 'ABAP for Cloud', link: '/configuration/launchpad' },
            // ]
           },
          { text: 'Productive Usage', link:  '/configuration/productive_usage'  },
          { text: 'Security',         link:  '/configuration/productive_usage'  },
          { text: 'Performance',      link: '/configuration/performance'  },
          { text: 'Clean Core',       link: '/configuration/clean_core'  },
          { text: 'UI5 Versions',     link: '/features/faq' },
          { text: 'ABAP Versions',    link: '/features/faq' },
   
        //   {
        //     text: 'More', 
        //     items: [
        //      
        //      
        //       
        //     ]
        //   }
        ]
      },
     {
         text: 'Advanced Topics',
         link: '/advanced/advanced',
         collapsed: true,
         items: [
                 { text: 'BTP Workzone', link: '/advanced/btp' },
                  { text: 'Stateful Sessions', link: '/advanced/stateful' },
                  { text: 'KPIs', link: '/advanced/kpi' },
                  { text: 'Remote App Call', link: '/advanced/rfc' },
                  { text: 'Downporting', link: '/advanced/downporting' },
                  { text: 'Renaming', link: '/advanced/renaming' },
                  { text: 'SE80 or ADT', link: '/features/faq' },
                 
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
        items: [  
            { text: 'General', link: '/insights/general' }, 
            { text: 'CI/CD', link: '/insights/ci' }, 
            { text: 'open-abap', link: '/advanced/open_abap' },
          ]
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
