---
outline: [2, 4]
---
# Use Cases

abap2UI5 fits many contexts, whether you work in a cloud-ready environment or with classic ABAP.

For a full overview, see the [Use Cases blog article](https://www.linkedin.com/pulse/use-cases-abap2ui5-overview-abap2ui5-udbde/?trackingId=6iIX%2FNk%2BCT0%2B4JorQjpRSQ%3D%3D).

### On-Stack Extension

#### Tier 1 Extension
For clean-core compliance, build applications that follow "keep the core clean" principles to keep your apps cloud-ready and upgrade-stable:
![Tier 1 on-stack extension architecture diagram](/get_started/image-11.png){ width=60% }

#### Tier 2 Extension
If certain APIs you need aren't yet released, you can still build a cloud-ready abap2UI5 app by accessing them through a separate Tier 2 wrapper:
![Tier 2 on-stack extension with wrapper for unreleased APIs](/get_started/image-21.png){ width=60% }

#### Tier 3 Extension
For a classic extension approach, use unreleased APIs for more freedom to customize and extend your system:
![Tier 3 on-stack extension with unreleased API access](/get_started/image-10.png){ width=60% }

### Side-by-Side Extension

#### Tier 1 Extension
Build applications separately from your S/4 system's lifecycle through remote API calls only. Stick to released APIs, and your apps will work with S/4HANA Public Cloud too:
![Tier 1 side-by-side extension with remote API calls](/get_started/image-22.png){ width=60% }

#### Tier 3 Extension
For more flexibility, build applications whose lifecycle is independent of your S/4 system using remote API calls:
![Tier 3 side-by-side extension with remote API calls](/get_started/image-23.png){ width=60% }

### Software as a Service (SaaS)
With a Tier 1 side-by-side extension, you can connect a single abap2UI5 app to multiple S/4 systems. Use the same abap2UI5 code across customer tenants and remote systems for a true SaaS scenario:
![SaaS architecture connecting one abap2UI5 app to multiple S/4 tenants](/get_started/image-9.png){ width=60% }
