---
outline: [2, 4]
description: What abap2UI5 costs for your users, systems, apps and support tier, worked out slider by slider - and why every line of the sheet comes to the same number.
---
# Cost Calculator

Every SAP project has a sheet like this one. Set the sliders to your landscape, pick a currency, and read off what abap2UI5 will cost you.

<div class="cost" data-cost>
<div class="cost-row"><label for="cost-users">Named users</label><input id="cost-users" type="range" min="0" max="15" value="6" data-stops="1|5|10|25|50|100|250|500|1000|2500|5000|10000|25000|50000|100000|250000" data-unit="user|users"><output for="cost-users">250 users</output></div>
<div class="cost-row"><label for="cost-system">Your system</label><input id="cost-system" type="range" min="0" max="8" value="5" data-stops="NetWeaver 7.02|NetWeaver 7.31|NetWeaver 7.40|NetWeaver 7.50|NetWeaver 7.52|S/4HANA on-premise|S/4HANA Private Cloud|BTP ABAP Environment|S/4HANA Public Cloud"><output for="cost-system">S/4HANA on-premise</output></div>
<div class="cost-row"><label for="cost-systems">Systems in the landscape</label><input id="cost-systems" type="range" min="0" max="12" value="2" data-stops="1|2|3|4|5|6|8|10|12|15|20|30|50" data-unit="system|systems"><output for="cost-systems">3 systems</output></div>
<div class="cost-row"><label for="cost-apps">Apps</label><input id="cost-apps" type="range" min="0" max="10" value="4" data-stops="1|2|3|5|10|20|50|100|200|500|1000" data-unit="app|apps"><output for="cost-apps">10 apps</output></div>
<div class="cost-row"><label for="cost-developers">Developers</label><input id="cost-developers" type="range" min="0" max="9" value="3" data-stops="1|2|3|5|10|20|50|100|200|500" data-unit="developer|developers"><output for="cost-developers">5 developers</output></div>
<div class="cost-row"><label for="cost-roundtrips">Load</label><input id="cost-roundtrips" type="range" min="0" max="6" value="3" data-stops="100|1000|10000|100000|1000000|10000000|100000000" data-unit="roundtrip a day|roundtrips a day"><output for="cost-roundtrips">100,000 roundtrips a day</output></div>
<div class="cost-row"><label for="cost-hosting">Frontend</label><input id="cost-hosting" type="range" min="0" max="2" value="2" data-stops="OpenUI5 from the CDN|SAPUI5 from the CDN|the UI5 your system ships"><output for="cost-hosting">the UI5 your system ships</output></div>
<div class="cost-row"><label for="cost-support">Support tier</label><input id="cost-support" type="range" min="0" max="4" value="0" data-stops="Community|Standard|Premium|Enterprise|Enterprise Plus"><output for="cost-support">Community</output></div>
<div class="cost-row"><label for="cost-term">Contract term</label><input id="cost-term" type="range" min="0" max="4" value="2" data-stops="1|2|3|5|10" data-unit="year|years"><output for="cost-term">3 years</output></div>
<div class="cost-row"><label for="cost-currency">Currency</label><select id="cost-currency" data-currency><option value="EUR" selected>EUR</option><option value="USD">USD</option><option value="GBP">GBP</option><option value="CHF">CHF</option><option value="INR">INR</option><option value="JPY">JPY</option><option value="BRL">BRL</option><option value="AUD">AUD</option></select></div>
<div class="cost-sheet">
<div class="cost-line"><span class="cost-what">Framework license<small>MIT, commercial use included</small></span><span class="cost-amount" data-amount>€0</span></div>
<div class="cost-line"><span class="cost-what">Per-user fee<small><span data-echo="cost-users">250 users</span>, and nobody is counting them</small></span><span class="cost-amount" data-amount>€0</span></div>
<div class="cost-line"><span class="cost-what">Per-system fee<small><span data-echo="cost-systems">3 systems</span>: abapGit pulls the same repository into each of them</small></span><span class="cost-amount" data-amount>€0</span></div>
<div class="cost-line"><span class="cost-what">Per-app fee<small><span data-echo="cost-apps">10 apps</span>, each one a class in your own system</small></span><span class="cost-amount" data-amount>€0</span></div>
<div class="cost-line"><span class="cost-what">Developer seats<small><span data-echo="cost-developers">5 developers</span>, with ADT, the playground and the VS Code extension</small></span><span class="cost-amount" data-amount>€0</span></div>
<div class="cost-line"><span class="cost-what">Platform<small><span data-echo="cost-system">S/4HANA on-premise</span>: the same class runs on every release from 7.02 up</small></span><span class="cost-amount" data-amount>€0</span></div>
<div class="cost-line"><span class="cost-what">Runtime<small><span data-echo="cost-roundtrips">100,000 roundtrips a day</span>, every one of them served by your own ABAP stack</small></span><span class="cost-amount" data-amount>€0</span></div>
<div class="cost-line"><span class="cost-what">Frontend hosting<small><span data-echo="cost-hosting">the UI5 your system ships</span>, and no frontend deployment</small></span><span class="cost-amount" data-amount>€0</span></div>
<div class="cost-line"><span class="cost-what">Support<small><span data-echo="cost-support">Community</span> tier: GitHub issues and Slack, the same as every other tier</small></span><span class="cost-amount" data-amount>€0</span></div>
<div class="cost-line"><span class="cost-what">Upgrades<small><span data-echo="cost-term">3 years</span> of releases, each one an abapGit pull</small></span><span class="cost-amount" data-amount>€0</span></div>
</div>
<div class="cost-total">
<div><span class="cost-total-what">Total, over <span data-echo="cost-term">3 years</span></span><output data-total>€0</output></div>
<div><span class="cost-total-what">Per year</span><output data-total>€0</output></div>
<div><span class="cost-total-what">Per user, per month</span><output data-total>€0</output></div>
</div>
</div>

The formula is short: every line is zero, and a sum of zeros is zero. Slide the users to 250,000, the systems to 50 and the support tier to Enterprise Plus - the sheet does not move, because nothing is counting. abap2UI5 is [MIT licensed](/resources/license), commercial use included, and an app built with it is a standard UI5 app served by the ABAP stack you already run.

## What is not on the sheet

Two things, and they are the same for any ABAP program: the SAP license you already have, and the time of whoever writes the app. Both were yours before abap2UI5 and stay yours with it.

## The one line that is not zero

abap2UI5 is open-source work. The people who build and maintain it do so in their free time, and the zero above is what they give away. If this page just took a budget line off your project, consider giving a little of it back: [sponsor the contributors](/resources/sponsor), and the open-source projects abap2UI5 is built on.
