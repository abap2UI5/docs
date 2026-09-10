---
outline: [2, 4]
description: What abap2UI5 costs for your users, systems, apps and support, worked out slider by slider - why every line of the sheet comes to the same number, what that number gets you, and who paid for it.
---
# Cost Calculator

Set the sliders to your landscape, tick your systems and your support, pick a currency, press Calculate, and read off what abap2UI5 will cost you.

<div class="cost" data-cost>
<div class="cost-inputs">
<div class="cost-row"><label for="cost-users">Named users</label><input id="cost-users" type="range" min="0" max="15" value="6" data-stops="1|5|10|25|50|100|250|500|1000|2500|5000|10000|25000|50000|100000|250000" data-unit="user|users"><output for="cost-users">250 users</output></div>
<div class="cost-row"><span class="cost-label" id="cost-system-label">Your systems</span><span class="cost-choices" role="group" aria-labelledby="cost-system-label" data-none="no system at all"><label><input type="checkbox" name="cost-system" value="702" data-choice="NetWeaver 7.02"> NetWeaver 7.02</label><label><input type="checkbox" name="cost-system" value="731" data-choice="NetWeaver 7.31"> NetWeaver 7.31</label><label><input type="checkbox" name="cost-system" value="740" data-choice="NetWeaver 7.40"> NetWeaver 7.40</label><label><input type="checkbox" name="cost-system" value="750" data-choice="NetWeaver 7.50"> NetWeaver 7.50</label><label><input type="checkbox" name="cost-system" value="752" data-choice="NetWeaver 7.52"> NetWeaver 7.52</label><label><input type="checkbox" name="cost-system" value="s4-onprem" data-choice="S/4HANA on-premise" checked> S/4HANA on-premise</label><label><input type="checkbox" name="cost-system" value="s4-private" data-choice="S/4HANA Private Cloud"> S/4HANA Private Cloud</label><label><input type="checkbox" name="cost-system" value="btp" data-choice="BTP ABAP Environment"> BTP ABAP Environment</label><label><input type="checkbox" name="cost-system" value="s4-public" data-choice="S/4HANA Public Cloud"> S/4HANA Public Cloud</label></span></div>
<div class="cost-row"><label for="cost-systems">Systems in the landscape</label><input id="cost-systems" type="range" min="0" max="12" value="2" data-stops="1|2|3|4|5|6|8|10|12|15|20|30|50" data-unit="system|systems"><output for="cost-systems">3 systems</output></div>
<div class="cost-row"><label for="cost-apps">Apps</label><input id="cost-apps" type="range" min="0" max="10" value="4" data-stops="1|2|3|5|10|20|50|100|200|500|1000" data-unit="app|apps"><output for="cost-apps">10 apps</output></div>
<div class="cost-row"><label for="cost-developers">Developers</label><input id="cost-developers" type="range" min="0" max="9" value="3" data-stops="1|2|3|5|10|20|50|100|200|500" data-unit="developer|developers"><output for="cost-developers">5 developers</output></div>
<div class="cost-row"><label for="cost-roundtrips">Load</label><input id="cost-roundtrips" type="range" min="0" max="6" value="3" data-stops="100|1000|10000|100000|1000000|10000000|100000000" data-unit="roundtrip a day|roundtrips a day"><output for="cost-roundtrips">100,000 roundtrips a day</output></div>
<div class="cost-row"><span class="cost-label" id="cost-support-label">Support</span><span class="cost-choices" role="radiogroup" aria-labelledby="cost-support-label"><label><input type="radio" name="cost-support" value="none" data-choice="No support"> No support</label><label><input type="radio" name="cost-support" value="community" data-choice="Community support" checked> Community support</label></span></div>
<div class="cost-row"><label for="cost-term">Contract term</label><input id="cost-term" type="range" min="0" max="4" value="2" data-stops="1|2|3|5|10" data-unit="year|years"><output for="cost-term">3 years</output></div>
<div class="cost-row"><label for="cost-currency">Currency</label><select id="cost-currency" data-currency><option value="EUR" selected>EUR</option><option value="USD">USD</option><option value="GBP">GBP</option><option value="CHF">CHF</option><option value="INR">INR</option><option value="JPY">JPY</option><option value="BRL">BRL</option><option value="AUD">AUD</option></select></div>
</div>
<p class="cost-actions"><button type="button" class="cost-calculate" data-calculate="Calculate" data-again="Change the numbers">Calculate</button></p>
<div class="cost-result" data-result hidden>
<div class="cost-sheet">
<div class="cost-line"><span class="cost-what">Framework license<small>MIT, commercial use included</small></span><span class="cost-amount" data-amount>€0</span></div>
<div class="cost-line"><span class="cost-what">Per-user fee<small><span data-echo="cost-users">250 users</span>, and nobody is counting them</small></span><span class="cost-amount" data-amount>€0</span></div>
<div class="cost-line"><span class="cost-what">Per-system fee<small><span data-echo="cost-systems">3 systems</span>: abapGit pulls the same repository into each of them</small></span><span class="cost-amount" data-amount>€0</span></div>
<div class="cost-line"><span class="cost-what">Per-app fee<small><span data-echo="cost-apps">10 apps</span>, each one a class in your own system</small></span><span class="cost-amount" data-amount>€0</span></div>
<div class="cost-line"><span class="cost-what">Developer seats<small><span data-echo="cost-developers">5 developers</span>, with ADT, the playground and the VS Code extension</small></span><span class="cost-amount" data-amount>€0</span></div>
<div class="cost-line"><span class="cost-what">Platform<small><span data-echo="cost-system">S/4HANA on-premise</span>: the same class runs on every one of them, from 7.02 up</small></span><span class="cost-amount" data-amount>€0</span></div>
<div class="cost-line"><span class="cost-what">Runtime<small><span data-echo="cost-roundtrips">100,000 roundtrips a day</span>, every one of them served by your own ABAP stack</small></span><span class="cost-amount" data-amount>€0</span></div>
<div class="cost-line"><span class="cost-what">Frontend hosting<small>the UI5 your system already ships, and no frontend deployment</small></span><span class="cost-amount" data-amount>€0</span></div>
<div class="cost-line"><span class="cost-what">Support<small><span data-echo="cost-support">Community support</span>: <a href="https://github.com/abap2UI5/abap2UI5/issues" target="_blank" rel="noopener">GitHub issues</a> and <a href="https://communityinviter.com/apps/abapgit/abap" target="_blank" rel="noopener">Slack</a> are open to everybody either way</small></span><span class="cost-amount" data-amount>€0</span></div>
<div class="cost-line"><span class="cost-what">Upgrades<small><span data-echo="cost-term">3 years</span> of releases, each one an abapGit pull</small></span><span class="cost-amount" data-amount>€0</span></div>
</div>
<div class="cost-total">
<div><span class="cost-total-what">Total, over <span data-echo="cost-term">3 years</span></span><output data-total>€0</output></div>
<div><span class="cost-total-what">Per year</span><output data-total>€0</output></div>
<div><span class="cost-total-what">Per user, per month</span><output data-total>€0</output></div>
</div>
</div>
</div>

<div class="cost-after" data-result hidden>

The formula is short: every line is zero, and a sum of zeros is zero, no matter what you cloicked before ;) . abap2UI5 is [MIT licensed](/resources/license), commercial use included, and what you build with it is a standard UI5 app served by the ABAP stack you already run.

<div class="cost-give">

## Free to use is not the same as free to make

Keep in mind that those zeros were written in somebody's evenings - the release you will pull next month, the answer under your issue, the 7.02 downport nobody asked for. That part of the bill is real; it is just not addressed to you.

And it is payable in a currency you already have. If this page took a line out of a budget, here is what puts a little of it back:

- **Sponsor some money.** [The contributors](/resources/sponsor), and the open-source projects abap2UI5 stands on. This is the one line that takes money, and the smallest amount is a real one.
- **Answer somebody.** A question in [Slack](https://communityinviter.com/apps/abapgit/abap) or under a [GitHub issue](https://github.com/abap2UI5/abap2UI5/issues) can save the next person an afternoon of searching.
- **Send what you built.** A bug report with a reproducer, a sample worth copying, a pull request - the feature you needed and wrote yourself is a feature everybody gets.
- **Say that it exists.** A blog post, a talk, a screenshot of your app in the launchpad, a word in the SAP Community. There is no marketing department; there is you.

Pick whichever is easiest this week. None of it comes with an invoice, and all of it counts.

</div>

</div>
