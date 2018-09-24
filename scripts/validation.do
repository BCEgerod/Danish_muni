use "C:\Users\au595748\Dropbox\KV13\Clarity of responsibility\kv13.dta" , clear

graph set window fontface default

describe *



gen ideology=(Spm24-1)/10 if Spm24 <12 & Spm03==1

gen count=1

keep komnr ideology count

collapse ideology (sum) count, by(komnr)
sort komnr

preserve
use "C:\Users\au595748\Dropbox\KV13\kapitel om valgvind\valgvind.dta", clear
keep if aar==2013
keep a parti knavnnr
encode parti, gen(partid)
ta parti 
drop parti 
replace knavnnr="." if knavnnr=="#N/A"
replace a="." if a==","
destring *, dpcomma replace
reshape wide a , j(partid) i(knavnnr)
gen netblue=(a1+a4+a5+a10)-(a2+a9+a8+a6)
rename knavnnr komnr
keep komnr netblue 
sort komnr
tempfile elec
save `elec'
restore 

merge 1:1 komnr using `elec'

reg ideology netblue [w=count], r
pwcorr [w=count]
twoway  scatter ideology netblue [w=count], msym(Oh) || lfitci ideology netblue [w=count],     ciplot(rline) ///
scheme(plotplain) legend(off) xtitle(" " "Net Support for Conservative Parties (2013)") ///
ytitle("Mean Ideology Score (2013)") text(0.38 0.4 "Pearsons R=0.43")

cd "C:\Users\au595748\Documents\GitHub\Danish_muni\images"

graph export validation1.eps, replace

use "C:\Users\au595748\Documents\GitHub\Danish_muni\data\elec05data.dta", clear

pwcorr *

scatter  netfv netkv , msym(Oh)  ||  lfitci  netfv netkv, ciplot(rline) ///
scheme(plotplain) legend(off) xtitle(" " "Net Support for Conservative Parties (Municipal)") ///
ytitle("Net Support for Conservative Parties (National)") text(-0.15 0.5 "Pearsons R=0.56")

cd "C:\Users\au595748\Documents\GitHub\Danish_muni\images"

graph export validation2.eps, replace
