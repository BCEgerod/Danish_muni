use "C:\Users\mvl\Dropbox\KV13\Clarity of responsibility\kv13.dta" , clear

graph set window fontface default

describe *

ta Spm24

gen ideology=(Spm24-1)/10 if Spm24 <12 & Spm03==1

gen count=1

keep komnr ideology count

collapse ideology (sum) count, by(komnr)
sort komnr

preserve
use "C:\Users\mvl\Dropbox\KV13\kapitel om valgvind\valgvind.dta", clear
keep if aar==2013
keep a parti knavnnr
encode parti, gen(partid)
ta parti 
drop parti 
replace knavnnr="." if knavnnr=="#N/A"
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

twoway lfitci ideology netblue [w=count] || scatter ideology netblue [w=count], msym(Oh) ///
scheme(plotplain) legend(off) xtitle(" " "Net Support for Conservative Parties") ///
ytitle("Mean Ideology Score (2013)") text(0.45 0.4 "Pearsons R=0.4")
