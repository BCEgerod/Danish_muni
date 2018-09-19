cd "C:\Users\au595748\Documents\GitHub\Danish_muni\data\"

import delim CityPolicy_18092018, delim(",") clear

foreach x of varlist * {

capture replace `x'="." if `x'=="NA" 

destring `x', replace
}


xtset muni year

gen difvotes=bluevote-l4.bluevote
replace difvotes=bluevote-l3.bluevote if year==1981
gen difsocdem=f4.correctsocdem-correctsocdem
hist difsoc

foreach x in soc votes{
reg dif`x' i.year
predict resid, resid
replace dif`x'=resid
drop resid
}
-
keep difsocdem difvotes muni year
-
keep if difsocdem!=.
keep if difvotes!=.

sort difvotes

gen bin=0
local i=0
forvalues x=0(10)1633 {
local i=`i'+1
forvalues t=1/10 {
replace bin=bin+`i' if _n==(`x'+`t')
}
}

gen largebin=round(bin*2/10)


bysort bin: egen binvotes=mean(difvotes)

bysort bin: egen binpolicy=mean(difsocdem)

bysort largebin: egen bin2votes=mean(difvotes)

bysort largebin: egen bin2policy=mean(difsocdem)


scatter difsocdem difvotes 

gen rug=-0.22
gen lab="|"
*legend(pos(5) ring(0) bmargin(medium) fcolor(%0) label(1 "Bins, n=10") label(2 "Bins, n=100") label(3 "Linear") label(4 "Lowess")) ///

scatter  binpolicy binvotes, msym(O) mcolor(black%30*0.3) msize(small) || ///
scatter  bin2policy bin2votes, msym(O) msize(large) mcolor(black%80) || ///
scatter rug difvotes, msym(none) mlab(lab) msize(tiny) || ///
lfit difsocdem difvotes, lpattern(solid)  lwidth(medthick) || lowess difsocdem difvotes, ///
scheme(plotplain) lpattern(dash) lwidth(medthick) bwidth(0.4) ///
legend(off) ///
xtitle("Difference in Net Support for Right-Wing Parties" "(Since last election)") ///
ytitle("Difference in Municipal Fiscal Conservertism" "(Between this and the next election)")

graph export "C:\Users\au595748\Documents\GitHub\Danish_muni\images\scatterplot.eps", replace

