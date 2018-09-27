cd "C:\Users\au595748\Documents\GitHub\Danish_muni\data\"

import delim CityPolicy_18092018, delim(",") clear


xtset muni year

replace fd_bluevote="." if fd_bluevote=="NA" 
replace bluevote="." if bluevote=="NA" 
replace fd_pop="." if fd_pop=="NA" 
replace log_pop="." if log_pop=="NA" 
destring fd_bluevote bluevote log_pop fd_pop, replace
replace fd_bluevote=bluevote-l3.bluevote if year==1981
replace fd_pop=log_pop-l3.log_pop if year==1981
tostring fd_bluevote bluevote log_pop fd_pop, replace force

export delim CityPolicy_24092018, delim(",") replace
