cd "C:\Users\au595748\Documents\GitHub\Danish_muni\data\"

import delim CityPolicy_18092018, delim(",") clear


xtset muni year

replace fd_bluevote="." if fd_bluevote=="NA" 
replace bluevote="." if bluevote=="NA" 
destring fd_bluevote bluevote, replace
replace fd_bluevote=bluevote-l3.bluevote if year==1981
tostring fd_bluevote bluevote, replace force

export delim CityPolicy_20092018, delim(",") replace
