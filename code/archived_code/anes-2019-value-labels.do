* Create Value Labels for ANES 2019 Dataset

cd "C:\Users\damon\Dropbox\Viability and Race\Data\anes_pilot_2019_dta"
use "anes-2019-cleaned.dta"

label define vote20demlabel -1 "Democrat" 0 "Neither" 1 "Republican"
label values vote20dem vote20demlabel

label define vote20candlabel 1 "Biden" 2 "Bloomberg" 3 "Booker" 4 "Buttigieg" 5 "Klobuchar" 6 "Sanders" 7 "Warren" 8 "Yang" 9 "Someone Else"
label values vote20cand vote20candlabel

label define electablelabel 1 "Shares Positions" 2 "Beat Trump" 3 "Neither" 4 "Don't Know"
label values electable electablelabel

label define vote20jblabel -1 "Biden" 0 "Neither" 1 "Trump"
label values vote20jb vote20jblabel

label define vote20ewlabel -1 "Warren" 0 "Neither" 1 "Trump"
label values vote20ew vote20ewlabel

label define vote20bslabel -1 "Sanders" 0 "Neither" 1 "Trump"
label values vote20bs vote20bslabel

label define ideollabel -3 "Very Liberal" -2 "Liberal" -1 "Lean Liberal" 0 "Neither" 1 "Lean Conservative" 2 "Conservative" 3 "Very Conservative"
label values lcself ideollabel
label values lcd ideollabel
label values lcr ideollabel

label define ideolabel2 -2 "Very Liberal" -1 "Liberal" 0 "Neither" 1 "Conservative" 2 "Liberal"
label values ideo5 ideollabel2

label define likert5 -2 "Strongly Disagree" -1 "Disagree" 0 "Neither" 1 "Agree" 2 "Strongly Agree"
label values pop1 likert5
label values pop2 likert5
label values pop3 likert5

label define pidlabel -3 "Strong Democrat" -2 "Democrat" -1 "Lean Democrat" 0 "Independent" 1 "Lean Republican" 2 "Republican" 3 "Strong Republican"
label values pid7 pidlabel

label define yesno 0 "No" 1 "Yes"
label values att1 yesno

label define genderlabel 0 "Female" 1 "Male"
label values gender genderlabel

label define blacklabel 0 "non-Black" 1 "Black"
label values black blacklabel

label define whitelabel 0 "non-white" 1 "white"
label values white whitelabel

label define newsintlabel 1 "Hardly" 2 "Now and then" 3 "Sometimes" 4 "Most of the time"
label values newsint newsintlabel

save "anes-2019-cleaned.dta",replace

tab electable

recode electable (2=1) (1 3 4 = 0), gen(elect2)

ttest elect2, by(black)
