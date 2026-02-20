

clear all
cd "C:\Users\nguyenl33\OneDrive - Aalto University\Documents"
*1. Append data and rename
import delimited using "tilinpaatos_tunnusluvut_2000_2009.csv", ///
    clear varnames(1) case(lower) encoding("UTF-8")

destring henkilostonlkm, replace ignore(",") force
gen status = 0   // continued / active
tempfile base
save `base', replace


import delimited using "tilinpaatos_tunnusluvut_2010_2015.csv", ///
    clear varnames(1) case(lower) encoding("UTF-8")

destring henkilostonlkm, replace ignore(",") force
gen status = 0
append using `base'
save `base', replace


import delimited using "tilinpaatos_tunnusluvut_2016_2022.csv", ///
    clear varnames(1) case(lower) encoding("UTF-8")

destring henkilostonlkm, replace ignore(",") force
gen status = 0
append using `base'
save `base', replace

* Append discontinued operations
import delimited using "tilinpaatos_tunnusluvut_2016_2022_lakanneet.csv", ///
    clear varnames(1) case(lower) encoding("UTF-8")

destring henkilostonlkm, replace ignore(",") force
gen status = 1   // discontinued
append using `base'
save `base', replace

import delimited using "tilinpaatos_tunnusluvut_2010_2015_lakanneet.csv", ///
    clear varnames(1) case(lower) encoding("UTF-8")

destring henkilostonlkm, replace ignore(",") force
gen status = 1   // discontinued
append using `base'
save `base', replace

import delimited using "tilinpaatos_tunnusluvut_2000_2009_lakanneet.csv", ///
    clear varnames(1) case(lower) encoding("UTF-8")

destring henkilostonlkm, replace ignore(",") force
gen status = 1   // discontinued
append using `base'
save `base', replace

use `base', clear
label define firm_status 0 "continued" 1 "discontinued"
label values status firm_status
* IDs / fiscal period
capture rename ytunnus                firm_id
capture rename yrityksennimi           firm_name
capture rename tilikaudenalkupvm       fy_start_raw
capture rename tilikaudenpaattymispvm  fy_end_raw
capture rename tilikaudenpituuskk      fy_length_months
capture rename tilintarkastuslauseke   audit_clause
capture rename henkilostonlkm          employees

* Income statement (P&L) 
capture rename liikevaihto             is_revenue
capture rename valmis_varasto_muutos   is_chg_fg_inventory
capture rename valmistus_omaan_kayt    is_own_use_production
capture rename liiketoiminnan_tuot     is_other_oper_income

capture rename ostot_tk_aikana         is_purchases
capture rename varastojen_muutos       is_chg_inventories
capture rename ulkopuoliset_palvelut   is_outsourced_services

capture rename bruttotulos             is_gross_profit

capture rename palkat                  is_wages
capture rename elakekulut              is_pension_costs
capture rename muut_henkilokulut       is_other_staff_costs

capture rename suunnitelma_poistot     is_depreciation
capture rename arvonalentumiset        is_impairments
capture rename vaihtuvien_arvonalen    is_curr_assets_writeoffs

capture rename liiketoiminnan_kulut    is_oper_expenses
capture rename liiketoiminnan_tulos    is_oper_profit

capture rename rah_tuotot_kons_yri     is_fin_inc_group
capture rename rah_tuotot_omistus      is_fin_inc_associates
capture rename rah_tuotot_sijoitus     is_fin_inc_investments
capture rename rah_muut_korkotuotot    is_fin_interest_inc_other
capture rename rah_arvonalentumiset    is_fin_writeoffs
capture rename rah_vaiht_arvoalen      is_fin_curr_inv_writeoffs
capture rename rah_korkokulut          is_interest_exp

capture rename tulos_ennen_sat_era     is_profit_before_extra
capture rename satunnaiset_tuotot      is_extra_income
capture rename satunnaiset_kulut       is_extra_expense
capture rename tulos_ennen_veroja      is_profit_before_tax

capture rename poistoero               is_depr_reserve_change
capture rename vapaaeht_varaus         is_voluntary_prov_change
capture rename tuloverot               is_income_taxes
capture rename muut_verot              is_other_taxes
capture rename lask_verovelka          is_deferred_tax_liab
capture rename tilikauden_tulos        is_net_income

capture rename poistoero_1             bs_depr_diff_stock
capture rename lask_verovelka_1        bs_deferred_tax_liab
capture rename tilikauden_tulos_1      net_income_dup

* Balance sheet: Assets 
capture rename perustamismenot         bs_startup_costs
capture rename tutkimusmenot           bs_research_costs
capture rename kehittamismenot         bs_development_costs
capture rename aineettomat_oikeudet    bs_intangible_rights
capture rename liikearvo               bs_goodwill
capture rename muut_pitka_menot        bs_other_lt_assets
capture rename ennakkomaksut           bs_advances_paid

capture rename maa_ja_vesialueet       bs_land_water
capture rename rakennukset             bs_buildings
capture rename koneet_ja_kalusto       bs_machinery_equip
capture rename muut_aineelliset_hyod   bs_other_tangible
capture rename ennakko_kesken          bs_cip_advances

capture rename osuudet_kons_yritys     bs_invest_subs
capture rename saamiset_kons_yritys    bs_recv_subs
capture rename osuudet_omistusyhteys   bs_invest_associates
capture rename saamiset_omistusyhteys  bs_recv_associates
capture rename muut_osakkeet_ja_osuudet bs_other_shares
capture rename muut_saamiset           bs_other_receivables
capture rename omat_osakkeet           bs_own_shares_asset

capture rename aineet_ja_tarvikkeet    bs_inv_raw_materials
capture rename keskeneraiset_tuotteet  bs_inv_wip
capture rename valmiit_tuotteet        bs_inv_finished_goods
capture rename muu_vaihto_omaisuus     bs_inv_other
capture rename vaihto_ennakkomaksut    bs_inv_advances

* Receivables (LT)
capture rename pitka_myyntisaamiset    bs_ar_lt
capture rename pitka_saamiset_kons_yri bs_recv_lt_subs
capture rename pitka_saamiset_omistus  bs_recv_lt_assoc
capture rename pitka_lainasaamiset     bs_loan_lt
capture rename pitka_lask_verosaaminen bs_def_tax_asset_lt
capture rename pitka_muut_saamiset     bs_other_recv_lt
capture rename pitka_maksamattomat_osak bs_unpaid_shares_lt
capture rename pitka_siirtosaamiset    bs_prepaid_lt

* Receivables (ST)
capture rename lyhyt_myyntisaamiset    bs_ar_st
capture rename lyhyt_saamiset_kons_yri bs_recv_st_subs
capture rename lyhyt_saamiset_omistus  bs_recv_st_assoc
capture rename lyhyt_lainasaamiset     bs_loan_st
capture rename lyhyt_lask_verosaaminen bs_def_tax_asset_st
capture rename lyhyt_muut_saamiset     bs_other_recv_st
capture rename lyhyt_maksamattomat_osak bs_unpaid_shares_st
capture rename lyhyt_siirtosaamiset    bs_prepaid_st

* Securities & cash
capture rename rah_osuudet_kons        bs_sec_subs
capture rename rah_omat_osakkeet       bs_sec_own_shares
capture rename rah_muut_osakkeet       bs_sec_other_shares
capture rename rah_muut_arvopaperit    bs_sec_other
capture rename rahat_ja_pankki         bs_cash

capture rename taseen_loppu_vastaava   bs_total_assets
capture rename taseen_loppusumma       bs_total_assets2

* Balance sheet: Equity & liabilities 
capture rename osake_osuus_paaoma      eq_share_capital
capture rename ylikurssirahasto        eq_share_premium
capture rename arvonkorotusrahasto     eq_revaluation_reserve
capture rename kayvanarvonrahasto      eq_fair_value_reserve

capture rename omat_osakkeet_rahasto   eq_own_shares_reserve
capture rename vararahasto             eq_statutory_reserve
capture rename yhtiojarjestys_rahasto  eq_articles_reserve
capture rename muut_rahastot           eq_other_reserves
capture rename edellisten_tk_tulos     eq_retained_earnings
capture rename paaomalainat            eq_capital_loans

capture rename vapaaeht_varaukset      bs_voluntary_provisions
capture rename elakevaraukset          bs_pension_provisions
capture rename verovaraukset           bs_tax_provisions
capture rename muut_pakoll_varaukset   bs_other_provisions

* Long-term liabilities
capture rename joukkovelkakirjalainat  debt_lt_bonds
capture rename vaihtovelkakirjalainat  debt_lt_convertible
capture rename pitka_paaomalainat      debt_lt_capital_loans
capture rename lainat_rahoituslaitos   debt_lt_bank_loans
capture rename elakelainat             debt_lt_pension_loans
capture rename saadut_ennakot          debt_lt_advances_received
capture rename ostovelat               debt_lt_trade_payables
capture rename rahoitusvekselit        debt_lt_bills_of_exchange
capture rename velat_kons_yrityksille  debt_lt_due_to_group
capture rename velat_omistusyhteys     debt_lt_due_to_assoc
capture rename muut_velat              debt_lt_other
capture rename siirtovelat             debt_lt_accruals

* Short-term liabilities
capture rename lyhyt_joukkovelka       debt_st_bonds
capture rename lyhyt_vaihtovelka       debt_st_convertible
capture rename lyhyt_paaomalainat      debt_st_capital_loans
capture rename lyhyt_laina_rah_laitos  debt_st_bank_loans
capture rename lyhyt_elakelainat       debt_st_pension_loans
capture rename lyhyt_saadut_ennakot    debt_st_advances_received
capture rename lyhyt_ostovelat         debt_st_trade_payables
capture rename lyhyt_rahoitusvekselit  debt_st_bills_of_exchange
capture rename lyhyt_velat_kons_yri    debt_st_due_to_group
capture rename lyhyt_velat_intressi_yri debt_st_due_to_interest_cos
capture rename lyhyt_muut_velat        debt_st_other
capture rename lyhyt_siirtovelat       debt_st_accruals

* KPI / ratios 
capture rename volyymi                 kpi_volume
capture rename liikevaihto_eur         kpi_revenue_eur
capture rename liikevaihdon_muutos     kpi_rev_growth_pct
capture rename liikevaihdon_muutos_    kpi_rev_growth_pct
capture rename bruttotuloksen_muutos   kpi_gross_profit_growth_pct
capture rename bruttotuloksen_muutos_  kpi_gross_profit_growth_pct

capture rename kayttokate              kpi_ebitda_margin_pct
capture rename kayttokate_             kpi_ebitda_margin_pct
capture rename liikevoitto             kpi_ebit_margin_pct
capture rename liikevoitto_            kpi_ebit_margin_pct
capture rename sijoitetun_paaoman_tuotto kpi_roic_pct
capture rename sijoitetun_paaoman_tuotto_ kpi_roic_pct
capture rename kokonaispaaoman_tuotto  kpi_roa_pct
capture rename kokonaispaaoman_tuotto_ kpi_roa_pct

capture rename current_ratio           kpi_current_ratio
capture rename omavaraisuusaste        kpi_equity_ratio_pct
capture rename omavaraisuusaste_       kpi_equity_ratio_pct
capture rename suhteellinen_velkaantuneisuus kpi_rel_debt_pct
capture rename suhteellinen_velkaantuneisuus_ kpi_rel_debt_pct

capture rename oma_paaoma_eur          kpi_equity_eur
capture rename kayttopaaoma            kpi_working_capital
capture rename myyntisaamisten_kiertoaika_pv kpi_ar_days
capture rename myyntisaamistenkiertoaikapv   kpi_ar_days

save "asiakastieto_fs_appended_en.dta", replace

* Merge yritysten_perustiedot.csv into firm-year data

clear all
cd "C:\Users\nguyenl33\OneDrive - Aalto University\Documents"

use "asiakastieto_fs_appended_en.dta", clear
capture confirm string variable firm_id
if _rc==0 {
    replace firm_id = subinstr(firm_id, "-", "", .)
    replace firm_id = subinstr(firm_id, " ", "", .)
}
else {
    tostring firm_id, replace format(%15.0f)
    replace firm_id = subinstr(firm_id, "-", "", .)
    replace firm_id = subinstr(firm_id, " ", "", .)
}

tempfile fs
save `fs', replace
import delimited using "yritysten_perustiedot.csv", ///
    clear varnames(1) case(lower) encoding("UTF-8") ///


capture drop kayntiosoite
capture drop kayntipostinumero
capture drop kayntitoimipaikka
capture drop postitusosoite
capture drop postitusnro
capture drop postitustoimipaikka
capture drop puhelin
capture drop kotisivu
capture drop riskiluokka
capture drop alv_rekisterissa
capture drop eper_rekisterissa
capture drop tyonantajarekisterissa
capture drop esg_ymparistovastuu
capture drop esg_sosiaalinen_vastuu
capture drop esg_hallinnollinen_vastuu
	
capture rename ytunnus firm_id
capture rename virallinen_nimi official_name
capture rename aloituspvm start_date
capture rename kaupparekisteripvm trade_register_date
capture rename yhtiomuoto legal_form
capture rename kunta municipality
capture rename maakunta region
capture rename toimialakoodi_taso2 industry_code_l2
capture rename toimialateksti_taso2 industry_text_l2
capture rename toimialakoodi_taso5 industry_code_l5
capture rename toimialateksti_taso5 industry_text_l5
capture rename henkiloluokka employee_size_class
capture rename liikevaihtoluokka turnover_size_class
capture rename kokoluokitusvuosi size_class_year
capture rename yrityksella_tuontia has_imports
capture rename yrityksella_vientia has_exports
capture rename yrityksen_tila company_status

tempfile master
save `master', replace
use `fs', clear
merge m:1 firm_id using `master', keep(match master) nogen
count if missing(official_name)   
summ industry_code_l2 industry_code_l5, meanonly

save "asiakastieto_fs_with_perustiedot_en.dta", replace

* Merge yritysten_perustiedot.csv into firm-year data (not all firm_id match)

clear all
cd "C:\Users\nguyenl33\OneDrive - Aalto University\Documents"

tempfile fs master

use "asiakastieto_fs_appended_en.dta", clear
capture confirm string variable firm_id
if _rc==0 {
    replace firm_id = subinstr(firm_id, "-", "", .)
    replace firm_id = subinstr(firm_id, " ", "", .)
}
else {
    tostring firm_id, replace format(%15.0f)
    replace firm_id = subinstr(firm_id, "-", "", .)
    replace firm_id = subinstr(firm_id, " ", "", .)
}

save `fs', replace

import delimited using "yritysten_perustiedot.csv", ///
    clear varnames(1) case(lower) encoding("UTF-8")

capture drop kayntiosoite
capture drop kayntipostinumero
capture drop kayntitoimipaikka
capture drop postitusosoite
capture drop postitusnro
capture drop postitustoimipaikka
capture drop puhelin
capture drop kotisivu
capture drop riskiluokka
capture drop alv_rekisterissa
capture drop eper_rekisterissa
capture drop tyonantajarekisterissa
capture drop esg_ymparistovastuu
capture drop esg_sosiaalinen_vastuu
capture drop esg_hallinnollinen_vastuu

capture rename ytunnus firm_id
capture confirm numeric variable firm_id
if _rc==0 {
    tostring firm_id, replace format(%08.0f)
}
replace firm_id = subinstr(firm_id, "-", "", .)
replace firm_id = subinstr(firm_id, " ", "", .)
* ------------------------------------------------------------

capture rename virallinen_nimi official_name
capture rename aloituspvm start_date
capture rename kaupparekisteripvm trade_register_date
capture rename yhtiomuoto legal_form
capture rename kunta municipality
capture rename maakunta region
capture rename toimialakooditaso2 industry_code_l2
capture rename toimialatekstitaso2 industry_text_l2
capture rename toimialakooditaso5 industry_code_l5
capture rename toimialatekstitaso5 industry_text_l5
capture rename henkiloluokka employee_size_class
capture rename liikevaihtoluokka turnover_size_class
capture rename kokoluokitusvuosi size_class_year
capture rename yrityksella_tuontia has_imports
capture rename yrityksella_vientia has_exports
capture rename yrityksen_tila company_status

gsort firm_id -size_class_year
duplicates drop firm_id, force
save `master', replace

use `fs', clear
merge m:1 firm_id using `master', keep(match master) nogen

save "asiakastieto_fs_with_perustiedot_en.dta", replace


*some fiscal year is not 1.1 to 31.12. 

* 1. SETUP 

clear all
cd "C:\Users\nguyenl33\OneDrive - Aalto University\Documents"
use "asiakastieto_fs_with_perustiedot_en.dta", clear

* Labor Cost & Value Added 
gen miss_pension    = missing(is_pension_costs)
gen miss_otherstaff = missing(is_other_staff_costs)

gen pension0 = is_pension_costs
replace pension0 = 0 if missing(pension0) & !missing(is_wages)

gen otherstaff0 = is_other_staff_costs
replace otherstaff0 = 0 if missing(otherstaff0) & !missing(is_wages)

gen labor_cost = is_wages + pension0 + otherstaff0
replace labor_cost = . if missing(is_wages)

gen depr0 = is_depreciation
replace depr0 = 0 if missing(depr0) & !missing(is_oper_profit)

gen ebitda = is_oper_profit + depr0
replace ebitda = . if missing(is_oper_profit)

gen value_added = is_oper_profit + depr0 + labor_cost
replace value_added = . if missing(is_oper_profit) | missing(labor_cost)

* Balance Sheet & Capital 
gen total_assets  = bs_total_assets
replace total_assets = bs_total_assets2 if missing(total_assets) & !missing(bs_total_assets2)
replace total_assets = . if total_assets<=0

egen K_tangible = rowtotal(bs_land_water bs_buildings bs_machinery_equip bs_other_tangible bs_cip_advances)

* Debt Structure 
egen debt_st = rowtotal(debt_st_bonds debt_st_convertible debt_st_capital_loans debt_st_bank_loans ///
                        debt_st_pension_loans debt_st_advances_received debt_st_trade_payables ///
                        debt_st_bills_of_exchange debt_st_due_to_group debt_st_due_to_interest_cos ///
                        debt_st_other debt_st_accruals)

egen debt_lt = rowtotal(debt_lt_bonds debt_lt_convertible debt_lt_capital_loans debt_lt_bank_loans ///
                        debt_lt_pension_loans debt_lt_advances_received debt_lt_trade_payables ///
                        debt_lt_bills_of_exchange debt_lt_due_to_group debt_lt_due_to_assoc ///
                        debt_lt_other debt_lt_accruals)

gen debt_total = debt_st + debt_lt
gen total_debt_to_asset = debt_total/total_assets 

* Ratios & Log Transformations
gen K_per_worker = K_tangible/employees if K_tangible>0 & employees>0
gen ln_K_pw      = log(K_per_worker)     if K_per_worker>0

gen va_pw = value_added/employees
gen ln_va_pw    = log(va_pw)             if va_pw>0

gen rev_pw      = is_revenue/employees  if is_revenue>0 & employees>0
gen ln_rev_pw   = log(rev_pw)           if rev_pw>0

gen operating_margin    = is_oper_profit/is_revenue if is_oper_profit!=. & is_revenue>0
gen ebitda_margin = ebitda/is_revenue         if ebitda!=.         & is_revenue>0

gen labor_share  = labor_cost/value_added    if labor_cost>=0 & value_added>0
gen labor_cost_pw= labor_cost/employees      if labor_cost!=. & employees>0
gen ln_emp = log(employees) if employees>0
* 2. VISUALIZATION 

* 2.1 Macro Trend Visuals 

    keep if inrange(fy_length_months, 11, 13)
    gen fy_end = .
    format fy_end %td
    replace fy_end = date(fy_end_raw, "YMD") if fy_end == .
    replace fy_end = date(fy_end_raw, "DMY") if fy_end == .
    gen year = year(fy_end)

    bysort year: egen n_firms = count(firm_id)
    bysort year: egen emp_total = total(employees)
    bysort year: egen share_discontinued = mean(status)

    twoway line n_firms year, ///
        title("Dataset coverage: number of firm-year obs") ///
        xtitle("Year") ytitle("Count")

    twoway line emp_total year, ///
        title("Dataset coverage: total employees (in sample)") ///
        xtitle("Year") ytitle("Employees (sum)")

    twoway line share_discontinued year, ///
        title("Share discontinued in dataset (by year)") ///
        xtitle("Year") ytitle("Mean(status)") ///
        ylabel(0(.1)1)

* 2.2 Value Added KPIs 
capture gen year = year(date(fy_end_raw, "YMD")) 
capture replace year = year(date(fy_end_raw, "DMY")) if missing(year)

bysort year: egen VA_total = total(value_added)
bysort year: egen emp_total = total(employees)
bysort year: egen med_va_pw = median(va_pw)
gen VA_per_worker = VA_total / emp_total

sort year
gen VA_index   = 100 * VA_total / VA_total[1]
gen VApw_index = 100 * VA_per_worker / VA_per_worker[1]
gen Med_index  = 100 * med_va_pw / med_va_pw[1]


*2014 eu debt crisis. VA/worker increase due to being lagging indicator CFA econ (business cycle)
twoway (line VA_index year, lpattern(dash)) ///
       (line VApw_index year, lwidth(medium)) ///
       (line Med_index year, lwidth(medium)), ///
    title("Productivity & Output Trends") ///
    subtitle("Indexed to Base Year = 100") ///
    legend(order(1 "Total VA" 2 "Mean VA/Worker" 3 "Median VA/Worker")) ///
    xtitle("Year") ///
    ytitle("Index (Base Year = 100)") ///
    scheme(s2color)

*  2.3 Continued vs Discontinued 
preserve
bysort year status: egen VA_total2  = total(value_added)
bysort year status: egen emp_total2 = total(employees)
gen VA_per_worker2 = VA_total2/emp_total2 if emp_total2>0
bysort year status: gen tag_ys = (_n==1)
keep if tag_ys
drop tag_ys
bysort status: egen base_vapw = max(cond(year==2000, VA_per_worker2, .))
bysort status: egen base_va   = max(cond(year==2000, VA_total2, .))

gen VApw_index2_fixed     = 100*VA_per_worker2/base_vapw if base_vapw<.
gen VA_total_index2_fixed = 100*VA_total2/base_va        if base_va<.

sort status year

twoway (line VApw_index2_fixed year if status==0, lcolor(blue)) ///
       (line VApw_index2_fixed year if status==1, lcolor(red)), ///
       title("VA per worker: continued vs discontinued") ///
       subtitle("Employment-weighted (Indexed 2000=100)") ///
       legend(order(1 "continued" 2 "discontinued")) ///
       xtitle("Year") ytitle("Index (2000=100)") ///
       yline(100, lpattern(dash) lcolor(gs12))

twoway (line VA_total_index2_fixed year if status==0, lcolor(blue)) ///
       (line VA_total_index2_fixed year if status==1, lcolor(red)), ///
       title("Total Value Added: continued vs discontinued") ///
       subtitle("Total output (Indexed 2000=100)") ///
       legend(order(1 "continued" 2 "discontinued")) ///
       xtitle("Year") ytitle("Index (2000=100)") ///
       yline(100, lpattern(dash) lcolor(gs12))

*already trimmed the top 1% outlier but the 2015 still spike
restore
* 3. PANEL & SECTOR SETUP

gen macro_sector = .
replace macro_sector = 1 if inrange(industry_code_l2, 10, 33) 
replace macro_sector = 2 if inrange(industry_code_l2, 41, 43) 
replace macro_sector = 3 if inrange(industry_code_l2, 45, 47) 
replace macro_sector = 4 if inrange(industry_code_l2, 49, 53) 
replace macro_sector = 5 if inrange(industry_code_l2, 58, 63) 
replace macro_sector = 6 if inrange(industry_code_l2, 69, 75) 
replace macro_sector = 7 if inrange(industry_code_l2, 1, 3)   
replace macro_sector = 8 if inrange(industry_code_l2, 5, 9)
replace macro_sector = 9 if inrange(industry_code_l2, 35, 39) 
replace macro_sector = 10 if inrange(industry_code_l2, 55, 56) 
replace macro_sector = 11 if inrange(industry_code_l2, 64, 66) 
replace macro_sector = 12 if industry_code_l2 == 68            
replace macro_sector = 13 if inrange(industry_code_l2, 77, 82) 

replace macro_sector = 14 if inrange(industry_code_l2, 84, 88) 
replace macro_sector = 15 if inrange(industry_code_l2, 90, 99) 
label define sector_lab ///
    1 "Manufacturing" 2 "Construction" 3 "Trade" 4 "Transport" ///
    5 "ICT" 6 "Prof. Services" 7 "Agriculture" 8 "Mining" ///
    9 "Utilities" 10 "Accom. & Food" 11 "Finance" 12 "Real Estate" ///
    13 "Admin Services" 14 "Public & Health" 15 "Arts & Other", replace

label values macro_sector sector_lab
drop if macro_sector == .
sort firm_id year fy_length_months
duplicates drop firm_id year, force
egen panel_id = group(firm_id)
xtset panel_id year
* 4. TFP CALCULATION

gen ln_VA=ln(value_added)
gen ln_K=  ln(K_tangible)
gen ln_L= ln(employees) 
reg ln_VA ln_K ln_L if value_added>0 & K_tangible>0 & employees>0
*robustness check
ssc install prodest, replace
gen materials= is_revenue-value_added
gen ln_material=ln(materials)
prodest ln_VA, free(ln_L) state(ln_K) proxy(ln_material) method(wrdg) va 
gen double TFP_wrdg = ln_VA - (_b[ln_L] * ln_L) - (_b[ln_K] * ln_K)


* 5. REGRESSIONS 

* Model 1: Determinants of Labor Productivity (VA per Worker) 
xtreg ln_va_pw ln_K_pw ln_emp operating_margin total_debt_to_asset i.year, fe robust

reg ln_va_pw ln_K_pw ln_emp operating_margin total_debt_to_asset i.year i.macro_sector, vce(cluster firm_id)


* Model 2: Determinants of TFP
xtreg TFP_wrdg ln_emp operating_margin total_debt_to_asset i.year , fe

reg TFP_wrdg ln_emp operating_margin total_debt_to_asset i.year i.macro_sector, vce(cluster firm_id)


	
* Construct total intangible assets 
* 2 issues: writeoff-reverse / sau phase R&D capitalize expense
egen K_intangible = rowtotal(bs_startup_costs bs_research_costs ///
                             bs_development_costs bs_intangible_rights bs_goodwill ///
                             bs_other_lt_assets)

gen K_int_per_worker = K_intangible / employees if employees > 0 & K_intangible>0
gen ln_Kint_pw      = log(K_int_per_worker)    if K_int_per_worker>0


* Regression including tangibles and intangibles
xtreg ln_va_pw ln_K_pw ln_Kint_pw ln_emp operating_margin total_debt_to_asset i.year , fe vce(cluster firm_id)

*estout
ssc install estout, replace
eststo clear

* TABLE 1: DETERMINANTS OF LABOR PRODUCTIVITY 

* Model 1: Pooled OLS (Cross-Section Benchmark)
reg ln_va_pw ln_K_pw ln_emp operating_margin total_debt_to_asset i.year i.macro_sector, vce(cluster firm_id)
eststo lab_ols

* Model 2: Fixed Effects (Within-Firm Baseline)
xtreg ln_va_pw ln_K_pw ln_emp operating_margin total_debt_to_asset i.year, fe robust
eststo lab_fe

* Model 3: Fixed Effects + Intangibles (The "Sophisticated" Model)
xtreg ln_va_pw ln_K_pw ln_Kint_pw ln_emp operating_margin total_debt_to_asset i.year, fe robust
eststo lab_fe_int

* TABLE 2: DETERMINANTS OF TFP (The Efficiency Story)

* Model 4: TFP Pooled OLS
reg TFP_wrdg ln_emp operating_margin total_debt_to_asset i.year i.macro_sector, vce(cluster firm_id)
eststo tfp_ols

* Model 5: TFP Fixed Effects
xtreg TFP_wrdg ln_emp operating_margin total_debt_to_asset i.year, fe robust
eststo tfp_fe
	
esttab lab_ols lab_fe lab_fe_int using "Table1_LaborProd.rtf", replace ///
    cells(b(star fmt(3)) se(par fmt(3))) ///
    legend label collabels(none) ///
    stats(N r2_a r2_w, fmt(0 3 3) labels("Observations" "Adj R-Squared" "Within R-Squared")) ///
    keep(ln_K_pw ln_Kint_pw ln_emp operating_margin total_debt_to_asset) ///
    order(ln_K_pw ln_Kint_pw ln_emp operating_margin total_debt_to_asset) ///
    varlabels(ln_K_pw "Tangible Capital (log)" ///
              ln_Kint_pw "Intangible Capital (log)" ///
              ln_emp "Firm Size (log)" ///
              operating_margin "Operating Margin" ///
              total_debt_to_asset "Debt Ratio") ///
    mtitles("Pooled OLS" "Fixed Effects" "FE + Intangibles") ///
    title("Table 1: Determinants of Labor Productivity") ///
    addnote("Standard errors in parentheses. * p<0.10, ** p<0.05, *** p<0.01")
	
esttab tfp_ols tfp_fe using "Table2_TFP.rtf", replace ///
    cells(b(star fmt(3)) se(par fmt(3))) ///
    legend label collabels(none) ///
    stats(N r2_a r2_w, fmt(0 3 3) labels("Observations" "Adj R-Squared" "Within R-Squared")) ///
    keep(ln_emp operating_margin total_debt_to_asset) ///
    varlabels(ln_emp "Firm Size (log)" ///
              operating_margin "Operating Margin" ///
              total_debt_to_asset "Debt Ratio") ///
    mtitles("Pooled OLS" "Fixed Effects") ///
    title("Table 2: Determinants of Total Factor Productivity (TFP)") ///
    addnote("Standard errors in parentheses. TFP calculated via Cobb-Douglas residuals.")



*Decomposition 

bysort year: egen total_emp_year = total(employees)
gen s_it = employees / total_emp_year
gen p_it = ln_va_pw

* calculate Components for Olley-Pakes
* Unweighted Mean Productivity (The "Within" baseline)
bysort year: egen p_bar_t = mean(p_it)

* Average Market Share (Should be 1/N)
bysort year: egen s_bar_t = mean(s_it)

*  The Covariance Term (Reallocation)
* Formula: Sum of (Share_i - AvgShare) * (Prod_i - AvgProd)
gen op_term = (s_it - s_bar_t) * (p_it - p_bar_t)
bysort year: egen reallocation = total(op_term)

* Aggregate Weighted Productivity (For checking)
bysort year: egen agg_prod = total(s_it * p_it)
preserve
    collapse (mean) p_bar_t reallocation agg_prod, by(year)

    label var p_bar_t "Within-Firm (Unweighted Mean)"
    label var reallocation "Reallocation (Covariance)"
   
    twoway (line p_bar_t year, lcolor(navy) lwidth(medium)) ///
           (line reallocation year, lcolor(maroon) lwidth(medium)), ///
           title("Olley-Pakes Productivity Decomposition") ///
           subtitle("Is aggregate growth driven by average efficiency or reallocation?") ///
           legend(order(1 "Within-Firm Component" 2 "Reallocation Component")) ///
           ytitle("Log Productivity Contribution") ///
           xtitle("Year") ///
           yline(0, lcolor(gs12))
restore
//quantile reg
ssc install xtqreg, replace

* Estimate effect at 10th, 50th, and 90th percentiles
xtqreg TFP_wrdg ln_emp operating_margin total_debt_to_asset i.year, quantile(.1 .5 .9)

ssc install winsor, replace
* Drop top/bottom 1% outliers robustness checks
winsor TFP_wrdg, p(0.01) gen(TFP_wrdg1)
xtqreg TFP_wrdg1 ln_emp operating_margin total_debt_to_asset i.year, quantile(.1 .5 .9)




