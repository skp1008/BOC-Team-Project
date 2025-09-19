* Set the working directory
cd "/Users/katherine/Downloads/BVAR"

* -------------------------------------------
* Step 1: Load and clean GDP data
* -------------------------------------------
import delimited "/Users/katherine/Downloads/BVAR/data/gdp.csv", clear

* Create date variable and format it
gen date_corrected = monthly(v1, "YM")
format date_corrected %tm

* Remove commas from GDP values and convert to numeric
gen gdp_numeric = real(subinstr(v2, ",", "", .))

* Drop unnecessary variables
drop v1 v2

* Save the cleaned GDP data to Stata format
save "/Users/katherine/Downloads/BVAR/data/gdp_cleaned.dta", replace


* -------------------------------------------
* Step 2: Load and clean CPI data
* -------------------------------------------
import delimited "/Users/katherine/Downloads/BVAR/data/cpi.csv", clear

* Create date variable and format it
gen date_corrected = monthly(date, "YM")
format date_corrected %tm

* Drop the old date variable
drop date

* Save the cleaned CPI data to Stata format
save "/Users/katherine/Downloads/BVAR/data/cpi_cleaned.dta", replace


* -------------------------------------------
* Step 3: Load and clean Labour (Unemployment) data
* -------------------------------------------
import delimited "/Users/katherine/Downloads/BVAR/data/labour.csv", clear

* Create date variable and format it
gen date_corrected = monthly(date, "YM")
format date_corrected %tm

* Drop the old date variable
drop date

* Format unemployment data for better display
format unemployment %6.1f

* Save the cleaned Labour data to Stata format
save "/Users/katherine/Downloads/BVAR/data/labour_cleaned.dta", replace


* -------------------------------------------
* Step 4: Load and clean Interest rate data
* -------------------------------------------
import delimited "/Users/katherine/Downloads/BVAR/data/interest.csv", clear

* Create date variable and format it
gen date_corrected = monthly(date, "YM")
format date_corrected %tm

* Drop the old date variable
drop date

* Format interest rate for better display
format rate %6.1f

* Rename the variable to 'interest_rate'
rename rate interest_rate

* Save the cleaned Interest rate data to Stata format
save "/Users/katherine/Downloads/BVAR/data/interest_cleaned.dta", replace


* -------------------------------------------
* Step 5: Load and clean Gas price data
* -------------------------------------------
import delimited "/Users/katherine/Downloads/BVAR/data/gas.csv", clear

* Create date variable and format it
gen date_corrected = monthly(date, "YM")
format date_corrected %tm

* Drop the old date variable
drop date

* Save the cleaned Gas price data to Stata format
save "/Users/katherine/Downloads/BVAR/data/gas_cleaned.dta", replace


* -------------------------------------------
* Step 6: Load and clean Exchange rate data
* -------------------------------------------
import delimited "/Users/katherine/Downloads/BVAR/data/exchange.csv", clear

* Create date variable and format it
gen date_corrected = monthly(date, "YM")
format date_corrected %tm

* Drop the old date variable
drop date

* Format exchange rate for better display
format rate %6.1f

* Rename the variable to 'exchange_rate'
rename rate exchange_rate

* Save the cleaned Exchange rate data to Stata format
save "/Users/katherine/Downloads/BVAR/data/exchange_cleaned.dta", replace


* -------------------------------------------
* Step 7: Merge all cleaned datasets
* -------------------------------------------

* Load GDP data first
use "/Users/katherine/Downloads/BVAR/data/gdp_cleaned.dta", clear

* Merge with CPI data
merge 1:1 date_corrected using "/Users/katherine/Downloads/BVAR/data/cpi_cleaned.dta"

drop _merge

* Merge with Labour data
merge 1:1 date_corrected using "/Users/katherine/Downloads/BVAR/data/labour_cleaned.dta"

drop _merge

* Merge with Interest rate data
merge 1:1 date_corrected using "/Users/katherine/Downloads/BVAR/data/interest_cleaned.dta"

drop _merge

* Merge with Gas price data
merge 1:1 date_corrected using "/Users/katherine/Downloads/BVAR/data/gas_cleaned.dta"

drop _merge

* Merge with Exchange rate data
merge 1:1 date_corrected using "/Users/katherine/Downloads/BVAR/data/exchange_cleaned.dta"

* -------------------------------------------
* Step 8: Check for merge conflicts and missing data
* -------------------------------------------

* List the first few rows to check the merged data
list date_corrected gdp_numeric cpi unemployment interest_rate price exchange_rate in 1/10

* If there are any missing values, handle them (you might drop or fill them depending on your approach)
* For example, if you want to drop observations with missing values:
drop if missing(gdp_numeric, cpi, unemployment, interest_rate, price, exchange_rate)

* -------------------------------------------
* Step 9: Save the final merged dataset
* -------------------------------------------
save "/Users/katherine/Downloads/BVAR/data/merged_data.dta", replace

* -------------------------------------------
* Step 10: Check for stationarity (Optional, but recommended before running VAR)
* -------------------------------------------
* Set time-series data format
tsset date_corrected

* Check stationarity using Augmented Dickey-Fuller test (ADF)
dfuller cpi, lags(12)
dfuller gdp_numeric, lags(12)
dfuller unemployment, lags(12)
dfuller interest_rate, lags(12)
dfuller price, lags(12)
dfuller exchange_rate, lags(12)

* If any variables are non-stationary, you can difference them or take logarithms
gen d_cpi = D.cpi
gen d_gdp_numeric = D.gdp_numeric
gen d_unemployment = D.unemployment
gen d_interest_rate = D.interest_rate
gen d_price = D.price
gen d_exchange_rate = D.exchange_rate

* Drop any missing values resulting from differencing
drop if missing(d_cpi, d_gdp_numeric, d_unemployment, d_interest_rate, d_price, d_exchange_rate)

* Check for missing values after differencing
sum d_cpi d_gdp_numeric d_unemployment d_interest_rate d_price d_exchange_rate

* Save the final cleaned dataset
save "/Users/katherine/Downloads/BVAR/data/cleaned_and_diff_data.dta", replace

tsset date_corrected
bayes: var d_cpi d_gdp_numeric d_unemployment d_interest_rate d_price d_exchange_rate
