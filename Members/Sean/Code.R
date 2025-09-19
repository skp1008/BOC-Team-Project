library(vars)       
library(ggplot2)
library(plm)
library(readxl)
library(dplyr)
library(tidyverse)
library(tibble)
library(car)
library(sandwich)
library(tseries)
library(reshape2)
library(zoo)
library(writexl)
library(stargazer)

df <- read_excel("Data Quarterly.xlsx")

df <- df %>%
  mutate(
    Quarter = as.numeric(str_remove(str_extract(Date, "Q[1-4]"), "Q")),
    Year = as.numeric(str_extract(Date, "\\d{4}"))
  )

# Drop the Date column
df_data <- df %>%
  select(-Date, -Quarter, -Year)

colnames(df_data) <- c(
  "GDP",
  "CADUSD",
  "Unemployment",
  "Expectation",
  "BOCI",
  "CPI_C",
  "CPI_M",
  "CPI_T",
  "CPI",
  "Output"
)

# Set the start year and quarter
start_year <- df$Year[1]
start_quarter <- df$Quarter[1]

#log the variables
log_GDP <- log(df_data$GDP)
log_CADUSD <- log(df_data$CADUSD)
log_BOCI <- log(df_data$BOCI)

#difference the logged variables
Dlog_GDP <- diff(log_GDP)
Dunemployment <- diff(df_data$Unemployment)
Dlog_CADUSD <- diff(log_CADUSD)
Dexpectation <- diff(df_data$Expectation)
Dlog_BOCI <- diff(log_BOCI)
Doutput <- diff(df_data$Output)

# Combine differenced variables into a data frame
stationary_data <- data.frame(
  Dlog_GDP = Dlog_GDP,
  Dunemployment = Dunemployment,
  Dlog_CADUSD = Dlog_CADUSD,
  Dexpectation = Dexpectation,
  Dlog_BOCI = Dlog_BOCI,
  CPI_C = df_data$CPI_C[-1],
  CPI_M = df_data$CPI_M[-1],
  CPI_T = df_data$CPI_T[-1],
  CPI = df_data$CPI[-1],
  Doutput = Doutput
)

# Create multivariate time series starting at 1995 Q1
data_ts_stationary <- ts(stationary_data, start = c(1990, 1), frequency = 4)

# Extract individual ts series from multivariate ts object
Dlog_GDP_ts <- data_ts_stationary[, "Dlog_GDP"]
Dunemployment_ts <- data_ts_stationary[, "Dunemployment"]
Dlog_BOCI <- data_ts_stationary[, "Dlog_BOCI"]
Dlog_CADUSD_ts <- data_ts_stationary[, "Dlog_CADUSD"]
CPI_C_ts<- data_ts_stationary[, "CPI_C"]
CPI_M_ts <- data_ts_stationary[, "CPI_M"]
CPI_T_ts <- data_ts_stationary[, "CPI_T"]
CPI_ts <- data_ts_stationary[, "CPI"]
Dexpectation_ts <- data_ts_stationary[, "Dexpectation"]
Doutput_ts<- data_ts_stationary[, "Doutput"]

#ADF test for stationarity ----
options(warn = -1)

diff_vars <- list(
  Dlog_GDP = Dlog_GDP,
  Dlog_CADUSD = Dlog_CADUSD,
  Dunemployment = Dunemployment,
  Dexpectation = Dexpectation,
  Dlog_BOCI = Dlog_BOCI,
  CPI_C = df_data$CPI_C[-1],
  CPI_M = df_data$CPI_M[-1],
  CPI_T = df_data$CPI_T[-1],
  CPI = df_data$CPI[-1],
  Doutput = Doutput
)

# Run ADF test on each and print results
for (var_name in names(diff_vars)) {
  series <- na.omit(diff_vars[[var_name]])  # remove NAs
  pval <- adf.test(series)$p.value
  status <- ifelse(pval < 0.05, "Stationary", "Non-stationary")
  cat(var_name, ":", status, "\n")
}

options(warn = 0)

#startdate

# Pick the row that corresponds to 1996 Q1
start_row <- 24  # adjust based on your data

# Slice all series before combining
Dlog_GDP_ts <- Dlog_GDP_ts[start_row:length(Dlog_GDP_ts)]
CPI_M_ts   <- CPI_M_ts[start_row:length(CPI_M_ts)]
CPI_T_ts   <- CPI_T_ts[start_row:length(CPI_T_ts)]
CPI_C_ts   <- CPI_C_ts[start_row:length(CPI_C_ts)]
CPI_ts     <- CPI_ts[start_row:length(CPI_ts)]
Dexpectation_ts <- Dexpectation_ts[start_row:length(Dexpectation_ts)]
Dunemployment_ts <- Dunemployment_ts[start_row:length(Dunemployment_ts)]
Dlog_CADUSD_ts  <- Dlog_CADUSD_ts[start_row:length(Dlog_CADUSD_ts)]
Dlog_BOCI <- Dlog_BOCI[start_row:length(Dlog_BOCI)]
Doutput_ts <- Doutput_ts[start_row:length(Doutput_ts)]

Dlog_GDP_ts <- ts(Dlog_GDP_ts, start = c(1996, 1), frequency = 4)
CPI_M_ts   <- ts(CPI_M_ts, start = c(1996, 1), frequency = 4)
CPI_T_ts   <- ts(CPI_T_ts, start = c(1996, 1), frequency = 4)
CPI_C_ts   <- ts(CPI_C_ts, start = c(1996, 1), frequency = 4)
CPI_ts     <- ts(CPI_ts, start = c(1996, 1), frequency = 4)
Dexpectation_ts <- ts(Dexpectation_ts, start = c(1996, 1), frequency = 4)
Dunemployment_ts <- ts(Dunemployment_ts, start = c(1996, 1), frequency = 4)
Dlog_CADUSD_ts  <- ts(Dlog_CADUSD_ts, start = c(1996, 1), frequency = 4)
Dlog_BOCI <- ts(Dlog_BOCI, start = c(1996, 1), frequency = 4)
Doutput_ts <- ts(Doutput_ts, start = c(1996, 1), frequency = 4)


# Model 1: CPI Median ----
VAR_data <- ts.union(
  Dlog_GDP_ts, CPI_M_ts, Doutput_ts, Dunemployment_ts, Dlog_CADUSD_ts, Dlog_BOCI, Dexpectation_ts)

#Lag Selection
lag_selection <- VARselect(VAR_data, lag.max = 4, type = "const")
cat("The optimal lag for model 1 is: ", lag_selection$selection, "\n")

VAR_est <- VAR(y = VAR_data, p = 2)
summary(VAR_est)

#Stability Check
roots_values <- roots(VAR_est)
stable <- all(Mod(roots_values) < 1)
cat("Is the VAR model 1 stable? ", stable, "\n")

forecast <- predict(VAR_est, n.ahead = 8)

#plot(forecast)
fc_inflation_median <- forecast$fcst$CPI_M_ts[, "fcst"]
print(fc_inflation_median)

irf_expectation_m <- vars::irf(VAR_est,
                               impulse = "Dexpectation_ts",
                               response = "CPI_M_ts",
                               n.ahead = 8,
                               boot = TRUE,
                               ci = 0.95)

# Downside Risk: Shock to Unemployment
irf_unemployment_m <- vars::irf(VAR_est,
                                impulse = "Dunemployment_ts",
                                response = "CPI_M_ts",
                                n.ahead = 8,
                                boot = TRUE,
                                ci = 0.95)

# --- 2. Extract IRF responses ---
# Skip t = 0 for forecast horizon
irf_up_m <- irf_expectation_m$irf$Dexpectation_ts[-1, "CPI_M_ts"]
irf_down_m <- irf_unemployment_m$irf$Dunemployment_ts[-1, "CPI_M_ts"]

# --- 3. Combine with baseline forecast ---
df_export_median <- data.frame(
  Quarter = 1:length(fc_inflation_median),
  Baseline = fc_inflation_median,
  Upside_Risk = fc_inflation_median + irf_up_m,
  Downside_Risk = fc_inflation_median + irf_down_m
)

# --- 4. Add calendar quarter labels ---
start_date <- as.yearqtr("2025 Q2", format="%Y Q%q")
df_export_median$Quarter_Date <- seq(start_date, by = 0.25, length.out = nrow(df_export_median))

# Reorder columns
df_export_median <- df_export_median[, c("Quarter", "Quarter_Date", "Baseline", "Upside_Risk", "Downside_Risk")]

# --- 5. Export to Excel ---
write_xlsx(df_export_median, path = "CPI_M_Forecast_Scenarios.xlsx")




























# Model 2: CPI Trim ----

VAR_data2 <- ts.union(
  Dlog_GDP_ts, CPI_T_ts, Doutput_ts, Dunemployment_ts, Dlog_CADUSD_ts, Dlog_BOCI, Dexpectation_ts)

#Lag Selection
lag_selection2 <- VARselect(VAR_data2, lag.max = 4, type = "const")
cat("The optimal lag for model 2 is: ", lag_selection2$selection, "\n")

VAR_est2 <- VAR(y = VAR_data2, lag_selection2$selection["AIC(n)"], type= "const")
summary(VAR_est2)

#Stability Check
roots_values2 <- roots(VAR_est2)
stable2 <- all(Mod(roots_values2) < 1)
cat("Is the VAR model 2 stable? ", stable2, "\n")

forecast2 <- predict(VAR_est2, n.ahead = 8)

#plot(forecast2)
fc_inflation_trim <- forecast2$fcst$CPI_T_ts[, "fcst"]
print(fc_inflation_trim)

# Use the irf() from the vars package
irf_expectation <- vars::irf(VAR_est2,
                             impulse = "Dexpectation_ts",
                             response = "CPI_T_ts",
                             n.ahead = 8,
                             boot = TRUE,
                             ci = 0.95)

irf_unemployment <- vars::irf(VAR_est2,
                              impulse = "Dunemployment_ts",
                              response = "CPI_T_ts",
                              n.ahead = 8,
                              boot = TRUE,
                              ci = 0.95)

irf_up <- irf_expectation$irf$Dexpectation_ts[, "CPI_T_ts"]

irf_up <- irf_expectation$irf$Dexpectation_ts[-1, "CPI_T_ts"]
irf_down <- irf_unemployment$irf$Dunemployment_ts[-1, "CPI_T_ts"]

df_export <- data.frame(
  Quarter = 1:length(fc_inflation_trim),
  Baseline = fc_inflation_trim,
  Upside_Risk = fc_inflation_trim + irf_up,
  Downside_Risk = fc_inflation_trim + irf_down
)

# Optionally, add calendar quarter labels
start_date <- as.yearqtr("2025 Q2", format="%Y Q%q")
df_export$Quarter_Date <- seq(start_date, by = 0.25, length.out = nrow(df_export))

# Reorder columns
df_export <- df_export[, c("Quarter", "Quarter_Date", "Baseline", "Upside_Risk", "Downside_Risk")]

# Export to Excel
write_xlsx(df_export, path = "CPI_T_Forecast_Scenarios.xlsx")




























# Model 3: CPI Common ----

VAR_data3 <- ts.union(
  Dlog_GDP_ts, CPI_C_ts, Doutput_ts, Dunemployment_ts, Dlog_CADUSD_ts, Dlog_BOCI, Dexpectation_ts)

#Lag Selection
lag_selection3 <- VARselect(VAR_data3, lag.max = 4, type = "const")
cat("The optimal lag for model 3 is: ", lag_selection3$selection, "\n")

VAR_est3 <- VAR(y = VAR_data3, p = 3)
summary(VAR_est3)

#Stability Check
roots_values3 <- roots(VAR_est3)
stable3 <- all(Mod(roots_values3) < 1)
cat("Is the VAR model 3 stable? ", stable3, "\n")

forecast3 <- predict(VAR_est3, n.ahead = 8)

#plot(forecast3)
fc_inflation_common <- forecast3$fcst$CPI_C_ts[, "fcst"]
print(fc_inflation_common)







# Upside Risk: Shock to Expectations
irf_expectation_c <- vars::irf(VAR_est3,
                               impulse = "Dexpectation_ts",
                               response = "CPI_C_ts",
                               n.ahead = 8,
                               boot = TRUE,
                               ci = 0.95)

# Downside Risk: Shock to Unemployment
irf_unemployment_c <- vars::irf(VAR_est3,
                                impulse = "Dunemployment_ts",
                                response = "CPI_C_ts",
                                n.ahead = 8,
                                boot = TRUE,
                                ci = 0.95)

# --- 2. Extract IRF responses (skip t=0) ---
irf_up_c <- irf_expectation_c$irf$Dexpectation_ts[-1, "CPI_C_ts"]
irf_down_c <- irf_unemployment_c$irf$Dunemployment_ts[-1, "CPI_C_ts"]

# --- 3. Combine with baseline forecast ---
df_export_common <- data.frame(
  Quarter = 1:length(fc_inflation_common),
  Baseline = fc_inflation_common,
  Upside_Risk = fc_inflation_common + irf_up_c,
  Downside_Risk = fc_inflation_common + irf_down_c
)

# --- 4. Add calendar quarter labels ---
start_date <- as.yearqtr("2025 Q2", format="%Y Q%q")
df_export_common$Quarter_Date <- seq(start_date, by = 0.25, length.out = nrow(df_export_common))

# Reorder columns
df_export_common <- df_export_common[, c("Quarter", "Quarter_Date", "Baseline", "Upside_Risk", "Downside_Risk")]

# --- 5. Export to Excel ---
write_xlsx(df_export_common, path = "CPI_C_Forecast_Scenarios.xlsx")


















# Model 4: CPI  ----

VAR_data4 <- ts.union(
  Dlog_GDP_ts, CPI_ts, Doutput_ts, Dunemployment_ts, Dlog_CADUSD_ts, Dlog_BOCI, Dexpectation_ts)

#Lag Selection
lag_selection4 <- VARselect(VAR_data4, lag.max = 4, type = "const")
cat("The optimal lag for model 4 is: ", lag_selection4$selection, "\n")

VAR_est4 <- VAR(y = VAR_data4, p = 1)
summary(VAR_est4)

#Stability Check
roots_values4 <- roots(VAR_est4)
stable4 <- all(Mod(roots_values4) < 1)
cat("Is the VAR model 4 stable? ", stable4, "\n")

forecast4 <- predict(VAR_est4, n.ahead = 8)

#plot(forecast4)
fc_inflation <- forecast4$fcst$CPI_ts[, "fcst"]
print(fc_inflation)

# Upside Risk: Shock to Expectations
irf_expectation_total <- vars::irf(VAR_est4,
                                   impulse = "Dexpectation_ts",
                                   response = "CPI_ts",
                                   n.ahead = 8,
                                   boot = TRUE,
                                   ci = 0.95)

# Downside Risk: Shock to Unemployment
irf_unemployment_total <- vars::irf(VAR_est4,
                                    impulse = "Dunemployment_ts",
                                    response = "CPI_ts",
                                    n.ahead = 8,
                                    boot = TRUE,
                                    ci = 0.95)

# --- 2. Extract IRF responses (skip t=0) ---
irf_up_total <- irf_expectation_total$irf$Dexpectation_ts[-1, "CPI_ts"]
irf_down_total <- irf_unemployment_total$irf$Dunemployment_ts[-1, "CPI_ts"]

# --- 3. Combine with baseline forecast ---
df_export_total <- data.frame(
  Quarter = 1:length(fc_inflation),
  Baseline = fc_inflation,
  Upside_Risk = fc_inflation + irf_up_total,
  Downside_Risk = fc_inflation + irf_down_total
)

# --- 4. Add calendar quarter labels ---
start_date <- as.yearqtr("2025 Q2", format="%Y Q%q")
df_export_total$Quarter_Date <- seq(start_date, by = 0.25, length.out = nrow(df_export_total))

# Reorder columns
df_export_total <- df_export_total[, c("Quarter", "Quarter_Date", "Baseline", "Upside_Risk", "Downside_Risk")]

# --- 5. Export to Excel ---
write_xlsx(df_export_total, path = "CPI_Forecast_Scenarios.xlsx")

















horizon <- 8
start_date <- tail(time(VAR_data), 1) + 1/12
dates <- seq(start_date, by = 1/12, length.out = horizon)

# Combine forecasts into one data frame
cpi_forecasts <- data.frame(
  date = dates,
  CPI_Median = fc_inflation_median,
  CPI_Trim = fc_inflation_trim,
  CPI_Common = fc_inflation_common,
  CPI_Headline = fc_inflation
)
write.csv(cpi_forecasts, "forecast_all_CPI.csv", row.names = FALSE)


#STATS TABLE


vars <- c(
  "Dlog_GDP",
  "Dlog_CADUSD",
  "Dunemployment",
  "Dexpectation", 
  "Dlog_BOCI", 
  "CPI_C", 
  "CPI_M", 
  "CPI_T", 
  "CPI", 
  "Doutput"
)

# Cut dataset starting from row 24
data_ts_stationary_cut <- data_ts_stationary[24:nrow(data_ts_stationary), ]

# Subset only selected variables
stats_table <- as.data.frame(data_ts_stationary_cut[ , vars])

colnames(stats_table) <- c(
  "\\Delta \\log(GDP)",
  "\\Delta \\log(CAD/USD)",
  "\\Delta \\text{Unemployment}",
  "\\Delta \\text{Expectation}",
  "\\Delta \\log(BOCI)",
  "CPI-Common",
  "CPI-Median",
  "CPI-Trim",
  "CPI (Headline)",
  "\\Delta \\text{Output}"
)

# Stargazer summary
#stargazer(stats_table,
#          type = "text",    # "latex" if you want real LaTeX output
#          digits = 2,
#          summary = TRUE,   # <-- add this!
#          summary.stat = c("n", "mean", "sd", "min", "median", "max"),
#          title = "Statistical Description of Variables",
#          out = "summary_stats.tex")

