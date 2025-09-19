### 

### install packages
#install.packages("fredr")
#install.packages("readxl")
#install.packages("janitor")
#install.packages("seasonal")
#install.packages("ggthemes")
#install.packages("scales")
#install.packages("tseries")
#install.packages("forecast")
install.packages("vars")
### system instructions 
#rm(list = ls())
options(scipen = 999)
fredr_set_key("1b5443fac8fc06aed29a8c3725031fc5") # FRED API key

### loading packages 
library(tidyverse)
library(ggplot2)
library(cansim)
library(readr)
library(gtools)
library(zoo)
library(fredr)
library(readxl)
library(janitor)
library(lubridate)
library(stats)
library(seasonal)
library(ggthemes)
library(scales)
library(tseries)
library(forecast)
library(tempdisagg)
library(vars)
# library(quantmod)
# library(dynlm)
# library(lmtest)
# library(sandwich)
# library(vars)

### loading data

# seasonally adjusted inflation data
raw_inflation <- get_cansim("18-10-0006-01")

relevant_columns <- c("REF_DATE", "Date", "VALUE")

inflation <- subset(raw_inflation, VECTOR == "v41690914")%>% 
  select(all_of(relevant_columns)) %>% 
  rename("inflation" = VALUE)

inflation_clean <- inflation %>% 
  mutate(Date = as.Date(as.numeric(Date))) %>% 
  arrange(Date) %>%  
  mutate(YoY_Inflation = (inflation - lag(inflation, 12)) / lag(inflation, 12) * 100) %>% 
  select("Date", "YoY_Inflation") %>% 
  na.omit() 

inflation_plot <- inflation_clean %>% 
  filter(Date >= as.Date("2014-01-01"))

cutoff_start <- max(inflation_plot$Date) - months(23)
cutoff_end   <- max(inflation_plot$Date)

ggplot(inflation_plot, aes(x = Date, y = YoY_Inflation)) +
  
  annotate("rect", xmin = cutoff_start, xmax = cutoff_end,
           ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5) +
  
  geom_hline(yintercept = 1, linetype = "dashed",
             color = "grey50", linewidth = 0.3) +
  geom_hline(yintercept = 3, linetype = "dashed",
             color = "grey50", linewidth = 0.3) +
  
  geom_line(color = "#0a1172", linewidth = 0.4) +
  labs(title = "Annual Inflation in Recent Years",
       x = "Year", y = "YoY % Change CPI") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y", expand = c(0.01,0)) + 
  theme_minimal() + theme(
    plot.title = element_text(size = 12,face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),    
    axis.text = element_text(size = 8), 
    axis.line = element_line(color = "black", linewidth = 0.4)
  )

adf.test(inflation_clean$YoY_Inflation)
 

# output slack 
raw_supply_slack <- get_cansim("16-10-0047-01")
supply_slack <- subset(raw_supply_slack, VECTOR == "v803313") %>% 
  select(all_of(c("Date", "VALUE"))) %>% 
  rename("supply_slack" = VALUE) %>% 
  mutate(Date = as.Date(as.numeric(Date)))

supply_plot <- supply_slack %>% 
  filter(Date >= as.Date("2014-01-01")) 

cutoff_start <- max(inflation_plot$Date) - months(8)
cutoff_end   <- max(inflation_plot$Date)

ggplot(supply_plot, aes(x = Date, y = supply_slack)) +
  
  annotate("rect", xmin = cutoff_start, xmax = cutoff_end,
           ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5) +
  
  geom_line(color = "#0a1172", linewidth = 0.4) +
  labs(title = "Total Inventory to Sales Ratio in Recent Years",
       x = "Year", y = "Total Inventory to Sales Ratio") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y", expand = c(0.01,0)) + 
  theme_minimal() + theme(
    plot.title = element_text(size = 12,face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),    
    axis.text = element_text(size = 8), 
    axis.line = element_line(color = "black", linewidth = 0.4)
  )

adf.test(supply_slack$supply_slack)

supply_slack_final <- supply_slack %>% 
  mutate(diff_supply_slack = supply_slack - lag(supply_slack))

# domestic economic growth
raw_growth <- get_cansim("36-10-0434-01")
growth <- subset(raw_growth, VECTOR == "v65201210") %>% 
  select(all_of(c("Date", "VALUE"))) %>% 
  rename("real_GDP" = VALUE) %>% 
  mutate(Date = as.Date(as.numeric(Date))) %>% 
  arrange(Date) %>%  
  mutate(GDP_growth = (real_GDP - lag(real_GDP, 12)) / lag(real_GDP, 12) * 100) %>% 
  na.omit()

growth_plot <- growth %>% 
  filter(Date >= as.Date("2015-01-01")) 

cutoff_start <- max(inflation_plot$Date) - months(8)
cutoff_end   <- max(inflation_plot$Date)

ggplot(growth_plot, aes(x = Date, y = GDP_growth)) +
  
  annotate("rect", xmin = cutoff_start, xmax = cutoff_end,
           ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5) +
  
  geom_line(color = "#0a1172", linewidth = 0.4) +
  labs(title = "Real GDP Growth in Recent Years",
       x = "Year", y = "Real GDP Growth (%)") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y", expand = c(0.01,0)) + 
  theme_minimal() + theme(
    plot.title = element_text(size = 12,face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),    
    axis.text = element_text(size = 8), 
    axis.line = element_line(color = "black", linewidth = 0.4)
  )

adf.test(growth$GDP_growth)

# labour market indicator
relevant_columns_FRED <- c("date", "value")

raw_unemployment <- read_excel("/Users/sahaj/Desktop/bank_of_canada/final_model/data/1410028701-eng.xlsx"
                             , sheet = "data")
unemployment <- raw_unemployment %>%  
  select(-"Gender 6 7", -"Age group", -"Statistics", -"Data type", -"Vector", -"Coordinate") %>% 
  pivot_longer(cols = -"Labour force characteristics", names_to = "date") %>% 
  pivot_wider(names_from = "Labour force characteristics", values_from ="value") %>% 
  mutate(date = as.Date(paste0(date, "-01"), format = "%Y-%m-%d"))

raw_R8 <- get_cansim('14-10-0077-01') %>% 
  subset(VECTOR == "v2440393")
R8 <- raw_R8 %>% 
  select("Date", "VALUE") %>% 
  rename("R8" = VALUE, "date" = Date) %>% 
  mutate(date = as.Date(as.numeric(date)))
  
raw_duration <- get_cansim("14-10-0342-01") %>% 
  subset(VECTOR %in% c("v1078667526", "v1078667742", "v1078668066"))
duration <- raw_duration %>% 
  select("Date", "VALUE", "Duration of unemployment") %>% 
  pivot_wider(names_from = "Duration of unemployment", values_from = "VALUE") %>% 
  rename("date" = Date) %>% 
  mutate(date = as.Date(as.numeric(date)))
  
raw_prime <- read_excel("/Users/sahaj/Desktop/bank_of_canada/final_model/data/1410028701-eng (1).xlsx", 
                        sheet = "Sheet1")
prime <- raw_prime %>% 
  select( -"Data type") %>% 
  pivot_longer(cols = -"Labour force characteristics", names_to = "date") %>% 
  pivot_wider(names_from = "Labour force characteristics", values_from ="value") %>% 
  select("date", "participation_rate") %>% 
  mutate(date = as.Date(paste0(date, "-01"), format = "%Y-%m-%d"))


raw_hours <- get_cansim("14-10-0032-01")
hours <- subset(raw_hours, VECTOR == "v2685138") %>% 
  select("Date", "VALUE") %>% 
  rename("average_hours_worked" = VALUE, "date" = Date) %>% 
  mutate(date = as.Date(as.numeric(date)))


raw_wages <- read_excel("/Users/sahaj/Desktop/bank_of_canada/final_model/data/1410006301-eng.xlsx", sheet = "data") %>% 
 subset(Vector == "v2132579") %>% 
 select(-"Gender 6 7", -"Age group", -"Vector", -"Coordinate", -"Type of work", - "North American Industry Classification System (NAICS) 5")

wages <- raw_wages %>% 
  pivot_longer(cols = -"Wages 4", names_to = "date") %>% 
  rename("average_hourly_wage" = value) %>% 
  select(-"Wages 4") %>% 
  mutate(date = as.Date(paste0(date, "-01"), format = "%Y-%m-%d"))

### cleaing labour market data

labour_dfs <- list(unemployment, R8, duration, prime, hours, wages)
labour_final <- Reduce(function(x, y) full_join(x, y, by = "date"), labour_dfs) %>% 
  clean_names()

# calculate job finding rate and separation rate

labour_final <- labour_final %>% 
  mutate(job_finding_rate = (1 - (unemployment - x1_to_4_weeks)/stats::lag(unemployment))*100,
         separation_rate = x1_to_4_weeks / stats::lag(employment) * 100) %>% 
  na.omit() %>% 
   rename("participation_rate_prime" = participation_rate_y)

# calculate long-term unemployment rate
labour_final <- labour_final %>% 
  mutate(long_term_unemployment_rate = x27_weeks_or_more / unemployment * 100)

# calculate wage growth 
labour_final <- labour_final %>% 
  arrange(date) %>%  
  mutate(wage_growth = (average_hourly_wage - lag(average_hourly_wage, 12)) / lag(average_hourly_wage, 12) * 100)

PCA_data <- labour_final %>% 
  select("date", "unemployment_rate", "r8", "long_term_unemployment_rate", "job_finding_rate",
         "separation_rate", "participation_rate_prime", "average_hours_worked", "wage_growth" ) 

# fixing seasonality
PCA_data$r8 <- PCA_data$r8_SA <- final(
  seas(ts(PCA_data$r8,
      start = c(year(min(PCA_data$date, na.rm = TRUE)),
                month(min(PCA_data$date, na.rm = TRUE))),
      frequency = 12)))

PCA_data$hours_SA <-
  final(seas(ts(PCA_data$average_hours_worked,
                start = c(year(min(PCA_data$date)), month(min(PCA_data$date))),
                frequency = 12), transform.function = "auto"))

### conducting PCA analysis 
plot(PCA_data$date, PCA_data$hours_SA, type = "l")

LMI_cols <- c("unemployment_rate", "r8_SA", "long_term_unemployment_rate", "job_finding_rate",
              "separation_rate", "participation_rate_prime", "hours_SA", "wage_growth")

LMI_data <- PCA_data %>% 
  arrange(date) %>% 
  select(date, all_of(LMI_cols)) %>% 
  drop_na()

flipped_variables <- c("unemployment_rate", 'r8_SA', "long_term_unemployment_rate", 
          "separation_rate")
LMI_data[flipped_variables] <- lapply(LMI_data[flipped_variables], function(z) -as.numeric(z))

LMI <- scale(LMI_data[LMI_cols])
fit <- prcomp(LMI, center = FALSE, scale. = FALSE)
pc1 <- fit$x[, 1]
load1 <- fit$rotation[, 1]

if (sum(load1, na.rm = TRUE) < 0) {
  pc1   <- -pc1
  load1 <- -load1}
LMI_z   <- as.numeric(scale(pc1))

out <- tibble(date = LMI_data$date, LMI = LMI_z)

LMI_output <- out %>% 
  mutate(Date = as.Date(as.numeric(date))) %>% 
  select("Date", "LMI")

LMI_plot <- LMI_output %>% 
  filter(Date >= as.Date("2015-05-01"))
  

cutoff_start <- max(inflation_plot$Date) - months(30)
cutoff_end   <- max(inflation_plot$Date)

ggplot(LMI_plot, aes(x = Date, y = LMI)) +
  
  annotate("rect", xmin = cutoff_start, xmax = cutoff_end,
           ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5) +
  
  geom_line(color = "#0a1172", linewidth = 0.4) +
  labs(title = "Labour Market Health Index in Recent Years",
       x = "Year", y = "Labout Market Health Index") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y", expand = c(0.01,0)) + 
  theme_minimal() + theme(
    plot.title = element_text(size = 12,face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),    
    axis.text = element_text(size = 8), 
    axis.line = element_line(color = "black", linewidth = 0.4)
  )

adf.test(LMI_output$LMI)



# trade policy uncertainty # trade policy uncertainty 
raw_policy_uncertainty <- fredr(series = "CANEPUINDXM")
policy_uncertainty <- raw_policy_uncertainty %>% 
  rename("policy_uncertainty" = value, "Date" = date) %>% 
  dplyr::select(all_of(c("Date", "policy_uncertainty")))

policy_plot <- policy_uncertainty %>% 
  filter(Date >= as.Date("2010-05-01"))


cutoff_start <- max(inflation_plot$Date) - months(12)
cutoff_end   <- max(inflation_plot$Date)

ggplot(policy_plot, aes(x = Date, y = policy_uncertainty)) +
  
  annotate("rect", xmin = cutoff_start, xmax = cutoff_end,
           ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5) +
  
  geom_line(color = "#0a1172", linewidth = 0.4) +
  labs(title = "Economic Policy Uncertainty Index in Recent Years",
       x = "Year", y = "Economic Policy Uncertainty Index") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y", expand = c(0.01,0)) + 
  theme_minimal() + theme(
    plot.title = element_text(size = 12,face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),    
    axis.text = element_text(size = 8), 
    axis.line = element_line(color = "black", linewidth = 0.4)
  )

# testing stationarity 

adf.test(policy_uncertainty$policy_uncertainty) 

# auto arima
ts_epu <- policy_uncertainty %>% 
  rename("date" = Date) %>% 
  filter(date <= as.Date("2025-07-01"))
ts_epu <- ts(ts_epu$policy_uncertainty, start = c(1985, 1), frequency = 12)


epu_auto <- auto.arima(ts_epu)
epu_forecast_length <- 12
k <- 3
k <- min(k, epu_forecast_length)
scale <- 2.5

innov <- c(
  abs(rnorm(k, sd = scale * sigma_hat)),   # positive-only (half-normal)
  sample(residuals(epu_auto), epu_forecast_length - k, replace = TRUE)            # back to normal
)

epu_shock_vals <- simulate(epu_auto, nsim = epu_forecast_length, innov = innov)
plot(epu_shock_vals)

epu_shock_ts <- ts(
  epu_shock_vals,
  start     = end(ts_epu) + c(0, 1),
  frequency = frequency(ts_epu)
)

epu_shock_ts

EPU_shock_full <- ts(
  c(as.numeric(ts_epu), as.numeric(epu_shock_ts)),
  start     = start(ts_epu),
  frequency = 12
)

plot(EPU_shock_full)

# baseline forecast 
baseline_epu <- forecast(epu_auto, h = 12) ### 12 months
base_epu_fc <- as.numeric(baseline_epu$mean)

plot(baseline_epu)

baseline_epu$mean

tail(baseline_epu)


# inflation expectations
raw_expecations <- read_excel("/Users/sahaj/Desktop/bank_of_canada/final_model/data/Business_Outlook_Survey-ed-2025-04-01.xlsx", 
                              sheet = "data", col_names = TRUE)
relevant_cols_expectations <- c("date", "BELOW1", "ONETWO", "TWOTHREE", "ABOVE3", "INFLNA")
expectations <- raw_expecations %>% 
  select(all_of(relevant_cols_expectations))


# calculate weighted inflation expectations 

bin_midpoints <- c(0.5, 1.5, 2.5, 3.5)  # midpoint for each bin

expectations_weighted <- expectations %>% 
  select(-INFLNA) %>% 
  mutate(BELOW1   = BELOW1 / 100,
         ONETWO   = ONETWO / 100, 
         TWOTHREE = TWOTHREE / 100, 
         ABOVE3   =  ABOVE3 / 100, 
         weighted_expectation = (BELOW1*bin_midpoints[1] +
           ONETWO*bin_midpoints[2] +
           TWOTHREE*bin_midpoints[3] +
           ABOVE3*bin_midpoints[4]))

ew <- expectations_weighted %>% 
  select("date", "weighted_expectation")\


BEIR <- read_excel("/Users/sahaj/Desktop/bank_of_canada/final_model/data/STATIC_ATABLE_V122544_V122553-BEIR.xlsx") 

q_z  <- zoo(ew$weighted_expectation, order.by = as.yearqtr(ew$date))
q_z  <- na.trim(q_z, sides = "both")     # drop leading/trailing NAs
q_exp <- as.ts(q_z) 
plot(q_exp)

m_z    <- zoo(BEIR$BEIR, order.by = as.yearmon(BEIR$date))
m_z    <- na.trim(m_z, sides = "both")
m_beir <- as.ts(m_z)  

start_date <- c(2001, 1)   # year, subperiod (quarter=1–4, month=1–12)
end_date   <- c(2025, 7)   # adjust based on your data

q_exp_aligned  <- window(q_exp,  start = start_date, end = end_date)
m_beir_aligned <- window(m_beir, start = start_date, end = end_date)

tail(m_beir_aligned)

plot(m_beir)

m_beir

fit_cl <- td(q_exp ~ m_beir, to = 12L,
             method = "chow-lin-maxlog", conversion = "average")
m_exp  <- predict(fit_cl)

plot(m_exp)
plot(q_exp)

firm_expectations <- expectations_weighted %>% 
  mutate(Date = yq(sub("Q", "-Q", expectations_weighted$date))) %>% 
  select(-"date") %>%  
  filter(Date >= as.Date("2015-01-01")) %>%  
  mutate(ABOVE3 = ABOVE3 * 100)

# plotting data
cutoff_start <- max(firm_expectations$Date) - months(20)
cutoff_end   <- max(firm_expectations$Date)

fe <- firm_expectations

Lmin <- min(fe$weighted_expectation, na.rm = TRUE)
Lmax <- max(fe$weighted_expectation, na.rm = TRUE)
Rmin <- min(fe$ABOVE3,              na.rm = TRUE)
Rmax <- max(fe$ABOVE3,              na.rm = TRUE)
if (Rmax == Rmin) Rmax <- Rmin + 1e-9  # avoid divide-by-zero

fe <- fe %>%
  mutate(ABOVE3_on_left = Lmin + (ABOVE3 - Rmin) * (Lmax - Lmin) / (Rmax - Rmin))

# inverse transform for the secondary axis labels
inv_to_right <- function(y_left) Rmin + (y_left - Lmin) * (Rmax - Rmin) / (Lmax - Lmin)

# ---- Plot: left = weighted_expectation (solid), right = ABOVE3 (solid) ----
ggplot(fe, aes(x = Date)) +
  
  annotate("rect", xmin = cutoff_start, xmax = cutoff_end,
           ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5) +
  
  geom_line(aes(y = weighted_expectation, colour = "Weighted Expected Inflation (%)"), linewidth = 0.4) +
  
  geom_line(aes(y = ABOVE3_on_left,      colour = "Share Expecting Inflation >3% (%)"), linewidth = 0.4, linetype = "dashed") +
  
  scale_color_manual(values = c("Weighted Expected Inflation (%)" = "#0a1172",
                                "Share Expecting Inflation >3% (%)" = "#6C6C6C"),
                     breaks = c("Weighted Expected Inflation (%)", "Share Expecting Inflation >3% (%)"), 
                     name = NULL) +
  scale_y_continuous(
    name = "Weighted Expected Inflation (%)",
    sec.axis = sec_axis(~ inv_to_right(.), name = "Share Expecting Inflation >3% (%)")
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0.01)) +
  labs(title = "Firm Inflation Expectations", x = "Year") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12,face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),    
    axis.text = element_text(size = 8), 
    axis.line = element_line(color = "black", linewidth = 0.4)
  ) + 
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")) + 
    guides(color = guide_legend(nrow = 1))
  

# consumer expectations
raw_consumer_expecations <- read_excel("/Users/sahaj/Desktop/bank_of_canada/final_model/data/CES_DEMOGRAPHICS.xlsx"
                                       , sheet = "data")
consumer_expecations <- raw_consumer_expecations %>% 
  select(all_of(c("date", "CES_C1_MID_TERM"))) %>% 
  rename("consumer_expectations" = CES_C1_MID_TERM)
expectations[ expectations$date > min(consumer_expecations$date),]
final_expectations <- left_join(expectations_weighted, consumer_expecations,
                                by = "date")

final_expectations <- final_expectations %>% 
  mutate(final = coalesce(1/2*(weighted_expectation + consumer_expectations),  
  weighted_expectation))

cor(final_expectations$weighted_expectation, final_expectations$consumer_expectations, use = "pairwise.complete.obs")




### merging data 


### transforming data 
fred_data$date <- as.Date(fred_data$date)
fred_data$date <- format(fred_data$date, "%Y-%m")


#### combining all the data to perform VAR Analysis


ts_supply <- ts(supply_slack$supply_slack, start = c(1992, 1), frequency = 12)

ts_auto <- auto.arima(ts_supply)
baseline_supply <- forecast(ts_auto, h = 1)
baseline_supply$mean








inflation_clean
supply_slack 
growth <- growth %>% 
  select("Date", "GDP_growth")
growth
LMI_output
df_exp <- data.frame(
  Date  = as.Date(as.yearmon(time(m_exp))),  # for monthly
  expectations = as.numeric(m_exp)
)

growth_ts <- ts(growth$GDP_growth, start = c(1998, 1), frequency = 12)
growth_auto <- auto.arima(growth_ts)
baseline_growth <- forecast(growth_auto, h = 1)
baseline_growth$mean

VAR_dfs <- list(inflation_clean, supply_slack, growth, LMI_output, df_exp)
VAR_final <- Reduce(function(x, y) full_join(x, y, by = "Date"), VAR_dfs) %>% 
  clean_names() %>% 
  filter(date >= as.Date("2000-01-01") & date <= as.Date("2025-07-01"))

VAR_final$supply_slack[nrow(VAR_final)] = baseline_supply$mean
VAR_final$gdp_growth[nrow(VAR_final)] = baseline_growth$mean

tail(VAR_final)

VAR_final <- VAR_final %>% 
  na.omit()

policy_uncertainty_new <- policy_uncertainty %>% 
  filter(Date >= as.Date("2000-01-01") & Date <= as.Date("2025-07-01"))

Y <- ts(VAR_final[, c("yo_y_inflation", "supply_slack", "gdp_growth", "lmi", "expectations")], start = c(2000,1), frequency = 12) 
X_exog <- ts(policy_uncertainty_new[, "policy_uncertainty"], start = c(2000, 1), frequency = 12) 

df_q <- VAR_final %>%
  mutate(qtr = as.yearqtr(date)) %>%           # e.g., 2001 Q1, Q2, ...
  group_by(qtr) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(date = as.Date(qtr)) %>%
  dplyr::select(date, everything(), -qtr) %>%
  arrange(date)

Y_new <- ts(df_q[, c("yo_y_inflation", "supply_slack", "gdp_growth", "lmi", "expectations")], start = c(2000,1), frequency = 4) 

X_new <- policy_uncertainty_new %>% 
  mutate( date = as.Date(Date),
    qtr = as.yearqtr(date)) %>%           # e.g., 2001 Q1, Q2, ...
  group_by(qtr) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(date = as.Date(qtr)) %>%
  dplyr::select(date, everything(), -qtr) %>%
  arrange(date)

X_new_ts <- ts(X_new[, "policy_uncertainty"], start = c(2000, 1), frequency = 4) 

lag_sel <- VARselect(Y_new, lag.max = 12, type = "const", exogen = X_new_ts)
lag_sel$selection
p <- lag_sel$selection["AIC(n)"]
p

var_fit <- VAR(Y, p = 4, type = "const", exogen = X_exog)
summary(var_fit)

q_ts <- aggregate(epu_shock_ts, nfrequency = 4, FUN = mean)
q_ts

D_future <- matrix(q_ts, ncol = 1, byrow = FALSE)
colnames(D_future) <- "policy_uncertainty"

hh <- 4
n <- nrow(Y_new)
train_Y <- Y_new[1:(n-hh), ]
train_X <- X_new_ts[1:(n-hh), , drop = FALSE]
test_Y  <- Y_new[(n-hh+1):n, ]
test_X  <- X_new_ts[(n-hh+1):n, , drop = FALSE]  # colnames must match!

var_train <- VAR(train_Y, p = 4, type = "const", exogen = train_X)
fc_test   <- predict(var_train, n.ahead = hh, dumvar = test_X)

rmse_out_infl <- sqrt(mean((test_Y[, "yo_y_inflation"] - fc_test$fcst$yo_y_inflation[, "fcst"])^2, na.rm = TRUE))
rmse_out_infl

hh <- 4
fc_shock_quart <- predict(var_fit, n.ahead = 4, dumvar = D_future)
fc_shock_quart

plot(fc_shock_quart)

inf_new <- df_q %>% 
  filter(date >= as.Date("2000-01-01"))
inflation_ts <- ts(inf_new[, "yo_y_inflation"], start = c(2000, 1), freq = 4)
last_time <- time(inflation_ts)[length(time(inflation_ts))]
freq <- frequency(inflation_ts)

future_dates <- as.yearmon(seq(from = last_time + 1/freq,
                               by   = 1/freq,
                               length.out = 4))

fc_infl <- data.frame(
  date  = future_dates,
  inflation  = fc_shock_quart$fcst$yo_y_inflation[, "fcst"])

hist_infl <- data.frame(
  date  = as.yearmon(time(inflation_ts)),
  inflation  = as.numeric(inflation_ts[, "yo_y_inflation"])
)

plot_infl <- bind_rows(hist_infl, fc_infl)

last_hist <- plot_infl %>% filter(Segment == "History") %>% slice_tail(n=1)

bridge <- last_hist %>%
  mutate(date = as.Date("2025-07-01"), Segment = "Forecast")

plot_infl <- bind_rows(plot_infl, bridge) %>% arrange(date)

plot_infl$date <- as.Date(as.yearmon(plot_infl$date))



q_labels <- function(x) format(as.yearqtr(x), "%Y Q%q")


ggplot(plot_infl, aes(x = qnum)) +
  geom_hline(yintercept = 1, linetype = "dashed",
             color = "grey50", linewidth = 0.3) +
  geom_hline(yintercept = 3, linetype = "dashed",
             color = "grey50", linewidth = 0.3) +
  
  geom_line(
    aes(y = inflation, linetype = Segment, color = Segment), linewidth = 0.4
  ) +
  scale_linetype_manual(
    values = c("History" = "solid", "Forecast" = "dashed"),
    name = NULL
  ) +
  scale_color_manual(
    values = c("History" = "#0a1172", "Forecast" = "red"), # navy vs red
    name   = NULL
  ) +
  scale_y_continuous(name = "Inflation (%)") +
  scale_x_continuous(breaks = pretty(plot_infl$qnum, n = 8), labels = q_labels) +
  labs(title = "Medium-term Inflation Forecast under Shock Scenario", x = "Year") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text  = element_text(size = 8),
    axis.line  = element_line(color = "black", linewidth = 0.4),
    legend.position = "bottom"
  ) +
  guides(linetype = guide_legend(nrow = 1))

hh <- 6
n <- nrow(Y)
train_Y <- Y[1:(n-hh), ]
train_X <- X_exog[1:(n-hh), , drop = FALSE]
test_Y  <- Y[(n-hh+1):n, ]
test_X  <- X_exog[(n-hh+1):n, , drop = FALSE]  # colnames must match!

var_train <- VAR(train_Y, p = 6, type = "const", exogen = train_X)
fc_test   <- predict(var_train, n.ahead = hh, dumvar = test_X)

# Example: RMSE for "inflation"
rmse_out_infl <- sqrt(mean((test_Y[, "yo_y_inflation"] - fc_test$fcst$yo_y_inflation[, "fcst"])^2, na.rm = TRUE))

rmse_out_infl



lag_sel <- VARselect(Y, lag.max = 12, type = "const", exogen = X_exog)
lag_sel$selection
p <- lag_sel$selection["AIC(n)"]
p

var_fit <- VAR(Y, p = 3, type = "const", exogen = X_exog)
summary(var_fit)

D_future <- matrix(base_epu_fc, ncol = 1, byrow = FALSE)
colnames(D_future) <- "policy_uncertainty"
hh <- 6
fc_base <- predict(var_fit, n.ahead = 6, dumvar = D_future)
fc_base

plot(fc_base, names = "y1") 

inf_new <- inflation_clean %>% 
  filter(Date >= as.Date("2000-01-01"))
inflation_ts <- ts(inf_new[, "YoY_Inflation"], start = c(2000, 1), freq = 12)
last_time <- time(inflation_ts)[length(time(inflation_ts))]
freq <- frequency(inflation_ts)

future_dates <- as.yearmon(seq(from = last_time + 1/freq,
                               by   = 1/freq,
                               length.out = 6))

fc_infl <- data.frame(
  date  = future_dates,
  inflation  = fc_base$fcst$yo_y_inflation[, "fcst"])

hist_infl <- data.frame(
  date  = as.yearmon(time(inflation_ts)),
  inflation  = as.numeric(inflation_ts[, "YoY_Inflation"])
)

# Combine history + forecast
plot_infl <- bind_rows(hist_infl, fc_infl)

plot_infl <- plot_infl %>%
  arrange(date) %>%
  mutate(Segment = ifelse(row_number() <= n() - 6, "History", "Forecast")) %>% 
  filter(date > as.Date("2015-01-01"))

plot_infl$date <- as.Date(as.yearmon(plot_infl$date))

ggplot(plot_infl, aes(x = date)) +
  geom_hline(yintercept = 1, linetype = "dashed",
             color = "grey50", linewidth = 0.3) +
  geom_hline(yintercept = 3, linetype = "dashed",
             color = "grey50", linewidth = 0.3) +
  
  geom_line(
    aes(y = inflation, linetype = Segment, color = Segment), linewidth = 0.4
  ) +
  scale_linetype_manual(
    values = c("History" = "solid", "Forecast" = "dashed"),
    name = NULL
  ) +
  scale_color_manual(
    values = c("History" = "#0a1172", "Forecast" = "red"), # navy vs red
    name   = NULL
  ) +
  scale_y_continuous(name = "Inflation (%)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0.01)) +
  labs(title = "Short-term Baseline Inflation Forecast", x = "Year") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text  = element_text(size = 8),
    axis.line  = element_line(color = "black", linewidth = 0.4),
    legend.position = "bottom"
  ) +
  guides(linetype = guide_legend(nrow = 1))


