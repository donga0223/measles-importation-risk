
library(tidycensus)
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(sf)
library(ggplot2)
library(tibble)


# TX county population and area size 
tx_county_area <- get_acs(
  geography = "county",
  variables = "B01003_001",
  state = "TX",
  geometry = TRUE,
  year = 2022
) %>%
  rename(population = estimate) %>%
  mutate(area_sqkm = as.numeric(st_area(geometry)) / 10^6,
         area_sqmi = area_sqkm * 0.386102,
         pop_density = population / area_sqmi) %>%
  select(NAME, population, area_sqmi, pop_density) %>%
  mutate(NAME = str_remove(NAME, " County")) %>%  # " County" 제거
  separate(NAME, into = c("county", "state"), sep = ", ", remove = FALSE) %>%
  select(-NAME)

## load data 
df <- read_excel("/Users/dk29776/Dropbox/UTAustin/measles/data/UT_Measles_07may.xlsx")
mmr <- read.csv("/Users/dk29776/Dropbox/UTAustin/measles/data/mmr_data_us_counties.csv")
weekly_flows_oneway <- read.csv("/Users/dk29776/Dropbox/UTAustin/measles/data/weekly_flows_one_way.csv")
weekly_flows_total <- read.csv("/Users/dk29776/Dropbox/UTAustin/measles/data/weekly_flows_total.csv")
dist <- read.csv("/Users/dk29776/Dropbox/UTAustin/measles/data/county_centroids_distances_kilometers.csv")

mennonite <- data.frame(
  county = c("Gaines", "Dawson", "Andrews", "Falls", "Lamar", "Cameron",
             "Nueces", "Bee", "San Patricio", "Victoria", "El Campo", "Wharton",
             "Hunt", "Bexar", "Travis", "Dallas", "Harris"),
  mennonite = c(rep(1,17))
) ## based on Elizabeth's report

age_vars <- c("B01001_001",  # total population
              
              # Male
              "B01001_003", "B01001_004", "B01001_005", "B01001_006", "B01001_007", "B01001_008",
              "B01001_009", "B01001_010", "B01001_011", "B01001_012", "B01001_013", "B01001_014",
              "B01001_015", "B01001_016", "B01001_017", "B01001_018", "B01001_019", "B01001_020",
              "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",
              
              # Female
              "B01001_027", "B01001_028", "B01001_029", "B01001_030", "B01001_031", "B01001_032",
              "B01001_033", "B01001_034", "B01001_035", "B01001_036", "B01001_037", "B01001_038",
              "B01001_039", "B01001_040", "B01001_041", "B01001_042", "B01001_043", "B01001_044",
              "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049")


age_data <- get_acs(
  geography = "county",
  variables = age_vars,
  state = "TX",
  year = 2022,
  survey = "acs5"
)

## grouping by age
age_groups <- tribble(
  ~variable,        ~age_group,
  "B01001_003", "age_0_4",   "B01001_027", "age_0_4",
  "B01001_004", "age_5_9",   "B01001_028", "age_5_9",
  "B01001_005", "age_10_17", "B01001_006", "age_10_17",
  "B01001_007", "age_10_17", "B01001_008", "age_10_17",
  "B01001_009", "age_18_64", "B01001_010", "age_18_64", "B01001_011", "age_18_64",
  "B01001_012", "age_18_64", "B01001_013", "age_18_64", "B01001_014", "age_18_64",
  "B01001_015", "age_18_64", "B01001_016", "age_18_64", "B01001_017", "age_18_64",
  "B01001_018", "age_18_64", "B01001_019", "age_18_64", "B01001_020", "age_65_plus",
  "B01001_021", "age_65_plus", "B01001_022", "age_65_plus", "B01001_023", "age_65_plus",
  "B01001_024", "age_65_plus", "B01001_025", "age_65_plus",
  
  "B01001_029", "age_10_17", "B01001_030", "age_10_17",
  "B01001_031", "age_18_64", "B01001_032", "age_18_64", "B01001_033", "age_18_64",
  "B01001_034", "age_18_64", "B01001_035", "age_18_64", "B01001_036", "age_18_64",
  "B01001_037", "age_18_64", "B01001_038", "age_18_64", "B01001_039", "age_18_64",
  "B01001_040", "age_18_64", "B01001_041", "age_18_64", "B01001_042", "age_18_64",
  "B01001_043", "age_18_64", "B01001_044", "age_65_plus", "B01001_045", "age_65_plus",
  "B01001_046", "age_65_plus", "B01001_047", "age_65_plus", "B01001_048", "age_65_plus",
  "B01001_049", "age_65_plus"
)

# aggregated population size and pct by age group
age_summary <- age_data %>%
  inner_join(age_groups, by = "variable") %>%
  group_by(NAME, age_group) %>%
  summarise(count = sum(estimate), .groups = "drop") %>%
  pivot_wider(names_from = age_group, values_from = count) %>%
  left_join(
    age_data %>% filter(variable == "B01001_001") %>%
      select(NAME, total = estimate),
    by = "NAME"
  ) %>%
  mutate(
    pct_age_0_4     = age_0_4 / total,
    pct_age_5_9     = age_5_9 / total,
    pct_age_10_17   = age_10_17 / total,
    pct_age_18_64 = age_18_64 / total,
    pct_age_65_plus = age_65_plus / total,
    county = str_remove(NAME, " County, Texas")
  ) %>%
  select(county, total, starts_with("age_"), starts_with("pct_"))





library(lubridate)

df_filter <- df %>%
  select(hospitalized = `Hospitalized?`, age = Age, 
         county = `Home County`,
         vaccine_status = `Vaccine Status (The unvaccinated/unknown category includes people with no documented doses of measles vaccine more than 14 days before symptom onset)`,
         date_of_rash_onset = `Date of Rash Onset (If rash onset date not available, the following hierarchy is used for date: symptom onset date, specimen collection date, hospital admission date, or date reported to the region)`) %>%
  mutate(date_of_rash_onset = as.Date(date_of_rash_onset),
         end_date = date_of_rash_onset + (6 - wday(date_of_rash_onset, week_start = 1)))

df_fitting <- df_filter %>%
  group_by(county, end_date) %>%
  summarise(inc = n(),
            age_below1 = sum(age < 1, na.rm = TRUE),
            age_1_3     = sum(age >= 1 & age <= 3, na.rm = TRUE),
            age_4_6     = sum(age >= 4 & age <= 6, na.rm = TRUE),
            age_7_18    = sum(age >= 7 & age <= 18, na.rm = TRUE),
            age_19plus  = sum(age > 18, na.rm = TRUE)) %>%
  arrange(end_date)

df_fitting %>%
  ggplot(aes(x = end_date, y = inc, group = county, color = county)) +
  geom_line()


## mobility
weekly_flows_origin_mat <- weekly_flows_oneway %>% 
  filter(origin_state == "TX", dest_state == "TX") %>%
  select(origin_county, dest_county, visits) %>%
  pivot_wider(
    names_from = dest_county,
    values_from = visits,
    values_fill = 0  # replace NA to 0 
    )

weekly_flows_dest_mat <- weekly_flows_oneway %>% 
  filter(origin_state == "TX", dest_state == "TX") %>%
  select(origin_county, dest_county, visits) %>%
  pivot_wider(
    names_from = origin_county,
    values_from = visits,
    values_fill = 0  # replace NA to 0 
  )


weekly_flows_origin_mat_big5 <- weekly_flows_origin_mat %>% 
  select(origin_county, Gaines, Lubbock, Terry, `El Paso`, Dawson)


weekly_flows_dest_mat_big5 <- weekly_flows_dest_mat %>% 
  select(dest_county, Gaines, Lubbock, Terry, `El Paso`, Dawson)

## dist
tx_counties <- counties(state = "TX", cb = TRUE, class = "sf")

# 중심점 좌표 계산
tx_centroids <- tx_counties %>%
  st_centroid() %>%
  select(NAME, geometry) %>%
  rename(county = NAME)

dist_matrix <- st_distance(tx_centroids)

# add names to matrix
rownames(dist_matrix) <- tx_centroids$county
colnames(dist_matrix) <- tx_centroids$county

dist_matrix_km <- dist_matrix/1000
dist_matrix_km <- as.data.frame(dist_matrix_km)
dist_matrix_km <- dist_matrix_km %>%
  mutate(across(everything(), as.numeric))
dist_df <- dist_matrix_km %>%
  as.data.frame() %>%
  rownames_to_column(var = "origin_county")

dist_df_big5 <- dist_df %>% 
  select(origin_county, Gaines, Lubbock, Terry, `El Paso`, Dawson)



# neighboring county list
neighbors_list <- st_touches(tx_counties)

# mapping neigboring county by name
neighbor_df <- lapply(seq_along(neighbors_list), function(i) {
  tibble(
    county = tx_counties$NAME[i],
    neighbor = tx_counties$NAME[neighbors_list[[i]]]
  )
}) %>% bind_rows() 

neighbor_inc <- neighbor_df %>% 
  left_join(df_fitting %>% 
              group_by(county) %>%
              summarise(neighbor_cum_inc = sum(inc, na.rm = TRUE)), by = c("neighbor" = "county")) %>%
  group_by(county) %>%
  summarise(n_neighbor = n(),
            neighbor_cumcum_inc = sum(neighbor_cum_inc, na.rm = TRUE))


neighbor_inc2 <- df_fitting %>% 
  group_by(county, end_date) %>%
  summarise(neighbor_cum_inc_thatweek = sum(inc, na.rm = TRUE)) %>%
  left_join(neighbor_inc, by = "county")


all_counties <- unique(tx_county_area$county)
all_weeks <- seq.Date(from = as.Date("2025-01-25"), to = as.Date("2025-05-31"), by = "week")

panel_template <- expand.grid(
  county = all_counties,
  week = all_weeks
)

data <- panel_template %>%
  left_join(df_fitting %>%
              select(county, end_date, inc), by = c("county", "week" = "end_date")) %>%
  left_join(mmr %>% 
              filter(State == "TX") %>%
              select(County, SY2022_23, SY2023_24) %>%
              mutate(SY2023_24_num = suppressWarnings(as.numeric(SY2023_24))) %>%
              mutate(vaccine = coalesce(SY2023_24_num, SY2022_23)) %>%
              select(-SY2023_24_num, -SY2022_23, -SY2023_24),
            by = c("county" = "County")) %>%
  left_join(mennonite, by = "county") %>%
  left_join(age_summary, by = "county") %>%
  mutate(mennonite = ifelse(is.na(mennonite), 0 ,1),
         case = ifelse(is.na(inc), 0 ,1),
         inc = ifelse(is.na(inc), 0, inc)) %>%
  left_join(neighbor_inc, by = c("county")) %>%
  #left_join(neighbor_inc2, by = c("county", "week" = "end_date")) %>%
  left_join(weekly_flows_origin_mat_big5, by = c("county" = "origin_county")) %>%
  left_join(weekly_flows_dest_mat_big5, by = c("county" = "dest_county"), suffix = c("_origin", "_dest")) %>%
  left_join(dist_df_big5, by = c("county" = "origin_county")) 
  

library(zoo)
data1 <- data %>%
  arrange(county, week) %>%
  group_by(county) %>%
  mutate(
    inc_lag1 = lag(inc, 1),
    inc_lag2 = lag(inc, 2),
    inc_lag3 = lag(inc, 3),
    inc_rollmean3 = rollmean(inc, k = 3, fill = NA, align = "right")
  ) %>%
  ungroup()


data2 <- data1 %>%
  select(-age_0_4, -age_5_9, -age_10_17, -age_18_64, -age_65_plus)

#data3 <- data2 %>%
#  group_by(county) %>%
#  arrange(week) %>%
#  mutate(
#    ever_had_case = cumsum(inc) > 0,
#    target_first_case = (replace_na(lag(ever_had_case), FALSE) == FALSE & ever_had_case == TRUE) * 1
#    
#  )

library(fastDummies)
data4 <- dummy_cols(data2, select_columns = "county", remove_first_dummy = FALSE, remove_selected_columns = FALSE)




df_train <- data4 %>%
  filter(week <= "2025-04-04") 
df_test <- data4 %>%
  filter(week > "2025-04-04") 
library(xgboost)


X <- df_train %>%
  ungroup() %>% 
  select(-case, -county, -inc) %>% #, -neighbor_cum_inc_thatweek) %>%
  mutate(across(everything(), as.numeric)) %>%
  as.matrix()

X[is.na(X)] <- 0             
X[is.infinite(X)] <- NA
X[is.na(X)] <- 0   

any(is.na(X))      
any(is.infinite(X)) 

new_X <- df_test %>%
  ungroup() %>% 
  select(-case, -county, -inc) %>% #, -neighbor_cum_inc_thatweek) %>%
  mutate(across(everything(), as.numeric)) %>%
  as.matrix()

new_X[is.na(new_X)] <- 0             
new_X[is.infinite(new_X)] <- NA      
new_X[is.na(new_X)] <- 0  



y <- df_train$case

dtrain <- xgb.DMatrix(data = X, label = y)


#params <- list(
#  objective = "reg:squarederror",  
#  eval_metric = "rmse"
#)

ratio <- sum(df_train$case == 0) / sum(df_train$case == 1)
params <- list(
  objective = "binary:logistic",   
  eval_metric = "logloss",         # 또는 auc
  scale_pos_weight = ratio  # unbalanced data
)


xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100
)  

xgb.importance(model = xgb_model) %>% 
  xgb.plot.importance()

yhat <- predict(xgb_model, newdata = new_X)

df_test_res <- df_test %>%
  mutate(xgb_est = yhat,
         xgb_est1 = ifelse(xgb_est<0.5,0,1))

# Plot Observed vs Predicted
plot(df_test_res$case, df_test_res$xgb_est,
     xlab = "Observed inc", ylab = "Predicted inc",
     main = "XGBoost Prediction vs Observed", pch = 19, col = "steelblue")

abline(0, 1, col = "red", lwd = 2)

table(df_test_res$case, df_test_res$xgb_est1)

mydate <- c("2025-04-12", "2025-04-19", "2025-04-26", "2025-05-03", 
            "2025-05-10", "2025-05-17", "2025-05-24", "2025-05-31")

df_test_res %>%
  filter(week == mydate[1]) %>%
  left_join(tx_county_area %>%
              select(county, geometry), by = "county") %>%
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = xgb_est), color = "black") +
  scale_fill_gradient(low = "lightblue", high = "black", na.value = "gray90") +
  labs(title = "Prediction map from full model",
       fill = "Predicted inc") +
  theme_minimal()



if(2==3){
df_test_res %>%
  filter(week == mydate[8]) %>%
  left_join(tx_county_area %>%
              select(county, geometry), by = "county") %>%
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = inc), color = "black") +
  scale_fill_gradient(low = "lightblue", high = "black", na.value = "gray90") +
  labs(title = "Prediction map from full model",
       fill = "Predicted inc") +
  theme_minimal()




library(ggplot2)
library(gganimate)
library(dplyr)
library(sf)

plot_data <- df_train %>%
  left_join(tx_county_area %>% select(county, geometry), by = "county") %>%
  st_as_sf()

# Plot
p <- ggplot(plot_data) +
  geom_sf(aes(fill = inc, geometry = geometry), color = "black") +
  scale_fill_gradient(low = "lightblue", high = "black", na.value = "gray90") +
  labs(title = "Predicted Measles Risk by County", 
       subtitle = "Week: {closest_state}", 
       fill = "Predicted inc") +
  theme_minimal() +
  transition_states(week, transition_length = 2, state_length = 1) +
  ease_aes('linear')

# Save the animation
animate(p, nframes = 11, fps = 1, width = 800, height = 600, renderer = gifski_renderer("measles_prediction.gif"))


df_test_res1 <- df_test_res %>%
  left_join(tx_county_area %>%
              select(county, geometry), by = "county") %>%
  st_as_sf() 

p1 <- ggplot(df_test_res1) +
  geom_sf(aes(fill = xgb_est, geometry = geometry), color = "black") +
  scale_fill_gradient(low = "lightblue", high = "black", na.value = "gray90") +
  labs(title = "Predicted Measles Risk by County", 
       subtitle = "Week: {closest_state}", 
       fill = "Predicted inc") +
  theme_minimal() +
  transition_states(week, transition_length = 2, state_length = 1) +
  ease_aes('linear')

# Save the animation
animate(p1, nframes = 8, fps = 1, width = 800, height = 600, renderer = gifski_renderer("measles_prediction.gif"))
}

