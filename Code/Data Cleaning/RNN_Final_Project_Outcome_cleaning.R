#--------------------------------------------------------------------
# RNN Final Project
#--------------------------------------------------------------------
# Economic Data Preparation
#--------------------------------------------------------------------

rm(list = ls(all = TRUE))

# Load libraries
library(tidyverse)
library(readxl)
library(lubridate)



# Load Datasets
#--------------------------------------------------------------------

# Metro area labels
metro_areas_labels <- read_csv("data/Metro_Areas_Labels.csv")

## Personal income
msa_pa_1969_2023 <- read_csv("data/msa_pa_1969_2023.csv")

mic_pa_1969_2023 <- read_delim("data/mic_pa_1969_2023.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)

## Economic profile
msa_econ_profil_1969_2023 <- read_csv("data/msa_econ_profil_1969_2023.csv")

mic_econ_profil_1969_2023 <- read_delim("data/mic_econ_profil_1969_2023.csv", 
                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

## GDP
msa_gdp_2001_2023 <- read_delim("data/msa_gdp_2001_2023.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

mic_gdp_cd_2001_2023 <- read_delim("data/mic_gdp_cd_2001_2023.csv", 
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

## Employment and wages
# Define base path
base_path <- "data/Census of Employment and Wages"

# Create an empty list to store yearly data
msa_empl_wages_list <- list()

# Loop through years 1990 to 2024
for (year in 1990:2024) {
  file_path <- file.path(
    base_path,
    paste0(year, "_all_county_high_level"),
    paste0("allhlcn", substr(year, 3, 4), ".xlsx") # e.g., allhlcn90.xlsx
  )
  
  if (file.exists(file_path)) {
    message("Loading: ", file_path)
    msa_empl_wages_list[[as.character(year)]] <- read_excel(file_path)
  } else {
    warning("File not found: ", file_path)
  }
}

# Combine all into one dataframe
msa_empl_wages_1990_2024 <- bind_rows(msa_empl_wages_list, .id = "year")


## Unemployment rates
msa_unempl_1990_2024 <- read_csv("data/Unemployment_rates_for_metropolitan_areas/annual.csv")

mic_unempl_1990_2024_1 <- read_excel("data/Unemployment_rates_for_micropolitan_areas/mic_unempl_1990_2024_1.xlsx")

mic_unempl_1990_2024_2 <- read_excel("data/Unemployment_rates_for_micropolitan_areas/mic_unempl_1990_2024_2.xlsx")

mic_unempl_1990_2024_3 <- read_excel("data/Unemployment_rates_for_micropolitan_areas/mic_unempl_1990_2024_3.xlsx")

mic_unempl_1990_2024 <- mic_unempl_1990_2024_1 %>% 
  bind_rows(mic_unempl_1990_2024_2) %>% 
  bind_rows(mic_unempl_1990_2024_3)


# Prepare Datasets
#--------------------------------------------------------------------

# Reshape from wide to long
msa_pa_prep <- msa_pa_1969_2023 %>%
  pivot_longer(
    cols = matches("^(19[6-9][0-9]|20[0-2][0-9])$"),  # matches 1969–2023
    names_to = "year",
    values_to = "value"
  ) %>%
  select(CBSA_code = GeoFIPS,
         CBSA_name = GeoName,
         description = Description,
         year,
         value
  ) %>% 
  filter(!is.na(CBSA_name)) %>% 
  mutate(description = case_when(
    description == "Personal income (thousands of dollars)" ~ 
      "Personal income (thousands of dollars)",
    description == "Per capita personal income (dollars) 2/" ~ 
      "Per capita personal income (dollars)",
    description == "Population (persons) 1/" ~ 
      "Population (persons)",
    )) %>% 
  pivot_wider(
    names_from = description,
    values_from = value
  ) %>% 
  mutate(
    CBSA_name = gsub("\\s*\\(Metropolitan Statistical Area\\)\\s*\\**", "", CBSA_name)
  )


# Reshape from wide to long
mic_pa_prep <- mic_pa_1969_2023 %>%
  mutate(across(matches("^(19[6-9][0-9]|20[0-2][0-9])$"),
                ~ suppressWarnings(as.numeric(.)))) %>% 
  pivot_longer(
    cols = matches("^(19[6-9][0-9]|20[0-2][0-9])$"),  # matches 1969–2023
    names_to = "year",
    values_to = "value"
  ) %>%
  select(CBSA_code = GeoFIPS,
         CBSA_name = GeoName,
         description = Description,
         year,
         value
  ) %>% 
  filter(!is.na(CBSA_name)) %>% 
  mutate(description = case_when(
    description == "Personal income (thousands of dollars)" ~ 
      "Personal income (thousands of dollars)",
    description == "Per capita personal income (dollars) 2/" ~ 
      "Per capita personal income (dollars)",
    description == "Population (persons) 1/" ~ 
      "Population (persons)",
  )) %>% 
  pivot_wider(
    names_from = description,
    values_from = value
  ) %>% 
  mutate(
    CBSA_code = as.character(gsub("[^0-9]", "", CBSA_code)),
    CBSA_name = gsub("\\s*\\(Micropolitan Statistical Area\\)\\s*\\**", "", CBSA_name)
  )


# Reshape from wide to long
msa_econ_profi_prep <- msa_econ_profil_1969_2023 %>%
  pivot_longer(
    cols = matches("^(19[6-9][0-9]|20[0-2][0-9])$"),  # matches 1969–2023
    names_to = "year",
    values_to = "value"
  ) %>%
  select(CBSA_code = GeoFIPS,
         CBSA_name = GeoName,
         description = Description,
         year,
         value
  ) %>% 
  filter(!is.na(CBSA_name),
         description %in% c(
           "Net earnings by place of residence",
           "Per capita net earnings 4/",
           "Per capita personal current transfer receipts 4/",
           "Per capita income maintenance benefits 4/",
           "Per capita unemployment insurance compensation 4/",
           "Per capita retirement and other 4/",
           "Per capita dividends, interest, and rent 4/"
           )) %>% 
  mutate(description = case_when(
    description == "Net earnings by place of residence" ~ 
      "Net earnings by place of residence (thousands of dollars)",
    description == "Per capita net earnings 4/" ~ 
      "Per capita net earnings (dollars)",
    description == "Per capita personal current transfer receipts 4/" ~ 
      "Per capita personal current transfer receipts (dollars)",
    description == "Per capita income maintenance benefits 4/" ~ 
      "Per capita income maintenance benefits (dollars)",
    description == "Per capita unemployment insurance compensation 4/" ~ 
      "Per capita unemployment insurance compensation (dollars)",
    description == "Per capita retirement and other 4/" ~ 
      "Per capita retirement and other (dollars)",
    description == "Per capita dividends, interest, and rent 4/" ~ 
      "Per capita dividends, interest, and rent (dollars)",
  )) %>% 
  pivot_wider(
    names_from = description,
    values_from = value
  ) %>% 
  mutate(
    CBSA_name = gsub("\\s*\\(Metropolitan Statistical Area\\)\\s*\\**", "", CBSA_name)
  )


# Reshape from wide to long
mic_econ_profi_prep <- mic_econ_profil_1969_2023 %>%
  mutate(across(matches("^(19[6-9][0-9]|20[0-2][0-9])$"),
                ~ suppressWarnings(as.numeric(.)))) %>% 
  pivot_longer(
    cols = matches("^(19[6-9][0-9]|20[0-2][0-9])$"),  # matches 1969–2023
    names_to = "year",
    values_to = "value"
  ) %>%
  select(CBSA_code = GeoFIPS,
         CBSA_name = GeoName,
         description = Description,
         year,
         value
  ) %>% 
  filter(!is.na(CBSA_name),
         description %in% c(
           "Net earnings by place of residence",
           "Per capita net earnings 4/",
           "Per capita personal current transfer receipts 4/",
           "Per capita income maintenance benefits 4/",
           "Per capita unemployment insurance compensation 4/",
           "Per capita retirement and other 4/",
           "Per capita dividends, interest, and rent 4/"
         )) %>% 
  mutate(description = case_when(
    description == "Net earnings by place of residence" ~ 
      "Net earnings by place of residence (thousands of dollars)",
    description == "Per capita net earnings 4/" ~ 
      "Per capita net earnings (dollars)",
    description == "Per capita personal current transfer receipts 4/" ~ 
      "Per capita personal current transfer receipts (dollars)",
    description == "Per capita income maintenance benefits 4/" ~ 
      "Per capita income maintenance benefits (dollars)",
    description == "Per capita unemployment insurance compensation 4/" ~ 
      "Per capita unemployment insurance compensation (dollars)",
    description == "Per capita retirement and other 4/" ~ 
      "Per capita retirement and other (dollars)",
    description == "Per capita dividends, interest, and rent 4/" ~ 
      "Per capita dividends, interest, and rent (dollars)",
  )) %>% 
  pivot_wider(
    names_from = description,
    values_from = value
  ) %>% 
  mutate(
    CBSA_code = as.character(gsub("[^0-9]", "", CBSA_code)),
    CBSA_name = gsub("\\s*\\(Micropolitan Statistical Area\\)\\s*\\**", "", CBSA_name)
  )


# Reshape from wide to long
msa_gdp_prep <- msa_gdp_2001_2023 %>%
  pivot_longer(
    cols = matches("^(19[6-9][0-9]|20[0-2][0-9])$"),  # matches 1969–2023
    names_to = "year",
    values_to = "value"
  ) %>%
  select(CBSA_code = GeoFips,
         CBSA_name = GeoName,
         description = Description,
         year,
         value
  ) %>% 
  filter(!is.na(CBSA_name)
         ) %>% 
  pivot_wider(
    names_from = description,
    values_from = value
  ) %>% 
  mutate(
    CBSA_name = gsub("\\s*\\(Metropolitan Statistical Area\\)\\s*\\**", "", CBSA_name),
    CBSA_code = as.character(CBSA_code)
  )


# Reshape from wide to long
mic_gdp_cd_prep <- mic_gdp_cd_2001_2023 %>%
  pivot_longer(
    cols = matches("^(19[6-9][0-9]|20[0-2][0-9])$"),  # matches 1969–2023
    names_to = "year",
    values_to = "Current-dollar GDP (thousands of current dollars)"
  ) %>%
  select(CBSA_code = GeoFips,
         CBSA_name = GeoName,
         year,
         `Current-dollar GDP (thousands of current dollars)`
  ) %>% 
  filter(!is.na(CBSA_name)
  ) %>% 
  mutate(
    CBSA_name = gsub("\\s*\\(Micropolitan Statistical Area\\)\\s*\\**", "", CBSA_name),
    CBSA_code = as.character(CBSA_code)
  )


# Prepare employment and wages data
msa_empl_wages_prep <- msa_empl_wages_1990_2024 %>% 
  filter(`Area Type` == "MSA",
         Ownership == "Total Covered",
         Industry %in% c("Total, all industries", "10 Total, all industries")) %>% 
  select(year,
         CBSA_code = `Area\r\nCode`,
         CBSA_name = Area,
         `Annual Average Establishment Count`,
         `Annual Average Employment`,
         `Annual Total Wages`,
         `Annual Average Weekly Wage`,
         `Annual Average Pay`
         ) %>% 
  mutate(
    CBSA_code = str_remove(CBSA_code, "^C"),  # Remove leading 'C'
    CBSA_code = paste0(CBSA_code, "0"),       # Append '0' at the end
    CBSA_name = gsub("\\s*\\MSA\\s*\\**", "", CBSA_name)
  )
  

# Prepare msa unemployment rate data
msa_unempl_prep <- msa_unempl_1990_2024 %>% 
  rename(year = observation_date) %>%                 # Rename observation_date to year
  mutate(year = as.character(year(year))) %>%         # Extract just the year number
  pivot_longer(
    cols = -year,                                     # All columns except year
    names_to = "CBSA_code",                           # New column with old column names
    values_to = "Unemployment Rate"                   # New column with the values
  ) %>%
  mutate(
    CBSA_code = sub("^LAUMT\\d{2}(\\d{5}).*", "\\1", CBSA_code)
  )


# Prepare mic unemployment rate data
mic_unempl_prep <- mic_unempl_1990_2024 %>% 
  rename(CBSA_code = `Series ID`) %>%                 # Rename observation_date to year
  filter(str_ends(as.character(CBSA_code), "3")) %>% 
  pivot_longer(
    cols = -CBSA_code,                                # All columns except year
    names_to = "year",                                # New column with old column names
    values_to = "Unemployment Rate"                   # New column with the values
  ) %>%
  mutate(year = str_replace(year, "^Annual\\s+", ""),
         CBSA_code = sub("^LAUMC\\d{2}(\\d{5}).*", "\\1", CBSA_code))
  


# Join Datasets
#--------------------------------------------------------------------

msa_combined <- msa_pa_prep %>%
  left_join(msa_econ_profi_prep, by = c("CBSA_code", "CBSA_name", "year")) %>% 
  left_join(msa_gdp_prep, by = c("CBSA_code", "CBSA_name", "year")) %>% 
  left_join(msa_empl_wages_prep, by = c("CBSA_code", "CBSA_name", "year")) %>% 
  left_join(msa_unempl_prep, by = c("CBSA_code", "year"))

msa_mic_combined <- msa_pa_prep %>%
  left_join(msa_econ_profi_prep, by = c("CBSA_code", "CBSA_name", "year")) %>% 
  left_join(msa_gdp_prep, by = c("CBSA_code", "CBSA_name", "year")) %>% 
  left_join(msa_empl_wages_prep, by = c("CBSA_code", "CBSA_name", "year")) %>% 
  left_join(msa_unempl_prep, by = c("CBSA_code", "year")) %>% 
  bind_rows(mic_pa_prep %>% 
              left_join(mic_econ_profi_prep, by = c("CBSA_code", "CBSA_name", "year")) %>% 
              left_join(mic_gdp_cd_prep, by = c("CBSA_code", "CBSA_name", "year")) %>% 
              left_join(mic_unempl_prep, by = c("CBSA_code", "year"))
            )  
  



# Save to files
# write_csv(msa_combined, "data/Outcome Data/msa_outcome_comb.csv")
# write_csv(msa_mic_combined, "data/Outcome Data/msa_mic_outcome_comb.csv")








