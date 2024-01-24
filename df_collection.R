library(assertthat)
library(ggplot2)
library(zoo)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggsci)

fig.dir <- "Figure/" # subfolder path are defined with ggsave
input.dir <- "../"

# collect dataset for visualization ----

MODEL_BASE_YEARS        <- c(1975, 1990, 2005, 2010, 2015)
HISTORICAL_YEARS        <- 1971:2015
# Future (not calibrated) model periods. Only level 2 chunks should reference these
MODEL_FUTURE_YEARS      <- seq(2020, 2100, 5)
MODEL_YEARS             <- c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)


repeat_add_columns <- function(x, y) {
  UNIQUE_JOIN_FIELD <- NULL           # silence package checks.
  assert_that(tibble::is_tibble(x))
  assert_that(tibble::is_tibble(y))

  x %>%
    mutate(UNIQUE_JOIN_FIELD = 1) %>%
    full_join(mutate(y, UNIQUE_JOIN_FIELD = 1), by = "UNIQUE_JOIN_FIELD") %>%
    select(-UNIQUE_JOIN_FIELD)
}

gather_time <- function(.data){
  .data %>%
    tidyr::gather(year, value, names(.)[grepl("[0-9]{4}", names(.))]) %>%
    mutate(year = as.integer(gsub("X", "", year))) %>%
    return()
}

# region mapping ----

iso_GCAM_regID <-  read.csv("iso_GCAM_regID.csv", skip = 6, header = T)
GCAM_region_names <-  read.csv("GCAM_region_names.csv", skip = 6, header = T)


cluster <- read.csv("ClusterRegion.csv") %>%
  select(region, REG, REG10_AR6, REG5_AR6 )

# 5 region ---
REG <- function(df){
  df %>%
    left_join(cluster, by = "region") %>%
    select(-region, -REG,-REG10_AR6) %>%
    group_by(across(c(-value))) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    rename(REG = REG5_AR6) ->
    df
  return(df)
}

N <- 5;
NCOL <-  3;
NWIDTH <- 12;

YEAR_BASE <- 2015
YEAR_start <- 1975

# ************ drivers ************ ----

# GDP ----
GDP <- read.csv("C:/Model/LaborModelingResults/GDP_gSSP2.csv") %>%
  select(-X, -scenario) %>%
  left_join(GCAM_region_names, by = "GCAM_region_ID") %>% # million 1990$
  select(-GCAM_region_ID) %>%
  select(region, year, value)

# GDPpc ----
GDPpc <- read.csv("C:/Model/LaborModelingResults/GDPpc_gSSP2.csv") %>%
  select(-X, -scenario) %>%
  left_join(GCAM_region_names, by = "GCAM_region_ID") %>% # million 1990$
  select(-GCAM_region_ID) %>%
  select(region, year, value)

# population ----
POP <- read.csv("POP_SSP2.csv") %>%
  mutate(value = ifelse(year < 2015, hist, futr)) %>%
  select(-hist, -futr) %>%
  select(region, year, value)


# rural population ----
WB_rural <- read.csv("C:/Model/heatstress/API_SP.RUR.TOTL_DS2_en_csv_v2_5455093.csv", fileEncoding = 'UTF-8-BOM', skip = 4, header = T) %>%
  gather_time() %>%
  mutate(iso = tolower(Country.Code)) %>%
  left_join(iso_GCAM_regID, by = "iso") %>%
  left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
  select(year, region, GCAM_region_ID, value) %>%
  filter(year<= YEAR_BASE) %>%
  na.omit() %>%
  group_by(region, year) %>%
  summarize(hist = sum(value))

WB_YEAR <- unique(WB_rural$year)

RuralPop <- read.csv("RuralPop.csv")

RuralPop.TW <- RuralPop %>%
  filter(region == "Taiwan",
         year == 2015) %>%
  select(-year) %>%
  as_tibble() %>%
  repeat_add_columns(tibble::tibble(year = WB_YEAR)) %>%
  select(region, year, value = Rural_pop) %>%
  filter(year < 2015)

RuralPop %>%
  filter(year >= 2015) %>%
  rename(futr = Rural_pop) %>%
  full_join(WB_rural, by = c("region", "year")) %>%
  mutate(value = ifelse(year < 2015, hist, futr)) %>%
  select(-X, -hist, -futr) %>%
  select(region, year, value) %>%
  bind_rows(RuralPop.TW) ->
  RURALPOP

RURALPOP %>%
  group_by(region) %>%
  mutate(index = value / value[year == 2015])

# labor force ----
# LF_GCAM7_L280.csv is from module_socio_L280.GDP_macro
# national.accounts.hist %>% select(GCAM_region_ID, year, labor.force) -> LF_GCAM7_L280

LF <- read.csv("LF_GCAM7_L280.csv") %>% # mpl
  left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
  select(-X) %>%
  mutate(LF = labor.force) %>%
  select(region, year, value = LF)

# LFS <- read.csv("LF_share.csv")
# POP %>%
#   rename(POP = value) %>%
#   select(region, year, POP) %>%
#   left_join(LFS, by = c("region", "year")) %>%
#   filter(year >= YEAR_start) %>%
#   group_by(region) %>%
#   mutate(labor.force.share = na.approx(labor.force.share)) %>%
#   mutate(LF = POP * labor.force.share / 10^3) %>%  # mpl
#   select(region, year, value = LF) ->
#   LF

# productivity growth ----
# hist --
AgTFP_USDA <- read.csv("AgTFPInternational2021_long.csv")

AGLU_ctry <- readr::read_csv("AGLU_ctry.csv", comment = "#")
iso_GCAM_regID <- readr::read_csv("iso_GCAM_regID.csv", comment = "#")
GCAM_region_names <- readr::read_csv("GCAM_region_names.csv", comment = "#")

AgTFP_USDA %>%
  filter(Attribute %in% c("Labor_Q", "Outall_Q")) %>% # $/kpl
  transmute(iso = tolower(ISO3), Attribute, year = Year, value = Value) %>%
  left_join(iso_GCAM_regID %>% select(iso, GCAM_region_ID)) %>%
  left_join(GCAM_region_names) %>%
  group_by_at(vars(-value, -iso)) %>%
  summarize(value = sum(value)) %>% ungroup() %>%
  spread(Attribute, value) %>%
  transmute(region, year, value = Outall_Q / Labor_Q) %>%
  group_by_at(vars(region)) %>%
  mutate(value = value / value[year == 2015]) %>%
  filter(year < 2015) %>%
  na.omit() -> A

# futr --
gcam_macro_TFP_open <- read.csv("gcam_macro_TFP_open.csv", skip = 6, header = T) %>%
  filter(scenario == "gSSP2")

gcam_macro_TFP_open %>%
  filter(region != "South America_Northern") %>%
  bind_rows(gcam_macro_TFP_open %>% filter(region == "South America_Southern") %>%
              mutate(region = "South America_Northern")) %>%
  filter(year >= 2015) %>%
  select(region, year, value = productivity) %>%
  mutate(value = ifelse(year == 2015, 1, value)) %>%
  bind_rows(A) -> eta

SCENARIO <- c("E0")

SCE_NM <- function(.data){
  .data %>%
    mutate(scenario = gsub("E0","Evolving", scenario),
           scenario = gsub("E3","Static", scenario)) %>%
    return()
}

GCAM <- function(query){
  df_list = list()
  for (i in 1:length(SCENARIO)){
    sce_name = SCENARIO[i]
    filename = paste0(input.dir,'/',sce_name,'/',query,'.csv')
    filename
    input = read.csv(filename, skip = 1, header = T)
    input %>%
      gather_time() %>%
      dplyr::select(-Units, -X) %>%
      dplyr::mutate(scenario = sce_name) -> middle
    names(middle) <- gsub("-", ".", names(middle)) # rename columns to replace dash to dot
    df_list[[i]] <- middle
  }
  return(df_list)
}


# ag labor ----
USDA_Labor <- read.csv("USDA_Labor.csv") %>% select(-GCAM_region_ID)

query <-  "LaborDemandSec"
df_list <- GCAM(query)
# collect total ag labor demand across scenarios
do.call(rbind, df_list) %>%
  group_by(region, year) %>%
  summarise(value = sum(value)) %>%
  filter(year > 2015) %>%
    bind_rows(USDA_Labor %>%
                as_tibble() %>%
                mutate(value = labor_ppl/10^6) %>%
                select(region, year, value)) -> LA

# ag output $ ----
AgTFP_USDA %>%
  filter(Attribute %in% c("Outall_Q")) %>% # $1000 at constant 2015 prices
  transmute(iso = tolower(ISO3), Attribute, year = Year, value = Value) %>%
  left_join(iso_GCAM_regID %>% select(iso, GCAM_region_ID)) %>%
  left_join(GCAM_region_names) %>%
  filter(!is.na(iso)) %>%
  filter(iso != "") %>%
  filter(!iso %in% c("etf", "csk", "blx", "ser", "ysr", "svu")) %>% # aggregated region by USDA, not country
  group_by_at(vars(-value, -iso)) %>%
  summarize(value = sum(value)/10^6) %>% ungroup() %>% # $ billion at constant 2015 prices
  spread(Attribute, value) %>%
  select(region, year, value = Outall_Q) ->
  Output_hist

query <-  "CropProdSec"
df_list <- GCAM(query)
do.call(rbind, df_list) %>%
  select(region, sector, year, Q = value) -> df_crop

query <-  "AnimalProdSec"
df_list <- GCAM(query)
do.call(rbind, df_list) %>%
  select(region, sector, year, Q = value)-> df_animal

query <-  "CropPrice"
df_list <- GCAM(query)
do.call(rbind, df_list) %>%
  select(region, sector, year, P = value)-> df_P_crop

query <-  "AnimalPrice"
df_list <- GCAM(query)
do.call(rbind, df_list) %>%
  select(region, sector, year, P = value)-> df_P_animal

df_crop %>% bind_rows(df_animal) %>% # Mt
  left_join(df_P_crop %>% bind_rows(df_P_animal),  # $/kg
            by = c("region", "sector", "year")) %>%
  mutate(value = P*Q) %>%
  group_by(region, year) %>%
  summarise(value = sum(value, na.rm = T)) %>% # billion 1975$, gdp_deflator(2015, 1975) = 3.507477
  mutate(value = value * 3.507477) ->
  df_V

df_crop %>% bind_rows(df_animal) %>% # Mt
  left_join(df_P_crop %>% bind_rows(df_P_animal),  # $/kg
            by = c("region", "sector", "year")) %>%
  filter(sector != "Forest") %>%
  mutate(value = P*Q) %>%
  group_by(region, year) %>%
  summarise(value = sum(value, na.rm = T)) %>% # billion 1975$, gdp_deflator(2015, 1975) = 3.507477
  mutate(value = value * 3.507477) ->
  df_V_nofor

Output_hist %>% mutate(source = "USDA") %>%
  bind_rows(df_V %>% mutate(source = "GCAM")) %>%
  bind_rows(df_V_nofor %>% mutate(source = "GCAM_no_For")) ->
  df_output

saveRDS(df_output, file = "C:/Model/LaborModelingResults/output.RDS")


# SUMMARY: ************ drivers ************ ----
LF %>% mutate(var = "LF", var1 = "pop") %>%
  bind_rows(RURALPOP %>% mutate(var = "rural", value = value / 10^6, var1 = "pop")) %>%
  bind_rows(POP %>% mutate(var = "pop", value = value / 10^3, var1 = "pop")) %>%
  bind_rows(GDP %>% mutate(var = "GDP", var1 = "gdp")) %>%
  bind_rows(GDPpc %>% mutate(var = "GDPpc", var1 = "gdp")) %>%
  bind_rows(eta %>% mutate(var = "eta", var1 = "pop")) %>%
  bind_rows(LA %>% mutate(var = "ag labor", var1 = "pop")) %>%
  na.omit() ->
  DRIVER

saveRDS(DRIVER, file = "C:/Model/LaborModelingResults/DRIVER.RDS")

DRIVER %>%
  filter(year >= YEAR_start) %>%
  group_by(region, var, var1) %>%
  mutate(index = value / value[year == 2015]) ->
  index.DRIVER

tapply(index.DRIVER$index, index.DRIVER$var, summary)

index.DRIVER %>%
  ggplot() +
  geom_rect(aes(xmin=1975, xmax=2015, ymin=-Inf,ymax=Inf), alpha=0.2, fill="light grey") +
  geom_line(aes(x = year, y = index, color = var), linewidth = 1.3) +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  facet_grid(var1~ region, scales = "free") +
  labs(x = "Year", y = "Relative change (2015 = 1)", color = "Driver") +
  scale_color_npg() +
  ggtitle("") +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(legend.position = "bottom") -> p; p

# ************ labor metrics ************ ----
SCENARIO <- c("E0", "E3","E4_eta_higher", "E4_eta_lower_NLS")

SCE_NM <- function(.data){
  .data %>%
    mutate(scenario = gsub("E0","Evolving", scenario),
           scenario = gsub("E3","Exp1", scenario),
           scenario = gsub("E4_theta_higher","Exp2", scenario),
           scenario = gsub("E4_theta_lower_NLS","Exp3", scenario)) %>%
    return()
}

# labor demand ----
USDA_Labor <- read.csv("USDA_Labor.csv") %>% select(-GCAM_region_ID)

USDA_Capital <- read.csv("USDA_Capital.csv") %>% select(-GCAM_region_ID)

query = "LaborDemandSec"
df_list = list()
for (i in 1:length(SCENARIO)){
  sce_name = SCENARIO[i]

  filename = paste0(input.dir,sce_name,'/',query,'.csv')
  filename
  input = read.csv(filename, skip = 1, header = T)

  input %>%
    gather_time() %>%
    select(-Units, -X) %>%
    mutate(scenario = sce_name) -> middle
  names(middle) <- gsub("-", ".", names(middle)) # rename columns to replace dash to dot
  df_list[[i]] <- middle
}
# collect total ag labor demand across scenarios
df_L <- do.call(rbind, df_list)

df_L %>%
  group_by(scenario, region, year) %>%
  summarise(value = sum(value)) %>%
  filter(year > 2015) ->
  df_LL

USDA_Labor %>%
  as_tibble() %>%
  mutate(scenario = "E0") %>%
  bind_rows(USDA_Labor %>%
              mutate(scenario = "E3")) %>%
  mutate(value = labor_ppl/10^6) %>%
  select(scenario, region, year, value) %>%
  # filter(year %in% c(seq(1975, 2015, 5))) %>%
  bind_rows(df_LL) ->
  df_LLL

# capital demand ----
query = "CapitalDemandSec"
df_list = list()
for (i in 1:length(SCENARIO)){
  sce_name = SCENARIO[i]

  filename = paste0(input.dir,sce_name,'/',query,'.csv')
  filename
  input = read.csv(filename, skip = 1, header = T)

  input %>%
    gather_time() %>%
    select(-Units, -X) %>%
    mutate(scenario = sce_name) -> middle
  names(middle) <- gsub("-", ".", names(middle)) # rename columns to replace dash to dot
  df_list[[i]] <- middle
}
# collect total ag labor demand across scenarios
df_K <- do.call(rbind, df_list)

df_K %>%
  group_by(scenario, region, year) %>%
  summarise(value = sum(value)) %>%
  filter(year > 2015) ->
  df_KK

USDA_Capital %>%
  mutate(scenario = "E0") %>%
  bind_rows(USDA_Capital %>%
              mutate(scenario = "E3")) %>%
  mutate(value = capital_1975USD / 10^9) %>%
  select(scenario, region, year, value) %>%
  # filter(year %in% c(seq(1975, 2015, 5))) %>%
  bind_rows(df_KK) ->
  df_KKK

# L/K ratio ----
# historcal labor and capital data does not have biomass sector
# so L/K exclude biomass sector info in the future
USDA_Labor %>%
  as_tibble() %>%
  mutate(scenario = "E0") %>%
  bind_rows(USDA_Labor %>%
              mutate(scenario = "E3")) %>%
  mutate(value = labor_ppl/10^6) %>%
  select(scenario, region, year, value) %>%
  # filter(year %in% c(seq(1975, 2015, 5))) %>%
  bind_rows(df_L %>%
              filter(sector != "biomass",
                     year > 2015) %>%
              group_by(scenario, region, year) %>%
              summarise(value = sum(value))) ->
  df_LK_L

USDA_Capital %>%
  mutate(scenario = "E0") %>%
  bind_rows(USDA_Capital %>%
              mutate(scenario = "E3")) %>%
  mutate(value = capital_1975USD / 10^9) %>%
  select(scenario, region, year, value) %>%
  # filter(year %in% c(seq(1975, 2015, 5))) %>%
  bind_rows(df_K %>%
              filter(sector != "biomass",
                     year > 2015) %>%
              group_by(scenario, region, year) %>%
              summarise(value = sum(value))) ->
  df_LK_K


  df_LK_L %>% rename(labor = value) %>%
    left_join(df_LK_K %>% rename(capital = value),
              by = c("scenario", "region", "year")) %>%
    mutate(value = labor /capital) %>%
    select(scenario, region, year, value) ->
    df_LK

  # ag labor to labor force ratio ----
  LF %>%
    mutate(scenario = "E0") %>%
    bind_rows(LF %>%
                mutate(scenario = "E3")) %>%
    rename(LF = value) %>%
    left_join(df_LLL %>%  rename(L = value),
              by = c("scenario", "region" ,"year")) %>%
    na.omit() %>%
    mutate(value = L / LF) %>%
    select(scenario, region, year, value) ->
    df_share

  # effective labor ----
  # GCAM report physical.L
  # eff.L  = physical.L * productivity multiplier = physical.L / IO multiplier

  gcam_macro_TFP_open <-  read.csv("gcam_macro_TFP_open.csv", skip = 6, header = T) %>%
    filter(scenario == "gSSP2")

  gcam_macro_TFP_open %>%
    filter(region != "South America_Northern") %>%
    bind_rows(gcam_macro_TFP_open %>% filter(region == "South America_Southern") %>%
                mutate(region = "South America_Northern")) -> gcam_TFP

  AgTFP_USDA %>%
    filter(Attribute %in% c("Labor_Q", "Outall_Q")) %>% # $/kpl
    transmute(iso = tolower(ISO3), Attribute, year = Year, value = Value) %>%
    left_join(iso_GCAM_regID %>% select(iso, GCAM_region_ID)) %>%
    left_join(GCAM_region_names) %>%
    group_by_at(vars(-value, -iso)) %>%
    summarize(value = sum(value)) %>% ungroup() %>%
    spread(Attribute, value) %>%
    transmute(region, year, value = Outall_Q / Labor_Q) %>%
    group_by_at(vars(region)) %>%
    mutate(value = value / value[year == 2015]) %>%
    filter(year <= 2019) %>%
    bind_rows(gcam_TFP %>%
                select(region, year, value = productivity) %>%
                filter(year >= 2020)) -> growth

  df_LLL %>%
    group_by(scenario, region, year) %>%
    summarise(value = sum(value)) %>%
    left_join(growth %>% rename(productivity = value), by = c("region", "year")) %>%
    mutate(productivity = ifelse(scenario == "E3" & year > 2015, 1, productivity),
           effective.labor = value * productivity) %>%
    select(scenario, region, year, value = effective.labor) ->
    df_eff

  # SUMMARY: ************ metrics ************ ----

  df_LLL %>% mutate(var = "Labor demand") %>%
    bind_rows(df_KKK %>% mutate(var = "Capital demand")) %>%
    bind_rows(df_LK %>% mutate(var = "LK ratio")) %>%
    bind_rows(df_share %>% mutate(var = "Labor share")) %>%
    bind_rows(df_eff %>% mutate(var = "Effective labor")) ->
    df_metrics

saveRDS(df_metrics, file = "C:/Model/LaborModelingResults/metrics.RDS")


# ************ ag market ************ ----
GCAM <- function(query){
  df_list = list()
  for (i in 1:length(SCENARIO)){
    sce_name = SCENARIO[i]
    filename = paste0(input.dir,'/',sce_name,'/',query,'.csv')
    filename
    input = read.csv(filename, skip = 1, header = T)
    input %>%
      gather_time() %>%
      dplyr::select(-Units, -X) %>%
      dplyr::mutate(scenario = sce_name) -> middle
    names(middle) <- gsub("-", ".", names(middle)) # rename columns to replace dash to dot
    df_list[[i]] <- middle
  }
  return(df_list)
}

landleaf <- function(df){
  splits <- strsplit(df$LandLeaf, '_')
  splits <- do.call(rbind, splits)
  df[c("Crop","Basin","Ref_Irr", "hi_lo")] <- splits
  return(df)
}

landname <- function(df){
  df %>%
    mutate(sector = gsub("C4","", sector),
           sector = ifelse(grepl("biomass",sector), "biomass", sector),
           sector = ifelse(grepl("Fruits",sector), "Fruits", sector),
           sector = ifelse(grepl("MiscCrop",sector), "MiscCrop", sector),
           sector = ifelse(grepl("NutsSeeds",sector), "MiscCrop", sector),
           sector = ifelse(grepl("Fodder",sector), "Fodder Crops", sector),
           sector = ifelse(grepl("Oil",sector), "OilPlant", sector)) ->
    df
  return(df)
}

SECTOR <- function(.data){
  .data %>%
    dplyr::mutate(sector = gsub("regional ", "", sector),
                  sector = gsub("_", " ", sector),
                  sector = gsub(" ", "", sector)) %>%
    return()
}

# SCENARIO <- c("E0", "E3","E4_theta_higher", "E4_theta_lower")
#
# SCE_NM <- function(.data){
#   .data %>%
#     mutate(scenario = gsub("E0","Evolving", scenario),
#            scenario = gsub("E3","Exp1", scenario),
#            scenario = gsub("E4_theta_higher","Exp2", scenario),
#            scenario = gsub("E4_theta_lower","Exp3", scenario)) %>%
#     return()
# }

# GCAM query ----

query <-  "CropProdSec"
df_list <- GCAM(query)
Q_crop <- do.call(rbind, df_list) %>%
  select(scenario, region, sector, year, value)

query <-  "AnimalProdSec"
df_list <- GCAM(query)
Q_animal <- do.call(rbind, df_list) %>%
  select(scenario, region, sector, year, value)

query = "CropDemand"
df_list <- GCAM(query)
D_crop <- do.call(rbind, df_list) %>%
  group_by(scenario, region, input, year) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  rename(sector = input) %>%
  SECTOR() %>%
  select(scenario, region, sector, year, value)

query = "AnimalDemand"
df_list <- GCAM(query)
D_animal <- do.call(rbind, df_list) %>%
  group_by(scenario, region, input, year) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  rename(sector = input) %>%
  SECTOR() %>%
  select(scenario, region, sector, year, value)

query <-  "LaborDemandSec"
df_list <- GCAM(query)
df_L <- do.call(rbind, df_list) %>%
  select(scenario, region, sector, year, value)

query <-  "CropPrice"
df_list <- GCAM(query)
P_crop <- do.call(rbind, df_list) %>%
  select(scenario, region, sector, year, value)

query <-  "AnimalPrice"
df_list <- GCAM(query)
P_animal <- do.call(rbind, df_list) %>%
  select(scenario, region, sector, year, value)

query <-  "Land"
df_list <- GCAM(query)
df_H <- do.call(rbind, df_list) %>%
  landleaf() %>%
  rename(sector = Crop) %>%
  landname() %>%
  mutate(value = value / 10, # km2 to million ha
         sector = gsub("C4", "", sector)) %>%
  group_by(scenario, region, sector, year) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  select(scenario, region, sector, year, value)

query = "Ag_import"
df_list <- GCAM(query)
df_IM <- do.call(rbind, df_list) %>%
  filter(grepl('imported', subsector)) %>%
  select(scenario, region, sector = input, year, value) %>%
  mutate(sector = gsub("traded ", "", sector)) %>%
  SECTOR() %>%
  na.omit() %>%
  select(scenario, region, sector, year, value)

query = "Ag_export"
df_list <- GCAM(query)
df_EX <- do.call(rbind, df_list) %>%
  mutate(region = gsub(" traded.*","",subsector),
         input = tolower(sector),
         input = gsub("traded ", "", input)) %>%
  select(scenario, region, sector = input, year, value) %>%
  SECTOR() %>%
  na.omit() %>%
  select(scenario, region, sector, year, value)

query <-  "LaborPrice"
df_list <- GCAM(query)
df_W <- do.call(rbind, df_list) %>%
  mutate(region = gsub("Labor_Ag", "", market),
         sector = "Wage") %>%
  select(scenario, region, sector, year, value)


# SUMMARY: ************ ag market ************ ----

Q_crop %>% mutate(var = "Production") %>%
  bind_rows(Q_animal %>% mutate(var = "Production")) %>%
  bind_rows(D_crop %>% mutate(var = "Consumption")) %>%
  bind_rows(D_animal %>% mutate(var = "Consumption")) %>%
  bind_rows(df_H %>% mutate(var = "Land")) %>%
  bind_rows(P_crop %>% mutate(var = "Price")) %>%
  bind_rows(P_animal %>% mutate(var = "Price")) %>%
  bind_rows(df_IM %>% mutate(var = "Import")) %>%
  bind_rows(df_EX %>% mutate(var = "Export")) %>%
  bind_rows(df_L %>% mutate(var = "Labor")) %>%
  bind_rows(df_W %>% mutate(var = "Wage")) ->
  MARKET

saveRDS(MARKET, file = "C:/Model/LaborModelingResults/market.RDS")

# ************ environment ************ ----
# SCENARIO <- c("E0", "E3","E4_theta_higher", "E4_theta_lower")
#
# SCE_NM <- function(.data){
#   .data %>%
#     mutate(scenario = gsub("E0","Evolving", scenario),
#            scenario = gsub("E3","Exp1", scenario),
#            scenario = gsub("E4_theta_higher","Exp2", scenario),
#            scenario = gsub("E4_theta_lower","Exp3", scenario)) %>%
#     return()
# }

landleaf <- function(df){
  splits <- strsplit(df$LandLeaf, '_')
  splits <- do.call(rbind, splits)
  df[c("Crop","Basin","Ref_Irr", "hi_lo")] <- splits
  return(df)
}


landname <- function(df){
  df %>%
    mutate(sector = gsub("C4","", sector),
           sector = ifelse(grepl("biomass",sector), "biomass", sector),
           sector = ifelse(grepl("Fruits",sector), "Fruits", sector),
           sector = ifelse(grepl("MiscCrop",sector), "MiscCrop", sector),
           sector = ifelse(grepl("NutsSeeds",sector), "MiscCrop", sector),
           sector = ifelse(grepl("Fodder",sector), "Fodder Crops", sector),
           sector = ifelse(grepl("Oil",sector), "OilPlant", sector)) ->
    df
  return(df)
}


# water withdrawal  ----
query = "WaterWithdrawal"
df_list <- GCAM(query)
df_W <- do.call(rbind, df_list) %>%
  group_by(scenario, region, sector, year) %>%
  summarise(value = sum(value)) %>%
  mutate(var = "water")

# LUC emission  ----
query = "LUCemission"
df_list <- GCAM(query)
df_E <- do.call(rbind, df_list) %>%
  landleaf() %>%
  rename(sector = Crop) %>%
  landname() %>%
  group_by(scenario, region, sector, year) %>%
  summarise(value = sum(value)) %>%
  mutate(var = "Emission")

# SUMMARY: ************ environment ************ ----

df_W %>% bind_rows(df_E) -> ENVIRONMENT
saveRDS(ENVIRONMENT, file = "C:/Model/LaborModelingResults/environment.RDS")

