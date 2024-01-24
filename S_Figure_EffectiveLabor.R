library(zoo)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggsci)
library(assertthat)

market <- readRDS("market.RDS")

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

N <- "10"
cluster <- read.csv("ClusterRegion.csv") %>% select(region, REG10_AR6) %>%
  rename(REG = REG10_AR6)

REG <- function(df){
  df %>%
    ungroup() %>%
    left_join(cluster, by = "region") %>%
    select(-region) %>%
    group_by(across(c(-value))) %>%
    summarise(value = sum(value, na.rm = T))  ->
    df
  return(df)
}

# define plot theme ----
fontfamily = "Arial"
windowsFonts("Arial" = windowsFont("Arial"))

theme0 <- theme(
  panel.border = element_rect(colour = "black", size=1),
  text = element_text(family= fontfamily, size = 15),
  axis.text.y = element_text(angle = 0, color = "black", size = 15, margin = margin(r = 10)),
  axis.text.x = element_text(angle = 0, color = "black", size = 15, margin = margin(t = 10), vjust= 0.5),
  axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
  axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
  axis.text.x.top =  element_blank(),  axis.title.x.top = element_blank(),
  strip.background = element_rect(fill="grey95"),
  strip.text = element_text(size = 16),
  plot.title = element_text(hjust = 0.5,margin=margin(0,0,15,0)),
  plot.margin = margin(t = 10, r = 15, b = 10, l = 10), #panel.spacing = unit(1, "lines"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)

theme_leg <- theme(legend.justification = "center",
                   legend.key.size = unit(1.5, "cm"),
                   legend.key.height=unit(1.5,"line"),
                   legend.spacing.x = unit(1, 'cm'), #legend.spacing.y = unit(5, 'cm'),
                   legend.text = element_text(margin = margin(l = -25,t=0, b=0), size = 15),
                   legend.box.margin=margin(-10, 10,-8,10),
                   legend.background = element_blank(),
                   legend.position = "bottom")

theme_add <- theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   strip.text.x = element_text(size = 12))

cluster <- read.csv("ClusterRegion.csv") %>% select(region, REG10_AR6) %>%
  rename(REG = REG10_AR6)

SCENARIO <- list.dirs(path = "OriginalDB/", full.names = F, recursive = F)
# excl <- c("ref", "NLC", "KL", "figures")
# SCENARIO <- SCENARIO[! SCENARIO %in% excl]
SCENARIO <- SCENARIO[grepl("para", SCENARIO)]

gcam_macro_TFP_open <- read.csv("gcam_macro_TFP_open.csv", skip = 6, header = T) %>%
  filter(scenario == "gSSP2")

gcam_macro_TFP_open %>%
  filter(scenario == "gSSP2") %>%
  filter(year >= 2020) %>%
  filter(region == "South America_Southern") %>%
  mutate(region = "South America_Northern") %>%
  bind_rows(gcam_macro_TFP_open %>%
              filter(scenario == "gSSP2") %>%
              filter(year >= 2020) %>%
              filter(region != "South America_Northern")) ->
  M_tfp

M_tfp %>% select(-scenario, -gcam.version) %>%
  as_tibble() %>%
  repeat_add_columns(tibble(scenario = SCENARIO)) %>%
  mutate(productivity = ifelse(productivity < 1, 1, productivity),
         g = productivity - 1,
         mult = ifelse(productivity < 1, 1, productivity),
         mult = ifelse(grepl("eta1", scenario), 1 + (1-0.3)*g, mult),
         mult = ifelse(grepl("eta2", scenario), 1 + (1-0.2)*g, mult),
         mult = ifelse(grepl("eta3", scenario), 1 + (1-0.1)*g, mult),
         mult = ifelse(grepl("eta4", scenario), 1 + (1+0.1)*g, mult),
         mult = ifelse(grepl("eta5", scenario), 1 + (1+0.2)*g, mult),
         mult = ifelse(grepl("eta6", scenario), 1 + (1+0.3)*g, mult),
         mult = ifelse(grepl("static", scenario), 1 , mult),
         mult = ifelse(mult < 1, 1, mult)) %>% # adjust to avoid decreasing labor productivity growth
  select(-productivity) ->
  eta_futr

eta_futr %>%
  filter(year == 2020) %>%
  mutate(year = 2015,
         mult = 1) %>%
  bind_rows(eta_futr) %>%
  as.data.frame() %>%
  select(scenario, region, year, eta = mult)->
  eta

# effective labor ----
market %>% filter(var %in%c("Labor"), year >= 2015) %>% select(-var, labor = value) %>%
  group_by(scenario, region, year) %>%
  summarise(labor = sum(labor)) %>%
  left_join(eta, by = c("scenario", "region", "year")) %>%
  mutate(value = labor * eta) -> EL

SCE_NM <- function(.data){
  .data %>%
    mutate(scenario = gsub("para_evo","Evolving", scenario),
           scenario = gsub("para_static","Static", scenario),
           scenario = gsub("para_eta1","Eta growth -30%", scenario),
           scenario = gsub("para_eta2","Eta growth -20%", scenario),
           scenario = gsub("para_eta3","Eta growth -10%", scenario),
           scenario = gsub("para_eta4","Eta growth +10%", scenario),
           scenario = gsub("para_eta5","Eta growth +20%", scenario),
           scenario = gsub("para_eta6","Eta growth +30%", scenario),
           scenario = gsub("para_ls_", "Rural population ", scenario),
           scenario = gsub("para_gamma", "Labor supply elasticity ", scenario)) %>%
    return()
}

# plot 10 region effective labor ----
# level --
EL %>%
  filter(scenario %in% SCENARIO) %>%
  left_join(cluster, by = "region") %>%
  group_by(scenario, REG, year) %>%
  summarise(Phy.L = sum(labor),
            Eff.L = sum(value)) %>%
  mutate(experiment = scenario,
         experiment = ifelse(grepl("eta", scenario), "Labor productivity growth", experiment),
         experiment = ifelse(grepl("evo", scenario), "Evolving", experiment),
         experiment = ifelse(grepl("static", scenario), "Static", experiment),
         experiment = ifelse(grepl("gamma", scenario), "Labor supply elasticity", experiment),
         experiment = ifelse(grepl("SSP", scenario), "Rural population", experiment)) %>%
  rename(variable = experiment) %>%
  SCE_NM() %>%
  ggplot() +
  geom_line(aes(x = year, y = Eff.L, color = scenario, linetype = variable)) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  facet_wrap(~ REG, ncol = 5, scales = "free_y") +
  labs(x = "Year", y = "Effective labor supplied",
       color = "scenario") +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(legend.position = "right") -> p1; p1

ggsave("S_Figure_EFL/effective labor level.png", p1, width = 18, height = 14)


# relative to 2015 --
EL %>%
  filter(scenario %in% SCENARIO) %>%
  left_join(cluster, by = "region") %>%
  group_by(scenario, REG, year) %>%
  summarise(Phy.L = sum(labor),
            Eff.L = sum(value)) %>%
  mutate(experiment = scenario,
         experiment = ifelse(grepl("eta", scenario), "Labor productivity growth", experiment),
         experiment = ifelse(grepl("gamma", scenario), "Labor supply elasticity", experiment),
         experiment = ifelse(grepl("evo", scenario), "Evolving", experiment),
         experiment = ifelse(grepl("static", scenario), "Static", experiment),
         experiment = ifelse(grepl("SSP", scenario), "Rural population", experiment)) %>%
  rename(variable = experiment) %>%
  group_by(scenario, REG) %>%
  mutate(index = Eff.L / Eff.L[year == 2015]) %>%
  SCE_NM() %>%
  ggplot() +
  geom_line(aes(x = year, y = index, color = scenario, linetype = variable)) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  # facet_wrap(~ REG, ncol = 5) +
  facet_wrap(~ REG, ncol = 5, scales = "free_y") +
  labs(x = "Year", y = "Effective labor supplied (2015 = 1)",
       color = "scenario") +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(legend.position = "right") -> p2; p2

ggsave("S_Figure_EFL/effective labor relative to 2015.png", p2, width = 18, height = 14)

# plot 10 region labor productivity ----
# scaler --
SCENARIO_eta <- c("para_eta1","para_eta2","para_eta3","para_eta4",
                  "para_eta5","para_eta6","para_evo", "para_static")

EL %>%
  filter(scenario %in% SCENARIO_eta) %>%
  left_join(cluster, by = "region") %>%
  group_by(scenario, REG, year) %>%
  mutate(Labor = sum(labor),
         weight = labor/Labor) %>%
  group_by(scenario,REG, region) %>%
  mutate(weight = weight[year == 2015],
         w.eta = weight*eta) %>%
  group_by(scenario, REG, year) %>%
  summarise(w.eta =sum(w.eta)) %>%
  mutate(experiment = scenario,
         experiment = ifelse(grepl("eta", scenario), "Labor productivity growth", experiment),
         experiment = ifelse(grepl("evo", scenario), "Evolving", experiment),
         experiment = ifelse(grepl("static", scenario), "Static", experiment),
         experiment = ifelse(grepl("gamma", scenario), "Labor supply elasticity", experiment),
         experiment = ifelse(grepl("SSP", scenario), "Rural population", experiment)) %>%
  rename(variable = experiment) %>%
  SCE_NM() %>%
  ggplot() +
  geom_line(aes(x = year, y = w.eta, color = scenario)) +
  # scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  facet_wrap(~ REG, ncol = 5) +
  # facet_wrap(~ REG, ncol = 5, scales = "free_y") +
  labs(x = "Year", y = "Labor productivity (2015 = 1)",
       color = "scenario") +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(legend.position = "right") -> p3; p3

  ggsave("S_Figure_EFL/labor productivity.png", p3, width = 18, height = 14)

