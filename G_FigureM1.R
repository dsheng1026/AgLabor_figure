library(ggplot2)
library(dplyr)
library(tidyr)
library(ggsci)
library(ggpubr)

# Note: this scripts include Fig.1 ----

fig.dir <- "Figure/"

# load data ----
driver <- readRDS("input/DRIVER.RDS")
metric <- readRDS("input/metrics.RDS")
output <- readRDS("input/output.RDS")
cluster <- read.csv("input/ClusterRegion.csv") %>% select(region, REG10_AR6) %>%
  rename(REG = REG10_AR6)

# remove fishery labor in historical periods ----
SHARE <- read.csv("input/FOR_FISH_share_L.csv") %>%
  select(-X, -GCAM_region_ID)

driver %>%
  filter(var == "ag labor") %>%
  left_join(SHARE, by = "region") %>%
  mutate(value = ifelse(year < 2020, value * (1-FISH), value)) %>%
  select(names(driver)) %>%
  bind_rows(driver %>% filter(var != "ag labor")) ->
  driver


YEAR_1 <- c(1975, 2015, 2020, 2050, 2100)
REG_1 <- c("World", "AFRICA", "CHINA+", "NORTH_AM")

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

# output ----
# drivers ----

output %>%
  spread(source, value) %>%
  mutate(value = ifelse(year < 2020, USDA, GCAM_no_For)) %>%
  select(region, year, value) %>%
  mutate(var = "Output", var1 = "GDP") ->
  out

out %>%
  bind_rows(driver) %>%
  left_join(cluster, by = "region") %>%
  filter(var %in% c("LF" , "rural", "pop", "ag labor", "Output")) %>%
  group_by(REG, var, year) %>%
  summarise(value = sum(value)) %>%
  group_by(REG, var) %>%
  mutate(index = value / value[year == 2015]) %>%
  filter(year %in% c(1975, 2050, 2100)) ->
  df.1

out %>%
  bind_rows(driver) %>%
  left_join(cluster, by = "region") %>%
  filter(var %in% c("LF" , "rural", "pop", "ag labor", "Output")) %>%
  group_by( var, year) %>%
  summarise(value = sum(value)) %>%
  group_by(var) %>%
  mutate(index = value / value[year == 2015]) %>%
  mutate(REG = "World") %>%
  select(names(df.1)) %>%
  bind_rows(df.1) %>%
  filter(year %in% c(1975, 2050, 2100)) ->
  df.1.all

df.1 %>%
  group_by(var, year) %>%
  summarise(MIN = min(index),
            MAX = max(index)) ->
  df.1.range

eta <- driver %>% filter(var == "eta") %>% unique()

driver %>% filter(var == "ag labor") %>%
  select(region, year, LA = value) %>%
  left_join(eta %>% select(region, year, eta = value), by = c("region", "year")) %>%
  left_join(cluster, by = "region") %>%
  mutate(value = LA * eta) %>%
  group_by(REG, year) %>%
  summarise(value = sum(value,na.rm = T),
            LA = sum(LA,na.rm = T)) %>%
  mutate(eta = value / LA) ->
  df_eta

df_eta %>%
  group_by(year) %>%
  summarise(value = sum(value),
            LA = sum(LA)) %>%
  mutate(REG = "World",
         eta = value / LA) %>%
  select(names(df_eta)) %>%
  bind_rows(df_eta) %>%
  mutate(var = "eta") %>%
  filter(year %in% c(1975,  2050, 2100)) %>%
  select(REG, var, year, value, index = eta) ->
  plot.eta

plot.eta %>%
  filter(REG != "World") %>%
  group_by(year) %>%
  summarise(MIN = min(index),
            MAX = max(index)) %>%
  filter(year %in% c(1975, 2050,  2100)) %>%
  mutate(var = "eta") %>%
  select(names(df.1.range)) ->
  eta.range

VARIABLE <- c("pop","Output","eta","ag labor","rural","LF" )

df.1.range %>% bind_rows(eta.range) %>%
  mutate(MIN = if_else(year == 1975, 1/MIN, MIN),
         MAX = if_else(year == 1975, 1/MAX, MAX)) %>%
  mutate(MAX = 100*(MAX - 1), MIN = 100 * (MIN - 1)) %>%
  mutate(year = factor(year, levels = c(1975, 2050, 2100),
                       labels = c("1975 - 2015", "2015 - 2050", "2015 - 2100"))) %>%
  mutate(var = factor(var, levels = c("pop", "LF", "rural", "ag labor", "eta", "Output"),
                      labels = c("Total Population", "Labor Force", "Rural Population", "Agiculture Labor",
                                 "Labor Productivity", "Agricultural Output"))) ->
  df_pointrange

df.1.all %>% bind_rows(plot.eta) %>%
  mutate(index = if_else(year == 1975, 1/index, index)) %>%
  mutate(index = (index - 1)*100) %>%
  mutate(year = factor(year, levels = c(1975, 2050, 2100),
                       labels = c("1975 - 2015", "2015 - 2050", "2015 - 2100"))) %>%
  mutate(var = factor(var, levels = c("pop", "LF", "rural", "ag labor", "eta", "Output"),
                      labels = c("Total Population", "Labor Force", "Rural Population", "Agiculture Labor",
                                 "Labor Productivity", "Agricultural Output"))) %>%
  filter(REG %in% REG_1) ->
  df_points

ggplot() +
  geom_rect(data = df_pointrange,
            aes(#xmin= which(levels(as.factor(year))=="1975 - 2015") -0.5,
              xmin = -Inf,
              xmax= which(levels(as.factor(year))=="1975 - 2015") +0.5,
              ymin=-Inf,ymax=Inf), alpha=0.1, fill="grey90") +
  geom_hline(yintercept = 0, color = "black") +
  geom_linerange(data = df_pointrange,
                 aes(x = year, ymin = MIN, ymax = MAX, group = interaction(year, var), color = var),
                 position = position_dodge(width = 0.75), size = 0.5) +
  geom_point(data = df_points,
             aes(x = year, y = index, group = interaction(year, var), shape = REG, fill = var),
             color = "black", size = 3, alpha = 0.8,
             position = position_dodge(width = 0.75)) +
  labs(x = "Period", y = "Growth Rate", color = "Variable", shape = "Region (Panel D)", fill = "Variable") +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  scale_shape_manual(values = c(22:24, 21)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  theme_bw() + theme0 + theme_leg + #theme_add +
  theme(legend.position = "right") +
  ggtitle("(D) Regional growth rate") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.margin = margin(t = 10, r = 18, b = 10, l = 10)) -> p1; p1

# ggsave(filename = paste0(fig.dir, "M_Figure1/driver.png"), p1,
#        width = 14, height = 16, dpi = 300, units = "in", device='png')

driver %>%
  filter(var %in% c("LF", "rural", "pop", "ag labor")) %>%
  left_join(eta %>% select(region, year, mult = value),
            by = c("region", "year")) %>%
  select(-var1) %>%
  spread(var, value) %>%
  mutate(eff = mult * `ag labor`) ->
  df.driver.lev

library(patchwork)

df.driver.lev %>%
  gather(var, value, mult:eff) %>%
  filter(var != "mult") %>%
  na.omit() %>% filter(var == "ag labor") %>%
  left_join(cluster) %>%
  group_by(REG, year) %>% summarize(value = sum(value)) %>% ungroup() %>%
  mutate(value = value / 1000) %>%
  ggplot() +
  geom_rect(aes(xmin=-Inf, xmax=2015, ymin=-Inf,ymax=Inf), alpha=0.1, fill="grey90") +
  geom_hline(yintercept = 0, color = "black") +
  geom_area(aes(x = year, y = value, fill = REG), stat="identity", color="black", size = 0.2) +
  geom_vline(xintercept = 2015, linetype = 2, size = 0.6, color = "grey50") +
  geom_vline(xintercept = 2003, linetype = 2, color = "red") +
  scale_fill_brewer(palette = "Paired") +
  scale_x_continuous(expand = c(0, 0), breaks = c(1975, 2000, 2015, 2050, 2075, 2100)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1)) +
  theme_bw() + theme0 + theme_leg +
  theme(legend.position = "right") +
  labs(x = "Year", y = "Billion People", fill = "Region (Panels A & B)") +
  ggtitle("(A) Agricultural labor input") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.margin = margin(t = 10, r = 18, b = 10, l = 10)) -> p1; p1

df.driver.lev %>%
  gather(var, value, mult:eff) %>%
  filter(var != "mult") %>%
  na.omit() %>% filter(var == "ag labor") %>%
  left_join(cluster) %>%
  group_by(REG, year) %>% summarize(value = sum(value)) %>% ungroup() %>%
  group_by(REG) %>%
  mutate(value = value - value [year == 2015]) %>%
  ggplot() +
  geom_rect(aes(xmin=-Inf, xmax=2015, ymin=-Inf,ymax=Inf), alpha=0.1, fill="grey90") +
  geom_vline(xintercept = 2015, linetype = 2, size = 0.6, color = "grey50") +
  geom_hline(yintercept = 0, color = "black") +
  #geom_area(aes(x = year, y = value, fill = REG), stat="identity", color="black", size = 0.2) +
  geom_line(aes(x = year, y = value, color = REG), size = 1) +
  geom_vline(xintercept = 2003, linetype = 2, color = "red") +
  scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Paired") +
  scale_x_continuous(expand = c(0, 0), breaks = c(1975, 2000, 2015, 2050, 2075, 2100)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-180, 180), breaks = seq(-150, 150, 50) ) +
  theme_bw() + theme0 + theme_leg +
  theme(legend.position = "none") +
  labs(x = "Year", y = "Million People (2015 = 0)", fill = "Region", color = "Region") +
  ggtitle("(B) Agricultural labor input change (2015 = 0)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.margin = margin(t = 10, r = 18, b = 10, l = 10)) -> p2


p1 + p2 + patchwork::plot_layout(guides = "collect") -> pp1; pp1

df.driver.lev %>%
  gather(var, value, mult:eff) %>%
  filter(var != "mult") %>%
  na.omit() %>% filter(var == "ag labor") %>%
  group_by(region, year) %>% summarize(value = sum(value)) %>%
  filter(year %in% c(2003, 2019)) %>%
  spread(year, value) %>%
  mutate(delta = `2019` - `2003`) -> A


df.driver.lev %>%
  gather(var, value, mult:eff) %>%
  filter(var != "mult") %>%
  na.omit() %>%
  group_by(year, var) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  filter(year >= 1975) %>%
  filter(var != "eff") %>%
  mutate(var = factor(var, levels = c("pop", "LF", "rural", "ag labor", "Effective labor"),
                      labels = c("Total Population", "Labor Force", "Rural Population", "Agiculture Labor", "Effective Labor"))) %>%
  ggplot() +
  geom_rect(aes(xmin=-Inf, xmax=2015, ymin=-Inf,ymax=Inf), alpha=0.1, fill="grey90") +
  geom_vline(xintercept = 2015, linetype = 2, size = 0.6, color = "grey50") +
  geom_line(aes(x = year, y = value/1000, color = var), size = 1.3) +
  labs(x = "Year", y = paste0("Billion People"), color = "Variable") +
  scale_x_continuous(expand = c(0, 0), breaks = c(1975, 2000, 2015, 2050, 2075, 2100)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10)) +
  scale_color_brewer(palette = "Set1") +
  theme_bw() + theme0 + theme_leg +
  theme(legend.position = "right") +
  ggtitle("(C) World population and labor changes") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.margin = margin(t = 10, r = 18, b = 10, l = 10)) -> p3;p3

ggplot() +
  geom_rect(data = df_pointrange,
            aes(#xmin= which(levels(as.factor(year))=="1975 - 2015") -0.5,
              xmin = -Inf,
              xmax= which(levels(as.factor(year))=="1975 - 2015") +0.5,
              ymin=-Inf,ymax=Inf), alpha=0.1, fill="grey90") +
  geom_hline(yintercept = 0, color = "black") +
  geom_linerange(data = df_pointrange,
                 aes(x = year, ymin = MIN, ymax = MAX, group = interaction(year, var), color = var),
                 position = position_dodge(width = 0.75), size = 0.5) +
  geom_point(data = df_points,
             aes(x = year, y = index, group = interaction(year, var), shape = REG, fill = var),
             color = "black", size = 3, alpha = 0.8,
             position = position_dodge(width = 0.75)) +
  labs(x = "Period", y = "Growth Rate (%)", color = "Variable", shape = "Region (Panel D)", fill = "Variable") +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  scale_shape_manual(values = c(22:24, 21)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  theme_bw() + theme0 + theme_leg + #theme_add +
  theme(legend.position = "right") +
  ggtitle("(D) Regional growth rate of key variables") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.margin = margin(t = 10, r = 18, b = 10, l = 10)) -> p4; p4


df_points %>%
  ggplot() +
  geom_line(aes(x = year, y  = value, color = var), size = 1) +
  geom_point(aes(x = year, y  = value, shape = REG), size = 3) +
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c(22:24, 21)) + theme_bw() + theme0 +
  labs(color = "Variable (Panels C & D)",
       shape = "Region (Panel D)")+ theme_leg + theme(legend.position = "right")+
  guides(color = guide_legend(order = 1))-> A;A

library(cowplot)
library(patchwork)
get_legend(A)-> p_leg2
get_legend(p1) -> p_leg1

(p1 + theme(legend.position = "none")) +
  p2 + p_leg1 + patchwork::plot_layout(guides = 'collect', widths = c(1, 1, 0.4)) -> pp1

p3 + theme(legend.position = "none") +
  p4 +
  theme(legend.position = "none") +
  p_leg2 +
  plot_layout(guides = 'collect', widths = c(1, 1, 0.4)) -> pp2

# global Fig1 ----
pp1/pp2 -> pp3; pp3
ggsave(paste0(fig.dir,"M_Figure1/driver_level1.png"), pp3, width = 18, height = 14)

# regional drivers ----
df.driver.lev %>%
  gather(var, value, mult:eff) %>%
  filter(var != "mult") %>%
  na.omit() %>%
  left_join(cluster) %>%
  group_by(year, REG, var) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  filter(year >= 1975) %>%
  filter(var != "eff") %>%
  mutate(var = factor(var, levels = c("pop", "LF", "rural", "ag labor", "Effective labor"),
                      labels = c("Total Population", "Labor Force", "Rural Population", "Agiculture Labor", "Effective Labor"))) %>%
  ggplot() +
  facet_wrap(~REG, nrow = 2, scale = "free_y") +
  geom_rect(aes(xmin=-Inf, xmax=2015, ymin=-Inf,ymax=Inf), alpha=0.1, fill="grey90") +
  geom_vline(xintercept = 2015, linetype = 2, size = 0.6, color = "grey50") +
  #geom_hline(yintercept = 0, color = "black") +

  geom_line(aes(x = year, y = value/1000, color = var), size = 1.3) +
  labs(x = "Year", y = paste0("Billion people"), color = "Variable") +
  scale_x_continuous(expand = c(0, 0), breaks = c(1975, 2000, 2015, 2050, 2075, 2100)) +
  #scale_y_continuous(expand = c(0, 0), limits = c(0, 10)) +
  scale_color_brewer(palette = "Set1") +
  theme_bw() + theme0 + theme_leg +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
        plot.margin = margin(t = 10, r = 18, b = 10, l = 10)) -> p3_reg;p3_reg
ggsave(paste0(fig.dir,"M_Figure1/driver_level1_reg.png"), p3_reg, width = 14, height = 8)

