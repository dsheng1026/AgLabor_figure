
library(ggplot2)
library(dplyr)
library(ggsci)
# plot Figure S2 ----

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

N <- "10"
cluster <- read.csv("input/ClusterRegion.csv") %>% select(region, REG10_AR6) %>%
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

fig.dir <- "Figure/"

load("FigureS2.rds")

labor_all %>%
  filter(group != "Ag_GCAM") %>%
  REG() %>%
  group_by(REG) %>%
  mutate(Total = sum(value),
         share = 100 * value / Total) ->
  share_sec

share_sec %>%
  mutate(group = gsub("Staples", "Staple crops", group),
         group = gsub("Fish", "Fishery", group)) ->
  df.Fig.S2

df.Fig.S2$group <- factor(df.Fig.S2$group,
                    levels = c("Staple crops", "Oil crops",
                               "Other crops", "Livestock",
                               "Fishery", "Forest"))

df.Fig.S2 %>%
  ggplot(aes(x = group, y = REG)) +
  geom_tile(aes(fill = share)) +
  geom_text(aes(label = round(share, 1))) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "", y = paste0(""), fill = "%") +
  scale_color_npg() +
  ggtitle("") +
  theme_bw() + theme0 + theme_leg ->
  labor.share.10REG; labor.share.10REG

ggsave(filename = paste0(fig.dir, "S_Figure2/labor_breakdown_share_10.png"), labor.share.10REG,
       width = 12, height = 10, dpi = 300, units = "in", device='png')

# data preparation ----

# SHARE <- read.csv("FOR_FISH_share_L.csv") %>%
#   select(-X, -GCAM_region_ID)
#
# USDA <- read.csv("USDA_ag_labor_reg.csv") %>%
#   select(-X, -GCAM_region_ID)
#
# FISH <- USDA %>% left_join(SHARE, by = "region") %>%
#   mutate(value = FISH * labor_ppl / 10^6, # ppl -> mpl
#          group = "Fish", var = "Labor") %>%
#   select(region, group, year, var, value)
#
# labor <- market %>%
#   filter(var == "Labor") %>%
#   filter(year == 2015, scenario == "para_evo",
#          sector != "biomass") %>%  # 0 labor in biomass in 2015, remove the column in plot
#   CLUSTER_SEC() %>%
#   group_by(region, group, year, var) %>%
#   summarise(value = sum(value)) # mpl
#
# labor %>%
#   group_by(region, year, var) %>%
#   summarise(value = sum(value)) %>%
#   mutate(group = "Ag_GCAM") %>%
#   select(names(labor)) %>%
#   bind_rows(labor) %>%
#   bind_rows(FISH %>% filter(year == 2015)) ->
#   labor_all
#
# save(labor_all, file = "FigureS2.rds")
