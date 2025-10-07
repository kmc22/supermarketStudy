# This script pulls data from faostat, filters it to regions and crops of interest,
# and then summarises temporal trends including visualizing them
# Author: Kimberly Carlson

# load required packages
if (!require("pacman")) install.packages("pacman", type="binary", dependencies = TRUE)

pacman::p_load(tidyverse,
               FAOSTAT)

# pull data from FAOStat and save to disk
fbs_new_bulk <- get_faostat_bulk(code = "FBS", data_folder = "output") # food balance sheets
fbs_old_bulk <- get_faostat_bulk(code = "FBSH", data_folder = "output") # food balance sheets
prod_bulk <- get_faostat_bulk(code = "QCL", data_folder = "output") # crop and livestock production

# optionally save to disk
#saveRDS(fbs_new_bulk, "output/fbs_all_data.rds")
#saveRDS(fbs_old_bulk, "output/fbsh_all_data.rds")
#saveRDS(prod_bulk, "output/qcl_all_data.rds")

# if data have already been saved to disk, read them in
#fbs_new_bulk <- readRDS("output/fbs_all_data.rds")
#fbs_old_bulk <- readRDS("output/fbsh_all_data.rds")
#prod_bulk <- readRDS("output/qcl_all_data.rds")

# get production data from 2000-2022 at a global level for oil crops
production <- prod_bulk %>%
  filter(area == "World") %>%
  filter(element == "production") %>%
  filter(year >= 2000) %>%
  filter(year <= 2022) %>%
  filter(item == "Soya bean oil" |
           item == "Groundnut oil" |
           item == "Sunflower-seed oil, crude" |
           item == "Rapeseed or canola oil, crude" |
           item == "Oil of palm kernel" |
           item == "Palm oil" |
           item == "Coconut oil" |
           item == "Oil of sesame seed" |
           item == "Olive oil" |
           item == "Oil of maize")

# get (new) food balance sheet data for regions of interest and world, for oil crops,
# through 2022
fbs_new <- fbs_new_bulk %>%
  filter(year <= 2022) %>%
  filter(area == "Australia" | 
           area == "United Kingdom of Great Britain and Northern Ireland" |
           area == "Netherlands (Kingdom of the)" |
           area == "World") %>%
  filter(element == "domestic_supply_quantity") %>%
  filter(item == "Soyabean Oil" |
           item == "Groundnut Oil" |
           item == "Sunflowerseed Oil" |
           item == "Rape and Mustard Oil" |
           item == "Palmkernel Oil" |
           item == "Palm Oil" |
           item == "Coconut Oil" |
           item == "Sesameseed Oil" |
           item == "Olive Oil" |
           item == "Maize Germ Oil" |
           item == "Oilcrops Oil, Other")

# get (old) food balance sheet data for regions of interest and world, for oil crops,
# from 2000 to 2009
fbs_old <- fbs_old_bulk %>%
  filter(area == "Australia" | 
           area == "United Kingdom of Great Britain and Northern Ireland" |
           area == "Netherlands (Kingdom of the)" |
           area == "World") %>%
  filter(element == "domestic_supply_quantity") %>%
  filter(item == "Soyabean Oil" |
           item == "Groundnut Oil" |
           item == "Sunflowerseed Oil" |
           item == "Rape and Mustard Oil" |
           item == "Palmkernel Oil" |
           item == "Palm Oil" |
           item == "Coconut Oil" |
           item == "Sesameseed Oil" |
           item == "Olive Oil" |
           item == "Maize Germ Oil" |
           item == "Oilcrops Oil, Other") %>%
  filter(year < 2010) %>%
  filter(year > 1999)

# get palm oil + palm kernel oil production globally by year
prod_palm <- production %>%
  filter(item == "Palm oil" | item == "Oil of palm kernel") %>%
  group_by(year) %>%
  summarise(palmoils = sum(value))

# get other oil crop production globally by year
prod_other <- production %>%
  filter(item != "Palm oil") %>%
  filter(item != "Oil of palm kernel") %>%
  group_by(year) %>%
  summarise(otheroils = sum(value))

# join tables to get total production of palm and other oils by year globally
prod_all <- prod_other %>%
  left_join(prod_palm) %>%
  mutate(percentpalm = palmoils/(palmoils+otheroils)*100,
         percentother = otheroils/(palmoils+otheroils)*100) %>%
  pivot_longer(cols = otheroils:percentother,
               values_to = "value")

# summarise domestic supply by year, for named oil crops, other oil crops, and palm
fbs_oils_new <- fbs_new %>%
  filter(item != "Palm Oil") %>%
  filter(item != "Palmkernel Oil") %>%
  filter(item != "Oilcrops Oil, Other") %>%
  group_by(area,year) %>%
  summarise(`Other Named Crop` = sum(value)) %>%
  mutate(dataset = "new")

fbs_other_new <- fbs_new %>%
  filter(item == "Oilcrops Oil, Other") %>%
  group_by(area,year) %>%
  summarise(Uncategorized = sum(value)) %>%
  mutate(dataset = "new")

fbs_palm_new <- fbs_new %>%
  filter(item == "Palm Oil" | item == "Palmkernel Oil") %>%
  group_by(area,year) %>%
  summarise(Palm = sum(value)) %>%
  mutate(dataset = "new")

fbs_oils_old <- fbs_old %>%
  filter(item != "Palm Oil") %>%
  filter(item != "Palmkernel Oil") %>%
  filter(item != "Oilcrops Oil, Other") %>%
  group_by(area,year) %>%
  summarise(`Other Named Crop` = sum(value))%>%
  mutate(dataset = "old")

fbs_other_old <- fbs_old %>%
  filter(item == "Oilcrops Oil, Other") %>%
  group_by(area,year) %>%
  summarise(Uncategorized = sum(value)) %>%
  mutate(dataset = "old")

fbs_palm_old <- fbs_old %>%
  filter(item == "Palm Oil" | item == "Palmkernel Oil") %>%
  group_by(area,year) %>%
  summarise(Palm = sum(value))%>%
  mutate(dataset = "old")

# combine these aggregated data
fbs_palm_update = bind_rows(fbs_palm_old,fbs_palm_new)

fbs_named_update = bind_rows(fbs_oils_old,fbs_oils_new)

fbs_other_update = bind_rows(fbs_other_old,fbs_other_new)

# calculate the contribution of each group to domestic supply, by year and location
fbs_all <- fbs_named_update %>%
  left_join(fbs_palm_update) %>%
  left_join(fbs_other_update) %>%
  select(-dataset) %>%
  mutate(palmpercent = Palm/(Palm+`Other Named Crop`+Uncategorized)*100,
         namedpercent = `Other Named Crop`/(Palm+`Other Named Crop`+Uncategorized)*100,
         uncategorizedpercent = Uncategorized/(Palm+`Other Named Crop`+Uncategorized)*100) %>%
  pivot_longer(cols = `Other Named Crop`:uncategorizedpercent,
               values_to = "value") %>%
  mutate(area = replace(area, area == "Netherlands (Kingdom of the)", "Netherlands")) %>%
  mutate(area = replace(area, area == "United Kingdom of Great Britain and Northern Ireland", "UK")) 

# visualize data
fbs_all %>%
  filter(name != "palmpercent") %>%
  filter(name != "uncategorizedpercent") %>%
  filter(name != "namedpercent") %>%
  ggplot(aes(x=year, y=value, group=name, fill = name)) +
  geom_area()+
  theme_bw()+
  facet_grid(area ~ .,scales = "free") +
  ylab("Domestic Supply (1000 tonnes)") +
  xlab("year") +
  theme(axis.title.x = element_blank())+
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  scale_x_continuous(limits= c(2000,2022)) +
  geom_vline(xintercept = 2006,linetype="dashed") +
  geom_vline(xintercept = 2010,color = "grey")

ggsave(paste("output/fbs_area.tif"),width = 6, height = 4)

fbs_all %>%
  filter(name != "Other Named Crop") %>%
  filter(name != "Palm") %>%
  filter(name != "Uncategorized") %>%
  ggplot(aes(x=year, y=value, group=name, color = name)) +
  geom_line()+
  theme_bw()+
  facet_grid(area ~ .)+
  ylab("Domestic Supply (% of vegetable oil)") +
  xlab("year")+
  theme(axis.title.x = element_blank())+
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  scale_x_continuous(limits= c(2000,2022))+
  geom_vline(xintercept = 2006,linetype="dashed")+
  geom_vline(xintercept = 2010,color = "grey")

ggsave(paste("output/fbs_line.tif"),width = 6, height = 4)

prod_all %>%
  filter(name != "percentpalm") %>%
  filter(name != "percentother") %>%
  ggplot(aes(x=year, y=value, group=name, fill = name)) +
  geom_area()+
  theme_bw()+
  ylab("Global Production (1000 tonnes)") +
  xlab("year")+
  theme(axis.title.x = element_blank())+
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  scale_x_continuous(limits= c(2000,2022))+
  geom_vline(xintercept = 2006,linetype="dashed")

ggsave(paste("output/prod_area.tif"),width = 6, height = 4)

prod_all %>%
  filter(name != "otheroils") %>%
  filter(name != "palmoils") %>%
  ggplot(aes(x=year, y=value, group=name, color = name)) +
  geom_line()+
  theme_bw()+
  ylab("Global Production (% of vegetable oil)") +
  xlab("year")+
  theme(axis.title.x = element_blank())+
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  scale_x_continuous(limits= c(2000,2022))+
  geom_vline(xintercept = 2006,linetype="dashed")

ggsave(paste("output/prod_line.tif"),width = 6, height = 4)