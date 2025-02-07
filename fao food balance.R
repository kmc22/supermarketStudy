library(tidyverse)
library(FAOSTAT)

#fbs_new <- read.csv("input/faostat/FAOSTAT_data_en_1-27-2025 fbs 2010- oils.csv")
#fbs_old <- read.csv("input/faostat/FAOSTAT_data_en_1-27-2025 fbs -2013 oils.csv")
production <- read.csv("input/faostat/FAOSTAT_data_en_1-28-2025 production.csv")

fbs_new_bulk<-get_faostat_bulk(code = "FBS", data_folder = "C:/Users/kc143/Dropbox/collaborativeManuscripts/meijaardErik/supermarketStudy/input/faostat")
fbs_old_bulk<-get_faostat_bulk(code = "FBSH", data_folder = "C:/Users/kc143/Dropbox/collaborativeManuscripts/meijaardErik/supermarketStudy/input/faostat")
#prod_bulk<-get_faostat_bulk(code = "QCL", data_folder = "C:/Users/kc143/Dropbox/collaborativeManuscripts/meijaardErik/supermarketStudy/input/faostat")

fbs_new <- fbs_new_bulk %>%
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

prod_palm <- production %>%
  filter(Item == "Palm oil" | Item == "Oil of palm kernel") %>%
  group_by(Year) %>%
  summarise(palmoils = sum(Value))
  
prod_other <- production %>%
  filter(Item != "Palm oil") %>%
  filter(Item != "Oil of palm kernel") %>%
  group_by(Year) %>%
  summarise(otheroils = sum(Value))

prod_all <- prod_other %>%
  left_join(prod_palm) %>%
  mutate(percentpalm = palmoils/(palmoils+otheroils)*100,
         percentother = otheroils/(palmoils+otheroils)*100) %>%
  pivot_longer(cols = otheroils:percentother,
               values_to = "value")

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

fbs_palm_update = bind_rows(fbs_palm_old,fbs_palm_new)

fbs_named_update = bind_rows(fbs_oils_old,fbs_oils_new)

fbs_other_update = bind_rows(fbs_other_old,fbs_other_new)

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

ggsave(paste("figures/fbs_area.png"),width = 6, height = 4)

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

ggsave(paste("figures/fbs_line.png"),width = 6, height = 4)

prod_all %>%
  filter(name != "percentpalm") %>%
  filter(name != "percentother") %>%
  ggplot(aes(x=Year, y=value, group=name, fill = name)) +
  geom_area()+
  theme_bw()+
  ylab("Global Production (1000 tonnes)") +
  xlab("year")+
  theme(axis.title.x = element_blank())+
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  scale_x_continuous(limits= c(2000,2022))+
  geom_vline(xintercept = 2006,linetype="dashed")

ggsave(paste("figures/prod_area.png"),width = 6, height = 4)

prod_all %>%
  filter(name != "otheroils") %>%
  filter(name != "palmoils") %>%
  ggplot(aes(x=Year, y=value, group=name, color = name)) +
  geom_line()+
  theme_bw()+
  ylab("Global Production (% of vegetable oil)") +
  xlab("year")+
  theme(axis.title.x = element_blank())+
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  scale_x_continuous(limits= c(2000,2022))+
  geom_vline(xintercept = 2006,linetype="dashed")

ggsave(paste("figures/prod_line.png"),width = 6, height = 4)


