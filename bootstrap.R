library(readxl)
library(tidyverse)
library(survey)
library(svrep)
library(srvyr)

oleo = 1

if (oleo == 0) {
  append = "nooleo"
} else {
  append = "yesoleo"}

# load data and prepare it
category_totals <- read_excel("input/products/Inclusion_Exclusion  (10_01_25) input.xlsx")

category_totals <- category_totals %>%
  dplyr::rename("supermarket" = "Supermarket",
                "category" = "Category",
                "subcategory" = "Subcategory",
                "products_total" = "Total No. Products",
                "products_sampled" = "No. Sampled Products",
                "exclusion_reason" = "Reason for Exclusion (if applicable)") %>%
  mutate(subcategory = replace(subcategory, subcategory == "N/a", category[subcategory == "N/a"])) %>%
  mutate(products_sampled = as.numeric(products_sampled))

ah<- read_excel("input/products/Supermarket Study (Update 03_02_25).xlsx", sheet = "Albert Heijn") %>%
  mutate(supermarket = "Albert Heijn") %>%
  filter(!is.na(`Product Name`))
sb<- read_excel("input/products/Supermarket Study (Update 03_02_25).xlsx", sheet = "Sainsburys") %>%
  mutate(supermarket = "Sainsburys") %>%
  filter(!is.na(`Product Name`))
ww<- read_excel("input/products/Supermarket Study (Update 03_02_25).xlsx", sheet = "Woolworths") %>%
  mutate(supermarket = "Woolworths") %>%
  filter(!is.na(`Product Name`))

# duplicates_ah <- data.frame(duplicates = ah$`Product Name`[duplicated(ah$`Product Name`)]) %>%
#   mutate(supermarket = "Albert Heijn")
# duplicates_sb <- data.frame(duplicates = sb$`Product Name`[duplicated(sb$`Product Name`)])%>%
#   mutate(supermarket = "Sainsburys")
# duplicates_ww <- data.frame(duplicates = ww$`Product Name`[duplicated(ww$`Product Name`)])%>%
#   mutate(supermarket = "Woolworths")
# 
# duplicates <- bind_rows(duplicates_ah,duplicates_sb,duplicates_ww)
# 
# write.csv(duplicates,"output/duplicates from products lists.csv",row.names = FALSE)

# prepare full data frame
ss <- bind_rows(ah,sb,ww) %>%
  dplyr::rename("category" = "Food Category",
                "product" = "Product Name",
                "Unspecified" = "Unspecified Oils",
                "subcategory" = "Subcategory") %>%
  mutate(across(Palm:`Palm/Coconut Assumption`, ~ ifelse(.x == "Y",1,0))) %>%
  mutate(Any = ifelse(rowSums(across(Palm:Unspecified))>0,1,0)) %>%
  unite(categoryUnique, c("category","supermarket"), remove = FALSE) %>%
  unite(subcategoryUnique,c("subcategory","supermarket"),remove = FALSE) %>%
  mutate(all = "all")

# remove duplicate products
ss<-ss %>%
  distinct(supermarket,product,.keep_all=TRUE)

# correct for oleochemical exclusion (or not)
oleochemicals <- ss %>%
  filter(Oleochemicals == 1) %>%
  group_by(supermarket,category,subcategory) %>%
  count() %>%
  rename("samp_oleo" = "n")

sampled <- ss %>%
  group_by(supermarket,category,subcategory) %>%
  count() %>%
  rename("samp" = "n")

category_totals <- category_totals %>%
  left_join(sampled)

category_totals %>%
   group_by(exclusion_reason) %>%
   summarise(total_products = sum(products_total,na.rm=TRUE),
             sampled_products = sum(samp,na.rm=TRUE))

category_totals <- category_totals %>%
  mutate(categories_sampled = if_else(!is.na(samp),products_total,0),
         categories_all = products_total) %>%
  filter(is.na(exclusion_reason) | exclusion_reason == "Doesn't contain oils")

summary_table <- category_totals %>%
  group_by(supermarket,supercategory,exclusion_reason) %>%
  summarise(total_products = sum(products_total,na.rm=TRUE),
            sampled_products = sum(samp,na.rm=TRUE),
            products_categories_sampled = sum(categories_sampled),
            products_categories_all = sum(categories_all)) %>%
  ungroup()

write.csv(summary_table,"output/summary_table.csv")

oil_totals <- category_totals %>%
  mutate(categories_sampled = if_else(!is.na(samp),products_total,0),
         categories_all = products_total) %>%
  group_by(supermarket,supercategory) %>%
  summarise(products_categories_sampled = sum(categories_sampled),
            products_categories_all = sum(categories_all)) %>%
  ungroup()

ss <- left_join(ss,category_totals,by = join_by(supermarket,category,subcategory))

unmatched<-ss %>%
  filter(is.na(products_sampled)) %>%
  select(supermarket,category,subcategory) %>%
  distinct()

# write.csv(unmatched,"output/unmatched subcategories from product list.csv",row.names = FALSE)

# categorize unspecified as oils
ss$ID <- seq.int(nrow(ss))

ss_u <- ss %>%
  mutate(across(Palm:Cacao, ~if_else(Unspecified == 1, 1, .))) %>%
  select(ID,Palm:Cacao) %>%
  rename_with(~paste0("u_",.),Palm:Cacao)

ss<-left_join(ss,ss_u,by = "ID") %>%
  select(-ID)

# bootstrap
if (oleo == 0) {
  ss <- ss %>%
    filter(Oleochemicals == 0)
}

# bootstrap
mydesign <- ss %>%
    as_survey_design(ids = 1,
                     strata = subcategoryUnique,
                     fpc = products_total)

mydesign_bootstrap <- mydesign %>%
  as_bootstrap_design(replicates = 1000)

myresult <- svyby(~Palm+Sunflower+Soya+Rapeseed+Coconut+Peanut+Olive+Maize+Sesame+Shea+Cacao+Unspecified+Any+u_Palm+u_Sunflower+u_Soya+u_Rapeseed+u_Coconut+u_Peanut+u_Olive+u_Maize+u_Sesame+u_Shea+u_Cacao, # variable to pass to function
                  by = ~all,  # grouping
                  design = mydesign_bootstrap, # design object
                  vartype = "ci", # report variation as confidence interval
                  FUN = svymean,# specify function from survey package, mean here
                  na.rm = TRUE)

myresult_s <- svyby(~Palm+Sunflower+Soya+Rapeseed+Coconut+Peanut+Olive+Maize+Sesame+Shea+Cacao+Unspecified+Any+u_Palm+u_Sunflower+u_Soya+u_Rapeseed+u_Coconut+u_Peanut+u_Olive+u_Maize+u_Sesame+u_Shea+u_Cacao, # variable to pass to function
                    by = ~supermarket,  # grouping
                    design = mydesign_bootstrap, # design object
                    vartype = "ci", # report variation as confidence interval
                    FUN = svymean,# specify function from survey package, mean here
                    na.rm = TRUE)

myresult_c <- svyby(~Palm+Sunflower+Soya+Rapeseed+Coconut+Peanut+Olive+Maize+Sesame+Shea+Cacao+Unspecified+Any+u_Palm+u_Sunflower+u_Soya+u_Rapeseed+u_Coconut+u_Peanut+u_Olive+u_Maize+u_Sesame+u_Shea+u_Cacao, # variable to pass to function
                    by = ~supercategory,  # grouping
                    design = mydesign_bootstrap, # design object
                    vartype = "ci", # report variation as confidence interval
                    FUN = svymean,# specify function from survey package, mean here
                    na.rm = TRUE)

myresult_sc <- svyby(~Palm+Sunflower+Soya+Rapeseed+Coconut+Peanut+Olive+Maize+Sesame+Shea+Cacao+Unspecified+Any+u_Palm+u_Sunflower+u_Soya+u_Rapeseed+u_Coconut+u_Peanut+u_Olive+u_Maize+u_Sesame+u_Shea+u_Cacao, # variable to pass to function
                     by = ~supermarket + supercategory,  # grouping
                     design = mydesign_bootstrap, # design object
                     vartype = "ci", # report variation as confidence interval
                     FUN = svymean,# specify function from survey package, mean here
                     na.rm = TRUE)

# correct for the non-sampled, non oil containing products
myresult_corrected <- oil_totals %>%
  summarise(products_categories_sampled = sum(products_categories_sampled),
            products_categories_all = sum(products_categories_all)) %>%
  mutate(weight_sampled = products_categories_sampled/products_categories_all,
         weight_unsampled = 1-weight_sampled) %>%
  mutate(all = "all") %>%
  left_join(myresult) %>%
  mutate(other = 0) %>%
  summarise(across(Palm:ci_u.u_Cacao, ~ weighted.mean(c(.x,0),c(weight_sampled,weight_unsampled))))%>%
  mutate(all = "all")

myresult_s_corrected <- oil_totals %>%
  group_by(supermarket) %>%
  summarise(products_categories_sampled = sum(products_categories_sampled),
            products_categories_all = sum(products_categories_all)) %>%
  mutate(weight_sampled = products_categories_sampled/products_categories_all,
         weight_unsampled = 1-weight_sampled) %>%
  left_join(myresult_s) %>%
  mutate(other = 0) %>%
  group_by(supermarket) %>%
  summarise(across(Palm:ci_u.u_Cacao, ~ weighted.mean(c(.x,0),c(weight_sampled,weight_unsampled)))) %>%
  mutate(all = "all")

myresult_c_corrected <- oil_totals %>%
  group_by(supercategory) %>%
  summarise(products_categories_sampled = sum(products_categories_sampled),
            products_categories_all = sum(products_categories_all)) %>%
  mutate(weight_sampled = products_categories_sampled/products_categories_all,
         weight_unsampled = 1-weight_sampled) %>%
  left_join(myresult_c) %>%
  mutate(other = 0) %>%
  group_by(supercategory) %>%
  summarise(across(Palm:ci_u.u_Cacao, ~ weighted.mean(c(.x,0),c(weight_sampled,weight_unsampled)))) %>%
  mutate(all = "all")

myresult_sc_corrected <- oil_totals %>%
  mutate(weight_sampled = products_categories_sampled/products_categories_all,
         weight_unsampled = 1-weight_sampled) %>%
  left_join(myresult_sc) %>%
  mutate(other = 0) %>%
  group_by(supermarket,supercategory) %>%
  summarise(across(Palm:ci_u.u_Cacao, ~ weighted.mean(c(.x,0),c(weight_sampled,weight_unsampled)))) %>%
  mutate(all = "all")

myresult_corrected_long<-myresult_corrected %>%
  pivot_longer(cols = Palm:ci_u.u_Cacao,
               names_to = "oil",
               values_to = "proportion") %>%
  mutate(metric = if_else(str_detect(oil, "ci_l"), "ci_l", 
                          ifelse(str_detect(oil,"ci_u"), "ci_u", "mean"))) %>%
  mutate(oil = str_remove_all(oil, "ci_l."),
         oil = str_remove_all(oil, "ci_u.")) %>%
  pivot_wider(names_from = metric,
              values_from = proportion) %>% 
  mutate(unspecified = if_else(str_detect(oil,"u_"),"add_unspecified","measured")) %>%
  mutate(oil = str_remove_all(oil,"u_")) %>%
  dplyr::rename("supermarket" = "all") %>%
  mutate(supercategory = "all")

myresult_s_corrected_long<-myresult_s_corrected %>%
  pivot_longer(cols = Palm:ci_u.u_Cacao,
               names_to = "oil",
               values_to = "proportion") %>%
  mutate(metric = if_else(str_detect(oil, "ci_l"), "ci_l", 
                          ifelse(str_detect(oil,"ci_u"), "ci_u", "mean"))) %>%
  mutate(oil = str_remove_all(oil, "ci_l."),
         oil = str_remove_all(oil, "ci_u.")) %>%
  pivot_wider(names_from = metric,
              values_from = proportion) %>% 
  mutate(unspecified = if_else(str_detect(oil,"u_"),"add_unspecified","measured")) %>%
  mutate(oil = str_remove_all(oil,"u_")) %>%
  mutate(supercategory = "all")

myresult_c_corrected_long<-myresult_c_corrected %>%
  pivot_longer(cols = Palm:ci_u.u_Cacao,
               names_to = "oil",
               values_to = "proportion") %>%
  mutate(metric = if_else(str_detect(oil, "ci_l"), "ci_l", 
                          ifelse(str_detect(oil,"ci_u"), "ci_u", "mean"))) %>%
  mutate(oil = str_remove_all(oil, "ci_l."),
         oil = str_remove_all(oil, "ci_u.")) %>%
  pivot_wider(names_from = metric,
              values_from = proportion) %>% 
  mutate(unspecified = if_else(str_detect(oil,"u_"),"add_unspecified","measured")) %>%
  mutate(oil = str_remove_all(oil,"u_")) %>%
  mutate(supermarket = "all")

myresult_sc_corrected_long<-myresult_sc_corrected %>%
  pivot_longer(cols = Palm:ci_u.u_Cacao,
               names_to = "oil",
               values_to = "proportion") %>%
  mutate(metric = if_else(str_detect(oil, "ci_l"), "ci_l", 
                          ifelse(str_detect(oil,"ci_u"), "ci_u", "mean"))) %>%
  mutate(oil = str_remove_all(oil, "ci_l."),
         oil = str_remove_all(oil, "ci_u.")) %>%
  pivot_wider(names_from = metric,
              values_from = proportion) %>%
  mutate(unspecified = if_else(str_detect(oil,"u_"),"add_unspecified","measured")) %>%
  mutate(oil = str_remove_all(oil,"u_"))

myresult_long <- myresult %>%
  pivot_longer(cols = Palm:ci_u.u_Cacao,
               names_to = "oil",
               values_to = "proportion") %>%
  mutate(metric = if_else(str_detect(oil, "ci_l"), "ci_l", 
                          ifelse(str_detect(oil,"ci_u"), "ci_u", "mean"))) %>%
  mutate(oil = str_remove_all(oil, "ci_l."),
         oil = str_remove_all(oil, "ci_u.")) %>%
  pivot_wider(names_from = metric,
              values_from = proportion) %>% 
  mutate(unspecified = if_else(str_detect(oil,"u_"),"add_unspecified","measured")) %>%
  mutate(oil = str_remove_all(oil,"u_")) %>%
  dplyr::rename("supermarket" = "all") %>%
  mutate(supercategory = "all")

myresult_s_long<-myresult_s %>%
  pivot_longer(cols = Palm:ci_u.u_Cacao,
               names_to = "oil",
               values_to = "proportion") %>%
  mutate(metric = if_else(str_detect(oil, "ci_l"), "ci_l", 
                          ifelse(str_detect(oil,"ci_u"), "ci_u", "mean"))) %>%
  mutate(oil = str_remove_all(oil, "ci_l."),
         oil = str_remove_all(oil, "ci_u.")) %>%
  pivot_wider(names_from = metric,
              values_from = proportion)%>%
  mutate(unspecified = if_else(str_detect(oil,"u_"),"add_unspecified","measured")) %>%
  mutate(oil = str_remove_all(oil,"u_")) %>%
  mutate(supercategory = "all")

myresult_c_long<-myresult_c %>%
  pivot_longer(cols = Palm:ci_u.u_Cacao,
               names_to = "oil",
               values_to = "proportion") %>%
  mutate(metric = if_else(str_detect(oil, "ci_l"), "ci_l", 
                          ifelse(str_detect(oil,"ci_u"), "ci_u", "mean"))) %>%
  mutate(oil = str_remove_all(oil, "ci_l."),
         oil = str_remove_all(oil, "ci_u.")) %>%
  pivot_wider(names_from = metric,
              values_from = proportion)%>%
  mutate(unspecified = if_else(str_detect(oil,"u_"),"add_unspecified","measured")) %>%
  mutate(oil = str_remove_all(oil,"u_")) %>%
  mutate(supermarket = "all")

myresult_sc_long<-myresult_sc %>%
  pivot_longer(cols = Palm:ci_u.u_Cacao,
               names_to = "oil",
               values_to = "proportion") %>%
  mutate(metric = if_else(str_detect(oil, "ci_l"), "ci_l", 
                          ifelse(str_detect(oil,"ci_u"), "ci_u", "mean"))) %>%
  mutate(oil = str_remove_all(oil, "ci_l."),
         oil = str_remove_all(oil, "ci_u.")) %>%
  pivot_wider(names_from = metric,
              values_from = proportion) %>%
  mutate(unspecified = if_else(str_detect(oil,"u_"),"add_unspecified","measured")) %>%
  mutate(oil = str_remove_all(oil,"u_"))


results_long <- bind_rows(myresult_long,myresult_s_long,myresult_c_long,myresult_sc_long)
results_corrected_long <- bind_rows(myresult_corrected_long,myresult_s_corrected_long,myresult_c_corrected_long,myresult_sc_corrected_long)

write.csv(results_long,paste("output/supermarketStudyBootstrap_uncorrected_", append, ".csv",sep = ""),row.names = FALSE)
write.csv(results_corrected_long,paste("output/supermarketStudyBootstrap_corrected_", append, ".csv",sep = ""),row.names = FALSE)
  
# plot CIs

myresult_corrected_long %>%
  filter(unspecified == "measured") %>%
  ggplot(aes(x = oil, y = mean*100)) +
  #geom_hline(yintercept=50)+
  geom_point() +
  coord_flip() +
  geom_errorbar(aes(ymin=ci_l*100, ymax=ci_u*100),    
                width=.25,) +
  ylab("% of products with vegetable oil")+
  theme_bw()+
  theme(axis.title.y = element_blank(),legend.position="none")+
  scale_x_discrete(limits=rev)

ggsave(paste("figures/corrected_overall_", append, ".png",sep = ""),width = 4, height = 4)

myresult_s_corrected_long %>%
  filter(unspecified == "measured") %>%
  ggplot(aes(x = oil, y = mean*100)) +
  #geom_hline(yintercept=50)+
  geom_point() +
  coord_flip() +
  facet_grid(. ~ supermarket) +
  geom_errorbar(aes(ymin=ci_l*100, ymax=ci_u*100),    
                width=.25,) +
  ylab("% of products with vegetable oil")+
  theme_bw()+
  theme(axis.title.y = element_blank(),legend.position="none")+
  scale_x_discrete(limits=rev)

ggsave(paste("figures/corrected_supermarket_", append, ".png",sep = ""),width = 5, height = 4)

myresult_sc_corrected_long %>%
  filter(unspecified == "measured") %>%
  ggplot(aes(x = oil, y = mean*100)) +
  #geom_hline(yintercept=50)+
  geom_point() +
  coord_flip() +
  facet_grid(supermarket ~ supercategory) +
  geom_errorbar(aes(ymin=ci_l*100, ymax=ci_u*100),    
                width=.25,) +
  ylab("% of products with vegetable oil")+
  theme_bw()+
  theme(axis.title.y = element_blank(),legend.position="none")+
  scale_x_discrete(limits=rev)

ggsave(paste("figures/corrected_supermarket_supercategory_", append, ".png",sep = ""),width = 6, height = 4)

myresult_c_corrected_long %>%
  filter(unspecified == "measured") %>%
  ggplot(aes(x = oil, y = mean*100)) +
  #geom_hline(yintercept=50)+
  geom_point() +
  coord_flip() +
  facet_grid(. ~ supercategory) +
  geom_errorbar(aes(ymin=ci_l*100, ymax=ci_u*100),    
                width=.25,) +
  ylab("% of products with vegetable oil")+
  theme_bw()+
  theme(axis.title.y = element_blank(),legend.position="none")+
  scale_x_discrete(limits=rev)

ggsave(paste("figures/corrected_supercategory_", append, ".png",sep = ""),width = 6, height = 4)

myresult_corrected_long %>%
  filter(unspecified == "add_unspecified") %>%
  ggplot(aes(x = oil, y = mean*100)) +
  #geom_hline(yintercept=50)+
  geom_point() +
  coord_flip() +
  geom_errorbar(aes(ymin=ci_l*100, ymax=ci_u*100),    
                width=.25,) +
  ylab("% of products with vegetable oil")+
  theme_bw()+
  theme(axis.title.y = element_blank())+
  scale_x_discrete(limits=rev)

ggsave(paste("figures/corrected_overall_unspecified_", append, ".png",sep = ""),width = 4, height = 4)

myresult_s_corrected_long %>%
  filter(unspecified == "add_unspecified") %>%
  ggplot(aes(x = oil, y = mean*100)) +
  #geom_hline(yintercept=50)+
  geom_point() +
  coord_flip() +
  facet_grid(. ~ supermarket) +
  geom_errorbar(aes(ymin=ci_l*100, ymax=ci_u*100),    
                width=.25,) +
  ylab("% of products with vegetable oil")+
  theme_bw()+
  theme(axis.title.y = element_blank())+
  #ggtitle("corrected for unsampled categories, by supermarket")+
  scale_x_discrete(limits=rev)

ggsave(paste("figures/corrected_supermarket_unspecified_", append, ".png",sep = ""),width = 5, height = 4)

myresult_sc_corrected_long %>%
  filter(unspecified == "add_unspecified") %>%
  ggplot(aes(x = oil, y = mean*100)) +
  #geom_hline(yintercept=50)+
  geom_point() +
  coord_flip() +
  facet_grid(supermarket ~ supercategory) +
  geom_errorbar(aes(ymin=ci_l*100, ymax=ci_u*100),    
                width=.25,) +
  ylab("% of products with vegetable oil")+
  theme_bw()+
  theme(axis.title.y = element_blank())+
  scale_x_discrete(limits=rev)

ggsave(paste("figures/corrected_supermarket_supercategory_unspecified_", append, ".png",sep = ""),width = 6, height = 4)

myresult_c_corrected_long %>%
  filter(unspecified == "add_unspecified") %>%
  ggplot(aes(x = oil, y = mean*100)) +
  #geom_hline(yintercept=50)+
  geom_point() +
  coord_flip() +
  facet_grid(. ~ supercategory) +
  geom_errorbar(aes(ymin=ci_l*100, ymax=ci_u*100),    
                width=.25,) +
  ylab("% of products with vegetable oil")+
  theme_bw()+
  theme(axis.title.y = element_blank())+
  scale_x_discrete(limits=rev)

ggsave(paste("figures/corrected_supercategory_unspecified_", append, ".png",sep = ""),width = 6, height = 4)

citation()

