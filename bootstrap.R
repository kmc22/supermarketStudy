# This script loads supermarket product data, prepares it, and then runs 
# the bootstrap analysis. There is an option to include or exclude oleochemicals.
# Author: Kimberly Carlson

# load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               readxl,
               survey,
               svrep,
               srvyr)

# set whether oleochemcials are included in the analysis (oleo = 1), or not (oleo = 0)
oleo = 1

if (oleo == 0) {
  append = "nooleo"
} else {
  append = "yesoleo"}


# load supermarket category total data
category_totals <- read_excel("input/products/Inclusion_Exclusion  (10_01_25) input.xlsx")

category_totals <- category_totals %>%
  dplyr::rename("supermarket" = "Supermarket",
                "category" = "Category",
                "subcategory" = "Subcategory",
                "products_total" = "Total No. Products",
                "products_sampled" = "No. Sampled Products",
                "exclusion_reason" = "Reason for Exclusion (if applicable)") %>%
  mutate(subcategory = replace(subcategory, subcategory == "N/a", category[subcategory == "N/a"])) %>%
  select(-products_sampled)

# load individual product data
ah <- read_excel("input/products/Supermarket Study (11_02_25 - duplicates restored).xlsx", sheet = "Albert Heijn") %>%
  mutate(supermarket = "Albert Heijn") %>%
  filter(!is.na(`Product Name`))
sb <- read_excel("input/products/Supermarket Study (11_02_25 - duplicates restored).xlsx", sheet = "Sainsburys") %>%
  mutate(supermarket = "Sainsburys") %>%
  filter(!is.na(`Product Name`))
ww <- read_excel("input/products/Supermarket Study (11_02_25 - duplicates restored).xlsx", sheet = "Woolworths") %>%
  mutate(supermarket = "Woolworths") %>%
  filter(!is.na(`Product Name`))

# prepare full data frame
ss <- bind_rows(ah,sb,ww) %>%
  dplyr::rename("category" = "Food Category",
                "product" = "Product Name",
                "Unspecified" = "Unspecified Oils",
                "subcategory" = "Subcategory") %>%
  mutate(across(Palm:`Palm/Coconut Assumption`, ~ ifelse(.x == "Y",1,0))) %>% # convert Y/N to 1/0
  mutate(Any = ifelse(rowSums(across(Palm:Unspecified))>0,1,0)) %>% # add a column to represent any oil product
  unite(categoryUnique, c("category","supermarket"), remove = FALSE) %>% # create combined columns
  unite(subcategoryUnique,c("subcategory","supermarket"),remove = FALSE) %>%
  mutate(all = "all")

# join individual product data to category totals
ss <- left_join(ss,category_totals,by = join_by(supermarket,category,subcategory)) %>%
  select(-exclusion_reason)

# remove unmatched products
ss <- ss %>%
  filter(!is.na(products_total))

# count oleochemicals sampled
ss %>%
  filter(Oleochemicals == 1) %>%
  count()

# assess population and sample size
sampled <- ss %>%
  group_by(supermarket,category,subcategory) %>%
  count() %>%
  rename("samp" = "n")

category_totals <- category_totals %>%
  left_join(sampled)

# calculate the number of products sampled
category_totals %>%
  group_by(exclusion_reason) %>%
  summarise(total_products = sum(products_total,na.rm=TRUE),
            sampled_products = sum(samp,na.rm=TRUE))

# filter category totals to those that have ingredients and were not duplicates
category_totals <- category_totals %>%
  mutate(categories_sampled = if_else(!is.na(samp), products_total, 0), # replace NAs with 0s
         categories_all = products_total) %>%
  filter(is.na(exclusion_reason) | exclusion_reason == "Doesn't contain oils")

# create table summarizing the population and sample
summary_table <- category_totals %>%
  group_by(supermarket,supercategory,exclusion_reason) %>%
  summarise(total_products = sum(products_total,na.rm=TRUE),
            sampled_products = sum(samp,na.rm=TRUE),
            products_categories_sampled = sum(categories_sampled),
            products_categories_all = sum(categories_all)) %>%
  ungroup()

write.csv(summary_table,"output/summary_table.csv")

# create table for later use at the supermarket/supercategory level
oil_totals <- category_totals %>%
  group_by(supermarket, supercategory) %>%
  summarise(products_categories_sampled = sum(categories_sampled),
            products_categories_all = sum(categories_all)) %>%
  ungroup()

# add unique id to each row
ss$ID <- seq.int(nrow(ss))

# create a new set of columns (_u) where unspecified oils are classified as all oils
ss_u <- ss %>%
  mutate(across(Palm:Cacao, ~if_else(Unspecified == 1, 1, .))) %>%
  select(ID, Palm:Cacao) %>%
  rename_with(~paste0("u_", .), Palm:Cacao)

ss <- left_join(ss, ss_u, by = "ID") %>%
  select(-ID)

# filter to only those products without oleochemicals, if relevant
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

# all
myresult <- svyby(~Palm+Sunflower+Soya+Rapeseed+Coconut+Peanut+Olive+Maize+Sesame+Shea+Cacao+Unspecified+Any+u_Palm+u_Sunflower+u_Soya+u_Rapeseed+u_Coconut+u_Peanut+u_Olive+u_Maize+u_Sesame+u_Shea+u_Cacao, # variable to pass to function
                  by = ~all,  # grouping
                  design = mydesign_bootstrap, # design object
                  vartype = "ci", # report variation as confidence interval
                  FUN = svymean,# specify function from survey package, mean here
                  na.rm = TRUE)

# supermarket (s)
myresult_s <- svyby(~Palm+Sunflower+Soya+Rapeseed+Coconut+Peanut+Olive+Maize+Sesame+Shea+Cacao+Unspecified+Any+u_Palm+u_Sunflower+u_Soya+u_Rapeseed+u_Coconut+u_Peanut+u_Olive+u_Maize+u_Sesame+u_Shea+u_Cacao, # variable to pass to function
                    by = ~supermarket,  # grouping
                    design = mydesign_bootstrap, # design object
                    vartype = "ci", # report variation as confidence interval
                    FUN = svymean,# specify function from survey package, mean here
                    na.rm = TRUE)

# supercategory (c)
myresult_c <- svyby(~Palm+Sunflower+Soya+Rapeseed+Coconut+Peanut+Olive+Maize+Sesame+Shea+Cacao+Unspecified+Any+u_Palm+u_Sunflower+u_Soya+u_Rapeseed+u_Coconut+u_Peanut+u_Olive+u_Maize+u_Sesame+u_Shea+u_Cacao, # variable to pass to function
                    by = ~supercategory,  # grouping
                    design = mydesign_bootstrap, # design object
                    vartype = "ci", # report variation as confidence interval
                    FUN = svymean,# specify function from survey package, mean here
                    na.rm = TRUE)

# supermarket and supercategory (sc)
myresult_sc <- svyby(~Palm+Sunflower+Soya+Rapeseed+Coconut+Peanut+Olive+Maize+Sesame+Shea+Cacao+Unspecified+Any+u_Palm+u_Sunflower+u_Soya+u_Rapeseed+u_Coconut+u_Peanut+u_Olive+u_Maize+u_Sesame+u_Shea+u_Cacao, # variable to pass to function
                     by = ~supermarket + supercategory,  # grouping
                     design = mydesign_bootstrap, # design object
                     vartype = "ci", # report variation as confidence interval
                     FUN = svymean,# specify function from survey package, mean here
                     na.rm = TRUE)

# correct for the non-sampled, non oil containing products for each of the bootstrap outputs
# via a weighted mean
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

# rotate the tables so that they are long

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

myresult_corrected_long <- myresult_corrected %>%
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

myresult_s_corrected_long <- myresult_s_corrected %>%
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

myresult_c_corrected_long <- myresult_c_corrected %>%
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

myresult_sc_corrected_long <- myresult_sc_corrected %>%
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

# bind different aggregation levels together
results_long <- bind_rows(myresult_long,
                          myresult_s_long,
                          myresult_c_long,
                          myresult_sc_long)

results_corrected_long <- bind_rows(myresult_corrected_long,
                                    myresult_s_corrected_long,
                                    myresult_c_corrected_long,
                                    myresult_sc_corrected_long)

# save results
write.csv(results_long,paste("output/supermarketStudyBootstrap_uncorrected_", append, ".csv",sep = ""),row.names = FALSE)
write.csv(results_corrected_long,paste("output/supermarketStudyBootstrap_corrected_", append, ".csv",sep = ""),row.names = FALSE)
