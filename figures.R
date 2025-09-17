# LOAD REQUIRED PACKAGES
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)

oleo = 1

if (oleo == 0) {
  append = "nooleo"
} else {
  append = "yesoleo"}

results_corrected_long<-read.csv(paste("output/supermarketStudyBootstrap_corrected_", append, ".csv",sep = ""))

# plot CIs

# aggregated across all supermarkets and supercategories
results_corrected_long %>%
  filter(supercategory == "all") %>%
  filter(supermarket == "all") %>%
  filter(unspecified == "measured") %>%
  ggplot(aes(x = reorder(oil,-mean), y = mean*100)) +
  #geom_hline(yintercept=50)+
  geom_point() +
  coord_flip() +
  geom_errorbar(aes(ymin=ci_l*100, ymax=ci_u*100),    
                width=.25,) +
  ylab("% of products with oil crop")+
  theme_bw()+
  theme(axis.title.y = element_blank(),legend.position="none")+
  scale_x_discrete(limits=rev)

ggsave(paste("output/corrected_overall_", append, ".tif",sep = ""),width = 4, height = 4)

# aggregated across categories, by supermarket
results_corrected_long %>%
  filter(supercategory == "all") %>%
  filter(supermarket != "all") %>%
  filter(unspecified == "measured") %>%
  ggplot(aes(x = reorder(oil,-mean), y = mean*100)) +
  #geom_hline(yintercept=50)+
  geom_point() +
  coord_flip() +
  facet_grid(. ~ supermarket) +
  geom_errorbar(aes(ymin=ci_l*100, ymax=ci_u*100),    
                width=.25,) +
  ylab("% of products with oil crop")+
  theme_bw()+
  theme(axis.title.y = element_blank(),legend.position="none")+
  scale_x_discrete(limits=rev)

ggsave(paste("output/corrected_supermarket_", append, ".tif",sep = ""),width = 5, height = 4)

# no aggregation (specific supermarket and supercategory)
results_corrected_long %>%
  filter(supercategory != "all") %>%
  filter(supermarket != "all") %>%
  filter(unspecified == "measured") %>%
  ggplot(aes(x = reorder(oil,-mean), y = mean*100)) +
  #geom_hline(yintercept=50)+
  geom_point() +
  coord_flip() +
  facet_grid(supermarket ~ supercategory) +
  geom_errorbar(aes(ymin=ci_l*100, ymax=ci_u*100),    
                width=.25,) +
  ylab("% of products with oil crop")+
  theme_bw()+
  theme(axis.title.y = element_blank(),legend.position="none")+
  scale_x_discrete(limits=rev)

ggsave(paste("output/corrected_supermarket_supercategory_", append, ".tif",sep = ""),width = 6, height = 4)

# aggregated across supermarkets, by category
results_corrected_long %>%
  filter(supercategory != "all") %>%
  filter(supermarket == "all") %>%
  filter(unspecified == "measured") %>%
  ggplot(aes(x = reorder(oil,-mean), y = mean*100)) +
  #geom_hline(yintercept=50)+
  geom_point() +
  coord_flip() +
  facet_grid(. ~ supercategory) +
  geom_errorbar(aes(ymin=ci_l*100, ymax=ci_u*100),    
                width=.25,) +
  ylab("% of products with oil crop")+
  theme_bw()+
  theme(axis.title.y = element_blank(),legend.position="none")+
  scale_x_discrete(limits=rev)

ggsave(paste("output/corrected_supercategory_", append, ".tif",sep = ""),width = 6, height = 4)

# aggregated across all supermarkets and supercategories
results_corrected_long %>%
  filter(supercategory == "all") %>%
  filter(supermarket == "all") %>%
  filter(unspecified == "add_unspecified") %>%
  ggplot(aes(x = reorder(oil,-mean), y = mean*100)) +
  #geom_hline(yintercept=50)+
  geom_point() +
  coord_flip() +
  geom_errorbar(aes(ymin=ci_l*100, ymax=ci_u*100),    
                width=.25,) +
  ylab("% of products with oil crop")+
  theme_bw()+
  theme(axis.title.y = element_blank())+
  scale_x_discrete(limits=rev)

ggsave(paste("output/corrected_overall_unspecified_", append, ".tif",sep = ""),width = 4, height = 4)

# aggregated across categories, by supermarket
results_corrected_long %>%
  filter(supercategory == "all") %>%
  filter(supermarket != "all")  %>%
  filter(unspecified == "add_unspecified") %>%
  ggplot(aes(x = reorder(oil,-mean), y = mean*100)) +
  #geom_hline(yintercept=50)+
  geom_point() +
  coord_flip() +
  facet_grid(. ~ supermarket) +
  geom_errorbar(aes(ymin=ci_l*100, ymax=ci_u*100),    
                width=.25,) +
  ylab("% of products with oil crop")+
  theme_bw()+
  theme(axis.title.y = element_blank())+
  #ggtitle("corrected for unsampled categories, by supermarket")+
  scale_x_discrete(limits=rev)

ggsave(paste("output/corrected_supermarket_unspecified_", append, ".tif",sep = ""),width = 5, height = 4)

# no aggregation (specific supermarket and supercategory)
results_corrected_long %>%
  filter(supercategory != "all") %>%
  filter(supermarket != "all") %>%
  filter(unspecified == "add_unspecified") %>%
  ggplot(aes(x = reorder(oil,-mean), y = mean*100)) +
  #geom_hline(yintercept=50)+
  geom_point() +
  coord_flip() +
  facet_grid(supermarket ~ supercategory) +
  geom_errorbar(aes(ymin=ci_l*100, ymax=ci_u*100),    
                width=.25,) +
  ylab("% of products with oil crop")+
  theme_bw()+
  theme(axis.title.y = element_blank())+
  scale_x_discrete(limits=rev)

ggsave(paste("output/corrected_supermarket_supercategory_unspecified_", append, ".tif",sep = ""),width = 6, height = 4)

# aggregated across supermarkets, by category
results_corrected_long %>%
  filter(supercategory != "all") %>%
  filter(supermarket == "all")  %>%
  filter(unspecified == "add_unspecified") %>%
  ggplot(aes(x = reorder(oil,-mean), y = mean*100)) +
  #geom_hline(yintercept=50)+
  geom_point() +
  coord_flip() +
  facet_grid(. ~ supercategory) +
  geom_errorbar(aes(ymin=ci_l*100, ymax=ci_u*100),    
                width=.25,) +
  ylab("% of products with oil crop")+
  theme_bw()+
  theme(axis.title.y = element_blank())+
  scale_x_discrete(limits=rev)

ggsave(paste("output/corrected_supercategory_unspecified_", append, ".tif",sep = ""),width = 6, height = 4)

