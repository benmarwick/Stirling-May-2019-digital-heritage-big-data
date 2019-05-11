library(tidyverse)

page_data_for_all_pages <- readRDS("data/page_data_for_all_pages.rds")

names(page_data_for_all_pages)

normalize = function(x) (x-min(x))/(max(x)-min(x))

#--------------------------------------------------------------------------
# take a look at the distribution of page variables
some_page_variables <- 
  page_data_for_all_pages %>% 
  select(page_wordcount, 
         page_wikilinks_out,
         page_wikilinks_in,
         page_cited_items_on) %>% 
  mutate(page_wikilinks_out_per_word = page_wikilinks_out / page_wordcount,
         page_cited_items_on_per_word = page_cited_items_on / page_wordcount) 
  
some_page_variables %>% 
  gather(variable, value) %>% 
ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap( ~ variable, 
              scales = "free") +
  scale_x_log10() +
  theme_minimal()

library(GGally)
ggpairs( some_page_variables %>% 
           mutate_all(log)) +
  theme_minimal()

# umap
library(uwot)
library("tmap")
data("World")

pages_umap_input <- 
  some_page_variables  %>% 
  mutate(page_wordcount_norm = normalize(page_wordcount),
         page_wikilinks_in_norm= normalize(page_wikilinks_in),
         page_wikilinks_out_per_word_norm = normalize(page_wikilinks_out_per_word),
         page_cited_items_on_per_word_norm = normalize(page_cited_items_on_per_word)) %>% 
  select(-page_wordcount,
         -page_cited_items_on, 
         -page_wikilinks_out,
         -page_wikilinks_in,
         -page_wikilinks_out_per_word,
         -page_cited_items_on_per_word) %>% 
  bind_cols(., page_data_for_all_pages[ , 'country'] ) %>% 
  filter_all(all_vars(!is.na(.))) %>% 
  left_join(World %>% 
              select(name, continent), 
            by = c('country' = 'name'))

# compute umap

pages_umap_input_selected <- 
  pages_umap_input %>% 
  select(-country, 
         -continent,
         -geometry
         )  
    
pages_umap_output <- 
  pages_umap_input_selected %>% 
  umap(., 
       n_neighbors = 60, 
       min_dist = 0.7,
       nn_method = "annoy",
       init = "spca") %>% 
  as_tibble()

# compute hdbscan clusters
library(dbscan)
hdbscan_out <- hdbscan(pages_umap_output, 
                       minPts = 5)

table(hdbscan_out$cluster)

main_plot <- 
ggplot(pages_umap_output,
       aes(V1, V2)) +
  geom_point(
    aes(colour = factor(hdbscan_out$cluster),
        size =  pages_umap_input_selected$page_wordcount_norm  ,
        shape = pages_umap_input$continent
        )) +
  scale_size(range = c(2,7), guide = FALSE) +
  scale_shape(name = "Continent") +
  scale_color_viridis_d(guide = FALSE) +
  theme_minimal() +
  theme(legend.position = c(0.1, 0.8)) +
  xlab("") +
  ylab("") 

main_plot

# train a feature-selecting classificator like random forests on 
# the cluster labels

rand_forest_input <- 
  pages_umap_input_selected %>% 
  mutate(clus = hdbscan_out$cluster) %>% 
  filter(clus != 0)

library(caret)

fit <- train(
  clus ~ .,
  data = rand_forest_input,
  method = "ranger",
  trControl = trainControl(method="cv", 
                            number = 10, 
                            allowParallel = TRUE, 
                            verbose = TRUE),
  importance = 'permutation')

fit
var_imp_tbl <- tibble(var = row.names(varImp(fit)$importance),
                      imp = varImp(fit)$importance$Overall)

theme_nogrid <- function (base_size = 12, base_family = "") {
  theme_bw(base_size = base_size, 
           base_family = base_family) %+replace% 
    theme(panel.grid = element_blank() )   
}

sub_plot <- 
ggplot(var_imp_tbl,
       aes(reorder( var, -imp ),
           imp)) +
  geom_col() +
  coord_flip() +
  xlab("") +
  ylab("") +
  theme_nogrid(base_size = 6)

# plot plus subplot
main_plot + 
  annotation_custom(ggplotGrob(sub_plot), 
                    xmin = 3, 
                    xmax = 6, 
                    ymin= 6.5, 
                    ymax= 3.5) 




pca_out <- prcomp(pages_umap_input_selected)

pca_out_df <- tibble(pc1 = pca_out$x[ , 1],
                     pc2 = pca_out$x[ , 2],
                     pc3 = pca_out$x[ , 3],
                     clus = hdbscan_out$cluster)

ggplot(pca_out_df,
       aes(pc1, 
           pc3,
           colour = factor(clus))) +
  geom_point() +
  scale_color_viridis_d()


#-------------------------------------------------------------------
# edit variables

revision_history_page_details <- 
  tibble(revision_history_page_details = map(page_data_for_all_pages$page_info_t, 
      ~.x$revision_history_page_details)) %>% 
  mutate(Site =         page_data_for_all_pages$Site,
         rh_n_editors = map_int(revision_history_page_details, ~n_distinct(.x$rh_user)),
         rh_n_edits =   map_int(revision_history_page_details, ~nrow(.x)),
         rh_user_simpson_idx = page_data_for_all_pages$rh_user_simpson_idx,
         rh_user_bot_prop = page_data_for_all_pages$rh_user_bot_prop,
         rh_revert_prop = page_data_for_all_pages$rh_revert_prop)

ggplot(revision_history_page_details,
       aes(rh_n_edits)) +
  geom_histogram()

revision_history_page_details_long <- 
revision_history_page_details %>% 
  select_if(is.numeric) %>% 
  gather(variable, value)

ggplot(revision_history_page_details_long,
       aes(value)) +
  geom_histogram() +
  facet_wrap( ~ variable, 
              scales = "free") +
  scale_x_log10() +
  theme_minimal()

library(GGally)
ggpairs( revision_history_page_details %>% 
           select(rh_user_simpson_idx, 
                  rh_user_bot_prop,
                  rh_revert_prop)) 


pages_umap_input_rh <- 
  revision_history_page_details  %>% 
  mutate(rh_n_editors_norm = normalize(rh_n_editors),
         rh_n_edits_norm = normalize(rh_n_edits)) %>% 
  select(-revision_history_page_details,
         -rh_n_editors,
         -rh_n_edits,
         -Site) %>% 
  bind_cols(., page_data_for_all_pages[ , 'country'] ) %>% 
  filter_all(all_vars(!is.na(.))) %>% 
  left_join(World %>% 
              select(name, continent), 
            by = c('country' = 'name'))

# compute umap

rh_pages_umap_input_selected <- 
  pages_umap_input_rh %>% 
  select(-country, 
         -continent,
         -geometry
  )  

rh_pages_umap_output <- 
  rh_pages_umap_input_selected %>% 
  umap(., 
       n_neighbors = 50, 
       min_dist = 0.9,
       nn_method = "annoy",
       init = "spca") %>% 
  as_tibble()

# compute hdbscan clusters
library(dbscan)
rh_hdbscan_out <- hdbscan(rh_pages_umap_output, 
                       minPts = 5)

table(rh_hdbscan_out$cluster)

rh_main_plot <- 
  ggplot(rh_pages_umap_output,
         aes(V1, V2)) +
  geom_point(
      aes(colour = factor(rh_hdbscan_out$cluster),
          size = rh_pages_umap_input_selected$rh_n_editors_norm,
          shape = pages_umap_input_rh$continent)) +
  scale_size(range = c(2,7), guide = FALSE) +
  scale_shape(name = "Continent") +
  scale_color_viridis_d(guide = FALSE) +
  theme_minimal() +
  theme(legend.position = c(0.2, 0.8)) +
  xlab("") +
  ylab("") 

rh_main_plot

# train a feature-selecting classificator like random forests on 
# the cluster labels

rh_rand_forest_input <- 
  rh_pages_umap_input_selected %>% 
  mutate(clus = rh_hdbscan_out$cluster) %>% 
  filter(clus != 0)

library(caret)

rh_fit <- train(
  clus ~ .,
  data = rh_rand_forest_input,
  method = "ranger",
  trControl = trainControl(method="cv", 
                           number = 10, 
                           allowParallel = TRUE, 
                           verbose = TRUE),
  importance = 'permutation')

rh_fit
rh_var_imp_tbl <- tibble(var = row.names(varImp(rh_fit)$importance),
                      imp = varImp(rh_fit)$importance$Overall)

theme_nogrid <- function (base_size = 12, base_family = "") {
  theme_bw(base_size = base_size, 
           base_family = base_family) %+replace% 
    theme(panel.grid = element_blank() )   
}

rh_sub_plot <- 
  ggplot(rh_var_imp_tbl,
         aes(reorder( var, -imp ),
             imp)) +
  geom_col() +
  coord_flip() +
  xlab("") +
  ylab("") +
  theme_nogrid(base_size = 6)

# plot plus subplot
rh_main_plot + 
  annotation_custom(ggplotGrob(rh_sub_plot), 
                    xmin = -10.0, 
                    xmax = -2, 
                    ymin=-7.5, 
                    ymax=-3.5) 


