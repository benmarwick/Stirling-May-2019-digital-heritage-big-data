names(page_data_for_all_pages$result)

#--------------------------------------------------------------------------
# take a look at the distribution of page variables
some_page_variables <- 
  page_data_for_all_pages$result %>% 
  select(page_wordcount, 
         page_wikilinks_out,
         page_wikilinks_in,
         page_cited_items_on) %>% 
  mutate(page_wikilinks_out_norm = page_wikilinks_out / page_wordcount,
         page_cited_items_on_norm = page_cited_items_on / page_wordcount) 
  
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
pages_umap_input <- 
  some_page_variables  %>% 
  mutate(page_wordcount_scaled = scale(page_wordcount),
         page_wikilinks_in_scaled = scale(page_wikilinks_in)) %>% 
  select(-page_wordcount,
         -page_cited_items_on, 
         -page_wikilinks_out,
         -page_wikilinks_in) %>% 
  bind_cols(., page_data_for_all_pages$result[ , 'country'] ) %>% 
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
       n_neighbors = 50, 
       min_dist = 0.9,
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
  geom_point(size = 3,
             aes(colour = factor(hdbscan_out$cluster))) +
  scale_color_viridis_d(guide = FALSE) +
  theme_minimal() +
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
                    xmin = -10.0, 
                    xmax = -2, 
                    ymin=-7.5, 
                    ymax=-3.5) 




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
  tibble(revision_history_page_details = map(page_data_for_all_pages$result$page_info_t, 
      ~.x$revision_history_page_details)) %>% 
  mutate(Site =         page_data_for_all_pages$result$Site,
         rh_n_editors = map_int(revision_history_page_details, ~n_distinct(.x$rh_user)),
         rh_n_edits =   map_int(revision_history_page_details, ~nrow(.x)),
         rh_user_simpson_idx = page_data_for_all_pages$result$rh_user_simpson_idx,
         rh_user_bot_prop = page_data_for_all_pages$result$rh_user_bot_prop,
         rh_revert_prop = page_data_for_all_pages$result$rh_revert_prop)

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
