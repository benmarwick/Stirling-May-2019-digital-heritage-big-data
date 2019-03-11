library(tidyverse)
library('rvest')
library(vegan)

# gets a random WP page
rando <- "https://en.wikipedia.org/wiki/Special:Random"

# gets data about n WP pages
n <- 100000
random_page_data <- rerun(n, get_data_about_page_safe(rando) )

# drops the pages that returned an error
random_page_data_lst <- transpose(random_page_data)[["result"]]

random_page_data_lst <- discard(random_page_data_lst, ~.x[1] == "some_problem")

# put page data into a table
random_page_data_tbl <- 
  tibble(page_wordcount =      map_int(random_page_data_lst, ~.x$page_wordcount),
         page_wikilinks_out =  map_int(random_page_data_lst, ~.x$page_wikilinks_out),
         page_wikilinks_in =   map_int(random_page_data_lst, ~.x$page_wikilinks_in),
         page_cited_items_on = map_int(random_page_data_lst, ~.x$page_cited_items_on),
         rh_user_simpson_idx = map_dbl(random_page_data_lst, ~.x$rh_user_simpson_idx),
         rh_user_bot_prop =    map_dbl(random_page_data_lst, ~.x$rh_user_bot_prop), 
         rh_revert_prop =      map_dbl(random_page_data_lst, ~.x$rh_revert_prop), 
         talk_page_wordcount = map_dbl(random_page_data_lst, ~.x$talk_page_wordcount),
         page_views_last_n_days_total = map_int(random_page_data_lst, ~.x$page_views_last_n_days_total)
  )

random_page_data_tbl %>% 
  gather(variable, value) %>% 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap( ~ variable, scales = "free")

