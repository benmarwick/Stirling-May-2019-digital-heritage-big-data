# list of lists
url <- "https://en.wikipedia.org/wiki/Lists_of_World_Heritage_Sites"

# Table of sites per country can be found at each of these pages
# probably the simplest entry point

# Africa
africa <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_Sites_in_Africa"

# Americas
n_america <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_Sites_in_North_America"
c_america <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_Sites_in_Central_America"
caribbean <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_Sites_in_the_Caribbean"
s_america <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_Sites_in_South_America"

# Asia
n_and_c_asia <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_Sites_in_Northern_and_Central_Asia"
w_asia <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_Sites_in_Western_Asia"
e_asia <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_sites_in_Eastern_Asia"
s_asia <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_sites_in_Southern_Asia"
# SE Asia list currently removed due to copyvio :(
se_asia <- "https://en.wikipedia.org/w/index.php?title=List_of_World_Heritage_Sites_in_Southeast_Asia&direction=prev&oldid=878049179"

# Europe
n_europe <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_Sites_in_Northern_Europe"
w_europe <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_sites_in_Western_Europe"
e_europe <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_sites_in_Eastern_Europe"
s_europe <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_Sites_in_Southern_Europe"

# Oceania
oceania <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_Sites_in_Oceania"

#-----------------------------------------------------

# Get the table of WH locations from each website listed above
library('rvest')
library('tidyverse')
library("parzer") # devtools::install_github("ropenscilabs/parzer")
library("tmap")
data("World")
library(vegan)

#-----------------------------------------------------
# get table of sites per region

get_table_of_sites_per_region <- function(region){
  
  # here are all tables on the page
  region_table_xml_all_tables <- 
    read_html(region) %>% 
    html_nodes("table") 
  
  # we need to get the xml of the table of sites, since we will use the XML later
if(region != se_asia){
  
which_table_has_the_sites <- 
   map(region_table_xml_all_tables, 
       ~html_table(.x, fill = TRUE))  %>% 
   map_lgl(~names(.x)[1] == "Site") 
  
  region_table_xml <- 
    keep(region_table_xml_all_tables, 
         which_table_has_the_sites)
  
  # here is the table of sites for that region
  # first column is called "Site"
  region_table_xml_table <- 
    map(region_table_xml, 
        html_table, 
        fill = TRUE) %>% 
    keep(~names(.x)[1] == "Site") %>% 
    .[[1]] %>% 
    as_tibble(., .name_repair = "unique")
  
} else {
  
  # the SE Asia table is troublesome 
  region_table_xml <- region_table_xml_all_tables[3]
  region_table_xml_table <- html_table(region_table_xml)[[1]]
}
  
  # filter so we only keep the cultural sites
  region_tbl <- 
    region_table_xml_table %>% 
    filter(str_detect(Criteria, 'Cultural')) %>% 
    mutate(Year_num = ifelse(is.integer(Year), Year, parse_number(Year))) 
  
  # scrape out the coords for mapping
  coords_chars <- "\\d|°|′|″|N|E|S|W|\\."
  region_coords <- 
    map_chr(str_extract_all(region_tbl$Location, 
                            coords_chars), 
            ~paste0(.x, collapse = "") %>% 
              str_replace_all(., "^[A-Z]*", "") %>% 
              str_extract(., ".+?(?=E|W)"))
  
  region_coords_clean <- 
    region_coords %>% 
    str_split("N|S") %>% 
    Reduce(rbind, .) %>% 
    as_tibble(., .name_repair = "universal") %>% 
    mutate(lat = parse_lat(`...1`),
           lon = parse_lon(`...2`))
  
  # attach clean coords to main table
  region_tbl_coords <- 
    region_tbl %>% 
    bind_cols(region_coords_clean)
  
  # get country of site from location text
  # Laos, Czech Republic, Micronesia, Zimbabwe, South Sudan, Chad, 
  # Central African Republic, Congo, Gabon, Cameroon, Nigeria, Bosnia and Herzegovina
  #  Cote d'Ivoire, Sierra Leone, Guyana, Belize, 
  country_names <-  paste(World$name, collapse="|")
  region_tbl_coords <- 
    region_tbl_coords %>% 
    mutate(country = str_extract(Location, 
                                 regex(country_names, 
                                       ignore.case=TRUE)))
  
  # get links to each site's wiki page 
  get_link <- function(html_table, Site){
    html_table %>% 
      html_nodes(xpath=paste0("//a[text()='", Site, "']")) %>% 
      html_attr("href")
  }
  
  region_tbl_coords_links <- 
    region_tbl_coords %>% 
    mutate(site_page_name = map_chr(Site, 
                                    ~get_link(region_table_xml, .x)[1])) %>% 
    mutate(site_page_link = as.character(str_glue('https://en.wikipedia.org{site_page_name}')))
  
  return(region_tbl_coords_links)
}
#-----------------------------------------------------


# get the wikilink to each page for each WH site in each region
tbl_africa  <-        map_df(africa ,      get_table_of_sites_per_region)

tbl_n_america  <-     map_df(n_america ,   get_table_of_sites_per_region)
tbl_c_america <-      map_df(c_america,   get_table_of_sites_per_region)
tbl_caribbean  <-     map_df(caribbean ,   get_table_of_sites_per_region) 
tbl_s_america  <-     map_df(s_america ,   get_table_of_sites_per_region)

tbl_n_and_c_asia  <-  map_df(n_and_c_asia,   get_table_of_sites_per_region) 
tbl_w_asia  <-        map_df(w_asia ,   get_table_of_sites_per_region) 
tbl_e_asia  <-        map_df(e_asia ,   get_table_of_sites_per_region) 
tbl_s_asia  <-        map_df(s_asia ,   get_table_of_sites_per_region) 

# special handling needed
tbl_se_asia  <-       map_df(se_asia ,   get_table_of_sites_per_region) 

tbl_n_europe <-       map_df(n_europe,   get_table_of_sites_per_region) 
tbl_w_europe <-       map_df(w_europe,   get_table_of_sites_per_region) 
tbl_e_europe <-       map_df(e_europe,   get_table_of_sites_per_region) 
tbl_s_europe <-       map_df(s_europe,   get_table_of_sites_per_region) 
tbl_russia <-         tbl_russia # has it's own weird page

tbl_oceania <-        map_df(oceania,    get_table_of_sites_per_region)

# Put them all into one big data frame
cols_we_want <-
  c(
    "Site"     ,
    "Location"     ,
    "Criteria"       ,
    "Areaha (acre)" ,
    "Year_num"    ,
    "lat"     ,
    "lon"          ,
    "country"      ,
    "site_page_name" ,
    "site_page_link"
  )

all_regions <- list(
  tbl_africa       ,
  tbl_n_america    ,
  tbl_c_america    ,
  tbl_caribbean    ,
  tbl_s_america    ,
  tbl_n_and_c_asia  ,
  tbl_w_asia       ,
  tbl_e_asia       ,
  tbl_s_asia       ,
  tbl_se_asia      ,
  tbl_n_europe     ,
  tbl_w_europe     ,
  tbl_e_europe     ,
  tbl_s_europe     ,
  tbl_russia,
  tbl_oceania
)

wh_wiki_table <- map_df(all_regions, ~select(., cols_we_want)) 
write_csv(wh_wiki_table, 'data/wh_wiki_table.csv')

# nothing from Russia or the UK

# sites that we have wikipages for
all_regions_cols_we_want_with_pages <- 
  wh_wiki_table %>% 
  filter(!is.na(site_page_name))

# sites that we do not have wikipages for
all_regions_cols_we_want_without_pages <- 
  wh_wiki_table %>% 
  filter(is.na(site_page_name))

# ratio of sites-with-pages to all-sites-in-the-country
country_site_page_ratio <- 
  wh_wiki_table %>% 
  group_by(country) %>% 
  count(pg = !is.na(site_page_name)) %>% 
  spread(pg,  n) %>% 
  mutate(no_page = ifelse(is.na(`FALSE`), 0,  `FALSE`),
         has_page = ifelse(is.na(`TRUE`), 0,  `TRUE`),
         total = no_page + has_page) %>% 
  mutate(ratio_sites_with_pages = has_page / total) %>% 
  filter(!is.na(total))


# spatial distribution of sites with no page
library(tmap)
data("World")

# tmap_mode("view") # for interactive
tmap_mode("plot")

tm_map_data <- 
World %>% 
  left_join(country_site_page_ratio,
            by = c( 'name' = 'country'))

tm_shape(tm_map_data) +
  tm_polygons("ratio_sites_with_pages",
              palette='viridis',
              popup.vars = 'name') +
  tm_shape(tm_map_data) +
  tm_symbols(col = "red", 
             alpha = 0.1,
             size = "total", 
             scale = 2)





#-----------------------------------------------------
#
# Now, for each site page, let's get some stuff...
#

#- page size
#- number of links on page
#- number of references on page
#- number of pages-linking-to
#- talk page size
#- revision history
#- number of edits
#- number of editors
#- diversity of editors
#- distribution of diff sizes
#- do editors co-occur on multiple pages?
#- geometric mean so edits per day and of words changed per day

get_data_about_page <- function(the_page) {
  
  page <-     
    the_page %>% 
    read_html() %>% 
    html_nodes("#content")
  
  page_name <- 
    the_page %>% 
    str_remove("https://en.wikipedia.org/wiki/")
  
  page_wordcount <- 
    page %>% 
    html_text() %>% 
    stringi::stri_count_words()
  
  page_wikilinks_out <- 
    page %>% 
    html_nodes("p > a") %>% 
    html_attr('href') %>% 
    length()
  
  page_wikilinks_in <- 
    the_page %>% 
    read_html() %>% 
    html_nodes("#t-whatlinkshere a") %>% 
    html_attr('href') %>% 
    str_remove("/wiki/") %>% 
    str_glue("https://en.wikipedia.org/w/index.php?title=",.,
             "&limit=10000") %>% 
    read_html() %>% 
    html_nodes("#mw-whatlinkshere-list li > a") %>% 
    html_attr('href') %>% 
    length()
  
  page_cited_items_on <- 
    page %>% 
    html_nodes(".reference a") %>% 
    html_text() %>% 
    unique() %>% 
    length()
  
  revision_history_page <- 
    the_page %>% 
    read_html() %>% 
    html_nodes("#ca-history a") %>% 
    html_attr('href') %>% 
    str_glue("https://en.wikipedia.org",.) %>% 
    str_replace("&action=history", 
                "&offset=&limit=10000&action=history")

  
  rh_date = revision_history_page %>% 
    read_html %>% 
    html_nodes('li > :nth-child(4)') %>% 
               html_text() %>% 
               lubridate::parse_date_time("H:M, d b Y")
  
  rh_user = revision_history_page %>% 
    read_html %>% 
    html_nodes( 'bdi') %>% 
               html_text()
  
  rh_size = revision_history_page %>% 
    read_html %>% 
    html_nodes( '.history-size') %>% 
               html_text() 
  
  rh_diff_size = revision_history_page %>% 
    read_html %>% 
    html_nodes( '.mw-diff-bytes') %>% 
               html_text() 
  
  rh_comment = revision_history_page %>% 
    read_html %>% 
               html_nodes(' #pagehistory') %>% 
               html_text() %>% 
               str_split(regex("\n")) %>%
               .[[1]] %>% 
               str_replace("^.+ . .", "") %>% 
               enframe %>% 
               filter(value != "") %>% 
               pull(value)
  

  revision_history_page_details  <- 
    tibble(rh_date = rh_date,
           rh_user = rh_user,
           rh_size = rh_size,
           rh_diff_size = rh_diff_size,
           rh_comment = rh_comment
           )
  
  revision_history_page_details <- 
    revision_history_page_details %>% 
    mutate(rh_size = parse_number(rh_size),
           rh_diff_size = parse_number(rh_diff_size)) 
    

  # bots and reverts
  revision_history_page_details <- 
    revision_history_page_details %>% 
    mutate(bot = ifelse(str_detect(rh_user, 
                                   regex('bot', 
                                         ignore_case = TRUE)), 
                        1, 0),
           revert = ifelse(str_detect(rh_comment, 
                                      regex('revert', 
                                            ignore_case = TRUE)), 
                           1, 0))
  
  # revision summary stats
  rh_user_simpson_idx <- 
    revision_history_page_details %>% 
    count(rh_user) %>% 
    summarise(simpson = diversity(n, 'simpson')) %>% 
    pull(simpson)
  
  rh_diff_size_cv <- 
    revision_history_page_details %>% 
    summarise(cv = sd(rh_diff_size) / mean(rh_diff_size)) %>% 
    pull(cv)
  
  rh_user_bot_prop <- 
    revision_history_page_details %>% 
    count(bot) %>% 
    summarise(bot_prop = ifelse(sum(bot) == 0, 
                                0, 
                                n[bot==1] / sum(n))) %>% 
    pull(bot_prop)
  
  rh_revert_prop <- 
    revision_history_page_details %>% 
    count(revert) %>% 
    summarise(revert_prop = ifelse(sum(revert) == 0, 
                                   0, 
                                   n[revert==1] / sum(n))) %>% 
    pull(revert_prop)
  
  talk_page_wordcount <- function(the_page){
    the_page %>% 
      read_html() %>% 
      html_nodes("#ca-talk a") %>% 
      html_attr('href') %>% 
      str_glue("https://en.wikipedia.org",.) %>% 
      read_html() %>% 
      html_nodes("#content") %>% 
      html_text() %>% 
      stringi::stri_count_words()
  }
  
  talk_page_wordcount_result <- talk_page_wordcount(the_page)
  
  n_days <- 100
  page_views_end <- str_remove_all(Sys.Date() - 1, "-")
  page_views_start <- str_remove_all(Sys.Date() - n_days, "-")
  
  page_views_last_n_days_tbl <- 
  the_page %>% 
    read_html() %>% 
    html_nodes("#ca-talk a") %>% 
    html_attr('href') %>% 
    str_remove("/wiki/Talk:") %>% 
  str_glue("https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia.org/all-access/all-agents/",
  .,
  "/daily/",
  page_views_start,
  "/",
  page_views_end) %>% 
    read_html() %>% 
    html_text() %>% 
    jsonlite::fromJSON() %>% 
    .[['items']] %>% 
    as_tibble()
  
  page_views_last_n_days_total = sum(page_views_last_n_days_tbl$views)
    
  
  return(list(page = page,
              page_wordcount = page_wordcount,
              page_wikilinks_out = page_wikilinks_out,
              page_wikilinks_in = page_wikilinks_in,
              page_cited_items_on = page_cited_items_on,
              revision_history_page_details = revision_history_page_details,
              rh_user_simpson_idx = rh_user_simpson_idx,
              rh_diff_size_cv = rh_diff_size_cv,
              rh_user_bot_prop = rh_user_bot_prop,
              rh_revert_prop = rh_revert_prop,
              talk_page_wordcount = talk_page_wordcount_result, 
              page_views_last_n_days_total = page_views_last_n_days_total))
  
}

# don't stop if there is an error, let's see how many we can get
get_data_about_page_safe <- 
  safely(get_data_about_page, 
         otherwise = "some_problem")
#-----------------------------------------------------

# Here's a function 

get_various_page_data_for_all_pages <- function(tbl_region){

# get these data for all pages in a set
# this is a time-consuming web-scraping step
region_tbl_coords_links_info <- 
  tbl_region %>% 
  filter(site_page_link != "https://en.wikipedia.orgNA") %>% 
  mutate(page_info = map(site_page_link, get_data_about_page_safe))

# flatten out some of the results into the table with one row per WH site
region_tbl_coords_links_info_flat <- 
  region_tbl_coords_links_info %>% 
  mutate(page_info_t = transpose(page_info)[["result"]]) %>% 
  filter(page_info_t != "some_problem") %>% 
  mutate(page_wordcount =        map_int(page_info_t, ~.x$page_wordcount),
         page_wikilinks_out =    map_int(page_info_t, ~.x$page_wikilinks_out),
         page_wikilinks_in =     map_int(page_info_t, ~.x$page_wikilinks_in),
         page_cited_items_on =   map_int(page_info_t, ~.x$page_cited_items_on),
         rh_user_simpson_idx =   map_dbl(page_info_t, ~.x$rh_user_simpson_idx),
         rh_user_bot_prop =      map_dbl(page_info_t, ~.x$rh_user_bot_prop), 
         rh_revert_prop =        map_dbl(page_info_t, ~.x$rh_revert_prop), 
         talk_page_wordcount =   map_int(page_info_t, ~.x$talk_page_wordcount),
         page_views_last_n_days_total = map_int(page_info_t, ~.x$page_views_last_n_days_total)
  )

return(region_tbl_coords_links_info_flat)

}

# don't stop if there is an error, let's see how many we can get
get_various_page_data_for_all_pages_safe <- 
  safely(get_various_page_data_for_all_pages, 
         otherwise = "some_problem")


wh_wiki_table <- readr::read_csv("data/wh_wiki_table.csv")

# this takes several hours
page_data_for_all_pages <- 
      get_various_page_data_for_all_pages_safe(wh_wiki_table)

# take a quick look
page_data_for_all_pages$result %>% 
  select_if(is.numeric)

page_data_for_all_pages_result <- page_data_for_all_pages$result 

# save it
saveRDS(page_data_for_all_pages$result, 
        'data/page_data_for_all_pages.rds')














