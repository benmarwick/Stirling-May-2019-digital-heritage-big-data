

russia <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_Sites_in_Russia"

russia__xml_tables <- 
read_html(russia) %>% 
  html_nodes("table")

russia_tables_tbl <- 
map(russia__xml_tables, 
    ~html_table(.x, fill = TRUE) %>% as_tibble(., .name_repair = "unique")) 

russia_tables_tbl_culture <- 
russia_tables_tbl[[2]] %>% 
  mutate(type = ifelse(Image == "", NA, Image),
         type = zoo::na.locf(type)) %>% 
  filter(type == "Cultural") %>% 
  slice(-1) %>% 
  select(Name, Location, Yearlisted, Description) %>% 
  set_names(c("Site", "Location", "Year", "Description")) %>% 
  mutate(Year_num = ifelse(is.integer(Year), Year, parse_number(Year))) 


# scrape out the coords for mapping
coords_chars <- "\\d|°|′|″|N|E|S|W|\\."
russia_tables_tbl_culture_coords <- 
  map_chr(str_extract_all(russia_tables_tbl_culture$Location, 
                          coords_chars), 
          ~paste0(.x, collapse = "") %>% 
            str_replace_all(., "^[A-Z]*", "") %>% 
            str_extract(., ".+?(?=E|W)"))

russia_coords_clean <- 
  russia_tables_tbl_culture_coords %>% 
  str_split("N|S") %>% 
  Reduce(rbind, .) %>% 
  as_tibble(., .name_repair = "universal") %>% 
  mutate(lat = parse_lat(`...1`),
         lon = parse_lon(`...2`))

# attach clean coords to main table
russia_tbl_coords <- 
  russia_tables_tbl_culture %>% 
  bind_cols(russia_coords_clean)

# get country of site from location text
russia_tbl_coords <- 
  russia_tbl_coords %>% 
  mutate(country = "Russia",
         Criteria = 'Cultural',
         `Areaha (acre)` = NA)
 
# get links to each site's wiki page 
get_link <- function(html_table, Site){
  html_table %>% 
    html_nodes(xpath=paste0("//a[text()='", Site, "']")) %>% 
    html_attr("href")
}

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

russia_tbl_coords_links <- 
  russia_tbl_coords %>% 
  mutate(site_page_name = map_chr(Site, 
                                  ~get_link(russia__xml_tables[2], .x)[1])) %>% 
  mutate(site_page_link = as.character(str_glue('https://en.wikipedia.org{site_page_name}'))) %>% 
  select(cols_we_want)

tbl_russia <- russia_tbl_coords_links


