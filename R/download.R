# Download and parse cjsg
cjsg_download <- function(path = '/home/storage/abj/projects/tjspCrim/d_cjsg') {
  
  timedownl_cjsg <- esaj::peek_cjsg(query = "", trial_start = '2014-01-01', trial_end = '2014-12-31')
  timedownl_cjsg
  
  message("Downloading files...")
  esaj::download_cjsg(query = "", path = path, 
                      trial_start = '2014-01-01', trial_end = '2014-12-31', max_page = 5)
  
  arqs <- dir(path = path, full.names = TRUE)
  message("Parsing files...")
  parse_cjsg(arqs)
}

# Download and parse cposg
cposg_download <- function(){ 
  
  message("Downloading files...")
  esaj::download_cposg(id = d_cjsg$id_lawsuit, path = '/home/storage/abj/projects/tjspCrim/d_cposg')
  
  simple_parser <- esaj::make_parser() %>% 
    esaj::parse_decisions() %>% 
    esaj::parse_parts() %>% 
    esaj::parse_data()
  
  message("Parsing files...")
  esaj::run_parser(file = d_cposg, parser = simple_parser, path = '/home/storage/abj/projects/tjspCrim/d_cposg')
}