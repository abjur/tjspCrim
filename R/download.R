# Download and parse cjsg
cjsg_download <- function(path = '/home/storage/abj/projects/tjspCrim/d_cjsg') {
  
  message("Downloading files...")
  esaj::download_cjsg(query = "", path = path, 
                      trial_start = '2014-01-01', trial_end = '2014-12-31', max_page = Inf)
  
  arqs <- dir(path = path, full.names = TRUE)
  message("Parsing files...")
  parseados <- esaj::parse_cjsg(arqs)
  saveRDS(parseados, "/home/storage/abj/projects/tjspCrim/data/d_cjsg.rds")
}

# Download and parse cposg
cposg_download <- function(path = '/home/storage/abj/projects/tjspCrim/d_cposg'){ 
  
  message("Downloading files...")
  esaj::download_cposg(id = d_cjsg$id, path = path)
  
  simple_parser <- esaj::make_parser() %>% 
    esaj::parse_decisions()
  
  message("Parsing files...")
  cposg <- dir(path = path, full.names = T)
  parseados <- esaj::run_parser(file = cposg, parser = simple_parser, path = path)
  saveRDS(parseados, "/home/storage/abj/projects/tjspCrim/data/d_cposg.rds")
}
