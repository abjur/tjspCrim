
# Download and parse cjsg
d_cjsg <- cjsg_download() %>% 
  mutate(id_lawsuit = stringr::str_replace_all(id_lawsuit, "[:punct:]",""))

# Download and parse cposg
d_cposg <- cposg_download()
