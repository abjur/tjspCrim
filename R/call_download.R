# Download and parse cjsg
cjsg_download(path = '/home/storage/abj/projects/tjspCrim/d_cjsg')

# criar d_cjsg para rodar cposg_download(path) . 
##  A função utiliza a variável numérica d_cjsg$id para baixar os processos.
d_cjsg <- readRDS("/home/storage/abj/projects/tjspCrim/data/d_cjsg.rds") %>%
  pre_process_cjsg()

# Download and parse cposg
cposg_download(path = '/home/storage/abj/projects/tjspCrim/d_cposg')