# Download cjsg

timedownl_cjsg <- 
  esaj::peek_cjsg(query = "", trial_start = '2014-01-01', trial_end = '2014-12-31')

d_cjsg <- 
  esaj::download_cjsg(query = "", path = '/home/storage/abj/projects/tjspCrim/d_cjsg', 
                      trial_start = '2014-01-01', trial_end = '2014-12-31', max_page = 10)

# Download cposg

d_cposg <- 
  esaj::download_cposg(id = d_cjsg$n_processo, path = '/home/storage/abj/projects/tjspCrim/d_cposg')

