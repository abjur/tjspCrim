pre_process_cjsg <- function(d){
  d %>%  
    filter(str_detect(court, "Criminal")) %>% 
    mutate(id = stringr::str_replace_all(id_lawsuit, "[:punct:]",""),
           court = str_replace_all(court, "Câmara Criminal Extraordinária", "Câmara Direito Criminal Extraordinária"),
           class_subject = str_replace(class_subject, 'Classe/Assunto:\n\t\t\t\t\t\t\t\t\t\t\t ', '')) %>%
    separate(class_subject, into = c("class", "subject"), sep = "/") %>% 
    mutate(court = str_replace(court, "[Ee]xtraordin[áa]ria", "Extraordinária"),
           court = str_replace(court, "[Cc][âa]mara", "Câmara"),
           court = str_replace(court,"[Cc]riminal", "Criminal")) %>% 
    select(-file, -summary, -txt_summary, -dt_decision, -dt_publication, -dt_registration, -id_decision, -id_lawsuit)
}

pre_process_decision <- function(d){
  
  regex_parcialmente_provido <- "parcial provimento|provimento em parte|provimento parcial"
  regex_provido <- "recurso provido|[Dd]eram provimento (ao|à)|[Mm]antiveram o|[rR]ecursos?( e [Rr]emessas?( necessária)? ?)? conhecidos e providos"
  regex_negaram <- "[Rr]ejeitaram os embargos|[Jj]ulgaram prejudicado|[Nn]egaram provimento|Nao conheceram|[Nn]ão providos?"
  
  d %>%
    select(id, decisions) %>% 
    unnest(decisions) %>% 
    distinct(id, .keep_all = TRUE) %>% 
    mutate(decision = stringr::str_to_lower(decision),
           decision = case_when(
             stringr::str_detect(decision, regex_parcialmente_provido) ~ "Parcialmente",
             stringr::str_detect(decision, regex_provido) ~ "Provido",
             stringr::str_detect(decision, regex_negaram) ~ "Negaram",
             TRUE ~ "Outros"
           )) %>% 
    select(-date)
}
