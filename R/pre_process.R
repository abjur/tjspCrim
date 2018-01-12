pre_process_cjsg <- function(d){
  d %>%  
    mutate(id = stringr::str_replace_all(id_lawsuit, "[:punct:]",""),
           court = str_replace_all(court, "Câmara Criminal Extraordinária", "Câmara Direito Criminal Extraordinária"),
           class_subject = str_replace(class_subject, 'Classe/Assunto:\n\t\t\t\t\t\t\t\t\t\t\t ', '')) %>%
    separate(class_subject, into = c("class", "subject"), sep = "/") %>% 
    mutate(court = str_replace(court, "[Ee]xtraordin[áa]ria", "Extraordinária"),
           court = str_replace(court, "[Cc][âa]mara", "Câmara"),
           court = str_replace(court,"[Cc]riminal", "Criminal")) %>% 
    select(-file, -summary, -txt_summary, -dt_publication, -dt_registration, -id_decision, -id_lawsuit) %>% 
    filter(str_detect(court, "Criminal"))
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

homog_base <- function(d){
  
  p_subject <- d_cposg_cjsg %>% 
    count(subject) %>% 
    mutate(prop = n/sum(n))
  
  amostra_processo <- function(d_court) {
    s <- sample(p_subject$subject, 1, prob = p_subject$prop)
    d_filtrado <- d_court %>% 
      filter(subject == s)
    if (nrow(d_filtrado) == 0) return(tibble(decision = NA_character_))
    d_filtrado %>% 
      sample_n(1)
  }
  
  map_dfr(unique(d_cposg_cjsg$court), ~{
    d_court <- filter(d_cposg_cjsg, court == .x)
    map_dfr(1:nrow(d_court), ~amostra_processo(d_court))
  })
}