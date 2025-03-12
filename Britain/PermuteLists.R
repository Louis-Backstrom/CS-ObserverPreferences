permute_lists <- function(data, chain_length = 25, n_chains = 10, thin = 5, progress = FALSE) {
  
  require(tidyverse)
  
  for (chain_number in 1:n_chains) {
    
    for (iteration in 1:chain_length) {
      
      if (iteration == 1) {
        data_start <- data %>% 
          ungroup()
      } else {
        data_start <- data_end %>% 
          select(-iteration)
      }
      
      data_end <- data_start %>% 
        mutate(checklist_group = ceiling(sample(1:max(checklist_number), replace = FALSE)[checklist_number] / 2)) %>% 
        group_by(checklist_group, scientific_name) %>% 
        mutate(row_number = row_number(),
               position = row_number + 0.5 * (max(row_number) == 1)) %>% 
        group_by(checklist_group, position) %>% 
        slice_sample(prop = 1) %>% 
        arrange(checklist_group, position) %>% 
        group_by(checklist_group) %>% 
        mutate(checklist_number = sort(checklist_number)) %>% 
        ungroup() %>% 
        select(scientific_name, checklist_number) %>% 
        mutate(iteration = iteration)
      
      if (iteration %% thin == 0) {
        if (iteration <= thin) {
          data_chain <- data_end
        } else {
          data_chain <- bind_rows(data_chain, data_end)
        }
      }
      
      if (progress == TRUE) {
        print(paste0("Chain ", chain_number, ": Iteration ", iteration))
      }
      
    }
    
    data_chain <- data_chain %>%
      mutate(chain_number = chain_number)
    
    if (chain_number == 1) {
      data_all <- data_chain
    } else {
      data_all <- bind_rows(data_all, data_chain)
    }
    
  }
  
  return(data_all)
  
}

permute_lists_biased <- function(data, bias = "ascending", chain_length = 100, n_chains = 10, thin = 10, progress = FALSE) {
  
  require(tidyverse)
  
  for (chain_number in 1:n_chains) {
    
    for (iteration in 1:chain_length) {
      
      if (iteration == 1) {
        if (bias == "ascending") {
          data_start <- data %>% 
            group_by(scientific_name) %>% 
            mutate(n_obs = n()) %>% 
            ungroup() %>% 
            mutate(reporting_rate = n_obs / max(checklist_number)) %>% 
            group_by(checklist_number) %>% 
            mutate(checklist_richness = n()) %>% 
            group_by(checklist_richness, checklist_number) %>% 
            mutate(checklist_number = cur_group_id()) %>% 
            ungroup() %>% 
            select(checklist_number, checklist_richness, scientific_name, reporting_rate)
        } else {
          data_start <- data %>% 
            group_by(scientific_name) %>% 
            mutate(n_obs = n()) %>% 
            ungroup() %>% 
            mutate(reporting_rate = n_obs / max(checklist_number)) %>% 
            group_by(checklist_number) %>% 
            mutate(checklist_richness = n()) %>% 
            group_by(-checklist_richness, checklist_number) %>% 
            mutate(checklist_number = cur_group_id()) %>% 
            ungroup() %>% 
            select(checklist_number, checklist_richness, scientific_name, reporting_rate)
        }
        
      } else {
        data_start <- data_end %>% 
          select(-iteration)
      }
      
      data_end <- data_start %>% 
        mutate(checklist_group = ceiling(sample(1:max(checklist_number), replace = FALSE)[checklist_number] / 2)) %>% 
        group_by(checklist_group, scientific_name) %>% 
        mutate(row_number = row_number(),
               position = row_number + 0.5 * (max(row_number) == 1)) %>% 
        group_by(checklist_group, position) %>% 
        arrange(checklist_group, position, reporting_rate) %>% 
        group_by(checklist_group) %>% 
        mutate(checklist_number = sort(checklist_number)) %>% 
        ungroup() %>% 
        select(scientific_name, checklist_number, reporting_rate) %>% 
        mutate(iteration = iteration)
      
      if (iteration %% thin == 0) {
        if (iteration <= thin) {
          data_chain <- data_end
        } else {
          data_chain <- bind_rows(data_chain, data_end)
        }
      }
      
      if (progress == TRUE) {
        print(paste0("Chain ", chain_number, ": Iteration ", iteration))
      }
      
    }
    
    data_chain <- data_chain %>%
      mutate(chain_number = chain_number) %>% 
      select(scientific_name, checklist_number, iteration, chain_number)
    
    if (chain_number == 1) {
      data_all <- data_chain
    } else {
      data_all <- bind_rows(data_all, data_chain)
    }
    
  }
  
  return(data_all)
  
}
