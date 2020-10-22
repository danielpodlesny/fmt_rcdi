# add microbe presence/absence info to df
tag_species_competition = function(df) {
  df %>% 
    
    # presence/absence in R/D
    mutate(existed.donor = replace_na(rel_abund.donor, 0) > 0,
           existed.recipient = replace_na(rel_abund.recipient, 0) > 0,
           existed.post = replace_na(rel_abund.post, 0) > 0) %>% 
    
    # tag species competition
    mutate(existed.both = existed.donor & existed.recipient,  
           existed.either = (existed.donor | existed.recipient) & !existed.both,
           existed.either.recipient = existed.either & existed.recipient,
           existed.either.donor = existed.either & existed.donor,
           existed.any = (existed.donor | existed.recipient)) %>% 
    
    return(.)
}

# summarize donor/recipient/post-FMT events based on presence/absence
summarize_existed = function(df) {
  df %>% 
    # presence/absence in R/D
    mutate(existed.donor = replace_na(rel_abund.donor, 0) > 0,
           existed.recipient = replace_na(rel_abund.recipient, 0) > 0,
           existed.post = replace_na(rel_abund.post, 0) > 0) %>% 
    
    # co-occurrence
    mutate(existed.both = existed.donor & existed.recipient,  
           existed.either = (existed.donor | existed.recipient) & !existed.both,
           existed.either.recipient = existed.either & existed.recipient,
           existed.either.donor = existed.either & existed.donor,
           existed.any = (existed.donor | existed.recipient)) %>% 
    
    summarize(.groups = 'drop',
      # totals
      n.species = length(unique(species)), 
      n.post = sum(existed.post, na.rm = T),
      n.donor = sum(existed.donor, na.rm = T),
      n.recipient = sum(existed.recipient, na.rm = T),
      n.either = sum(existed.either, na.rm = T),
      n.either.donor = sum(existed.either.donor, na.rm = T),
      n.either.recipient = sum(existed.either.recipient, na.rm = T),
      n.any = sum(existed.any, na.rm = T),
      n.both = sum(existed.both, na.rm = T),
      n.before = sum(!existed.post & existed.both, na.rm = T),
      n.post.unique = sum(existed.post & !existed.donor & !existed.recipient, na.rm = T),
      n.donor.unique = sum(!existed.post & existed.donor & !existed.recipient, na.rm = T),
      n.recipient.unique = sum(!existed.post & !existed.donor & existed.recipient, na.rm = T),
      
      
      # % of recipient-specific found in post-FMT
      n.post.either.recipient = 
        sum(existed.post & existed.either.recipient, na.rm = T), 
      f.either_recipient.post = 
        n.post.either.recipient / n.either.recipient,
      r.post.recipient = 
        sum(ifelse(existed.post & existed.either.recipient, 
                   rel_abund.post, 0), na.rm = T),
      r.recipient.post = 
        sum(ifelse(existed.post & existed.either.recipient, 
                   rel_abund.recipient, 0), na.rm = T),
      
      # % of donor-specific found in post-FMT
      n.post.either.donor = 
        sum(existed.post & existed.either.donor, na.rm = T), 
      f.either_donor.post = 
        n.post.either.donor / n.either.donor,
      r.post.donor = 
        sum(ifelse(existed.post & existed.either.donor, 
                   rel_abund.post, 0), na.rm = T),
      r.donor.post = 
        sum(ifelse(existed.post & existed.either.donor, 
                   rel_abund.donor, 0), na.rm = T),
      
      # % of any found in post-FMT
      n.post.any = sum(existed.post & existed.any, na.rm = T), 
      f.any.post = n.post.any / n.any,
      r.post.any = 
        sum(ifelse(existed.post & existed.any, 
                   rel_abund.post, 0), na.rm = T), 
      
      # % of either found in post-FMT
      n.post.either = sum(existed.post & existed.either, na.rm = T), 
      f.either.post = n.post.either / n.either,
      r.post.either = sum(ifelse(existed.post & existed.either, 
                                 rel_abund.post, 0), na.rm = T), 
      
      # % of both found in post-FMT
      n.post.both = sum(existed.post & existed.both, na.rm = T), 
      f.both.post = n.post.both / n.both,
      r.post.both = 
        sum(ifelse(existed.post & existed.both, 
                   rel_abund.post, 0), na.rm = T),
      r.recipient.both = 
        sum(ifelse(existed.post & existed.both, 
                   rel_abund.recipient, 0), na.rm = T),
      r.donor.both = 
        sum(ifelse(existed.post & existed.both, 
                   rel_abund.donor, 0), na.rm = T),
      
      # % shared before but not found post-FMT
      r.recipient.before = 
        sum(ifelse(!existed.post & existed.both, 
                   rel_abund.recipient, 0), na.rm = T),
      r.donor.before = 
        sum(ifelse(!existed.post & existed.both, 
                   rel_abund.donor, 0), na.rm = T),
      
      # % unique
      r.donor.unique = 
        sum(ifelse(!existed.post & existed.donor & !existed.recipient, 
                   rel_abund.donor, 0), na.rm = T),
      r.recipient.unique = 
        sum(ifelse(!existed.post & !existed.donor & existed.recipient, 
                   rel_abund.recipient, 0), na.rm = T),
      r.post.unique = 
        sum(ifelse(existed.post & !existed.donor & !existed.recipient, 
                   rel_abund.post, 0), na.rm = T),
      
      # % of post-FMT found in recipient, donor, either, both (competing)
      f.post.any = n.post.any / n.post,
      f.post.either = n.post.either / n.post,
      f.post.either.recipient = n.post.either.recipient / n.post,
      f.post.either.donor = n.post.either.donor / n.post,
      f.post.both = n.post.both / n.post,
    ) %>% 
    return()
}

mp_cooccurrence <- function(mp_species.long) {
  # MetaPhlAn2 species-level matrix
  mp_species_table.matrix <-
    mp_species.long %>% 
    select(species, Name, rel_abund) %>% 
    filter(!grepl(species, pattern = 'unclassified')) %>% 
    pivot_wider(id_cols = 'Name', names_from = 'species', values_from ='rel_abund') %>% 
    mutate_at(.vars = vars(everything(), -Name), 
              .funs = funs(replace_na(as.numeric(. > 0), 0))) %>%
    column_to_rownames('Name') %>% 
    as.matrix()
  
  # MetaPhlAn2 genus-level matrix
  mp_genus_table.matrix <-
    mp_species.long %>% 
    select(genus, Name, rel_abund) %>% 
    filter(!grepl(genus, pattern = 'unclassified')) %>% 
    group_by(Name, genus) %>% 
    summarize(rel_abund = as.numeric(sum(rel_abund, na.rm = T) > 0), groups = 'drop') %>% 
    pivot_wider(id_cols = 'Name', names_from = 'genus', values_from ='rel_abund') %>% 
    mutate_at(.vars = vars(everything(), -Name), 
              .funs = funs(replace_na(as.numeric(. > 0), 0))) %>%
    column_to_rownames('Name') %>% 
    as.matrix()
  
  # MetaPhlAn2 family-level matrix
  mp_family_table.matrix <-
    mp_species.long %>% 
    select(family, Name, rel_abund) %>% 
    filter(!grepl(family, pattern = 'unclassified')) %>% 
    group_by(Name, family) %>% 
    summarize(rel_abund = as.numeric(sum(rel_abund, na.rm = T) > 0), groups = 'drop') %>% 
    pivot_wider(id_cols = 'Name', names_from = 'family', values_from ='rel_abund') %>% 
    mutate_at(.vars = vars(everything(), -Name), 
              .funs = funs(replace_na(as.numeric(. > 0), 0))) %>%
    column_to_rownames('Name') %>% 
    as.matrix()
  
  # family
  mp_family_table.dist <- mp_family_table.matrix %*% t(mp_family_table.matrix)
  mp_family_table.long <- 
    distmat_to_long(distmat = as.data.frame(mp_family_table.dist), 
                    value_name = 'n.shared_family',
                    rm_diag = T)
  mp_family_counts <- diag(mp_family_table.dist) %>% 
    as.data.frame() %>% 
    rownames_to_column('Sample') %>% 
    rename(n.family = '.')
  
  # genus
  mp_genus_table.dist <- mp_genus_table.matrix %*% t(mp_genus_table.matrix)
  mp_genus_table.long <- 
    distmat_to_long(distmat = as.data.frame(mp_genus_table.dist), 
                    value_name = 'n.shared_genus',
                    rm_diag = T)
  mp_genus_counts <- diag(mp_genus_table.dist) %>% 
    as.data.frame() %>% 
    rownames_to_column('Sample') %>% 
    rename(n.genus = '.')
  
  # species
  mp_species_table.dist <- mp_species_table.matrix %*% t(mp_species_table.matrix)
  mp_species_table.long <- 
    distmat_to_long(distmat = as.data.frame(mp_species_table.dist), 
                    value_name = 'n.shared_species',
                    rm_diag = T)
  mp_species_counts <- diag(mp_species_table.dist) %>% 
    as.data.frame() %>% 
    rownames_to_column('Sample') %>% 
    rename(n.species = '.')
  
  mp_taxa_counts <- 
    full_join(mp_species_counts, mp_genus_counts, by = 'Sample') %>% 
    full_join(., mp_family_counts, by = 'Sample')
  
  mp_taxa_table.long <- 
    mp_family_table.long %>% 
    rowwise() %>% 
    mutate(Sample_Pair = paste0(sort(c(row, col)), collapse = ',')) %>% 
    ungroup() %>% select(-row, -col) %>% 
    full_join(., mp_genus_table.long %>% 
                rowwise() %>% 
                mutate(Sample_Pair = paste0(sort(c(row, col)), collapse = ',')) %>% 
                ungroup() %>% select(-row, -col), 
              by = c('Sample_Pair')) %>% 
    full_join(., mp_species_table.long %>% 
                rowwise() %>% 
                mutate(Sample_Pair = paste0(sort(c(row, col)), collapse = ',')) %>% 
                ungroup() %>% select(-row, -col), 
              by = c('Sample_Pair')) %>% 
    select(Sample_Pair, everything())
  
  return(list('mp_taxa_counts' = mp_taxa_counts, 'mp_taxa_table.long' = mp_taxa_table.long))
}
