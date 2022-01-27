n_distinct(df_all_filters$sequence)

n_distinct(df_all_filters$family_name_corrected)
n_distinct(df_all_filters$genus_name_corrected)
n_distinct(df_all_filters$species_name_corrected)

seamounts <- df_all_filters %>%
  filter(habitat_type %in% c("Pelagique", "Sommet"))

n_distinct(seamounts$sequence)
n_distinct(seamounts$family_name_corrected)
n_distinct(seamounts$genus_name_corrected)
n_distinct(seamounts$species_name_corrected)


infos_seamounts <- data.frame()

sample <- unique(seamounts$code_explo)

for (i in 1:length(sample)) {
  df <- seamounts %>%
    filter(code_explo == sample[i])
  infos_seamounts[i, "sample"] <- sample[i]
  infos_seamounts[i, "site"] <- unique(df$site_name)
  infos_seamounts[i, "habitat"] <- unique(df$habitat_type)
  infos_seamounts[i, "n_sequence"] <- n_distinct(df$sequence)
  infos_seamounts[i, "n_family"] <- n_distinct(df$family_name_corrected)
  infos_seamounts[i, "n_genus"] <- n_distinct(df$genus_name_corrected)
  infos_seamounts[i, "n_species"] <- n_distinct(df$species_name_corrected)
  
}


pelagique <- seamounts %>% 
  filter(habitat_type=="Pelagique")
n_distinct(pelagique$sequence)
n_distinct(pelagique$species_name_corrected)

sommet <- seamounts %>% 
  filter(habitat_type=="Sommet")
n_distinct(sommet$sequence)
n_distinct(sommet$species_name_corrected)

