Make a heatmap with multiple layers of labels for microbial taxonomy
====================================================================

For example, families grouped by phylum

Libraries

    library(plyr)
    library(tidyverse)
    library(ggplot2)
    library(viridis) # only necessary for pretty viridis coloring
    library(mctoolsr) # not necessary if creating taxa summary table on your own

Get data ready for plotting
---------------------------

### Create taxa tables

    # Summarize taxonomy
    tax_sum_fam <- summarize_taxonomy(input_rar_noneg, level = 5)

    # Summarize taxonomy by grouping variable
    tax_sum_fam_species <- taxa_summary_by_sample_type(taxa_smry_df = tax_sum_fam,
                                                       metadata_map = input_rar_noneg$map_loaded, 
                                                       type_header = "GatherVariable", 
                                                       filter_level = 0.05, test_type = "KW")

### Reorder and gather taxa tables

    # Reorder data (if you'd like)
    col.order <- c("my", "column", "order", "here")
    tax_sum_fam_species_ord <- tax_sum_fam_species[col.order]

    # gather the data frame
    tax_sum_fam_species_ord_melt <- tax_sum_fam_species_ord %>%
      rownames_to_column() %>%
      gather(key = `GatherVariable`, `Relative Abundance`, -rowname) %>%
      mutate(`Relative Abundance` = round(`Relative Abundance`, digits = 3)) %>%
      inner_join(tax_sum_fam_species %>%
                   rownames_to_column() %>%
                   select(rowname, starts_with("pvals")), 
                 by = "rowname") %>%
      separate(rowname, into = c("Kingdom", "Phylum", "Class", "Order", "Family"), sep = "; ") %>%
      mutate(Kingdom = gsub("[[:alpha:]]__", replacement = "", Kingdom),
             Phylum = gsub("[[:alpha:]]__", replacement = "", Phylum),
             Class = gsub("[[:alpha:]]__", replacement = "", Class),
             Order = gsub("[[:alpha:]]__", replacement = "", Order),
             Family = gsub("[[:alpha:]]__", replacement = "", Family),
             `Relative Abundance` = 100 * `Relative Abundance`) %>%
      mutate(`GatherVariable` = factor(`GatherVariable`, levels = rev(col.order)))

### Plot heatmap

    heatmap <- ggplot(data = tax_sum_fam_species_ord_melt,
                             aes(x = Family , y = `GatherVariable`, fill = `Relative Abundance`)) + 
      geom_tile(height = 1) + geom_text(aes(label = `Relative Abundance`), color = "white", size = rel(4.5)) +
    # Optional: Display the pvalues as red points on heatmap
      # geom_point(data = tax_sum_fam_species_ord_melt %>% filter(pvalsFDR <= 0.05), 
      #            aes(x = Family , y = `Moss Species`, alpha = rev(pvals)),
      #            color = "red") + 
      facet_grid(~ Phylum, scales = "free", space = "free", drop = TRUE) +
      scale_x_discrete(expand = c(0,0)) +
      scale_y_discrete(expand = c(0,0), drop = TRUE) +
      theme(strip.placement = "outside",
            strip.background = element_rect(size = 0.5, color = "white"),
            panel.border = element_blank(),
            panel.spacing = unit(0, "snpc"),
            axis.text.x = element_text(angle = 45, hjust = 1, size = rel(1.5)),
            axis.text.y = element_text(angle = 0, hjust = 0, face = "italic", size = rel(1.5)),
            strip.text.x = element_text(hjust = 0.5, size = rel(0.8), margin = margin(c(18,0,18,0))),
            axis.title = element_blank(),
            legend.position = "none") +
      scale_fill_viridis(na.value = "white") + # optional viridis colors
      ggtitle("Taxonomy Heatmap by GatherVariable")
