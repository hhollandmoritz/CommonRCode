# Plot an NMDS or PCoA plot using ggplot




#### Required Libraries


```r
library(plyr)
library(tidyverse)
library(ggplot2)
library(viridis) # only necessary for pretty viridis coloring
library(vegan)
```

## Run ordination
### Create distance matrix


```r
md_transformed <- t(sqrt(input_rar_noneg$data_loaded)) # input_rar_noneg$data_loaded is a taxa table from mctoolsr
dm <- vegdist(md_transformed, method = "bray")
```

### Run NMDS
Stress guidlines: stress < 0.05 provides an excellent representation in reduced dimensions, < 0.1 is great, < 0.2 is good/ok, and stress < 0.3 provides a poor representation. 



```r
md.nmds <- metaMDS(dm, k = 2, trymax = 100)
```

Troubleshooting: If NMDS doesn't converge, try increasing ```trymax``` or the number of dimensions ```k =```.  
### Run PCoA 


```r
md.pcoa <- cmdscale(dm, k = 2, eig = TRUE, add = TRUE)
# Get percentages of each axis
md.pct_ex <- round((md.pcoa$eig/sum(md.pcoa$eig)) * 100, 1)
```

### Prepare data for plotting
#### NMDS Preparation


```r
# prepare nmds points
md.nmds.mapdata <- md.nmds$points %>% data.frame() %>%
  rownames_to_column(var = "SampleID") %>%
  inner_join(input_rar_filt_reps$map_loaded, by = "SampleID") %>% # input_rar_filt_reps$map_loaded is the mapping file
  select(SampleID, MDS1, MDS2, everything())

# get stress value
stress.md = paste("stress =", round(md.nmds$stress, digits = 4))

# Get NMDS hulls
group.chulls <- plyr::ddply(md.nmds.mapdata, .(GroupingVariable), function(df) df[chull(df$MDS1, df$MDS2), ])
```

#### PCOA Preparation



```r
md.pcoa.mapdata <- md.pcoa$points %>% data.frame() %>%
  rownames_to_column(var = "SampleID") %>%
  rename(PCOA1 = X1, PCOA2 = X2) %>%
  inner_join(input_rar_filt_reps$map_loaded, by = "SampleID") %>% # input_rar_filt_reps$map_loaded is the mapping file
  select(SampleID, PCOA1, PCOA2, everything())

# Get NMDS hulls
group.chulls <- plyr::ddply(md.pcoa.mapdata, .(GroupingVariable), function(df) df[chull(df$PCOA1, df$PCOA2), ])
```

### Plot Data
#### Plot NMDS


```r
nmds.plot <- ggplot(md.nmds.mapdata, aes(x = MDS1, y = MDS2)) +
  geom_point(alpha = 0.5, aes(color = GroupingVariable)) + 
  geom_polygon(data = group.chulls, 
               aes(x = MDS1, y = MDS2, group = as.factor(GroupingVariable)), 
               alpha = 0.1, fill = "grey40") +
  annotate("text", x = Inf, y = Inf, label = stress.md, hjust = 1, vjust = 1) +
  ggtitle("NMDS Plot Title")
nmds.plot

# Save plot 
ggsave("figures/NMDSplot.svg", nmds.plot, width = 7, height = 5, dpi = 400)
ggsave("figures/NMDSplot.pdf", nmds.plot, width = 7, height = 5, dpi = 400)
```

#### Plot PCOA


```r
pcoa.plot <- ggplot(md.pcoa.mapdata, aes(x = PCOA1, y = PCOA2)) +
  geom_point(alpha = 0.5, aes(color = GroupingVariable)) + 
  geom_polygon(data = group.chulls, 
               aes(x = PCOA1, y = PCOA2, group = as.factor(GroupingVariable)), 
               alpha = 0.1, fill = "grey40") +
  xlab(paste0("PCOA1 (", md.pct_ex[1], " %)")) + 
  ylab(paste0("PCOA2 (", md.pct_ex[2], " %)")) + 
  ggtitle("PCOA Plot Title")
pcoa.plot 

# Save plot 
ggsave("figures/PCOAplot.svg", pcoa.plot, width = 7, height = 5, dpi = 400)
ggsave("figures/PCOAplot.pdf", pcoa.plot, width = 7, height = 5, dpi = 400)
```

