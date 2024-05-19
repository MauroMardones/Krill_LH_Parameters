---
title: "Derive growth parameters and natural mortality rates for krill while accounting for spatial heterogeneity in the Western Antarctic Peninsula."
subtitle: "Working Paper to be submitted in a CCAMLR EMM-WG 2024"
author: "Mardones, M; Cárdenas, C., Krüger, L., Santa Cruz, F."
date:  "19 May, 2024"
bibliography: param.bib
csl: apa.csl
link-citations: yes
linkcolor: blue
output:
  html_document:
    keep_md: true
    toc: true
    toc_deep: 3
    toc_float:
      collapsed: false
      smooth_scroll: false
    theme: cosmo
    fontsize: 0.9em
    linestretch: 1.7
    html-math-method: katex
    self-contained: true
    code-tools: true
editor_options: 
  markdown: 
    wrap: 72
---



```r
rm(list = ls())
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE,
                      warning = FALSE,
                      fig.align = 'center',
                      fig.width = 8,
                      fig.height = 6,
                      dev = 'jpeg',
                      dpi = 300)
#XQuartz is a mess, put this in your onload to default to cairo instead
options(bitmapType = "cairo") 
# (https://github.com/tidyverse/ggplot2/issues/2655)
# Lo mapas se hacen mas rapido
```




```r
library(here)
library(kableExtra)
library(ggthemes)
library(ggrepel)
library(ggridges)
#analisis
library(ggpubr)
library(easystats) # multiples unciones analiticas
library(sf)
library(tidyverse, quietly = TRUE)
library(modelsummary)
library(terra) # replace raster
library(TropFishR)
library(mixR)
library(CCAMLRGIS)
```


# Methodology

The object `ohbio2` come from data exploration analysis in data request
CCAMLR data. This objetc have bio information from krill.



Join data set with master as `c1` set. This join is trought
`obs_haul_id` variable to get geoposition variables


```r
ohbio2 <- left_join(c1, ohbio, by="obs_haul_id")
names(ohbio2)
```

```
##  [1] "c1_id.x"                   "obs_haul_id"              
##  [3] "obs_logbook_id.x"          "obs_haul_number"          
##  [5] "haul_number.x"             "vessel_name"              
##  [7] "vessel_nationality_code"   "fishing_purpose_code"     
##  [9] "season_ccamlr"             "target_species"           
## [11] "asd_code"                  "trawl_technique"          
## [13] "catchperiod_code"          "date_catchperiod_start"   
## [15] "datetime_set_start"        "datetime_set_end"         
## [17] "datetime_haul_start"       "datetime_haul_end"        
## [19] "datetime_timezone"         "depth_gear_set_end_m"     
## [21] "depth_gear_haul_start_m"   "depth_bottom_set_end_m"   
## [23] "depth_bottom_haul_start_m" "latitude_set_end"         
## [25] "longitude_set_end"         "latitude_haul_start"      
## [27] "longitude_haul_start"      "gear_type_code"           
## [29] "gear_type"                 "mesh_code"                
## [31] "trawl_net_number"          "notes"                    
## [33] "trawl_duration_depth_h"    "trawl_duration_total_h"   
## [35] "krill_greenweight_kg"      "c1_id.y"                  
## [37] "obs_logbook_id.y"          "haul_number.y"            
## [39] "taxon_code"                "taxon_scientific_name"    
## [41] "taxon_family"              "maturity_stage"           
## [43] "sex_code"                  "length_total_cm"          
## [45] "greenweight_kg"
```



```r
ohbio3 <- ohbio2 %>%
  mutate(Year = year(date_catchperiod_start),
         Month = month(date_catchperiod_start),
         Day = day(date_catchperiod_start)) %>% 
  #toupper() para convertir los valores a mayúsculas
  mutate(sex_code = toupper(sex_code)) %>% 
  dplyr::select(7, 9, 11, 12, 14, 24, 25, 29, 42, 44, 46, 47, 43, 45) |>  
  filter(asd_code=="481")
```


First thing is get different rater layer to join krill data length
according different porpoises.


```r
# Cargo linea de costa
coast <- load_Coastline()
coast1<- st_as_sf(coast) 
coast2 = st_transform(coast1, "+proj=latlong +ellps=WGS84")
# Uso las agrupaciones de Strata
strata <- st_read("~/DOCAS/Mapas/Antarctic_SHPfiles/Strata.shp",
                quiet=T)
strata=st_transform(strata, "+proj=latlong +ellps=WGS84")
```
Test Strata SSMU, just to know another way to join data, but this kind of spatial structuration is deprecated to mamagemente use (Figure\@ref(fig:maptest2).



Grouping bio data into stratas


```r
ohbio6 <- st_as_sf(ohbio3 %>% 
                     drop_na(latitude_set_end), 
                   coords = c("longitude_set_end", "latitude_set_end"),  
                  crs = "+proj=latlong +ellps=WGS84")
```

## Study Area


```r
# y testeo el mapa
ssmap <- ggplot()+
  geom_sf(data = strata |> 
            filter(ID != "Outer") |>  
           mutate(ID = str_replace_all(ID, "Extra", "GS")),color="red")+
  geom_sf(data = ohbio6 |>
            drop_na(length_total_cm) |> 
            filter(Year>2017), 
          aes(color = length_total_cm)) +
  geom_text_repel(data = strata |>  
            filter(ID != "Outer") |>  
           mutate(ID = str_replace_all(ID, "Extra", "GS")), 
            aes(x = Labx, y = Laby, 
                label = ID), 
            min.segment.length = 0,
                             box.padding = 2,
                             max.overlaps = 10)+
  geom_sf(data = coast2, colour="black", fill=NA)+
  scale_color_viridis_c(option = "G",
                        name="Length (cm)")+
  ylim(230000, 2220000)+
  xlim(-3095349 , -1858911)+
  # coord_sf(crs = 32610)+ #sistema de prpyecccion para campos completos
  coord_sf(crs = 6932)+
  theme_bw()
ssmap
```

<div class="figure" style="text-align: center">
<img src="index_files/figure-html/maptest-1.jpeg" alt="Strata Maps in 48.1"  />
<p class="caption">Strata Maps in 48.1</p>
</div>


Length composition by Strata CCAMLR to visualization first. First step is group data into to poligons strata.


```r
strata <- st_make_valid(strata)
sf4 <- st_join(strata, ohbio6)
# Save an object to a file
saveRDS(sf4, file = "sf4.rds")
```

Load RData

```r
# Restore the object
sf4 <- readRDS("sf4.rds")
```

Statistical difference


```r
m <- aov(length_total_cm ~ID, data=sf4)
plot(TukeyHSD(m))
```

<img src="index_files/figure-html/unnamed-chunk-6-1.jpeg" style="display: block; margin: auto;" />


## Data

histogram length data to viz in anthor way.



```r
jzstrata <- ggplot(sf4 %>% 
                     mutate(ID = if_else(ID == "Extra", "GERLASHE", ID)) %>% 
               filter(Year>2010,
                      ID !="Outer"),
             aes(x=length_total_cm, 
                 y = as.factor(Month), 
                 fill=ID))+
  geom_density_ridges(stat = "binline", bins = 30, 
                      scale = 1.9, 
                      draw_baseline = FALSE,
                      alpha=0.9)+
  facet_grid(Year~ID) +   
  geom_vline(xintercept = 3.6, color = "red")+
  scale_x_continuous(breaks = seq(from = 1, to = 10, 
                                  by = 2))+
  scale_y_discrete(breaks = seq(from = 1, 
                                to = 12, by = 4))+
  scale_fill_viridis_d(name="Strata",
                       option="F")+
  theme_few()+
  xlab("Length (cm.)")+
  ylab("")
jzstrata
```

<img src="index_files/figure-html/unnamed-chunk-7-1.jpeg" style="display: block; margin: auto;" />
Now, must filter the DF


```r
sf5 <- sf4 |> 
  dplyr::select(c("ID",
        "date_catchperiod_start",
        "length_total_cm",
        "Year",
        "Month",
        "sex_code",
         "greenweight_kg")) |> 
  mutate(ID = if_else(ID == "Extra", "GERLASHE", ID)) |>  
  filter(ID !="Outer") |> 
  data.frame()
```



Calculating the proportion of records per year and per stratum in a table. This table allows us to establish criteria for defining the strata on which we will analyze krill parameter. We leave out just `JOIN`.



```r
# Calcular la proporción
proporcion <- (round(table(sf5$ID) / sum(table(sf5$ID))*100,2))

# Crear una tabla con las proporciones
tabla_proporcion <- as.data.frame(proporcion)
tabla_proporcion$Categoria <- rownames(tabla_proporcion)
rownames(tabla_proporcion) <- NULL

# Renombrar las columnas
colnames(tabla_proporcion) <- c("Strata", "%")

# Mostrar la tabla utilizando kbl()
kbl(tabla_proporcion, 
    caption = "Proportion length register by Strata")  |> 
  kable_classic(full_width = F, 
                html_font = "Cambria") |> 
  kable_styling(bootstrap_options = "striped", 
                latex_options = "striped")
```

<table class=" lightable-classic table table-striped" style="color: black; font-family: Cambria; width: auto !important; margin-left: auto; margin-right: auto; color: black; margin-left: auto; margin-right: auto;">
<caption>Proportion length register by Strata</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Strata </th>
   <th style="text-align:right;"> % </th>
   <th style="text-align:left;"> NA </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> BS </td>
   <td style="text-align:right;"> 62.71 </td>
   <td style="text-align:left;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EI </td>
   <td style="text-align:right;"> 6.58 </td>
   <td style="text-align:left;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GERLASHE </td>
   <td style="text-align:right;"> 12.13 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> JOIN </td>
   <td style="text-align:right;"> 0.14 </td>
   <td style="text-align:left;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SSIW </td>
   <td style="text-align:right;"> 18.43 </td>
   <td style="text-align:left;"> 5 </td>
  </tr>
</tbody>
</table>

## Parameters estimation `k` and `L_inf_`




```r
#MALE

sf5fil <- sf5 |>
  filter(ID %in% c("BS", "EI", "GERLASHE", "SSIW"),
         sex_code %in% "M") |>  
  mutate(date_catchperiod_start = as.Date(date_catchperiod_start)) |>  
  mutate(yearly_Group = floor_date(date_catchperiod_start, "year")) |>   # New column
  drop_na(length_total_cm) 

# Definir los nombres correspondientes a cada objeto lfq_results
nombres <- c("SSI", "BS", "GERLASHE", "EI")
# Crear una lista para almacenar los resultados de cada ID
lfq_results <- list()
# Iterar sobre cada ID
for (id in unique(sf5fil$ID)) {
  # Obtener el nombre correspondiente
  nombre <- nombres[which(unique(sf5fil$ID) == id)]
  # Filtrar los datos para el ID actual
  df_id <- sf5fil %>% filter(ID == id)
  # Crear un objeto lfq para el ID actual
  lfq <- lfqCreate(data = df_id,
                   Lname = "length_total_cm",
                   Dname = "yearly_Group",
                   bin_size = 0.1)
  # Agregar el resultado a la lista
  lfq_results[[id]] <- lfq
  # Graficar el objeto lfq
  plot(lfq, Fname = "catch",
       main = nombre)
}
```

<img src="index_files/figure-html/unnamed-chunk-10-1.jpeg" style="display: block; margin: auto;" /><img src="index_files/figure-html/unnamed-chunk-10-2.jpeg" style="display: block; margin: auto;" /><img src="index_files/figure-html/unnamed-chunk-10-3.jpeg" style="display: block; margin: auto;" /><img src="index_files/figure-html/unnamed-chunk-10-4.jpeg" style="display: block; margin: auto;" />

```r
#FEMALE

sf5filhe <- sf5 |>
  filter(ID %in% c("BS", "EI", "GERLASHE", "SSIW"),
         sex_code %in% "F") |>  
  mutate(date_catchperiod_start = as.Date(date_catchperiod_start)) |>  
  mutate(yearly_Group = floor_date(date_catchperiod_start, "year")) |>   # New column
  drop_na(length_total_cm) 

# Definir los nombres correspondientes a cada objeto lfq_results
nombres <- c("SSI", "BS", "GERLASHE", "EI")
# Crear una lista para almacenar los resultados de cada ID
lfq_resultshe <- list()
# Iterar sobre cada ID
for (id in unique(sf5filhe$ID)) {
  # Obtener el nombre correspondiente
  nombre <- nombres[which(unique(sf5filhe$ID) == id)]
  # Filtrar los datos para el ID actual
  df_idhe <- sf5filhe %>% filter(ID == id)
  # Crear un objeto lfq para el ID actual
  lfqhe <- lfqCreate(data = df_idhe,
                   Lname = "length_total_cm",
                   Dname = "yearly_Group",
                   bin_size = 0.1)
  # Agregar el resultado a la lista
  lfq_resultshe[[id]] <- lfqhe
  # Graficar el objeto lfq
  plot(lfqhe, Fname = "catch",
       main = nombre)
}
```

<img src="index_files/figure-html/unnamed-chunk-10-5.jpeg" style="display: block; margin: auto;" /><img src="index_files/figure-html/unnamed-chunk-10-6.jpeg" style="display: block; margin: auto;" /><img src="index_files/figure-html/unnamed-chunk-10-7.jpeg" style="display: block; margin: auto;" /><img src="index_files/figure-html/unnamed-chunk-10-8.jpeg" style="display: block; margin: auto;" />

Identified difference in length distribution just to male:


```r
Bhattacharya(lfq_results$EI)
```

```
## Interactive session needed for Bhattacharya.
```

```
## NULL
```



```r
Bhattacharya(lfq_results$SSIW)
```

```
## Interactive session needed for Bhattacharya.
```

```
## NULL
```



```r
Bhattacharya(lfq_results$BS)
```

```
## Interactive session needed for Bhattacharya.
```

```
## NULL
```



```r
Bhattacharya(lfq_results$GERLASHE)
```

```
## Interactive session needed for Bhattacharya.
```

```
## NULL
```

now we assign objet to male `lfq_result` and female `lfq_resulthe` and plot with `lfqRestructure()`


Male


```r
# Definir los nombres correspondientes a cada objeto lfq_results
nombres <- c("SSI", "BS", "GERLASHE", "EI")
# Iterar sobre cada objeto lfq almacenado en lfq_results
for (i in seq_along(lfq_results)) {
  lfq <- lfq_results[[i]]
  # Obtener el nombre correspondiente
  nombre <- nombres[i]
  # Restructurar el objeto lfq
  lfqbin <- lfqRestructure(lfq, MA = 3, addl.sqrt = TRUE)
  # Graficar el objeto lfq reestructurado
  plot(lfqbin, hist.col = c("white", "black"),
       image.col = c(rep(rgb(1,0.8,0.8),1000), "white", 
                     rep(rgb(0.8,0.8,1),1000)),
       ylim = c(0,max(lfqbin$midLengths+0.5)),
       main = nombre)
  # Ajustar curvas al objeto lfq reestructurado
  tmp <- lfqFitCurves(lfqbin, par = list(Linf=6.5, 
                                          K=0.45,
                                          t_anchor=0.5),
                      draw = TRUE, col=4, lty=2)
}
```

<img src="index_files/figure-html/unnamed-chunk-15-1.jpeg" style="display: block; margin: auto;" /><img src="index_files/figure-html/unnamed-chunk-15-2.jpeg" style="display: block; margin: auto;" /><img src="index_files/figure-html/unnamed-chunk-15-3.jpeg" style="display: block; margin: auto;" /><img src="index_files/figure-html/unnamed-chunk-15-4.jpeg" style="display: block; margin: auto;" />

Female


```r
# Crear una lista para almacenar los resultados de PW_results <- list()

# Definir los nombres correspondientes a cada objeto lfq_results
nombres <- c("SSI", "BS", "GERLASHE", "EI")

# Iterar sobre cada objeto lfq almacenado en lfq_results
for (i in seq_along(lfq_resultshe)) {
  lfqhe <- lfq_resultshe[[i]]
  
  # Obtener el nombre correspondiente
  nombre <- nombres[i]
  
  # Restructurar el objeto lfq
  lfqbinhe <- lfqRestructure(lfqhe, MA = 3, addl.sqrt = TRUE)
  
  # Graficar el objeto lfq reestructurado
  plot(lfqbinhe, hist.col = c("white", "black"),
       image.col = c(rep(rgb(1,0.8,0.8),1000), "white", 
                     rep(rgb(0.8,0.8,1),1000)),
       ylim = c(0,max(lfqbinhe$midLengths+0.5)),
       main = nombre)
  
  # Ajustar curvas al objeto lfq reestructurado
  tmp <- lfqFitCurves(lfqbinhe, par = list(Linf=6.5, 
                                          K=0.45,
                                          t_anchor=0.5),
                      draw = TRUE, col=4, lty=2)
}
```

<img src="index_files/figure-html/unnamed-chunk-16-1.jpeg" style="display: block; margin: auto;" /><img src="index_files/figure-html/unnamed-chunk-16-2.jpeg" style="display: block; margin: auto;" /><img src="index_files/figure-html/unnamed-chunk-16-3.jpeg" style="display: block; margin: auto;" /><img src="index_files/figure-html/unnamed-chunk-16-4.jpeg" style="display: block; margin: auto;" />
### Modal Progression analysis with `ELEFAN` to estimated `Linf`and `K`

Method based on @Pauly1987 and @Mildenberger2017

#### Krill Male


```r
#### Parameters to `SSWI`

# run ELEFAN with simulated annealing
res_SAsswi <- ELEFAN_SA(lfq_results$SSIW, 
                    SA_time = 60*0.5, 
                    MA = 5, 
                    agemax = 5,
                    seasonalised = TRUE, addl.sqrt = TRUE,
                    init_par = list(Linf = 6, 
                                    K = 0.5, 
                                    t_anchor = 0.5,
                                    C=0.5, 
                                    ts = 0.5),
                    low_par = list(Linf = 5,
                                   K = 0.01,
                                   t_anchor = 0, 
                                   C = 0, 
                                   ts = 0),
                    up_par = list(Linf = 7.5,
                                  K = 1, 
                                  t_anchor = 1,
                                  C = 1, ts = 1))
```

```
## Simulated annealing is running. 
## This will take approximately 0.5 minutes.
## timeSpan = 30.012484 maxTime = 30
## Emini is: -0.2570441366
## xmini are:
## 6.418895303 0.7678352515 0.5033732662 0.490536049 0.8187059425 
## Totally it used 30.012503 secs
## No. of function call is: 1116
```

```r
# run ELEFAN with genetic algorithm
res_GAsswi <- ELEFAN_GA(lfq_results$SSIW, 
                    MA = 5, 
                    seasonalised = TRUE, 
                    maxiter = 10, 
                    agemax = 5,
                    addl.sqrt = TRUE,
                     low_par = list(Linf = 5,
                                   K = 0.5,
                                   t_anchor = 0, 
                                   C = 0, 
                                   ts = 0),
                    up_par = list(Linf = 7.5,
                                  K = 1, 
                                  t_anchor = 1,
                                  C = 1, ts = 1),
                    monitor = FALSE)
```

```
## Genetic algorithm is running. This might take some time.
```

<img src="index_files/figure-html/unnamed-chunk-17-1.jpeg" style="display: block; margin: auto;" />

```r
#### Parameters to `EI`

# run ELEFAN with simulated annealing
res_SAei <- ELEFAN_SA(lfq_results$EI, 
                    SA_time = 60*0.5, 
                    MA = 5, 
                    agemax = 5,
                    seasonalised = TRUE, addl.sqrt = TRUE,
                    init_par = list(Linf = 6, 
                                    K = 0.5, 
                                    t_anchor = 0.5,
                                    C=0.5, 
                                    ts = 0.5),
                    low_par = list(Linf = 5,
                                   K = 0.01,
                                   t_anchor = 0, 
                                   C = 0, 
                                   ts = 0),
                    up_par = list(Linf = 7.5,
                                  K = 1, 
                                  t_anchor = 1,
                                  C = 1, ts = 1))
```

```
## Simulated annealing is running. 
## This will take approximately 0.5 minutes.
## timeSpan = 30.008584 maxTime = 30
## Emini is: -0.2367092272
## xmini are:
## 7.314306291 0.9412915653 0.6182883382 0.7314349413 0.4690019973 
## Totally it used 30.008602 secs
## No. of function call is: 1333
```

```r
# run ELEFAN with genetic algorithm
res_GAei <- ELEFAN_GA(lfq_results$EI, 
                    MA = 5, 
                    seasonalised = TRUE, 
                    maxiter = 10, 
                    agemax = 5,
                    addl.sqrt = TRUE,
                     low_par = list(Linf = 5,
                                   K = 0.5,
                                   t_anchor = 0, 
                                   C = 0, 
                                   ts = 0),
                    up_par = list(Linf = 7.5,
                                  K = 1, 
                                  t_anchor = 1,
                                  C = 1, ts = 1),
                    monitor = FALSE)
```

```
## Genetic algorithm is running. This might take some time.
```

<img src="index_files/figure-html/unnamed-chunk-17-2.jpeg" style="display: block; margin: auto;" />

```r
#### Parameters to `BS`

# run ELEFAN with simulated annealing
res_SAbs <- ELEFAN_SA(lfq_results$BS, 
                    SA_time = 60*0.5, 
                    MA = 5, 
                    agemax = 5,
                    seasonalised = TRUE, addl.sqrt = TRUE,
                   init_par = list(Linf = 6, 
                                    K = 0.5, 
                                    t_anchor = 0.5,
                                    C=0.5, 
                                    ts = 0.5),
                    low_par = list(Linf = 5,
                                   K = 0.01,
                                   t_anchor = 0, 
                                   C = 0, 
                                   ts = 0),
                    up_par = list(Linf = 7.5,
                                  K = 1, 
                                  t_anchor = 1,
                                  C = 1, ts = 1))
```

```
## Simulated annealing is running. 
## This will take approximately 0.5 minutes.
## timeSpan = 30.001463 maxTime = 30
## Emini is: -0.3182638225
## xmini are:
## 5.593892905 0.604283656 0.1065582037 0.643085584 0.9565868378 
## Totally it used 30.001483 secs
## No. of function call is: 1233
```

```r
# run ELEFAN with genetic algorithm
res_GAbs <- ELEFAN_GA(lfq_results$BS, 
                    MA = 5, 
                    seasonalised = TRUE, 
                    maxiter = 10, 
                    agemax = 5,
                    addl.sqrt = TRUE,
                    low_par = list(Linf = 5,
                                   K = 0.01,
                                   t_anchor = 0, 
                                   C = 0, 
                                   ts = 0),
                    up_par = list(Linf = 7.5,
                                  K = 1, 
                                  t_anchor = 1,
                                  C = 1, ts = 1),
                    monitor = FALSE)
```

```
## Genetic algorithm is running. This might take some time.
```

<img src="index_files/figure-html/unnamed-chunk-17-3.jpeg" style="display: block; margin: auto;" />

```r
#### Parameters to `GERLASHE`

# run ELEFAN with simulated annealing
res_SAgs <- ELEFAN_SA(lfq_results$GERLASHE, 
                    SA_time = 60*0.5, 
                    MA = 5, 
                    agemax = 5,
                    seasonalised = TRUE, addl.sqrt = TRUE,
                    init_par = list(Linf = 6, 
                                    K = 0.5, 
                                    t_anchor = 0.5,
                                    C=0.5, 
                                    ts = 0.5),
                    low_par = list(Linf = 5,
                                   K = 0.01,
                                   t_anchor = 0, 
                                   C = 0, 
                                   ts = 0),
                    up_par = list(Linf = 7.5,
                                  K = 1, 
                                  t_anchor = 1,
                                  C = 1, ts = 1))
```

```
## Simulated annealing is running. 
## This will take approximately 0.5 minutes.
## timeSpan = 30.004772 maxTime = 30
## Emini is: -0.2869010907
## xmini are:
## 5.789427124 0.7127949446 0.4013525844 0.5469309241 0.1102455743 
## Totally it used 30.00479 secs
## No. of function call is: 1696
```

```r
# run ELEFAN with genetic algorithm
res_GAgs <- ELEFAN_GA(lfq_results$GERLASHE, 
                    MA = 5, 
                    seasonalised = TRUE, 
                    maxiter = 10, 
                    agemax = 5,
                    addl.sqrt = TRUE,
                    low_par = list(Linf = 5,
                                   K = 0.01,
                                   t_anchor = 0, 
                                   C = 0, 
                                   ts = 0),
                    up_par = list(Linf = 7.5,
                                  K = 1, 
                                  t_anchor = 1,
                                  C = 1, ts = 1),
                    monitor = FALSE)
```

```
## Genetic algorithm is running. This might take some time.
```

<img src="index_files/figure-html/unnamed-chunk-17-4.jpeg" style="display: block; margin: auto;" />
#### Krill Female



```r
#### Parameters to `SSWI`

# run ELEFAN with simulated annealing
res_SAsswihe <- ELEFAN_SA(lfq_resultshe$SSIW, 
                    SA_time = 60*0.5, 
                    MA = 5, 
                    agemax = 5,
                    seasonalised = TRUE, addl.sqrt = TRUE,
                    init_par = list(Linf = 6, 
                                    K = 0.5, 
                                    t_anchor = 0.5,
                                    C=0.5, 
                                    ts = 0.5),
                    low_par = list(Linf = 5,
                                   K = 0.01,
                                   t_anchor = 0, 
                                   C = 0, 
                                   ts = 0),
                    up_par = list(Linf = 7.5,
                                  K = 1, 
                                  t_anchor = 1,
                                  C = 1, ts = 1))
```

```
## Simulated annealing is running. 
## This will take approximately 0.5 minutes.
## timeSpan = 30.008171 maxTime = 30
## Emini is: -0.2316358367
## xmini are:
## 6.003211424 0.6594774183 0.1511300988 0.1896827698 0.9636289105 
## Totally it used 30.00819 secs
## No. of function call is: 1176
```

```r
# run ELEFAN with genetic algorithm
res_GAsswihe <- ELEFAN_GA(lfq_resultshe$SSIW, 
                    MA = 5, 
                    seasonalised = TRUE, 
                    maxiter = 10, 
                    agemax = 5,
                    addl.sqrt = TRUE,
                     low_par = list(Linf = 5,
                                   K = 0.01,
                                   t_anchor = 0, 
                                   C = 0, 
                                   ts = 0),
                    up_par = list(Linf = 7.5,
                                  K = 1, 
                                  t_anchor = 1,
                                  C = 1, ts = 1),
                    monitor = FALSE)
```

```
## Genetic algorithm is running. This might take some time.
```

<img src="index_files/figure-html/unnamed-chunk-18-1.jpeg" style="display: block; margin: auto;" />

```r
#### Parameters to `EI`

# run ELEFAN with simulated annealing
res_SAeihe <- ELEFAN_SA(lfq_resultshe$EI, 
                    SA_time = 60*0.5, 
                    MA = 5, 
                    agemax = 5,
                    seasonalised = TRUE, addl.sqrt = TRUE,
                    init_par = list(Linf = 6, 
                                    K = 0.5, 
                                    t_anchor = 0.5,
                                    C=0.5, 
                                    ts = 0.5),
                    low_par = list(Linf = 5,
                                   K = 0.01,
                                   t_anchor = 0, 
                                   C = 0, 
                                   ts = 0),
                    up_par = list(Linf = 7.5,
                                  K = 1, 
                                  t_anchor = 1,
                                  C = 1, ts = 1))
```

```
## Simulated annealing is running. 
## This will take approximately 0.5 minutes.
## timeSpan = 30.020357 maxTime = 30
## Emini is: -0.2964366709
## xmini are:
## 5.585338324 0.9580215203 0.2021615843 0.7399474084 0.666007027 
## Totally it used 30.020379 secs
## No. of function call is: 1319
```

```r
# run ELEFAN with genetic algorithm
res_GAeihe <- ELEFAN_GA(lfq_resultshe$EI, 
                    MA = 5, 
                    seasonalised = TRUE, 
                    maxiter = 10, 
                    agemax = 5,
                    addl.sqrt = TRUE,
                     low_par = list(Linf = 5,
                                   K = 0.01,
                                   t_anchor = 0, 
                                   C = 0, 
                                   ts = 0),
                    up_par = list(Linf = 7.5,
                                  K = 1, 
                                  t_anchor = 1,
                                  C = 1, ts = 1),
                    monitor = FALSE)
```

```
## Genetic algorithm is running. This might take some time.
```

<img src="index_files/figure-html/unnamed-chunk-18-2.jpeg" style="display: block; margin: auto;" />

```r
#### Parameters to `BS`


# run ELEFAN with simulated annealing
res_SAbshe <- ELEFAN_SA(lfq_resultshe$BS, 
                    SA_time = 60*0.5, 
                    MA = 5, 
                    agemax = 5,
                    seasonalised = TRUE, addl.sqrt = TRUE,
                   init_par = list(Linf = 6, 
                                    K = 0.5, 
                                    t_anchor = 0.5,
                                    C=0.5, 
                                    ts = 0.5),
                    low_par = list(Linf = 5,
                                   K = 0.01,
                                   t_anchor = 0, 
                                   C = 0, 
                                   ts = 0),
                    up_par = list(Linf = 7.5,
                                  K = 1, 
                                  t_anchor = 1,
                                  C = 1, ts = 1))
```

```
## Simulated annealing is running. 
## This will take approximately 0.5 minutes.
## timeSpan = 30.016515 maxTime = 30
## Emini is: -0.4349139705
## xmini are:
## 5.923948187 0.8310357623 0.5259462683 0.3320332617 0.281676233 
## Totally it used 30.016542 secs
## No. of function call is: 1183
```

```r
# run ELEFAN with genetic algorithm
res_GAbshe <- ELEFAN_GA(lfq_resultshe$BS, 
                    MA = 5, 
                    seasonalised = TRUE, 
                    maxiter = 10, 
                    agemax = 5,
                    addl.sqrt = TRUE,
                    low_par = list(Linf = 5,
                                   K = 0.01,
                                   t_anchor = 0, 
                                   C = 0, 
                                   ts = 0),
                    up_par = list(Linf = 7.5,
                                  K = 1, 
                                  t_anchor = 1,
                                  C = 1, ts = 1),
                    monitor = FALSE)
```

```
## Genetic algorithm is running. This might take some time.
```

<img src="index_files/figure-html/unnamed-chunk-18-3.jpeg" style="display: block; margin: auto;" />

```r
#### Parameters to `GERLASHE`


# run ELEFAN with simulated annealing
res_SAgshe <- ELEFAN_SA(lfq_resultshe$GERLASHE, 
                    SA_time = 60*0.5, 
                    MA = 5, 
                    agemax = 5,
                    seasonalised = TRUE, addl.sqrt = TRUE,
                    init_par = list(Linf = 6, 
                                    K = 0.5, 
                                    t_anchor = 0.5,
                                    C=0.5, 
                                    ts = 0.5),
                    low_par = list(Linf = 5,
                                   K = 0.01,
                                   t_anchor = 0, 
                                   C = 0, 
                                   ts = 0),
                    up_par = list(Linf = 7.5,
                                  K = 1, 
                                  t_anchor = 1,
                                  C = 1, ts = 1))
```

```
## Simulated annealing is running. 
## This will take approximately 0.5 minutes.
## timeSpan = 30.0125 maxTime = 30
## Emini is: -0.3825402709
## xmini are:
## 5.659959408 0.9045192397 0.9905485124 0.03362630308 0.549246957 
## Totally it used 30.012521 secs
## No. of function call is: 1570
```

```r
# run ELEFAN with genetic algorithm
res_GAgshe <- ELEFAN_GA(lfq_resultshe$GERLASHE, 
                    MA = 5, 
                    seasonalised = TRUE, 
                    maxiter = 10, 
                    agemax = 5,
                    addl.sqrt = TRUE,
                    low_par = list(Linf = 5,
                                   K = 0.01,
                                   t_anchor = 0, 
                                   C = 0, 
                                   ts = 0),
                    up_par = list(Linf = 7.5,
                                  K = 1, 
                                  t_anchor = 1,
                                  C = 1, ts = 1),
                    monitor = FALSE)
```

```
## Genetic algorithm is running. This might take some time.
```

<img src="index_files/figure-html/unnamed-chunk-18-4.jpeg" style="display: block; margin: auto;" />

Resuming parameters to both sex  by strata to krill

```r
#male
GAbs <- unlist(res_GAbs$par)
SAbs <- unlist(res_SAbs$par)
GAei <- unlist(res_GAei$par)
SAei <- unlist(res_SAei$par)
GAsswi <- unlist(res_GAsswi$par)
SAsswi <- unlist(res_SAsswi$par)
GAgs <- unlist(res_GAgs$par)
SAgs <- unlist(res_SAgs$par)
#female
GAbshe <- unlist(res_GAbshe$par)
SAbshe <- unlist(res_SAbshe$par)
GAeihe <- unlist(res_GAeihe$par)
SAeihe <- unlist(res_SAeihe$par)
GAsswihe <- unlist(res_GAsswihe$par)
SAsswihe <- unlist(res_SAsswihe$par)
GAgshe <- unlist(res_GAgshe$par)
SAgshe <- unlist(res_SAgshe$par)
# join 
t_k_linf_m <- rbind(GAbs[1:2],
                  SAbs[1:2],
                  GAei[1:2],
                  SAei[1:2],
                  GAsswi[1:2],
                  SAsswi[1:2],
                  GAgs[1:2],
                  SAgs[1:2])
t_k_linf_f <- rbind(GAbshe[1:2],
                  SAbshe[1:2],
                  GAeihe[1:2],
                  SAeihe[1:2],
                  GAsswihe[1:2],
                  SAsswihe[1:2],
                  GAgshe[1:2],
                  SAgshe[1:2])
row_names <- c("GA BS", "SA BS", "GA EI", 
                 "SA EI", "GA SSWI", "SA SSWI",
                 "GA GS", "SA GS")
rownames(t_k_linf_m) <- row_names
rownames(t_k_linf_f) <- row_names
col_names_m <- c("L inf Male", "K Male")
col_names_f <- c("L inf Female", "K Female")
colnames(t_k_linf_m) <- col_names_m
colnames(t_k_linf_f) <- col_names_f


total_para <- round(cbind(t_k_linf_f,
                    t_k_linf_m),3)
t_k_linf_total <- as_tibble(total_para, rownames = "group")


means <- t_k_linf_total %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
  mutate(group = "Mean")

t_k_linf <- bind_rows(t_k_linf_total, means)
```


```r
write_csv(t_k_linf, "parametros_krill.csv")
```


total Table with `L_inf_` and `K`


```r
t_k_linf %>%
  separate(group, into = c("METHOD", "STRATA"), sep = " ") %>% 
  kbl(booktabs = T,
      position="ht!",
    caption = "Parametres LH in different strata with 2 algoritms to estimation") %>%
  kable_styling(latex_options = c("striped",
                                  "condensed"),
                full_width = FALSE)
```

<table class="table" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Parametres LH in different strata with 2 algoritms to estimation</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> METHOD </th>
   <th style="text-align:left;"> STRATA </th>
   <th style="text-align:right;"> L inf Female </th>
   <th style="text-align:right;"> K Female </th>
   <th style="text-align:right;"> L inf Male </th>
   <th style="text-align:right;"> K Male </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> GA </td>
   <td style="text-align:left;"> BS </td>
   <td style="text-align:right;"> 6.058000 </td>
   <td style="text-align:right;"> 0.6740 </td>
   <td style="text-align:right;"> 6.018000 </td>
   <td style="text-align:right;"> 0.726000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SA </td>
   <td style="text-align:left;"> BS </td>
   <td style="text-align:right;"> 5.924000 </td>
   <td style="text-align:right;"> 0.8310 </td>
   <td style="text-align:right;"> 5.594000 </td>
   <td style="text-align:right;"> 0.604000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GA </td>
   <td style="text-align:left;"> EI </td>
   <td style="text-align:right;"> 6.675000 </td>
   <td style="text-align:right;"> 0.5570 </td>
   <td style="text-align:right;"> 6.865000 </td>
   <td style="text-align:right;"> 0.834000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SA </td>
   <td style="text-align:left;"> EI </td>
   <td style="text-align:right;"> 5.585000 </td>
   <td style="text-align:right;"> 0.9580 </td>
   <td style="text-align:right;"> 7.314000 </td>
   <td style="text-align:right;"> 0.941000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GA </td>
   <td style="text-align:left;"> SSWI </td>
   <td style="text-align:right;"> 6.487000 </td>
   <td style="text-align:right;"> 0.7600 </td>
   <td style="text-align:right;"> 5.739000 </td>
   <td style="text-align:right;"> 0.718000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SA </td>
   <td style="text-align:left;"> SSWI </td>
   <td style="text-align:right;"> 6.003000 </td>
   <td style="text-align:right;"> 0.6590 </td>
   <td style="text-align:right;"> 6.419000 </td>
   <td style="text-align:right;"> 0.768000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GA </td>
   <td style="text-align:left;"> GS </td>
   <td style="text-align:right;"> 6.009000 </td>
   <td style="text-align:right;"> 0.6840 </td>
   <td style="text-align:right;"> 6.719000 </td>
   <td style="text-align:right;"> 0.355000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SA </td>
   <td style="text-align:left;"> GS </td>
   <td style="text-align:right;"> 5.660000 </td>
   <td style="text-align:right;"> 0.9050 </td>
   <td style="text-align:right;"> 5.789000 </td>
   <td style="text-align:right;"> 0.713000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mean </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 6.050125 </td>
   <td style="text-align:right;"> 0.7535 </td>
   <td style="text-align:right;"> 6.307125 </td>
   <td style="text-align:right;"> 0.707375 </td>
  </tr>
</tbody>
</table>

Made VB curves by methot and strata by male

separated group



```r
t_k_linf_sep <- t_k_linf %>% 
  filter(group != "Mean") %>% 
   separate(group, into = c("METHOD", "STRATA"), sep = " ")
```



```r
# Definir la función de Von Bertalanffy
von_bertalanffy <- function(t, L_inf, K) {
  L_inf * (1 - exp(-K * (t-t0)))
}

# Generar una secuencia de tiempos (por ejemplo, de 0 a 5 años)
t0 <- -0.3
time <- seq(0, 7, by = 0.1)

# Calcular longitud esperada para cada zona y tiempo
df_curves_male <- t_k_linf_sep %>%
  dplyr::select(c(1:2, 5:6)) %>% 
  rowwise() %>%
  mutate(length = list(von_bertalanffy(time, `L inf Male`, `K Male`))) %>%
  unnest(length)
# Agregar el vector de tiempo al DataFrame df_curves
df_curves_male$time <- rep(time, nrow(t_k_linf_sep))
# Graficar con ggplot
male <- ggplot(df_curves_male,
               aes(x = time, 
                              y = length,
                              group=STRATA,
                              colour=STRATA)) +
  geom_line(linewidth=1.1) +
  labs(title = "Male",
       x = "years",
       y = "Length (cm)") +
  theme_few()+
  facet_wrap(.~METHOD)+
  scale_colour_viridis_d(option="H",
                         name="Strata")
```
Made VB curves by methot and strata by female


```r
# Calcular longitud esperada para cada zona y tiempo
df_curves_female <- t_k_linf_sep %>%
  dplyr::select(c(1:4)) %>% 
  rowwise() %>%
  mutate(length = list(von_bertalanffy(time, `L inf Female`, `K Female`))) %>%
  unnest(length)
# Agregar el vector de tiempo al DataFrame df_curves
df_curves_female$time <- rep(time, nrow(t_k_linf_sep))
# Graficar con ggplot
female <- ggplot(df_curves_female,
               aes(x = time, 
                              y = length,
                              group=STRATA,
                              colour=STRATA)) +
  geom_line(linewidth=1.1) +
  labs(title = "Female",
       x = "years",
       y = "Length (cm)") +
  theme_few()+
  facet_wrap(.~METHOD)+
  scale_colour_viridis_d(option="H",
                         name="Strata")
```

both plot


```r
ggarrange(female, male, common.legend = TRUE,
          ncol=1,
          legend="bottom")
```

<img src="index_files/figure-html/unnamed-chunk-25-1.jpeg" style="display: block; margin: auto;" />


### Method NO LINEAR MODEL 


We use firts a descomposition methot with library `mixR` and then we use `nlmer` (@Bates2015) to calcularte parametrs


```r
sf52020 <- sf5 %>% 
  filter(sex_code %in% c("F", "M")) %>% 
  drop_na(length_total_cm)

# Lista para almacenar los modelos
modelos <- list()

# Lista de estratos y sexos
estratos <- unique(sf52020$ID)
sexos <- unique(sf52020$sex_code)

# Bucle para ajustar modelos para cada combinación de estrato y sexo
for (estrato in estratos) {
  for (sexo in sexos) {
    # Filtrar los datos para el estrato y sexo actual
    datos_subset <- sf52020%>% filter(ID == estrato, sex_code == sexo)
    
    # Aplicar mixfit y guardar el modelo en la lista
    mod <- mixfit(datos_subset$length_total_cm, ncomp = 4,
                   pi = c(0.5, 0.5, 0.5, 0.5), 
                   mu = c(3, 4, 5, 6), 
                   sd = c(0.2, 0.2, 0.2, 0.2))
    
    modelos[[paste(estrato, sexo, sep = "_")]] <- mod
  }
}

# Puedes acceder a cada modelo usando modelos$nombre_del_estrato_y_sexo
# Por ejemplo, para acceder al modelo para el estrato "A" y sexo "M":
# modelos$A_M

# compruebo el n de comp

s_weibull = select(sf5a$length_total_cm, ncomp = 2:6, family = 'weibull')

s_normal = select(sf5a$length_total_cm, ncomp = 2:6)




# fit a Normal mixture model
mod1 = mixfit(sf5a$length_total_cm, ncomp = 4,
              pi = c(0.5, 0.5, 0.5, 0.5), 
              mu = c( 3, 4, 5, 6), 
              sd = c(0.2, 0.2, 0.2, 0.2))
mod2_weibull = mixfit(sf6$length_total_cm, family = 'weibull', ncomp = 3)

s_weibull = select(sf5a$length_total_cm, ncomp = 2:6, family = 'weibull')

s_normal = select(sf5a$length_total_cm, ncomp = 2:6)
plot(s_weibull)
plot(s_normal)

# plot the fitted model# plot the fitted model# plot the fitted model
plot(mod1)
plot(mod2_weibull)

# fit a Normal mixture model (equal variance)
mod1_ev = mixfit(sf6$length_total_cm, ncomp = 2, ev = TRUE)
```

## Natural Mortality in Krill

In this exercises different bioanalogic methods are tested with General Algoritm

#### MALE


```r
MSSWI <- c(res_GAsswi$par, list(agemax = res_GAsswi$agemax))
# use the function M_empirical to estimate natural mortality
Msswi <- M_empirical(Linf = MSSWI$Linf, K_l = MSSWI$K, 
                  tmax = MSSWI$agemax, temp = 5,
                  tm50 = 2,
                  method = c("Pauly_Linf", "Hoenig", 
                            "Hoenig",
                             "AlversonCarney",
                             "RikhterEfanov"
                             ))
# Bransfield
BS <- c(res_GAbs$par, list(agemax = res_GAbs$agemax))

# use the function M_empirical to estimate natural mortality
Mbs <- M_empirical(Linf = BS$Linf, K_l = BS$K, 
                  tmax = BS$agemax, temp = 5,
                  tm50 = 2,
                  method = c("Pauly_Linf", "Hoenig", 
                              "Hoenig",
                             "AlversonCarney",
                             "RikhterEfanov"
                             ))
# GERLASHE
GS <- c(res_GAgs$par, list(agemax = res_GAgs$agemax))

# use the function M_empirical to estimate natural mortality
Mgs <- M_empirical(Linf = GS$Linf, K_l = GS$K, 
                  tmax = GS$agemax, temp = 5,
                   tm50 = 2,
                  method = c("Pauly_Linf", "Hoenig", 
                             "Hoenig",
                             "AlversonCarney",
                             "RikhterEfanov"
                             ))
# Elephand Island
MEI <- c(res_GAei$par, list(agemax = res_GAei$agemax))

# use the function M_empirical to estimate natural mortality
Mei <- M_empirical(Linf = MEI$Linf, K_l = MEI$K, 
                  tmax = MEI$agemax, temp = 5,
                  tm50 = 2,
                  method = c("Pauly_Linf", "Hoenig", 
                             "Hoenig",
                             "AlversonCarney",
                             "RikhterEfanov"
                             ))
```

#### FEMALE


```r
MSSWIhe <- c(res_GAsswihe$par, list(agemax = res_GAsswihe$agemax))
# use the function M_empirical to estimate natural mortality
Msswihe <- M_empirical(Linf = MSSWIhe$Linf, K_l = MSSWIhe$K, 
                  tmax = MSSWIhe$agemax, temp = 5,
                  tm50 = 2,
                  method = c("Pauly_Linf", 
                            "Hoenig",
                             "AlversonCarney",
                             "RikhterEfanov"
                             ))
# Bransfield
BShe <- c(res_GAbshe$par, list(agemax = res_GAbshe$agemax))

# use the function M_empirical to estimate natural mortality
Mbshe <- M_empirical(Linf = BShe$Linf, K_l = BShe$K, 
                  tmax = BShe$agemax, temp = 5,
                  tm50 = 2,
                  method = c("Pauly_Linf", 
                              "Hoenig",
                             "AlversonCarney",
                             "RikhterEfanov"
                             ))
# GERLASHE
GShe <- c(res_GAgshe$par, list(agemax = res_GAgshe$agemax))

# use the function M_empirical to estimate natural mortality
Mgshe <- M_empirical(Linf = GShe$Linf, K_l = GShe$K, 
                  tmax = GShe$agemax, temp = 5,
                   tm50 = 2,
                  method = c("Pauly_Linf", 
                             "Hoenig",
                             "AlversonCarney",
                             "RikhterEfanov"
                             ))
# Elephand Island
MEIhe <- c(res_GAeihe$par, list(agemax = res_GAeihe$agemax))

# use the function M_empirical to estimate natural mortality
Meihe <- M_empirical(Linf = MEIhe$Linf, K_l = MEIhe$K, 
                  tmax = MEIhe$agemax, temp = 5,
                  tm50 = 2,
                  method = c("Pauly_Linf", 
                             "Hoenig",
                             "AlversonCarney",
                             "RikhterEfanov"
                             ))
```


```r
# junto las bases

Total_M_male <- cbind(Mei, Mbs, Mgs, Msswi)
colnames(Total_M_male) <- c("EI", "BS" , "GS", "SSWI")
mean_m <- colMeans(Total_M_male, na.rm = TRUE)
Total_M_Mean_male <- rbind(Total_M_male, Mean = mean_m)
# Agregar los promedios como una nueva fila al final del array

Total_M_femal <- cbind(Meihe, Mbshe, Mgshe, Msswihe)
colnames(Total_M_femal) <- c("EI", "BS" , "GS", "SSWI")
mean_m <- colMeans(Total_M_femal, na.rm = TRUE)
Total_M_Mean_femal <- rbind(Total_M_femal, Mean = mean_m)
```



```r
Total_M_Mean_male  %>%
  kbl(booktabs = T,
      position="ht!",
    caption = "Estimated M by Strata for Male") %>%
  kable_styling(latex_options = c("striped",
                                  "condensed"),
                full_width = FALSE)
```

<table class="table" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Estimated M by Strata for Male</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> EI </th>
   <th style="text-align:right;"> BS </th>
   <th style="text-align:right;"> GS </th>
   <th style="text-align:right;"> SSWI </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Alverson and Carney (1975) </td>
   <td style="text-align:right;"> 0.6450 </td>
   <td style="text-align:right;"> 0.7330 </td>
   <td style="text-align:right;"> 1.106 </td>
   <td style="text-align:right;"> 0.7400 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hoenig (1983) - Joint Equation </td>
   <td style="text-align:right;"> 0.8690 </td>
   <td style="text-align:right;"> 0.8690 </td>
   <td style="text-align:right;"> 0.869 </td>
   <td style="text-align:right;"> 0.8690 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hoenig (1983) - Fish Equation </td>
   <td style="text-align:right;"> 0.8470 </td>
   <td style="text-align:right;"> 0.8470 </td>
   <td style="text-align:right;"> 0.847 </td>
   <td style="text-align:right;"> 0.8470 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pauly (1980) - Length Equation </td>
   <td style="text-align:right;"> 1.0770 </td>
   <td style="text-align:right;"> 1.0210 </td>
   <td style="text-align:right;"> 0.620 </td>
   <td style="text-align:right;"> 1.0270 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rikhter and Efanov (1976) </td>
   <td style="text-align:right;"> 0.7680 </td>
   <td style="text-align:right;"> 0.7680 </td>
   <td style="text-align:right;"> 0.768 </td>
   <td style="text-align:right;"> 0.7680 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mean </td>
   <td style="text-align:right;"> 0.8412 </td>
   <td style="text-align:right;"> 0.8476 </td>
   <td style="text-align:right;"> 0.842 </td>
   <td style="text-align:right;"> 0.8502 </td>
  </tr>
</tbody>
</table>

```r
Total_M_Mean_femal  %>%
  kbl(booktabs = T,
      position="ht!",
    caption = "Estimated M by Strata for Female") %>%
  kable_styling(latex_options = c("striped",
                                  "condensed"),
                full_width = FALSE)
```

<table class="table" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Estimated M by Strata for Female</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> EI </th>
   <th style="text-align:right;"> BS </th>
   <th style="text-align:right;"> GS </th>
   <th style="text-align:right;"> SSWI </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Alverson and Carney (1975) </td>
   <td style="text-align:right;"> 0.8880 </td>
   <td style="text-align:right;"> 0.7780 </td>
   <td style="text-align:right;"> 0.769 </td>
   <td style="text-align:right;"> 0.7040 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hoenig (1983) - Joint Equation </td>
   <td style="text-align:right;"> 0.8690 </td>
   <td style="text-align:right;"> 0.8690 </td>
   <td style="text-align:right;"> 0.869 </td>
   <td style="text-align:right;"> 0.8690 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hoenig (1983) - Fish Equation </td>
   <td style="text-align:right;"> 0.8470 </td>
   <td style="text-align:right;"> 0.8470 </td>
   <td style="text-align:right;"> 0.847 </td>
   <td style="text-align:right;"> 0.8470 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pauly (1980) - Length Equation </td>
   <td style="text-align:right;"> 0.8340 </td>
   <td style="text-align:right;"> 0.9710 </td>
   <td style="text-align:right;"> 0.982 </td>
   <td style="text-align:right;"> 1.0300 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rikhter and Efanov (1976) </td>
   <td style="text-align:right;"> 0.7680 </td>
   <td style="text-align:right;"> 0.7680 </td>
   <td style="text-align:right;"> 0.768 </td>
   <td style="text-align:right;"> 0.7680 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mean </td>
   <td style="text-align:right;"> 0.8412 </td>
   <td style="text-align:right;"> 0.8466 </td>
   <td style="text-align:right;"> 0.847 </td>
   <td style="text-align:right;"> 0.8436 </td>
  </tr>
</tbody>
</table>


Plot to female

```r
# Convertir la matriz a data.frame
df_female <- as.data.frame(Total_M_Mean_femal)

# Agregar la columna de métodos
df_female$Method <- rownames(Total_M_Mean_femal)

# Convertir el data.frame a formato largo
df_long <- df_female %>%
  pivot_longer(cols = EI:SSWI, 
               names_to = "Stratum", 
               values_to = "Value")

# Dot Plot
m_female<- ggplot(df_long %>%
                              drop_na() %>%
                              filter(Method != "Mean"), 
                            aes(x = Stratum, 
                                y = Value,
                                fill = Method)) +
    geom_point(size = 3,     
              shape = 21,   
              color = "black") + 
  geom_text_repel(aes(label = round(Value, 3)),  
                  size = 3,                      
                  box.padding = 0.35,            
                  point.padding = 0.5,         
                  segment.color = 'grey50',
                  min.segment.length = 0, 
                  nudge_y = 0.05,   
                  direction = "both") +
  labs(title = "Female",
       x = "",
       y = "Natural Mortality",
       fill = "Method") +
  theme_few() +
  scale_fill_viridis_d(option = "H") +
  ylim(0, 2)
```

Plot to male

```r
# Convertir la matriz a data.frame
df_male <- as.data.frame(Total_M_Mean_male)

# Agregar la columna de métodos
df_male$Method <- rownames(Total_M_Mean_male)

# Convertir el data.frame a formato largo
df_long_male <- df_male %>%
  pivot_longer(cols = EI:SSWI, 
               names_to = "Stratum", 
               values_to = "Value") 

# Dot Plot
m_male<- ggplot(df_long_male %>%
                              drop_na() %>%
                              filter(Method != "Mean"), 
                            aes(x = Stratum, 
                                y = Value,
                                fill = Method)) +
  geom_point(size = 3,     
              shape = 21,   
              color = "black") +  
  geom_text_repel(aes(label = round(Value, 3)),  
                  size = 3,                      
                  box.padding = 0.35,            
                  point.padding = 0.5,         
                  segment.color = 'grey50',
                  min.segment.length = 0, 
                  nudge_y = 0.05,   
                  direction = "both") +
  labs(title = "Male",
       x = "",
       y = "Natural Mortality",
       fill = "Method") +
  theme_few() +
  scale_fill_viridis_d(option = "H") +
  ylim(0, 2)
```

both plot


```r
ggarrange(m_female, m_male, 
          common.legend = TRUE,
          ncol=1,
          legend="bottom")
```

<img src="index_files/figure-html/unnamed-chunk-33-1.jpeg" style="display: block; margin: auto;" />
Statistical diferences




# References


