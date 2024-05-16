---
title: "Derive growth parameters and Natural Mortality in krill considering spatial heterogenity"
subtitle: "Working Paper to be submitted in a CCAMLR EMM-WG 2024"
author: "Mardones, M; Cárdenas, C."
date:  "16 May, 2024"
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

# Background

# Methodology



A important piece of information for a stock evaluation refers to the biological components such as average sizes and weights across areas and years. To do this, we will explore the biological data and prepare the output to add it into stock assessment integrate model [@Methot2013].

The object `ohbio2` come from data exploration analysis in data request
CCAMLR data. This objetc have bio information from krill.




```r
#cargo objeto
meta <- get("METADATA")
c1 <- get("C1")
ohbio <- get("OBS_HAUL_BIOLOGY")
```

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

Firsts glance. Test how many register have by year. In this case,
`length_total_cm` by season ccamlr. Same exercise in date period
`date_catchperiod_start` to separate dates.


```r
ohbio3 <- ohbio2 %>%
  mutate(Year = year(date_catchperiod_start),
         Month = month(date_catchperiod_start),
         Day = day(date_catchperiod_start)) %>% 
  #toupper() para convertir los valores a mayúsculas
  mutate(sex_code = toupper(sex_code))
```

Save data further analysis

```r
length481 <-ohbio3 %>% 
  dplyr::select(7, 9, 11, 12, 14, 24, 25, 29, 42, 44, 46, 47, 43, 45) |>   filter(asd_code=="481")
#save(length481, file = "length481.RData")
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
names(length481)
```

```
##  [1] "vessel_nationality_code" "season_ccamlr"          
##  [3] "asd_code"                "trawl_technique"        
##  [5] "date_catchperiod_start"  "latitude_set_end"       
##  [7] "longitude_set_end"       "gear_type"              
##  [9] "maturity_stage"          "length_total_cm"        
## [11] "Year"                    "Month"                  
## [13] "sex_code"                "greenweight_kg"
```

```r
ohbio6 <- st_as_sf(length481 %>% 
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

<img src="index_files/figure-html/unnamed-chunk-8-1.jpeg" style="display: block; margin: auto;" />
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

### Method `ELEFAN` 

(@Tobias2017)



```r
sf5fil <- sf5 |>
  filter(ID %in% c("BS", "EI", "GERLASHE", "SSIW")) |>  
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

<img src="index_files/figure-html/unnamed-chunk-11-1.jpeg" style="display: block; margin: auto;" /><img src="index_files/figure-html/unnamed-chunk-11-2.jpeg" style="display: block; margin: auto;" /><img src="index_files/figure-html/unnamed-chunk-11-3.jpeg" style="display: block; margin: auto;" /><img src="index_files/figure-html/unnamed-chunk-11-4.jpeg" style="display: block; margin: auto;" />
Identified distribution


```r
ei <- Bhattacharya(lfq_results$EI)
```

```
## Interactive session needed for Bhattacharya.
```

```r
ss <- Bhattacharya(lfq_results$SSIW)
```

```
## Interactive session needed for Bhattacharya.
```

```r
bs <- Bhattacharya(lfq_results$BS)
```

```
## Interactive session needed for Bhattacharya.
```

```r
gs <- Bhattacharya(lfq_results$GERLASHE)
```

```
## Interactive session needed for Bhattacharya.
```
ahora damos asignaciones a curvas para representación


```r
# Crear una lista para almacenar los resultados de PW_results <- list()

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

<img src="index_files/figure-html/unnamed-chunk-13-1.jpeg" style="display: block; margin: auto;" /><img src="index_files/figure-html/unnamed-chunk-13-2.jpeg" style="display: block; margin: auto;" /><img src="index_files/figure-html/unnamed-chunk-13-3.jpeg" style="display: block; margin: auto;" /><img src="index_files/figure-html/unnamed-chunk-13-4.jpeg" style="display: block; margin: auto;" />



Proceed to fit Modal Progression analysis with `ELEFAN`

#### Parameters to `SSIW`


```r
# Response surface analyss
res_RSAsswi <- ELEFAN(lfq = lfq_results$SSIW,  
                  MA = 5,
                  Linf_range = seq(4.5, 7, 
                                   length.out = 30),
                  K_range = exp(seq(log(0.1),
                                    log(2),
                                    length.out = 30)),
                  C = 0.5,
                  ts = 0.5,
                  method = "cross",
                  cross.date = lfq_results$SSIW$dates[3],
                  cross.midLength = lfq_results$SSIW$midLengths[5],
                  contour = TRUE,
                  add.values = FALSE,
                  agemax = 5,
                  hide.progressbar = TRUE)
```

```
## Optimisation procuedure of ELEFAN is running. 
## This will take some time. 
## The process bar will inform you about the process of the calculations.
```



```r
# run ELEFAN with simulated annealing
res_SAsswi <- ELEFAN_SA(lfq_results$SSIW, 
                    SA_time = 60*0.5, 
                    MA = 5, 
                    agemax = 5,
                    seasonalised = TRUE, addl.sqrt = TRUE,
                    init_par = list(Linf = 4.5, 
                                    K = 0.5, 
                                    t_anchor = 0.5,
                                    C=0.5, 
                                    ts = 0.5),
                    low_par = list(Linf = 4,
                                   K = 0.01,
                                   t_anchor = 0, 
                                   C = 0, 
                                   ts = 0),
                    up_par = list(Linf = 6.5,
                                  K = 1, 
                                  t_anchor = 1,
                                  C = 1, ts = 1))
```

```
## Simulated annealing is running. 
## This will take approximately 0.5 minutes.
## timeSpan = 30.005021 maxTime = 30
## Emini is: -0.2844106025
## xmini are:
## 5.324971939 0.912516579 0.6134328698 0.09638525543 0.3336850045 
## Totally it used 30.005036 secs
## No. of function call is: 1649
```



```r
# run ELEFAN with genetic algorithm
res_GAsswi <- ELEFAN_GA(lfq_results$SSIW, 
                    MA = 5, 
                    seasonalised = TRUE, 
                    maxiter = 10, 
                    agemax = 5,
                    addl.sqrt = TRUE,
                    low_par = list(Linf = 4.5, 
                                   K = 0.01, 
                                   t_anchor = 0, 
                                   C = 0, 
                                   ts = 0),
                    up_par = list(Linf = 6.5, 
                                  K = 1, 
                                  t_anchor = 1,
                                  C = 1, ts = 1),
                    monitor = FALSE)
```

```
## Genetic algorithm is running. This might take some time.
```

<img src="index_files/figure-html/unnamed-chunk-16-1.jpeg" style="display: block; margin: auto;" />

Table with different method


```r
# show results
RSAsswi <- unlist(res_RSAsswi$par)
GAsswi <- unlist(res_GAsswi$par)
SAsswi <- unlist(res_SAsswi$par)
parsswi <- round(rbind(RSAsswi, GAsswi, SAsswi),3)

parsswi %>%
  kbl(booktabs = T,
      position="ht!",
    caption = "Parametres LH to SSWI") %>%
    kable_styling(latex_options = c("striped",
                                  "condensed"),
                full_width = FALSE)
```

<table class="table" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Parametres LH to SSWI</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Linf </th>
   <th style="text-align:right;"> K </th>
   <th style="text-align:right;"> t_anchor </th>
   <th style="text-align:right;"> C </th>
   <th style="text-align:right;"> ts </th>
   <th style="text-align:right;"> phiL </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> RSAsswi </td>
   <td style="text-align:right;"> 5.448 </td>
   <td style="text-align:right;"> 0.640 </td>
   <td style="text-align:right;"> 0.790 </td>
   <td style="text-align:right;"> 0.500 </td>
   <td style="text-align:right;"> 0.500 </td>
   <td style="text-align:right;"> 1.279 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GAsswi </td>
   <td style="text-align:right;"> 5.989 </td>
   <td style="text-align:right;"> 0.579 </td>
   <td style="text-align:right;"> 0.532 </td>
   <td style="text-align:right;"> 0.859 </td>
   <td style="text-align:right;"> 0.880 </td>
   <td style="text-align:right;"> 1.318 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SAsswi </td>
   <td style="text-align:right;"> 5.325 </td>
   <td style="text-align:right;"> 0.913 </td>
   <td style="text-align:right;"> 0.613 </td>
   <td style="text-align:right;"> 0.096 </td>
   <td style="text-align:right;"> 0.334 </td>
   <td style="text-align:right;"> 1.413 </td>
  </tr>
</tbody>
</table>
#### Parameters to `EI`


```r
# Response surface analyss
res_RSAei <- ELEFAN(lfq = lfq_results$EI,  
                  MA = 5,
                  Linf_range = seq(4.5, 7, 
                                   length.out = 30),
                  K_range = exp(seq(log(0.1),
                                    log(2),
                                    length.out = 30)),
                  C = 0.5,
                  ts = 0.5,
                  method = "cross",
                  cross.date = lfq_results$EI$dates[3],
                  cross.midLength = lfq_results$EI$midLengths[5],
                  contour = TRUE,
                  add.values = FALSE,
                  agemax = 5,
                  hide.progressbar = TRUE)
```

```
## Optimisation procuedure of ELEFAN is running. 
## This will take some time. 
## The process bar will inform you about the process of the calculations.
```



```r
# run ELEFAN with simulated annealing
res_SAei <- ELEFAN_SA(lfq_results$EI, 
                    SA_time = 60*0.5, 
                    MA = 5, 
                    agemax = 5,
                    seasonalised = TRUE, addl.sqrt = TRUE,
                    init_par = list(Linf = 4.5, 
                                    K = 0.5, 
                                    t_anchor = 0.5,
                                    C=0.5, 
                                    ts = 0.5),
                    low_par = list(Linf = 4,
                                   K = 0.01,
                                   t_anchor = 0, 
                                   C = 0, 
                                   ts = 0),
                    up_par = list(Linf = 6.5,
                                  K = 1, 
                                  t_anchor = 1,
                                  C = 1, ts = 1))
```

```
## Simulated annealing is running. 
## This will take approximately 0.5 minutes.
## timeSpan = 30.001442 maxTime = 30
## Emini is: -0.2111109108
## xmini are:
## 5.863710068 0.9813904597 0.873453021 0.9993372839 0.5819024704 
## Totally it used 30.001453 secs
## No. of function call is: 2048
```



```r
# run ELEFAN with genetic algorithm
res_GAei <- ELEFAN_GA(lfq_results$EI, 
                    MA = 5, 
                    seasonalised = TRUE, 
                    maxiter = 10, 
                    agemax = 5,
                    addl.sqrt = TRUE,
                    low_par = list(Linf = 4.5, 
                                   K = 0.01, 
                                   t_anchor = 0, 
                                   C = 0, 
                                   ts = 0),
                    up_par = list(Linf = 6.5, 
                                  K = 1, 
                                  t_anchor = 1,
                                  C = 1, ts = 1),
                    monitor = FALSE)
```

```
## Genetic algorithm is running. This might take some time.
```

<img src="index_files/figure-html/unnamed-chunk-20-1.jpeg" style="display: block; margin: auto;" />


```r
# show results
RSAei <- unlist(res_RSAei$par)
GAei <- unlist(res_GAei$par)
SAei <- unlist(res_SAei$par)
parei <- round(rbind(RSAei, GAei, SAei),3)
```



#### Parameters to `BS`


```r
# Response surface analyss
res_RSAbs <- ELEFAN(lfq = lfq_results$BS,  
                  MA = 5,
                  Linf_range = seq(4.5, 7, 
                                   length.out = 30),
                  K_range = exp(seq(log(0.1),
                                    log(2),
                                    length.out = 30)),
                  C = 0.5,
                  ts = 0.5,
                  method = "cross",
                  cross.date = lfq_results$BS$dates[3],
                  cross.midLength = lfq_results$BS$midLengths[5],
                  contour = TRUE,
                  add.values = FALSE,
                  agemax = 5,
                  hide.progressbar = TRUE)
```

```
## Optimisation procuedure of ELEFAN is running. 
## This will take some time. 
## The process bar will inform you about the process of the calculations.
```



```r
# run ELEFAN with simulated annealing
res_SAbs <- ELEFAN_SA(lfq_results$BS, 
                    SA_time = 60*0.5, 
                    MA = 5, 
                    agemax = 5,
                    seasonalised = TRUE, addl.sqrt = TRUE,
                    init_par = list(Linf = 4.5, 
                                    K = 0.5, 
                                    t_anchor = 0.5,
                                    C=0.5, 
                                    ts = 0.5),
                    low_par = list(Linf = 4,
                                   K = 0.01,
                                   t_anchor = 0, 
                                   C = 0, 
                                   ts = 0),
                    up_par = list(Linf = 6.5,
                                  K = 1, 
                                  t_anchor = 1,
                                  C = 1, ts = 1))
```

```
## Simulated annealing is running. 
## This will take approximately 0.5 minutes.
## timeSpan = 30.00739 maxTime = 30
## Emini is: -0.3184663901
## xmini are:
## 5.310221989 0.8441457965 0.2786562045 0.3598986657 0.9663543715 
## Totally it used 30.007403 secs
## No. of function call is: 1829
```



```r
# run ELEFAN with genetic algorithm
res_GAbs <- ELEFAN_GA(lfq_results$BS, 
                    MA = 5, 
                    seasonalised = TRUE, 
                    maxiter = 10, 
                    agemax = 5,
                    addl.sqrt = TRUE,
                    low_par = list(Linf = 4.5, 
                                   K = 0.01, 
                                   t_anchor = 0, 
                                   C = 0, 
                                   ts = 0),
                    up_par = list(Linf = 6.5, 
                                  K = 1, 
                                  t_anchor = 1,
                                  C = 1, ts = 1),
                    monitor = FALSE)
```

```
## Genetic algorithm is running. This might take some time.
```

<img src="index_files/figure-html/unnamed-chunk-24-1.jpeg" style="display: block; margin: auto;" />


```r
# show results
RSAbs <- unlist(res_RSAbs$par)
GAbs <- unlist(res_GAbs$par)
SAbs <- unlist(res_SAbs$par)
parbs <- round(rbind(RSAbs, GAbs, SAbs),3)
```

#### Parameters to `GERLASHE`


```r
# Response surface analyss
res_RSAgs <- ELEFAN(lfq = lfq_results$GERLASHE,  
                  MA = 5,
                  Linf_range = seq(4.5, 7, 
                                   length.out = 30),
                  K_range = exp(seq(log(0.1),
                                    log(2),
                                    length.out = 30)),
                  C = 0.5,
                  ts = 0.5,
                  method = "cross",
                  cross.date = lfq_results$GERLASHE$dates[3],
                  cross.midLength = lfq_results$GERLASHE$midLengths[5],
                  contour = TRUE,
                  add.values = FALSE,
                  agemax = 5,
                  hide.progressbar = TRUE)
```

```
## Optimisation procuedure of ELEFAN is running. 
## This will take some time. 
## The process bar will inform you about the process of the calculations.
```



```r
# run ELEFAN with simulated annealing
res_SAgs <- ELEFAN_SA(lfq_results$GERLASHE, 
                    SA_time = 60*0.5, 
                    MA = 5, 
                    agemax = 5,
                    seasonalised = TRUE, addl.sqrt = TRUE,
                    init_par = list(Linf = 4.5, 
                                    K = 0.5, 
                                    t_anchor = 0.5,
                                    C=0.5, 
                                    ts = 0.5),
                    low_par = list(Linf = 4,
                                   K = 0.01,
                                   t_anchor = 0, 
                                   C = 0, 
                                   ts = 0),
                    up_par = list(Linf = 6.5,
                                  K = 1, 
                                  t_anchor = 1,
                                  C = 1, ts = 1))
```

```
## Simulated annealing is running. 
## This will take approximately 0.5 minutes.
## timeSpan = 30.004488 maxTime = 30
## Emini is: -0.3892859855
## xmini are:
## 5.28623276 0.9116002417 0.3847553197 0.06077500898 0.3120612055 
## Totally it used 30.0045 secs
## No. of function call is: 2521
```



```r
# run ELEFAN with genetic algorithm
res_GAgs <- ELEFAN_GA(lfq_results$GERLASHE, 
                    MA = 5, 
                    seasonalised = TRUE, 
                    maxiter = 10, 
                    agemax = 5,
                    addl.sqrt = TRUE,
                    low_par = list(Linf = 4.5, 
                                   K = 0.01, 
                                   t_anchor = 0, 
                                   C = 0, 
                                   ts = 0),
                    up_par = list(Linf = 6.5, 
                                  K = 1, 
                                  t_anchor = 1,
                                  C = 1, ts = 1),
                    monitor = FALSE)
```

```
## Genetic algorithm is running. This might take some time.
```

<img src="index_files/figure-html/unnamed-chunk-28-1.jpeg" style="display: block; margin: auto;" />


```r
# show results
RSAgs <- unlist(res_RSAgs$par)
GAgs <- unlist(res_GAgs$par)
SAgs <- unlist(res_SAgs$par)
pargs <- round(rbind(RSAgs, GAgs, SAgs),3)
```
total Table with `L_inf_` and `K`


```r
t_k_linf <- rbind(parsswi, parei, parbs, pargs)
t_k_linf %>%
  kbl(booktabs = T,
      position="ht!",
    caption = "Parametres LH in different strata with 3 types of algoritms") %>%
  kable_styling(latex_options = c("striped",
                                  "condensed"),
                full_width = FALSE)
```

<table class="table" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Parametres LH in different strata with 3 types of algoritms</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Linf </th>
   <th style="text-align:right;"> K </th>
   <th style="text-align:right;"> t_anchor </th>
   <th style="text-align:right;"> C </th>
   <th style="text-align:right;"> ts </th>
   <th style="text-align:right;"> phiL </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> RSAsswi </td>
   <td style="text-align:right;"> 5.448 </td>
   <td style="text-align:right;"> 0.640 </td>
   <td style="text-align:right;"> 0.790 </td>
   <td style="text-align:right;"> 0.500 </td>
   <td style="text-align:right;"> 0.500 </td>
   <td style="text-align:right;"> 1.279 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GAsswi </td>
   <td style="text-align:right;"> 5.989 </td>
   <td style="text-align:right;"> 0.579 </td>
   <td style="text-align:right;"> 0.532 </td>
   <td style="text-align:right;"> 0.859 </td>
   <td style="text-align:right;"> 0.880 </td>
   <td style="text-align:right;"> 1.318 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SAsswi </td>
   <td style="text-align:right;"> 5.325 </td>
   <td style="text-align:right;"> 0.913 </td>
   <td style="text-align:right;"> 0.613 </td>
   <td style="text-align:right;"> 0.096 </td>
   <td style="text-align:right;"> 0.334 </td>
   <td style="text-align:right;"> 1.413 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RSAei </td>
   <td style="text-align:right;"> 5.879 </td>
   <td style="text-align:right;"> 0.470 </td>
   <td style="text-align:right;"> 0.750 </td>
   <td style="text-align:right;"> 0.500 </td>
   <td style="text-align:right;"> 0.500 </td>
   <td style="text-align:right;"> 1.211 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GAei </td>
   <td style="text-align:right;"> 5.301 </td>
   <td style="text-align:right;"> 0.891 </td>
   <td style="text-align:right;"> 0.435 </td>
   <td style="text-align:right;"> 0.359 </td>
   <td style="text-align:right;"> 0.570 </td>
   <td style="text-align:right;"> 1.399 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SAei </td>
   <td style="text-align:right;"> 5.864 </td>
   <td style="text-align:right;"> 0.981 </td>
   <td style="text-align:right;"> 0.873 </td>
   <td style="text-align:right;"> 0.999 </td>
   <td style="text-align:right;"> 0.582 </td>
   <td style="text-align:right;"> 1.528 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RSAbs </td>
   <td style="text-align:right;"> 5.448 </td>
   <td style="text-align:right;"> 0.640 </td>
   <td style="text-align:right;"> 0.790 </td>
   <td style="text-align:right;"> 0.500 </td>
   <td style="text-align:right;"> 0.500 </td>
   <td style="text-align:right;"> 1.279 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GAbs </td>
   <td style="text-align:right;"> 6.032 </td>
   <td style="text-align:right;"> 0.673 </td>
   <td style="text-align:right;"> 0.420 </td>
   <td style="text-align:right;"> 0.431 </td>
   <td style="text-align:right;"> 0.811 </td>
   <td style="text-align:right;"> 1.389 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SAbs </td>
   <td style="text-align:right;"> 5.310 </td>
   <td style="text-align:right;"> 0.844 </td>
   <td style="text-align:right;"> 0.279 </td>
   <td style="text-align:right;"> 0.360 </td>
   <td style="text-align:right;"> 0.966 </td>
   <td style="text-align:right;"> 1.377 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RSAgs </td>
   <td style="text-align:right;"> 5.448 </td>
   <td style="text-align:right;"> 0.640 </td>
   <td style="text-align:right;"> 0.790 </td>
   <td style="text-align:right;"> 0.500 </td>
   <td style="text-align:right;"> 0.500 </td>
   <td style="text-align:right;"> 1.279 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GAgs </td>
   <td style="text-align:right;"> 5.518 </td>
   <td style="text-align:right;"> 0.593 </td>
   <td style="text-align:right;"> 0.611 </td>
   <td style="text-align:right;"> 0.732 </td>
   <td style="text-align:right;"> 0.454 </td>
   <td style="text-align:right;"> 1.256 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SAgs </td>
   <td style="text-align:right;"> 5.286 </td>
   <td style="text-align:right;"> 0.912 </td>
   <td style="text-align:right;"> 0.385 </td>
   <td style="text-align:right;"> 0.061 </td>
   <td style="text-align:right;"> 0.312 </td>
   <td style="text-align:right;"> 1.406 </td>
  </tr>
</tbody>
</table>


### Method NO LINEAR MODEL 


We use firts a descomposition methot with library `mixR` and then we use `nlmer` (@Bates2015) to calcularte parametrs


```r
# Agregar la nueva columna con los intervalos de longitud total
sf6 <- sf5 |> 
  drop_na(length_total_cm) |> 
  filter(ID %in% "GERLASHE",
         Year==2018,
         Month==5)

sf5a <-  sf5 |> 
  drop_na(length_total_cm ) |> 
  filter(ID %in% "BS",
         Year==2018)
  
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

```r
MSSWI <- c(res_GAsswi$par, list(agemax = res_RSAsswi$agemax))
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
BS <- c(res_GAbs$par, list(agemax = res_RSAbs$agemax))

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
GS <- c(res_GAgs$par, list(agemax = res_RSAgs$agemax))

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
MEI <- c(res_GAei$par, list(agemax = res_RSAei$agemax))

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


```r
# junto las bases

Total_M <- cbind(Mei, Mbs, Mgs, Msswi)
colnames(Total_M) <- c("EI", "BS" , "GS", "SSWI")
mean <- colMeans(Total_M, na.rm = TRUE)
# Agregar los promedios como una nueva fila al final del array
Total_M_Mean <- rbind(Total_M, Mean = mean)
```



```r
Total_M_Mean  %>%
  kbl(booktabs = T,
      position="ht!",
    caption = "Estimated M by Strata") %>%
  kable_styling(latex_options = c("striped",
                                  "condensed"),
                full_width = FALSE)
```

<table class="table" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Estimated M by Strata</caption>
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
   <td style="text-align:right;"> 0.6030 </td>
   <td style="text-align:right;"> 0.7790 </td>
   <td style="text-align:right;"> 0.8530 </td>
   <td style="text-align:right;"> 0.8660 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hoenig (1983) - Joint Equation </td>
   <td style="text-align:right;"> 0.8690 </td>
   <td style="text-align:right;"> 0.8690 </td>
   <td style="text-align:right;"> 0.8690 </td>
   <td style="text-align:right;"> 0.8690 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hoenig (1983) - Fish Equation </td>
   <td style="text-align:right;"> 0.8470 </td>
   <td style="text-align:right;"> 0.8470 </td>
   <td style="text-align:right;"> 0.8470 </td>
   <td style="text-align:right;"> 0.8470 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pauly (1980) - Length Equation </td>
   <td style="text-align:right;"> 1.2090 </td>
   <td style="text-align:right;"> 0.9710 </td>
   <td style="text-align:right;"> 0.9160 </td>
   <td style="text-align:right;"> 0.8820 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rikhter and Efanov (1976) </td>
   <td style="text-align:right;"> 0.7680 </td>
   <td style="text-align:right;"> 0.7680 </td>
   <td style="text-align:right;"> 0.7680 </td>
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
   <td style="text-align:right;"> 0.8592 </td>
   <td style="text-align:right;"> 0.8468 </td>
   <td style="text-align:right;"> 0.8506 </td>
   <td style="text-align:right;"> 0.8464 </td>
  </tr>
</tbody>
</table>


# References


