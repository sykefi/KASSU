## Female immigration ##
##--------------------##                           

## Function returns female immigration from the selected municipality using Statistics Finland data. 
## Immigration is allocated to the study area in proportion to the existing population. 

## GET DATA ##
##----------##

## Read function parameters 
fun_immigration_female=function(vaesto_naiset, mun_nro, col){
  
## Greate pxweb query 
pxweb_query_list = 
    list("Alue"=c(paste0("KU",mun_nro)),
         "Vuosi" =  as.character(pred_years),
         "Sukupuoli" = c('2'),
         "Ikä" = c('*'),
         "Tiedot" = c("vm4142"))

## Download data 
px_data = 
    pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/muutl/statfin_muutl_pxt_11a7.px",
              query = pxweb_query_list)
  
## Convert to data.frame 
net_immigration_f= as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")  
  
## Drop extra rows
net_immigration_f=subset(net_immigration_f, net_immigration_f$Ikä != "Yhteensä") 
  
## Drop extra columns
drops = c("Alue", "Sukupuoli")
net_immigration_f= net_immigration_f[ , !(names(net_immigration_f) %in% drops)] 

## Aggregate data using age and year and calculate mean immigration
net_immigration_f_mean=aggregate(Nettomaahanmuutto ~ Ikä, net_immigration_f, mean)

## Change row order to match population data (5-9 year old should be on the second row)
net_immigration_f_mean = net_immigration_f_mean[c(1,10,2:9,11:nrow(net_immigration_f_mean)), ] # new order 

## MODIFY MIGRATION DATA ##
##-----------------------##

## Divide the migration to 1-year age groups
## Set replicates for each row
duptimes = c(5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,26)
  
  ## Create an index of the rows you want with duplications
  idx = rep(1:nrow(net_immigration_f_mean), duptimes)
  
  ## Use that index to generate your new data frame
  dup_mig = net_immigration_f_mean[idx,]
  
  ## Replicate migration columns to match the sub-study areas
  n = col-1 # Number of sub-study areas minus existing migration column
  dup_mig = cbind(dup_mig, replicate(n,dup_mig$Nettomaahanmuutto)) # Replicate column [Nettomaahamuutto]

## MODIFY POPULATION DATA ##
##------------------------##
  
## Add [id] to the population data that match the age groups from immigration data (0-4, 5-9 etc..)
func = unlist(lapply(1:ceiling(75/5), function(x) {replicate(5, x)}))
id = func[1:nrow(vaesto_naiset)]
vaesto_naiset  = cbind(vaesto_naiset,id)

## Add also [id] for missing rows (76-101)
vaesto_naiset[is.na(vaesto_naiset)] = 16

## Calculate percentage by group
vaesto_naiset_perc = transform(vaesto_naiset,
                       perc = ave(vaesto_naiset,
                                  id,
                                  FUN = prop.table))

## Drop unnecessary columns
drop = c("V1","V2","V3","V4","V5","V6","V7","V8","id")
vaesto_naiset_perc = vaesto_naiset_perc[,!(names(vaesto_naiset_perc) %in% drop)]
vaesto_naiset = vaesto_naiset[,1:col]

## Allocate the migration to the study area by multiplying the percentage of the existing population with the migration data.
vaesto_naiset_perc = vaesto_naiset_perc*dup_mig[,2:ncol(dup_mig)]

output6=as.matrix(vaesto_naiset_perc)
return (output6)
}

