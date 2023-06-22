## Male immigration ##
##------------------##                         

## Function returns male immigration from the selected municipality using Statistics Finland data. 
## Immigration is allocated to the study area in proportion to the existing population. 

## GET DATA ##
##----------##

## Read function parameters
fun_immigration_male <- function(vaesto_miehet, mun_nro, col){

## Greate pxweb query 
pxweb_query_list = 
    list("Alue"=c(paste0("KU",mun_nro)),
         "Vuosi" = as.character(pred_years),
         "Sukupuoli" = c('1'),
         "Ikä" = c('*'),
         "Tiedot" = c("vm4142"))
  
## Download data 
px_data = 
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/muutl/statfin_muutl_pxt_11a7.px",
          query = pxweb_query_list)

  
## Convert to data.frame 
net_immigration_m = as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")  
  
## Drop extra rows
 net_immigration_m = subset(net_immigration_m, net_immigration_m$Ikä != "Yhteensä") 

## Drop extra columns
drops = c("Alue", "Sukupuoli") # Drop extra columns
net_immigration_m = net_immigration_m[ , !(names(net_immigration_m) %in% drops)] 
  
## Aggregate data using age and year and calculate mean immigration
net_immigration_m_mean = aggregate(Nettomaahanmuutto ~ Ikä, net_immigration_m, mean)
  
## Change row order to match population data
net_immigration_m_mean = net_immigration_m_mean[c(1,10,2:9,11:nrow(net_immigration_m_mean)), ] # new order 

## MODIFY MIGRATION DATA ##
##-----------------------##

## Divide the migration to 1-year age groups
## Set replicates for each row
duptimes = c(5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,26)
  
## Create an index of the rows you want to duplicate
idx = rep(1:nrow(net_immigration_m_mean), duptimes)
  
## Use that index to generate your new data frame
dup_mig = net_immigration_m_mean[idx,]

## MODIFY POPULATION DATA ##
##------------------------##

## Add [id] for the population data that match the age groups from immigration data (0-4, 5-9 etc..)
func = unlist(lapply(1:ceiling(75/5), function(x) {replicate(5, x)}))
id = func[1:nrow(vaesto_miehet)]
vaesto_miehet = cbind(vaesto_miehet,id)
  
## Add also [id] for missing rows (76-101)
vaesto_miehet[is.na(vaesto_miehet)] = 16
  
## Calculate percentage by group
vaesto_miehet_perc = transform(vaesto_miehet,
                             perc = ave(vaesto_miehet,
                             id,
                             FUN = prop.table))
  
## Drop unnecessary columns
drop = c("V1","V2","V3","V4","V5","V6","V7","V8","id")
vaesto_miehet_perc = vaesto_miehet_perc[,!(names(vaesto_miehet_perc) %in% drop)]

vaesto_miehet = vaesto_miehet[,1:col]

## ALLOCATE MIGRATION DATA TO STUYDY AREA
## Allocate the migration to the study area by multiplying the percentage of the existing population with the migration data.
vaesto_miehet_perc = vaesto_miehet_perc*dup_mig[,2]

output7 = as.matrix(vaesto_miehet_perc)

return (output7)}

