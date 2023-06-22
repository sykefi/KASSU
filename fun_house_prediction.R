## Predict the household demand ##
## Määritetään asuntotarve pienalueilla ##
##--------------------------------------##

## Read function parameters 
fun_house_prediction = function(population_prediction, INPUT_family_size, INPUT_household_rates, 
                           INPUT_household_housetype_distribution,INPUT_household_meansize, 
                           INPUT_floor_area,INPUT_stock_reserve, INPUT_stock_loss,
                           INPUT_existing_rhr_stock, n_estimation_years){

## STEP 1 ##
##--------##
## Create matrix of the following age groups and aggregate population to it.
## 0-6 v, 7-14 v, 15-17 v, 18-23 v, 24-29 v, 30-44 v, 45-64 v, 65-74 v, 75- v
n_agegroups = 9 
n_FamilySizes = 3
agg_population =  array(0, dim=c(n_agegroups, col, n_estimation_years))

## Aggregate population
agg_population[1,,] = colSums(population_prediction[1:7,,])
agg_population[2,,] = colSums(population_prediction[8:15,,])
agg_population[3,,] = colSums(population_prediction[16:18,,])
agg_population[4,,] = colSums(population_prediction[19:24,,])
agg_population[5,,] = colSums(population_prediction[25:30,,])
agg_population[6,,] = colSums(population_prediction[31:45,,])
agg_population[7,,] = colSums(population_prediction[46:65,,])
agg_population[8,,] = colSums(population_prediction[66:74,,])
agg_population[9,,] = colSums(population_prediction[75:101,,])


## STEP 2 ##
## Multiply population prediction with size of household-dwelling unit (INPUT_family_size) ##
## Kerrotaan väestöennuste asuntojen kokojakaumalla                                        ##
##-----------------------------------------------------------------------------------------##
step2 = foreach(i = 1:(col))%do%{  ## create index for sub-regions
  foreach(year = 1:(n_estimation_years))%do%{  ## create index for estimation years
    list2 = (INPUT_family_size[,,i] * agg_population[,i,year])}
}

## STEP 3 ##
## Multiply output of the STEP2 with household rates in different age groups (INPUT_household_rates) ##
## Kerrotaan asuntokuntiin jaettu väestöennuste asuntokuntien muodostumiskertoimilla                 ##
##----------------------------------------------------------------------------------------------------##
step3 = foreach(i = 1:(col))%do%{ 
  foreach(year = 1:(n_estimation_years))%do%{ 
    list3 = step2[[i]][[year]]*INPUT_household_rates[,year, i]}
}

## STEP 4 ##
## Create total number of household-dwelling units by combining the age groups from STEP3 ##
## Lasketaan asuntokuntien kokonaismäärät (1 hlö, 2 hlö, 3+ hlö)                          ##
##----------------------------------------------------------------------------------------##
step4 = foreach(i = 1:(col))%do%{ 
  foreach(year = 1:(n_estimation_years))%do%{
    list4 = colSums(step3[[i]][[year]])}
}

## STEP 5 ##
## Convert household-dwelling units to houses by multiplying with house type distribution (INPUT_household_housetype_distribution) ##
## Muunnetaan asuntokunnat asunnoiksi talotyyppijakauman mukaan                                                                    ##
##---------------------------------------------------------------------------------------------------------------------------------##
step5 = foreach(i = 1:(col))%do%{
  foreach(year = 1:(n_estimation_years))%do%{
    list5 = step4[[i]][[year]] * INPUT_household_housetype_distribution[,,i]}
}

## STEP 6 ##
## Add housing stock reservation rate (INPUT_stock_reserve) ##
## Lisätään asuntovarauma                                   ##
##----------------------------------------------------------##
step6 = foreach(i = 1:(col))%do%{
  foreach(year = 1:(n_estimation_years))%do%{
    list6 = step5[[i]][[year]] * INPUT_stock_reserve}
}


## STEP 7 ##
## Combine different family size houses for total housing stock                         ##
## Lasketaan yhteen eri perhekoon asuntojen lukumäärä asuntojen kokonaistarvetta varten ##
##--------------------------------------------------------------------------------------##
## Create output matrix
est_stock = array(0, dim=c(n_estimation_years,3,col))

## Sum data
foreach(i = 1:(col))%do%{
  foreach(year = 1:(n_estimation_years))%do%{ 
    step7 = (colSums(step6[[i]][[year]]))
    est_stock[year,,i] = step7
    colnames(est_stock) = c("omakotitalo","rivitalo","kerrostalo") # Give better column names
    est_stock[is.nan(est_stock)] <- 0}
}


## STEP 8 ##
## Sum all sub-regions for municipality value     ##
## Summataan arvot kuntakohtaista tarvetta varten ##
##------------------------------------------------##
sum_est_stock = apply(est_stock, MARGIN=c(1, 2), sum)


## STEP 9 ##
## Convert house demand to floor space by multiplying with house mean size (INPUT_household_meansize) ##                      
## Muunnetaan asuntotarve (asuntojen lukumäärä) kerrosalaksi kertomalla asuntojen keskikoon mukaan    ##
##----------------------------------------------------------------------------------------------------##
est_stock_floor = foreach(i = 1:(col))%do%{ 
  foreach(year = 1:(n_estimation_years))%do%{
    step9 = step6[[i]][[year]] * INPUT_household_meansize[,,i]}
}


## STEP 10 ##
## Calculate change rate (floor area (m2 ) -> floor area (kem2)). Only for apartments     ##
## Lasketaan muuntokerroin lattiapinta-alasta kerrosalaksi (vain kerrostealojen osalta)  ##
##---------------------------------------------------------------------------------------##
foreach(i = 1:(col))%do%{ # select sub-region
  foreach(year = 1:(n_estimation_years))%do%{# select prediction year
    foreach(j = 1:3)%do%{ # select detached house (OKT)
      foreach(k = 4:6)%do%{ # select row house (RT)
        foreach(l = 7:9)%do%{ # select apartment house (KT)
          est_stock_floor[[i]][[year]][[j]] = est_stock_floor[[i]][[year]][[j]] * INPUT_floor_area[[1]] 
          est_stock_floor[[i]][[year]][[k]] = est_stock_floor[[i]][[year]][[k]] * INPUT_floor_area[[2]]
          est_stock_floor[[i]][[year]][[l]] = est_stock_floor[[i]][[year]][[l]] * INPUT_floor_area[[3]]}}}}
}

## STEP 11 ##
## Combine floor area predictions to get value for every sub region ##
## Yhdistetään kerrosalaennusteet pienalueittain                    ##
##------------------------------------------------------------------##
## Create output matrix
est_stock_floor_area = array(0, dim=c(n_estimation_years,3, col))

foreach(i = 1:(col))%do%{ 
  foreach(year = 1:(n_estimation_years))%do%{ 
    List11 = (colSums(est_stock_floor[[i]][[year]]/1000)) # ( divide to get unit: 1000m2)
      est_stock_floor_area[year,,i] = List11 
        colnames(est_stock_floor_area) = c("omakotitalo","rivitalo","kerrostalo")
          est_stock_floor_area[is.nan(est_stock_floor_area)] <- 0
     }
}

## STEP 12 ##
## Calculate municipal level estimations by combining sub-regions ##
## Summataan arvot kuntakohtaista tarvetta varten                 ## 
##----------------------------------------------------------------##
sum_est_stock_floor_area = apply(est_stock_floor_area, MARGIN=c(1, 2), sum)


## STEP 13 ##
## Subtract yearly stock loss (INPUT_stock_loss) ##
## Vähennetään vuotuinen asuntokannan poistuma   ##
##-----------------------------------------------##
## Create output matrix
existing_rhr_stock_area = array(INPUT_existing_rhr_stock$`Olemassa oleva asuntojen kerrosala`, dim=c(col, 3, n_estimation_years))

foreach(year = 2:n_estimation_years)%do%{ # first year will be constant
  existing_rhr_stock_area[,,1] = INPUT_existing_rhr_stock$`Olemassa oleva asuntojen kerrosala` # existing housing stock taken from variable INPUT_existing_rhr_stock (1000 kem2)
    existing_rhr_stock_area[,,year] = existing_rhr_stock_area[,,year-1]*(1-INPUT_stock_loss) # reduce yearly stock loss
      colnames(existing_rhr_stock_area) = c("omakotitalo","rivitalo","kerrostalo")
        existing_rhr_stock_area[is.nan(existing_rhr_stock_area)] = 0 
}

## STEP 14 ##
## Copy results to compatible matrix             ##
## Siirretään tulosdata yhteneväiseen matriisiin ##
##-----------------------------------------------##
## Create output matrix
rhr_stock_floor_area = array(0, dim=c(n_estimation_years,3, col))

foreach(i = 1:(col))%do%{
  foreach(year = 1:n_estimation_years)%do%{
    rhr_stock_floor_area[year,,i] = existing_rhr_stock_area [i,,year]
  }}

## STEP 15 ##
## Repeat step 13 for the housing stock (number of houses) ##
## Toistetaan kohta 13 asuntojen lukumäärälle                    ##
##---------------------------------------------------------------##
existing_rhr_stock = array(INPUT_existing_rhr_stock$`Olemassa oleva asuntojen lukumaara`, dim=c(col, 3, n_estimation_years))

# Vahennetaan vuosittainen poistuma
foreach(year = 2:n_estimation_years)%do%{ # first year will be constant
  existing_rhr_stock[,,1] = INPUT_existing_rhr_stock$`Olemassa oleva asuntojen lukumaara` # first year will be constant
  existing_rhr_stock[,,year] = existing_rhr_stock[,,year-1]*(1-INPUT_stock_loss) # reduce yearly stock loss
  colnames(existing_rhr_stock) = c("omakotitalo","rivitalo","kerrostalo")
  existing_rhr_stock[is.nan(existing_rhr_stock)] <- 0 
}

## STEP 16 ##
## Repeat step 14 for the housing stock (number of houses) ##
## Toistetaan kohta 14 asuntojen lukumäärälle              ##
##---------------------------------------------------------##
## Create output matrix
rhr_stock = array(0, dim=c(n_estimation_years,3, col))

foreach(i = 1:(col))%do%{
  foreach(year = 1:n_estimation_years)%do%{
    rhr_stock[year,,i] = existing_rhr_stock[i,,year]}
}

## STEP 16 ##
## Calculate municipal level estimations by combining sub-regions ##
## Summataan arvot kuntakohtaista tarvetta varten                 ## 
##----------------------------------------------------------------##
sum_rhr_stock_floor_area = apply(rhr_stock_floor_area, MARGIN=c(1, 2), sum)
sum_rhr_stock = apply(rhr_stock, MARGIN=c(1, 2), sum)


##----------------------------------------------------------------##
## Output data ##
## est_stock: Regional housing need according to the population prediction (number of houses)
## sum_est_stock: Municipality specific housing need according to the population prediction (number of house)

## est_stock_floor_area: Regional housing need according to the population prediction (floor area, 1000kem2)
## sum_est_stock_floor_area: Municiplaity specific housing need according to the population prediction (floor area, 1000kem2)

## rhr_stock: Existing, regional housing stock according to RHR. (number of house)
## sum_rhr_stock: Existing, municipal specific housing stock according to RHR. (number of house)

## rhr_stock_floor_area: Existing, regional housing stock according to RHR (floor area, 1000kem2)
## sum_rhr_stock_floor_area: Existing, municipal specific housing stock according to RHR (floor area, 1000kem2)
##-----------------------------------------------------------------##



## STEP 18 ##
## Calculate production & reducion need for housing stock by comparing population prediction based housing need to essting (RHR-based) housing stock ##
## Määritetään vuotuiset tuotanto ja vähennystarpeet vertaamalla ennusteen mukaista asuntotarvetta olemassa olevaan asuntokantaan (RHR)              ##
##---------------------------------------------------------------------------------------------------------------------------------------------------##

## Number of houses
## Regional predictions 
h_prod_red = round(est_stock - rhr_stock) # if positive, reduction needed. If negative, production needed.

## Municipal specific predictions 
mun_h_prod_red = round(sum_est_stock - sum_rhr_stock) # if positive, reduction needed. If negative, production needed.


## Floor area (1000 kem2)
## Regional predictions 
h_floor_prod_red = round(est_stock_floor_area - rhr_stock_floor_area) # if positive, reduction needed. If negative, production needed.

## Municipal specific predictions 
mun_h_floor_prod_red = round(sum_est_stock_floor_area -sum_rhr_stock_floor_area) # if positive, reduction needed. If negative, production needed.



## Write output data ##
##-------------------##
output = list("Olemassa oleva asuntokanta pienalueilla (lkm)" = rhr_stock,
             "Olemassa oleva asuntokanta kunnassa (lkm)" = sum_rhr_stock, 
             "Olemassa oleva asuntokanta pienalueilla (1000kem2)" = rhr_stock_floor_area,
             "Olemassa oleva asuntokanta kunnassa (1000m2)" = sum_rhr_stock_floor_area,
     
             "Vaestoennusteen mukainen asuntotarve pienalueilla (lkm)" = est_stock,
             "Vaestoennusteen mukainen asuntotarve kunnassa (lkm)" = sum_est_stock,
             "Vaestoennusteen mukainen asuntotarve pienalueilla (1000kem2)" = est_stock_floor_area,
             "Vaestoennusteen mukainen asuntotarve kunnassa (1000kem2)" = sum_est_stock_floor_area,
             
             "Asuntojen tuotanto_vähennystarve pienalueilla (lkm)" = h_prod_red,
             "Asuntojen tuotanto_vähennystarve pienalueilla (1000kem2)" = h_floor_prod_red,
             
             "Asuntojen tuotanto_vähennystarve kunnassa (lkm)" = mun_h_prod_red,
             "Asuntojen tuotanto_vähennystarve kunnassa (1000kem2)" = mun_h_floor_prod_red)
             
return(output)
}

















