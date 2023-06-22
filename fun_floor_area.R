## Rate from area to floor area in apartment buildings  (k-m2)         ##
## Kerrostalojen lattiapinta-alan muuntokerroin kerrosneliöiksi (k-m2) ##
##---------------------------------------------------------------------##

## Muodostetaan kerroin pinta-alan muuntamiseksi kerrosneliöiksi. lasketaan yhteen uniikkien kerrostalojen sisaltämien huoneistojen pinta-alat
## ja verrataan tätä kerrostalon ilmoitettuun kerrosalaan. Tämän perusteella muodostetaan kaupunkikohtainen muuntokerroin

## Read function parameters 
fun_floor_area = function(start_year, rhr, area){
  
## Create connectionto sql-server 
connection_name = paste("Driver={SQL Server};server=server_name;database=database_name;trusted_connection=yes;", sep="")
cn = odbcDriverConnect(connection=connection_name)
  
## Caluculate number of residents in different apartment and house types
rhr_data = sqlQuery(cn, paste("SELECT H.rakennustunnus, H.huoneisto, COUNT(v.Id) as Asukkaita, H.huoneluku, H.pinta_ala, 
R.kerrosala, R.kayttotarkoitus_id, p.area_id
FROM ",rhr," huoneisto H
INNER JOIN ",rhr," rakennus R
ON R.rakennustunnus = H.rakennustunnus
INNER JOIN ",rhr," vaesto V 
ON V.rakennustunnus = H.rakennustunnus AND V.huoneisto=H.huoneisto
INNER JOIN ",area," p 
on p.xyind = r.ykre_ruutu_xyind
WHERE R.kayttotarkoitus_id <='05'
and kerrosala > 0
Group by H.rakennustunnus, H.huoneisto, H.huoneluku, H.pinta_ala, R.kerrosala, R.kayttotarkoitus_id, p.area_id", sep=""))

## Extract detached houses ## 
##-------------------------##
DetachedHouses = subset(rhr_data,kayttotarkoitus_id == 21 | kayttotarkoitus_id == 22)

## Sum the household area from every apartment 
Detachedhouses_comparison=DetachedHouses %>%
  group_by(rakennustunnus) %>% 
  transmute(summa_pinta_ala=sum(pinta_ala))

## Divide area with floor area 
DetachedHousesRatio=DetachedHouses$kerrosala/Detachedhouses_comparison$summa_pinta_ala

## Remove outliers (values over 2 ja below 1.5 
DetachedHousesRatio=subset(DetachedHousesRatio,DetachedHousesRatio>=1 & DetachedHousesRatio<=1.5)

## Extract mean values
MeanDetachedHousesRatio=mean(DetachedHousesRatio)


## Extract attached buildings ##
##----------------------------##
attachedHouses = subset(rhr_data,kayttotarkoitus_id == 11 | kayttotarkoitus_id == 12 | kayttotarkoitus_id == 13)

## Sum the household area from every apartment 
attachedhouses_comparison=attachedHouses %>%
  group_by(rakennustunnus) %>% 
  transmute(summa_pinta_ala=sum(pinta_ala))

## Divide area with floor area 
attachedHousesRatio=attachedHouses$kerrosala/attachedhouses_comparison$summa_pinta_ala

## Remove outliers (values over 2 ja below 1.5 
attachedHousesRatio=subset(attachedHousesRatio,attachedHousesRatio >= 1 & attachedHousesRatio <= 1.5)

## Extract mean values
MeanAttachedHousesRatio=mean(attachedHousesRatio)


## Extract apartment buildings ##
##-----------------------------##
apartmentHouses = subset(rhr_data,kayttotarkoitus_id == 32 | kayttotarkoitus_id == 39)

## Sum the household area from every apartment 
apartmenthouses_comparison=apartmentHouses %>%
  group_by(rakennustunnus) %>% 
  transmute(summa_pinta_ala=sum(pinta_ala))

## Divide area with floor area 
apartmentHousesRatio=apartmentHouses$kerrosala/apartmenthouses_comparison$summa_pinta_ala

## Remove outliers (values over 2 ja below 1.5 
apartmentHousesRatio=subset(apartmentHousesRatio,apartmentHousesRatio >= 1 & apartmentHousesRatio <= 1.5)

## Extract mean values
MeanApartmentHousesRatio=mean(apartmentHousesRatio)

output=list(MeanDetachedHousesRatio, MeanAttachedHousesRatio, MeanApartmentHousesRatio)

return(output)}
