##  Household rates in different age groups ##
##  Asuntokuntien muodostumiskertoimet      ##
##------------------------------------------##

## Calculate household rate by dividing the number of certain age residents in households with the total residents in household
## e.g. Household of 4 people where there is one 0-6 year old the percentage is 1/4=0.25
                                     
## Read function parameters 
fun_household_rates=function(start_year, rhr, area, col){
  
## Create output matrix ###
n_age_groups=9 #number of different age groups
ak_rate = array(0, dim=c(n_age_groups,n_estimation_years, col))

## Create connectionto sql-server ###
connection_name = paste("Driver={SQL Server};server=server_name;database=database_name;trusted_connection=yes;", sep="")
cn = odbcDriverConnect(connection=connection_name)

## Calculate the number of 0-6 year old in households ##
population_0_6 = sqlQuery(cn, paste("select ikaryhma='v_0_6', askunta=SUM(1/cast(v_yht as float)), henk=COUNT(v.id), p.area_id
from ",rhr,"vaesto v inner join
                                 (select  r.kunta_id, v.rakennustunnus, huoneisto, kayttotarkoitus_id, v_yht=count(v.id), r.ykre_ruutu_xyind, r.kaytossaolotilanne_id
                                 from ",rhr,"vaesto v 	inner join ",rhr," rakennus r on v.rakennustunnus=r.rakennustunnus
                                 group by v.rakennustunnus, huoneisto, kayttotarkoitus_id, r.kaytossaolotilanne_id, r.kunta_id, r.ykre_ruutu_xyind)
                                 as a on v.rakennustunnus=a.rakennustunnus and v.huoneisto=a.huoneisto
                                 inner join ",area," p on p.xyind = a.ykre_ruutu_xyind
                                 WHERE ika between 0 and 6 and kaytossaolotilanne_id < 6 and (a.kayttotarkoitus_id IN (011,012,013,021,022,032,039))
                                 group by p.area_id", sep=""))
  
## 7-14 year old ##
population_7_14 = sqlQuery(cn, paste("select ikaryhma='v_7_14', askunta=SUM(1/cast(v_yht as float)), henk=COUNT(v.id), p.area_id
from ",rhr,"vaesto v inner join
                                 (select  r.kunta_id, v.rakennustunnus, huoneisto, kayttotarkoitus_id, v_yht=count(v.id), r.ykre_ruutu_xyind, r.kaytossaolotilanne_id
                                 from ",rhr,"vaesto v 	inner join ",rhr," rakennus r on v.rakennustunnus=r.rakennustunnus
                                 group by v.rakennustunnus, huoneisto, kayttotarkoitus_id, r.kaytossaolotilanne_id,  r.kunta_id, r.ykre_ruutu_xyind)
                                 as a on v.rakennustunnus=a.rakennustunnus and v.huoneisto=a.huoneisto
                                 inner join ",area," p on p.xyind = a.ykre_ruutu_xyind
                                 WHERE ika between 7 and 14 and kaytossaolotilanne_id < 6 and (v_yht <20 and a.kayttotarkoitus_id IN (011,012,013,021,022,032,039)) 
                                 group by p.area_id", sep=""))

## 15-17 year old ##
population_15_17 = sqlQuery(cn, paste("select ikaryhma='v_15_17', askunta=SUM(1/cast(v_yht as float)), henk=COUNT(v.id), p.area_id
from ",rhr,"vaesto v inner join
                                 (select  r.kunta_id, v.rakennustunnus, huoneisto, kayttotarkoitus_id, v_yht=count(v.id), r.ykre_ruutu_xyind, r.kaytossaolotilanne_id
                                 from ",rhr," vaesto v 	inner join ",rhr,"rakennus r on v.rakennustunnus=r.rakennustunnus
                                 group by v.rakennustunnus, huoneisto, kayttotarkoitus_id,r.kaytossaolotilanne_id, r.kunta_id, r.ykre_ruutu_xyind)
                                 as a on v.rakennustunnus=a.rakennustunnus and v.huoneisto=a.huoneisto
                                 inner join ",area," p on p.xyind = a.ykre_ruutu_xyind
                                 WHERE ika between 15 and 17 and kaytossaolotilanne_id < 6 and (v_yht <20 and a.kayttotarkoitus_id IN (011,012,013,021,022,032,039))
                                 group by p.area_id", sep=""))
## 18-23 year old ##
population_18_23 = sqlQuery(cn, paste("select ikaryhma='v_18_23', askunta=SUM(1/cast(v_yht as float)), henk=COUNT(v.id), p.area_id
from ",rhr," vaesto v inner join
                                 (select  r.kunta_id, v.rakennustunnus, huoneisto, kayttotarkoitus_id, v_yht=count(v.id), r.ykre_ruutu_xyind, r.kaytossaolotilanne_id
                                 from ",rhr," vaesto v 	inner join ",rhr," rakennus r on v.rakennustunnus=r.rakennustunnus
                                 group by v.rakennustunnus, huoneisto, kayttotarkoitus_id, r.kaytossaolotilanne_id, r.kunta_id, r.ykre_ruutu_xyind)
                                 as a on v.rakennustunnus=a.rakennustunnus and v.huoneisto=a.huoneisto
                                 inner join ",area," p on p.xyind = a.ykre_ruutu_xyind
                                 WHERE ika between 18 and 23 and kaytossaolotilanne_id < 6 and (v_yht <20 and a.kayttotarkoitus_id IN (011,012,013,021,022,032,039))
                                 group by p.area_id", sep=""))

## 24-29 year old
population_24_29 = sqlQuery(cn, paste("select ikaryhma='v_24_29', askunta=SUM(1/cast(v_yht as float)), henk=COUNT(v.id), p.area_id
from ",rhr," vaesto v inner join
                                 (select  r.kunta_id, v.rakennustunnus, huoneisto, kayttotarkoitus_id, v_yht=count(v.id), r.ykre_ruutu_xyind, r.kaytossaolotilanne_id
                                 from ",rhr," vaesto v 	inner join ",rhr," rakennus r on v.rakennustunnus=r.rakennustunnus
                                 group by v.rakennustunnus, huoneisto, kayttotarkoitus_id, r.kaytossaolotilanne_id, r.kunta_id, r.ykre_ruutu_xyind)
                                 as a on v.rakennustunnus=a.rakennustunnus and v.huoneisto=a.huoneisto
                                 inner join ",area," p on p.xyind = a.ykre_ruutu_xyind
                                 WHERE ika between 24 and 29 and kaytossaolotilanne_id < 6 and (v_yht <20 and a.kayttotarkoitus_id IN (011,012,013,021,022,032,039))
                                 group by p.area_id", sep=""))


## 30-44 year old ##
population_30_44 = sqlQuery(cn, paste("select ikaryhma='v_30_44', askunta=SUM(1/cast(v_yht as float)), henk=COUNT(v.id), p.area_id
from ",rhr," vaesto v inner join
                                 (select  r.kunta_id, v.rakennustunnus, huoneisto, kayttotarkoitus_id, v_yht=count(v.id), r.ykre_ruutu_xyind, r.kaytossaolotilanne_id
                                 from ",rhr," vaesto v 	inner join ",rhr," rakennus r on v.rakennustunnus=r.rakennustunnus
                                 group by v.rakennustunnus, huoneisto, kayttotarkoitus_id, r.kaytossaolotilanne_id, r.kunta_id, r.ykre_ruutu_xyind)
                                 as a on v.rakennustunnus=a.rakennustunnus and v.huoneisto=a.huoneisto
                                 inner join ",area," p on p.xyind = a.ykre_ruutu_xyind
                                 WHERE ika between 30 and 44 and kaytossaolotilanne_id < 6 and (v_yht <20 and a.kayttotarkoitus_id IN (011,012,013,021,022,032,039))
                                 group by p.area_id", sep=""))


## 45-64 year old ##
population_45_64 = sqlQuery(cn, paste("select ikaryhma='v_45_64', askunta=SUM(1/cast(v_yht as float)), henk=COUNT(v.id), p.area_id
from ",rhr," vaesto v inner join
                                 (select  r.kunta_id, v.rakennustunnus, huoneisto, kayttotarkoitus_id, v_yht=count(v.id), r.ykre_ruutu_xyind, r.kaytossaolotilanne_id
                                 from ",rhr," vaesto v 	inner join ",rhr," rakennus r on v.rakennustunnus=r.rakennustunnus
                                 group by v.rakennustunnus, huoneisto, kayttotarkoitus_id, r.kaytossaolotilanne_id, r.kunta_id, r.ykre_ruutu_xyind)
                                 as a on v.rakennustunnus=a.rakennustunnus and v.huoneisto=a.huoneisto
                                 inner join ",area," p on p.xyind = a.ykre_ruutu_xyind
                                 WHERE ika between 45 and 64 and kaytossaolotilanne_id < 6 and (v_yht <20 and a.kayttotarkoitus_id IN (011,012,013,021,022,032,039))
                                 group by p.area_id", sep=""))


## 65-74 year old ##
population_65_74 = sqlQuery(cn, paste("select ikaryhma='v_65_74', askunta=SUM(1/cast(v_yht as float)), henk=COUNT(v.id), p.area_id
from ",rhr," vaesto v inner join
                                 (select  r.kunta_id, v.rakennustunnus, huoneisto, kayttotarkoitus_id, v_yht=count(v.id), r.ykre_ruutu_xyind, r.kaytossaolotilanne_id
                                 from ",rhr," vaesto v 	inner join ",rhr," rakennus r on v.rakennustunnus=r.rakennustunnus
                                 group by v.rakennustunnus, huoneisto, kayttotarkoitus_id, r.kaytossaolotilanne_id, r.kunta_id, r.ykre_ruutu_xyind)
                                 as a on v.rakennustunnus=a.rakennustunnus and v.huoneisto=a.huoneisto
                                 inner join ",area," p on p.xyind = a.ykre_ruutu_xyind
                                 WHERE ika between 65 and 74 and kaytossaolotilanne_id < 6 and (v_yht <20 and a.kayttotarkoitus_id IN (011,012,013,021,022,032,039))
                                 group by p.area_id", sep=""))

## 75- year old
population_75_ = sqlQuery(cn, paste("select ikaryhma='v_75_', askunta=SUM(1/cast(v_yht as float)), henk=COUNT(v.id), p.area_id
from ",rhr," vaesto v inner join
                                 (select  r.kunta_id, v.rakennustunnus, huoneisto, kayttotarkoitus_id, v_yht=count(v.id), r.ykre_ruutu_xyind, r.kaytossaolotilanne_id
                                 from ",rhr," vaesto v 	inner join ",rhr," rakennus r on v.rakennustunnus=r.rakennustunnus
                                 group by v.rakennustunnus, huoneisto, kayttotarkoitus_id, r.kaytossaolotilanne_id, r.kunta_id, r.ykre_ruutu_xyind)
                                 as a on v.rakennustunnus=a.rakennustunnus and v.huoneisto=a.huoneisto
                                 inner join ",area," p on p.xyind = a.ykre_ruutu_xyind
                                 WHERE ika >= 75 and kaytossaolotilanne_id < 6 and (v_yht <20 and a.kayttotarkoitus_id IN (011,012,013,021,022,032,039))
                                 group by p.area_id", sep=""))

## Bind rows ##
all = rbind(population_0_6, population_7_14,population_15_17, population_18_23, population_24_29, 
           population_30_44, population_45_64, population_65_74, population_75_)

## Divide the number of certain age residents in household by the total residents in household ##
all = transform(all, kerroin = askunta / henk)

## Insert data in output matrix ##
foreach(i = 1:col) %do%{
  sub1 = subset(all$kerroin, all$area_id == i)
  ak_rate[,,i] = sub1}

## Give better column names ##
colnames(ak_rate) = c(seq(start_year, start_year + n_estimation_years -1,1))
rownames(ak_rate) = c("1_7v", "8_13v", "14_17v","18_25v","26_31v","32_49v","51_65v","66_74v","75_101v")
             
return(ak_rate)}


