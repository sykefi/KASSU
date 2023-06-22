## Mean size of different households (m2)                                   ##                                                          
## Erikokoisten asuntokuntien keskimääräinen pinta-ala talotyypeittäin (m2) ## 
##--------------------------------------------------------------------------##

## Read function parameters ##
fun_household_meansize = function(start_year, rhr, area, col){

## Create connection to sql-server ##
connection_name = paste("Driver={SQL Server};server=server_name;database=database_name;trusted_connection=yes;", sep="")
cn = odbcDriverConnect(connection=connection_name)

## Calculate number of residents in different apartment and house types ##
rhr_data = sqlQuery(cn, paste("SELECT H.rakennustunnus, H.huoneisto, Asukkaita, p.area_id, h.huoneluku, h.pinta_ala, r.kerrosala, r.kayttotarkoitus_id
FROM ",rhr," huoneisto H
           INNER JOIN ",rhr," rakennus R --inner join koska huoneistolle oltava rakennus
                      ON R.rakennustunnus = H.rakennustunnus
								INNER JOIN ",area," p on p.xyind = r.ykre_ruutu_xyind -- KASSU-mallissa käytettävä aluejako
										LEFT OUTER JOIN (SELECT rakennustunnus, huoneisto, COUNT(Id) as Asukkaita, --left join koska voi olla tyhjillään eli ei asukkaita huoneistolla (NULL tuloksena)
                                                                                       COUNT(CASE
                                                                                                WHEN ika>=65 THEN id
                                                                                                END) AS Asukkaita65,
                                                                                       COUNT(CASE
                                                                                                  WHEN ika>=18 THEN id
                                                                                                  END) AS Asukkaita18
                                                                 FROM ",rhr," vaesto
                                                                 GROUP BY rakennustunnus, huoneisto) V ON V.rakennustunnus=H.rakennustunnus AND V.huoneisto=H.huoneisto
                                                                 WHERE 
Asukkaita is not null AND
H.kaytossaolotilanne_id IN ('01', '04', '05')
		   AND R.kayttotarkoitus_id IN ('011','012','013','021','022','032','039')
           AND H.pinta_ala >= 7
           AND R.kaytossaolotilanne_id NOT IN ('06', '07')
           AND R.valmiusaste_id = '1'
           AND (
             R.kayttotarkoitus_id NOT IN (041,131,811,819,891,892,893,899,931,941,999) 
                      OR(
                      R.kayttotarkoitus_id IN (041,811,819,891,892,893,899,931,941,999) 
                      --näistä asunnot joissa asukkaita. 
                                 AND V.Asukkaita > 0) 
                                 OR(
                      R.kayttotarkoitus_id = 131
                      --näistä asunnot joissa asukkaita ja yli 65v max 5 ja yli 18v max 10. 
                                 AND V.Asukkaita > 0
                                 AND V.asukkaita65 <= 5
                                 AND V.Asukkaita18 <= 10))      
						", sep=""))
## Create output matrix ##
n_households = 3 # number of households
n_housetypes = 3 # number of house types
shareMatrixMeanSize = array(0, dim=c(n_households,n_housetypes, col))

## Exctract population for different housetypes ##
  # Detached houses
  detached_1 = subset(rhr_data, rhr_data$Asukkaita==1 & rhr_data$kayttotarkoitus_id==11 |  rhr_data$Asukkaita==1 & rhr_data$kayttotarkoitus_id==12 |
                        rhr_data$Asukkaita==1 & rhr_data$kayttotarkoitus_id==13)
  detached_2 = subset(rhr_data, rhr_data$Asukkaita==2 & rhr_data$kayttotarkoitus_id==11 |  rhr_data$Asukkaita==2 & rhr_data$kayttotarkoitus_id==12 |
                        rhr_data$Asukkaita==2 & rhr_data$kayttotarkoitus_id==13)
  detached_3 = subset(rhr_data, rhr_data$Asukkaita>=3 & rhr_data$kayttotarkoitus_id==11 |  rhr_data$Asukkaita>=3 & rhr_data$kayttotarkoitus_id==12 |
                        rhr_data$Asukkaita>=3 & rhr_data$kayttotarkoitus_id==13)
  # Attached houses
  attached_1 = subset(rhr_data, rhr_data$Asukkaita==1 & rhr_data$kayttotarkoitus_id==21 |  rhr_data$Asukkaita==1 & rhr_data$kayttotarkoitus_id==22)
  attached_2 = subset(rhr_data, rhr_data$Asukkaita==2 & rhr_data$kayttotarkoitus_id==21 |  rhr_data$Asukkaita==2 & rhr_data$kayttotarkoitus_id==22)
  attached_3 = subset(rhr_data, rhr_data$Asukkaita>=3 & rhr_data$kayttotarkoitus_id==21 |  rhr_data$Asukkaita>=3 & rhr_data$kayttotarkoitus_id==22)
  
  # Apartment buildings
  apartment_1 = subset(rhr_data, rhr_data$Asukkaita==1 & rhr_data$kayttotarkoitus_id==32 |  rhr_data$Asukkaita==1 & rhr_data$kayttotarkoitus_id==39)
  apartment_2 = subset(rhr_data, rhr_data$Asukkaita==2 & rhr_data$kayttotarkoitus_id==32 |  rhr_data$Asukkaita>=2 & rhr_data$kayttotarkoitus_id==39)
  apartment_3 = subset(rhr_data, rhr_data$Asukkaita>=3 & rhr_data$kayttotarkoitus_id==32 |  rhr_data$Asukkaita>=3 & rhr_data$kayttotarkoitus_id==39)


## Calculate mean size (m2) for different housetypes in different households ##
foreach(region = 1:length(unique(rhr_data$area_id))) %do%{
  sub_detached_1 = subset(detached_1, detached_1$area_id==region)
  sub_attached_1 = subset(attached_1, attached_1$area_id==region)
  sub_apartment_1 = subset(apartment_1, apartment_1$area_id==region)
  
  sub_detached_2 = subset(detached_2, detached_2$area_id==region)
  sub_attached_2 = subset(attached_2, attached_2$area_id==region)
  sub_apartment_2 = subset(apartment_2, apartment_2$area_id==region)
  
  sub_detached_3 = subset(detached_3, detached_3$area_id==region)
  sub_attached_3 = subset(attached_3, attached_3$area_id==region)
  sub_apartment_3 = subset(apartment_3, apartment_3$area_id==region)
  
  shareMatrixMeanSize[1,1, region]=mean(sub_detached_1$pinta_ala)
  shareMatrixMeanSize[1,2, region]=mean(sub_attached_1$pinta_ala)
  shareMatrixMeanSize[1,3, region]=mean(sub_apartment_1$pinta_ala)
  
  shareMatrixMeanSize[2,1,region]=mean(sub_detached_2$pinta_ala)
  shareMatrixMeanSize[2,2,region]=mean(sub_attached_2$pinta_ala)
  shareMatrixMeanSize[2,3,region]=mean(sub_apartment_2$pinta_ala)
  
  shareMatrixMeanSize[3,1,region]=mean(sub_detached_3$pinta_ala)
  shareMatrixMeanSize[3,2,region]=mean(sub_attached_3$pinta_ala)
  shareMatrixMeanSize[3,3,region]=mean(sub_apartment_3$pinta_ala)
}

## Give better column names ##
colnames(shareMatrixMeanSize) = c("omakotitalo","rivitalo","kerrostalo")
rownames(shareMatrixMeanSize) = c("1 hlö", "2 hlö", "3+ hlö")

## Convert NAN values with zero
shareMatrixMeanSize[is.nan(shareMatrixMeanSize)] = 0
shareMatrixMeanSize[is.na(shareMatrixMeanSize)] = 0

return(shareMatrixMeanSize)}










