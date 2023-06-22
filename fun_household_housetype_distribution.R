## Percentage of different households and house types          ##
## Erikokoisten asuntokuntien jakautuminen eri talotyyppeihin  ##
##-------------------------------------------------------------##                                                

## Read function parameters ##
fun_household_housetype_distribution=function(start_year, rhr, area, col){

## Create connectionto sql-server ##
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
shareMatrix = array(0, dim=c(n_households,n_housetypes, col))

## Extract population in different house types ##
  ## Detached houses
  detached_1= subset(rhr_data, rhr_data$Asukkaita==1 & rhr_data$kayttotarkoitus_id==11 |  rhr_data$Asukkaita==1 & rhr_data$kayttotarkoitus_id==12 |
                        rhr_data$Asukkaita==1 & rhr_data$kayttotarkoitus_id==13)
  detached_2= subset(rhr_data, rhr_data$Asukkaita==2 & rhr_data$kayttotarkoitus_id==11 |  rhr_data$Asukkaita==2 & rhr_data$kayttotarkoitus_id==12 |
                        rhr_data$Asukkaita==2 & rhr_data$kayttotarkoitus_id==13)
  detached_3= subset(rhr_data, rhr_data$Asukkaita>=3 & rhr_data$kayttotarkoitus_id==11 |  rhr_data$Asukkaita>=3 & rhr_data$kayttotarkoitus_id==12 |
                        rhr_data$Asukkaita>=3 & rhr_data$kayttotarkoitus_id==13)
  ## Attached houses
attached_1= subset(rhr_data, rhr_data$Asukkaita==1 & rhr_data$kayttotarkoitus_id==21 |  rhr_data$Asukkaita==1 & rhr_data$kayttotarkoitus_id==22)
attached_2= subset(rhr_data, rhr_data$Asukkaita==2 & rhr_data$kayttotarkoitus_id==21 |  rhr_data$Asukkaita==2 & rhr_data$kayttotarkoitus_id==22)
attached_3= subset(rhr_data, rhr_data$Asukkaita>=3 & rhr_data$kayttotarkoitus_id==21 |  rhr_data$Asukkaita>=3 & rhr_data$kayttotarkoitus_id==22)

  ## Apartment buildings
apartment_1= subset(rhr_data, rhr_data$Asukkaita==1 & rhr_data$kayttotarkoitus_id==32 |  rhr_data$Asukkaita==1 & rhr_data$kayttotarkoitus_id==39)
apartment_2= subset(rhr_data, rhr_data$Asukkaita==2 & rhr_data$kayttotarkoitus_id==32 |  rhr_data$Asukkaita>=2 & rhr_data$kayttotarkoitus_id==39)
apartment_3= subset(rhr_data, rhr_data$Asukkaita>=3 & rhr_data$kayttotarkoitus_id==32 |  rhr_data$Asukkaita>=3 & rhr_data$kayttotarkoitus_id==39)

## Bind rows together ##
tot_1_household=rbind(detached_1,attached_1, apartment_1)
tot_2_household=rbind(detached_2,attached_2,apartment_2)
tot_3_household=rbind(detached_3,attached_3,apartment_3)

## Calculate the percentage of different house holds in different house types ##
foreach(region = unique(rhr_data$area_id)) %do%{
shareMatrix[1,1, region]=(nrow(subset(detached_1,detached_1$area_id==region)))/(nrow(subset(tot_1_household, tot_1_household$area_id==region)))
shareMatrix[1,2, region]=(nrow(subset(attached_1,attached_1$area_id==region)))/(nrow(subset(tot_1_household, tot_1_household$area_id==region)))
shareMatrix[1,3, region]=(nrow(subset(apartment_1,apartment_1$area_id==region)))/(nrow(subset(tot_1_household, tot_1_household$area_id==region)))

shareMatrix[2,1,region]=(nrow(subset(detached_2,detached_2$area_id==region)))/(nrow(subset(tot_2_household, tot_2_household$area_id==region)))
shareMatrix[2,2,region]=(nrow(subset(attached_2,attached_2$area_id==region)))/(nrow(subset(tot_2_household, tot_2_household$area_id==region)))
shareMatrix[2,3,region]=(nrow(subset(apartment_2,apartment_2$area_id==region)))/(nrow(subset(tot_2_household, tot_2_household$area_id==region)))

shareMatrix[3,1,region]=(nrow(subset(detached_3,detached_3$area_id==region)))/(nrow(subset(tot_3_household, tot_3_household$area_id==region)))
shareMatrix[3,2,region]=(nrow(subset(attached_3,attached_3$area_id==region)))/(nrow(subset(tot_3_household, tot_3_household$area_id==region)))
shareMatrix[3,3,region]=(nrow(subset(apartment_3,apartment_3$area_id==region)))/(nrow(subset(tot_3_household, tot_3_household$area_id==region)))
}

## Give better column names ##
colnames(shareMatrix) = c("omakotitalo","rivitalo","kerrostalo")
rownames(shareMatrix) = c("1 hlö", "2 hlö", "3+ hlö")

return(shareMatrix)}


