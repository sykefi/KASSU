## Existing housing stock (count of buildings & floor space (1000 kem2)) ##                                      
## Olemassa oleva rakennuskanta (lukumäärä & kerrosala (1000 kem2))      ##
##------------------------------------------------------------------##

## Read function parameters 
fun_existing_rhr_stock = function(rhr, area, col){

## Create connectionto sql-server  
connection_name = paste("Driver={SQL Server};server=server_name;trusted_connection=yes")
cn = odbcDriverConnect(connection=connection_name)


## Fetch existing apartment count ##
## Haetaan olemassa rakennusten maara ##
ExistingApartment = sqlQuery(cn, paste("SELECT count (H.huoneisto) as huoneisto_lkm, R.kayttotarkoitus_id, area_id
FROM ",rhr,"huoneisto H
           INNER JOIN ",rhr," rakennus R --inner join koska huoneistolle oltava rakennus
                      ON R.rakennustunnus = H.rakennustunnus
							INNER JOIN ",area," p on p.xyind = r.ykre_ruutu_xyind
                                 LEFT OUTER JOIN (SELECT rakennustunnus, huoneisto, COUNT(Id) as Asukkaita, --left join koska voi olla tyhjillään eli ei asukkaita huoneistolla (NULL tuloksena)
                                                                                       COUNT(CASE
                                                                                                WHEN ika>=65 THEN id
                                                                                                END) AS Asukkaita65,
                                                                                       COUNT(CASE
                                                                                                  WHEN ika>=18 THEN id
                                                                                       END) AS Asukkaita18
                                                                 FROM ",rhr,"vaesto
                                                                 GROUP BY rakennustunnus, huoneisto) V ON V.rakennustunnus=H.rakennustunnus AND V.huoneisto=H.huoneisto
WHERE 
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
								 group by p.area_id,r.kayttotarkoitus_id
									", sep=""))

## Set NA = 0
ExistingApartment[is.na(ExistingApartment)] = 0

## Create outpu matrix
ExistingApartmentCount = matrix(nrow=col, ncol=3)

## Give better column names 
colnames(ExistingApartmentCount) = c("omakotitalo","rivitalo","kerrostalo")

## Extract different house types
# Detached house
foreach (regions = 1: col) %do% { 
  detachedApartmentCount=subset(ExistingApartment, ExistingApartment$area_id==regions) 
  ExistingApartmentCount[regions,1]=sum(detachedApartmentCount[detachedApartmentCount$kayttotarkoitus_id ==11| 
                                                             detachedApartmentCount$kayttotarkoitus_id ==12 | 
                                                             detachedApartmentCount$kayttotarkoitus_id ==13,]$huoneisto_lkm)}
# Attached house
foreach (regions = 1: col) %do% { 
  attachedApartmentCount=subset(ExistingApartment, ExistingApartment$area_id==regions)
  ExistingApartmentCount[regions,2]=sum(attachedApartmentCount[attachedApartmentCount$kayttotarkoitus_id ==21| 
                                                             attachedApartmentCount$kayttotarkoitus_id ==22,]$huoneisto_lkm)}

# Apartments
foreach (regions = 1: col) %do% { #
  apartmentApartmentCount=subset(ExistingApartment, ExistingApartment$area_id==regions)
  ExistingApartmentCount[regions,3]=sum(apartmentApartmentCount[apartmentApartmentCount$kayttotarkoitus_id ==32|
                                                              apartmentApartmentCount$kayttotarkoitus_id ==39,]$huoneisto_lkm)}


## Fetch existing floor space
## Haetaan olemassa oleva kerrosala
existingStock = sqlQuery(cn, paste("SELECT sum(kerrosala) AS kerrosala, R.kayttotarkoitus_id, area_id
FROM ",rhr," huoneisto H
           INNER JOIN ",rhr," rakennus R --inner join koska huoneistolle oltava rakennus
                      ON R.rakennustunnus = H.rakennustunnus
							INNER JOIN ",area," p on p.xyind = r.ykre_ruutu_xyind
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
								 group by p.area_id,r.kayttotarkoitus_id
									", sep=""))

sum(existingStock$kerrosala)
## Set NA = 0
existingStock[is.na(existingStock)] = 0

## Create outpu matrix
ExistingFloorArea = matrix(nrow=col, ncol=3)

## Give better column names 
colnames(ExistingFloorArea) = c("omakotitalo","rivitalo","kerrostalo")

## Extract different housetypes
# Detached house
foreach (regions = 1: col) %do% { # n_region = area_ididen maara
  detachedStock=subset(existingStock, existingStock$area_id==regions) # Irrotetaan data area_id:n mukaan
  ExistingFloorArea[regions,1]=sum(detachedStock[detachedStock$kayttotarkoitus_id ==11| # Valitaan omakotitalot 
                                                   detachedStock$kayttotarkoitus_id ==12 | 
                                                   detachedStock$kayttotarkoitus_id ==13,]$kerrosala)}

# Attached house
foreach (regions = 1: col) %do% { # n_region = area_ididen maara
  attachedStock=subset(existingStock, existingStock$area_id==regions)
  ExistingFloorArea[regions,2]=sum(attachedStock[attachedStock$kayttotarkoitus_id ==21| # Valitaan rivitalot
                                                   attachedStock$kayttotarkoitus_id ==22,]$kerrosala)}

# Apartments
foreach (regions = 1: col) %do% { # n_region = area_ididen maara
  apartmentStock=subset(existingStock, existingStock$area_id==regions)
  ExistingFloorArea[regions,3]=sum(apartmentStock[apartmentStock$kayttotarkoitus_id ==32|# Valitaan kerrostalot
                                                    apartmentStock$kayttotarkoitus_id ==39,]$kerrosala)}

## convert to 1000m2 
ExistingFloorArea_ = ExistingFloorArea/1000

output=list("Olemassa oleva asuntojen lukumaara" = ExistingApartmentCount, "Olemassa oleva asuntojen kerrosala" = ExistingFloorArea_)

return(output)
}

