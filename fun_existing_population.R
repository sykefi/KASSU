## Fetch the existing population from the study area ##         
##---------------------------------------------------##     

## Function returns amount of existing population in the study area
## Data is from the Building and Dwelling Register (DVV) 

## Read function parameters
fun_existing_population = function(area, rhr, col, n_ages){
  
## Create connection (cn) to MS SQL-server
cn = odbcDriverConnect(connection="Driver={SQL Server};server=server_name;trusted_connection=yes;")

## Fetch the existing population from the study area 
rhr_pop = sqlQuery(cn, paste("SELECT H.rakennustunnus, H.huoneisto, Asukkaita, sukupuoli_id, ika, p.area_id
FROM ",rhr," huoneisto H
           INNER JOIN ",rhr," rakennus R --inner join koska huoneistolle oltava rakennus
                      ON R.rakennustunnus = H.rakennustunnus
								INNER JOIN ",area," p on p.xyind = r.ykre_ruutu_xyind -- KASSU-mallissa käytettävä aluejako
										LEFT OUTER JOIN (SELECT rakennustunnus, sukupuoli_id, ika, huoneisto, COUNT(Id) as Asukkaita, --left join koska voi olla tyhjillään eli ei asukkaita huoneistolla (NULL tuloksena)
                                                                                       COUNT(CASE
                                                                                                WHEN ika>=65 THEN id
                                                                                                END) AS Asukkaita65,
                                                                                       COUNT(CASE
                                                                                                WHEN ika>=18 THEN id
                                                                                                END) AS Asukkaita18
                                                                 FROM ",rhr," vaesto
                                                                 GROUP BY rakennustunnus, huoneisto, sukupuoli_id, ika) V ON V.rakennustunnus=H.rakennustunnus AND V.huoneisto=H.huoneisto
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
                             order by area_id asc" , sep=""))

## Set NA = 0 
rhr_pop[is.na(rhr_pop)]  =  0

## Create unique ID for different sub-study  areas
ID = unique(rhr_pop$area_id)

## Set the maximum age to 100 (Over 100 year old will be aggregated to 100)
rhr_pop[rhr_pop$ika > 100,3] = 100
rhr_pop = ddply(rhr_pop,.(area_id, sukupuoli_id, ika),summarize,asukkaita=sum(Asukkaita))

## Separate males 
male_pop = subset(rhr_pop, sukupuoli_id == 1)
male_pop %>% group_by(area_id, ika) %>% summarise(asukkaita = sum(asukkaita))

## Separate females
female_pop = subset(rhr_pop, sukupuoli_id == 2)
female_pop %>% group_by(area_id, ika) %>% summarise(asukkaita = sum(asukkaita))

## Create output matrix and calculate existing population
pop_matrix_m = matrix(0,nrow=n_ages,ncol=col) # male
pop_matrix_f = matrix(0,nrow=n_ages,ncol=col) # female

## Males
for(inde in 0:n_ages-1){
  for(inde_b in 1:col){
    
    dummy_m = ID[inde_b]
    dummy_indices_m  =  which(male_pop[,3] == inde & male_pop[,1]==dummy_m) #select rows where age = inde 
    lukumaara  =  male_pop[dummy_indices_m,4] 
    if (length(lukumaara>0)){pop_matrix_m[inde+1,inde_b] =  lukumaara} 
    else pop_matrix_m[inde+1,inde_b] = 0
    {  
      
## Females
      dummy_f = ID[inde_b]
      dummy_indices_f  =  which(female_pop[,3]== inde & female_pop[,1]==dummy_f) #select rows where age = inde 
      lukumaara  =  female_pop[dummy_indices_f,4] 
      if (length(lukumaara>0)){pop_matrix_f[inde+1,inde_b] =  lukumaara} 
      else pop_matrix_f[inde+1,inde_b] = 0
    }
  }
}

## aggregate population matrices in to a list
output1 = list("vaesto_naiset"=pop_matrix_f, "vaesto_miehet"= pop_matrix_m)

return(output1)
}


