## Size of Household-dwelling unit ##
## Asuntokuntien kokojakauma       ##     
##---------------------------------##

## We use three different family size:
## 1 person household
## 2 person household
## 3 or more person household

## Read function parameters
fun_familysize = function(start_year, rhr, area, col){

## Create output matrix
n_age_groups = 9 # number of age groups
n_family_size = 3 # number of different family size groups
f_size = array(0,dim=c(n_age_groups, n_family_size, col))
  
## Create connectionto sql-server
cn  =  odbcDriverConnect(connection="Driver={SQL Server};server=server_name;trusted_connection=yes;")

## Code below uses auxilliary table [RHR_ASUNTOPOIMINTA2021] that includes housing stock in Finland. 
## Define the location of the auxiliary table in sql-server
aux_table = "RHR_ASUNTOPOIMINTA2021"

## Fetch 0-6 year old
population_0_6  = sqlQuery(cn, paste("Select r.rakennustunnus, r.huoneisto, r.Asukkaita, ikaryhma0_6v, r.kunta_id, area_id
from ",aux_table," r
inner join (select v.rakennustunnus, v.huoneisto,count(v.id) as ikaryhma0_6v FROM ",rhr,"vaesto v
where v.ika between 0 and 6 group by v.rakennustunnus, v.huoneisto
                                ) v on  v.rakennustunnus = r.rakennustunnus and v.huoneisto = r.huoneisto 
                                inner join ",area," P on p.xyind = r.ykre_ruutu_xyind", sep=""))

## Remove NA-values
population_0_6_ = na.omit(population_0_6)

## Calculate the percentage for every households 
foreach (regions = 1: length(unique(population_0_6_$area_id))) %do% {
  hlo1 = subset(population_0_6_, population_0_6_$Asukkaita==1 & population_0_6_$area_id==regions) #1 person household
  f_size[1,1,regions] = nrow(hlo1)/nrow(subset(population_0_6_,population_0_6_$area_id==regions))

  hlo2 = subset(population_0_6_, population_0_6_$Asukkaita==2 & population_0_6_$area_id==regions) #2 person household
  f_size[1,2,regions] = nrow(hlo2)/nrow(subset(population_0_6_,population_0_6_$area_id==regions))

  hlo3 = subset(population_0_6_, population_0_6_$Asukkaita>=3 & population_0_6_$area_id==regions) #3 person household
  f_size[1,3,regions] = nrow(hlo3)/nrow(subset(population_0_6_,population_0_6_$area_id==regions))}


## 7-12 year old 
population_7_12 = sqlQuery(cn, paste("Select r.rakennustunnus, r.huoneisto, r.Asukkaita, ikaryhma7_12v, r.kunta_id, p.area_id
from ",aux_table," r inner join (select v.rakennustunnus, v.huoneisto,count(v.id) as ikaryhma7_12v FROM ",rhr,"vaesto v
                                where v.ika between 7 and 12 group by v.rakennustunnus, v.huoneisto
                                ) v on  v.rakennustunnus = r.rakennustunnus and v.huoneisto = r.huoneisto 
                                inner join ",area," P on p.xyind = r.ykre_ruutu_xyind" , sep=""))

population_7_12_ = na.omit(population_7_12)

foreach (regions = 1: length(unique(population_7_12_$area_id))) %do% {
  hlo1 = subset(population_7_12_, population_7_12_$Asukkaita==1 & population_7_12_$area_id==regions) 
  f_size[2,1,regions] = nrow(hlo1)/nrow(subset(population_7_12_,population_7_12_$area_id== regions))
  
  hlo2 = subset(population_7_12_, population_7_12_$Asukkaita==2 & population_7_12_$area_id==regions) 
  f_size[2,2,regions] = nrow(hlo2)/nrow(subset(population_7_12_,population_7_12_$area_id== regions))
  
  hlo3 = subset(population_7_12_, population_7_12_$Asukkaita>=3 & population_7_12_$area_id==regions) 
  f_size[2,3,regions] = nrow(hlo3)/nrow(subset(population_7_12_,population_7_12_$area_id== regions))}


## 13-16 year old 
population_13_16  = sqlQuery(cn, paste("Select r.rakennustunnus, r.huoneisto, r.Asukkaita, ikaryhma13_16v, r.kunta_id, p.area_id
from ",aux_table," r inner join (select v.rakennustunnus, v.huoneisto,count(v.id) as ikaryhma13_16v FROM ",rhr,"vaesto v
                                  where v.ika between 13 and 16 group by v.rakennustunnus, v.huoneisto
                                  ) v on  v.rakennustunnus = r.rakennustunnus and v.huoneisto = r.huoneisto 
                                  inner join ",area," P on p.xyind = r.ykre_ruutu_xyind" , sep=""))

population_13_16_ = na.omit(population_13_16)

foreach (regions = 1: length(unique(population_13_16_$area_id))) %do% {
  hlo1 = subset(population_13_16_, population_13_16_$Asukkaita==1 & population_13_16_$area_id==regions) 
  f_size[3,1,regions] = nrow(hlo1)/nrow(subset(population_13_16_, population_13_16_$area_id==regions))
  
  hlo2 = subset(population_13_16_, population_13_16_$Asukkaita==2 & population_13_16_$area_id==regions)
  f_size[3,2,regions] = nrow(hlo2)/nrow(subset(population_13_16_, population_13_16_$area_id==regions))
  
  hlo3 = subset(population_13_16_, population_13_16_$Asukkaita>=3 & population_13_16_$area_id==regions) 
  f_size[3,3,regions] = nrow(hlo3)/nrow(subset(population_13_16_, population_13_16_$area_id==regions))}


## 17-24 year old 
population_17_24  = sqlQuery(cn, paste("Select r.rakennustunnus, r.huoneisto, r.Asukkaita, ikaryhma17_24v, r.kunta_id, p.area_id
from ",aux_table," r inner join (select v.rakennustunnus, v.huoneisto,count(v.id) as ikaryhma17_24v FROM ",rhr,"vaesto v
                                  where v.ika between 17 and 24 group by v.rakennustunnus, v.huoneisto
                                  ) v on  v.rakennustunnus = r.rakennustunnus and v.huoneisto = r.huoneisto 
                                  inner join ",area," P on p.xyind = r.ykre_ruutu_xyind" , sep=""))

population_17_24_ = na.omit(population_17_24)

foreach (regions = 1: length(unique(population_17_24_$area_id))) %do% {
  hlo1 = subset(population_17_24_, population_17_24_$Asukkaita==1 & population_17_24_$area_id==regions) 
  f_size[4,1,regions] = nrow(hlo1)/nrow(subset(population_17_24_,population_17_24_$area_id==regions))
  
  hlo2 = subset(population_17_24_, population_17_24_$Asukkaita==2 & population_17_24_$area_id==regions) 
  f_size[4,2,regions] = nrow(hlo2)/nrow(subset(population_17_24_,population_17_24_$area_id==regions))
  
  hlo3 = subset(population_17_24_, population_17_24_$Asukkaita>=3 & population_17_24_$area_id==regions)
  f_size[4,3,regions] = nrow(hlo3)/nrow(subset(population_17_24_,population_17_24_$area_id==regions))}


## 25-30 year old 
population_25_30  = sqlQuery(cn, paste("Select r.rakennustunnus, r.huoneisto, r.Asukkaita, ikaryhma25_30v, r.kunta_id, p.area_id
from ",aux_table," r inner join (select v.rakennustunnus, v.huoneisto,count(v.id) as ikaryhma25_30v FROM ",rhr,"vaesto v
                                  where v.ika between 25 and 30 group by v.rakennustunnus, v.huoneisto
                                  ) v on  v.rakennustunnus = r.rakennustunnus and v.huoneisto = r.huoneisto 
                                  inner join ",area," P on p.xyind = r.ykre_ruutu_xyind" , sep=""))

population_25_30_ = na.omit(population_25_30)

foreach (regions = 1: length(unique(population_25_30_$area_id))) %do% {
  hlo1 = subset(population_25_30_, population_25_30_$Asukkaita==1 & population_25_30_$area_id==regions) 
  f_size[5,1,regions] = nrow(hlo1)/nrow(subset(population_25_30_,population_25_30_$area_id==regions))
  
  hlo2 = subset(population_25_30_, population_25_30_$Asukkaita==2 & population_25_30_$area_id==regions) 
  f_size[5,2,regions] = nrow(hlo2)/nrow(subset(population_25_30_,population_25_30_$area_id==regions))
  
  hlo3 = subset(population_25_30_, population_25_30_$Asukkaita>=3 & population_25_30_$area_id==regions) 
  f_size[5,3,regions] = nrow(hlo3)/nrow(subset(population_25_30_,population_25_30_$area_id==regions))}


## 31-49 year old 
population_31_49  = sqlQuery(cn, paste("Select r.rakennustunnus, r.huoneisto, r.Asukkaita, ikaryhma31_49v, r.kunta_id, p.area_id
from ",aux_table," r inner join (select v.rakennustunnus, v.huoneisto,count(v.id) as ikaryhma31_49v FROM ",rhr,"vaesto v
                                  where v.ika between 31 and 49 group by v.rakennustunnus, v.huoneisto
                                  ) v on  v.rakennustunnus = r.rakennustunnus and v.huoneisto = r.huoneisto 
                                  inner join ",area," P on p.xyind = r.ykre_ruutu_xyind" , sep=""))

population_31_49_ = na.omit(population_31_49)

foreach (regions = 1: length(unique(population_31_49_$area_id))) %do% {
  hlo1 = subset(population_31_49_, population_31_49_$Asukkaita==1 & population_31_49_$area_id==regions) 
  f_size[6,1,regions] = nrow(hlo1)/nrow(subset(population_31_49_, population_31_49_$area_id==regions))
  
  hlo2 = subset(population_31_49_, population_31_49_$Asukkaita==2 & population_31_49_$area_id==regions) 
  f_size[6,2,regions] = nrow(hlo2)/nrow(subset(population_31_49_, population_31_49_$area_id==regions))
  
  hlo3 = subset(population_31_49_, population_31_49_$Asukkaita>=3 & population_31_49_$area_id==regions) 
  f_size[6,3,regions] = nrow(hlo3)/nrow(subset(population_31_49_, population_31_49_$area_id==regions))}


## 50-64 year old 
population_50_64  = sqlQuery(cn, paste("Select r.rakennustunnus, r.huoneisto, r.Asukkaita, ikaryhma50_64v, r.kunta_id, p.area_id
from ",aux_table," r inner join (select v.rakennustunnus, v.huoneisto,count(v.id) as ikaryhma50_64v FROM ",rhr,"vaesto v
                                  where v.ika between 50 and 64 group by v.rakennustunnus, v.huoneisto
                                  ) v on  v.rakennustunnus = r.rakennustunnus and v.huoneisto = r.huoneisto 
                                  inner join ",area," P on p.xyind = r.ykre_ruutu_xyind" , sep=""))

population_50_64_ = na.omit(population_50_64)

foreach (regions = 1: length(unique(population_50_64_$area_id))) %do% {
  hlo1 = subset(population_50_64_, population_50_64_$Asukkaita==1 & population_50_64_$area_id==regions) 
  f_size[7,1,regions] = nrow(hlo1)/nrow(subset(population_50_64_,population_50_64_$area_id==regions))
  
  hlo2 = subset(population_50_64_, population_50_64_$Asukkaita==2 & population_50_64_$area_id==regions) 
  f_size[7,2,regions] = nrow(hlo2)/nrow(subset(population_50_64_,population_50_64_$area_id==regions))
  
  hlo3 = subset(population_50_64_, population_50_64_$Asukkaita>=3 & population_50_64_$area_id==regions) 
  f_size[7,3,regions] = nrow(hlo3)/nrow(subset(population_50_64_,population_50_64_$area_id==regions))}


## 65-74 year old 
population_65_74  = sqlQuery(cn, paste("Select r.rakennustunnus, r.huoneisto, r.Asukkaita, ikaryhma65_74v, r.kunta_id, p.area_id
from ",aux_table," r inner join (select v.rakennustunnus, v.huoneisto,count(v.id) as ikaryhma65_74v FROM ",rhr,"vaesto v
                                  where v.ika between 65 and 74 group by v.rakennustunnus, v.huoneisto
                                  ) v on  v.rakennustunnus = r.rakennustunnus and v.huoneisto = r.huoneisto 
                                  inner join ",area," P on p.xyind = r.ykre_ruutu_xyind" , sep=""))

population_65_74_ = na.omit(population_65_74)

foreach (regions = 1: length(unique(population_65_74_$area_id))) %do% {
  hlo1 = subset(population_65_74_, population_65_74_$Asukkaita==1 & population_65_74_$area_id==regions) 
  f_size[8,1,regions] = nrow(hlo1)/nrow(subset(population_65_74_, population_65_74_$area_id==regions))
  
  hlo2 = subset(population_65_74_, population_65_74_$Asukkaita==2 & population_65_74_$area_id==regions)
  f_size[8,2,regions] = nrow(hlo2)/nrow(subset(population_65_74_, population_65_74_$area_id==regions))
  
  hlo3 = subset(population_65_74_, population_65_74_$Asukkaita>=3 & population_65_74_$area_id==regions) 
  f_size[8,3,regions] = nrow(hlo3)/nrow(subset(population_65_74_, population_65_74_$area_id==regions))}



## 75- year old 
population_75  = sqlQuery(cn, paste("Select r.rakennustunnus, r.huoneisto, r.Asukkaita, ikaryhma75_v, r.kunta_id, p.area_id
from ",aux_table," r inner join (select v.rakennustunnus, v.huoneisto,count(v.id) as ikaryhma75_v FROM ",rhr,"vaesto v
                                  where v.ika >=75 group by v.rakennustunnus, v.huoneisto
                                  ) v on  v.rakennustunnus = r.rakennustunnus and v.huoneisto = r.huoneisto 
                                  inner join ",area," P on p.xyind = r.ykre_ruutu_xyind" , sep=""))

population_75_ = na.omit(population_75)

foreach (regions = 1: length(unique(population_75_$area_id))) %do% {
  hlo1 = subset(population_75_, population_75_$Asukkaita==1 & population_75_$area_id==regions) 
  f_size[9,1,regions] = nrow(hlo1)/nrow(subset(population_75_, population_75_$area_id==regions))
  
  hlo2 = subset(population_75_, population_75_$Asukkaita==2 & population_75_$area_id==regions) 
  f_size[9,2,regions] = nrow(hlo2)/nrow(subset(population_75_, population_75_$area_id==regions))
  
  hlo3 = subset(population_75_, population_75_$Asukkaita>=3 & population_75_$area_id==regions) 
  f_size[9,3,regions] = nrow(hlo3)/nrow(subset(population_75_, population_75_$area_id==regions))}

## Give better column names
colnames(f_size)  =  c("1 hlo", "2 hlo", "3+ hlo")
rownames(f_size)  =  c("1_7v", "8_13v", "14_17v","18_25v","26_31v","32_49v","51_65v","66_74v","75_101v")

return(f_size)

}
