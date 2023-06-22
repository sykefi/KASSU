## Female out migration         ##
## Author: @Arto Viinikka, SYKE ##                              

## Function returns amount of female out-migration from the study area using past 5 year average
## Data is from the Statistics Finland grid data (250 m * 250 m) 

# Read function parameters
fun_out_migration_female = function(area, mod_outmig_rate){
  
## Create connection (cn) to MS SQL-server
cn = odbcDriverConnect(connection="Driver={SQL Server};server=server_name;trusted_connection=yes;")
  
## Read migration data from SQL-server
mig_out_female = sqlQuery(cn, paste("Select p.area_id as area_lahto, SUM(v_0_3n) AS v_0_3n, SUM(v_4_6n) AS v_4_6n, SUM(v_7_9n) AS v_7_9n, SUM(v_10_14n) AS v_10_14n, SUM(v_15_17n) AS v_15_17n, SUM(v_18n) AS v_18n, 
                  SUM(v_19n) AS v_19n, SUM(v_20n) AS v_20n, SUM(v_21n) AS v_21n, SUM(v_22n) AS v_22n, SUM(v_23n) AS v_23n, SUM(v_24n) AS v_24n,SUM(v_25n) AS v_25n, SUM(v_26n) AS v_26n, 
                  SUM(v_27n) AS v_27n, SUM(v_28n) AS v_28n, SUM(v_29n) AS v_29n, SUM(v_30n) AS v_30n,  SUM(v_31n) AS v_31n, SUM(v_32n) AS v_32n, SUM(v_33n) AS v_33n, 
                  SUM(v_34n) AS v_34n, SUM(v_35n) AS v_35n, SUM(v_36n) AS v_36n,SUM(v_37n) AS v_37n, SUM(v_38n) AS v_38n, SUM(v_39n) AS v_39n, SUM(v_40_44n) AS v_40_44n, 
                  SUM(v_45_49n) AS v_45_49n, SUM(v_50_54n) AS v_50_54n, SUM(v_55_59n) AS v_55_59n, SUM(v_60_64n) AS v_60_64n, SUM(v_65_69n) AS v_65_69n, SUM(v_70_74n) AS v_70_74n,  
                  SUM(v_75n) AS v_75n From ",migration," m
                  inner join ",area," p -- CHANGE STUDY AREA
                  on p.xyind = m.lxyind
                  where m.vuosi IN (2017, 2018, 2019, 2020, 2021) -- sum of the 5 year in-migration
                  group by  p.area_id, m.vuosi
                  order by area_lahto asc")) 
  
## Calculate 5 year average 
Outmigration_female = mig_out_female %>% 
                        group_by(area_lahto) %>% 
                        summarise_each(list(mean))

## MODIFY DATA ##
##-------------##

## Convert data frame to matrix
## We exclude the [area_id] column
df_mig = data.matrix(Outmigration_female[,2:ncol(Outmigration_female)])

## Transpose matrix
df_mig = t(df_mig)

## Divide the migration to 1-year age groups
## Set replicates for each row
duptimes = c(4,3,3,5,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,5,5,5,5,5,5,26)

## Create an index of the rows you want with duplications
idx = rep(1:nrow(df_mig), duptimes)

## Use that index to generate your new data frame
dup_mig = df_mig[idx,]

## Divide migration to 1 year age groups
dup_mig[1:4,] = dup_mig[1:4,]/4
dup_mig[5:7,] = dup_mig[5:7,]/3
dup_mig[8:10,] = dup_mig[8:10,]/3
dup_mig[11:15,] = dup_mig[11:15,]/5
dup_mig[16:18,] = dup_mig[16:18,]/3
dup_mig[41:45,] = dup_mig[41:45,]/5
dup_mig[46:50,] = dup_mig[46:50,]/5
dup_mig[51:55,] = dup_mig[51:55,]/5
dup_mig[56:60,] = dup_mig[56:60,]/5
dup_mig[61:65,] = dup_mig[61:65,]/5
dup_mig[66:70,] = dup_mig[66:70,]/5
dup_mig[71:75,] = dup_mig[71:75,]/5
dup_mig[76:101,] = dup_mig[76:101,]/26

## ADJUST THE MIGRATION RATE ##
##---------------------------##

## Variable [mod_inmig_rate] is created in kassu.RMD master file
dup_mig = dup_mig * mod_outmig_rate

## Write migration to output file
output4 = dup_mig
  
return (output4)}

