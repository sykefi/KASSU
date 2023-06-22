## Male out-migration            ##
## Author: @Arto Viinikka, SYKE  ##                              

## Function returns amount of male out-migration from the study area using past 5 year average
## Data is from the Statistics Finland grid data (250 m * 250 m) 

## Read function parameters
fun_out_migration_male = function(area, mod_outmig_rate){
  
## Create connection (cn) to MS SQL-server
cn = odbcDriverConnect(connection="Driver={SQL Server};server=server_name;trusted_connection=yes;")
  
## Read migration data from SQL-server
mig_out_male = sqlQuery(cn, paste("Select p.area_id as area_lahto,SUM(v_0_3m) AS v_0_3m, SUM(v_4_6m) AS v_4_6m, SUM(v_7_9m) AS v_7_9m, SUM(v_10_14m) AS v_10_14m, SUM(v_15_17m) AS v_15_17m, SUM(v_18m) AS v_18m, 
                SUM(v_19m) AS v_19m, SUM(v_20m) AS v_20m, SUM(v_21m) AS v_21m, SUM(v_22m) AS v_22m, SUM(v_23m) AS v_23m, SUM(v_24m) AS v_24m,SUM(v_25m) AS v_25m, SUM(v_26m) AS v_26m, 
                SUM(v_27m) AS v_27m, SUM(v_28m) AS v_28m, SUM(v_29m) AS v_29m, SUM(v_30m) AS v_30m,  SUM(v_31m) AS v_31m, SUM(v_32m) AS v_32m, SUM(v_33m) AS v_33m, 
                SUM(v_34m) AS v_34m, SUM(v_35m) AS v_35m, SUM(v_36m) AS v_36m,SUM(v_37m) AS v_37m, SUM(v_38m) AS v_38m, SUM(v_39m) AS v_39m, SUM(v_40_44m) AS v_40_44m, 
                SUM(v_45_49m) AS v_45_49m, SUM(v_50_54m) AS v_50_54m, SUM(v_55_59m) AS v_55_59m, SUM(v_60_64m) AS v_60_64m, SUM(v_65_69m) AS v_65_69m, SUM(v_70_74m) AS v_70_74m,  
                SUM(v_75m) AS v_75m  From ",migration," m
                inner join ",area," p 
                on p.xyind = m.lxyind
                where m.vuosi IN (2017, 2018, 2019, 2020, 2021)-- sum of the 5 year in-migration
                group by  p.area_id, m.vuosi
                order by area_lahto asc"))
  
## Calculate 5 year average  
Outmigration_male = mig_out_male %>% 
  group_by(area_lahto) %>% 
  summarise_each(list(mean))

## MODIFY DATA ##
##-------------##

## Convert data frame to matrix
## We exclude the [area_id] columns
df_mig = data.matrix(Outmigration_male[,2:ncol(Outmigration_male)])

## Transpose matrix
df_mig = t(df_mig)

## Divide the migration to 1-year age groups
## First, 
## Set replicates for each row
duptimes = c(4,3,3,5,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,5,5,5,5,5,5,26)

## Create an index of the rows you want with duplication
idx = rep(1:nrow(df_mig), duptimes)

## Use that index to generate your new data frame
dup_mig = df_mig[idx,]

## Divide migration according to the population in each age group 
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
output5 = dup_mig
  
return (output5)}

