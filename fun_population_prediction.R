## Population prediction model   ##
## Author: @Arto Viinikka, SYKE  ##                                  

## Model uses demographic trend calculations to predict the future population for a given area

## RUN THE PREDICTION MODEL ##
##--------------------------##

## Read function parameters ##
fun_population_prediction = function(area, mun_nro, mun_name, rhr_year, mod_inmig_rate, 
                                     mod_outmig_rate, mod_fert_rate,mod_mort_rate, vaesto_naiset,vaesto_miehet,col){

## FEMALES ##
##---------##
  
## Create dummy variables ##
estimation_pop_f = array(0,dim = c(n_ages,col,n_estimation_years)) 
in_migration_f = matrix(0,nrow = n_ages,ncol = col)
out_migration_f = matrix(0,nrow = n_ages,ncol = col)
net_migration_f = matrix(0,nrow = n_ages,ncol = col)
yearly_in_migration_f = array(0,dim = c(n_ages,col,n_estimation_years))
yearly_out_migration_f = array(0,dim = c(n_ages,col,n_estimation_years))
yearly_net_migration_f = array(0,dim = c(n_ages,col,n_estimation_years))
immigration_f = matrix(0,nrow = n_ages,ncol = col)
tot_immigration_f = array(0,dim = c(n_ages,col,n_estimation_years))
estimation_mort_f = array(0,dim = c(n_ages,col,n_estimation_years))
estimation_birth_f = array(0,dim = c(1,col,n_estimation_years))

## Run the model ##
##---------------##
for (year in 1: n_estimation_years){
  if (year == 1){
    estimation_pop_f[,,1] = (vaesto_naiset)}
      if (year > 1){
  
  ## Age population by one year ##
  ##----------------------------##
  estimation_pop_f[2:99,,year] = estimation_pop_f[1:98,,year-1] 
    estimation_pop_f[100,,year] = colSums(estimation_pop_f[99:100,,year-1])
        
  ## Calculate region based birth ##
  ##------------------------------##
    ## Fetch the fertility rates
    birth_rates = fun_birth(mun_nro, mod_fert_rate)
    
      ## Extract female fertility rates
      birth_rates_f = birth_rates$birth_female
      
        ## Calculate the birth using existing population and fertility rates
        total_birth =  sum((estimation_pop_f[14:50,,year]*birth_rates_f[1:37,,year])) 
        
          ## Adjust the birth using previous year regional birth rates
          row_perc = vaesto_naiset[1:3,]/(sum(vaesto_naiset[1:3,]))
            row_perc = colSums(row_perc)
              estimation_pop_f[1,,year] = row_perc*total_birth
              
                ## Gather the yearly birth information in variable 
                estimation_birth_f[1,,year] = estimation_pop_f[1,,year]
                
                 ## Precaution to exclude negative values
                 estimation_birth_f[estimation_birth_f < 0] = 0 
  
  ## Fetch region based migration ##
  ##------------------------------##
      ## In migration
      in_migration_f = fun_in_migration_female(area, mod_inmig_rate)
      
      ## Out migration
      out_migration_f = fun_out_migration_female(area, mod_outmig_rate) 
            
          ## Gather migration data in a variable
          yearly_in_migration_f[,,year] =  in_migration_f
          yearly_out_migration_f[,,year] = out_migration_f
          yearly_net_migration_f[,,year] = in_migration_f - out_migration_f
          
  ## Fetch region based net-immigration ##
  ##------------------------------------##
  immigration_f = fun_immigration_female(vaesto_naiset, mun_nro, col) 
  
    ## Gather the immigration data in a variable 
    tot_immigration_f[,,year] = immigration_f
          

  ## Calculate the effect of migration to population ##
  ##-------------------------------------------------##
  estimation_pop_f[,,year] = estimation_pop_f[,,year] + in_migration_f - out_migration_f + immigration_f

  ## Calculate mortality ##
  ##---------------------##
  
  ## Fetch mortality rates
  mortality_rates = fun_mortality(mun_nro, mod_mort_rate) 
  
    ## Extract female mortality rates
    mortality_rates_f = mortality_rates$mortality_female 
    
      ## Calculate the effect of mortality to population
      estimation_pop_f[,,year] = estimation_pop_f[,,year]*(mortality_rates_f[,,year]) 
      
        ## Gather the mortality data in a variable 
        estimation_mort_f[,,year] = (estimation_pop_f[,,year])-(estimation_pop_f[,,year]*mortality_rates_f[,,year])
  
  ## Round estimations ##
  ##-------------------##
  estimation_pop_f[,,year] = round(estimation_pop_f[,,year], 0) 
    estimation_pop_f[,,year][estimation_pop_f[,,year] < 0]= 0 # Precaution to exclude negative values. Not really needed!
      }}


## EXTRACT OUTPUT DATA ##
##      Female         ##
##---------------------##

## Population prediction ##
##-----------------------##
## Create output matrix
mun_pop_f_output = matrix(0,nrow=n_ages,ncol=n_estimation_years) 

## Municipality population prediction in 1 year age groups
for (year in 1:n_estimation_years){
  mun_pop_f_output[,year]= rowSums(estimation_pop_f[,,year])}

## Municipality population prediction (total population)
temp_f = colSums(estimation_pop_f)
  mun_tot_pop_f = colSums(temp_f)

## Regional population predictions (total population)
reg_tot_pop_f = colSums(estimation_pop_f)


## In-migration ##
##--------------##
## Round In-migration rates
yearly_in_migration_f = round(yearly_in_migration_f, 0)

## Regional in-migration (total population)
in_migration_f_reg = colSums(yearly_in_migration_f)

## Municipality in-migration in 1 year age groups
in_migration_f_mun = colSums(aperm(yearly_in_migration_f, c(2,1,3)))

## Municipality in-migration (total population)
in_migration_tot_f_mun = colSums(in_migration_f_reg)


## Out-migration ##
##---------------##
## Round Out-migration rates
yearly_out_migration_f = round(yearly_out_migration_f, 0)

## Regional out-migration (total population)
out_migration_f_reg = colSums(yearly_out_migration_f)

## Municipality out-migration in 1 year age groups
out_migration_f_mun = colSums(aperm(yearly_out_migration_f, c(2,1,3)))

## Municipality out-migration (total population)
out_migration_tot_f_mun = colSums(out_migration_f_reg)


## Net-migration ##
##---------------##
## Round Net-migration rates
yearly_net_migration_f = round(yearly_net_migration_f, 0)

## Regional net-migration (total population)
net_migration_f_reg = colSums(yearly_net_migration_f)

## Municipality net-migration in 1 year age groups
net_migration_f_mun = colSums(aperm(yearly_net_migration_f, c(2,1,3)))

## Municipality net-migration (total population)
net_migration_tot_f_mun = colSums(net_migration_f_reg)

  
## Net-immigration ##
##-----------------##
## Round immigration rates
tot_immigration_f = round(tot_immigration_f, 0)

## Regional net-immigration (total population)
net_immigration_f_reg = colSums(tot_immigration_f)

## Municipality net-immigration in 1 year age groups
net_immigration_f_mun = colSums(aperm(tot_immigration_f, c(2,1,3)))

## Municipality net-immigration (total population)
net_immigration_tot_f_mun = colSums(net_immigration_f_reg)


## Mortality ##
##-----------##
## Round mortality rates
estimation_mort_f = round(estimation_mort_f, 0)

## Regional mortality (total population)
mort_f_reg = colSums(estimation_mort_f)

## Municipality mortality in 1 year age groups
mort_f_mun = colSums(aperm(estimation_mort_f, c(2,1,3)))

## Municipality mortality (total population)
tot_mort_f_mun = colSums(mort_f_reg)


## Birth ##
##-------##
## Round birth rates
estimation_birth_f = round(estimation_birth_f, 0)

## Regional birth (total population)
birth_f_reg = colSums(estimation_birth_f)

## Municipality birth (total population)
birth_f_mun = colSums(birth_f_reg)


## MALES ##
##-------##
## Create dummy variables for males
estimation_pop_m = array(0,dim = c(n_ages,col,n_estimation_years)) 
in_migration_m = matrix(0,nrow = n_ages,ncol = col)
out_migration_m = matrix(0,nrow = n_ages,ncol = col)
net_migration_m = matrix(0,nrow = n_ages,ncol = col)
yearly_in_migration_m = array(0,dim = c(n_ages,col,n_estimation_years))
yearly_out_migration_m = array(0,dim = c(n_ages,col,n_estimation_years))
yearly_net_migration_m = array(0,dim = c(n_ages,col,n_estimation_years))
immigration_m = matrix(0,nrow = n_ages,ncol = col)
tot_immigration_m = array(0,dim = c(n_ages,col,n_estimation_years))
estimation_mort_m = array(0,dim = c(n_ages,col,n_estimation_years))
estimation_birth_m = array(0,dim = c(1,col,n_estimation_years))


## Run the model ##
##---------------##
for (year in 1: n_estimation_years){
  if (year == 1){
    estimation_pop_m[,,1] = (vaesto_miehet)}
  if (year > 1){
    
    ## Age population by one year ##
    ##----------------------------##
    estimation_pop_m[2:99,,year] = estimation_pop_m[1:98,,year-1] 
      estimation_pop_m[100,,year] = colSums(estimation_pop_m[99:100,,year-1])
    
    ## Calculate region based birth ##
    ##------------------------------##
    ## Fetch the fertility rates
    birth_rates = fun_birth(mun_nro, mod_fert_rate)
    
      ## Extract male fertility rates
      birth_rates_m = birth_rates$birth_male
    
        ## Calculate the birth using existing population and fertility rates
        total_birth =  sum((estimation_pop_f[14:50,,year]*birth_rates_m[1:37,,year])) 
    
          ## Adjust the birth using previous year regional birth rates
          row_perc = vaesto_miehet[1:3,]/sum(vaesto_miehet[1:3,])
            row_perc = colSums(row_perc)
              estimation_pop_m[1,,year] = row_perc*total_birth
    
            ## Gather the yearly birth information in variable 
            estimation_birth_m[1,,year] = estimation_pop_m[1,,year]
    
              ## Precaution to exclude negative values
              estimation_birth_m[estimation_birth_m < 0] = 0 
    
    ## Fetch region based migration ##
    ##------------------------------##
    ## In migration
    in_migration_m = fun_in_migration_male(area, mod_inmig_rate)
    
      ## Out migration
      out_migration_m = fun_out_migration_male(area, mod_outmig_rate) 
    
          ## Gather migration data in a variable
          yearly_in_migration_m[,,year] =  in_migration_m
          yearly_out_migration_m[,,year] = out_migration_m
          yearly_net_migration_m[,,year] = in_migration_m - out_migration_m
    
    ## Fetch region based net-immigration ##
    ##------------------------------------##
    immigration_m = fun_immigration_male(vaesto_miehet, mun_nro, col) 
    
      ## Gather the immigration data in a variable 
      tot_immigration_m[,,year] = immigration_m
    
    ## Calculate the effect of migration to population ##
    ##-------------------------------------------------##
    estimation_pop_m[,,year] = estimation_pop_m[,,year] + in_migration_m - out_migration_m + immigration_m
    
    ## Calculate mortality ##
    ##---------------------##
    
    ## Fetch mortality rates
    mortality_rates = fun_mortality(mun_nro, mod_mort_rate) 
    
      ## Extract female mortality rates
      mortality_rates_m = mortality_rates$mortality_male 
    
        ## Calculate the effect of mortality to population
        estimation_pop_m[,,year] = estimation_pop_m[,,year]*(mortality_rates_m[,,year]) 
    
          ## Gather the mortality data in a variable 
          estimation_mort_m[,,year] = (estimation_pop_m[,,year])-(estimation_pop_m[,,year]*mortality_rates_m[,,year])
    
    ## Round estimations ##
    ##-------------------##
    estimation_pop_m[,,year] = round(estimation_pop_m[,,year], 0) 
      estimation_pop_m[,,year][estimation_pop_m[,,year] < 0]= 0 # Precaution to exclude negative values. Not really needed!
  }}


## EXTRACT OUTPUT DATA  ##
##      MALES           ##
##----------------------##


## Population prediction ##
##-----------------------##
## Create output matrix
mun_pop_m_output = matrix(0,nrow=n_ages,ncol=n_estimation_years) 

## Municipality population prediction in 1 year age groups
for (year in 1:n_estimation_years){
  mun_pop_m_output[,year] = rowSums(estimation_pop_m[,,year])}

## Municipality population prediction (total population)
temp_m = colSums(estimation_pop_m)
  aggregate_tot_m = colSums(temp_m)

## Regional population prediction (total population)
reg_tot_pop_m = colSums(estimation_pop_m)


## In-migration ##
##--------------##
## Round In-migration rates
yearly_in_migration_m = round(yearly_in_migration_m, 0)

## Regional in-migration (total population)
in_migration_m_reg = colSums(yearly_in_migration_m)

## Municipality in-migration in 1 year age groups
in_migration_m_mun = colSums(aperm(yearly_in_migration_m, c(2,1,3)))

## Municipality in-migration (total population)
in_migration_tot_m_mun = colSums(in_migration_m_reg)


## Out-migration ##
##---------------##
## Round Out-migration rates
yearly_out_migration_m = round(yearly_out_migration_m, 0)

## Regional Outmigration (total population)
out_migration_m_reg = colSums(yearly_out_migration_m)

## Municipality Outmigration in 1 year age groups
out_migration_m_mun = colSums(aperm(yearly_out_migration_m, c(2,1,3)))

## Municipality Outmigration (total population)
out_migration_tot_m_mun = colSums(out_migration_m_reg)


## Net-migration ##
##---------------##
## Round Netmigration rates
yearly_net_migration_m = round(yearly_net_migration_m, 0)

## Regional netmigration (total population)
net_migration_m_reg = colSums(yearly_net_migration_m)

## Municipality netmigration in 1 year age groups
net_migration_m_mun = colSums(aperm(yearly_net_migration_m, c(2,1,3)))

## Municipality netmigration (total population)
net_migration_tot_m_mun = colSums(net_migration_m_reg)


## Net-immigration ##
##-----------------##
## Round Net-immigration rates
tot_immigration_m = round(tot_immigration_m, 0)

## Regional netimmigration (total population)
tot_immigration_m_reg = colSums(tot_immigration_m)

## Municipality netimmigration in 1 year age groups
net_immigration_m_mun = colSums(aperm(tot_immigration_m, c(2,1,3)))

## Municipality netimmigration (total population)
net_immigration_tot_m_mun = colSums(tot_immigration_m_reg)


## Mortality ##
##-----------##
## Round Mortality rates
estimation_mort_m = round(estimation_mort_m, 0)

## Regional mortality (total population)
tot_mort_m_reg = colSums(estimation_mort_m)

## Municipality mortality in 1 year age groups
mort_m_mun = colSums(aperm(estimation_mort_m, c(2,1,3)))

## Municipality mortality (total population)
tot_mort_m_mun = colSums(tot_mort_m_reg)


## Birth ##
##-------##
## Round Birth rates
estimation_birth_m = round(estimation_birth_m, 0)

## Regional birth (total population)
birth_m_reg = colSums(estimation_birth_m)

## Municipality birth (total population)
birth_m_mun = colSums(birth_m_reg)


## Write predictions out as a list ##
##---------------------------------##
output<-list("m ikavuosittain alue" = estimation_pop_m, "m ikavuosittain kunta" = mun_pop_m_output,"m yhteensa alue" = reg_tot_pop_m,
             "m yhteensa kunta"= aggregate_tot_m, "m tulomuutto alue" = yearly_in_migration_m,"m tulomuutto kunta" = in_migration_m_mun, "m tulomuutto alue yht" = in_migration_m_reg,
             "m tulomuutto kunta yht" = in_migration_tot_m_mun,"m lahtomuutto alue" = yearly_out_migration_m, "m lahtomuutto kunta" = out_migration_m_mun, "m lahtomuutto alue yht" =
               out_migration_m_reg, "m lahtomuutto kunta yht" = out_migration_tot_m_mun, "m nettomuutto alue" = yearly_net_migration_m, "m nettomuutto kunta" = net_migration_m_mun, 
             "m nettomuutto alue yht" = net_migration_m_reg, "m nettomuutto kunta yht" = net_migration_tot_m_mun, "m nettomaahanmuutto alue" = tot_immigration_m, "m nettomaahanmuutto kunta"
             = net_immigration_m_mun, "m nettomaahanmuutto alue yht" = tot_immigration_m_reg, "m nettomaahanmuutto kunta yht" = net_immigration_tot_m_mun, "m kuolleisuus alue" = estimation_mort_m,
             "m kuolleisuus kunta" = mort_m_mun, "m kuolleisuus alue yht" = tot_mort_m_reg, "m kuolleisuus kunta yht" = tot_mort_m_mun, "m syntyvyys alue" = estimation_birth_m,
             "m syntyvyys kunta" = birth_m_mun, "m syntyvyys alue yht" = birth_m_reg, "m syntyvyys kunta yht" = birth_m_mun,

             "n ikavuosittain alue" = estimation_pop_f, "n ikavuosittain kunta" = mun_pop_f_output, "n yhteensa alue" = reg_tot_pop_f,
             "n yhteensa kunta" = mun_tot_pop_f, "n tulomuutto alue" = yearly_in_migration_f, "n tulomuutto kunta" = in_migration_f_mun, "n tulomuutto alue yht" = in_migration_f_reg,
             "n tulomuutto kunta yht" = in_migration_tot_f_mun,"n lahtomuutto alue" = yearly_out_migration_f, "n lahtomuutto kunta" = out_migration_f_mun, "n lahtomuutto alue yht" =
               out_migration_f_reg, "n lahtomuutto kunta yht" = out_migration_tot_f_mun, "n nettomuutto alue" = yearly_net_migration_f, "n nettomuutto kunta" = net_migration_f_mun, 
             "n nettomuutto alue yht" = net_migration_f_reg, "n nettomuutto kunta yht" = net_migration_tot_f_mun, "n nettomaahanmuutto alue" = immigration_f, "n nettomaahanmuutto kunta"
             = net_immigration_f_mun, "n nettomaahanmuutto alue yht" = net_immigration_f_reg, "n nettomaahanmuutto kunta yht" = net_immigration_tot_f_mun, "n kuolleisuus alue" = estimation_mort_f,
             "n kuolleisuus kunta" = mort_f_mun, "n kuolleisuus alue yht" = mort_f_reg, "n kuolleisuus kunta yht" = tot_mort_f_mun, "n syntyvyys alue" = estimation_birth_f,
             "n syntyvyys kunta" = birth_f_mun, "n syntyvyys alue yht" = birth_f_reg, "n syntyvyys kunta yht" = birth_f_mun)

return(output)}



