## Birth rates ##              
##-------------##

## Function returns birth rates from Statistics Finland interface 

## Read function parameters
fun_birth=function(mun_nro,mod_fert_rate){

## Fertility rates ##
## Create PXWEB query 
pxweb_query_list = 
    list("Kunta"=c(paste0("KU",mun_nro)),
         "Vuosi" = c ('*'),
         "Ikä" = c('*'),
         "Tiedot" = c("hedkerroin_e21"))
  
## Read fertility rates from stat.fi
px_data = 
    pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/vaenn/statfin_vaenn_pxt_139j.px",
              query = pxweb_query_list)
  
## Convert to data.frame 
fert_rates = as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")   

## Drop extra columns 
drops = c("Kunta","Vuosi") # Drop extra columns
fert_rates= fert_rates[ , !(names(fert_rates) %in% drops)] 

## Give better column names
colnames(fert_rates) = c("Ikä","Kerroin")
  
## Change correct unit (permille)
fert_rates$Kerroin=fert_rates$Kerroin/1000

## Modify fertility rates if necessary
fert_rates$Kerroin=fert_rates$Kerroin* mod_fert_rate


## Sex rates ###
## Create PXWEB query 
pxweb_query_list = 
  list("Alue"=c(paste0("KU",mun_nro)),
       "Äidin ikä" = c("SSS"),
       "Vuosi" = as.character(pred_years),
       "Sukupuoli" = c("1","2"),
       "Tiedot" = c("vm01"))

## Read infant sex rates 
px_data = 
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/synt/statfin_synt_pxt_12dq.px",
            query = pxweb_query_list)


## Convert to data.frame 
sex_rates = as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")   

## Drop extra columns 
drops = c("Alue","äidin ikä") # Drop extra columns
sex_rates= sex_rates[ , !(names(sex_rates) %in% drops)] 

## Calculate total amount of birth
total_birth=sum(sex_rates$`Elävänä syntyneet`)

## Calculate the percentage of male birth
male_birth=subset(sex_rates, sex_rates$Sukupuoli=='Miehet')
male_birth=(sum(male_birth$`Elävänä syntyneet`))/total_birth
male_birth = 0.5
  
  ## Calculate the percentage of female birth
female_birth=subset(sex_rates, sex_rates$Sukupuoli=='Naiset')
female_birth=(sum(female_birth$`Elävänä syntyneet`))/total_birth
female_birth = 0.5

## Mortality rates ###
## Read male mortality rates
pxweb_query_list = 
  list("Kunta"=c(paste0("KU",mun_nro)),
       "Vuosi" = c ('*'),
       "Sukupuoli" = c('1'),
       "Ikä" = c('alle0'),
       "Tiedot" = c("kuolkerroin_e21"))

## Read male mortality rates, permille, from stat.fi
px_data = 
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/vaenn/statfin_vaenn_pxt_139k.px",
            query = pxweb_query_list)

## Convert to data.frame 
infant_mortality_male = as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")  

## Drop extra columns
drops = c("Kunta", "Ikä","Sukupuoli") # Drop extra columns
infant_mortality_male= infant_mortality_male[ , !(names(infant_mortality_male) %in% drops)] 

## Give better column names
colnames(infant_mortality_male) = c("Vuosi", "Kerroin")

## Change [Kerroin] type to numeric
infant_mortality_male$Kerroin=as.numeric(infant_mortality_male$Kerroin)

## Change correct unit (permille)
infant_mortality_male$Kerroin=(1-infant_mortality_male$Kerroin/1e3) 

## Read female mortality rates ##
pxweb_query_list = 
  list("Kunta"=c(paste0("KU",mun_nro)),
       "Vuosi" = c ('*'),
       "Sukupuoli" = c('2'),
       "Ikä" = c('alle0'),
       "Tiedot" = c("kuolkerroin_e21"))

## Read female mortality rates, permille, from stat.fi
px_data = 
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/vaenn/statfin_vaenn_pxt_139k.px",
            query = pxweb_query_list)

## Convert to data.frame 
infant_mortality_female = as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")  

## Drop extra columns
drops = c("Kunta", "Ikä","Sukupuoli") # Drop extra columns
infant_mortality_female= infant_mortality_female[ , !(names(infant_mortality_female) %in% drops)]

## Give better column names
colnames(infant_mortality_female) = c("Vuosi", "Kerroin")

## Change [Kerroin] type to numeric
infant_mortality_female$Kerroin=as.numeric(infant_mortality_female$Kerroin)

## Change correct unit (permille)
infant_mortality_female$Kerroin=(1-infant_mortality_female$Kerroin/1e3) 

## Create empty output matrix ##
n_ages=101
birth=array(0,dim=c(n_ages,6,n_estimation_years))
colnames(birth) = c("age","fert_rate","sex_f","sex_m","mort_f", "mort_m")
birth[,1,]=c(1:101)# fix first column for [age]

## Attach created variables to emtpy matrix
birth[14:50,2,]=fert_rates$Kerroin #fert_rate
birth[14:50,3,]=female_birth #female sex rate
birth[14:50,4,]=male_birth #male sex rate

## Infant mortality
## Female
foreach(year=1:n_estimation_years)%do%{ # vuosi
  value=as.numeric(infant_mortality_female[year,2])
  birth[14:50,5,year]=value}

## Male
foreach(year=1:n_estimation_years)%do%{ # vuosi
  value=as.numeric(infant_mortality_male[year,2])
  birth[14:50,6,year]=value}

## create area specific birthrate
## Female
birth_female=array(0,dim=c(37,col,n_estimation_years))
dummy_birth_f=(birth[14:50,2,])*(birth[14:50,3,])*(birth[14:50,5,])
birth_female[1:37,,]=dummy_birth_f 

## Male
birth_male=array(0,dim=c(37,col,n_estimation_years))
dummy_birth_m=(birth[14:50,2,])*(birth[14:50,4,])*(birth[14:50,6,])
birth_male[1:37,,]=dummy_birth_m 

output9=list("birth_female"=birth_female,"birth_male"=birth_male)
return(output9)}

