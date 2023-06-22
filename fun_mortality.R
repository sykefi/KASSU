## Mortality rates ##            
##-----------------##

## Function returns mortality rates from Statistics Finland interface 

fun_mortality = function(mun_nro, mod_mort_rate){


## PXWEB query 
  pxweb_query_list  =  
    list("Kunta"=c(paste0("KU",mun_nro)),
         "Vuosi" = c ('*'),
         "Sukupuoli" = c('1'),
         "Ikä" = c('*'),
         "Tiedot" = c("kuolkerroin_e21"))
  
## Read male mortality rates, permille, from stat.fi
px_data  =  
    pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/vaenn/statfin_vaenn_pxt_139k.px",
              query = pxweb_query_list)

## Convert to data.frame 
mortality_male  =  as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")  
  
## Drop extra rows
mortality_male = subset(mortality_male, mortality_male$Ikä != "Vuoden aikana syntyvät") 
  
## Drop extra columns
drops  =  c("Kunta","Sukupuoli") # Drop extra columns
mortality_male =  mortality_male[ , !(names(mortality_male) %in% drops)] 

## Give better column names
colnames(mortality_male)  =  c("Vuosi", "Ikä", "Kerroin")

## Modify mortality rates
mortality_male$Kerroin =  mortality_male$Kerroin * mod_mort_rate

## Change correct unit (permille)
mortality_male$Kerroin = (1-mortality_male$Kerroin/1e3) 

## Convert Column [Ikä] as character. Required when inserting new rows with Ikä = 100
mortality_male$Ikä  =  as.character(mortality_male$Ikä)

# Add mortality rate to 100 year old by duplicating 99 year old rate
mortality_male = rbind(mortality_male,
      mortality_male%>% 
        filter(Ikä== 99)%>% 
        mutate(Ikä = 100,
               Vuosi = Vuosi, Kerroin=Kerroin))

## Duplicate Column [Kerroin] according to number of areas
mortality_male = cbind(mortality_male, replicate(col,mortality_male$Kerroin))

## Create a list of data.frames separated by "Vuosi" column.
mortality_male = split(mortality_male, mortality_male$Vuosi)

## Drop extra columns
mortality_male  =  lapply(mortality_male, function(x) x[!(names(x) %in% c("Kerroin", "Ikä", "Vuosi"))])


## Create emtpy mortality matrix
mortality_m = array(0,dim=c(n_ages,col,n_estimation_years))

## Loop rates into matrix
foreach(year=1:n_estimation_years)%do%{ # valitaan alue
 value = as.matrix(mortality_male[[year]])
 mortality_m[,,year] = value[,]}

## Female ##

## PXWEB query 
pxweb_query_list  =  
  list("Kunta"=c(paste0("KU",mun_nro)),
       "Vuosi" = c ('*'),
       "Sukupuoli" = c('2'),
       "Ikä" = c('*'),
       "Tiedot" = c("kuolkerroin_e21"))

## Read male mortality rates, permille, from stat.fi
px_data  =  
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/vaenn/statfin_vaenn_pxt_139k.px",
            query = pxweb_query_list)

## Convert to data.frame 
mortality_female  =  as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")  

## Drop extra rows
mortality_female = subset(mortality_female, mortality_female$Ikä != "Vuoden aikana syntyvät") 

## Drop extra columns
drops  =  c("Kunta","Sukupuoli") # Drop extra columns
mortality_female =  mortality_female[ , !(names(mortality_female) %in% drops)] 

## Give better column names
colnames(mortality_female)  =  c("Vuosi", "Ikä", "Kerroin")

## Change correct unit (permille)
mortality_female$Kerroin = (1-mortality_female$Kerroin/1e3) 

## Convert Column [Ikä] as character. Required when inserting new rows with Ikä = 100
mortality_female$Ikä  =  as.character(mortality_female$Ikä)

# Add mortality rate to 100 year old by duplicating 99 year old rate
mortality_female = rbind(mortality_female,
                        mortality_female%>% 
                        filter(Ikä == 99)%>% 
                        mutate(Ikä = 100,
                               Vuosi = Vuosi, Kerroin=Kerroin))

## Duplicate Column [Kerroin] according to number of areas
mortality_female = cbind(mortality_female, replicate(col,mortality_female$Kerroin))

## Create a list of data.frames separated by "Vuosi" column.
mortality_female = split(mortality_female, mortality_female$Vuosi)

## Drop extra columns
mortality_female  =  lapply(mortality_female, function(x) x[!(names(x) %in% c("Kerroin", "Ikä", "Vuosi"))])


## Create emtpy mortality matrix
mortality_f = array(0,dim=c(n_ages,col,n_estimation_years))

## Loop rates into matrix
foreach(year=1:n_estimation_years)%do%{ # valitaan alue
  value = as.matrix(mortality_female[[year]])
  mortality_f[,,year] = value[,]}

output8 = list("mortality_female" = mortality_f,"mortality_male" = mortality_m)

return(output8)}




