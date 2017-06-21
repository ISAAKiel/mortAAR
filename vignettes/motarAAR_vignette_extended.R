## ------------------------------------------------------------------------
library(tidyverse)  
muen <- muensingen  
muen %>% plyr::dlply("age", identity)  
muen <- muen %>%  
replace(muen == ">50", "50-70") %>%   
replace(muen == ">60", "60-70") %>%   
replace(muen == "10+/-1", "9-12") %>%   
replace(muen == "10", "10-11") %>%   
replace(muen == "7-8", "7-9") %>%   
replace(muen == "9-10", "9-11")    
muen <- muen %>% tidyr::separate(age, c("from", "to"))  
muen <- muen %>% transform( from = as.numeric(from), to = as.numeric(to) )  
muen_prep <- prep.life.table( muen, grnam = "sex", agebeg = "from", ageend = "to", methode = "Standard", age.range = "excluded" )  
muen_result <- muen_prep %>% life.table()  
muen_result %>% plot(display = c("dx","qx", "lx", "ex", "rel_popx"))  

## ------------------------------------------------------------------------
library(tidyverse)  
mag <- magdalenenberg  
mag %>% plyr::dlply("a", identity)  
mag <- mag %>% replace(mag == "60-x", "60-69")  
mag <- mag %>% tidyr::separate(a, c("from", "to"))  
mag <- mag %>% transform( from = as.numeric(from), to = as.numeric(to) )  
mag_prep <- prep.life.table( mag, dec = "Dx", agebeg = "from", ageend = "to", methode = "Equal5", age.range = "included" )  
mag_result <- mag_prep %>% life.table()  
mag_result %>% plot(display = c("qx", "ex", "rel_popx"))  

## ------------------------------------------------------------------------
comp <- list(mag_prep$Deceased, muen_prep$All)  
names(comp) <- c("Magdalenenberg", "Muensingen")  
comp_result <- comp %>% life.table()  
comp_result %>% plot(display = c("dx","qx", "lx","ex", "rel_popx"))  

