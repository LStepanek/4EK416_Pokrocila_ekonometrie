###############################################################################
###############################################################################
###############################################################################

## nutné spouštět v R 64-bit !!! ----------------------------------------------

###############################################################################

## nastavuji pracovní složku --------------------------------------------------

while(!grepl("__seminarni_prace__$", getwd())){
    
    setwd(choose.dir())
    
}

mother_working_directory <- getwd()


## ----------------------------------------------------------------------------

###############################################################################

## spouštím sekvenci skriptů --------------------------------------------------

for(my_script in c(
    
    "initialization",              # spouštím inicializaci
    "helper_functions",            # definuji pomocné funkce    
    "data_loading",                # loaduju data
    "data_processing",             # upravuji data, vytvářím některé výstupy
    "data_saving_and_reloading",   # ukládám zpracovaná data
    "data_postprocessing",         # upravuji data do finální podoby
    "exploratory_data_analysis",   # průzkumová analýza dat
    "hierarchical_model"           # vytvářím hierarchický model
    
)){
    
    setwd(mother_working_directory)
    
    source(
        paste(my_script, ".R", sep = ""),
        echo = TRUE,
        encoding = "UTF-8"
    )
    
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





