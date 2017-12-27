###############################################################################
###############################################################################
###############################################################################

## nastavuji složku se vstupy -------------------------------------------------

setwd(paste(mother_working_directory, "vstupy", sep = "/"))


## ----------------------------------------------------------------------------

#### pokud není v pracovní složce alespoň jeden ze souborů
#### "properties_2016_processed" či "properties_2017_processed", jsou
#### zpracovaná data uložena pod těmito jmény, aby byla napříště
#### pouze nahrána bez nutného processingu (asynchronní úloha) ----------------

#### pokud jsou oba soubory v pracovní složce přítomny, jsou nahrány včetně
#### datových typů všech jejich proměnných (bez nutného processingu) ----------

if(
    !all(
        c(
            "properties_2016_processed.csv",
            "properties_2017_processed.csv"
        ) %in% dir()
    )
){
    
    for(
        my_dataset_name in paste(
            "properties_",
            c("2016", "2017"),
            sep = ""
        )
    ){
        
        ## ukládám datové typy proměnných v každém souboru --------------------
        
        my_data <- get(my_dataset_name)
        
        my_classes <- NULL
        
        for(i in 1:dim(my_data)[2]){
            
            my_classes <- c(
                
                my_classes,
                class(my_data[, i])
                
            )
            
        }
        
        writeLines(
            
            text = my_classes,
            con = paste(
                my_dataset_name,
                "_column_classes",
                ".txt",
                sep = ""
            )
            
        )
        
        
        ## nyní ukládám vždy celý dataset pomocí rychlého uložení -------------
        
        fwrite(
            
            x = my_data,
            file = paste(
                my_dataset_name,
                "_processed",
                ".csv",
                sep = ""
            ),
            sep = ";"
            
        )
        
        
        ## logovací hlášky ----------------------------------------------------
        
        flush.console()
        
        print(
            paste(
                "Právě uložen dataset '",
                my_dataset_name,
                "_processed",
                ".csv",    
                "'.",
                sep = ""
            )
        )
        
    }
    
    
}else{
    
    ## pokud ale oba soubory se zpracovanými daty již v pracovní složce
    ## existují, konečně je nahtávám pomocí rychlého nahrání (a bez nutného
    ## processingu)
    ## předtím je samozřejmě nutné nahrát soubory s datovými typy
    ## proměnných -------------------------------------------------------------
    
    for(
        my_dataset_name in paste(
            "properties_",
            c("2016", "2017"),
            sep = ""
        )
    ){
        
        ## nejdříve nahrávám soubory s datovými typy proměnných datasetů ------
        
        my_column_classes <- readLines(
            
            con = paste(
                my_dataset_name,
                "_column_classes",
                ".txt",
                sep = ""
            ),
            encoding = "UTF-8"
            
        )
        
        
        ## nyní nahrávám oba datasety pomocí rychlého čtení -------------------
        
        assign(
            my_dataset_name,
            data.frame(
                fread(
                    
                    input = paste(
                        my_dataset_name,
                        "_processed",
                        ".csv",
                        sep = ""
                    ),
                    sep = ";",
                    header = TRUE,
                    check.names = FALSE,
                    encoding = "UTF-8",
                    colClasses = my_column_classes
                    
                )
            )
        )
        
        
        ## logovací hlášky ----------------------------------------------------
        
        flush.console()
        
        print(
            paste(
                "Právě načten dataset '",
                my_dataset_name,
                "_processed",
                ".csv",    
                "'.",
                sep = ""
            )
        )
        
        
    }
    
}


## ----------------------------------------------------------------------------

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





