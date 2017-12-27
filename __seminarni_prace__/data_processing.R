###############################################################################
###############################################################################
###############################################################################

## zpracovávám data -----------------------------------------------------------

## nastavuji složku se vstupy -------------------------------------------------

setwd(paste(mother_working_directory, "vstupy", sep = "/"))


## ----------------------------------------------------------------------------

#### následující procedura proběhne pouze na nativních datech, tj.
#### bez přípony "_processed" -------------------------------------------------

if(
    !all(
        c(
            "properties_2016_processed.csv",
            "properties_2017_processed.csv"
        ) %in% dir()
    )
){
    
    #### nejdříve určuji datové typy v datasetech "properties_2016"
    #### a "properties_2017"; jde-li o kategorickou proměnnou, kóduju ji
    #### ve smyslu self-labelingu podle codebooku "zillow_data_dictionary" ----
    
    if(
        all(colnames(properties_2016) == colnames(properties_2017))
    ){
        
        ## pro každý z datasetů "properties_2016" a "properties_2017" měním
        ## datové typy na nejvhodnější možné ----------------------------------
        
        for(
            my_dataset_name in paste(
                "properties_",
                c("2016", "2017"),
                sep = ""
            )
        ){
            
            my_data <- get(my_dataset_name)
            
            
            ## pro každou proměnné v datasetu se ptám, zda je dle codebooku
            ## "zillow_data_dictionary" dozajista kategorická -----------------
            
            for(my_variable in colnames(my_data)){
                
                if(
                    my_variable %in% tolower(ls()[grepl("ID$", ls())])
                ){
                    
                    ## pokud kategorická určitě je, překódovávám její
                    ## kategorie podle codebooku ve smyslu self-labelingu -----
                    
                    my_codebook <- get(
                        ls()[grepl("ID$", ls())][
                            tolower(
                                ls()[grepl("ID$", ls())]
                            ) == my_variable
                        ]
                    )
                    
                    for(my_level in my_codebook[, 1]){
                        
                        my_data[
                            which(my_data[, my_variable] == my_level),
                            my_variable
                        ] <- my_codebook[
                            my_codebook[, 1] == my_level,
                            2
                        ]
                        
                    }
                    
                    
                    ## nakonec ji ukládám jako datový typ faktor --------------
                    
                    my_data[, my_variable] <- as.factor(
                        as.character(
                            my_data[, my_variable]
                        )
                    )
                    
                }else{
                    
                    ## pokud není podle codebooku apriorně kategorická,
                    ## testuji, zda nemůže být určitě numerická,
                    ## v případě falešné pozitivivy, tedy absence nově
                    ## vzniklých chybějících hodnot po koerci
                    ## na reálná čísla, by taková proměnná jistě byla
                    ## v codebooku -- to už je ale nyní vyloučeno, je tedy
                    ## numerická ----------------------------------------------
                    
                    if(
                        isPotentiallyNumeric(
                            my_data[, my_variable]
                        ) &
                        !(
                            grepl("id[0-9]*$", my_variable)
                        ) &
                        !(
                            grepl(
                                "(block|city|county|hood|zip)$",
                                my_variable
                            )
                        )
                    ){
                        
                        ## proměnnou ukládám jako "numeric" -------------------
                        
                        my_data[, my_variable] <- as.numeric(
                            as.character(
                                my_data[, my_variable]
                            )
                        )
                        
                    }else{
                        
                        ## ve zbývajícím případě ji musím ošetřit jako
                        ## kategorickou proměnnou, ale bez self-labeling
                        ## kódování kategorií ---------------------------------
                        
                        my_data[, my_variable] <- as.factor(
                            as.character(
                                my_data[, my_variable]
                            )
                        )
                        
                    }
                    
                }
                
                
                ## logovací hlášky --------------------------------------------
                
                flush.console()
                
                print(
                    paste(
                        my_dataset_name,
                        ": proměnná '",
                        my_variable,
                        "' přetypována na '",
                        class(my_data[, my_variable]),
                        "'.",
                        sep = ""
                    )
                )
                
            }
            
            
            ## ukládám nově upravený dataset zpátky pod jméno původního
            ## datasetu -------------------------------------------------------
            
            assign(
                my_dataset_name,
                my_data
            )
            
            
        }
        
    }
    
    
    #### manuálně opravuji nesprávnou automatickou koerci ---------------------
    
    for(
        my_dataset_name in paste("properties_", c("2016", "2017"), sep = "")
    ){
        
        my_data <- get(my_dataset_name)
        
        
        ## následující proměnné jsou kategorické ------------------------------
        
        for(my_variable in c(
            
            "fips"
            
        )){
            
            my_data[, my_variable] <- as.factor(
                as.character(
                    my_data[, my_variable]
                )
            )
            
            
            ## logovací hlášky ------------------------------------------------
            
            flush.console()
            
            print(
                paste(
                    my_dataset_name,
                    ": proměnná '",
                    my_variable,
                    "' přetypována na '",
                    class(my_data[, my_variable]),
                    "'.",
                    sep = ""
                )
            )
            
        }
        
        
        ## ukládám nově upravený dataset zpátky pod jméno původního
        ## datasetu -----------------------------------------------------------
        
        assign(
            my_dataset_name,
            my_data
        )
        
    }
    
}


## ----------------------------------------------------------------------------

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





