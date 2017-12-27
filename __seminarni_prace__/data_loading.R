###############################################################################
###############################################################################
###############################################################################

## nastavuji složku se vstupy -------------------------------------------------

setwd(paste(mother_working_directory, "vstupy", sep = "/"))


## loaduji data ---------------------------------------------------------------

#### nejdříve nahrávám všechny .csv, pak .xslx soubory ------------------------

for(my_suffix in c(".csv", ".xlsx")){
    
    for(
        my_filename in dir()[
            grepl(
                paste("\\", my_suffix, "$", sep = ""),
                dir()
            )
        ]
    ){
        
        if(
            my_suffix == ".csv"
        ){
            
            ## nahrávám všechny .csv soubory ----------------------------------
            
            assign(
                
                if(
                    !paste(
                        gsub(
                            paste("\\", my_suffix, "$", sep = ""),
                            "",
                            my_filename
                        ),
                        "_processed",
                        my_suffix,
                        sep = ""
                    ) %in% dir()
                ){
                    gsub(
                        paste("\\", my_suffix, "$", sep = ""),
                        "",
                        my_filename
                    )
                },
                read.table(
                    
                    file = if(
                        !paste(
                            gsub(
                                paste("\\", my_suffix, "$", sep = ""),
                                "",
                                my_filename
                            ),
                            "_processed",
                            my_suffix,
                            sep = ""
                        ) %in% dir()
                    ){
                        my_filename
                    },
                    header = TRUE,
                    sep = ",",
                    dec = ".",
                    row.names = NULL,
                    comment.char = "",
                    check.names = FALSE,
                    colClasses = "character"
                    
                )
                
            )
            
        }else{
            
            ## nahrávám všechny listy všech .xlsx souborů; předpokládám,
            ## že jména všech listů jsou navzájem různá -----------------------
            
            for(
                my_sheetname in getSheetNames(my_filename)
            ){
                
                my_data <- read.xlsx(
                    
                    xlsxFile = my_filename,
                    sheet = my_sheetname,
                    colNames = TRUE,
                    rowNames = FALSE,
                    check.names = FALSE
                    
                )
                
                for(i in 1:dim(my_data)[2]){
                    
                    my_data[, i] <- as.character(my_data[, i])
                    
                }
                
                assign(
                    
                    my_sheetname,
                    my_data
                    
                )
                
            }
            
        }
        
        
        ## logovací hlášky ----------------------------------------------------
        
        flush.console()
        
        print(
            paste(
                "Právě nahrán ",
                if(
                    grepl("\\.csv$", my_filename)
                ){
                    "soubor"
                }else{
                    "list"
                },
                " '",
                my_filename,
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





