###############################################################################
###############################################################################
###############################################################################

## merguji vždy dataset "properties_2016" s "train_2016_v2" a dále
## "properties_2017" s "train_2017" -------------------------------------------

for(my_year in c("2016", "2017")){
    
    assign(
        
        paste(
            "merged_data_",
            my_year,
            sep = ""
        ),
        merge(
            x = get(
                paste(
                    "train_",
                    my_year,
                    if(my_year == "2016"){"_v2"},
                    sep = ""
                )
            ),
            y = get(
                paste(
                    "properties_",
                    my_year,
                    sep = ""
                )
            ),
            by = "parcelid",
            all.x = TRUE    # left outer join,
                            # tj. budou zachována všechna "parcelid" z "x",
                            # ale ne nutně všechna "parcelid" z "y"
        )
        
    )
    
    ## logovací hlášky --------------------------------------------------------
    
    flush.console()
    
    print(
        paste(
            "Právě joinovány datasety '",
            paste(
                "train_",
                my_year,
                if(my_year == "2016"){"_v2"},
                sep = ""
            ),
            "' a '",
            paste(
                "properties_",
                my_year,
                sep = ""
            ),
            "'.",
            sep = ""
        )
    )
    
}


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím jeden velký dataset trénovacích dat -------------------------------

train_data <- rbind(
    
    merged_data_2016,
    merged_data_2017
    
)


## ----------------------------------------------------------------------------

###############################################################################

## přidávám ještě proměnnou "transaction_year" a "transaction_month"
## pro každé pozorování datasetu "train_data" ---------------------------------

train_data <- data.frame(
    
    train_data[
        ,
        1:(which(colnames(train_data) == "transactiondate") - 1)
    ],
    "transaction_year" = format(
        train_data[
            ,
            which(colnames(train_data) == "transactiondate")
        ],
        format = "%Y"
    ),
    "transaction_month" = factor(
        format(
            train_data[
                ,
                which(colnames(train_data) == "transactiondate")
            ],
            format = "%b"
        ),
        levels = as.character(c(1:12))
    ),
    train_data[
        ,
        (which(colnames(train_data) == "transactiondate") + 1):
        dim(train_data)[2]
    ]
    
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





