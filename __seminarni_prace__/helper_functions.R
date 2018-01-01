###############################################################################
###############################################################################
###############################################################################

## definuji pomocné funkce ----------------------------------------------------

isPotentiallyNumeric <- function(
    
    x,
    original.na.mark = ""
    
){
    
    # '''
    # Vrací TRUE tehdy a jen tehdy, pokud po koerci vektoru textových
    # hodnot "x" na datový typ "numeric" nevzniknou nové chybějící
    # hodnoty (NA).
    # Argument "original.na.mark" vyjadřuje, jak jsou značeny
    # chybějící hodnoty v původním vektoru "x".
    # '''
    
    return(
        !any(
            is.na(
                suppressWarnings(
                    as.numeric(
                        x[
                            x != original.na.mark
                        ]
                    )
                )
            )
        )
    )
    
}


## ----------------------------------------------------------------------------

getMyAccuracy <- function(
    
    my_table

){
    
    # '''
    # Vrací přesnost pro konfuzní matici "my_table".
    # '''
    
    return(
        sum(diag(my_table)) / sum(my_table)
    )
    
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





