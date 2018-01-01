###############################################################################
###############################################################################
###############################################################################

## vytvářím hierarchický model ------------------------------------------------

my_lmer <- lmer(
    
    formula = paste(
        
        "logerror",
        " ~ ",
        paste(
            c(
                numeric_regressors_of_interest,
                factor_regressors_of_interest
            ),
            collapse = " + "
        ),
        " + (1 | parcelid)",
        sep = ""
        
    ),
    data = train_set
    
)


summary(my_lmer)


## tisknu výstup --------------------------------------------------------------

my_table <- cbind(
    
    summary(my_lmer)$coefficients,
    "p-value" = (
        1 - unlist(
            lapply(
                abs(summary(my_lmer)$coefficients[, "t value"]),
                pnorm
            )
        )
    ) * 2
    
)


print(
    xtable(
        my_table,
        align = rep("", ncol(my_table) + 1),
        digits = 3
    ),
    floating = FALSE,
    tabular.environment = "tabular",
    hline.after = NULL,
    include.rownames = TRUE,
    include.colnames = TRUE
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





