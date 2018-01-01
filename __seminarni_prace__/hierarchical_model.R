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
        #" + (1 | transaction_year)",
        #" + (1 | transaction_month)",
        sep = ""
        
    ),
    data = train_set
    
)


## vylepšuji model minimalizací AIC kritéria ----------------------------------

#new_lmer <- step(my_lmer)


## sumář modelu ---------------------------------------------------------------

#coef(summary(my_lmer))
#summary(my_lmer)


## ----------------------------------------------------------------------------

###############################################################################

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

## predikce -------------------------------------------------------------------

test_set[
    test_set[, "airconditioningtypeid"] == "Evaporative Cooler",
    "airconditioningtypeid"
] <- NA

my_prediction <- predict(
    
    object = new_lmer$model,
    newdata = test_set,
    allow.new.levels = TRUE,
    type = "response",
    re.form = NULL
    
)


## střední absolutní chyba predikce (MAE) -------------------------------------

mean(abs(my_prediction - test_set[, "logerror"]), na.rm = TRUE)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





