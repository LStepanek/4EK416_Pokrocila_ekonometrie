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

new_lmer <- step(my_lmer)


## sumář modelu ---------------------------------------------------------------

my_summary <- summary(my_lmer)


## ----------------------------------------------------------------------------

###############################################################################

## tisknu výstup --------------------------------------------------------------

my_table <- cbind(
    
    my_summary$coefficients#,
    #"p-value" = (
    #    1 - unlist(
    #        lapply(
    #            abs(my_summary$coefficients[, "t value"]),
    #            pnorm
    #        )
    #    )
    #) * 2
    
)


print(
    xtable(
        my_table,
        align = rep("", ncol(my_table) + 1),
        digits = c(0, 3, 3, 0, 3, 3)
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

my_train_prediction <- predict(
    
    object = my_lmer,#new_lmer$model,
    newdata = train_set,
    allow.new.levels = TRUE,
    type = "response",
    re.form = NULL
    
)


test_set[
    test_set[, "airconditioningtypeid"] == "Evaporative Cooler",
    "airconditioningtypeid"
] <- NA

my_test_prediction <- predict(
    
    object = my_lmer,#new_lmer$model,
    newdata = test_set,
    allow.new.levels = TRUE,
    type = "response",
    re.form = NULL
    
)


## střední absolutní chyba predikce (MAE) -------------------------------------

mean(abs(my_train_prediction - train_set[, "logerror"]), na.rm = TRUE)
mean(abs(my_test_prediction - test_set[, "logerror"]), na.rm = TRUE)


## ----------------------------------------------------------------------------

###############################################################################

## diagnostika modelu ---------------------------------------------------------

#### diagram reziduí proti vyrovnaným hodnotám --------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

setEPS()
postscript(
    file = "residua_vs_vyrovnane_hodnoty.eps",
    width = 6,
    height = 5,
    pointsize = 12
)

par(mar = c(4.1, 4.25, 0.1, 0.1))

plot(
    resid(my_lmer) ~ fitted(my_lmer),
    xlab = expression(
        paste("vyrovnané hodnoty (", hat(bolditalic(y)), ")", sep = "")
    ),
    ylab = expression(
        paste("realizace reziduí (", hat(bolditalic(e)), ")", sep = "")
    ),
    col = "blue"
)

dev.off()

setwd(mother_working_directory)


#### histogram reziduí --------------------------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

setEPS()
postscript(
    file = "histogram_rezidui.eps",
    width = 6,
    height = 5,
    pointsize = 12
)

par(mar = c(4.1, 4.1, 2.1, 0.1))

hist(
    resid(my_lmer),
    xlab = expression(
        paste("realizace reziduí (", hat(bolditalic(e)), ")", sep = "")
    ),
    ylab = "absolutní frekvence",
    main = "",
    col = "lightgrey"
)

dev.off()

setwd(mother_working_directory)


#### kvantil-kvantil diagram reziduí ------------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

setEPS()
postscript(
    file = "qq_plot_rezidui.eps",
    width = 6,
    height = 6,
    pointsize = 12
)

par(mar = c(4.1, 4.1, 0.1, 0.1))

qqnorm(
    resid(my_lmer),
    xlab = "teoretické kvantily",
    ylab = "výběrové kvantily",
    main = ""
)
abline(
    a = 0,
    b = 1,
    lty = 2,
    col = "blue",
    cex = 0.7
)

dev.off()

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





