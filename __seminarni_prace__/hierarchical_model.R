###############################################################################
###############################################################################
###############################################################################

## nejdříve rozděluji data na trénovací a testovací množinu -------------------

#### do trénovací množiny zahrnu 70 % dat -------------------------------------

train_set_portion <- 0.7

set.seed(2017)


#### vytvářím množinu indexů pozorování, která budou zahrnuta do trénovací
#### množiny ------------------------------------------------------------------

train_set_indices <- sample(
    
    c(1:dim(train_data)[1]),
    floor(dim(train_data)[1] * train_set_portion),
    replace = FALSE
    
)


#### vytvářím trénovací a testovací množinu -----------------------------------

train_set <- train_data[
    train_set_indices
    ,
]

test_set <- train_data[
    setdiff(
        c(
            1:dim(train_data)[1]
        ),
        train_set_indices
    )
    ,
]


## ----------------------------------------------------------------------------

###############################################################################

## definuji regresory zájmu ---------------------------------------------------

regressors_of_interest <- c(
    
    setdiff(
        colnames(train_data)[
            unname(
                unlist(
                    lapply(
                        1:dim(train_data)[2],
                        function(i) class(train_data[, i])
                    )
                )
            ) == "numeric"
        ],
        c(
            "logerror"
        )
    ),
    colnames(train_data)[
        unname(
            unlist(
                lapply(
                    1:dim(train_data)[2],
                    function(i) class(train_data[, i])
                )
            )
        ) == "factor"
    ][
        !grepl(
            "(parcelid|error|fips|desc|block|city|county|hood|zip)$",
            colnames(train_data)[
                unname(
                    unlist(
                        lapply(
                            1:dim(train_data)[2],
                            function(i) class(train_data[, i])
                        )
                    )
                ) == "factor"
            ]
        )
    ]
    
)


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím hierarchický model ------------------------------------------------

my_lmer <- lmer(
    
    formula = paste(
        
        "logerror",
        " ~ ",
        paste(regressors_of_interest, collapse = " + "),
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





