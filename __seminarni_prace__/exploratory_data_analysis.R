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

## vytipovávám proměnné s největším podílem chybějících hodnot; které
## bude nutné vyloučit před vytvořením modelu ---------------------------------

which_to_omit <- colnames(train_set)[
    apply(
        train_set,
        2,
        function(x) sum(is.na(x)) / length(x) * 100  # vrací, kolik procent
                                                     # dané proměnné tvoří
                                                     # chybějící hodnoty (NA)
    ) > 50
]   # je-li procento chybějících hodnot dané proměnné větší než 50,
    # danou proměnnou do modelu nezahrnu (a uložím ji do vektoru
    # "which_to_omit")


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





