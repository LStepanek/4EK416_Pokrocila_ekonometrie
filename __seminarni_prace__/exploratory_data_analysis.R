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

#### nejdříve numerické regresory zájmu ---------------------------------------

numeric_regressors_of_interest <- setdiff(
    
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
    
)


#### nyní kategorické regresory zájmu -----------------------------------------

factor_regressors_of_interest <- colnames(train_data)[
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


## ----------------------------------------------------------------------------

###############################################################################

## vytipovávám numerické proměnné s největším podílem chybějících hodnot;
## ty bude nutné vyloučit před vytvořením modelu ------------------------------

which_to_omit <- colnames(train_set)[
    apply(
        train_set,
        2,
        function(x) sum(is.na(x)) / length(x) * 100  # vrací, kolik procent
                                                     # dané proměnné tvoří
                                                     # chybějící hodnoty (NA)
    ) > 25
]   # je-li procento chybějících hodnot dané proměnné větší než 25,
    # danou proměnnou do modelu nezahrnu (a uložím ji do vektoru
    # "which_to_omit")


numeric_regressors_of_interest <- setdiff(
    
    numeric_regressors_of_interest,
    which_to_omit
    
)


## ----------------------------------------------------------------------------

###############################################################################

## redukuji počet numerických regresorů na základě korelační struktury --------

#### počítám korelační matici -------------------------------------------------

my_correlations <- suppressWarnings(
    
    cor(        
        x = train_set[, numeric_regressors_of_interest],
        method = "pearson",
        use = "pairwise.complete.obs"        
    )
    
)


#### kvůli vykreslení korelogramu nahrazuji nespočitatelné korelace (kvůli
#### chybějícím hodnotám, NA) nulami ------------------------------------------

my_correlations[is.na(my_correlations)] <- 0


#### ukládám korelogram -------------------------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

cairo_ps(
    file = "korelogram_numeric.eps",
    width = 8,
    height = 8,
    pointsize = 14
)

par(mar = c(0.1, 0.1, 0.1, 0.1))

corrgram(
    x = my_correlations,
    order = FALSE,
    lower.panel = panel.pie,
    labels = 1:dim(my_correlations)[1]
)

dev.off()

setwd(mother_working_directory)


#### dle korelogramu vyřazuji z numerických regresorů ty, které silně
#### korelují s některými jinými regresory (kolinearita) ----------------------

numeric_regressors_of_interest <- numeric_regressors_of_interest[
    -c(3, 4, 5, 6, 14, 15, 16)
    #-c(3, 4, 14)
]


#### nakonec ještě přeškálovávám numerické regresory na interval (0, 1) -------

train_set[
    ,
    numeric_regressors_of_interest
] <- apply(
    train_set[
        ,
        numeric_regressors_of_interest
    ],
    2,
    function(x){
        (
            x - min(x, na.rm = TRUE)
        ) / (
            max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
        )
    }
)

test_set[
    ,
    numeric_regressors_of_interest
] <- apply(
    test_set[
        ,
        numeric_regressors_of_interest
    ],
    2,
    function(x){
        (
            x - min(x, na.rm = TRUE)
        ) / (
            max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
        )
    }
)


## ----------------------------------------------------------------------------

###############################################################################

## obdobně pro kategorické regresory
## zkoumám vždy chí-kvadrát statistiku (a p-hodnotu) mezi všemi možnými
## dvojicemi kategorických regresorů a z dvojic, které spolu významně
## souvisí (zamítnutí H_0 o nezávislosti) vyberu jen jeden regresor -----------

#### do tabulky "factor_association" ukládám Cramerova V ----------------------

factor_association <- matrix(
    rep(0, length(factor_regressors_of_interest) ^ 2),
    nrow = length(factor_regressors_of_interest)
)

for(i in 1:length(factor_regressors_of_interest)){
    
    for(j in 1:length(factor_regressors_of_interest)){
        
        my_table <- table(
            train_set[
                ,
                factor_regressors_of_interest[i]
            ],
            train_set[
                ,
                factor_regressors_of_interest[j]
            ]
        )
        
        if(is.nan(suppressWarnings(chisq.test(my_table)$statistic))){
            
            factor_association[i, j] <- 1.0
            
        }else{
            
            factor_association[i, j] <- sqrt(
                suppressWarnings(
                    chisq.test(my_table)$statistic
                ) / sum(my_table)
            )
            
        }
        
        ## logovací hlášky ----------------------------------------------------
        
        flush.console()
        
        print(
            paste(
                "Proces hotov z ",
                format(
                    round(
                        (
                            (i - 1) / length(factor_regressors_of_interest) +
                            j / length(factor_regressors_of_interest) ^ 2
                        ) * 100,
                        digits = 2
                    ),
                    nsmall = 2
                ),
                " %.",
                sep = ""
            )
        )
        
    }
    
}


#### při malých velikostech tabulek může být Cramerovo V větší než 1.0,
#### což ošetřuji -------------------------------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

cairo_ps(
    file = "korelogram_factor.eps",
    width = 8,
    height = 8,
    pointsize = 14
)

par(mar = c(0.1, 0.1, 0.1, 0.1))

factor_association[factor_association > 1.0] <- 1.0

corrgram(
    x = factor_association,
    order = FALSE,
    lower.panel = panel.pie,
    labels = 1:dim(factor_association)[1]
)

dev.off()

setwd(mother_working_directory)


#### zcela spolu asociují faktory s indexy 3, 4, 5, 9, 13, 14, 16;
#### ponechávám z nich jen ten s indexem 3 ------------------------------------

factor_regressors_of_interest <- factor_regressors_of_interest[
    -c(4, 5, 9, 13, 14, 16)
]


#### u kategorických proměnnýc je třeba vyřadit i ty faktory, které mají
#### některé hodnoty prakticky nezastoupeny; pro ně by se model neměl
#### na čem "naučit" odhadovat logerror ---------------------------------------

#### expertně tedy vyřazuji ---------------------------------------------------

factor_regressors_of_interest <- setdiff(
    
    factor_regressors_of_interest,
    c(
        "taxdelinquencyflag",
        "fireplaceflag",
        "storytypeid",
        "pooltypeid10",
        "pooltypeid2",
        "pooltypeid7",
        "decktypeid"        
    )
    
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





