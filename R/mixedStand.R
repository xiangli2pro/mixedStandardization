#' @name mixedStand
#' @title Standardize inputs for fair variable selection in sparse penalized regression.
#' @author Xiang Li, Qing Pan, Yong Ma
#'
#' @description
#' `mixedStand( )` standardize variables using specified standardization method.
#'
#' @param x an input data frame.
#' @param y outcome variable.
#' @param standardization a character of specified standardization methods. Users can choose from 5 methods:
#' `Zscore`, `Gelman`, `Minmax`, `Bring`, `Mixed`. The default is the `Mixed` method.
#' @param continuousVars either a character vector of continuous variable names, or a
#' numeric vector of continuous variable column indices.
#' @param binaryVars either a character vector of binary variable names, or a
#' numeric vector of binary variable column indices.
#' @param categoricalVars either a character vector of categorical variable names, or a
#' numeric vector of categorical variable column indices.
#' @param pDichoto threshold of converting continuous variable to binary variable.
#' @param family can be "binomial" for logistic regression and "gaussian" for linear regression.
#'
#' @return Standardized data.
#' \item{xStand}{standardized data}
#'
#' @importFrom dplyr %>% mutate_all mutate_at select all_of
#' @importFrom stats predict
#'
#' @export
#' @rdname mixedStand
mixedStand <- function(
  x,
  y,
  standardization = "Mixed",
  continuousVars = NULL,
  binaryVars = NULL,
  categoricalVars = NULL,
  pDichoto = 0.5,
  family = "binomial"
  ){

  # x: name of input dataset.
  # y: name of outcome variable
  # standardization: can be "Zscore", "Gelman", "Minmax", "Bring", "Mixed".
  # continuousVars: either a character vector of continuous variable names, or a
  #                 numeric vector of continuous variable column indices.
  # binaryVars: either a character vector of binary variable names, or a
  #             numeric vector of binary variable column indices.
  # categoricalVars: either a character vector of categorical variable names, or a
  #                  numeric vector of categorical variable column indices.
  # pDichoto: threshold of converting continuous variable to binary variable.
  # family: can be "binomial" for logistic regression and "gaussian" for linear regression

  ##*****
  # check if parameters meet requirement
  ##*****

  # check if x exist
  if(!(exists("x") && is.data.frame(get("x")))){

    stop("\n---- the input dataset does not exist ----")
  }

  # check if standardization method is available
  if(! standardization %in% c("Zscore", "Gelman", "Minmax", "Bring", "Mixed")){

    stop("\n---- standardization can only be Zscore, Gelman, Minmax, Bring or Mixed ----")
  }

  # check if argument inputs are valid
  if(all(c(is.null(continuousVars),
           is.null(binaryVars),
           is.null(categoricalVars)))){

    stop("\n---- continuousVars, binaryVars and categoricalVars cannot all be NULL ----")
  }

  # check if argument inputs are consistent
  typeIdx <- which(c(!is.null(continuousVars),
                     !is.null(binaryVars),
                     !is.null(categoricalVars)) == TRUE)

  if(length(typeIdx) >= 2){

    varsType <- c("continuousVars", "binaryVars", "categoricalVars")[typeIdx]
    varsClass <- sapply(varsType, function(var) class(get(var)))

    if(length(unique(varsClass)) >= 2){

      stop(paste0("\n---- ",
                  paste(varsType, collapse = ", "),
                  " have to be the same class:",
                  "\n---- either character vector of variable names or numeric vector of variable indices ----"))
    }
  }

  # check if continuous variable names are valid
  if(is.character(continuousVars) && any(!continuousVars %in% names(x))){

    varsIdx <- which(!continuousVars %in% names(x))

    stop(paste0("\n---- continuous variable: ",
                paste(continuousVars[varsIdx], collapse = ", "),
                ", do not exist in the dataset ----"))
  }

  # check if binary variable names are valid
  if(is.character(binaryVars) && any(!binaryVars %in% names(x))){

    varsIdx <- which(!binaryVars %in% names(x))

    stop(paste0("\n---- binary variable: ",
                paste(binaryVars[varsIdx], collapse = ", "),
                ", do not exist in the dataset ----"))
  }

  # check if categorical variable names are valid
  if(is.character(categoricalVars) && any(!categoricalVars %in% names(x))){

    varsIdx <- which(!categoricalVars %in% names(x))

    stop(paste0("\n---- categorical variable: ",
                paste(categoricalVars[varsIdx], collapse = ", "),
                ", do not exist in the dataset ----"))
  }

  # check claimed continuous variables
  if(!is.null(continuousVars)){

    continuousData <- x %>%
      select(all_of(continuousVars))

    typeCheck <- which(sapply(continuousData, is.numeric) == FALSE)

    if(length(typeCheck) >= 1){

      stop(paste0("\n---- variable(index): ",
                  paste(continuousVars[typeCheck], collapse = ", "),
                  ", are not continuous as specified----"))
    }
  }

  # check claimed binary variables
  if(!is.null(binaryVars)){

    binaryData <- x %>%
      select(all_of(binaryVars))

    typeCheck <- which(sapply(binaryData, function(var) length(unique(var)) == 2) == FALSE)

    if(length(typeCheck) >= 1){

      stop(paste0("\n---- variable(index): ",
                  paste(binaryVars[typeCheck], collapse = ", "),
                  ", are not binary as specified ----"))
    }
  }

  # check claimed categorical variables
  if(!is.null(categoricalVars)){

    categoricalData <- x %>%
      select(all_of(categoricalVars))

    typeCheck <- which(sapply(categoricalData, function(var) length(unique(var)) >= 3) == FALSE)

    if(length(typeCheck) >= 1){

      stop(paste0("\n---- variable(index): ",
                  paste(categoricalVars[typeCheck], collapse = ", "),
                  ", are not categorical as specified (< 3 levels) ----"))
    }
  }

  message("** parameter check completed **")
  message("-------------------------------------------")


  ##*****
  # standardize
  ##*****


  xRaw <- x %>%
    select(all_of(c(continuousVars, binaryVars, categoricalVars)))

  if(standardization == "Zscore"){

    # Zscore:
    # first convert binary/categorical to dummy variables
    # then scale all variables by one standard deviation

    xDummies <- caret::dummyVars(~., data = xRaw, fullRank = T)
    xStand <- predict(xDummies, xRaw) %>%
      as.data.frame() %>%
      mutate_all(., function(var) var / stats::sd(var, na.rm = TRUE))

  } else if(standardization == "Gelman"){

    # Gelman:
    # first scale continuous variables by 2 standard deviations
    # then convert binary/categorical to dummy variables

    xStand0 <- xRaw %>%
      mutate_at(c(continuousVars), function(var) var / 2 / stats::sd(var, na.rm = TRUE))
    xDummies <- caret::dummyVars(~., data = xStand0, fullRank = T)
    xStand <- predict(xDummies, xStand0) %>%
      as.data.frame()

  } else if(standardization == "Minmax"){

    # Gelman:
    # first transform continuous variables to the range between [0,1]
    # then convert binary/categorical to dummy variables

    xStand0 <- xRaw %>%
      mutate_at(c(continuousVars), function(var) (var - min(var, na.rm = TRUE)) / (max(var, na.rm = TRUE) - min(var, na.rm = TRUE)))
    xDummies <- caret::dummyVars(~., data = xStand0, fullRank = T)
    xStand <- predict(xDummies, xStand0) %>%
      as.data.frame()

  } else if(standardization == "Bring"){

    # Bring:
    # first convert binary/categorical to dummy variables
    # then scale all variables by their partial standard deviations

    xDummies <- caret::dummyVars(~., data = xRaw, fullRank = T)
    xStand0 <- predict(xDummies, xRaw) %>%
      as.data.frame()

    varVif <- car::vif(stats::lm(y ~ ., data = data.frame(y, xStand0)))
    partialStand <- sapply(xStand0, stats::sd, na.rm = TRUE)/ sqrt(varVif)* sqrt(nrow(xStand0)-1)/ sqrt(nrow(xStand0) - ncol(xStand0))

    xStand <- xStand0 %>%
      sweep(2, partialStand, `/`) %>%
      as.data.frame()

  } else if(standardization == "Mixed"){

    # Mixed:
    # first convert continuous & categorical to binary variables
    # then scale all variables by their two standard deviations

    xStand <- xRaw %>%
      mutate_at(categoricalVars, function(var) categorical2binary(var, y, pDichoto, family)) %>%
      mutate_at(continuousVars, function(var) ifelse(var <= stats::quantile(var, probs = pDichoto), 1, 0)) %>%
      mutate_at(binaryVars, function(var) {
        bi_index <- (var == unique(var)[which.min(table(var))[1]])
        var_stand <- vector(length = length(var))
        var_stand[bi_index] <- 1
        var_stand[!bi_index] <- 0
        var_stand
      }) %>%
      mutate_all(., function(var) var/ 2/ stats::sd(var, na.rm = TRUE))
  }

  # return standardized input
  xStand
}

## global define the variable .
utils::globalVariables(".")

## transform categorical variable to binary variable
categorical2binary <- function(var, y, pDichoto, family) {

  # if y is binary, y has to take values of 0, 1
  if(family == "binomial"){

    y <- ifelse(y == unique(y)[which.min(unique(y))], 0, 1)
  }

  # levels of the categorical variable
  var_levels <- levels(var)

  # log-likelihood ratio test
  lrt_test <- function(var, y) {

    mod <- data.frame(y, var) %>%
      stats::glm(y ~ var, data = ., family = family)

    lrt <- stats::anova(mod, test = "LRT")

    # check if P-value less then 0.05
    lrt[2, 5] < 0.05
  }

  # if variable is significant
  if(lrt_test(var, y)) {

    # coefficients
    mod_coef <- data.frame(y, var) %>%
      stats::glm(y ~ var, data = ., family = family) %>%
      stats::coef()

    # add into the reference level effect
    mod_coef[2:length(mod_coef)] <- mod_coef[2:length(mod_coef)] + mod_coef[1]

    # arrange the level by their effect
    var_coef_order <- order(mod_coef)

  # if variable is NOT significant, randomly rank the levels
  } else {

    var_coef_order <- sample(c(1:length(var_levels)),
                           length(var_levels),
                           replace = FALSE)
  }

  # frequency of each level ordered by their rankings
  var_freq_order <- table(var)[var_coef_order] / length(var)

  # find the level with corresponding cumulative frequency closest to pDichoto
  level_cut <- which.min(abs(cumsum(var_freq_order) - pDichoto))[1]
  if (level_cut == length(levels(var))) {

    level_cut = level_cut - 1
  }

  # define the standardized binary variable
  bi_index <- var %in% (var_levels[var_coef_order])[c(1:level_cut)]

  var_stand <- vector(length = length(var))
  var_stand[bi_index] <- 1
  var_stand[!bi_index] <- 0

  # return standardized variable
  var_stand
}











