anesrake <-
    function(inputter, dataframe, caseid, 
             weightvec = NULL, cap = 5, verbose = FALSE, maxit = 1000, 
             type = "pctlim", pctlim = 5, nlim = 5, filter = 1, choosemethod = "total", 
             iterate = TRUE, convcrit = 0.01, force1 = TRUE, center.baseweights=TRUE) {
        ## APPLY FILTER TO ALL VARIABLES ##
        dataframe <- dataframe[filter == 1, ]
        caseid <- caseid[filter == 1]
        weightvec <- weightvec[filter == 1]
        mat <- as.data.frame(dataframe)
        origtype = type
        fullvars <- 0
        if (is.null(weightvec)) { # produce a vector of weights if no baseweights provided
            weightvec <- rep(1, length(caseid))
        }
        if(center.baseweights==TRUE){ # ensure that baseweights mean to 1
            weightvec <- weightvec/mean(weightvec, na.rm=TRUE)
        }
        if (length(weightvec) != length(caseid)) {
            stop("weight vector does not contain the same number of cases as data frame")
        }
        prevec <- weightvec
        not100 <- NULL
        not100 <- names(inputter)[!(sapply(inputter, function(x) sum(x) %in% c(1, 100)))] # Finds variable targets that do not add to 1 or 100
        if(!is.null(not100) & force1==FALSE & length(not100)>0){
            warning(paste("Targets for", not100, "do not sum to 100%. Did you make a typo entering the targets?"))
            warning(paste("You can force variables to sum to 1 by setting force1 to 'TRUE'"))
        }
        if(sum(names(inputter) %in% names(dataframe))!=length(names(inputter)))
            stop(paste("The names of the target variables should match the names of the data frame they are being matched to. The variable(s) -", names(inputter)[!(names(inputter) %in% names(dataframe))], "- were not found in the data frame"))
        if(force1==TRUE){
            if(!is.null(not100) & length(not100)>0)
                warning(paste("Targets for", not100, "do not sum to 100%. Adjusting values to total 100%"))
            inputter <- lapply(inputter, function(x) x/sum(x))
        }
        illegalnegs <- sum(unlist(inputter)<0)
        ## Identifies discrepancies for each level of each variable from the target
        discrep1 <- anesrakefinder(inputter, dataframe, weightvec, 
                                   choosemethod)
        if (type == "nolim") { # Forces all variables in
            towers <- inputter
        }
        if (type == "pctlim") { # Chooses only variables that are off by more than a given percentage
            towers <- selecthighestpcts(discrep1, inputter, pctlim)
        }
        if (type == "nlim") { # Chooses the n variables that are off by the most (with nlim indentifying the size of n)
            towers <- selectnhighest(discrep1, inputter, nlim)
        }
        if (type == "nmin") { # Chooses the variables that are off by more than a given percentage, but supplements up to a total of n variables if fewer than n are off by that amount
            towers <- selecthighestpcts(discrep1, inputter, pctlim, 
                                        tostop = 0)
            towers2x <- selectnhighest(discrep1, inputter, nlim)
            if (length(towers) > length(towers2x)) {
                type <- "pctlim"
            }
            if (length(towers) < length(towers2x)) {
                towers <- towers2x
            }
        }
        if (type == "nmax") { # Chooses the variables that are off by more than a given percentage unless there are more than n variables off by that percentage, in which case the most discrepant n are chosen
            fullvars <- 0
            discrep1 <- anesrakefinder(inputter, dataframe, weightvec, 
                                       choosemethod)
            towers <- selecthighestpcts(discrep1, inputter, pctlim)
            towers2x <- selectnhighest(discrep1, inputter, nlim)
            if (length(towers) > length(towers2x)) {
                towers <- towers2x
                fullvars <- 1
            }
        }
        ## Runs raking algorithm, capping after each full set of variables
        ranweight <- rakelist(towers, mat, caseid, weightvec, cap, 
                              verbose, maxit, convcrit)
        iterations <- ranweight$iterations
        iter1 <- ranweight$iterations
        weightout <- ranweight$weightvec
        ## Adjusts weights by running further iterations with additional variables if new variables are off by more than a specified amount for pctlim
        if (type == "pctlim" & iterate == TRUE) {
            ww <- 0
            it <- 0
            while (ww < 1) {
                it <- it + 1
                addtotowers <- selecthighestpcts(anesrakefinder(inputter, 
                                                                dataframe, weightout), inputter, pctlim, tostop = 0, 
                                                 warn = 0) ## TEST TO ENSURE THIS WORKS WELL
                adders <- addtotowers[!(names(addtotowers) %in% names(towers))]
                tow2 <- c(towers, adders)
                towersx <- towers
                towers <- tow2
                if (sum(as.numeric(names(towersx) %in% names(towers))) == 
                    length(towers)) {
                    ww <- 1
                }
                if (sum(as.numeric(names(towersx) %in% names(towers))) != 
                    length(towers)) {
                    if(verbose==TRUE)
                        print(paste("Additional variable(s) off after raking, rerunning with variable(s) included"))
                    ranweight <- rakelist(towers, mat, caseid, weightvec, 
                                          cap, verbose, maxit, convcrit)
                    weightout <- ranweight$weightvec
                }
                if (it > 10) {
                    ww <- 1
                }
                iterations <- ranweight$iterations
            }
        }
                                        # Does the same thing for nmax
        if (type == "nmax" & fullvars == 0 & iterate == TRUE) {
            ww <- 0
            it <- 0
            rundiscrep <- discrep1
            discrep2 <- rep(0, length(discrep1))
            while (ww < 1) {
                it <- it + 1
                rundiscrep <- rundiscrep + discrep2
                discrep2 <- anesrakefinder(inputter, dataframe, weightout)
                addtotowers <- selecthighestpcts(discrep2, inputter, 
                                                 pctlim, tostop = 0)
                tow2 <- c(towers, addtotowers)
                towersx <- towers
                towers <- unique(tow2)
                names(towers) <- unique(names(tow2))
                if (sum(as.numeric(names(towersx) %in% names(towers))) == 
                    length(towers)) {
                    ww <- 1
                }
                if (sum(as.numeric(names(towersx) %in% names(towers))) != 
                    length(towers)) {
                    if(verbose==TRUE)
                        print(paste("Additional variable(s) off after raking, rerunning with variable(s) included"))
                    ranweight <- rakelist(towers, mat, caseid, weightvec, 
                                          cap, verbose, maxit, convcrit)
                    weightout <- ranweight$weightvec
                }
                if (sum(as.numeric(names(towersx) %in% names(towers))) > 
                    nlim) {
                    print("variable maximum reached, running on most discrepant overall variables")
                    towers <- selectnhighest(discrep1, inputter, 
                                             nlim)
                    ranweight <- rakelist(towers, mat, caseid, weightvec, 
                                          cap, verbose, maxit, convcrit)
                    weightout <- ranweight$weightvec
                    iterations <- 0
                    ww <- 1
                }
                if (it >= 10) {
                    ww <- 1
                }
                iterations <- ranweight$iterations
            }
        }
        names(weightout) <- caseid
                                        # Produces final output
        out <- list(weightvec = weightout, type = type, caseid = caseid, 
                    varsused = names(towers), choosemethod = choosemethod, 
                    converge = ranweight$converge, nonconvergence = ranweight$nonconvergence, 
                    targets = inputter, dataframe = dataframe, iterations = iterations, 
                    iterate = iterate, prevec=prevec)
        class(out) <- c("anesrake", "anesrakelist")
        out
    }
