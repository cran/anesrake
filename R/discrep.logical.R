discrep.logical <-
function(datavec, targetvec, weightvec) {
    dat <- wpct(datavec, weightvec)
    out <- c(targetvec[2] - dat[1], targetvec[1] - dat[2])
    out
}
