discrep.default <-
function(datavec, targetvec, weightvec) {
    dat <- wpct(datavec, weightvec)
    out <- targetvec - dat
    out
}
