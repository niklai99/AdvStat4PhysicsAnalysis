library(listarrays)

# compute box likelihood
likelihood <- function(val, j) {
    if (val == 1) {
        return(j / 5)
    }
    if (val == 0) {
        return((5 - j) / 5)
    }
  else return(NA)
}

# create the plot given the 6 posteriors and the iteration index
create_plot <- function(posteriors, iter) {
    par(mfrow = c(2, 3), oma = c(0, 0, 2, 0))

    rownames(posteriors) <- c("B0", "B1", "B2", "B3", "B4", "B5")
    for (i in 1:dim(posteriors)[1]) {
        box <- rownames(posteriors)[i]
        plot(
            seq(0, iter, 1),
            posteriors[box, ],
            pch      = 20,
            cex      = 1,
            ylim     = c(0, 1),
            xlab     = "trial",
            ylab     = "probability",
            main     = box,
            cex.main = 1,
            cex.lab  = 1,
            xaxt     = "n"
        )
        axis(1, at = 0:999)
        grid()
    }
    mtext("Six Boxes Toy Model : inference", outer = TRUE, cex = 1.5)
}


# initial state
iter       <- 0
posterior  <- array(rep(1 / 6, 6)) # uniform when we have no information
posteriors <- expand_dims(posterior, which_dim = 2)

repeat{

    # print the 6 probabilities
    print(posterior)

    # plot probability distributions
    create_plot(posteriors, iter)

    # input color
    x <- strtoi(
        readline(
            prompt = "extracted color (0 = black, 1 = white, 2 = stop): "
        )
    )
    cat("\n")

    # stop condition
    if (x == 2) {
        break
    }

    # update prior
    prior <- posterior

    # compute likelihoods
    likelihoods <- c(
        likelihood(x, 0),
        likelihood(x, 1),
        likelihood(x, 2),
        likelihood(x, 3),
        likelihood(x, 4),
        likelihood(x, 5)
    )

    # compute evidence
    evidence   <- sum(likelihoods * prior)

    # compute posterior
    posterior  <- likelihoods * prior / evidence

    # store posteriors together
    posteriors <- cbind(posteriors, posterior)

    # update iteration state
    iter <- iter + 1

}