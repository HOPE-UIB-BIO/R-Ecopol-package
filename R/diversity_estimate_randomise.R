#' @title Estimating diversity and evenness based on resampling without replacement
#' @param data_matrix Data matrix
#' @param sample_size The minimum sample size
#' @param set_margin Are levels as columns or rows? 1 = rows, 2 = columns
#' @param rand Number of randomisations
#' @return Data frame with  Hill numbers N0, N1, N2, the evenness ratios N2/N1, N2/N0, and the
#' modified versions of N2/N1 and N2/N0. In addition, it estimate unbiased confidence intervals
#' @description Estimating diversity and evenness based on resampling without replacement. Code adapted from Chao et al. 2014
diversity_estimate_randomise <-
  function(data_matrix,
           sample_size,
           set_margin = c(1, 2),
           rand = 999) {

    util_check_class("data_matrix", "matrix")

    util_check_class("set_margin", "numeric")

    util_check_vector_values("set_margin", c(1, 2))

    if(missing(sample_size)){
      sample_size <- min(apply(data_matrix, set_margin, sum))
    }

    util_check_class("sample_size", "numeric")

    assertthat::assert_that(
      round(sample_size) == sample_size,
      msg = "'sample_size' has be integer")

    util_check_class("rand", "numeric")

    assertthat::assert_that(
      rand ==  round(rand),
      msg = "'rand' must be a 'integer'")

    # pre-alocate space
    count <- vector() #vector for species number
    n1_temp <- vector()  #vector for N1
    n2_temp <- vector()  #vector for N2
    epie <- vector()
    n2_n1 <- vector()   #vector for N2/N1
    n1_n0 <- vector()   #vector for N1/N0
    n1_n2 <- vector()  #vector for N1-N2
    n2_n1_mod <- vector() #vector for mod N2/N1
    n1_n0_mod <- vector() #vector for modify N1/N0
    abundance_matrix <- matrix()
    hill_diversity <- NULL

    # helper function
    .estim.boot.comm.ind <- function(species){
      species_observed <- sum(species > 0) # observed species
      n <- sum(species) # sample size
      f1 <- sum(species == 1) 	# singleton
      f2 <- sum(species == 2) 	# doubleton
      a <- ifelse(f1 == 0, 0, (n - 1) * f1 / ((n - 1) * f1 + 2 * f2) * f1 / n)
      b <- sum(species / n * (1 - species / n) ^ n)
      w <- a / b  		# adjusted factor for rare species in the sample
      f0_hat <- ceiling(ifelse(f2 == 0, (n - 1) / n * f1 * (f1 - 1) / 2, (n - 1) / n * f1 ^ 2/ 2 / f2)) # estimation of unseen species via Chao1
      prob_hat <- species / n * (1 - w * (1 - species / n) ^ n) # estimation of relative abundance of observed species in the sample
      prob_hat_unse <- rep(2 * f2/((n - 1) * f1 + 2 * f2), f0_hat)	# estimation of relative abundance of unseen species in the sample
      return(c(prob_hat, prob_hat_unse)) # Output: a vector of estimated relative abundance
    }


    for (j in 1:nrow(data_matrix)){

      n1 <- colnames(data_matrix) #species names or pollen types
      ab1 <- as.vector(data_matrix[j,]) #frequencies of species or pollen in each sample

      vec1 <- NULL    #a vector for the species or pollen pool
      for(i in 1:length(n1)) { #1: number of species or pollen types

        v1 <- rep(n1[i], ab1[i]) #repeat species names
        vec1 <- c(vec1, v1) #a vector that repeat the species or pollen types for the occurrences
      }

      for(i in 1:rand) {
        rsample <- sample(vec1, size = sample_size, replace = FALSE) #replacement = FALSE hypergeometriske distr.
        count[i] <- sum(table(rsample) > 0) #Species numbers (N0)

        N <- sum(table(rsample) > 0) #a number needed in the equations below
        p1 <- table(rsample)/sum(table(rsample)) #proportions

        n1_temp[i] <- exp(sum(-p1*log(p1))) #The exponential of Shannon diversity index (= N1)
        n2_temp[i] <- 1/sum(p1^2) #The inverse of the Simpson index (= N2)
        epie[i] <- N/(N-1)*(1-sum(p1^2))
        n1_n2[i] <- (exp(sum(-p1*log(p1))))-(1/sum(p1^2))
        n2_n1[i] <- (1/sum(p1^2))/(exp(sum(-p1*log(p1)))) #Evenness ratio N2/N1
        n1_n0[i] <- (exp(sum(-p1*log(p1))))/length(unique(rsample)) #Evenness ratio N1/N0
        n2_n1_mod[i] <- (1/sum(p1^2)-1)/(exp(sum(-p1*log(p1)))-1) #Modified version
        n1_n0_mod[i] <- (exp(sum(-p1*log(p1)))-1)/(sum(table(rsample)>0)-1) #Modified version
      }

      # Mean number of "rand" values
      hill_n_0 <- mean(count)
      hill_n_1 <- mean(n1_temp)
      hill_n_2 <- mean(n2_temp)

      # Confidence intervals for hill_n_0, hill_n_1, hill_n_2

      #function from Chao et al 2014
      estimated_prob <-
        .estim.boot.comm.ind(table(vec1))

      #randomisering av nye prob estimates
      abundance_matrix <-
        stats::rmultinom(200, sample_size, estimated_prob)

      error_0 <-
        stats::qnorm(0.975) * stats::sd(apply(abundance_matrix, 2,
                                              function(x) sum(x > 0)))
      error_1 <-
        stats::qnorm(0.975) * stats::sd(apply(abundance_matrix, 2,
                                              function(x) exp(sum(-table(x)/sum(table(x)) * log(table(x)/sum(table(x)))))))
      error_2 <-
        stats::qnorm(0.975) * stats::sd(apply(abundance_matrix,
                                              2, function(x) 1/sum((table(x)/sum(table(x)))^2)))

      hill_n_0_lower <- hill_n_0 - error_0
      hill_n_0_upper <- hill_n_0 + error_0
      hill_n_1_lower <- hill_n_1 - error_1
      hill_n_1_upper <- hill_n_1 + error_1
      hill_n_2_lower <- hill_n_2 - error_2
      hill_n_2_upper <- hill_n_2 + error_2

      # Evenness ratios and PIE

      evenness_0 <- mean(n2_n1)
      evenness_1 <- mean(n1_n0)
      evenness_3 <- mean(n1_n2)
      pie <- mean(epie)
      modE0 <- mean(n2_n1_mod)
      modE1 <- mean(n1_n0_mod)

      temp_res <-
        c("n0" = hill_n_0,
          "n0_lower" = hill_n_0_lower,
          "n0_upper" = hill_n_0_upper,
          "n1" = hill_n_1,
          "n1_lower" = hill_n_1_lower,
          "n1_upper" = hill_n_1_upper,
          "n2" = hill_n_2,
          "n2_lower" = hill_n_2_lower,
          "n2_upper" = hill_n_2_upper,
          "pie" = pie,
          "n2_dividedby_n1" = evenness_0,
          "n1_dividedby_n0" = evenness_1,
          "n1_minus_n2" = evenness_3,
          "n2_divided_by_n1_mod" = modE0,
          "n1_divided_by_n0_mod" = modE1)

      hill_diversity <- rbind(hill_diversity, temp_res)
    }

    rownames(hill_diversity) <- rownames(data_matrix)
    return(hill_diversity)
  }
