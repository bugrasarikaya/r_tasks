set.seed(2016556506)
#Q1A:
output_primality <- function(vector) {
  non_primes <- primes <- c()
  for (number in vector) {
    if (number < 2) primality <- FALSE
    else if(number == 2) primality <- TRUE
    else {
      for (i in 2: (number - 1)) {
        if (number %% i == 0) {
          primality <- FALSE
          break;
        } else primality <- TRUE
      }
    }
    ifelse(primality, primes <- c(primes, number), non_primes <- c(non_primes, number))
  }
  cat("Prime numbers: ", primes, "\n")
  cat("Non-prime numbers: ")
  for (non_prime in non_primes) {
    cat(non_prime)
    prime_factors <- c()
    if (non_prime > 2) {
      for (i in 2: (non_prime - 1)) {
        if (i < 2) primality <- FALSE
        else if(i == 2) primality <- TRUE
        else {
          for (j in 2: (i - 1)) {
            if (i %% j == 0) {
              primality <- FALSE
              break;
            } else primality <- TRUE
          }
        }
        if(non_prime %% i == 0 & primality) prime_factors <- c(prime_factors, i)
      }
    }
    if (length(prime_factors) != 0) {
      cat(" [")
      cat(prime_factors, sep = " ")
      cat("] ")
    } else cat(" ")
  }
  cat("\n")
}
output_primality(c(89, 107, 597, 931, 1083))
#Q2A:
library(tidyverse)
order_words <- function(strings) {
  strings <- str_to_lower(str_replace(str_replace(strings, ".$", ""), "\\.", ""))
  ordered_words <- words <- c()
  for (sentence in strings) {
    words <- str_split(sentence, " ")
    for (word in words) ordered_words <- c(ordered_words, word)
  }
  for (i in 1: (length(ordered_words) - 1)) {
    for (j in 1: (length(ordered_words) - i)) {
      if (str_length(ordered_words[j]) > str_length(ordered_words[j + 1])) {
        temp <- ordered_words[j]
        ordered_words[j] <- ordered_words[j + 1]
        ordered_words[j + 1] <- temp
      } else if (str_length(ordered_words[j]) == str_length(ordered_words[j + 1])) {
        if (ordered_words[j] > ordered_words[j + 1]) {
          temp <- ordered_words[j]
          ordered_words[j] <- ordered_words[j + 1]
          ordered_words[j + 1] <- temp
        }
      }
    }
  }
  return(paste(ordered_words, collapse = " "))
}
print(order_words(stringr::sentences[sample(length(stringr::sentences), 6)]))