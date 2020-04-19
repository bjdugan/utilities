# Password generator and evaluator
# generally performs close to http://www.passwordmeter.com/ but not exactly

eval_pwd <- function(pwd) {


  eligible_chars <- list(
    a = letters,
    A = LETTERS,
    n = 0:9,
    s = c('!','@','#','$','%','^','&','*', '(', ')','-','_','=','+',
          '`','~', ',','<', '.','>','/','?', '|', ';', ':',
          '[','{', ']', '}')
  )
  strength <- list(n_chars = NA,
                   n_upper = NA,
                   n_lower = NA,
                   n_symbols = NA,
                   n_numbers = NA,
                   inner_sn = NA,
                   composition = NA) # 8 char len, at least 3/4 other n_*'s.
  weakness <- list(only_letters = NA,
                   only_numbers = NA,
                   repeated_chars = NA,
                   consecutive_upper = NA,
                   consecutive_lower = NA,
                   consecutive_number = NA,
                   sequential_number = NA,
                   sequential_letter = NA,
                   sequential_symbol = NA)

  pwd <- unlist(strsplit(pwd, "")) #break into individual characters

  strength$n_chars <- length(pwd) * 4
  strength$n_upper <- (length(pwd) - sum(pwd %in% LETTERS)) * 2
  strength$n_lower <- (length(pwd) - sum(pwd %in% letters)) * 2
  strength$n_symbols <- sum(pwd %in% eligible_chars$s) * 6
  strength$n_numbers <- sum(pwd %in% 0:9) * 4
  strength$inner_sn <- (sum(pwd[2:(length(pwd) - 1)] %in% eligible_chars$s) +
                          sum(pwd[2:(length(pwd) - 1)] %in% 0:9)) * 2
  strength$composition <- sum(ifelse(strength$n_chars > 0, 1, 0),
                              ifelse(strength$n_upper > 0, 1, 0),
                              ifelse(strength$n_lower > 0, 1, 0),
                              ifelse(strength$n_symbols > 0, 1, 0),
                              ifelse(strength$n_numbers > 0, 1, 0)) * 2

  weakness$only_letters <- ifelse(all(pwd %in% c(letters, LETTERS)), length(pwd), 0)
  weakness$only_numbers <- ifelse(all(pwd %in% 0:9), length(pwd), 0)
  # case-insensitive. -1 for each same character, but staggered as longer passwords
  # tend to include repeated characters. Thus, doggy123 would get -2, doggy1123 -3, doggy11233 - 4
  # however this seems to have an odd scaling element: doggy112233 - 5 (4 repeats), but
  # doggyy112233 -9 (5 repeat elements)
  weakness$repeated_chars <- NA
  x <- matrix(c(pwd[1:(length(pwd) - 1)] %in% LETTERS,
              pwd[2:length(pwd)] %in% LETTERS), ncol = 2)
  weakness$consecutive_upper <- sum(apply(x, 1, all)) * 2
  x <- matrix(c(pwd[1:(length(pwd) - 1)] %in% letters,
                pwd[2:length(pwd)] %in% letters), ncol = 2)
  weakness$consecutive_lower <- sum(apply(x, 1, all)) * 2
  x <- matrix(c(pwd[1:(length(pwd) - 1)] %in% 0:9,
                pwd[2:length(pwd)] %in% 0:9), ncol = 2)
  weakness$consecutive_number <- sum(apply(x, 1, all)) * 2

  x <- suppressWarnings(matrix(c(as.numeric(pwd[1:length(pwd)]),
                                 as.numeric(pwd[2:length(pwd)]), NA,
                                 as.numeric(pwd[3:length(pwd)]), NA, NA),
                                 ncol = 3))
  x <- data.frame(x)
  x[,4] <- x[,1] + 1 == x[,2]
  x[,5] <- x[,1] + 2 == x[,3]
  x[,6] <- x[,1] - 1 == x[,2]
  x[,7] <- x[,1] - 2 == x[,3]
  weakness$sequential_number <- sum(apply(x[,c("V5", "V7")], 1, any, na.rm = T)) * 3


  x <- tolower(pwd)
  for (i in 1:length(x)) {
    if (x[i] %in% letters) {
      alpha_seq <- paste0(x[i], letters[grep(x[i], letters) + 1], letters[grep(x[i], letters) + 2])
      x[i] <- grepl(alpha_seq, paste(x, collapse = "")) * 3
    } else {
      x[i] <- 0
    }
  }
  weakness$sequential_letter <- sum(as.numeric(x))

  x <- tolower(pwd)
  for (i in 1:length(x)) {
    if (x[i] %in% eligible_chars$s) {
      symbol_seq <- paste0(x[i], eligible_chars$s[grep(x[i], eligible_chars$s, fixed = T) + 1],
                           eligible_chars$s[grep(x[i], eligible_chars$s, fixed = T) + 2])
      x[i] <- grepl(symbol_seq, paste(x, collapse = ""), fixed = T) * 3
    } else {
      x[i] <- 0
    }
  }
  weakness$sequential_symbol <- sum(as.numeric(x)) # !@#$, i.e., just running L to R across keyboard

  return(sum(as.numeric(strength), na.rm = T) - sum(as.numeric(weakness), na.rm = T))
}

# Is there any need to have all types of characters as different list elements?
# cant these just be pooled and skip the 1st sample stage?
make_pwd <- function(n_pwds = 3, n_chars = 8, include_chars = F) {

  eligible_chars <- list(
    a = letters,
    A = LETTERS,
    n = 0:9
  )

  if (include_chars == T) {
    eligible_chars$s = c('!','@','#','$','%','^','&','*','_','-','=',
                                 '+', '~', '<', '>', ',', '.', '?')
  }
  pwds <- list()
  length(pwds) <- n_pwds
  for (i in 1:n_pwds) {
    pwd <- ""
    for (j in 1:n_chars) {
      # take 1 character at random from 1 vector of characters at random, append
      pwd <- paste0(pwd, sample(eligible_chars[[sample(1:length(eligible_chars), 1)]], 1))
    }
    pwds[[i]] <- pwd
  }
  pwds <- as.data.frame(unlist(pwds), stringsAsFactors = F)
  colnames(pwds) <- "password"
  pwds$score <- lapply(pwds$password, eval_pwd)
  pwds$rating <- "Weak"
  pwds[pwds$score >= 50, "rating"] <- "Moderate"
  pwds[pwds$score >= 75, "rating"] <- "Strong"

  # functionality here to call this again if majority are not strong? or to select just the strongest n?

  return(pwds)
}


# just a test ####
# test_pwd_maker <- function(n = 10) {
#   t1 <- Sys.time()
#   x <- make_pwd(n)
#   t2 <- Sys.time()
#   return(cat(n, "passwords generated in", t2 - t1, "seconds.",
#              "\n", round(nrow(x[x$score >= 75, ]) / n * 100, 2),  "% strong",
#              "\n", round(nrow(x[x$score < 75 & x$score >= 50, ]) / n * 100, 2), "% moderate",
#              "\n", round(nrow(x[x$score < 50, ]) / n * 100,  2), "% weak"
#              ))
# }

make_pwd()
