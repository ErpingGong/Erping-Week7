
set.seed(123)


n <- 1000

race <- sample(c("Race1", "Race2"), n, replace = TRUE, prob = c(0.5, 0.5))
gender <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.5, 0.5))

vote_preference <- character(n)
for (i in 1:n) {
  prob_CandidateA <- 0.5 # Default probability
  if (race[i] == "Race1" & gender[i] == "Male") {
    prob_CandidateA <- 0.6
  } else if (race[i] == "Race1" & gender[i] == "Female") {
    prob_CandidateA <- 0.7
  } else if (race[i] == "Race2" & gender[i] == "Male") {
    prob_CandidateA <- 0.4
  } else if (race[i] == "Race2" & gender[i] == "Female") {
    prob_CandidateA <- 0.3
  }
  
  vote_preference[i] <- ifelse(runif(1) < prob_CandidateA, "CandidateA", "CandidateB")
}

voters_df <- data.frame(Race = race, Gender = gender, VotePreference = vote_preference)
head(voters_df)