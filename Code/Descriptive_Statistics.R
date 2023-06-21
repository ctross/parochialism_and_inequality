
############# Descriptives
# Giving
dgive <- rowSums(model_dat_Inland$Giving)-diag(model_dat_Inland$Giving)
dgive <- dgive[which(rowSums(model_dat_Inland$Giving) != 0)]

mean(dgive)
median(dgive)
sd(dgive)
max(dgive)
min(dgive)


dgive <- rowSums(model_dat_Coast$Giving)-diag(model_dat_Coast$Giving)
dgive <- dgive[which(rowSums(model_dat_Coast$Giving) != 0)]

mean(dgive)
median(dgive)
sd(dgive)
max(dgive)
min(dgive)

# Leaving
dgive <- rowSums(model_dat_Inland$Taking)-diag(model_dat_Inland$Taking)
dgive <- dgive[which(rowSums(model_dat_Inland$Taking) != 0)]

mean(dgive)/2
median(dgive)/2
sd(dgive)/2
max(dgive)/2
min(dgive)/2


dgive <- rowSums(model_dat_Coast$Taking)-diag(model_dat_Coast$Taking)
dgive <- dgive[which(rowSums(model_dat_Coast$Taking) != 0)]

mean(dgive)/2
median(dgive)/2
sd(dgive)/2
max(dgive)/2
min(dgive)/2

# Punishing
dgive <- rowSums(model_dat_Inland$Reducing)-diag(model_dat_Inland$Reducing)
dgive <- dgive[which(rowSums(model_dat_Inland$Reducing) != 0)]

mean(dgive)
median(dgive)
sd(dgive)
max(dgive)
min(dgive)


dgive <- rowSums(model_dat_Coast$Reducing)-diag(model_dat_Coast$Reducing)
dgive <- dgive[which(rowSums(model_dat_Coast$Reducing) != 0)]

mean(dgive)
median(dgive)
sd(dgive)
max(dgive)
min(dgive)



