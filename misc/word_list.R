library(wordle)
wordle_dict_rand <- pracma::randperm(wordle_dict, length(wordle_dict))
save(wordle_dict_rand, file = 'data/wordle_dict_rand.Rda')

word_list <- read.csv('data/words frequency.csv')

which(word_list==toupper('which'))
word_list$Rank   <- rank(word_list$Count)
word_list$Rank   <- length(word_list$Count) - rank(word_list$Count) + 1
wordle_dict_common <- tolower(word_list[word_list$Rank<=5000,"Word"])
wordle_dict_common <- pracma::randperm(wordle_dict_common, length(wordle_dict_common))
save(wordle_dict_common, file = 'data/wordle_dict_common.Rda')
