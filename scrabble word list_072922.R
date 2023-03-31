#https://boardgames.stackexchange.com/questions/38366/latest-collins-scrabble-words-list-in-text-file


# Loading libraries and data ----
#load the tidyverse
library("tidyverse")
library("data.table")

 start_time <- Sys.time() #to measure how long this takes

# words from here https://github.com/powerlanguage/word-lists/blob/master/common-7-letter-words.txt
 word_list1 <- read.table("C:/Users/drsin/OneDrive/Documents/R Projects/wordle/Collins_2019.txt",
                          sep = "\n",colClasses = "character", strip.white = TRUE,
                          blank.lines.skip = TRUE )
#word_list <- read.table("C:/Users/drsin/OneDrive/Documents/R Projects/wordle/Collins_2019.txt", nrows=5000, sep = "\n")
# word_list <- fread("C:/Users/drsin/OneDrive/Documents/R Projects/wordle/Collins_2019.txt", sep = "\n")
 #for (ind in 1:nrow(word_list)){

  # word_list[[ind, 1]] <- tolower(word_list[ind, 1])
  # word_list[[ind, 1]] <- gsub(" ", "", word_list[[ind, 1]])
  # print(ind)

 #}
 end_time <- Sys.time()
 duration <- end_time - start_time

Scoring_Word <- function(word){
  #i'm not handling duplicate letters at all right now
  print("in funct")
  letter_vec <-  unlist(strsplit(word, split = ""))
  print(letter_vec)
  value <- 0
  for (i in 1:length(letter_vec)) {
    position <- letter_vec[i]== char_frequencies$letters
    value[i] <- all_y[position]
    print(i)
    if (i==length(letter_vec)) {
      # print("I am here")
       print(sum(value))
      return(total <- sum(value))
    }
    
  }
} 


Scoring_Word_Unique <- function(word){
  # This does only score on unique letters
  # print(word)
  letter_vec <-  unlist(strsplit(word, split = ""))
  unique_letter_vec <- unique(letter_vec)
  # print(unique_letter_vec)
  #print(length(unique_letter_vec))
  
  value <- 0
  if (length(unique_letter_vec)== 0) {
    return(value)
  } else{
    for (i in 1:length(unique_letter_vec)) {
      position <- unique_letter_vec[i]== char_frequencies$letters
      value[i] <- all_y[position]
      # print(i)
      # print(value)
      if (i==length(unique_letter_vec)) {
        # print("I am here")
        # print(sum(value))
        return(total <- sum(value))
      }
      
    }
  }
}
Removing_Letters <- function (word, chosen_word, num_lett) {
  ind <- 1
  print(chosen_word)
  char_vec <- unlist(strsplit(chosen_word, ""))
  test <- word
  for (ind in 1:num_lett) {
    test <- str_replace_all(test, char_vec[ind], "")
    
    #print(char_vec[ind])
    #print(test)
    
  }
  return(test)
}

# making for all letters frequency table ----
letters <- unlist(strsplit(word_list1$V1, split = ""))
char_frequencies <- as.data.frame(table(letters))
#char_frequencies

num_words <- nrow(word_list1)  #I should read this in
#num_words <- 5
word_scores <- data.frame(word_name = word_list1[1:num_words,1],
                          word_length = rep(0, times = num_words),
                          word_guess1 = rep(0, times = num_words),
                          word_guess2 = rep(0, times = num_words),
                          word_guess3 = rep(0, times = num_words),
                          word_guess4 = rep(0, times = num_words),
                          score = rep(0, times = num_words), 
                          score_guess1 = rep(0, times=num_words),
                          score_guess2 = rep(0, times=num_words),
                          score_guess3 = rep(0, times=num_words),
                          score_guess4 = rep(0, times=num_words)
)

word_scores$word_length <-  str_length(word_scores$word_name)



# Creating some visualizations of the frequency table ----

#raw counts
# ggplot(char_frequencies, aes(x= fct_reorder(char_frequencies[,1], char_frequencies[,2])
#                              , char_frequencies[,2] )) +
#   geom_col()+
#   theme_classic()

#normalized

 common <- sum(char_frequencies[,2])
 all_y=(char_frequencies[,2]/common)

# 
# 
# six_words <- filter(word_scores, word_scores$word_length == 6)
# six_letters <- unlist(strsplit(six_words[,1], split = ""))
# six_char_frequencies <- as.data.frame(table(six_letters))
# six_common <- max(six_char_frequencies[,2])
# six_y=(six_char_frequencies[,2]/six_common)
# 
# 
# seven_words <- filter(word_scores, word_scores$word_length == 7)
# seven_letters <- unlist(strsplit(seven_words[,1], split = ""))
# seven_char_frequencies <- as.data.frame(table(seven_letters))
# seven_common <- max(seven_char_frequencies[,2])
# seven_y=(seven_char_frequencies[,2]/seven_common)
# 
# #letter_order <- fct_reorder(char_frequencies[,1], char_frequencies[,2])
# #letter_order <- fct_rev(letter_order)
# let2 <- char_frequencies[,1]
# freq_by_length <- data.frame(let2, all_y, five_y, six_y, seven_y)
# tidy_freq_by_length <- pivot_longer(freq_by_length, names_to = "word_length", cols =2:5)
# 
# 
# tidy_freq_by_length %>%
#   group_by(word_length) %>%
#   ggplot(aes(x= let2, y=value, fill = word_length )) +
#   geom_col(alpha = 0.5, position = "dodge")+
#   ggtitle("Frequency Table") +
#   ylab("Normalized Counts") +
#   xlab ("Letter") +
#   theme_classic()
# 
# 


# Bulk of the code is here ----



# Calculates the initial scores for all words.-----
# This calculates the score for all words, without worrying about duplicate letts.
ind2 <- 1

for (ind2 in 1:num_words){
  print(word_scores[[ind2, "word_name"]])
  score_ind2 <- Scoring_Word(word_scores[[ind2, "word_name"]])
  word_scores[[ind2,"score"]] <- score_ind2
}


ind2 <- 0
for (ind2 in 1:num_words){
  # print(word_scores[[ind2,1]])
  #score_ind2 <- Scoring_Word_Unique(word_scores[[ind2,1]])
  # print(score_ind2)
  #  word_scores[[ind2,3]] <- score_ind2
  score_u_ind2 <- Scoring_Word_Unique(word_scores[[ind2,"word_name"]])
  word_scores[[ind2,"score_guess1"]] <- score_u_ind2
  
}


# working just with 5 letter words for the moment



 
 five_words <- filter(word_scores, word_scores$word_length == 5)
 five_letters <- unlist(strsplit(five_words[,1], split = ""))
 five_char_frequencies <- as.data.frame(table(five_letters))
 five_common <- max(five_char_frequencies[,2])
 five_y=(five_char_frequencies[,2]/five_common)

 
 
 # Finding the best first word
 top_fivewords <- five_words %>%
  arrange(desc(score_guess1))
 
 fivewords_word_1 <- top_fivewords$word_name[1]

#now we need a function that sees if a word has the letters of the word_1
#and removes them and then calculates the word score
# this is finding GUESS 2
# Word 1= arose -----

 ind3 <- 1
 for (ind3 in 1:num_words) {
   test<- five_words[[ind3,"word_name"]]
   word_scores[[ind3, "word_guess2"]]<- Removing_Letters(test, fivewords_word_1, five_words[[ind3, "word_length"]] )
   #print(word_scores[[ind3, "word_guess2"]])
   score_ind3 <- Scoring_Word_Unique( five_words[[ind3, "word_guess2"]])
   word_scores[[ind3,"score_guess2"]] <- score_ind3
   #print (c("output of small list ", top_words[[ind3,4]]))
 }

# top_words <- word_scores %>%
 #  arrange(desc(score_guess2))

#word_2 <- top_words$word_name[1]

# 
# # This is for GUESS 3
# ind4 <- 1
# for (ind4 in 1:num_words) {
# 
#   test<- word_scores[[ind4,"word_guess2"]]
#   word_scores[[ind4,"word_guess3"]] <- Removing_Letters(test, word_2, word_scores[[ind4, "word_length"]] )
#   #print ( word_scores[[ind4,"word_guess3"]])
#   score_ind4 <- Scoring_Word_Unique(word_scores[[ind4,"word_guess3"]])
#   word_scores[[ind4,"score_guess3"]] <- score_ind4
# 
# }
# #top_words <- word_scores %>%
#  # arrange(desc(score_guess3))
# 
# 
# #word_3 <- top_words$word_name[1]
# 
# #this is for GUESS 4
# ind4 <- 1
# for (ind4 in 1:num_words) {
#   test<- word_scores[[ind4,"word_guess3"]]
#   word_scores[[ind4,"word_guess4"]] <- Removing_Letters(test, word_3, word_scores[[ind4, "word_length"]] )
#   # print ( word_scores[[ind4,"word_guess4"]])
#   score_ind4 <- Scoring_Word_Unique(word_scores[[ind4,"word_guess4"]])
#   word_scores[[ind4,"score_guess4"]] <- score_ind4
# 
# }
# 
# top_words <- word_scores %>%
#   arrange(desc(score_guess4))
# 
# word_4 <- top_words$word_name[1]
# 
# word_scores2 <- word_scores %>%
#   select(word_name, score_guess1, score_guess2, score_guess3, score_guess4)
# 
# 
# # 
# # word_scores_reshaped <- pivot_longer(word_scores2, cols = 2:5, names_to = "score_type", values_to = "score")
# # #levels = c("score_guess1", "score_guess2", "score_guess3")
# # word_scores_reshaped$score_type <- as.factor(word_scores_reshaped$score_type)
# # 
# # ggplot(word_scores_reshaped, aes(score, fill = score_type))+
# #   geom_density(alpha= 0.5) +
# #   theme_classic()
# # 
# # #word_scores_reshaped <- factor(score_type, ordered = TRUE)
# # # working on letter frequencies graph -----
# # 
# # #ggplot(test_letters, aes(x= fct_reorder(test_letters[,1], test_letters[,2])
# # #                         +                              , y, fill= lett_fect)) + geom_col()
# # 
# # guess <- rep("not guessed", times = 26)
# # 
# # char_frequencies <- cbind(char_frequencies, guess)
# # 
# # 
# # 
# # 
# # 
# # letter_vec <-  unlist(strsplit(word_4, split = ""))
# # print(letter_vec)
# # for (i in 1:length(letter_vec)) {
# #   position <- letter_vec[i]== char_frequencies$letters
# #   char_frequencies$guess[position] <- "Guess 4"}
# # 
# # 
# # letter_vec <-  unlist(strsplit(word_3, split = ""))
# # print(letter_vec)
# # for (i in 1:length(letter_vec)) {
# #   position <- letter_vec[i]== char_frequencies$letters
# #   
# #   char_frequencies$guess[position] <- "Guess 3"}
# # 
# # 
# # letter_vec <-  unlist(strsplit(word_2, split = ""))
# # print(letter_vec)
# # for (i in 1:length(letter_vec)) {
# #   position <- letter_vec[i]== char_frequencies$letters
# #   char_frequencies$guess[position] <- "Guess 2"
# #   
# # }
# # 
# # letter_vec <-  unlist(strsplit(word_1, split = ""))
# # print(letter_vec)
# # for (i in 1:length(letter_vec)) {
# #   position <- letter_vec[i]== char_frequencies$letters
# #   char_frequencies$guess[position] <- "Guess 1"
# #   
# # }
# # 
# # 
# # ggplot(char_frequencies, aes(x= fct_reorder(char_frequencies[,1], char_frequencies[,2])
# #                              , y , fill = char_frequencies[,3])) +
# #   geom_col()+
# #   theme_classic()
# # 
# # 
# # 
# # 
# # letter_order <- fct_reorder(char_frequencies[,1], char_frequencies[,2])
# # letter_order <- fct_rev(letter_order)
# # ggplot(char_frequencies, aes(x= letter_order
# #                              , y , fill = guess)) +
# #   geom_col()+
# #   ggtitle("When Letters are Guessed") +
# #   ylab("Normalized Counts") +
# #   xlab ("Letter") +
# #   theme_classic()
# # 
