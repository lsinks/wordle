
# Loading libraries and data ----
#load the tidyverse
library("tidyverse")

word_list <- read.table("C:/Users/drsin/OneDrive/Documents/R Projects/wordle/sgb-words.txt")

#Functions ----
#scoring code uses the counting code from
#https://www.r-bloggers.com/2018/12/rrrrs-in-r-letter-frequency-in-r-package-names/

Scoring_Word <- function(word){
  #i'm not handling duplicate letters at all right now
  letter_vec <-  unlist(strsplit(word, split = ""))
  value <- 0
  for (i in 1:length(letter_vec)) {
    position <- letter_vec[i]== char_frequencies$letters
    value[i] <- y[position]
   # print(i)
    if (i==5) {
     # print("I am here")
     # print(sum(value))
      return(total <- sum(value))
      }
    
  }
} 
#king the frequency table ----
letters <- unlist(strsplit(word_list[,1], split = ""))
char_frequencies <- as.data.frame(table(letters))

common <- max(char_frequencies[,2])
y=(char_frequencies[,2]/common)



#word_scores_reshaped <- factor(score_type, ordered = TRUE)
# working on letter frequencies graph -----

#ggplot(test_letters, aes(x= fct_reorder(test_letters[,1], test_letters[,2])
#                         +                              , y, fill= lett_fect)) + geom_col()

#guess <- rep("not guessed", times = 26)
guess <- rep("", times = 26)
char_frequencies <- cbind(char_frequencies, guess)



Labeling_Guess <- function(input_word, label = "Not Guessed"){
  letter_vec <-  unlist(strsplit(input_word, split = ""))
  print(letter_vec)
  for (i in 1:length(letter_vec)) {
    position <- letter_vec[i]== char_frequencies$letters
    print(position)
    print(char_frequencies$guess[position])
    char_frequencies$guess[position] <- guess_label
  }
   
  }
  
 Labeling_Guess(guess_label = "guess crone", input_word = "crone")