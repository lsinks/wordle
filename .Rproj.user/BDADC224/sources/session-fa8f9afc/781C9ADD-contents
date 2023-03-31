
# Loading libraries and data ----
#load the tidyverse
library("tidyverse")

sgb.words <- read.table("C:/Users/drsin/OneDrive/Documents/R Projects/wordle/sgb-words.txt")

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


Scoring_Word_Unique <- function(word){
  # This does only score on unique letters
  # print(word)
  letter_vec <-  unlist(strsplit(word, split = ""))
  unique_letter_vec <- unique(letter_vec)
  #print(unique_letter_vec)
  #print(length(unique_letter_vec))
  
  value <- 0
  if (length(unique_letter_vec)== 0) {
    return(value)
  } else{
      for (i in 1:length(unique_letter_vec)) {
          position <- unique_letter_vec[i]== char_frequencies$letters
      value[i] <- y[position]
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
  #print(chosen_word)
  char_vec <- unlist(strsplit(chosen_word, ""))
  test <- word
  for (ind in 1:num_lett) {
  test <- str_replace_all(test, char_vec[ind], "")
 #print(char_vec[ind])
  #print(test)

  }
  return(test)
}

# making the frequency table ----
letters <- unlist(strsplit(sgb.words[,1], split = ""))
char_frequencies <- as.data.frame(table(letters))
#char_frequencies

# Creating some visualizations of the frequency table ----

#raw counts
# ggplot(char_frequencies, aes(x= fct_reorder(char_frequencies[,1], char_frequencies[,2])
#                              , char_frequencies[,2] )) +
#   geom_col()+
#   theme_classic()

#normalized
common <- max(char_frequencies[,2])
y=(char_frequencies[,2]/common)

#ggplot(char_frequencies, aes(x= fct_reorder(char_frequencies[,1], char_frequencies[,2])
 #                            , y )) +
 # geom_col()+
 # theme_classic()

#calculate the score for for my standard guesses ----

#the two functions should give the same score since they are all unique letters

#crone_score <- Scoring_Word("crone")
#might_score <- Scoring_Word ("might")
#sadly_score <- Scoring_Word ("sadly")


#u_crone_score <- Scoring_Word_Unique("crone")
#u_might_score <- Scoring_Word_Unique ("might")
#U_sadly_score <- Scoring_Word ("sadly")



# Bulk of the code is here ----

num_words <- 5757
#num_words <- 5
small_list <- data.frame(word_name = sgb.words[1:num_words,1], 
                    score =rep(0, times = num_words), 
                    unique_score = rep(0, times=num_words),
                    post_word_one_unique = rep(0, times=num_words),
                    post_word_two_unique = rep(0, times=num_words),
                    post_word_three_unique = rep(0, times=num_words)
                                                )
#word <- small_list[[1,1]] #think this is trash
#Scoring_Word(word)  #this is trash


start_time <- Sys.time() #to measure how long this takes
# Calculates the initial scores for all words.-----
# This calculates the score for all words, without worrying about duplicate letts.
ind2 <- 0
for (ind2 in 1:num_words){
  #print(small_list[[ind2,1]])
  score_ind2 <- Scoring_Word(small_list[[ind2,1]])
  small_list[[ind2,2]] <- score_ind2
}


ind2 <- 0
for (ind2 in 1:num_words){
 # print(small_list[[ind2,1]])
#score_ind2 <- Scoring_Word_Unique(small_list[[ind2,1]])
 # print(score_ind2)
#  small_list[[ind2,3]] <- score_ind2
score_u_ind2 <- Scoring_Word_Unique(small_list[[ind2,1]])
small_list[[ind2,3]] <- score_u_ind2

}


end_time <- Sys.time()
end_time - start_time


# Finding the best first word
top_words <- small_list %>%
 arrange(desc(unique_score))

word_1 <- top_words$word_name[1]
ggplot(top_words, aes(unique_score))+
  geom_histogram()



#now we need a function that sees if a word has the letters of the word_1
#and removes them and then calculates the word score
#top word is their
# Word 1= arose -----
num_lett <- 5
ind3 <- 1
for (ind3 in 1:num_words) {
 # print(top_words$word_name[ind3])
  
  #char_vec <- unlist(strsplit(word_1, ""))
  #str_replace_all(test, char_vec[1], "")
  test<- small_list[[ind3,1]]
  lvec <- Removing_Letters(test, word_1, num_lett )
  #print(lvec)
  #test <- str_replace_all(test, char_vec[1], "")
  #test <- str_replace_all(test, char_vec[2], "")
  #test <-  str_replace_all(test, char_vec[3], "")
  #test <- str_replace_all(test, char_vec[4], "")
  #test <- str_replace_all(test, char_vec[5], "")
 # print(test)
  #lvec <- gsub("[a r o s e]", "", test)  #this actually works.  How do I use the string?
  #lvec <-  unlist(strsplit(word_1, split = ""))
  #lvec<- "t|h|e|i|r" #how do I contruct this automatically

  #new_let <- str_remove_all(pattern= lvec, string= test)
 # print(lvec)
  score_ind3 <- Scoring_Word_Unique(lvec)
 # print("writing score")
 # print(c(ind3, " ", score_ind3, "for the word ", test, "sent as ", lvec))
  
  small_list[[ind3,4]] <- score_ind3
  #print (c("output of small list ", top_words[[ind3,4]]))
}

top_words2 <- small_list %>%
  arrange(desc(post_word_one_unique))


word_2 <- top_words2$word_name[1]


# 
# #ggplot(top_words2, aes(post_word_one_unique))+
#   #geom_histogram()
# 
# # top word 2 is until
# 
ind4 <- 1
for (ind4 in 1:num_words) {
  # print(top_words$word_name[ind3])
  test<- small_list[[ind4,1]]
  lvec <- Removing_Letters(test, word_1, num_lett )
  lvec <- Removing_Letters(lvec, word_2, num_lett )
  #lvec <- gsub("[u n t i l a r o s e]", "", test)  #this actually works.  How do I use the string?
  #lvec <-  unlist(strsplit(word_1, split = ""))
  #lvec<- "t|h|e|i|r" #how do I contruct this automatically

  #new_let <- str_remove_all(pattern= lvec, string= test)
  # print(lvec)
  score_ind4 <- Scoring_Word_Unique(lvec)
  # print("writing score")
  # print(c(ind3, " ", score_ind3, "for the word ", test, "sent as ", lvec))


  small_list[[ind4,5]] <- score_ind4
  #print (c("output of small list ", top_words[[ind3,4]]))
}

top_words2 <- small_list %>%
  arrange(desc(post_word_two_unique))


word_3 <- top_words2$word_name[1]

small_list2 <- small_list %>%
  select(-post_word_three_unique) %>%
  select(-score)
 
small_list_reshaped <- pivot_longer(small_list2, cols = 2:4, names_to = "score_type", values_to = "score")
#levels = c("unique_score", "post_word_one_unique", "post_word_two_unique")
small_list_reshaped$score_type <- as.factor(small_list_reshaped$score_type)

#small_list_reshaped <- factor(score_type, ordered = TRUE)

ggplot(small_list_reshaped, aes(score, fill = score_type))+
  geom_density(alpha= 0.5) +
  theme_classic()

# #ggplot(top_words2, aes(post_word_two_unique))+
#   geom_histogram()
# 
# a= Scoring_Word_Unique("arose")+ Scoring_Word_Unique("until") +Scoring_Word_Unique("dumpy")
# b= Scoring_Word_Unique("crone")+ Scoring_Word_Unique("mighty") +Scoring_Word_Unique("sadly")
#  