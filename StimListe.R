## Alan Nielsen
## June 25, 2017
## Stimlist Creation for Nielsen, Ota, & Dingemanse
## This script sets up trials for the sampled within-subjects design

require(reshape2)
require(plyr)
require(magrittr)
require(lme4)
require(dplyr)
require(tidyr)
require(broom)
require(lattice)
require(doBy)
require(stringr)

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

mainPath <- 'D:/Google Drive/Experiments/Collaborations/MitsMark/'

setwd(file.path(mainPath, "data"))


###########################################################################################


RI <- read.csv("RedupIdeo.csv")           # Read in the Reduplicative ideophones 
NRI <- read.csv("NonRedupIdeo.csv")       # Read in the Non-Reduplicative ideophones
NAdj <- read.csv("NormAdjs.csv")          # Read in normal adjectives


################################################################################################
## Write a all of the functions

#################################################################################################

# This function is supposed to do the sampling - once for every seednum, and return the sample

createsample <- function(seedval) {              
  
  set.seed(seedval)   # Setting the seed is used because we are yoking participants to each other- e.g. we might have 20 different samples and 25 participants using
  # the same seed 
  
  RI <- RI[sample(nrow(RI)),]                         # Scrambles the order of the Reduplicated Ideophones 
  RISplit <- split(RI, rep(1:4, each= 6))             # Split the dataframe into two equal parts
  RIWords <- RISplit$'1'                              # use half of the RI as RI
  SRIWords <- RISplit$'2'                             # use half of the RI as SRI (split them below)
  SRIWords %<>%  mutate(Japanese = str_sub(Japanese, start=nchar(as.character(Japanese))/2 + 1))  # Splits the ideophones
  
  NRI <- NRI[sample(nrow(NRI)),]                       # Scrambles the order of the Non-Reduplicated Ideophones
  NRISplit <- split(NRI, rep(1:4, each= 6))            # Split the dataframe into two equal parts
  NRIWords <- NRISplit$'1'                                  # use half of the NRI as NRI
  RNRIWords <- NRISplit$'2'                                 # use half of the NRI as RNRI (reduplicated below)
  RNRIWords$Word2 <- RNRIWords$Japanese
  RNRIWords <- unite(RNRIWords, Japanese, Japanese, Word2, sep = "", remove = TRUE) 
  
  NAdj <- NAdj[sample(nrow(NAdj)),]                    # Scrambles the order of the Normal Adjectives
  NAdjSplit <- split(NAdj, rep(1:4, each= 6))         # Split the dataframe into two equal parts
  NAdjWords <- NAdjSplit$'1'                                # use half of the NA as NA
  RNAdjWords <- NAdjSplit$'2'                               # use half of the NA as RNA (reduplicated below)
  RNAdjWords$Word2 <- RNAdjWords$Japanese
  RNAdjWords <- unite(RNAdjWords, Japanese, Japanese, Word2, sep = "", remove = TRUE) 
  
  RIWords$WordType <- "RI"      # add a wordtype column to all of them showing their wordtype
  SRIWords$WordType <- "SRI"
  NRIWords$WordType <- "NRI"
  RNRIWords$WordType <- "RNRI"
  NAdjWords$WordType <- "NAdj"
  RNAdjWords$WordType <- "RNAdj"
  
  sampleX <- rbind(RIWords, SRIWords, NRIWords, RNRIWords, NAdjWords, RNAdjWords)
  names(sampleX)[names(sampleX) == 'Japanese'] <- 'Word'                    # Change some names
  names(sampleX)[names(sampleX) == 'Engl'] <- 'TrueMeaning'              # Change some names
  sampleX <- subset(sampleX, select = c(Word, TrueMeaning, WordType))       # Select only the columns we need

  return(sampleX)
}


##################################################################

createtraining <- function(sampleY) {
  
  ####################################################################
  ## Set up Training trials
  rm(.Random.seed, envir=globalenv())           # this script resets the seed to something random
  
  Training_FullWithin <- sampleY       # Stick all the sampled word types together into a list
  TrainingFullWithin1 <- Training_FullWithin[sample(nrow(Training_FullWithin)),]                    # Scramble the list
  TrainingFullWithin2 <- Training_FullWithin[sample(nrow(Training_FullWithin)),]                    # Scramble the list
  Training <- rbind(TrainingFullWithin1, TrainingFullWithin2)                            # Stick the two lists together to make a training list (in training all words are seen twice in two blocks)
  
  return(Training)
  
}


createtesting <- function(sampleY) {
  
  ## Set up Testing trials  
  
  rm(.Random.seed, envir=globalenv())           # this script resets the seed to something random
  
  TestingCorrect <- sampleY                                                             # Stick the correct meanings into a list
  TestingCorrect$TrialType <- "T"
  TestingCorrect$TargetMeaning <- TestingCorrect$TrueMeaning
  TestingCorrect$LegalTrial <- "Y"                                                      # notes that all target trials are legal trials
  
  
  TestingIncorrect <- transform(sampleY, TargetMeaning = sample(TrueMeaning))           # Create a second list where meanings are randomly assigned
  TestingIncorrect$TrialType <- "D"
  TestingIncorrect$LegalTrial <- ifelse(TestingIncorrect$TargetMeaning == TestingIncorrect$TrueMeaning, "N", "Y")      # puts in a column that tells you if the trial is legal
  
  Testing <- rbind(TestingCorrect, TestingIncorrect)
  Testing <- Testing[sample(nrow(Testing)),]
  
  TestCase <- length(unique(Testing$LegalTrial))  # checks what the values of the whole columns LegalTrial are - if they are all legal then this will just return 1,
                                                  # if not it will return 2
  
  if(TestCase == 1) {
    return(Testing)                       # if there are only legal trials, return Testing to the function
  } else {
    createtesting(sampleY)                # if there are illegal trials, then re-call the function
  }
  
  
}



############################################################################################################################
## Here is the function that actually writes out all of the csvs etc.
# Don't worry about the warnings that get kicked - they are just from the split function and are working correctly

writestimuli <- function(seeds, perseed) {  
  # do n times, storing the current iteration number in 'i'
  for (i in 1:seeds) {
    seednum <- i
    
    seedval <- sample(1:10000,1)
    
    sampleY <- createsample(seedval)
    
    for (j in 1:perseed) {
      
      training <- createtraining(sampleY)
      training <- cbind(Id=paste(seednum, j, sep="-"), training)
      write.table(training, paste(seednum, "-", j, "-Training", ".csv", sep = ''),
                  sep = "\t", row.names = FALSE)
      
      testing <- createtesting(sampleY)
      testing <- cbind(Id=paste(seednum, j, sep="-"), testing)
      write.table(testing, paste(seednum, "-", j, "-Testing", ".csv", sep = ''),
                  sep = "\t", row.names = FALSE)
    }
  }
}



#################################################################################################################################
# Now we write a function that calls writestimuli - we give this function a target standard deviation for how similar the counts of words being 
# sampled has to be, and it will keep calling writestimuli until it settles on a list that is well sampled enough

setwd(file.path(mainPath, "data/stimlists"))



#### I want to feed this function a targetSD, and a maximum number of tries to get to that targetSD (so it doesn't just hang the computer forever)
# I tried to implement this a few different ways but I just can't get it to work properly

# SelectBalanced <- function(TargetSD, MaxTries, TryNum) { 
#   
#   writestimuli(25,20)
#   
#   files  <- list.files(pattern = '\\-1-Testing.csv')          # Read one testing file from each seed into a dataframe
#   tables <- lapply(files, read.csv, header = TRUE, sep = '\t')
#   AllTrials <- do.call(rbind , tables)
# 
#   AllTrials$Word <- as.factor(AllTrials$Word)
# 
#   WordCounts <- count(AllTrials, Word)
#   
#   SD = sd(WordCounts$n)
#   
#   if(TryNum < MaxTries){
#     if(SD == 0){
#       print("Complete")
#     } else {
#         print(SD)
#         TryNum <- TryNum + 1
#         return(TryNum)
#         SelectBalanced(0, 10, TryNum)
#     }
#   } else {
#       print("Failed to Reach TargetSD")
#   }
#   
# 
# }


######## Here I try to run it without the fancy if statements- it should just run forever, right?

SelectBalanced2 <- function(MinNum, seeds, perseed) { 
  
  writestimuli(seeds,perseed)
  
  files  <- list.files(pattern = '\\-1-Testing.csv')          # Read one testing file from each seed into a dataframe
  tables <- lapply(files, read.csv, header = TRUE, sep = '\t')
  AllTrials <- do.call(rbind , tables)
  
  AllTrials$Word <- as.factor(AllTrials$Word)
  
  WordCounts <- count(AllTrials, Word)
  MaxN <- max(WordCounts$n)
  MinN <- min(WordCounts$n)
  SD <- sd(WordCounts$n)
  Diff = MaxN - MinN
  
  if(MinN >= MinNum){
      print(paste(MaxN, MinN, Diff, SD))
      print("Complete")
    }else {
      print(paste(MaxN, MinN, Diff, SD))
      SelectBalanced2(MinNum, seeds, perseed)
    }
  
}


######################
# 


SelectBalanced2(20, 25, 20)




