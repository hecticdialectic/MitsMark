## June 19, 2017
## Stimulus preparation for Nielsen, Ota, & Dingemanse

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

## The types of stims being used in this experiment are as follows: (Natural types taken from languages are marked with **)

# RI = Reduplicative Ideophones **
# SRI = Split Reduplicative Ideophones
# NRI = Non-Reduplicative Ideophones **
# RNRI = Reduplicated Non-Reduplicative Ideophones
# NAdj = Normal Adjectives **
# RNAdj = Reduplicated Normal Adjectives

RI <- read.csv("RedupIdeo.csv") # Read in the Reduplicative ideophones file as a dataframe
names(RI)[names(RI) == 'Japanese'] <- 'Word'

SRI <- RI
SRI %<>%     # Split the strings in the word column in half- so kashikashi becomes kashi (the split version of the reduplicative ideophone)
  mutate(Word = str_sub(Word, start=nchar(as.character(Word))/2 + 1)) 

NRI <- read.csv("NonRedupIdeo.csv")
names(NRI)[names(NRI) == 'Japanese'] <- 'Word'

RNRI <- NRI # Copy to a new dataframe to be modified for Reduplicated Non-reduplicative ideophones
RNRI$Word2 <- RNRI$Word
RNRI <- unite(RNRI, Word, Word, Word2, sep = "", remove = TRUE) # Reduplicate the word


NAdj <- read.csv("NormAdjs.csv")              # Read in normal adjectives
names(NAdj)[names(NAdj) == 'Japanese'] <- 'Word'

RNAdj <- NAdj # Copy to a new dataframe to be modified for Reduplicated Normal Adjectives
RNAdj$Word2 <- RNAdj$Word
RNAdj <- unite(RNAdj, Word, Word, Word2, sep = '', remove= TRUE)

RI$WordType <- "RI"      # add a wordtype column to all of them showing their wordtype
SRI$WordType <- "SRI"
NRI$WordType <- "NRI"
RNRI$WordType <- "RNRI"
NAdj$WordType <- "NAdj"
RNAdj$WordType <- "RNAdj"

WordList <- rbind(RI, SRI, NRI, RNRI, NAdj, RNAdj) # Write this all into one word list

write.csv(WordList, "WordList.csv", row.names = FALSE)

rm(RI, SRI, NRI, RNRI, NAdj, RNAdj)

###################################################              Stimlist Creation Begins Here                   ########################################################

##################### So, that's just a creation of all possible words, but for actual experiments that's not how things go

RI <- read.csv("RedupIdeo.csv")                     # Read in the Reduplicative ideophones 
RI <- RI[sample(nrow(RI)),]                         # Scrambles the order of the dataframe
RIsplit <- split(RI, rep(1:2, each= 10))            # Split the dataframe into two equal parts
RI <- RIsplit$'1'                                   # use half of the RI as RI
SRI <- RIsplit$'2'                                  # use half of the RI as SRI (split them below)
SRI %<>%  mutate(Japanese = str_sub(Japanese, start=nchar(as.character(Japanese))/2 + 1)) 


NRI <- read.csv("NonRedupIdeo.csv")                  # Read in the Non-Reduplicative ideophones
NRI <- NRI[sample(nrow(NRI)),]                       # Scrambles the order of the dataframe
NRIsplit <- split(NRI, rep(1:2, each= 10))           # Split the dataframe into two equal parts
NRI <- NRIsplit$'1'                                  # use half of the NRI as NRI
RNRI <- NRIsplit$'2'                                 # use half of the NRI as RNRI (reduplicated below)
RNRI$Word2 <- RNRI$Japanese
RNRI <- unite(RNRI, Japanese, Japanese, Word2, sep = "", remove = TRUE) 


NAdj <- read.csv("NormAdjs.csv")                     # Read in normal adjectives
NAdj <- NAdj[sample(nrow(NAdj)),]                    # Scrambles the order of the dataframe
NAdjsplit <- split(NAdj, rep(1:2, each= 10))         # Split the dataframe into two equal parts
NAdj <- NAdjsplit$'1'                                # use half of the NA as NA
RNAdj <- NAdjsplit$'2'                               # use half of the NA as RNA (reduplicated below)
RNAdj$Word2 <- RNAdj$Japanese
RNAdj <- unite(RNAdj, Japanese, Japanese, Word2, sep = "", remove = TRUE) 

RI$WordType <- "RI"      # add a wordtype column to all of them showing their wordtype
SRI$WordType <- "SRI"
NRI$WordType <- "NRI"
RNRI$WordType <- "RNRI"
NAdj$WordType <- "NAdj"
RNAdj$WordType <- "RNAdj"

############################################################# Some Different Experimental Designs ####################################################

############################################################   1 - Full Between Subjects Design

### Read in the files
RI <- read.csv("RedupIdeo.csv") # Read in the Reduplicative ideophones file as a dataframe
names(RI)[names(RI) == 'Japanese'] <- 'Word'
SRI <- RI
SRI %<>%     # Split the strings in the word column in half- so kashikashi becomes kashi (the split version of the reduplicative ideophone)
  mutate(Word = str_sub(Word, start=nchar(as.character(Word))/2 + 1)) 
NRI <- read.csv("NonRedupIdeo.csv")
names(NRI)[names(NRI) == 'Japanese'] <- 'Word'
RNRI <- NRI # Copy to a new dataframe to be modified for Reduplicated Non-reduplicative ideophones
RNRI$Word2 <- RNRI$Word
RNRI <- unite(RNRI, Word, Word, Word2, sep = "", remove = TRUE) # Reduplicate the word
NAdj <- read.csv("NormAdjs.csv")              # Read in normal adjectives
names(NAdj)[names(NAdj) == 'Japanese'] <- 'Word'
RNAdj <- NAdj # Copy to a new dataframe to be modified for Reduplicated Normal Adjectives
RNAdj$Word2 <- RNAdj$Word
RNAdj <- unite(RNAdj, Word, Word, Word2, sep = '', remove= TRUE)
RI$WordType <- "RI"      # add a wordtype column to all of them showing their wordtype
SRI$WordType <- "SRI"
NRI$WordType <- "NRI"
RNRI$WordType <- "RNRI"
NAdj$WordType <- "NAdj"
RNAdj$WordType <- "RNAdj"


####################### Output sample Files

SelectedStims <- RI[sample(nrow(RI)), ]
Training <- rbind(SelectedStims[sample(nrow(SelectedStims)),], SelectedStims[sample(nrow(SelectedStims)),])
Training <- subset(Training, select = c(Word, Engl, Dutch, WordType))
write.csv(Training, "Training_FullBetween_.csv")

TestingCorrect <- SelectedStims[sample(nrow(SelectedStims)),]
TestingCorrect$Correct <- "Y"
TestingIncorrect <- transform(SelectedStims, Word = sample(Word))
TestingIncorrect$Correct <- "N"
Testing <- rbind(TestingCorrect, TestingIncorrect)
Testing<- subset(Testing, select = c(Word, Engl, Dutch, WordType, Correct))
Testing <- Testing[sample(nrow(Testing)),]
write.csv(Testing, "Testing_FullBetween_.csv")



# Cleanup
rm(RI, SRI, NRI, RNRI, NAdj, RNAdj)
rm(NAdjsplit, NRIsplit, RIsplit)
rm (Testing, TestingIncorrect, TestingCorrect, Training, Training2, Training1)


############################################################   2 - Full Within Subjects Design

## Read in the files

RI <- read.csv("RedupIdeo.csv")                     # Read in the Reduplicative ideophones 
RI <- RI[sample(nrow(RI)),]                         # Scrambles the order of the dataframe
RIsplit <- split(RI, rep(1:2, each= 10))            # Split the dataframe into two equal parts
RI <- RIsplit$'1'                                   # use half of the RI as RI
SRI <- RIsplit$'2'                                  # use half of the RI as SRI (split them below)
SRI %<>%  mutate(Japanese = str_sub(Japanese, start=nchar(as.character(Japanese))/2 + 1)) 


NRI <- read.csv("NonRedupIdeo.csv")                  # Read in the Non-Reduplicative ideophones
NRI <- NRI[sample(nrow(NRI)),]                       # Scrambles the order of the dataframe
NRIsplit <- split(NRI, rep(1:2, each= 10))           # Split the dataframe into two equal parts
NRI <- NRIsplit$'1'                                  # use half of the NRI as NRI
RNRI <- NRIsplit$'2'                                 # use half of the NRI as RNRI (reduplicated below)
RNRI$Word2 <- RNRI$Japanese
RNRI <- unite(RNRI, Japanese, Japanese, Word2, sep = "", remove = TRUE) 


NAdj <- read.csv("NormAdjs.csv")                     # Read in normal adjectives
NAdj <- NAdj[sample(nrow(NAdj)),]                    # Scrambles the order of the dataframe
NAdjsplit <- split(NAdj, rep(1:2, each= 10))         # Split the dataframe into two equal parts
NAdj <- NAdjsplit$'1'                                # use half of the NA as NA
RNAdj <- NAdjsplit$'2'                               # use half of the NA as RNA (reduplicated below)
RNAdj$Word2 <- RNAdj$Japanese
RNAdj <- unite(RNAdj, Japanese, Japanese, Word2, sep = "", remove = TRUE) 

RI$WordType <- "RI"      # add a wordtype column to all of them showing their wordtype
SRI$WordType <- "SRI"
NRI$WordType <- "NRI"
RNRI$WordType <- "RNRI"
NAdj$WordType <- "NAdj"
RNAdj$WordType <- "RNAdj"

####################### Output sample Files

Training_FullWithin <- rbind(RI, SRI, NRI, RNRI, NAdj, RNAdj)
names(Training_FullWithin)[names(Training_FullWithin) == 'Japanese'] <- 'Word'
Training_FullWithin <- subset(Training_FullWithin, select = c(Word, Engl, Dutch, WordType))
TrainingFullWithin1 <- Training_FullWithin[sample(nrow(Training_FullWithin)),]
TrainingFullWithin2 <- Training_FullWithin[sample(nrow(Training_FullWithin)),]
Training_FullWithin <- rbind(TrainingFullWithin1, TrainingFullWithin2)
write.csv(Training_FullWithin, "Training_FullWithin.csv")

TestingCorrect <- TrainingFullWithin1
TestingCorrect$Correct <- "Y"
TestingIncorrect <- transform(TrainingFullWithin2, Word = sample(Word))
TestingIncorrect$Correct <- "N"
Testing_FullWithin <- rbind(TestingCorrect, TestingIncorrect)
Testing_FullWithin <- Testing_FullWithin[sample(nrow(Testing_FullWithin)),]
write.csv(Testing_FullWithin, "Testing_FullWithin.csv")

# Tidy up the Space
rm(RI, SRI, NRI, RNRI, NAdj, RNAdj)
rm(NAdjsplit, NRIsplit, RIsplit)
rm (Testing_FullWithin, TestingIncorrect, TestingCorrect, Training_FullWithin, TrainingFullWithin2, TrainingFullWithin1)


############################################################   3 - Sampled Within Subjects Design

RI <- read.csv("RedupIdeo.csv")                     # Read in the Reduplicative ideophones 
RI <- RI[sample(nrow(RI)),]                         # Scrambles the order of the dataframe
RIsplit <- split(RI, rep(1:4, each= 6))            # Split the dataframe into two equal parts
RI <- RIsplit$'1'                                   # use half of the RI as RI
SRI <- RIsplit$'2'                                  # use half of the RI as SRI (split them below)
SRI %<>%  mutate(Japanese = str_sub(Japanese, start=nchar(as.character(Japanese))/2 + 1)) 


NRI <- read.csv("NonRedupIdeo.csv")                  # Read in the Non-Reduplicative ideophones
NRI <- NRI[sample(nrow(NRI)),]                       # Scrambles the order of the dataframe
NRIsplit <- split(NRI, rep(1:4, each= 6))           # Split the dataframe into two equal parts
NRI <- NRIsplit$'1'                                  # use half of the NRI as NRI
RNRI <- NRIsplit$'2'                                 # use half of the NRI as RNRI (reduplicated below)
RNRI$Word2 <- RNRI$Japanese
RNRI <- unite(RNRI, Japanese, Japanese, Word2, sep = "", remove = TRUE) 


NAdj <- read.csv("NormAdjs.csv")                     # Read in normal adjectives
NAdj <- NAdj[sample(nrow(NAdj)),]                    # Scrambles the order of the dataframe
NAdjsplit <- split(NAdj, rep(1:4, each= 6))         # Split the dataframe into two equal parts
NAdj <- NAdjsplit$'1'                                # use half of the NA as NA
RNAdj <- NAdjsplit$'2'                               # use half of the NA as RNA (reduplicated below)
RNAdj$Word2 <- RNAdj$Japanese
RNAdj <- unite(RNAdj, Japanese, Japanese, Word2, sep = "", remove = TRUE) 

RI$WordType <- "RI"      # add a wordtype column to all of them showing their wordtype
SRI$WordType <- "SRI"
NRI$WordType <- "NRI"
RNRI$WordType <- "RNRI"
NAdj$WordType <- "NAdj"
RNAdj$WordType <- "RNAdj"

####################### Output sample Files

Training_FullWithin <- rbind(RI, SRI, NRI, RNRI, NAdj, RNAdj)
names(Training_FullWithin)[names(Training_FullWithin) == 'Japanese'] <- 'Word'
Training_FullWithin <- subset(Training_FullWithin, select = c(Word, Engl, Dutch, WordType))
TrainingFullWithin1 <- Training_FullWithin[sample(nrow(Training_FullWithin)),]
TrainingFullWithin2 <- Training_FullWithin[sample(nrow(Training_FullWithin)),]
Training_FullWithin <- rbind(TrainingFullWithin1, TrainingFullWithin2)
write.csv(Training_FullWithin, "Training_SampledWithin.csv")

TestingCorrect <- TrainingFullWithin1
TestingCorrect$Correct <- "Y"
TestingIncorrect <- transform(TrainingFullWithin2, Word = sample(Word))
TestingIncorrect$Correct <- "N"
Testing_FullWithin <- rbind(TestingCorrect, TestingIncorrect)
Testing_FullWithin <- Testing_FullWithin[sample(nrow(Testing_FullWithin)),]
write.csv(Testing_FullWithin, "Testing_SampledWithin.csv")

# Tidy up the Space
rm(RI, SRI, NRI, RNRI, NAdj, RNAdj)
rm(NAdjsplit, NRIsplit, RIsplit)
rm (Testing_FullWithin, TestingIncorrect, TestingCorrect, Training_FullWithin, TrainingFullWithin2, TrainingFullWithin1)


############################################################   4 - Crossed Design

# 4) Crossed Design
# Instead of limiting the number of meanings of each type in a within-subjects design, the within-subjects bit could be which Word Types each participant had to learn- i.e. a given participant could learn 10 meanings from each of the 4 word types, for at total of 40 meanings (again close to 38)
# From 6 Word Types there are 15 ways to pick 4 types- so there would be 15 "subconditions), which hopefully wouldn't turn out to matter

RI <- read.csv("RedupIdeo.csv")                     # Read in the Reduplicative ideophones 
RI <- RI[sample(nrow(RI)),]                         # Scrambles the order of the dataframe
RIsplit <- split(RI, rep(1:2, each= 10))            # Split the dataframe into two equal parts
RI <- RIsplit$'1'                                   # use half of the RI as RI
SRI <- RIsplit$'2'                                  # use half of the RI as SRI (split them below)
SRI %<>%  mutate(Japanese = str_sub(Japanese, start=nchar(as.character(Japanese))/2 + 1)) 


NRI <- read.csv("NonRedupIdeo.csv")                  # Read in the Non-Reduplicative ideophones
NRI <- NRI[sample(nrow(NRI)),]                       # Scrambles the order of the dataframe
NRIsplit <- split(NRI, rep(1:2, each= 10))           # Split the dataframe into two equal parts
NRI <- NRIsplit$'1'                                  # use half of the NRI as NRI
RNRI <- NRIsplit$'2'                                 # use half of the NRI as RNRI (reduplicated below)
RNRI$Word2 <- RNRI$Japanese
RNRI <- unite(RNRI, Japanese, Japanese, Word2, sep = "", remove = TRUE) 


NAdj <- read.csv("NormAdjs.csv")                     # Read in normal adjectives
NAdj <- NAdj[sample(nrow(NAdj)),]                    # Scrambles the order of the dataframe
NAdjsplit <- split(NAdj, rep(1:2, each= 10))         # Split the dataframe into two equal parts
NAdj <- NAdjsplit$'1'                                # use half of the NA as NA
RNAdj <- NAdjsplit$'2'                               # use half of the NA as RNA (reduplicated below)
RNAdj$Word2 <- RNAdj$Japanese
RNAdj <- unite(RNAdj, Japanese, Japanese, Word2, sep = "", remove = TRUE) 

RI$WordType <- "RI"      # add a wordtype column to all of them showing their wordtype
SRI$WordType <- "SRI"
NRI$WordType <- "NRI"
RNRI$WordType <- "RNRI"
NAdj$WordType <- "NAdj"
RNAdj$WordType <- "RNAdj"

####################### Output sample Files
# Possible Combinations:
# RI, SRI, NRI, RNRI
# RI, SRI, NRI, NAdj
# RI, SRI, NRI, RNAdj
# RI, SRI, RNRI, NAdj
# RI, SRI, RNRI, RNAdj
# RI, SRI, NAdj, RNAdj
# RI, NRI, RNRI, NAdj
# RI, NRI, RNRI, RNAdj
# RI, NRI, NAdj, RNAdj
# RI, RNRI, NAdj, RNAdj
# SRI, NRI, RNRI, NAdj
# SRI, NRI, RNRI, RNAdj
# SRI, NRI, NAdj, RNAdj
# SRI, RNRI, NAdj, RNAdj
# NRI, RNRI, NAdj, RNAdj

Training <- rbind(NRI, RNRI, NAdj, RNAdj)
names(Training)[names(Training) == 'Japanese'] <- 'Word'
Training <- subset(Training, select = c(Word, Engl, Dutch, WordType))
Training1 <- Training[sample(nrow(Training)),]
Training2 <- Training[sample(nrow(Training)),]
Training <- rbind(Training1, Training2)
write.csv(Training, "Training_Crossed.csv")

TestingCorrect <- Training1
TestingCorrect$Correct <- "Y"
TestingIncorrect <- transform(Training2, Word = sample(Word))
TestingIncorrect$Correct <- "N"
Testing <- rbind(TestingCorrect, TestingIncorrect)
Testing <- Testing[sample(nrow(Testing)),]
write.csv(Testing, "Testing_Crossed.csv")

# Cleanup
rm(RI, SRI, NRI, RNRI, NAdj, RNAdj)
rm(NAdjsplit, NRIsplit, RIsplit)
rm (Testing, TestingIncorrect, TestingCorrect, Training, Training2, Training1)

############################################################   5 - Modified Crossed Design

## as above but possible combinations:

# RI, SRI, NRI, RNRI
# RI, SRI, NRI, RNAdj
# RI, SRI, RNRI, NAdj
# RI, SRI, NAdj, RNAdj
# RI, NRI, RNRI, NAdj
# RI, NRI, NAdj, RNAdj
# SRI, NRI, RNRI, RNAdj
# SRI, RNRI, NAdj, RNAdj
# NRI, RNRI, NAdj, RNAdj

############################################################   7 - Limited Between Subjects Design

### Read in the files
RI <- read.csv("RedupIdeo.csv") # Read in the Reduplicative ideophones file as a dataframe
names(RI)[names(RI) == 'Japanese'] <- 'Word'
SRI <- RI
SRI %<>%     # Split the strings in the word column in half- so kashikashi becomes kashi (the split version of the reduplicative ideophone)
  mutate(Word = str_sub(Word, start=nchar(as.character(Word))/2 + 1)) 
NRI <- read.csv("NonRedupIdeo.csv")
names(NRI)[names(NRI) == 'Japanese'] <- 'Word'
RNRI <- NRI # Copy to a new dataframe to be modified for Reduplicated Non-reduplicative ideophones
RNRI$Word2 <- RNRI$Word
RNRI <- unite(RNRI, Word, Word, Word2, sep = "", remove = TRUE) # Reduplicate the word
NAdj <- read.csv("NormAdjs.csv")              # Read in normal adjectives
names(NAdj)[names(NAdj) == 'Japanese'] <- 'Word'
RNAdj <- NAdj # Copy to a new dataframe to be modified for Reduplicated Normal Adjectives
RNAdj$Word2 <- RNAdj$Word
RNAdj <- unite(RNAdj, Word, Word, Word2, sep = '', remove= TRUE)
RI$WordType <- "RI"      # add a wordtype column to all of them showing their wordtype
SRI$WordType <- "SRI"
NRI$WordType <- "NRI"
RNRI$WordType <- "RNRI"
NAdj$WordType <- "NAdj"
RNAdj$WordType <- "RNAdj"

####################### Output sample Files

SelectedStims <- RNAdj[sample(nrow(RNAdj),10), ]
Training <- rbind(SelectedStims[sample(nrow(SelectedStims)),], SelectedStims[sample(nrow(SelectedStims)),])
Training <- subset(Training, select = c(Word, Engl, Dutch, WordType))
write.csv(Training, "Training_LimBetween_.csv")

TestingCorrect <- SelectedStims[sample(nrow(SelectedStims)),]
TestingCorrect$Correct <- "Y"
TestingIncorrect <- transform(SelectedStims, Word = sample(Word))
TestingIncorrect$Correct <- "N"
Testing <- rbind(TestingCorrect, TestingIncorrect)
Testing<- subset(Testing, select = c(Word, Engl, Dutch, WordType, Correct))
Testing <- Testing[sample(nrow(Testing)),]
write.csv(Testing, "Testing_LimBetween_.csv")



# Cleanup
rm(RI, SRI, NRI, RNRI, NAdj, RNAdj)
rm(NAdjsplit, NRIsplit, RIsplit)
rm (Testing, TestingIncorrect, TestingCorrect, Training, Training2, Training1)



