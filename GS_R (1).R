# Load necessary packages #
library(psych)
library(afex)
library(data.table)
library(rcompanion)

# testing

# DATA FORMATTING (turning variables into factors for further analysis) #
GS_Data$condition <- as.factor(GS_Data$condition)
GS_Data$`Same location` <- as.factor(GS_Data$`Same location`)
GS_Data$Game <- as.factor(GS_Data$Game)
GS_Data$`E's location` <- as.factor(GS_Data$`E's location`)
GS_Data$`Child's location` <- as.factor(GS_Data$`Child's location`)
GS_Data$`Same location` <- as.factor(GS_Data$`Same location`)
GS_Data$Placement <- as.factor(GS_Data$Placement)


# PLACEMENT OF TOY AT ANY LOCATION #
# (We exclude trials in which participants don't place the toy anywhere, so this analysis has to come before trial exclusion) #
# First, construct the glmm with the full random effects structure #
placementmodel <- mixed(Placement ~ condition + (1+condition|id) + (1+condition|trial_num), data = GS_Data, family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)), method = 'LRT', check_contrasts=FALSE)
# It tells us that there is singularity in the model, and Singmann & Kellen (2019) suggest iteratively removing the most complex terms until we arrive at a model that doesn't face this problem #
# Remove random slope of trial number #
placementmodel1 <- mixed(Placement ~ condition + (1+condition|id) + (1|trial_num), data = GS_Data, family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)), method = 'LRT', check_contrasts=FALSE)
# Still singularity, so now we remove random slope of participant #
placementmodel2 <- mixed(Placement ~ condition + (1|id) + (1|trial_num), data = GS_Data, family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)), method = 'LRT', check_contrasts=FALSE)
# placementmodel2 does not face convergence errors #

placementmodel2
# This shows that the full model is NOT a better fit than the model without 'condition' #
# We find no evidence supporting the hypothesis that participants are more likely to place the toy anywhere in one condition compared to the other #
# (The function 'mixed' compares the full model to a model that is identical except for the removal of 'condition', using an anova) #


# TRIAL EXCLUSION #
# How many trials are to be dropped? #
describe.by(GS_Data, GS_Data$drop)
# Of 272 test trials, 44 are to be dropped (18 in the experimental condition) #
# Remove trials in which 'drop' = 'yes' #
GS_Data <- GS_Data[which(GS_Data$drop=='no'), ]


# SUMMARY (first the full dataset, then by condition) #
summary(GS_Data)
# Number of trials for each game is similar: (54 housecar, 59 housetrain, 53 towerblock, 62 tubeball) #
# Same for E's location: 75 left, 77 middle, 76 right #
# Child's location may be skewed to the left: 92 left, 61 middle, 75 right #
describe.by(GS_Data, GS_Data$condition)
# Children place the toy in the same location as E in 49% of control trials, and 67% of experimental trials #


# PRIMARY ANALYSIS; SAME-LOCATION PLACEMENTS #
# First, construct the full glmm model and see if we face any estimation problems #
fullmodel <- mixed(`Same location` ~ condition + (1+condition|id) + (1+condition|trial_num), data = GS_Data, family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)), method = 'LRT', check_contrasts=FALSE)
# The model faces singularity, so we iteratively remove the most complex terms until we arrive a model that does not face this problem #
# First, remove random slope of trial number #
fullmodel1 <- mixed(`Same location` ~ condition + (1+condition|id) + (1|trial_num), data = GS_Data, family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)), method = 'LRT', check_contrasts=FALSE)
# Still singularity, so now we remove random slope of participant #
fullmodel2 <- mixed(`Same location` ~ condition + (1|id) + (1|trial_num), data = GS_Data, family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)), method = 'LRT', check_contrasts=FALSE)
# fullmodel2 does not face convergence errors #

fullmodel2
# This shows that the full model is a better fit than the model without 'condition' #
# Now we need the odds ratio (our effect size), which is just the exponent of the fixed effect estimate #
summary(fullmodel2)
# This shows that the fixed effect estimate is 0.86240, so OR = exponent(.8624) #
exp(0.8624)


# COMPARISON TO CHANCE #
# We need to turn the 'Same location' variable back into a numeric data format, so that we can calculate a mean per participant #
GS_Data$`Same location` <- as.numeric(GS_Data$`Same location`)
# (Turning factor data into numeric data sets the values to '1' and '2', instead of '0' and '1', so subtract 1 from all 'same location' cells) #
GS_Data$`Same location` <- GS_Data$`Same location` - 1

# We need to calculate each participant's 'same location' rate per condition #
# First, form a dataset that is just the experimental condition #
ExpCond <- GS_Data[which(GS_Data$condition=='exp'), ]
# Then we form a new dataset that is just participant id and each participant's 'same location' rate #
ExpCondAverage <- data.table(id=ExpCond$id, same_location = ExpCond$`Same location`)
ExpCondAverage <- ExpCondAverage[, (same_location.Sum=mean(same_location)), by = id]
# Then we add the variable names for our new dataset #
names(ExpCondAverage) <- c("id", 'same_location')

# Now we do all of this for the control condition #
CtrlCond <- GS_Data[which(GS_Data$condition=='ctrl'), ]
CtrlCondAverage <- data.table(id=CtrlCond$id, same_location = CtrlCond$`Same location`)
CtrlCondAverage <- CtrlCondAverage[, (same_location.Sum=mean(same_location)), by = id]
names(CtrlCondAverage) <- c("id", 'same_location')

# We can now do our analysis #
# First,the wilcox.test for each condition, where our alternative hypothesis is that the rate of 'same location' is above chance #
wilcox.test(ExpCondAverage$same_location, mu = 0.33, alternative = "greater", exact = FALSE)
wilcox.test(CtrlCondAverage$same_location, mu = 0.33, alternative = "greater", exact=FALSE)
# We also need to report the median 'same location' rate per condition, as well as effect size 'r' #
median(ExpCondAverage$same_location)
median(CtrlCondAverage$same_location)
wilcoxonOneSampleR(ExpCondAverage$same_location, mu = 0.33, alternative = "greater")
wilcoxonOneSampleR(CtrlCondAverage$same_location, mu = 0.33, alternative = "greater")
