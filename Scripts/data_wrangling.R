# DATA WRANGLING

setwd("../Data")
rm(list = ls())
load("231004_WTA.RData")
load("231004_wta_final.Rdata")
wta1 <- WTA
wta2 <- wta_final
rm(WTA,wta_final)

# Library loading

if(is.element("labelled", installed.packages()[, 1])==FALSE)
	{
	install.packages("labelled","xlsx")
	}
library(labelled)
library(xlsx)
	
# Changes in variables

names(wta1)[12] <- "round_level" # The "round" variable of the DB WTA is renamed to "round_level" the equivalent of the DB WTA_final
names(wta1)[13] <- "retirement"  # The "Retirement" variable of the DB WTA changes the name to "retirement" the equivalent of the DB WTA_final
wta1$round_level <- as.character(wta1$round_level)
wta1$retirement <- as.character(wta1$retirement)

wta1$tourney_category <- rep("WTA Tour",dim(wta1)[1])
wta1$tourney_id <- rep("",dim(wta1)[1])
wta1$tourney_name <- rep("",dim(wta1)[1])
wta1$round <- rep("",dim(wta1)[1])
wta1$match_outcome <- wta1$Final_Partit
wta1$sum_age <- wta1$winner_age + wta1$loser_age
wta1$mean_age <- wta1$sum_age/2 
wta1$sex <- "Female"

aux <- names(wta2)
wta1 <- wta1[,aux]
rm(aux)

# Changes before Database mergers

# Se incorporen a les categories el terme “ Round” per ser equivalents a les categories de “round level” de la BD wta_final
wta1$round_level <- paste(wta1$round_level,"Round")

# Les variables “dif_age” i “dif_rank” es recalculan pels partits que venen de BD WTA perquè solament estaven calculats en valors absoluts
wta1$dif_rank <- wta1$winner_rank-wta1$loser_rank
wta1$dif_age <- wta1$winner_age-wta1$loser_age 

# Database Merge

wta <- rbind(wta1,wta2)
rm(wta1,wta2)

# Variable treatment "SCORE"

score <- strsplit(wta$score, split = " ")
result <- rep("X",dim(wta)[1])
for(i in 1:dim(wta)[1])
{
	splitted_string <- score[[i]]
	if (length(splitted_string)!=0)
		{
		result[i] <- splitted_string[length(splitted_string)]
	}
}

result2 <- rep("Complete",dim(wta)[1])
result2[(substr(result, 1, 1)=="0" | substr(result, 1, 1)=="1" | substr(result, 1, 1)=="2" | substr(result, 1, 1)=="3") & ( substr(result, 3, 3)=="0" | substr(result, 3, 3)=="1" | substr(result, 3, 3)=="2" | substr(result, 3, 3)=="3") & substr(result, 4, 4)!="(" ] <- "Retirement"
result2[result=="RET" | result=="RE" | result=="abandoned"] <- "Retirement"
result2[result=="DEF"] <- "Default"
result2[result=="W/O"] <- "WalkOver"
result2[result=="X"] <- "Retirement"

wta$Match_Outcome <- result2
rm(score,i,splitted_string,result,result2)

# Match files to review

setwd("../Results")
aux <- c("tourney_name","year","winner_name","loser_name","score")  

# Llista.WTA.1: Corresponds to Two matches with "Abandoned" value in the "Final Match" variable and "Complete" value in the "match_outcome" variable. They are "Retirement"

llista.wta.1 <- wta[wta$match_outcome=="Complete" & wta$Final_Partit=="Abandoned",aux]
write.csv2(llista.wta.1,"llista_WTA_1.csv",row.names=FALSE)

# Llista.WTA.2: There are 31 matches with the variable "score" empty and the variable games=0 and the value "Complete" in "Final Match". They are all matches from 2017, 2018, 2019, the matches are searched in the DB of the WTA website. most are not found and those that are found are "retired" in qualifying rounds

llista.wta.2 <- wta[wta$Match_Outcome=="Retirement" & wta$Final_Partit=="Complete",aux]
write.csv2(llista.wta.2,"llista_WTA_2.csv",row.names=FALSE)

# Llista.WTA.3: Corresponds to 19 matches with an incomplete score and "Complete" value in "Final Match" and "match_outcome". They are "Retirement" 

llista.wta.3 <- wta[wta$score=="" & wta$Final_Partit=="Complete",aux]
write.csv2(llista.wta.3,"llista_WTA_3.csv",row.names=FALSE)
wta[wta$score=="" & wta$Final_Partit=="Complete",]$games <- NA
rm(aux,llista.wta.1,llista.wta.2,llista.wta.3)

wta <- wta[, -c(12, 13, 26, 27, 28)]
wta$Retirement <- (wta$Match_Outcome=="Retirement")
wta$WalkOver <- (wta$Match_Outcome=="WalkOver")
wta$Default <- (wta$Match_Outcome=="Default")

# Fusion with DB "WTA_def_090524.csv"

setwd("../Data")
wta.aux <- read.csv2("WTA_def_090524.csv",encoding="UTF-8")
aux <- c("Match_Outcome","Retirement","WalkOver","Type_W.O","Region_Injury","Source.WO","Default","Type_Def","Source.DEF")
wta.aux <- wta.aux[,aux]
wta <- wta[, -c(24 ,25 , 26, 27)]
wta <- cbind(wta,wta.aux)
wta$Retirement <- (wta$Match_Outcome=="Retirement")
wta$WalkOver <- (wta$Match_Outcome=="WalkOver")
wta$Default <- (wta$Match_Outcome=="Default")
names(wta)[27] <- "Type_WO"
names(wta)[28] <- "Region_Injury"
names(wta)[29] <- "Source_WO"
names(wta)[31] <- "Type_Default"
names(wta)[32] <- "Source_Default"
rm(aux,wta.aux)

# Labels and Levels:

wta$tourney_category <- factor(wta$tourney_category,
                               levels = c("ITF Women's World Tennis Tour", "WTA 125 Tournaments", "WTA Tour"),
                               labels = c("ITF Women's World Tennis Tour", "WTA 125 Tournaments", "WTA Tour"))
attr(wta$tourney_category,"label") <- "Tourney Category"
attr(wta$tourney_level,"label") <- "Tourney Level"
attr(wta$tourney_id,"label") <- "Tourney ID"
attr(wta$tourney_name,"label") <- "Tourney Name"
attr(wta$year,"label") <- "Year of the Tourney (1975-2019)"
wta$surface <- factor(wta$surface,
                      levels = c("Carpet","Clay","Grass","Hard"),
                      labels = c("Carpet","Clay","Grass","Hard"))
attr(wta$surface,"label") <- "Tournament playing surface"
wta$best_of <- factor(wta$best_of,
                      levels = c("3"),
                      labels = c("Best of 3 sets"))
attr(wta$best_of,"label") <- "Number of sets necessary to win the tennis match"
wta$round_level <- factor(wta$round_level,
                          levels = c("Final Round","Preliminary Round","Qualifying Round"),
                          labels = c("Final Round","Preliminary Round","Qualifying Round"))
attr(wta$round_level,"label") <- "Match round level"
attr(wta$round,"label") <- "Round Level"
attr(wta$score,"label") <- "Score of the match"
attr(wta$games,"label") <- "Number of games in the match"
attr(wta$winner_name,"label") <- "Name of the winning tennis player of the match"
attr(wta$loser_name,"label") <- "Name of the losing tennis player of the match"
wta$winner_hand <- factor(wta$winner_hand,
                          levels = c("L","R","U"),
                          labels = c("Left","Right","Unknown"))
attr(wta$winner_hand,"label") <- "Hand of the winning tennis player of the match"
wta$loser_hand <- factor(wta$loser_hand,
                         levels = c("L","R","U"),
                         labels = c("Left","Right","Unknown"))
attr(wta$loser_hand,"label") <- "Hand of the losing tennis player of the match"
attr(wta$winner_age,"label") <- "Age of the winning tennis player of the match"
attr(wta$loser_age,"label") <- "Age of the losing tennis player of the match"
attr(wta$dif_age,"label") <- "Age difference between the winning and losing tennis players"
attr(wta$sum_age,"label") <- "Sum of the ages of tennis players"
attr(wta$mean_age,"label") <- "Mean of the ages of tennis players"
attr(wta$winner_rank,"label") <- "Ranking position of the winning tennis player of the match"
attr(wta$loser_rank,"label") <- "Ranking position of the losing tennis player of the match"
attr(wta$dif_rank,"label") <- "Difference in ranking positions between the winning and the losing tennis player"
wta$Match_Outcome <- factor(wta$Match_Outcome,
                         levels = c("Complete","Default","Retirement","WalkOver"),
                         labels = c("Complete","Default","Retirement","WalkOver"))
attr(wta$Match_Outcome,"label") <- "How the tennis match ends"
wta$Retirement <- factor(wta$Retirement,
                         levels = c("FALSE","TRUE"),
                         labels = c("No Retirement","Retirement"))
attr(wta$Retirement,"label") <- "The tennis match ended due to the retirement of a tennis player"
wta$WalkOver <- factor(wta$WalkOver,
                       levels = c("FALSE","TRUE"),
                       labels = c("No WalkOver","WalkOver"))
attr(wta$WalkOver,"label") <- "The tennis match was not played due to walkover"
wta$Type_WO <- factor(wta$Type_WO,
                      levels = c("Illness","Injury","Other","Unknown",""),
                      labels = c("Illness","Injury","Other","Unknown",""))
attr(wta$Type_WO,"label") <- "Type of WalkOver"
wta$Region_Injury <- factor(wta$Region_Injury,
                     levels = c("Head/Neck","Lower Limbs","Upper Limbs","Trunk","Other","Unknown",""),
                     labels = c("Head/Neck","Lower Limbs","Upper Limbs","Trunk","Other","Unknown",""))
attr(wta$Region_Injury,"label") <- "Region injury"
attr(wta$Source_WO,"label") <- "Source of WalkOver information"
wta$Default <- factor(wta$Default,
                      levels = c("FALSE","TRUE"),
                      labels = c("No Default","Default"))
attr(wta$Default,"label") <- "The tennis match ended due to the default of a tennis player"
wta$Type_Default <- factor(wta$Type_Default,
                    levels = c("Abuse of Ball","Abuse of Racquet","Leaving the Court","Verbal Abuse","Unsportsmanlike Conduct","Unknown",""),
                    labels = c("Abuse of Ball","Abuse of Racquet","Leaving the Court","Verbal Abuse","Unsportsmanlike Conduct","Unknown",""))
attr(wta$Type_Default,"label") <- "Type of Default"
attr(wta$Source_Default,"label") <- "Source of Default information"

# Save .RData

save.image("WTA.RData")

# Save .xlsx

write.xlsx(wta, "WTA_def.xlsx", row.names = FALSE)