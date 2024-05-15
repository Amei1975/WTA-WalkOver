# Installation of missing libraries

libraries <- c("labelled","SmartEDA","epitools","ggplot2","pandoc","compareGroups","epiR","car")
check.libraries <- is.element(libraries, installed.packages()[, 1])==FALSE
libraries.to.install <- libraries[check.libraries]
if (length(libraries.to.install!=0)) 
{
    install.packages(libraries.to.install)
}

# Installation of "wo" class

rm(list = ls())
source("wo_class.R")

#############
# DATA LOAD #
#############

# source("data_wrangling.R",encoding="UTF-8",echo=TRUE) 

setwd("../Data")
load("WTA.RData")
setwd("../Results")

# Variable_Table.R

library(labelled)
sink("Variable_Table.txt")
generate_dictionary(wta)
sink()


#############################
# EXPLORATORY DATA ANALYSIS #
#############################

# Library charge

library(SmartEDA)
library(epitools)
library(ggplot2)
library(pandoc)

pandoc_install()
pandoc_activate()

# Exploratory Data Analysis

aux <- c("tourney_category", "tourney_level", "year", "surface", "round_level", "games", "winner_hand", "loser_hand", "winner_age", "loser_age", "dif_age",
         "sum_age", "mean_age", "winner_rank", "loser_rank", "dif_rank", "Match_Outcome", "WalkOver") 
wta_eda <- wta[,aux]
rm(aux)

ExpReport(wta_eda, Template = NULL, Target = NULL, label = NULL, theme = "Default", op_file = "1 - Exploratory Data Analysis.html", op_dir = getwd(), sc = NULL, sn = NULL, Rc = NULL)

wo_1000(year_i=1975,year_f=2019)$wo$dd

# Descriptive Walkovers and Defaults

# Description of the causes of walkovers and defaults in matches

table(wta[wta$WalkOver=="WalkOver",]$Type_WO)
round(prop.table(table(wta[wta$WalkOver=="WalkOver",]$Type_WO))*100,1)

table(wta[wta$Default=="Default",]$Type_Default)
round(prop.table(table(wta[wta$Default=="Default",]$Type_Default))*100,1)

# Distribution of the injured region in Walkovers caused by injury.

table(wta[wta$Type_WO=="Injury",]$Region_Injury)
round(prop.table(table(wta[wta$Type_WO=="Injury",]$Region_Injury))*100,1)


# Walkover by year

# Table

sink("1.1 - Incidence of Walkovers in WTA tennis matches by year (1975-2019).txt")
wo_1000(year_i=1975,year_f=2019)$dd[,1:4]
sink()

# Plot

png(file="1.2 - Incidence of Walkovers in WTA tennis matches by year (1975-2019).png", width=800, height=600)
plot.wo(year_i=1975,year_f=2019,title_wo="Incidence of Walkovers in WTA tennis matches by year (1975-2019)")
dev.off()


#######################
# BIVARIATE ANALYSIS  #
#######################

# Library charge

library(compareGroups)
library(ggplot2)

aux <- c("tourney_category","year","surface","round_level","winner_hand","loser_hand","winner_age","loser_age","dif_age","mean_age","winner_rank","loser_rank","dif_rank","WalkOver") 
wta_ba <- wta[,aux]
rm(aux)

# Normality tests

ks.test(wta_ba$winner_age, "pnorm", mean=mean(wta_ba$winner_age, na.rm=TRUE), sd=sd(wta_ba$winner_age, na.rm=TRUE))
ks.test(wta_ba$loser_age, "pnorm", mean=mean(wta_ba$loser_age, na.rm=TRUE), sd=sd(wta_ba$loser_age, na.rm=TRUE))
ks.test(wta_ba$dif_age, "pnorm", mean=mean(wta_ba$dif_age, na.rm=TRUE), sd=sd(wta_ba$dif_age, na.rm=TRUE))
ks.test(wta_ba$wta_ba$mean_age, "pnorm", mean=mean(wta_ba$wta_ba$mean_age, na.rm=TRUE), sd=sd(wta_ba$wta_ba$mean_age, na.rm=TRUE))
ks.test(wta_ba$winner_rank, "pnorm", mean=mean(wta_ba$winner_rank, na.rm=TRUE), sd=sd(wta_ba$winner_rank, na.rm=TRUE))
ks.test(wta_ba$loser_rank, "pnorm", mean=mean(wta_ba$loser_rank, na.rm=TRUE), sd=sd(wta_ba$loser_rank, na.rm=TRUE))
ks.test(wta_ba$dif_rank, "pnorm", mean=mean(wta_ba$dif_rank, na.rm=TRUE), sd=sd(wta_ba$dif_rank, na.rm=TRUE))

# compareGroups (Continuous variables are not normal, which is why method=2 is used)

descriptive_wta <- descrTable(WalkOver ~ tourney_category + year + surface + round_level + winner_hand + loser_hand + winner_age + loser_age + dif_age + mean_age + winner_rank + loser_rank + dif_rank, chisq.test.perm = TRUE ,data = wta_ba, method=2)

compare_wta <- compareGroups(WalkOver ~ tourney_category + year + surface + round_level + winner_hand + loser_hand + winner_age + loser_age + dif_age + mean_age + winner_rank + loser_rank + dif_rank, chisq.test.perm = TRUE ,data = wta_ba, method=2)

descriptive_wta
compare_wta

table_wta <- createTable(compare_wta)
export2word(table_wta, file='2 - Bivariate Descriptive Analysis.docx')

# Plots

png(file="2.01 - Tourney Category vs WalkOver.png", width=600, height=450)
ggplot(wta_ba, aes(y=tourney_category)) +
               geom_bar(aes(fill = WalkOver), position = "fill") +
               coord_cartesian(xlim = c(0, 0.01)) +
               scale_fill_manual(values=c("#B9D3EE", "#36648B")) +
               labs(x="% WalkOver",y=NULL,title="Tourney Category",fill="WalkOver") + 
               theme(plot.title = element_text(hjust=0.5))
dev.off()

png(file="2.02 - Surface vs WalkOver.png", width=600, height=450)
ggplot(wta_ba, aes(y=surface)) +
               geom_bar(aes(fill = WalkOver), position = "fill") +
               coord_cartesian(xlim = c(0, 0.01)) +
               scale_fill_manual(values=c("#B9D3EE", "#36648B")) +
               labs(x="% WalkOver",y=NULL,title="Surface",fill="WalkOver") + 
               theme(plot.title = element_text(hjust=0.5))
dev.off()

png(file="2.03 - Round Level vs WalkOver.png", width=600, height=450)
ggplot(wta_ba, aes(y=round_level)) +
               geom_bar(aes(fill = WalkOver), position = "fill") +
               coord_cartesian(xlim = c(0, 0.01)) +
               scale_fill_manual(values=c("#B9D3EE", "#36648B")) +
               labs(x="% WalkOver",y=NULL,title="Round Level",fill="WalkOver") + 
               theme(plot.title = element_text(hjust=0.5))
dev.off()

png(file="2.04 - Winner Hand vs WalkOver.png", width=600, height=450)
ggplot(wta_ba, aes(y=winner_hand)) +
               geom_bar(aes(fill = WalkOver), position = "fill") +
               coord_cartesian(xlim = c(0, 0.01)) +
               scale_fill_manual(values=c("#B9D3EE", "#36648B")) +
               labs(x="% WalkOver",y=NULL,title="Winner Hand",fill="WalkOver") + 
               theme(plot.title = element_text(hjust=0.5))
dev.off()

png(file="2.05 - Loser Hand vs WalkOver.png", width=600, height=450)
ggplot(wta_ba, aes(y=loser_hand)) +
               geom_bar(aes(fill = WalkOver), position = "fill") +
               coord_cartesian(xlim = c(0, 0.01)) +
               scale_fill_manual(values=c("#B9D3EE", "#36648B")) +
               labs(x="% WalkOver",y=NULL,title="Loser Hand",fill="WalkOver") + 
               theme(plot.title = element_text(hjust=0.5))
dev.off()

png(file="2.06 - BoxPlot Winner Age vs WalkOver.png", width=600, height=450)
ggplot(wta_ba, aes(x=WalkOver, y=winner_age)) +
               geom_boxplot(fill='#A4A4A4', color="black") +
               labs(x="WalkOver",y="Age",title="Winner Age") +
               theme(plot.title = element_text(hjust=0.5))
dev.off()

png(file="2.07 - BoxPlot Loser Age vs WalkOver.png", width=600, height=450)
ggplot(wta_ba, aes(x=WalkOver, y=loser_age)) +
               geom_boxplot(fill='#A4A4A4', color="black") +
               labs(x="WalkOver",y="Age",title="Loser Age") +
               theme(plot.title = element_text(hjust=0.5))
dev.off()

png(file="2.08 - BoxPlot Age Difference vs WalkOver.png", width=600, height=450) 
ggplot(wta_ba, aes(x=WalkOver, y=dif_age)) +
               geom_boxplot(fill='#A4A4A4', color="black") +
               labs(x="WalkOver",y="Age",title="Age difference between the winning and losing tennis players") +
               theme(plot.title = element_text(hjust=0.5))
dev.off()

png(file="2.09 - BoxPlot Mean Age vs WalkOver.png", width=600, height=450)  
ggplot(wta_ba, aes(x=WalkOver, y=mean_age)) +
               geom_boxplot(fill='#A4A4A4', color="black") +
               labs(x="WalkOver",y="Age",title="Mean of the ages of tennis players") +
               theme(plot.title = element_text(hjust=0.5))
dev.off()

png(file="2.10 - BoxPlot Winner Rank vs WalkOver.png", width=600, height=450)  
ggplot(wta_ba, aes(x=WalkOver, y=winner_rank)) +
               geom_boxplot(fill='#A4A4A4', color="black") +
               labs(x="WalkOver",y="Rank",title="Ranking position of the winning tennis player of the match") +
               theme(plot.title = element_text(hjust=0.5))
dev.off()

png(file="2.11 - BoxPlot Loser Rank vs WalkOver.png", width=600, height=450)  
ggplot(wta_ba, aes(x=WalkOver, y=loser_rank)) +
               geom_boxplot(fill='#A4A4A4', color="black") +
               labs(x="WalkOver",y="Rank",title="Ranking position of the losing tennis player of the match") +
               theme(plot.title = element_text(hjust=0.5))
dev.off()

png(file="2.12 - BoxPlot Rank Difference vs WalkOver.png", width=600, height=450)  
ggplot(wta_ba, aes(x=WalkOver, y=dif_rank)) +
               geom_boxplot(fill='#A4A4A4', color="black") +
               labs(x="WalkOver",y="Rank",title="Difference in ranking positions between the winning and the losing tennis player") +
               theme(plot.title = element_text(hjust=0.5))
dev.off()  


###########################
# VARIABLE TRANSFORMATION #
###########################

# Transformation of variables for use in epidemiological mesures, joinpoint analyisis and multivariable analysis 

# Creation of the variable "Winner Age >= 21 y.o."

wta$winner_age_21 <- cut(wta$winner_age, breaks = c(0,21,Inf),
                                         labels = c("No", "Yes"),
                                         right = FALSE)
attr(wta$winner_age_21,"label") <- "Winner Age >= 21 y.o."

# Creation of the variable "Loser Age >= 21 y.o."

wta$loser_age_21 <- cut(wta$loser_age, breaks = c(0,21,Inf),
                                       labels = c("No", "Yes"),
                                       right = FALSE)
attr(wta$loser_age_21,"label") <- "Loser Age >= 21 y.o."

# Creation of the variable "Age difference between the winning and losing tennis players >= 0"

wta$dif_age_0 <- cut(wta$dif_age, breaks = c(-100,0,Inf),
                                  labels = c("No", "Yes"),
                                  right = FALSE)
attr(wta$dif_age_0,"label") <- "Age difference between the winning and losing tennis players >= 0"

# Creation of the variable "Mean of the ages of tennis players >= 21 y.o."

wta$mean_age_21 <- cut(wta$mean_age, breaks = c(0,21,Inf),
                                     labels = c("No", "Yes"),
                                     right = FALSE)
attr(wta$mean_age_21,"label") <- "Mean of the ages of tennis players >= 21 y.o."

# Creation of the variable "Ranking position of the winning tennis player of the match >= 400"

wta$winner_rank_400 <- cut(wta$winner_rank, breaks = c(0,400,Inf),
                                            labels = c("No", "Yes"),
                                            right = FALSE)
attr(wta$winner_rank_400,"label") <- "Ranking position of the winning tennis player of the match >= 400"

# Creation of the variable "Ranking position of the losing tennis player of the match >= 400"

wta$loser_rank_400 <- cut(wta$loser_rank, breaks = c(0,400,Inf),
                                          labels = c("No", "Yes"),
                                          right = FALSE)
attr(wta$loser_rank_400,"label") <- "Ranking position of the losing tennis player of the match >= 400"

# Creation of the variable "Difference in ranking positions between the winning and the losing tennis player >= -50"

wta$dif_rank_50 <- cut(wta$dif_rank, breaks = c(-2000,-50,Inf),
                                     labels = c("No", "Yes"),
                                     right = FALSE)
attr(wta$dif_rank_50,"label") <- "Difference in ranking positions between the winning and the losing tennis player >= -50"


############################
# EPIDEMIOLOGICAL MEASURES #
############################

library(epiR) # This library includes the "epi.2by2" function.

sink("3 - Epidemiological Measures.txt")

# Walkovers per 1000 matches:

cat(sep = "\n")
cat("ALL MATCHES:")
cat(sep = "\n")
cat("------------")
cat(sep = "\n")
cat(sep = "\n")

wo_1000(title_wo="All matches:")

# Incidende rate ratio by epi.2by2

print.epi <- function(a,A,ref,var)
{
    cat("Variable: ",var," -  Factor: ",A)
    cat(sep = "\n")
    cat("Walkovers per 1000 matches: ",a$tab[1,3])
    cat(sep = "\n")
    cat(sep = "\n")
    cat("Variable: ",var," - ",A," vs ",ref)
    cat(sep = "\n")
    cat("Incidence rate ratio:       ",paste0(round(a$massoc.summary$est[1],2)," (", round(a$massoc.summary$lower[1] ,2)," to ", round(a$massoc.summary$upper[1],2),")"))
    cat(sep = "\n")
    cat("Attributable risk:          ",paste0(round(a$massoc.summary$est[2],2)," (", round(a$massoc.summary$lower[2] ,2)," to ", round(a$massoc.summary$upper[2],2),")"))
    cat(sep = "\n")
    cat(sep = "\n")
}


# VARIABLE Tourney Category 

# Walkovers per 1000 matches:

cat(sep = "\n")
cat("TOURNEY CATEGORY:")
cat(sep = "\n")
cat("-----------------")
cat(sep = "\n")
cat(sep = "\n")

wo_table <- table(wta$tourney_category, wta$WalkOver)
wo_table
cat(sep = "\n")

wo_1000(tourney_category="ITF Women's World Tennis Tour",title_wo="Tourney Category - ITF Women's World Tennis Tour:")
wo_1000(tourney_category="WTA 125 Tournaments",title_wo="Tourney Category - WTA 125 Tournaments")
wo_1000(tourney_category="WTA Tour",title_wo="Tourney Category - WTA Tour")

# Reference: ITF Women's World Tennis Tour (ITF Women's World Tennis Tour is chosen as the reference surface as it is the most common)
WalkOver_Ref <- wo_table["ITF Women's World Tennis Tour", "WalkOver"]
Matches_Ref <- wo_table["ITF Women's World Tennis Tour", "WalkOver"] + wo_table["ITF Women's World Tennis Tour", "No WalkOver"]

# Tourney Category: WTA 125 Tournaments vs ITF Women's World Tennis Tour

WalkOver_A <- wo_table["WTA 125 Tournaments", "WalkOver"]
Matches_A <- wo_table["WTA 125 Tournaments", "WalkOver"] + wo_table["WTA 125 Tournaments", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.WTA_125 <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

# Tourney Category: WTA Tour vs ITF Women's World Tennis Tour

WalkOver_A <- wo_table["WTA Tour", "WalkOver"]
Matches_A <- wo_table["WTA Tour", "WalkOver"] + wo_table["WTA Tour", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.WTA_Tour <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

print.epi(epi.WTA_125,"WTA 125 Tournaments","ITF Women's World Tennis Tour","Tourney Category")
print.epi(epi.WTA_Tour,"WTA Tour","ITF Women's World Tennis Tour","Tourney Category")

rm(epi.WTA_125, epi.WTA_Tour)
rm(wo_table, WalkOver_Ref, Matches_Ref, WalkOver_A, Matches_A, wo_epiR)


# VARIABLE Surface

# Walkovers per 1000 matches:

cat(sep = "\n")
cat("SURFACE:")
cat(sep = "\n")
cat("--------")
cat(sep = "\n")
cat(sep = "\n")

wo_table <- table(wta$surface, wta$WalkOver)
wo_table
cat(sep = "\n")

wo_1000(surface="Carpet",title_wo="Surface - Carpet:")
wo_1000(surface="Clay",title_wo="Surface - Clay:")
wo_1000(surface="Grass",title_wo="Surface - Grass:")
wo_1000(surface="Hard",title_wo="Surface - Hard:")

# Reference: Hard (Hard is chosen as the reference surface as it is the most common)
WalkOver_Ref <- wo_table["Hard", "WalkOver"]
Matches_Ref <- wo_table["Hard", "WalkOver"] + wo_table["Hard", "No WalkOver"]

# Surface: Carpet vs Hard

WalkOver_A <- wo_table["Carpet", "WalkOver"]
Matches_A <- wo_table["Carpet", "WalkOver"] + wo_table["Carpet", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.Carpet <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

# Surface: Clay vs Hard

WalkOver_A <- wo_table["Clay", "WalkOver"]
Matches_A <- wo_table["Clay", "WalkOver"] + wo_table["Clay", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.Clay <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

# Surface: Grass vs Hard

WalkOver_A <- wo_table["Grass", "WalkOver"]
Matches_A <- wo_table["Grass", "WalkOver"] + wo_table["Grass", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.Grass <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

print.epi(epi.Carpet,"Carpet","Hard","Surface")
print.epi(epi.Clay,"Clay","Hard","Surface")
print.epi(epi.Grass,"Grass","Hard","Surface")

rm(epi.Carpet, epi.Clay, epi.Grass)
rm(wo_table, WalkOver_Ref, Matches_Ref, WalkOver_A, Matches_A, wo_epiR)

# VARIABLE Round Level

# Walkovers per 1000 matches:

cat(sep = "\n")
cat("ROUND LEVEL:")
cat(sep = "\n")
cat("------------")
cat(sep = "\n")
cat(sep = "\n")

wo_table <- table(wta$round_level, wta$WalkOver)
wo_table
cat(sep = "\n")

wo_1000(round_level="Final Round",title_wo="Round Level - Final Round:")
wo_1000(round_level="Preliminary Round",title_wo="Round Level - Preliminary Round")
wo_1000(round_level="Qualifying Round",title_wo="Round Level - Qualifying Round")

# Reference: Preliminary Round (Preliminary Round is chosen as the reference surface as it is the most common)
WalkOver_Ref <- wo_table["Preliminary Round", "WalkOver"]
Matches_Ref <- wo_table["Preliminary Round", "WalkOver"] + wo_table["Preliminary Round", "No WalkOver"]

# Round Level: Final Round vs Preliminary Round

WalkOver_A <- wo_table["Final Round", "WalkOver"]
Matches_A <- wo_table["Final Round", "WalkOver"] + wo_table["Final Round", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.Final <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

# Round Level: Qualifying Round vs Preliminary Round

WalkOver_A <- wo_table["Qualifying Round", "WalkOver"]
Matches_A <- wo_table["Qualifying Round", "WalkOver"] + wo_table["Qualifying Round", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.Qualifying <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

print.epi(epi.Final,"Final Round","Preliminary Round","Round Level")
print.epi(epi.Qualifying,"Qualifying Round","Preliminary Round","Round Level")

rm(epi.Final, epi.Qualifying)
rm(wo_table, WalkOver_Ref, Matches_Ref, WalkOver_A, Matches_A, wo_epiR)


# VARIABLE Winner Hand

# Walkovers per 1000 matches:

cat(sep = "\n")
cat("WINNER HAND:")
cat(sep = "\n")
cat("------------")
cat(sep = "\n")
cat(sep = "\n")

wo_table <- table(wta$winner_hand, wta$WalkOver)
wo_table
cat(sep = "\n")

wo_1000(winner_hand="Left",title_wo="Winner Hand - Left:")
wo_1000(winner_hand="Right",title_wo="Winner Hand - Right")
wo_1000(winner_hand="Unknown",title_wo="Winner Hand - Unknown")

# Reference: Right (Right is chosen as the reference surface as it is the most common)
WalkOver_Ref <- wo_table["Right", "WalkOver"]
Matches_Ref <- wo_table["Right", "WalkOver"] + wo_table["Right", "No WalkOver"]

# Winner Hand: Left vs Right

WalkOver_A <- wo_table["Left", "WalkOver"]
Matches_A <- wo_table["Left", "WalkOver"] + wo_table["Left", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.Left <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

# Winner Hand: Unknown vs Right

WalkOver_A <- wo_table["Unknown", "WalkOver"]
Matches_A <- wo_table["Unknown", "WalkOver"] + wo_table["Unknown", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.Unknown <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

print.epi(epi.Left,"Left","Right","Winner Hand")
print.epi(epi.Unknown,"Unknown","Right","Winner Hand")

rm(epi.Left, epi.Unknown)
rm(wo_table, WalkOver_Ref, Matches_Ref, WalkOver_A, Matches_A, wo_epiR)


# VARIABLE Loser Hand

# Walkovers per 1000 matches:

cat(sep = "\n")
cat("LOSER HAND:")
cat(sep = "\n")
cat("-----------")
cat(sep = "\n")
cat(sep = "\n")

wo_table <- table(wta$loser_hand, wta$WalkOver)
wo_table
cat(sep = "\n")

wo_1000(loser_hand="Left",title_wo="Loser Hand - Left:")
wo_1000(loser_hand="Right",title_wo="Loser Hand - Right")
wo_1000(loser_hand="Unknown",title_wo="Loser Hand - Unknown")

# Reference: Right (Right is chosen as the reference surface as it is the most common)
WalkOver_Ref <- wo_table["Right", "WalkOver"]
Matches_Ref <- wo_table["Right", "WalkOver"] + wo_table["Right", "No WalkOver"]

# Loser Hand: Left vs Right

WalkOver_A <- wo_table["Left", "WalkOver"]
Matches_A <- wo_table["Left", "WalkOver"] + wo_table["Left", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.Left <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

# Loser Hand: Unknown vs Right

WalkOver_A <- wo_table["Unknown", "WalkOver"]
Matches_A <- wo_table["Unknown", "WalkOver"] + wo_table["Unknown", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.Unknown <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

print.epi(epi.Left,"Left","Right","Loser Hand")
print.epi(epi.Unknown,"Unknown","Right","Loser Hand")

rm(epi.Left, epi.Unknown)
rm(wo_table, WalkOver_Ref, Matches_Ref, WalkOver_A, Matches_A, wo_epiR)


# VARIABLE Winner Age

# Walkovers per 1000 matches:

cat(sep = "\n")
cat("WINNER AGE:")
cat(sep = "\n")
cat("-----------")
cat(sep = "\n")
cat(sep = "\n")

wo_table <- table(wta$winner_age_21, wta$WalkOver)
wo_table
cat(sep = "\n")

wo_1000(winner_age_f=21,title_wo="Winner Age < 21 y.o. :")
wo_1000(winner_age_i=21,title_wo="Winner Age >= 21 y.o. :")

# Reference: Winner Age < 21 y.o.
WalkOver_Ref <- wo_table["No", "WalkOver"]
Matches_Ref <- wo_table["No", "WalkOver"] + wo_table["No", "No WalkOver"]

# Winner Age: < 21 y.o. vs >= 21 y.o.

WalkOver_A <- wo_table["Yes", "WalkOver"]
Matches_A <- wo_table["Yes", "WalkOver"] + wo_table["Yes", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.Yes <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

print.epi(epi.Yes,">= 21 y.o.","< 21 y.o.","Winner Age")

rm(epi.Yes)
rm(wo_table, WalkOver_Ref, Matches_Ref, WalkOver_A, Matches_A, wo_epiR)


# VARIABLE Loser Age

# Walkovers per 1000 matches:

cat(sep = "\n")
cat("LOSER AGE:")
cat(sep = "\n")
cat("----------")
cat(sep = "\n")
cat(sep = "\n")

wo_table <- table(wta$loser_age_21, wta$WalkOver)
wo_table
cat(sep = "\n")

wo_1000(loser_age_f=21,title_wo="Loser Age < 21 y.o. :")
wo_1000(loser_age_i=21,title_wo="Loser Age >= 21 y.o. :")

# Reference: Loser Age < 21 y.o.
WalkOver_Ref <- wo_table["No", "WalkOver"]
Matches_Ref <- wo_table["No", "WalkOver"] + wo_table["No", "No WalkOver"]

# Loser Age: < 21 y.o. vs >= 21 y.o.

WalkOver_A <- wo_table["Yes", "WalkOver"]
Matches_A <- wo_table["Yes", "WalkOver"] + wo_table["Yes", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.Yes <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

print.epi(epi.Yes,">= 21 y.o.","< 21 y.o.","Loser Age")

rm(epi.Yes)
rm(wo_table, WalkOver_Ref, Matches_Ref, WalkOver_A, Matches_A, wo_epiR)


# VARIABLE Difference Age

# Walkovers per 1000 matches:

cat(sep = "\n")
cat("AGE DIFFERENCE BETWEEN THE WINNING AND LOSING TENNIS PLAYERS:")
cat(sep = "\n")
cat("-------------------------------------------------------------")
cat(sep = "\n")
cat(sep = "\n")

wo_table <- table(wta$dif_age_0, wta$WalkOver)
wo_table
cat(sep = "\n")

wo_1000(dif_age_f=0,title_wo="Age Difference < 0 y.o. :")
wo_1000(dif_age_i=0,title_wo="Age Difference >= 0 y.o. :")

# Reference: Age Difference < 0 y.o.
WalkOver_Ref <- wo_table["No", "WalkOver"]
Matches_Ref <- wo_table["No", "WalkOver"] + wo_table["No", "No WalkOver"]
 
# Age Difference: < 0 y.o. vs >= 0 y.o.
 
WalkOver_A <- wo_table["Yes", "WalkOver"]
Matches_A <- wo_table["Yes", "WalkOver"] + wo_table["Yes", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.Yes <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")
 
print.epi(epi.Yes,">= 0 y.o.","< 0 y.o.","Age Difference")

rm(epi.Yes)
rm(wo_table, WalkOver_Ref, Matches_Ref, WalkOver_A, Matches_A, wo_epiR)


# VARIABLE Mean Age

# Walkovers per 1000 matches:

cat(sep = "\n")
cat("MEAN OF THE AGES OF TENNIS PLAYERS:")
cat(sep = "\n")
cat("-----------------------------------")
cat(sep = "\n")
cat(sep = "\n")

wo_table <- table(wta$mean_age_21, wta$WalkOver)
wo_table
cat(sep = "\n")

wo_1000(mean_age_f=21,title_wo="Mean Age < 21 y.o. :")
wo_1000(mean_age_i=21,title_wo="Mean Age >= 21 y.o. :")

# Reference: Mean Age < 21 y.o.
WalkOver_Ref <- wo_table["No", "WalkOver"]
Matches_Ref <- wo_table["No", "WalkOver"] + wo_table["No", "No WalkOver"]

# Mean Age: < 21 y.o. vs >= 21 y.o.

WalkOver_A <- wo_table["Yes", "WalkOver"]
Matches_A <- wo_table["Yes", "WalkOver"] + wo_table["Yes", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.Yes <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

print.epi(epi.Yes,">= 21 y.o.","< 21 y.o.","Mean Age")

rm(epi.Yes)
rm(wo_table, WalkOver_Ref, Matches_Ref, WalkOver_A, Matches_A, wo_epiR)


# VARIABLE Winner Rank

# Walkovers per 1000 matches:

cat(sep = "\n")
cat("WINNER RANK:")
cat(sep = "\n")
cat("------------")
cat(sep = "\n")
cat(sep = "\n")

wo_table <- table(wta$winner_rank_400, wta$WalkOver)
wo_table
cat(sep = "\n")

wo_1000(winner_rank_f=400,title_wo="Ranking position of the winning tennis player of the match < 400")
wo_1000(winner_rank_i=400,title_wo="Ranking position of the winning tennis player of the match >= 400")

# Reference: Ranking position < 400
WalkOver_Ref <- wo_table["No", "WalkOver"]
Matches_Ref <- wo_table["No", "WalkOver"] + wo_table["No", "No WalkOver"]

# Ranking position: < 400 vs >= 400

WalkOver_A <- wo_table["Yes", "WalkOver"]
Matches_A <- wo_table["Yes", "WalkOver"] + wo_table["Yes", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.Yes <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

print.epi(epi.Yes,">= 400","< 400","Ranking position of the winning tennis player of the match")

rm(epi.Yes)
rm(wo_table, WalkOver_Ref, Matches_Ref, WalkOver_A, Matches_A, wo_epiR)


# VARIABLE Loser Rank

# Walkovers per 1000 matches:

cat(sep = "\n")
cat("LOSER RANK:")
cat(sep = "\n")
cat("-----------")
cat(sep = "\n")
cat(sep = "\n")

wo_table <- table(wta$loser_rank_400, wta$WalkOver)
wo_table
cat(sep = "\n")

wo_1000(loser_rank_f=400,title_wo="Ranking position of the losing tennis player of the match < 400")
wo_1000(loser_rank_i=400,title_wo="Ranking position of the losing tennis player of the match >= 400")

# Reference: Ranking position < 400
WalkOver_Ref <- wo_table["No", "WalkOver"]
Matches_Ref <- wo_table["No", "WalkOver"] + wo_table["No", "No WalkOver"]

# Ranking position: < 400 vs >= 400

WalkOver_A <- wo_table["Yes", "WalkOver"]
Matches_A <- wo_table["Yes", "WalkOver"] + wo_table["Yes", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.Yes <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

print.epi(epi.Yes,">= 400","< 400","Ranking position of the losing tennis player of the match")

rm(epi.Yes)
rm(wo_table, WalkOver_Ref, Matches_Ref, WalkOver_A, Matches_A, wo_epiR)


# VARIABLE Difference Rank

# Walkovers per 1000 matches:

cat(sep = "\n")
cat("DIFFERENCE IN RANKING POSITIONS BETWEEN THE WINNING AND THE LOSING TENNIS PLAYER:")
cat(sep = "\n")
cat("---------------------------------------------------------------------------------")
cat(sep = "\n")
cat(sep = "\n")

wo_table <- table(wta$dif_rank_50, wta$WalkOver)
wo_table
cat(sep = "\n")

wo_1000(dif_rank_f=-50,title_wo="Difference in ranking positions between the winning and the losing tennis player < -50")
wo_1000(dif_rank_i=-50,title_wo="Difference in ranking positions between the winning and the losing tennis player >= -50")

# Reference: Difference in ranking positions between the winning and the losing tennis player < -50
WalkOver_Ref <- wo_table["No", "WalkOver"]
Matches_Ref <- wo_table["No", "WalkOver"] + wo_table["No", "No WalkOver"]

# Difference in ranking positions between the winning and the losing tennis player < -50 vs >= -50

WalkOver_A <- wo_table["Yes", "WalkOver"]
Matches_A <- wo_table["Yes", "WalkOver"] + wo_table["Yes", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.Yes <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

print.epi(epi.Yes,">= -50","< -50","Difference in ranking positions between the winning and the losing tennis player")

rm(epi.Yes)
rm(wo_table, WalkOver_Ref, Matches_Ref, WalkOver_A, Matches_A, wo_epiR)

sink()


############################
# JOINPOINT ANALYSIS       #
############################

# Data Preparation

setwd("../Data/Joinpoint")

# Walkovers per 1000 matches:

data.joinpoint <- wo_1000()$dd[wo_1000()$dd$year>=1996 & wo_1000()$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"00 - JP_Global.csv",row.names=FALSE)

# VARIABLE Tourney Category 

data.joinpoint <- wo_1000(tourney_category="ITF Women's World Tennis Tour")$dd[wo_1000(tourney_category="ITF Women's World Tennis Tour")$dd$year>=1996 & wo_1000(tourney_category="ITF Women's World Tennis Tour")$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"01 - JP_Tourney_ITF.csv",row.names=FALSE)
data.joinpoint <- wo_1000(tourney_category="WTA 125 Tournaments")$dd[wo_1000(tourney_category="WTA 125 Tournaments")$dd$year>=1996 & wo_1000(tourney_category="WTA 125 Tournaments")$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"02 - JP_Tourney_WTA_125.csv",row.names=FALSE)
data.joinpoint <- wo_1000(tourney_category="WTA Tour")$dd[wo_1000(tourney_category="WTA Tour")$dd$year>=1996 & wo_1000(tourney_category="WTA Tour")$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"03 - JP__Tourney_WTA_Tour.csv",row.names=FALSE)

# VARIABLE Surface

data.joinpoint <- wo_1000(surface="Carpet")$dd[wo_1000(surface="Carpet")$dd$year>=1996 & wo_1000(surface="Carpet")$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"04 - JP_Surface_Carpet.csv",row.names=FALSE)
data.joinpoint <- wo_1000(surface="Clay")$dd[wo_1000(surface="Clay")$dd$year>=1996 & wo_1000(surface="Clay")$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"05 - JP_Surface_Clay.csv",row.names=FALSE)
data.joinpoint <- wo_1000(surface="Grass")$dd[wo_1000(surface="Grass")$dd$year>=1996 & wo_1000(surface="Grass")$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"06 - JP_Surface_Grass.csv",row.names=FALSE)
data.joinpoint <- wo_1000(surface="Hard")$dd[wo_1000(surface="Hard")$dd$year>=1996 & wo_1000(surface="Hard")$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"07 - JP_Surface_Hard.csv",row.names=FALSE)

# VARIABLE Round Level

data.joinpoint <- wo_1000(round_level="Final Round")$dd[wo_1000(round_level="Final Round")$dd$year>=1996 & wo_1000(round_level="Final Round")$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"08 - JP_Round_Final.csv",row.names=FALSE)
data.joinpoint <- wo_1000(round_level="Preliminary Round")$dd[wo_1000(round_level="Preliminary Round")$dd$year>=1996 & wo_1000(round_level="Preliminary Round")$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"09 - JP_Round_Preliminary.csv",row.names=FALSE)
data.joinpoint <- wo_1000(round_level="Qualifying Round")$dd[wo_1000(round_level="Qualifying Round")$dd$year>=1996 & wo_1000(round_level="Qualifying Round")$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"10 - JP_Round_Qualifying.csv",row.names=FALSE)

# VARIABLE Winner Hand

data.joinpoint <- wo_1000(winner_hand="Left")$dd[wo_1000(winner_hand="Left")$dd$year>=1996 & wo_1000(winner_hand="Left")$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"11 - JP_Winner_Hand_Left.csv",row.names=FALSE)
data.joinpoint <- wo_1000(winner_hand="Right")$dd[wo_1000(winner_hand="Right")$dd$year>=1996 & wo_1000(winner_hand="Right")$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"12 - JP_Winner_Hand_Right.csv",row.names=FALSE)
data.joinpoint <- wo_1000(winner_hand="Unknown")$dd[wo_1000(winner_hand="Unknown")$dd$year>=1996 & wo_1000(winner_hand="Unknown")$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"13 - JP_Winner_Hand_Unknown.csv",row.names=FALSE)

# VARIABLE Loser Hand

data.joinpoint <- wo_1000(loser_hand="Left")$dd[wo_1000(loser_hand="Left")$dd$year>=1996 & wo_1000(loser_hand="Left")$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"14 - JP_Loser_Hand_Left.csv",row.names=FALSE)
data.joinpoint <- wo_1000(loser_hand="Right")$dd[wo_1000(loser_hand="Right")$dd$year>=1996 & wo_1000(loser_hand="Right")$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"15 - JP_Loser_Hand_Right.csv",row.names=FALSE)
data.joinpoint <- wo_1000(loser_hand="Unknown")$dd[wo_1000(loser_hand="Unknown")$dd$year>=1996 & wo_1000(loser_hand="Unknown")$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"16 - JP_Loser_Hand_Unknown.csv",row.names=FALSE)

# VARIABLE Winner Age

data.joinpoint <- wo_1000(winner_age_f=21)$dd[wo_1000(winner_age_f=21)$dd$year>=1996 & wo_1000(winner_age_f=21)$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"17 - JP_Winner_Age_less_21.csv",row.names=FALSE)
data.joinpoint <- wo_1000(winner_age_i=21)$dd[wo_1000(winner_age_i=21)$dd$year>=1996 & wo_1000(winner_age_i=21)$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"18 - JP_Winner_Age_more_21.csv",row.names=FALSE)

# VARIABLE Loser Age

data.joinpoint <- wo_1000(loser_age_f=21)$dd[wo_1000(loser_age_f=21)$dd$year>=1996 & wo_1000(loser_age_f=21)$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"19 - JP_Loser_Age_less_21.csv",row.names=FALSE)
data.joinpoint <- wo_1000(loser_age_i=21)$dd[wo_1000(loser_age_i=21)$dd$year>=1996 & wo_1000(loser_age_i=21)$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"20 - JP_Loser_Age_more_21.csv",row.names=FALSE)

# VARIABLE Difference Age

data.joinpoint <- wo_1000(dif_age_f=0)$dd[wo_1000(dif_age_f=0)$dd$year>=1996 & wo_1000(dif_age_f=0)$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"21 - JP_Difference_Age_less_0.csv",row.names=FALSE)
data.joinpoint <- wo_1000(dif_age_i=0)$dd[wo_1000(dif_age_i=0)$dd$year>=1996 & wo_1000(dif_age_i=0)$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"22 - JP_Difference_Age_more_0.csv",row.names=FALSE)

# VARIABLE Mean Age

data.joinpoint <- wo_1000(mean_age_f=21)$dd[wo_1000(mean_age_f=21)$dd$year>=1996 & wo_1000(mean_age_f=21)$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"23 - JP_Mean_Age_less_21.csv",row.names=FALSE)
data.joinpoint <- wo_1000(mean_age_i=21)$dd[wo_1000(mean_age_i=21)$dd$year>=1996 & wo_1000(mean_age_i=21)$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"24 - JP_Mean_Age_more_21.csv",row.names=FALSE)

# VARIABLE Winner Rank

data.joinpoint <- wo_1000(winner_rank_f=400)$dd[wo_1000(winner_rank_f=400)$dd$year>=1996 & wo_1000(winner_rank_f=400)$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"25 - JP_Winner_Rank_less_400.csv",row.names=FALSE)
data.joinpoint <- wo_1000(winner_rank_i=400)$dd[wo_1000(winner_rank_i=400)$dd$year>=1996 & wo_1000(winner_rank_i=400)$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"26 - JP_Winner_Rank_more_400.csv",row.names=FALSE)

# VARIABLE Loser Rank

data.joinpoint <- wo_1000(loser_rank_f=400)$dd[wo_1000(loser_rank_f=400)$dd$year>=1996 & wo_1000(loser_rank_f=400)$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"27 - JP_Loser_Rank_less_400.csv",row.names=FALSE)
data.joinpoint <- wo_1000(loser_rank_i=400)$dd[wo_1000(loser_rank_i=400)$dd$year>=1996 & wo_1000(loser_rank_i=400)$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"28 - JP_Loser_Rank_more_400.csv",row.names=FALSE)

# VARIABLE Difference Rank

data.joinpoint <- wo_1000(dif_rank_f=-50)$dd[wo_1000(dif_rank_f=-50)$dd$year>=1996 & wo_1000(dif_rank_f=-50)$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"29 - JP_Difference_Rank_less_-50.csv",row.names=FALSE)
data.joinpoint <- wo_1000(dif_rank_i=-50)$dd[wo_1000(dif_rank_i=-50)$dd$year>=1996 & wo_1000(dif_rank_i=-50)$dd$year<=2018 ,c("year","incidence","var_year")]
write.csv(data.joinpoint,"30 - JP_Difference_Rank_more_-50.csv",row.names=FALSE)

rm(data.joinpoint)


############################
# MULTIVARIABLE ANALYSIS   #
############################

setwd("../../")
setwd("Results")

# Library charge

library(car)
library(ggplot2)

## Data preparation

# Definition reference category

wta$tourney_category <- relevel(wta$tourney_category,ref="ITF Women's World Tennis Tour")  
wta$surface <- relevel(wta$surface,ref="Hard") 
wta$round_level <- relevel(wta$round_level,ref="Preliminary Round") 
wta$winner_hand <- relevel(wta$winner_hand,ref="Right") 
wta$loser_hand <- relevel(wta$loser_hand,ref="Right") 
wta$winner_age_21 <- relevel(wta$winner_age_21,ref="No") 
wta$loser_age_21 <- relevel(wta$loser_age_21,ref="No") 
wta$dif_age_0 <- relevel(wta$dif_age_0,ref="No") 
wta$mean_age_21 <- relevel(wta$mean_age_21,ref="No") 
wta$winner_rank_400 <- relevel(wta$winner_rank_400,ref="No") 
wta$loser_rank_400 <- relevel(wta$loser_rank_400,ref="No") 
wta$dif_rank_50 <- relevel(wta$dif_rank_50,ref="No") 

# Initial Model

wta_initial <- glm(WalkOver ~ year + tourney_category + surface + round_level + winner_hand + loser_hand + 
                              winner_age_21 + loser_age_21 + dif_age_0 + mean_age_21 + winner_rank_400 + loser_rank_400 + dif_rank_50,
                   data = wta, 
				   family = binomial) 
				   
# Plot of linearity

table_year <- table(wta$year,wta$WalkOver) 
year <- as.numeric(rownames(table_year)) 
ln_odd_year <- log((table_year[,2]+.1) / (table_year[,1]+.1)) 
png(file="5.1 - Plot of linearity for the year variable.png", width=600, height=450) 
plot(year,ln_odd_year,xlab="Years",ylab="ln (odd)",las=1, main="WalkOver")
lines(lowess(ln_odd_year~year), col=4, lwd=2)
dev.off()
rm(table_year, year, ln_odd_year)

# Test of linearity

wta_linearity <- wta
wta_linearity$log_year <- log(wta_linearity$year) * wta_linearity$year
wta_linearity_test <- glm(WalkOver ~ year + log_year + tourney_category + surface + round_level + winner_hand + loser_hand + 
                                     winner_age_21 + loser_age_21 + dif_age_0 + mean_age_21 + winner_rank_400 + loser_rank_400 + dif_rank_50,
                          data = wta_linearity, 
				          family = binomial) 
summary(wta_linearity_test)
rm(wta_linearity,wta_linearity_test)

# Test for multicollinearity

vif(wta_initial)

wta_initial <- glm(WalkOver ~ year + tourney_category + surface + round_level + winner_hand + loser_hand + 
                              winner_age_21 + loser_age_21 + dif_age_0 + loser_rank_400 + dif_rank_50,
                   data = wta, 
				   family = binomial) 
vif(wta_initial)

## Binary logistic regression

# Initial Model

wta_initial <- glm(WalkOver ~ year + tourney_category + surface + round_level + winner_hand + loser_hand + 
                              winner_age_21 + loser_age_21 + dif_age_0 + loser_rank_400 + dif_rank_50,
                   data = wta, 
				   family = binomial) 
summary(wta_initial)

# Model selection

step(wta_initial)

# Definitive Model

wta_def <- glm(WalkOver ~ round_level + loser_age_21 + loser_rank_400 + dif_rank_50,
               data = wta, 
			   family = binomial) 
summary(wta_def)

# Goodness of fit.

dev <- wta_def$deviance
nullDev <- wta_def$null.deviance
modelChi <- nullDev - dev
modelChi

chigl <- wta_def$df.null - wta_def$df.residual
chisq.prob <- 1 - pchisq(modelChi, chigl)
chisq.prob

rm(dev,nullDev,modelChi,chigl,chisq.prob)

## Results

# Coefficients
summary(wta_def)$coefficients

# Odds Ratio
exp(wta_def$coefficients)

# Confidence Intervals of Odds Ratio
exp(confint(wta_def))

sink("5.3 - Table OR.txt")
table_OR <- cbind(exp(wta_def$coefficients),exp(confint(wta_def)))
table_OR <- as.data.frame(table_OR) 
names(table_OR) <- c("OR","lower_OR","upper_OR")
table_OR$Variable <- c("Constant", "Round Level: Final Round","Round Level: Qualifying","Loser Age > 21","Loser Rank > 400","Difference Rank > 50")
table_OR <- table_OR[,c("Variable","OR","lower_OR","upper_OR")]
table_OR <- table_OR[-1, ]
table_OR
sink()

# Plot

png(file="5.4 - Odds Ratio of the predictor variables.png", width=600, height=450) 
ggplot(table_OR, aes(x = Variable, y = OR)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_OR, ymax = upper_OR), width = 0.2) +
  coord_flip() +  # Voltea el gráfico para una mejor visualización
  xlab("Predictors") +
  ylab("Odds Ratios (IC 95%)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 14)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "blue") +
  geom_text(aes(label = paste("OR:", round(OR, 2), "\n[", round(lower_OR, 2), "-", round(upper_OR, 2), "]")), 
            hjust = 1.1, size = 4, position = position_dodge(width = 0.8))
dev.off()

## Model Validation

# Test for multicollinearity

vif(wta_def)

# Tests for Residuals

wta_pred <- wta[!is.na(wta$dif_rank_50) & !is.na(wta$loser_age_21),]
wta_def <- glm(WalkOver ~ round_level + loser_age_21 + loser_rank_400 + dif_rank_50,
               data = wta_pred, 
			   family = binomial) 

wta_pred$predicted.probability <- fitted(wta_def)
wta_pred$studentized.residuals <- rstudent(wta_def)
wta_pred$df_beta <- dfbeta(wta_def)
wta_pred$dffit <- dffits(wta_def)
wta_pred$leverage <- hatvalues(wta_def)

# Studentized Residuals

studentized.residuals_2.0 <- (nrow(wta_pred[wta_pred$studentized.residuals > 2,])/nrow(wta_pred))*100
studentized.residuals_2.5 <- (nrow(wta_pred[wta_pred$studentized.residuals > 2.5,])/nrow(wta_pred))*100

# Leverage

mean_leverage <- mean(wta_pred$leverage)
levereage_2.0 <- (nrow(wta_pred[wta_pred$leverage > 2*mean_leverage,])/nrow(wta_pred))*100
levereage_3.0 <- (nrow(wta_pred[wta_pred$leverage > 3*mean_leverage,])/nrow(wta_pred))*100

# DfBeta 
 
df_beta_1 <- (nrow(wta_pred[wta_pred$df_beta_1 > 1,])/nrow(wta_pred))*100

# Output

sink("5.5 - Tests for Residuals.txt")
cat("Studentized Residuals:")
cat(sep = "\n")
quantile(abs(wta_pred$studentized.residuals),c(0.95,0.99,0.999))
cat(sep = "\n")
cat("Percentage of matches with Studentized Residuals greater than 2:   ",round(studentized.residuals_2.0,1)," %")
cat(sep = "\n")
cat("Percentage of matches with Studentized Residuals greater than 2.5: ",round(studentized.residuals_2.5,1)," %")
cat(sep = "\n")
cat(sep = "\n")
cat("Leverage:")
cat(sep = "\n")
quantile(wta_pred$leverage,c(0.5,0.8,0.9,0.95,0.99))
cat(sep = "\n")
cat("Percentage of matches with Leverage greater than 2 times the average of Leverage:",round(levereage_2.0,1)," %")
cat(sep = "\n")
cat("Percentage of matches with Leverage greater than 3 times the average of Leverage:",round(levereage_3.0,1)," %")
cat(sep = "\n")
cat(sep = "\n")
cat("DfBeta:")
cat(sep = "\n")
quantile(abs(wta_pred$df_beta),c(0.95,0.99,0.999))
cat(sep = "\n")
cat("Percentage of matches with DfBeta greater than 1:",round(df_beta_1,1)," %")
cat(sep = "\n")
cat(sep = "\n")
sink()