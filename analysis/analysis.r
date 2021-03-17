library(dplyr)
library(tidyverse)
library(ggpubr)
library(afex)
library(emmeans)
library(ggplot2)
library(psych)
library(lmerTest)
library(hrbrthemes)
library(viridis)
library(sjstats)
library(rgl)
library(plotly)
library(lme4)
library(stats)
library(jtools)
"""
rad_01 to rad_18: consumption per round
rad_19: feeling while using
rad_20: part of the group
rad_21: responsibility towards the group
rad_22: others' responsibility towards the group
rad_23 to rad_27: PA scale
"""
# data retrieval and variable naming ####
# read-in data (base)
dw <- read.csv("~/Desktop/thesis_decarb/analysis/cpr_v2_18022021.csv",
               sep=";", stringsAsFactors=FALSE)
names(dw)[1:11] <- c("id","ip","cond","start_time","start_date","end_time","end_date",
                     "session_length","page_trail","pages_visited","user_agent")
dw <- dw %>% mutate(start_date=as.Date(start_date, format = "%m/%d/%y"))
dw <- dw %>% mutate(end_date=as.Date(end_date, format = "%m/%d/%y"))
# read-in data (prolific)
dp <- read.csv("~/Desktop/thesis_decarb/analysis/cpr_v2_prolific_24022021.csv",
               sep=";", stringsAsFactors=FALSE)
names(dp)[1:11] <- c("id","ip","cond","start_time","start_date","end_time","end_date",
                     "session_length","page_trail","pages_visited","user_agent")
dp <- dp %>% mutate(start_date=as.Date(start_date, format = "%m/%d/%y"))
dp <- dp %>% mutate(end_date=as.Date(end_date, format = "%m/%d/%y"))
dw$vi_1 <- NULL
dp$vi_1 <- NULL
# combine data
df <- bind_rows(dw,dp)
rm(dw)
rm(dp)
# remove non-responses by id (tested by the ANOVAs)
df <- df[!(df$id %in% c(84582366,419847541,423497083,599306793,NA)),]

# articulate conditions
df$anon <- NA; df$ecol <- NA
df$anon[df$cond == "112acf"] <- 0; df$ecol[df$cond == "112acf"] <- 0 # nonanon, inef
df$anon[df$cond == "128569"] <- 1; df$ecol[df$cond == "128569"] <- 0 # anon, inef
df$anon[df$cond == "214da7"] <- 0; df$ecol[df$cond == "214da7"] <- 1 # nonanon, eff
df$anon[df$cond == "22ec89"] <- 1; df$ecol[df$cond == "22ec89"] <- 1 # anon, eff


# exlusion criteria ####
# pages_visited, session_length, pop_01 (group simulation; 0no, 1yes, 2idonotcare, 3ididdothetaskalone)
df <- df %>% filter(pages_visited >= 33)
df <- df %>% filter(start_date > as.Date("01/19/21", format = "%m/%d/%y"))
df <- df %>% filter(pop_01 != "3" | is.na(pop_01)) #filter(mydf, is.na(y) | y != "a")
df <- df %>% filter(rad_att != 1 | is.na(rad_att))


# data transformations ####
# calculate means per stage (NAs rm)
df$consum1 <- rowMeans(df[,c("rad_01","rad_02","rad_03","rad_04","rad_05","rad_06")],
                       na.rm = TRUE)
df$consum2 <- rowMeans(df[,c("rad_07","rad_08","rad_09","rad_10","rad_11","rad_12")],
                       na.rm = TRUE)
df$consum3 <- rowMeans(df[,c("rad_13","rad_14","rad_15","rad_16","rad_17","rad_18")],
                       na.rm = TRUE)

# PA score (ranges from 0 to 6); watch out for NAs in PA scale
df$PA <- (df$rad_23 + df$rad_24 + (6-df$rad_25) + (6-df$rad_26) + (6-df$rad_27))/5


# SVO decoding (opt_01 to opt_09)
# 0: 500/500 (prosocial); 1: 570/300 (competitive); 2: 500/100 (individualistic)
svo_vars <- c("opt_01","opt_02","opt_03","opt_04","opt_05","opt_06",
              "opt_07","opt_08","opt_09")

decode_svo <- function() {
  # storage vector
  store <- c()
  # one column per participant
  frme <- data.frame(t(df[,svo_vars]))
  # convert to factor
  for (i in svo_vars) {
    df[,i] <- factor(df[,i])
  }
  # classify
  for (i in 1:dim(frme)[2]) {
    # prosocial 0
    if (length(frme[,i][frme[,i]==0]) >= 6) {
      store <- c(store,"prosocial")
    # competitive 1
    } else if (length(frme[,i][frme[,i]==1]) >= 6) {
      store <- c(store,"competitive")
    # individualistic 2
    } else if (length(frme[,i][frme[,i]==2]) >= 6) {
      store <- c(store,"individualistic")
    } else {
      store <- c(store,"non-classifiable")
    }
  }
  # add vector to df
  df$svo <- store
  return(df)
}
df <- decode_svo()

# PA quantiles for test
df$PA[is.na(df$PA)] <- mean(df$PA,na.rm=TRUE)
quantile(df$PA)[2]
quantile(df$PA)[4]
df$PAQ <- ''
df$PAQ[df$PA < quantile(df$PA)[2]] <- 1
df$PAQ[df$PA >= quantile(df$PA)[2] & df$PA < quantile(df$PA)[3]] <- 2
df$PAQ[df$PA >= quantile(df$PA)[3] & df$PA < quantile(df$PA)[4]] <- 3
df$PAQ[df$PA >= quantile(df$PA)[4]] <- 4
df$PAQ <- factor(df$PAQ)

# data comparison
df$quality <- NA
df$quality[df$pop_01 == 3] <- "medium"
df$quality[df$pop_01 != 3] <- "high"
table(df$svo[df$quality == "medium"])
table(df$svo[df$quality == "high"])
df %>%
  ggplot( aes(x=quality, y=consum1, fill=quality)) +
  geom_boxplot()
df %>%
  ggplot( aes(x=quality, y=consum2, fill=quality)) +
  geom_boxplot()
df %>%
  ggplot( aes(x=quality, y=consum3, fill=quality)) +
  geom_boxplot()
df %>%
  ggplot( aes(x=quality, y=PA, fill=quality)) +
  geom_boxplot()

# assumptions ####
shapiro.test(c(df$rad_01,df$rad_02,df$rad_03,df$rad_04,df$rad_05,df$rad_06,df$rad_07,df$rad_08,df$rad_09,df$rad_10,df$rad_11,df$rad_12,df$rad_13,df$rad_14,df$rad_15,df$rad_16,df$rad_17,df$rad_18))

# manipulation checks ####
### Mixed ANOVA: perc. groupnorm ~ poolsize*groupnorm(*anonymity)
# every betw.subj. cond. must have observations
# create dedicated df
create_df_perc_gn <- function() {
  frme <- data.frame(c(df$id,df$id),
                     c(df$rad_others_con1,df$rad_others_con2),
                     c(df$ecol,df$ecol),
                     c(rep(1,dim(df)[1]),rep(2,dim(df)[1])),
                     c(df$anon,df$anon))
  names(frme) <- c("id","perc_groupnorm","groupnorm","poolsize","anonymity")
  frme$poolsize <- factor(frme$poolsize)
  frme$groupnorm <- factor(frme$groupnorm)
  frme$anonymity <- factor(frme$anonymity)
  return(frme)
}
df_perc_gn <- create_df_perc_gn()
# Mixed anova
perc_gn_aov <- aov_car(perc_groupnorm ~ groupnorm*anonymity*poolsize + Error(id/poolsize),
                       data = df_perc_gn)
eta_sq(perc_gn_aov)
summary(perc_gn_aov)
# main effects per stage
perc_gn_aov_p1 <- aov(perc_groupnorm ~ groupnorm*anonymity, data=df_perc_gn[df_perc_gn$poolsize==1,])
summary(perc_gn_aov_p1)
perc_gn_aov_p2 <- aov(perc_groupnorm ~ groupnorm*anonymity, data=df_perc_gn[df_perc_gn$poolsize==2,])
summary(perc_gn_aov_p2)
# perceptions means and SD per stage (ecol.eff=1)
mean(df$rad_others_con1[df$ecol == 1]) # stage 1, eff
sd(df$rad_others_con1[df$ecol == 1])
mean(df$rad_others_con1[df$ecol == 0]) # stage 1, inef
sd(df$rad_others_con1[df$ecol == 0])
mean(df$rad_others_con2[df$ecol == 1]) # stage 3, eff
sd(df$rad_others_con2[df$ecol == 1])
mean(df$rad_others_con2[df$ecol == 0]) # stage 3, inef
sd(df$rad_others_con2[df$ecol == 0])

### RM ANOVA: perc. poolsize ~ poolsize(*groupnorm*anonymity)
create_df_perc_ps <- function() {
  frme <- data.frame(c(df$id,df$id),
                     c(df$rad_battery1,df$rad_battery2),
                     c(df$ecol,df$ecol),
                     c(rep(1,dim(df)[1]),rep(2,dim(df)[1])),
                     c(df$anon,df$anon),
                     c(df$PA,df$PA))
  names(frme) <- c("id","perc_poolsize","groupnorm","poolsize","anonymity","PA")
  frme$poolsize <- factor(frme$poolsize)
  frme$groupnorm <- factor(frme$groupnorm)
  frme$anonymity <- factor(frme$anonymity)
  return(frme)
}
df_perc_ps <- create_df_perc_ps()
# Mixed model incl. groupnorm and anonymity
perc_ps_aov <- aov_car(perc_poolsize ~ poolsize+groupnorm+anonymity + Error(id/poolsize),
                       data = df_perc_ps)
summary(perc_ps_aov)
eta_sq(perc_ps_aov)
# means and SD
mean(df$rad_battery1)
sd(df$rad_battery1)
mean(df$rad_battery2)
sd(df$rad_battery2)

# OR
# RM anova only poolsize
aov_ez("id", "perc_poolsize", df_perc_ps, within = c("poolsize"), 
       anova_table=list(correction = "none", es = "none"))

### one-way ANOVA: PA ~ anonymity
anon_aov <- aov(PA ~ anon, data=df)
summary(anon_aov)

### mixed ANOVA: PA ~ ...
# Mixed model incl. groupnorm and anonymity
perc_ps_aov <- aov_car(PA ~ poolsize*groupnorm*anonymity + Error(id/poolsize),
                       data = df_perc_ps)
summary(perc_ps_aov)
# means and SD (1: anon, 0: non-anonymous)
mean(df$PA[df$anon == 0],na.rm=TRUE)
sd(df$PA[df$anon == 0],na.rm=TRUE)
mean(df$PA[df$anon == 1],na.rm=TRUE)
sd(df$PA[df$anon == 1],na.rm=TRUE)


# main effects ####
### mixed ANOVA: consumption ~ poolsize*groupnorm*anonymity
create_df_cons <- function() {
  frme <- data.frame(c(df$id,df$id),
                     c(df$consum1,df$consum3),
                     c(df$ecol,df$ecol),
                     c(rep(1,dim(df)[1]),rep(2,dim(df)[1])),
                     c(df$anon,df$anon),
                     c(df$PA,df$PA),
                     c(df$sex,df$sex),
                     c(df$age,df$age),
                     c(df$edu,df$edu))
  names(frme) <- c("id","consumption","groupnorm","poolsize","anonymity","PA","sex","age","edu")
  #frme$poolsize <- factor(frme$poolsize)
  #frme$groupnorm <- factor(frme$groupnorm)
  #frme$anonymity <- factor(frme$anonymity)
  return(frme)
}
df_cons <- create_df_cons()

# mixed anova
cons_aov <- aov_car(consumption ~ poolsize*groupnorm + Error(id/poolsize),
                       data = df_cons)
summary(cons_aov)
# regression
lmeModel = lmer(consumption ~ groupnorm*PA*poolsize + (1|id), data=df_cons)
summary(lmeModel)
# main w anonymity
cons_aov <- aov_car(consumption ~ poolsize*groupnorm*anonymity + Error(id/poolsize),data = df_cons)
summary(cons_aov)
eta_sq(cons_aov)
### main without anonymity
cons_noanon_aov <- aov_car(consumption ~ poolsize*groupnorm + Error(id/poolsize),data = df_cons)
summary(cons_noanon_aov)
eta_sq(cons_noanon_aov)
# main effects per stage
cons_aov_p1 <- aov(consumption ~ groupnorm*anonymity, data=df_cons[df_cons$poolsize==1,])
summary(cons_aov_p1)
cons_aov_p2 <- aov(consumption ~ groupnorm*anonymity, data=df_cons[df_cons$poolsize==2,])
summary(cons_aov_p2)
# pre-programmed feedback follow t test
t.test(df_cons$consumption[df_cons$groupnorm == 1 & df_cons$poolsize == 1]*50,rep(322,79))
t.test(df_cons$consumption[df_cons$groupnorm == 0 & df_cons$poolsize == 1]*50,rep(185,96))
t.test(df_cons$consumption[df_cons$groupnorm == 1 & df_cons$poolsize == 2]*50,rep(171,79))
t.test(df_cons$consumption[df_cons$groupnorm == 0 & df_cons$poolsize == 2]*50,rep(358,96))

# means table
# all
options(pillar.sigfig = 5)
mt <- group_by(df_cons, groupnorm, poolsize, anonymity) %>%
  summarise(
    count = n(),
    mean = mean(50*consumption, na.rm = TRUE),
    sd = sd(50*consumption, na.rm = TRUE)
  )
# no anonymity
mtnoanon <- group_by(df_cons, groupnorm, poolsize) %>%
  summarise(
    count = n(),
    mean = mean(50*consumption, na.rm = TRUE),
    sd = sd(50*consumption, na.rm = TRUE)
  )
# sd groupnorm:poolsize
sd(df_cons$consumption[df_cons$groupnorm == 1 & df_cons$poolsize == 1]*50)
sd(df_cons$consumption[df_cons$groupnorm == 1 & df_cons$poolsize == 2]*50)
sd(df_cons$consumption[df_cons$groupnorm == 0 & df_cons$poolsize == 1]*50)
sd(df_cons$consumption[df_cons$groupnorm == 0 & df_cons$poolsize == 2]*50)
# total means per row
mean(c(df$consum1[df$ecol == 0 & df$anon == 1] *50,df$consum3[df$ecol == 0 & df$anon == 1] *50)) # anon inef
mean(c(df$consum1[df$ecol == 1 & df$anon == 1] *50,df$consum3[df$ecol == 1 & df$anon == 1] *50)) # anon eff
mean(c(df$consum1[df$ecol == 0 & df$anon == 0] *50,df$consum3[df$ecol == 0 & df$anon == 0] *50)) # nonanon inef
mean(c(df$consum1[df$ecol == 1 & df$anon == 0] *50,df$consum3[df$ecol == 1 & df$anon == 0] *50)) # nonanon eff
# grand means
mean(c(df$consum1[df$ecol == 0 & df$anon == 1] *50,df$consum3[df$ecol == 0 & df$anon == 1] *50,df$consum1[df$ecol == 1 & df$anon == 1] *50,df$consum3[df$ecol == 1 & df$anon == 1] *50)) # anon
mean(c(df$consum1[df$ecol == 0 & df$anon == 0] *50,df$consum3[df$ecol == 0 & df$anon == 0] *50,df$consum1[df$ecol == 1 & df$anon == 0] *50,df$consum3[df$ecol == 1 & df$anon == 0] *50)) # nonanon
# total means per column
mean(df$consum1*50)
mean(df$consum3*50)
# grand grand M
mean(c(df$consum1*50,df$consum3*50))

# plot for visual explanation
mtplot <- mt %>% select(-anonymity) %>% summarize(mm = mean(mean, na.rm = TRUE))
ggplot(mt, aes(x=poolsize, y=mean, color=groupnorm)) + 
  geom_point(size=2)

# mediators ####
create_df_med <- function() {
  frme <- data.frame(c(df$id,df$id),
                     c(df$consum1,df$consum3),
                     c(df$ecol,df$ecol),
                     c(rep(1,dim(df)[1]),rep(2,dim(df)[1])),
                     c(df$anon,df$anon),
                     c(df$svo,df$svo),
                     c(df$tri_01_Efficiency,df$tri_01_Efficiency),
                     c(df$tri_01_Fairness,df$tri_01_Fairness),
                     c(df$tri_01_Profit,df$tri_01_Profit),
                     c(df$rad_21,df$rad_21),
                     c(df$rad_22,df$rad_22),
                     c(df$rad_19,df$rad_19),
                     c(df$rad_20,df$rad_20),
                     c(df$sex,df$sex),
                     c(df$age,df$age),
                     c(df$edu,df$edu),
                     c(df$PA,df$PA))
  names(frme) <- c("id","consumption","groupnorm","poolsize","anonymity","SVO",
                   "efficiency","fairness","profit","resp_self","resp_others",
                   "feeling","group_identity","sex","age","edu","PA")
  frme$poolsize <- factor(frme$poolsize)
  frme$groupnorm <- factor(frme$groupnorm)
  frme$anonymity <- factor(frme$anonymity)
  return(frme)
}
df_med <- create_df_med() 
### mixed ANOVA: consumption ~ poolsize*groupnorm*anonymity*SVO
# mixed model
df_med_svo <- df_med %>% filter(SVO != "non-classifiable")
svo_aov <- aov_car(consumption ~ groupnorm*SVO*poolsize + Error(id/poolsize),
                    data = df_med_svo)
summary(svo_aov)
eta_sq(svo_aov)
# means main
mtsvo <- group_by(df_med_svo, SVO) %>%
  summarise(
    count = n(),
    mean = mean(50*consumption, na.rm = TRUE),
    sd = sd(50*consumption, na.rm = TRUE)
  )
# means interaction: svo:groupnorm:poolsize
mtsvoint <- group_by(df_med_svo, SVO, groupnorm, poolsize) %>%
  summarise(
    count = n(),
    mean = mean(50*consumption, na.rm = TRUE),
    sd = sd(50*consumption, na.rm = TRUE)
  )
mtsvoint <- rbind(mtsvoint[9:12,],mtsvoint[1:4,],mtsvoint[5:8,]) # reorder
levels(mtsvoint$groupnorm) <- c("inef", "eff")
levels(mtsvoint$poolsize) <- c("large", "small")
# plot 4 dims
ggplot(mtsvoint %>% filter(SVO == "individualistic"), aes(x=poolsize, y=mean, color=groupnorm)) + 
  geom_point(size=5) +
  ggtitle("individualistic")
# svo and sex, age, edu
svosex <- group_by(df_med_svo, SVO, sex) %>% # female = 1
  summarise(
    count = n(),
    mean = mean(50*consumption, na.rm = TRUE),
    sd = sd(50*consumption, na.rm = TRUE)
  )
svoage <- group_by(df_med_svo, SVO, age) %>%
  summarise(
    count = n(),
    mean = mean(50*consumption, na.rm = TRUE),
    sd = sd(50*consumption, na.rm = TRUE)
  )
svoedu <- group_by(df_med_svo, SVO, edu) %>%
  summarise(
    count = n(),
    mean = mean(50*consumption, na.rm = TRUE),
    sd = sd(50*consumption, na.rm = TRUE)
  )
# visualize as bar chart
levels(mtsvoint$groupnorm) <- c("inefficient", "efficient")
levels(mtsvoint$poolsize) <- c("large","small")
mtsvoint$cc <- paste(mtsvoint$groupnorm,paste("/",mtsvoint$SVO,sep=""),sep="")
mtsvoint$id <- 1:12
mtsvoint$cc <- factor(mtsvoint$cc, levels = c("inefficient/prosocial",
                                              "efficient/prosocial",
                                              "inefficient/competitive",
                                              "efficient/competitive",
                                              "inefficient/individualistic",
                                              "efficient/individualistic"))
ggplot(mtsvoint, aes(x=poolsize, y=mean, group=id, label=cc, fill=cc)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  scale_fill_manual(values=c("#ff9f80", "#9fdf9f", "#ff4000",
                             "#40bf40","#802000", "#206020")) +
  xlab("Pool size") + 
  ylab("Consumption in Wh") +
  theme(legend.title = element_blank()) +
  theme_apa()

### consumption ~ G*E*F*resp_to_group*resp_from_group(*poolsize*groupnorm*anonymity)
meds_lm <- lm(consumption ~ poolsize*groupnorm*anonymity+efficiency+fairness+
                profit+resp_self+resp_others+feeling+group_identity,
                   data = df_med)
summary(meds_lm)

### consumption ~ responsibility (self towards the group)
lm_resp <- lm(rowMeans(data.frame(df$consum1,df$consum3,df$consum2))*50 ~ df$rad_21+df$rad_22)
summary(lm_resp)
eta_sq(lm_resp)
### consumption ~ G*E*F
create_df_gef <- function() {
  frme <- data.frame(df$id,
                     rowMeans(data.frame(df$consum1,df$consum3,df$consum2)),
                     df$tri_01_Efficiency,
                     df$tri_01_Fairness,
                     df$tri_01_Profit,
                     df$svo)
  names(frme) <- c("id","consumption","efficiency","fairness","profit","svo")
  return(frme)
}
df_gef <- create_df_gef()
# lm on consumption
gef_lm <- lm(consumption ~ efficiency*fairness*profit,data = df_gef)
summary(gef_lm)
# anova on svo
df_gef <- df_gef %>% filter(svo != "non-classifiable")
df_gef <- df_gef[complete.cases(df_gef), ]
df_gef$svo[df_gef$svo == 'prosocial'] <- 0
df_gef$svo[df_gef$svo == 'competitive'] <- 1
df_gef$svo[df_gef$svo == 'individualistic'] <- 2
gef_lm <- lm(svo ~ efficiency*fairness*profit,data = df_gef)
summary(gef_lm)

### mixed ANOVA: others’ behavior ~ poolsize*groupnorm*anonymity
create_df_oth_beh <- function() {
  frme <- data.frame(c(df$id,df$id),
                     c(df$rad_others_beh1,df$rad_others_beh2),
                     c(df$ecol,df$ecol),
                     c(rep(1,dim(df)[1]),rep(2,dim(df)[1])),
                     c(df$anon,df$anon),
                     c(df$svo,df$svo))
  names(frme) <- c("id","others_behavior","groupnorm","poolsize","anonymity","svo")
  frme$poolsize <- factor(frme$poolsize)
  frme$groupnorm <- factor(frme$groupnorm)
  frme$anonymity <- factor(frme$anonymity)
  return(frme)
}
df_oth_beh <- create_df_oth_beh()
# mixed anova
beh_aov <- aov_car(others_behavior ~ poolsize*groupnorm + Error(id/poolsize),
                   data = df_oth_beh)
summary(beh_aov)
### one-way ANOVA: group identity ~ anonymity
group_id_aov <- aov(rad_20 ~ anon, data=df)
summary(group_id_aov)

# PA ~ SVO (Wodzicki, Schwämmlein, Cress, & Kimmerle, 2011)
df$con_mean <- rowMeans(df[,c("consum1","consum2","consum3")])
summary(lm(con_mean ~ svo*PA, data=df %>% filter(svo != "non-classifiable")))

# visualize main hypothesis ####
mtmain <- group_by(df_med, groupnorm, poolsize) %>%
  summarise(
    count = n(),
    mean = mean(50*consumption, na.rm = TRUE),
    sd = sd(50*consumption, na.rm = TRUE)
  )
levels(mtmain$groupnorm) <- c("inefficient", "efficient")
levels(mtmain$poolsize) <- c("large","small")
mtmain$id <- 1:4
ggplot(mtmain, aes(x=poolsize, y=mean, group=id, label=groupnorm, fill=groupnorm)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  scale_fill_manual(values=c("#802000","#ff9f80")) +
  xlab("Pool size") + 
  ylab("Consumption in Wh") +
  theme(legend.title = element_blank()) +
  theme_apa()

df$anon <- factor(df$anon)
levels(df$anon) <- c("non-anonymous","anonymous")
ggplot(df, aes(x=anon, y=PA+1)) + 
  geom_violin(width=1) +
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  xlab("Anonymity level") +
  ylab("Perceived anonymity") +
  theme_apa()
  

