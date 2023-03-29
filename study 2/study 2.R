##########################
# study 2 analysis
# body composition humor
##########################

#load packages
pckgs <- c('lme4', 'lmerTest', 'car', 'parameters', 'effectsize', 'jtools', 'haven', 'afex',
           'interactions', 'Rmisc', 'psych', 'tidyverse', 'ggpubr', 'ggcorrplot', 'emmeans')

for (i in 1:length(pckgs)) {
  if (!(pckgs[[i]] %in% installed.packages())) {
    install.packages(pckgs[[i]])
  }
  lapply(pckgs[[i]], library, character.only = T)
}

df <- read_sav('study 2/Body Composition Humor.sav')

#check column names
colnames(df)

#extract 'strong' variables (perceived strength)
dfStr <- df %>%
  select(id, LSStrong, LLStrong, HSStrong, HLStrong, Sex:Relationship)

#rename columns
strNames <- paste0(
  rep('stim', each = 4),
  1:4,
  rep(c('Lo', 'Hi'), each = 2),
  rep(c('Sm', 'Lg'), each = 1)
)

names(dfStr)[2:5] <- strNames

#reshape to long
strL <- dfStr %>%
  gather(key = 'stimID', value = 'strength',
         stim1LoSm:stim4HiLg) %>%
  separate(col = 'stimID', into = c('stimID', 'target'), sep = 5) %>%
  separate(col = 'target', into = c('fat', 'muscle'), sep = 2) %>%
  mutate(fat = ifelse(fat == 'Lo', 'low', 'high')) %>%
  mutate(muscle = ifelse(muscle == 'Sm', 'small', 'large')) 

#manipulation check
test1 <- afex::aov_4(strength ~ fat * muscle + (fat * muscle|id), data = strL, anova_table = list(es = 'pes'))
test1

test2.1 <- afex::aov_4(strength ~ muscle + (muscle|id), data = filter(strL, fat == 'low'), anova_table = list(es = 'pes'))
test2.1

test2.2 <- afex::aov_4(strength ~ muscle + (muscle|id), data = filter(strL, fat == 'high'), anova_table = list(es = 'pes'))
test2.2

strL %>% 
  group_by(fat, muscle) %>%
  rstatix::get_summary_stats(strength, type = 'mean_sd')


#extract 'necessary' columns
df <- df %>%
  select(id, LSAffil1:LSSD3, LLAffil1:LLSD3, HSAffil1:HSSD3, HLAffil1:HLSD3, Sex:Relationship)

#LS = low fat, small muscle
#LL = low fat, large muscle
#HS = high fat, small muscle
#HL = high fat, large muscle
#all targets were male

#create vector for new column names
newNames <- paste0(
  rep('stim'),
  rep(1:4, each = 12),
  rep(c('Lo', 'Hi'), each = 24),
  rep(c('Sm', 'Lg'), each = 12),
  rep(c('Affil','SelfE','Aggro','SelfD'), each = 3),
  rep(1:3)
)

#apply to dataframe
names(df)[2:49] <- newNames


#reshape to long
dfL <- df %>%
  gather(key = 'stimID', value = 'rating',
         stim1LoSmAffil1:stim4HiLgSelfD3) %>%
  separate(col = 'stimID', into = c('stimID', 'hold'), sep = 5) %>%
  separate(col = 'hold', into = c('fat', 'hold'), sep = 2) %>%
  separate(col = 'hold', into = c('muscle', 'item'), sep = 2) %>%
  pivot_wider(names_from = 'item', values_from = 'rating')


#check single item correlations
corrCheck <- dfL %>%
  select(id, Affil1:SelfD3) %>%
  group_by(id) %>%
  summarize(
    affil1 = mean(Affil1, na.rm = T),
    affil2 = mean(Affil2, na.rm = T),
    affil3 = mean(Affil3, na.rm = T),
    selfE1 = mean(SelfE1, na.rm = T),
    selfE2 = mean(SelfE2, na.rm = T),
    selfE3 = mean(SelfE3, na.rm = T),
    aggro1 = mean(Aggro1, na.rm = T),
    aggro2 = mean(Aggro2, na.rm = T),
    aggro3 = mean(Aggro3, na.rm = T),
    selfD1 = mean(SelfD1, na.rm = T),
    selfD2 = mean(SelfD2, na.rm = T),
    selfD3 = mean(SelfD3, na.rm = T)
  )

#affiliation
corr.test(corrCheck[,2:4])
psych::alpha(corrCheck[,2:4])
#self-enhancing
corr.test(corrCheck[,5:7])
psych::alpha(corrCheck[,5:7])
#aggressive
corr.test(corrCheck[,8:10])
psych::alpha(corrCheck[,8:10])
#self-defeating
corr.test(corrCheck[,11:13])
psych::alpha(corrCheck[,11:13])

#get averages of the items for composites
dfL$affiliative <- rowMeans(dfL[,9:11], na.rm = T)
dfL$selfEnhance <- rowMeans(dfL[,12:14], na.rm = T)
dfL$aggressive <- rowMeans(dfL[,15:17], na.rm = T)
dfL$selfDefeat <- rowMeans(dfL[,18:20], na.rm = T)


#select final items for analyses
dfLF <- dfL %>%
  select(id:muscle, affiliative:selfDefeat) %>%
  mutate(fat = ifelse(fat == 'Lo', 'Low', 'High')) %>%
  mutate(muscle = ifelse(muscle == 'Sm', 'Small', 'Large')) %>%
  gather(key = 'humorStyle', value = 'perception',
         affiliative:selfDefeat) %>%
  group_by(id, fat, muscle, humorStyle) %>%
  summarize(
    perception = mean(perception, na.rm =)
  )

dfLF$humorStyle <- factor(dfLF$humorStyle, levels = c('affiliative', 'selfEnhance', 'aggressive', 'selfDefeat'))


#assumptions
#outliers
dfLF %>%
  group_by(fat, muscle, humorStyle) %>%
  rstatix::identify_outliers(perception)

#normality
dfLF %>%
  group_by(fat, muscle, humorStyle) %>%
  rstatix::shapiro_test(perception)


#model
fit1 <- aov_4(perception ~ fat * muscle * humorStyle + (fat * muscle * humorStyle|id), data = dfLF, anova_table = list(es = 'pes'))
summary(fit1)
fit1$anova_table[5]

#check 3-way interaction effects
#affiliative
fit2.1 <- aov_4(perception ~ fat * muscle + (fat * muscle|id), data = filter(dfLF, humorStyle == 'affiliative'), anova_table = list(es = 'pes'))
summary(fit2.1)

#self-enhance
fit2.2 <- aov_4(perception ~ fat * muscle + (fat * muscle|id), data = filter(dfLF, humorStyle == 'selfEnhance'), anova_table = list(es = 'pes'))
summary(fit2.2)
fit2.2$anova_table[5]

dfLF %>%
  filter(humorStyle == 'selfEnhance') %>%
  group_by(fat) %>%
  rstatix::get_summary_stats(perception, type = 'mean_sd')

dfLF %>%
  filter(humorStyle == 'selfEnhance') %>%
  group_by(muscle) %>%
  rstatix::get_summary_stats(perception, type = 'mean_sd')

#aggressive
fit2.3 <- aov_4(perception ~ fat * muscle + (fat * muscle|id), data = filter(dfLF, humorStyle == 'aggressive'), anova_table = list(es = 'pes'))
summary(fit2.3)

#self-defeat
fit2.4 <- aov_4(perception ~ fat * muscle + (fat * muscle|id), data = filter(dfLF, humorStyle == 'selfDefeat'), anova_table = list(es = 'pes'))
summary(fit2.4)
fit2.4$anova_table[5]

#fat:muscle interaction
#small muscle
fit2.41 <- aov_4(perception ~ fat + (fat|id), data = filter(dfLF, humorStyle == 'selfDefeat' & muscle == 'Small'), anova_table = list(es = 'pes'))

(p2.41 <- summary(pairs(emmeans(fit2.41, ~ fat))))
t_to_d(t = p2.41$t.ratio, df_error = p2.41$df, paired = T)

dfLF %>%
  filter(humorStyle == 'selfDefeat' & muscle == 'Small') %>%
  group_by(fat) %>%
  rstatix::get_summary_stats(perception, type = 'mean_sd')

#large muscle
fit2.42 <- aov_4(perception ~ fat + (fat|id), data = filter(dfLF, humorStyle == 'selfDefeat' & muscle == 'Large'), anova_table = list(es = 'pes'))

(p2.42 <- summary(pairs(emmeans(fit2.42, ~ fat))))
t_to_d(t = p2.42$t.ratio, df_error = p2.42$df, paired = T)

dfLF %>%
  filter(humorStyle == 'selfDefeat' & muscle == 'Large') %>%
  group_by(fat) %>%
  rstatix::get_summary_stats(perception, type = 'mean_sd')


# humor style correlations
corrs_df <- dfLF %>%
  group_by(id, humorStyle) %>%
  rstatix::get_summary_stats(perception, type = 'mean') %>%
  select(id, humorStyle, mean) %>%
  pivot_wider(names_from = 'humorStyle', values_from = 'mean')

corrs <- psych::corr.test(corrs_df[,2:5])
corrs

ggcorrplot(corrs$r,
           type = 'lower',
           lab = T,
           p.mat = corrs$p)

# ggsave('study 2 - humor style correlations.jpg', device = 'jpeg', units = 'cm', path = 'study 2')



#make a violin plot of the effects

#summarize the data (participant level)
dfSum <- summarySE(dfLF,
                   measurevar = 'perception',
                   groupvars = c('id', 'fat', 'muscle', 'humorStyle'),
                   na.rm = T)

muscleLabs <- c('Large-Muscle Targets', 'Small-Muscle Targets')
names(muscleLabs) <- c('Large', 'Small')

#group level
dfSumG <- summarySE(dfLF,
                    measurevar = 'perception',
                    groupvars = c('fat', 'muscle', 'humorStyle'),
                    na.rm = T)

dfSumG

annText1 <- data.frame(x = 4, y = 7, lab = '+',
                       fat = factor('High', levels = c('High', 'Low')),
                       muscle = factor('Large', levels = c('Large', 'Small')),
                       humorStyle = factor('selfDefeat', levels = c('affiliative', 'selfEnhance', 'aggressive', 'selfDefeat')))
annText2 <- data.frame(x = 4, y = 7, lab = '*',
                       fat = factor('High', levels = c('High', 'Low')),
                       muscle = factor('Small', levels = c('Large', 'Small')),
                       humorStyle = factor('selfDefeat', levels = c('affiliative', 'selfEnhance', 'aggressive', 'selfDefeat')))



#plot
ggplot(dfSum, aes(humorStyle, perception, fill = fat)) +
  geom_violin(alpha = .2) +
  geom_point(aes(humorStyle, perception, color = fat),
             alpha = .4, size = .5, shape = 7, position = position_jitterdodge(.15, .1, .9)) +
  geom_point(data = dfSumG, aes(humorStyle, perception),
             size = 2, color = 'black', position = position_dodge(.9)) +
  geom_errorbar(data = dfSumG, aes(humorStyle, perception,
                                   ymin = perception - ci, ymax = perception + ci),
                width = .15, color = 'black', alpha = .8, position = position_dodge(.9)) +
  scale_color_manual(values = c('gold', 'navy'),
                     labels = c('High\nFat', 'Low\nFat')) +
  scale_fill_manual(values = c('gold', 'navy'),
                    labels = c('High\nFat', 'Low\nFat')) +
  labs(x = '',
       y = 'Humor Style Perception',
       fill = 'Fat',
       color = 'Fat') +
  scale_y_continuous(limit = c(1,7),
                     breaks = seq(1,7,1)) +
  scale_x_discrete(labels = c('Affiliative \nHumor', 'Self-Enhancing \nHumor', 'Aggressive \nHumor', 'Self-Defeating \nHumor')) +
  theme_classic() +
  facet_grid(~ muscle, labeller = labeller(muscle = muscleLabs)) +
  theme(legend.position = 'bottom') +
  #add indicators of significance
  geom_text(data = annText1, aes(x = x, y = y), label = '*', size = 5) +
  geom_text(data = annText2, aes(x = x, y = y), label = '+', size = 5)

# ggsave('study 2 violin plot.jpg', device = 'jpeg', units = 'cm', path = 'study 2')


#test moderations of strength ratings
#drop redundant columns from strL
strL2 <- strL %>%
  select(id, stimID, strength)

dfLF2 <- dfL %>%
  select(id:muscle, affiliative:selfDefeat) %>%
  mutate(fat = ifelse(fat == 'Lo', 'Low', 'High')) %>%
  mutate(muscle = ifelse(muscle == 'Sm', 'Small', 'Large')) %>%
  gather(key = 'humorStyle', value = 'perception',
         affiliative:selfDefeat) %>%
  left_join(strL2, by = c('id' = 'id', 'stimID' = 'stimID')) %>%
  pivot_wider(names_from = 'humorStyle', values_from = 'perception') %>%
  mutate(musC = ifelse(muscle == 'Large', 1, -1)) %>%
  mutate(fatC = ifelse(fat == 'High', 1, -1))


#test moderations with perceived strength
#affiliative
t1 <- lmer(affiliative ~ musC * fatC * strength + (1|id), data = dfLF2)
summary(t1)

sim_slopes(t1, pred = 'strength', modx = 'fatC')

interact_plot(t1,
              pred = 'strength',
              modx = 'fatC',
              plot.points = T,
              interval = T,
              int.type = 'confidence',
              modx.labels = c('Low', 'High'),
              legend.main = 'Body Fat',
              x.label = 'Strength Rating',
              y.label = 'Affiliative Humor Capacity',
              jitter = .15,
              point.alpha = .4,
              colors = 'blue') +
  scale_y_continuous(limits = c(1,7),
                     breaks = seq(1,7,1)) +
  scale_x_continuous(limits = c(1,7),
                     breaks = seq(1,7,1)) +
  theme_classic() +
  theme(legend.position = 'top')


#self-enhancing
t2 <- lmer(selfEnhance ~ musC * fatC * strength + (1|id), data = dfLF2)
summary(t2)


#aggressive
t3 <- lmer(aggressive ~ musC * fatC * strength + (1|id), data = dfLF2)
summary(t3)


#self-defeating
t4 <- lmer(selfDefeat ~ musC * fatC * strength + (1|id), data = dfLF2)
summary(t4)

sim_slopes(t4, pred = 'strength', modx = 'musC')

interact_plot(t4,
              pred = 'strength',
              modx = 'musC',
              plot.points = T,
              interval = T,
              int.type = 'confidence',
              modx.labels = c('Low', 'High'),
              legend.main = 'Body Fat',
              x.label = 'Strength Rating',
              y.label = 'Self-Defeating Humor Capacity',
              jitter = .15,
              point.alpha = .4,
              colors = 'blue') +
  scale_y_continuous(limits = c(1,7),
                     breaks = seq(1,7,1)) +
  scale_x_continuous(limits = c(1,7),
                     breaks = seq(1,7,1)) +
  theme_classic() +
  theme(legend.position = 'top')


#model contrasts
#specify 3: 
#1: self-oriented vs other-oriented
c1 <- c(-1, 1, -1, 1)
#2: ego-boost vs ego-deplete
c2 <- c(0, 1, 0, -1)
#3: harm vs help
c3 <- c(1, 0, -1, 1)

cMat <- cbind(c1, c2, c3)


#set sum contrasts for fat and muscle variables
dfLFc <- dfLF %>%
  mutate(fatC = ifelse(fat == 'High', 1, -1)) %>%
  mutate(musC = ifelse(muscle == 'Large', 1, -1))

#model contrast effects
cMod1 <- lmer(perception ~ fatC * musC * humorStyle + (1|id), contrasts = list(humorStyle = cMat), data = dfLFc)
summary(cMod1)

sim_slopes(cMod1, pred = 'humorStylec1', modx = 'musC')
sim_slopes(cMod1, pred = 'humorStylec3', modx = 'musC')

