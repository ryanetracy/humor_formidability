#########################
# study 1 analysis
# strength humor styles
#########################

#load packages
pckgs <- c('lme4', 'lmerTest', 'car', 'parameters', 'effectsize', 'jtools', 'haven', 'performance',
           'interactions', 'Rmisc', 'psych', 'tidyverse', 'ggpubr', 'ggcorrplot', 'emmeans')

for (i in 1:length(pckgs)) {
  if (!(pckgs[[i]] %in% installed.packages())) {
    install.packages(pckgs[[i]])
  }
  lapply(pckgs[[i]], library, character.only = T)
}

df <- read_sav('study 1/Strength Humor Styles.sav')

#look at column names (have to rename them prior to reshaping)
colnames(df)

#create a dataframe of str variables (manipulation check of perceived strength)
dfStr <- df %>%
  select(id, Strong1STR:Race)

#compute average of strong vs weak targets (manipulation check)
dfStr$strong <- rowMeans(dfStr[,2:5], na.rm = T)
dfStr$weak <- rowMeans(dfStr[,6:9], na.rm = T)

t.test(dfStr$strong, dfStr$weak, paired = T)
effsize::cohen.d(dfStr$strong, dfStr$weak, paired = T)

dfStr %>%
  select(strong, weak) %>%
  apply(2, mean)

dfStr %>%
  select(strong, weak) %>%
  apply(2, sd)

#select raw data columns (i.e., remove averaged items)
df <- df %>%
  select(id:Weak4SD3, Sex:Race)

#create a vector of new column names
newNames <- paste0(
  rep('stim', each = 96),
  rep(1:8, each = 12),
  rep(c('Strg', 'Weak'), each = 48),
  rep(c('Affil','SelfE','Aggro','SelfD'), each = 3),
  rep(1:3)
)

#rename columns
names(df)[2:97] <- newNames


#reshape to long
dfL <- df %>%
  gather(key = 'stimID', value = 'rating',
         stim1StrgAffil1:stim8WeakSelfD3) %>%
  separate(col = 'stimID', into = c('stimID', 'manip'), sep = 5) %>%
  separate(col = 'manip', into = c('strength', 'item'), sep = 4) %>%
  pivot_wider(names_from = 'item', values_from = 'rating')


#check correlations of the items and their alphas
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
dfL$affiliative <- rowMeans(dfL[,7:9], na.rm = T)
dfL$selfEnhance <- rowMeans(dfL[,10:12], na.rm = T)
dfL$aggressive <- rowMeans(dfL[,13:15], na.rm = T)
dfL$selfDefeat <- rowMeans(dfL[,16:18], na.rm = T)

#select final items for analyses
dfLF <- dfL %>%
  select(id:strength, affiliative:selfDefeat) %>%
  mutate(strength = ifelse(strength == 'Strg', 'strong', 'weak')) %>%
  gather(key = 'humorStyle', value = 'perception',
         affiliative:selfDefeat) %>%
  mutate(strC = ifelse(strength == 'strong', 1, -1))

dfLF$humorStyle <- factor(dfLF$humorStyle, levels = c('affiliative', 'selfEnhance', 'aggressive', 'selfDefeat'))

#model 
fit1.0 <- lmer(perception ~ strC * humorStyle + (strC|id), data = dfLF)
#fit1.1 <- lmer(perception ~ strC * humorStyle + (strC * humorStyle|id), data = dfLF)
#fit1.2 <- lmer(perception ~ strC * humorStyle + (humorStyle|id), data = dfLF)
fit1.3 <- lmer(perception ~ strC * humorStyle + (1|id) + (0 + strC|stimID), data = dfLF)
#fit1.4 <- lmer(perception ~ strC * humorStyle + (1|id) + (0 + humorStyle|stimID), data = dfLF)
fit1.5 <- lmer(perception ~ strC * humorStyle + (1|id) + (1|stimID), data = dfLF)

#test fits of models (only those with no fit or convergence issues)
anova(fit1.0, fit1.3, fit1.5)

#effects
anova(fit1.0)
eta_squared(fit1.0)
performance::icc(fit1.0)
performance::r2(fit1.0)

#model random effects and cis
randEffsSum <- function(model) {
  print(summary(model))
  print(confint(model)^2)
  print(icc(model))
  print(r2(model))
}

randEffsSum(fit1.0)

# sjPlot::plot_model(fit1.0, type = 're', sort.est = T, grid = F)

#conduct simple effects tests
#affiliative
fit2 <- lmer(perception ~ strC + (strC|id), data = filter(dfLF, humorStyle == 'affiliative'))
anova(fit2)
eta_squared(fit2)

(p2 <- summary(pairs(emmeans(fit2, ~ strC, lmer.df = 'satterthwaite'))))
t_to_d(t = p2$t.ratio, df_error = p2$df, paired = T)

#self-enhancing
fit3 <- lmer(perception ~ strC + (strC|id), data = filter(dfLF, humorStyle == 'selfEnhance'))
anova(fit3)
eta_squared(fit3)

(p3 <- summary(pairs(emmeans(fit3, ~ strC, lmer.df = 'satterthwaite'))))
t_to_d(t = p3$t.ratio, df_error = p3$df, paired = T)

#aggressive
fit4 <- lmer(perception ~ strC + (strC|id), data = filter(dfLF, humorStyle == 'aggressive'))
anova(fit4)
eta_squared(fit4)

(p4 <- summary(pairs(emmeans(fit4, ~ strC, lmer.df = 'satterthwaite'))))
t_to_d(t = p4$t.ratio, df_error = p4$df, paired = T)


#self-defeating
fit5 <- lmer(perception ~ strC + (strC|id), data = filter(dfLF, humorStyle == 'selfDefeat'))
anova(fit5)
eta_squared(fit5)

(p5 <- summary(pairs(emmeans(fit5, ~ strC, lmer.df = 'satterthwaite'))))
t_to_d(t = p5$t.ratio, df_error = p5$df, paired = T)


#make a violin plot of the effects

#summarize the data (participant level)
dfSum <- summarySE(dfLF,
                   measurevar = 'perception',
                   groupvars = c('id', 'strength', 'humorStyle'),
                   na.rm = T)
#group level
dfSumG <- summarySE(dfLF,
                    measurevar = 'perception',
                    groupvars = c('strength', 'humorStyle'),
                    na.rm = T)

#plot
ggplot(dfSum, aes(humorStyle, perception, fill = strength)) +
  geom_violin(alpha = .2) +
  geom_point(aes(humorStyle, perception, color = strength),
             alpha = .4, size = .5, shape = 7, position = position_jitterdodge(.15, .1, .9)) +
  geom_point(data = dfSumG, aes(humorStyle, perception),
             size = 2, color = 'black', position = position_dodge(.9)) +
  geom_errorbar(data = dfSumG, aes(humorStyle, perception,
                                   ymin = perception - ci, ymax = perception + ci),
                width = .15, color = 'black', alpha = .8, position = position_dodge(.9)) +
  scale_color_manual(values = c('gold', 'navy'),
                     labels = c('Strong', 'Weak')) +
  scale_fill_manual(values = c('gold', 'navy'),
                    labels = c('Strong', 'Weak')) +
  labs(x = '',
       y = 'Humor Style Perception',
       fill = 'Strength',
       color = 'Strength') +
  scale_y_continuous(limits = c(1,7),
                     breaks = seq(1,7,1)) +
  scale_x_discrete(labels = c('Affiliative \nHumor', 'Self-Enhancing \nHumor', 'Aggressive \nHumor', 'Self-Defeating \nHumor')) +
  theme_classic() +
  theme(legend.position = 'bottom') +
  #add indications of significant differences
  annotate(geom = 'text', x = 1:4, y = 6.75, size = 4, label = c('+', '', '***', '***'))

# ggsave('study 1 violin plot.jpg', device = 'jpeg', units = 'cm', path = 'study 1')


# get correlations of the 4 humor styles
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

# ggsave('study 1 - humor style correlations.jpg', device = 'jpeg', units = 'cm', path = 'study 1')


#additional analyses

#rename dfStr columns
strNames <- paste0(
  rep('stim', each = 8),
  rep(1:8)
)

names(dfStr)[2:9] <- strNames

dfStrL <- dfStr %>%
  gather(key = 'stimID', value = 'strRating',
         stim1:stim8) %>%
  select(id, stimID, strRating)

#merge this data with the dfLF dataframe
dfLF2 <- dfLF %>%
  left_join(dfStrL, by = c('stimID' = 'stimID', 'id' = 'id')) %>%
  pivot_wider(names_from = 'humorStyle', values_from = 'perception')


#test effect of perceived strength for each type of humor
#affiliative
t1 <- lmer(affiliative ~ strRating * strC + (1|id) + (1|stimID), data = dfLF2)
summary(t1)

effect_plot(t1,
            pred = 'strRating',
            plot.points = T,
            interval = T,
            int.type = 'confidence',
            x.label = 'Strength Rating',
            y.label = 'Affiliative Humor Capacity',
            jitter = .15,
            colors = 'gold',
            point.color = 'navy',
            point.alpha = .4) +
  scale_y_continuous(limits = c(1,7),
                     breaks = seq(1,7,1)) +
  scale_x_continuous(limits = c(1,7),
                     breaks = seq(1,7,1)) +
  theme_classic()

#self-enhancing
t2 <- lmer(selfEnhance ~ strRating * strC + (1|id) + (1|stimID), data = dfLF2)
summary(t2)

effect_plot(t2,
            pred = 'strRating',
            plot.points = T,
            interval = T,
            int.type = 'confidence',
            x.label = 'Strength Rating',
            y.label = 'Self-Enhancing Humor Capacity',
            jitter = .15,
            colors = 'gold',
            point.color = 'navy',
            point.alpha = .4) +
  scale_y_continuous(limits = c(1,7),
                     breaks = seq(1,7,1)) +
  scale_x_continuous(limits = c(1,7),
                     breaks = seq(1,7,1)) +
  theme_classic()

#aggresive
t3 <- lmer(aggressive ~ strRating * strC + (1|id) + (1|stimID), data = dfLF2)
summary(t3)

effect_plot(t3,
            pred = 'strRating',
            plot.points = T,
            interval = T,
            int.type = 'confidence',
            x.label = 'Strength Rating',
            y.label = 'Aggressive Humor Capacity',
            jitter = .15,
            colors = 'gold',
            point.color = 'navy',
            point.alpha = .4) +
  scale_y_continuous(limits = c(1,7),
                     breaks = seq(1,7,1)) +
  scale_x_continuous(limits = c(1,7),
                     breaks = seq(1,7,1)) +
  theme_classic()

#self-defeating
t4 <- lmer(selfDefeat ~ strRating * strC + (1|id) + (1|stimID), data = dfLF2)
summary(t4)

effect_plot(t4,
            pred = 'strRating',
            plot.points = T,
            interval = T,
            int.type = 'confidence',
            x.label = 'Strength Rating',
            y.label = 'Self-Defeating Humor Capacity',
            jitter = .15,
            colors = 'gold',
            point.color = 'navy',
            point.alpha = .4) +
  scale_y_continuous(limits = c(1,7),
                     breaks = seq(1,7,1)) +
  scale_x_continuous(limits = c(1,7),
                     breaks = seq(1,7,1)) +
  theme_classic()

