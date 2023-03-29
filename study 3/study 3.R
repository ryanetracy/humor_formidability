#########################
# study 3 analysis
# fwhr humor styles
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

df <- read_sav('study 3/fWHR Humor Style.sav')

#check column names
colnames(df)

#extract 'strong' and 'aggressive' columns (manipulation checks)
dfMan <- df %>%
  select(id, 
         High1Aggressive, High1Strong,
         High2Aggressive, High2Strong,
         High3Aggressive, High3Strong,
         High4Aggressive, High4Strong,
         High5Aggressive, High5Strong,
         High6Aggressive, High6Strong,
         High7Aggressive, High7Strong,
         High8Aggressive, High8Strong,
         High9Aggressive, High9Strong,
         High10Aggressive, High10Strong,
         
         Low1Aggressive, Low1Strong,
         Low2Aggressive, Low2Strong,
         Low3Aggressive, Low3Strong,
         Low4Aggressive, Low4Strong,
         Low5Aggressive, Low5Strong,
         Low6Aggressive, Low6Strong,
         Low7Aggressive, Low7Strong,
         Low8Aggressive, Low8Strong,
         Low9Aggressive, Low9Strong,
         Low10Aggressive, Low10Strong,
         
         Sex:ChildExp)

#rename columns
manNames <- paste0(
  rep('stim', each = 40),
  rep(1:20, each = 2),
  rep(c('Hi', 'Lo'), each = 20),
  rep(c('Agg', 'Str'), each = 1)
)

names(dfMan)[2:41] <- manNames

dfManL <- dfMan %>%
  gather(key = 'stimID', value = 'rating',
         stim1HiAgg:stim20LoStr) %>%
  separate(col = 'stimID', into = c('stimID', 'trait'), sep = -3) %>%
  separate(col = 'stimID', into = c('stimID', 'fwhr'), sep = -2) %>%
  mutate(fwhrC = ifelse(fwhr == 'Hi', 1, -1)) %>%
  pivot_wider(names_from = 'trait', values_from = 'rating')


#manipulation check
#aggressive
aggM1 <- lmer(Agg ~ fwhrC + (1|id) + (1|stimID), data = dfManL)
summary(aggM1)
t_to_d(t = 21.39, df_error = 2223, paired = T)

#strength
strM1 <- lmer(Str ~ fwhrC + (1|id) + (1|stimID), data = dfManL)
summary(strM1)
t_to_d(t = 25.66, df_error = 2222, paired = T)

dfManL %>%
  group_by(fwhr) %>%
  rstatix::get_summary_stats(c(Agg, Str), type = 'mean_sd')

#extract 'necessary' columns
df <- df %>%
  select(id, 
         High1Affil1:High1SD3,
         High2Affil1:High2SD3,
         High3Affil1:High3SD3,
         High4Affil1:High4SD3,
         High5Affil1:High5SD3,
         High6Affil1:High6SD3,
         High7Affil1:High7SD3,
         High8Affil1:High8SD3,
         High9Affil1:High9SD3,
         High10Affil1:High10SD3,
         
         Low1Affil1:Low1SD3,
         Low2Affil1:Low2SD3,
         Low3Affil1:Low3SD3,
         Low4Affil1:Low4SD3,
         Low5Affil1:Low5SD3,
         Low6Affil1:Low6SD3,
         Low7Affil1:Low7SD3,
         Low8Affil1:Low8SD3,
         Low9Affil1:Low9SD3,
         Low10Affil1:Low10SD3,
         
         Sex:ChildExp)


#create vector for new column names
newNames <- paste0(
  rep('stim'),
  rep(1:20, each = 12),
  rep(c('Hi', 'Lo'), each = 120),
  rep(c('Affil','SelfE','Aggro','SelfD'), each = 3),
  rep(1:3)
)

#apply to dataframe
names(df)[2:241] <- newNames


#reshape to long
dfL <- df %>%
  gather(key = 'stimID', value = 'response',
         stim1HiAffil1:stim20LoSelfD3) %>%
  separate(col = 'stimID', into = c('stimID', 'fwhr'), sep = -8) %>%
  separate(col = 'fwhr', into = c('fwhr', 'item'), sep = 2) %>%
  pivot_wider(names_from = 'item', values_from = 'response')


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


#select final items for analysis
dfLF <- dfL %>%
  select(id:fwhr, affiliative:selfDefeat) %>%
  mutate(fwhrC = ifelse(fwhr == 'Hi', 1, -1)) %>%
  gather(key = 'humorStyle', value = 'perception',
         affiliative:selfDefeat)

dfLF$humorStyle <- factor(dfLF$humorStyle, levels = c('affiliative', 'selfEnhance', 'aggressive', 'selfDefeat'))


#fit test models
t0 <- lmer(perception ~ fwhrC * humorStyle + (1|id), data = dfLF)
t1 <- lmer(perception ~ fwhrC * humorStyle + (fwhrC|id), data = dfLF)
#t2 <- lmer(perception ~ fwhrC * humorStyle + (humorStyle|id), data = dfLF)
#t3 <- lmer(perception ~ fwhrC * humorStyle + (fwhrC * humorStyle|id), data = dfLF)
t4 <- lmer(perception ~ fwhrC * humorStyle + (1|id) + (1|stimID), data = dfLF)
t5 <- lmer(perception ~ fwhrC * humorStyle + (fwhrC|id) + (1|stimID), data = dfLF)
#t6 <- lmer(perception ~ fwhrC * humorStyle + (fwhrC|id) + (fwhrC|stimID), data = dfLF)

#test models with no convergence issues
anova(t0, t1, t4, t5)

# sjPlot::plot_model(t5, type = 're', sort.est = T, grid = F)

#model
fit1 <- lmer(perception ~ fwhrC * humorStyle + (fwhrC|id) + (1|stimID), data = dfLF)
anova(fit1)
eta_squared(fit1)

#model random effects and cis
randEffsSum <- function(model) {
  print(summary(model))
  print(confint(model)^2)
  print(icc(model))
  print(r2(model))
}

randEffsSum(fit1)

#model at each level of humor style
#affiliative
fit2 <- lmer(perception ~ fwhrC + (fwhrC|id) + (1|stimID), data = filter(dfLF, humorStyle == 'affiliative'))
summary(fit2)
#confint(fit2)
standardize_parameters(fit2)
t_to_d(t = -1.64, df_error = 18.2185, paired = T)


#self-enhance
fit3 <- lmer(perception ~ fwhrC + (fwhrC|id) + (1|stimID), data = filter(dfLF, humorStyle == 'selfEnhance'))
summary(fit3)
#confint(fit3)
standardize_parameters(fit3)
t_to_d(t = -1.877, df_error = 18.36388, paired = T)


#aggressive
fit4 <- lmer(perception ~ fwhrC + (fwhrC|id) + (1|stimID), data = filter(dfLF, humorStyle == 'aggressive'))
summary(fit4)
#confint(fit4)
standardize_parameters(fit4)
t_to_d(t = 3.363, df_error = 21.27086, paired = T)


#self-defeat
fit5 <- lmer(perception ~ fwhrC + (fwhrC|id) + (1|stimID), data = filter(dfLF, humorStyle == 'selfDefeat'))
summary(fit5)
#confint(fit5)
standardize_parameters(fit5)
t_to_d(t = -2.605, df_error = 18.99123, paired = T)



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

# ggsave('study 3 - humor style correlations.jpg', device = 'jpeg', units = 'cm', path = 'study 3')


#make a violin plot of the effects

#summarize the data (participant level)
dfSum <- summarySE(dfLF,
                   measurevar = 'perception',
                   groupvars = c('id', 'fwhr', 'humorStyle'),
                   na.rm = T)
#group level
dfSumG <- summarySE(dfLF,
                    measurevar = 'perception',
                    groupvars = c('fwhr', 'humorStyle'),
                    na.rm = T)
dfSumG

#plot
ggplot(dfSum, aes(humorStyle, perception, fill = fwhr)) +
  geom_violin(alpha = .2) +
  geom_point(aes(humorStyle, perception, color = fwhr),
             alpha = .4, size = .5, shape = 7, position = position_jitterdodge(.15, .1, .9)) +
  geom_point(data = dfSumG, aes(humorStyle, perception),
             size = 2, color = 'black', position = position_dodge(.9)) +
  geom_errorbar(data = dfSumG, aes(humorStyle, perception,
                                   ymin = perception - ci, ymax = perception + ci),
                width = .15, color = 'black', alpha = .8, position = position_dodge(.9)) +
  scale_color_manual(values = c('gold', 'navy'),
                     labels = c('High \nfWHR', 'Low \nfWHR')) +
  scale_fill_manual(values = c('gold', 'navy'),
                    labels = c('High \nfWHR', 'Low \nfWHR')) +
  scale_y_continuous(limits = c(1, 7),
                     breaks = seq(1,7,1)) +
  labs(x = '',
       y = 'Humor Style Perception',
       fill = 'fWHR',
       color = 'fWHR') +
  scale_x_discrete(labels = c('Affiliative \nHumor', 'Self-Enhancing \nHumor', 'Aggressive \nHumor', 'Self-Defeating \nHumor')) +
  theme_classic() +
  theme(legend.position = 'bottom') +
  annotate(geom = 'text', x = 1:4, y = 6.5, label = c('', '+', '**', '*'))

# ggsave('study 3 violin plot.jpg', device = 'jpeg', units = 'cm', path = 'study 3')



#test effects of continuous variables (aggression, strength)
conts <- dfManL %>%
  select(id, stimID, fwhr, Agg, Str) %>%
  rstatix::convert_as_factor(id, stimID)

dfLCols <- dfL %>%
  select(id, stimID, fwhr, affiliative:selfDefeat) %>%
  rstatix::convert_as_factor(id, stimID)

#merge dataframes
dfLF2 <- conts %>%
  left_join(dfLCols, by = c('id' = 'id', 'stimID' = 'stimID', 'fwhr' = 'fwhr')) %>%
  mutate(fwhrC = ifelse(fwhr == 'Hi', 1, -1))

#affiliative
t1 <- lmer(affiliative ~ fwhrC * Agg * Str + (1|id) + (1|stimID), data = dfLF2)
summary(t1)

#self-enhancing
t2 <- lmer(selfEnhance ~ fwhrC * Agg * Str + (1|id) + (1|stimID), data = dfLF2)
summary(t2)

#aggressive
t3 <- lmer(aggressive ~ fwhrC * Agg * Str + (1|id) + (1|stimID), data = dfLF2)
summary(t3)

#self-defeating
t4 <- lmer(selfDefeat ~ fwhrC * Agg * Str + (1|id) + (1|stimID), data = dfLF2)
summary(t4)

sim_slopes(t4, pred = 'Agg', modx = 'Str', mod2 = 'fwhrC')

interact_plot(t4, 
              pred = 'Agg', 
              modx = 'Str', 
              mod2 = 'fwhrC',
              plot.points = T,
              interval = T,
              int.type = 'confidence',
              jitter = .15,
              point.alpha =.2,
              mod2.labels = c('Low fWHR', 'High fWHR'),
              legend.main = 'Perceived\nStrength',
              x.label = 'Perceived Aggression',
              y.label = 'Self-Defeating Humor Capacity') +
  scale_x_continuous(limits = c(1,7),
                     breaks = seq(1,7,1)) +
  theme_classic()
