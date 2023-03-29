########################################
# study 4 analysis
# traps vs. sterns muscle humor styles
########################################

#load packages
pckgs <- c('lme4', 'lmerTest', 'car', 'parameters', 'effectsize', 'jtools', 'haven', 'afex',
           'interactions', 'Rmisc', 'psych', 'tidyverse', 'ggpubr', 'ggcorrplot', 'emmeans')

for (i in 1:length(pckgs)) {
  if (!(pckgs[[i]] %in% installed.packages())) {
    install.packages(pckgs[[i]])
  }
  lapply(pckgs[[i]], library, character.only = T)
}

df <- read_sav('study 4/Humor Neck 12-8-21.sav')

#check column names
colnames(df)

#select 'necessary' columns
df <- df %>%
  select(id:Relationship)

#reshape to long
dfL <- df %>%
  gather(key = 'stim', value = 'response',
         SSAffil1:LLSD3) %>%
  separate(col = 'stim', into = c('traps', 'stim'), sep = 1) %>%
  separate(col = 'stim', into = c('stern', 'item'), sep = 1) %>%
  pivot_wider(names_from = 'item', values_from = 'response')

dfLF$humorStyle <- factor(dfLF$humorStyle, levels = c('affiliative', 'selfEnhance', 'aggressive', 'selfDefeat'))

#check single item correlations
corrCheck <- dfL %>%
  select(id, Affil1:SD3) %>%
  group_by(id) %>%
  summarize(
    affil1 = mean(Affil1, na.rm = T),
    affil2 = mean(Affil2, na.rm = T),
    affil3 = mean(Affil3, na.rm = T),
    selfE1 = mean(SE1, na.rm = T),
    selfE2 = mean(SE2, na.rm = T),
    selfE3 = mean(SE3, na.rm = T),
    aggro1 = mean(Agg1, na.rm = T),
    aggro2 = mean(Agg2, na.rm = T),
    aggro3 = mean(Agg3, na.rm = T),
    selfD1 = mean(SD1, na.rm = T),
    selfD2 = mean(SD2, na.rm = T),
    selfD3 = mean(SD3, na.rm = T)
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
dfL$affiliative <- rowMeans(dfL[,8:10], na.rm = T)
dfL$selfEnhance <- rowMeans(dfL[,11:13], na.rm = T)
dfL$aggressive <- rowMeans(dfL[,14:16], na.rm = T)
dfL$selfDefeat <- rowMeans(dfL[,17:19], na.rm = T)


#select final items for analysis 
dfLF <- dfL %>%
  select(id:stern, affiliative:selfDefeat) %>%
  mutate(traps = ifelse(traps == 'L', 'Large', 'Small')) %>%
  mutate(stern = ifelse(stern == 'L', 'Large', 'Small')) %>%
  gather(key = 'humorStyle', value = 'perception',
         affiliative:selfDefeat)

dfLF$humorStyle <- factor(dfLF$humorStyle, levels = c('affiliative', 'selfEnhance', 'aggressive', 'selfDefeat'))

#model
fit1 <- aov_4(perception ~ traps * stern * humorStyle + (traps * stern * humorStyle|id), data = dfLF)
summary(fit1)
fit1$anova_table[5]

#simple effect test of traps:humorStyle
#affiliative
fit2 <- aov_4(perception ~ traps + (traps|id), data = filter(dfLF, humorStyle == 'affiliative'))
summary(fit2)
fit2$anova_table[5]

#self-enhance
fit3 <- aov_4(perception ~ traps + (traps|id), data = filter(dfLF, humorStyle == 'selfEnhance'))
summary(fit3)
fit3$anova_table[5]

#aggressive *
fit4 <- aov_4(perception ~ traps + (traps|id), data = filter(dfLF, humorStyle == 'aggressive'))
summary(fit4)
fit4$anova_table[5]

p4 <- summary(pairs(emmeans(fit4, ~ traps)))
p4
t_to_d(t = p4$t.ratio, df_error = p4$df, paired = T)

#self-defeat
fit5 <- aov_4(perception ~ traps + (traps|id), data = filter(dfLF, humorStyle == 'selfDefeat'))
summary(fit5)
fit5$anova_table[5]


sumG <- Rmisc::summarySE(dfLF,
                         measurevar = 'perception',
                         groupvars = c('traps', 'stern', 'humorStyle'),
                         na.rm = T)
sumG


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

# ggsave('study 4 - humor style correlations.jpg', device = 'jpeg', units = 'cm', path = 'study 4')
