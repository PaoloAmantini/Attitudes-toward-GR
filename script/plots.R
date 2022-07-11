#  PLOT:Egalitarian attitudes by countri CI

ggplot(df, aes(x=country, y= mean, group=country, color=country,fill=country)) +       # ggplot2 plot with confidence intervals
  geom_point(shape= 5) +
  geom_errorbar(aes(ymin=un_ci, ymax=up_ci), width=.2,
                position=position_dodge(0.05))+
  xlab("Country")+
  ylab("Egalitarian attitudes")+
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme_bw()+
  theme(legend.position = "bottom")+
  ggtitle("95% confidence intervals")



# distribution of items from ISSP

data1 %>%
  ggplot(aes(x=value,fill=as.factor(color))) +
  geom_bar() +
  scale_fill_brewer(palette = "Set1") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)) +
  xlab("Question agreement scale") +
  ylab("Number of respondent") +
  facet_wrap(~C_ALPHAN)

# distribution of the index I created 

ggplot(dat,aes(x=pub_egat, group=SEX, fill= as.factor(SEX))) +
  geom_density(color="black", alpha=0.6) +
  scale_fill_brewer(palette = "Set2") +
  geom_vline(xintercept = 3.481315,
             color = "red", size=.6)+
  theme_bw()

# attitudes by group of age by country

ggplot(cag_se, aes(x= as.factor(age_gr), y= m_pat, fill= as.factor(age_gr)))+ 
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none")+
  facet_wrap(~C_ALPHAN)

# attitudes by group of age by gender

ggplot(cag_se, aes(x= as.factor(age_gr), y= m_pat, fill= as.factor(age_gr))) + 
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none")+
  facet_wrap(~SEX)

# attirudes in Married couple

ggplot(mar, aes(x= as.factor(MARITAL), y= mar_at, fill= as.factor(SEX))) + 
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none")+
  facet_wrap(~SEX)



