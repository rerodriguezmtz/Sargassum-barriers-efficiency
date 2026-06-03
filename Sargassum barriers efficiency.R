# Code used for manuscript "Evaluating the efficiency of barrier systems for pelagic Sargassum management"

Sys.setenv(TZ='GMT')
rm(list=ls(all=TRUE))
setwd("D:/Articulos/En proceso/001 -  Sargazo barreras/Para R")
BarrEff= read.csv("Data barriers containment.csv", sep = ",")

#Load libraries
library (ggpubr)
library (pgirmess) 
library(plyr)
library(ggplot2)

str(BarrEff)

# Transformations
BarrEff$Country=factor(BarrEff$Country, levels = c("Mexico", "Dominican Republic", "Martinique"))
BarrEff$Site=as.factor(BarrEff$Site)
BarrEff$Month=factor(BarrEff$Month, levels = c("May", "June", "July"))
BarrEff$Date= as.factor(BarrEff$Date)
BarrEff$WD= as.numeric(BarrEff$WD)
BarrEff$Barrier_draft_cm= as.numeric(BarrEff$Barrier_draft_cm)
BarrEff$Barrier_freeboard_cm= as.numeric(BarrEff$Barrier_freeboard_cm)
BarrEff$Barrier_type= as.factor(BarrEff$Barrier_type)


# Sargasso tons per km2 per month per site
TableCountry<- ddply(BarrEff, c("Country"), function(d) {c(
                                                Min = round(min(d$Total_biomass_Tons.km2),2),
                                                Max = round(max(d$Total_biomass_Tons.km2),2),
                                                Mean = round(mean(d$Total_biomass_Tons.km2),2),
                                                SD = round(sd(d$Total_biomass_Tons.km2),2))})  
TableCountry

write.table(TableCountry, "Table Sargassum tons per month per site.csv", quote=F, sep = ",", row.names=F)

# Barriers efficiency per site
TableEfficiency<- ddply(BarrEff, c("Site"), function(d) {c(
  Min = round(min(d$Efficiency_percent),2),
  Max = round(max(d$Efficiency_percent),2),
  Mean = round(mean(d$Efficiency_percent),2),
  SD = round(sd(d$Efficiency_percent),2))})  

TableEfficiency

write.table(TableEfficiency, "Table Efficiency.csv", quote=F, sep = ",", row.names=F)


# Plot biomass (tons/km2) per country
ggboxplot(BarrEff,x = "Country", y = "Total_biomass_Tons.km2", 
          fill= "Country", palette = c("#00AFBB", "#E7B800", "#FC4E07"))+
  theme(legend.position = "none")+
    xlab("Country")+
    ylab("Sargassum biomass (Tons/km2)")

#Plot barrier efficiency (%)
ggerrorplot(BarrEff, x = "Site", y = "Efficiency_percent", desc_stat = "mean_ci", color = "Barrier_type",
            palette="set1", size=1)+ 
  labs(x="Site", y = "Barrier efficiency (%)")+
  geom_hline(yintercept = mean(BarrEff$Efficiency_percent), linetype = 2)+ 
  stat_compare_means(label.y = 115, label.x=5)+        # Add global Kruskal-Wallis p-value
  stat_compare_means(label = "p.signif", ref.group = ".all.", label.y=105)+ 
  theme_grey(base_size=14) + theme(axis.text.x = element_text(angle=0), legend.position = "right") 

#Statistical comparison between countries (Kruskal-Wallis and Wilcoxon)
library (pgirmess) 

kruskal.test(BarrEff$Total_biomass_Tons.km2~BarrEff$Country)
compare_means(Total_biomass_Tons.km2 ~ Country,  data = BarrEff)

kruskal.test(BarrEff$Efficiency_percent~BarrEff$Site) 
res <- compare_means(Efficiency_percent ~ Barrier_type, data = BarrEff)
print(head(res, 36))

## In situ data on Sargassum strandings
insitu= read.csv("Abu-May-July.csv", sep = ",")

str(insitu)

insitu$Year=factor (insitu$Year, levels = c("2018", "2019", "2021", "2022", "2023", "2024", "2025"))
insitu$Site= factor (insitu$Site, levels = c("EEZ", "D", "E", "F"))

ggplot(insitu, aes(x = factor(Year), y = Abundance_t, fill = factor(Year))) +
  geom_boxplot() +
  scale_y_continuous(
    name = expression(italic("Sargassum") ~ "(tons)")
  ) +
  facet_grid(Site ~ ., scales = "free_y") +
  xlab("Year") +
  theme_bw(base_size = 14) +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "none"
  )
