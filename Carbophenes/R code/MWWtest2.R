library(tidyverse)
library(dplyr)
library(readxl)
library(glue)
library(broom)
library(ggtext)

data <- read_rds("/Users/neptoon/Desktop/Carbophenes/CLEANED_MELTING_DATA.rds")%>%
  filter(Ffield == "Kowalik", Functional == "Pristine", Size == 2, MD == c("AHNPT"))

kt <- kruskal.test(Temperature ~ N, data=data)
print(kt)

if(kt$p.value < 0.05){
  pt <- pairwise.wilcox.test(data$Temperature, g=data$N, p.adjust.method="BH")
  print(pt)
}

poster.df <- data %>% filter(Ffield == "Kowalik", Functional == "Pristine", Size == 2, MD == c("AHNPT"))

poster.one <- ggplot(poster.df,aes(x=N, y=Temperature, color=MD)) +
  geom_boxplot(aes(fill = MD, colour=MD, fill=MD), outlier.size = 0) +
  geom_jitter(position=dodge, aes(shape=factor(MD)), name="MD",size = I(1.25)) +
  scale_fill_manual(name  = element_blank(), values = c("lightblue", "pink"), labels=c("AHNPT", "NPT")) +
  scale_color_manual(name  = element_blank(), values = c("blue", "red"), labels=c("AHNPT", "NPT")) +
  theme_bw() +
  xlab("N-carbophene") +
  ylab("Temperature [K]") +
  scale_shape_discrete(name  = element_blank(), labels=c("AHNPT", "NPT")) +
  theme(legend.box = "horizontal",
        legend.justification = 'right',
        legend.position="none",
        legend.background = element_rect(size=0.05), 
        legend.title=element_text(size=0),
        legend.text=element_text(size=18),
        axis.text.x = element_text(size=18, color = "black"),
        axis.text.y = element_text(size=18, color = "black"),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        panel.border=element_rect(colour = 'black', fill=NA, size = 1.25),
        panel.grid.major = element_line(colour = 'lightgray', size = 0.5),
        panel.grid.minor = element_line(colour = 'lightgray', size = 0.25),
        panel.background = element_rect(size = 1))

poster.one +
  geom_line(data=tibble(x=c(3, 4.25), y=c(1400, 1400)),
            aes(x=x, y=y),
            inherit.aes=FALSE) +
  geom_line(data=tibble(x=c(2, 4.25), y=c(1475, 1475)),
            aes(x=x, y=y),
            inherit.aes=FALSE) +
  geom_line(data=tibble(x=c(1, 4.25), y=c(1670, 1670)),
            aes(x=x, y=y),
            inherit.aes=FALSE) +
  geom_text(data=tibble(x=3.625, y=1405),
            aes(x=x, y=y, label="*"), size=7,
            inherit.aes=FALSE) +
  geom_text(data=tibble(x=3.125, y=1480),
            aes(x=x, y=y, label="*"), size=7,
            inherit.aes=FALSE) +
  geom_text(data=tibble(x=2.625, y=1675),
            aes(x=x, y=y, label="*"), size=7,
            inherit.aes=FALSE)

poster.one



data.funct <- read_rds("/Users/neptoon/Desktop/Carbophenes/CLEANED_MELTING_DATA.rds")%>%
  filter(Ffield == "Kowalik", N == 3, Size == 2, MD == c("AHNPT"))

ktFunct <- kruskal.test(Temperature ~ Functional, data=data.funct)
print(ktFunct)

if(ktFunct$p.value < 0.05){
  ptFunct <- pairwise.wilcox.test(data.funct$Temperature, g=data.funct$Functional, p.adjust.method = "BH")
  print(ptFunct)
}

row.labels <- c(`Pristine` = "Pristine",`CO` = "CO",`COOH` = "COOH", `NH2` = bquote(NH[2]),`NO2` = bquote(NO[2]),`6` = "OH")

functCorrelation <- ggplot(data.funct,aes(x=Functional, y=Temperature, color=Functional)) +
  geom_boxplot(aes(fill = Functional, colour=Functional, fill=Functional),outlier.size = 0) +
  geom_jitter(position=dodge, aes(shape=factor(Functional)), name="Functional",size = I(1.25)) +
  scale_fill_manual(name  = element_blank(), 
                    values = c("coral", "pink", "lightgreen", "gray", "brown1", "lightblue")) +
  scale_color_manual(name  = element_blank(), 
                     values = c("coral3", "red", "darkgreen", "black", "brown4", "blue")) +
  theme_bw() +
  xlab("Functional") +
  ylab("Temperature [K]") +
  scale_shape_discrete(name  = element_blank()) +
  theme(legend.box = "horizontal",
        legend.justification = 'right',
        legend.position="none",
        legend.background = element_rect(size=0.05), 
        legend.title=element_text(size=0),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size=18, color = "black"),
        axis.text.y = element_text(size=18, color = "black"),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        panel.border=element_rect(colour = 'black', fill=NA, size = 1.25),
        panel.grid.major = element_line(colour = 'lightgray', size = 0.5),
        panel.grid.minor = element_line(colour = 'lightgray', size = 0.25),
        panel.background = element_rect(size = 1))   +
  scale_x_discrete(labels=row.labels)

functCorrelation +
  geom_line(data=tibble(x=c(0.75, 5.25), y=c(1165, 1165)),
            aes(x=x, y=y),
            inherit.aes=FALSE) +
  geom_line(data=tibble(x=c(2.75, 6.25), y=c(1490, 1490)),
            aes(x=x, y=y),
            inherit.aes=FALSE) +
  geom_text(data=tibble(x=2.5, y=1145),
            aes(x=x, y=y, label="n.c."),
            inherit.aes=FALSE) +
  geom_text(data=tibble(x=4.5, y=1510),
            aes(x=x, y=y, label="n.c."),
            inherit.aes=FALSE)

functCorrelation
