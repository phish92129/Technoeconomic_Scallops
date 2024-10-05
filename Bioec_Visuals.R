# Load libraries for analysis

library(readr)
library(tidyr)
library(lubridate)
library(DT)
library(shinyBS)
library(zoo)
library(shinyjs)
library(readxl) # For reading excel tables
library(dplyr) # For data manipulation
library(plyr)  # For data manipulation and to seemingly interfere with dplyr
library(hablar) # For the function 'retype' that assigns values to data frame columns (ie character or numeric)
library(ggplot2) # For graphics 
library(treemapify) # Create a treemap even though donut charts are prettier
library(openxlsx)
library(FinCal)
library(cowplot)

Analysis1 <- read.csv('Output_Analysis_1.csv')

Analysis1$Harvest.Year[Analysis1$Harvest.Year == "Y2"] <- "Summer - Year 2 (2.75-3.0 years Grow Out)"
Analysis1$Harvest.Year[Analysis1$Harvest.Year == "Y3"] <- "Summer - Year 3 (3.75-4.0 years Grow Out)"

# Break Even point for adductor in Scenarios
P1 <- ggplot(Analysis1, aes(x=Product, y = Break.Even.Price..Adductor., color = Grow.Out.Method))  + 
  geom_line(aes(),size = 1.1)+
  facet_grid(~Harvest.Year) +
  theme_minimal() +
  scale_color_brewer(palette = 'Set1') +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ggtitle("") +
  ylab("Price/lb ($)")+
  xlab("")+
  labs(color = "Grow Out Method") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),legend.position = "none")

# Break Even for whole scallop in Scenarios
P2 <- ggplot(Analysis1, aes(x=Product, y = Break.Even.Price..Whole.Scallop., color = Grow.Out.Method))  + 
  geom_line(aes(), size = 1.1)+
  facet_grid(~Harvest.Year) +
  theme_minimal() +
  scale_color_brewer(palette = 'Set1') +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ggtitle("") +
  ylab("Price/Scallop ($)")+
  xlab("Target Scallop Spat (Initial)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),legend.position = "bottom",
        strip.text.x = element_blank() )

plot_grid(P1,P2, nrow = 2, labels = 'AUTO')

ggplot(Analysis1, aes(x=Product, y = Cost.of.Production, color = Grow.Out.Method))  + 
  geom_line(aes())+
  facet_grid(~Harvest.Year) +
  theme_minimal() +
  scale_fill_brewer(palette = 'Set1') +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ggtitle("") +
  ylab("")+
  xlab("")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

ggplot(Analysis1, aes(x=Product, y = Lease.Footprint..Acres., color = Grow.Out.Method))  + 
  geom_line(aes(linetype=Harvest.Year), size = 1.1)+
  theme_minimal() +
  scale_color_brewer(palette = 'Set1') +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  labs(color = 'Grow Out Method', linetype = 'Harvest Timing') +
  ggtitle("") +
  ylab("Lease Size (Acres)")+
  xlab("Target Scallop Spat (Initial)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

cost.breakdown <- Analysis1[,c(2,3,4,5,10,11,12,13,33)]
cost.breakdown <- gather(cost.breakdown,key = 'Category', value = 'Costs', c(5,6,7,8,9))

ggplot(cost.breakdown, aes(x=Product, y = Costs, fill = Category))  + 
  geom_bar(position="stack", stat="identity")+
  theme_minimal() +
  facet_grid(Harvest.Year~Grow.Out.Method)+
  scale_fill_brewer(palette = 'Set1') +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ggtitle("") +
  ylab("")+
  xlab("")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

Analysis2 <- read.csv('Output_Analysis_2.csv')



Analysis2$fac.Product <- as.factor(Analysis2$Product)

ggplot(Analysis2, aes(x=Cohort.Age, y = Break.Even.Price..Adductor., color = fac.Product))  + 
  geom_line()+
  theme_minimal() +
  scale_fill_brewer(palette = 'Set1') +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ggtitle("") +
  ylab("")+
  xlab("")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

Analysis2.sub <- subset(Analysis2, Cohort.Age > 2.7)
Analysis2.sub <- subset(Analysis2.sub, Product == 100000 | Product == 150000 | Product == 200000)
Analysis2.sub$Product <- format(Analysis2.sub$Product, scientific = FALSE)
Analysis2.sub$fac.Product <- as.factor(Analysis2.sub$Product)
Analysis2.sub$season.year <- as.factor(paste(Analysis2.sub$Harvest.Season,Analysis2.sub$Harvest.Year, sep = '-'))
Analysis2.sub$season.year <- factor(Analysis2.sub$season.year, levels = c('Summer-Y2','Fall-Y3','Winter-Y3','Spring-Y3','Summer-Y3','Fall-Y4'))

P3 <- ggplot(Analysis2.sub, aes(x=season.year, y = Break.Even.Price..Adductor., color = fac.Product, group=fac.Product))  + 
  geom_line()+
  theme_minimal() +
  scale_color_brewer(palette = 'Set1') +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ggtitle("") +
  ylab("BE10 (Adductor/lb)")+
  xlab("")+
  labs(color='Starting Scallop Spat')+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),legend.position = "none")

library(dplyr)
Diff.Analysis<- Analysis2.sub %>%
  dplyr::group_by(fac.Product) %>%
  dplyr::arrange(Cohort.Age) %>%
  dplyr::mutate(diff= -(Break.Even.Price..Adductor. - lag(Break.Even.Price..Adductor., default = first(Break.Even.Price..Adductor.))))

P4 <- ggplot(Diff.Analysis, aes(x=season.year, y = diff, color = fac.Product))  + 
  geom_point()+
  geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') +
  theme_minimal() +
  scale_color_brewer(palette = 'Set1') +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ggtitle("") +
  ylab("% Difference in BE10")+
  xlab("Harvest Season")+
  labs(color='Starting Scallop Spat')+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),legend.position = "bottom")

plot_grid(P3,P4, nrow = 2, labels='AUTO')

ggplot(subset(Diff.Analysis, Cohort.Age >3), aes(x=Cohort.Age, y = diff, color = fac.Product))  + 
  geom_line()+
  theme_minimal() +
  scale_fill_brewer(palette = 'Set1') +
  geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ggtitle("") +
  ylab("")+
  xlab("")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

ggplot(Analysis2, aes(x=Cohort.Age, y = Break.Even.Price..Whole.Scallop., color = fac.Product))  + 
  geom_line()+
  theme_minimal() +
  scale_fill_brewer(palette = 'Set1') +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ggtitle("") +
  ylab("")+
  xlab("")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

Analysis3 <- read.csv('Output_Analysis_3.csv')
Analysis3$Product <- as.numeric(format(Analysis3$Product, scientific = FALSE))

options( scipen = 999 )

P5 <- ggplot(Analysis3, aes(x=Product, y = Break.Even.Price..Adductor., color = Scenario,shape = Scenario))  + 
  geom_jitter()+
  theme_minimal() +
  scale_color_brewer(palette = 'Set1') +
  geom_rect(aes(ymin = 21,ymax = 81,xmin = 5000, xmax = 295000), color = 'red', alpha = 0) +
  geom_rect(aes(ymin = 10,ymax = 15,xmin = 705000, xmax = 1045000), color = 'blue', alpha =0) +
  annotate("text", x=150000, y = 18, label = "Small-Scale Farm Operation") +
  annotate("text", x=850000, y = 7, label = "Large-Scale Farm Operation") +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ggtitle("") +
  ylab("BE10 price (Adductor/lb)")+
  xlab("Starting Scallop Spat")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

Analysis3.subL <- subset(Analysis3, Product < 300000)
P6 <- ggplot(Analysis3.subL, aes(x=Product, y = Break.Even.Price..Adductor., color = Scenario,shape = Scenario))  + 
  geom_jitter()+
  theme_minimal() +
  scale_color_brewer(palette = 'Set1') +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ggtitle("") +
  ylab("BE10 price (Adductor/lb)")+
  xlab("Starting Scallop Spat")+
  theme(panel.border = element_rect(colour = "red", fill=NA, size=1), legend.position = "none")

Analysis3.subH <- subset(Analysis3, Product > 700000)
P7 <- ggplot(Analysis3.subH, aes(x=Product, y = Break.Even.Price..Adductor., color = Scenario, shape = Scenario))  + 
  geom_jitter()+
  theme_minimal() +
  scale_color_brewer(palette = 'Set1') +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ggtitle("") +
  ylab("")+
  xlab("Starting Scallop Spat")+
  theme(panel.border = element_rect(colour = "blue", fill=NA, size=1), legend.position = "none")

P8 <- plot_grid(P6,P7, nrow=1)

plot_grid(P5,P8, nrow = 2, labels = 'AUTO')

Analysis4 <- read.csv('Output_Analysis_4.csv')

library("gg3D")
library("viridis")   
SubsetA4 <- subset(Analysis4,IRR.Output!='NA') 

# Alex de Konings farm is not present here
ggplot(SubsetA4, aes(x=Product, y=Price, z=IRR.Output, color = IRR.Output)) + 
  theme_void()+
  scale_color_viridis() +
  axes_3D() +
  stat_3D() 

SubsetA4.Add <- subset(Analysis4,IRR.Output!='NA' & Product.Type == 'Adductor')
ggplot(SubsetA4.Add, aes(x=Product, y = IRR.Output, color = Price))  + 
  geom_point()+
  theme_minimal() +
  facet_grid(Grow.Out.Method~Harvest.Year) +
  geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') +
  scale_color_viridis() +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ggtitle("Internal Rate of Return for Adductor Only Scallop Production Scaled by Price and Production Size") +
  ylab("Internal Rate of Return")+
  xlab("Product")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

ggplot(SubsetA4.Add, aes(x=Product, y = NPV.Output, color = Price))  + 
  geom_point()+
  theme_minimal() +
  facet_grid(Grow.Out.Method~Harvest.Year) +
  geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') +
  scale_color_viridis() +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ggtitle("Internal Rate of Return for Whole Scallop Production Scaled by Price and Production Size") +
  ylab("Internal Rate of Return")+
  xlab("Product")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

SubsetA4.whole <- subset(Analysis4,IRR.Output!='NA' & Product.Type != 'Adductor')
ggplot(SubsetA4.whole, aes(x=Product, y = IRR.Output, color = Price))  + 
  geom_point()+
  theme_minimal() +
  facet_grid(Grow.Out.Method~Harvest.Year) +
  geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') +
  scale_color_viridis() +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ggtitle("Internal Rate of Return for Whole Scallop Production Scaled by Price and Production Size") +
  ylab("Internal Rate of Return")+
  xlab("Product")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

ggplot(SubsetA4.whole, aes(x=Product, y = NPV.Output, color = Price))  + 
  geom_point()+
  theme_minimal() +
  facet_grid(Grow.Out.Method~Harvest.Year) +
  geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') +
  scale_color_viridis() +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ggtitle("") +
  ylab("")+
  xlab("")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

library(plotly)

SubsetA4.Add$Year.GOM <- paste(SubsetA4.Add$Harvest.Year,SubsetA4.Add$Grow.Out.Method)
SubsetA4.Add$Year.GOM <- as.factor(SubsetA4.Add$Year.GOM)

fig <- plot_ly(SubsetA4.Add, x = ~Product, y = ~Price, z = ~IRR.Output, color = ~SubsetA4.Add$Year.GOM, colors = c('#BF382A', '#0C4B8E','green','yellow'),size=.5)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Product'),
                                   yaxis = list(title = 'Price/lbs'),
                                   zaxis = list(title = 'IRR')))
fig
