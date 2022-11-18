##############################################################################################################################
##################################### Interaction ML Analysis       ##########################################################
##################################### AUTHOR: JMP                   ##########################################################
##################################### DATE: 10.03.2022              ##########################################################
##############################################################################################################################
#--------------------------------------------------------------------------------------------------------------------
#PREPARATION --------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------

#INSTALL AND LOAD PACKAGES
packages <- c("here", "readxl", "ggplot2")
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
}
for (package in packages) {
  library(package, character.only=T)
}

sessionInfo()

#Read in data
if(Sys.info()["nodename"]=="EPIC02G44CTQ05P"|
   Sys.info()["nodename"]=="EPI9TPH7M3"|
   Sys.info()["nodename"]=="LAPTOP-KHT5IA2Q"|
   Sys.info()["nodename"]=="BLACKBEAN"){
  veg <- read_excel(here("data","Histogram 2022-10-05.xlsx"),
                    sheet = "Veg 2way Interaction RDs")
}

if(Sys.info()["nodename"]=="EPIC02G44CTQ05P"|
   Sys.info()["nodename"]=="EPI9TPH7M3"|
   Sys.info()["nodename"]=="LAPTOP-KHT5IA2Q"|
   Sys.info()["nodename"]=="BLACKBEAN"){
  fruit <- read_excel(here("data","Histogram 2022-10-05.xlsx"), 
                      sheet = "Fruit 2way Interact RDs")
}

summary(veg)

# Histogram for veg RDs
p <- ggplot(veg, aes(x=RD*100)) + 
  geom_histogram(binwidth=1, color="black", fill="gainsboro")

# Add ref lines
p2 <- p+ geom_vline(aes(xintercept=-1),
              color="darkgray", linetype="dashed", size=1)

p3 <- p2 + geom_vline(aes(xintercept=-3),
                    color="black", linetype="dashed", size=1)

p4 <- p3 + scale_color_grey() + theme_classic(base_size = 20) +
  theme(legend.position="top") +
  labs(x="No. of excess cases per 100 deliveries", y = "Frequency") +
  theme(axis.text = element_text(size = 20)) 

p4

# Histogram for fruit RDs
f <- ggplot(fruit, aes(x=RD*100)) + 
  geom_histogram(binwidth=1, color="black", fill="gainsboro")

# Add ref lines
f2 <- f+ geom_vline(aes(xintercept=-1),
                    color="darkgray", linetype="dashed", size=1)

f3 <- f2 + geom_vline(aes(xintercept=-2),
                      color="black", linetype="dashed", size=1)

f4 <- f3 + scale_color_grey() + theme_classic(base_size = 20) +
  theme(legend.position="top") +
  labs(x="No. of excess cases per 100 deliveries", y = "Frequency")  +
  theme(axis.text = element_text(size = 20))  

f4