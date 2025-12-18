
TPP <- read.csv("tpp.csv") #loading data

#rows and columns
nrow(TPP)
ncol(TPP)

table (TPP$Criminal.method) #looking at criminal methods

#distinguishing between violent and nonviolent methods
violent<-c("Armed intimidation/standoff",
           "Chemical or biological weapon deployment", "Explosives",
           "Firearms: civilian", "Firearms: military","Hostage-taking",
           "Unarmed assault", "Vehicle ramming", "Arson", "Other weapons")
nonviolent<-c("Blockading/unlawful assembly", 
              "Blockading/unlawful assembly/unlawful assembly",
              "Perjury/obstruction of justice", "Providing material support", 
              "Threat/Harassment")

#adding new column to label crimes 
TPP$Violence <- NA
TPP$Violence[TPP$Criminal.method %in% violent] <-"Violent"
TPP$Violence[TPP$Criminal.method %in% nonviolent] <-"Nonviolent"

table(TPP$Violence)
table(TPP$Violence, TPP$Gender)

#manual mean calculation based on table data 
(165/2263)*100 #Women make up ~7.29% of all violent crime since 1990
(308/2197)*100 #however, they make up ~14.02% of nonviolent crime

#reformatting dates to make it easier to separate election years
TPP$Date <- as.Date(TPP$Date, format = "%m/%d/%Y")
TPP$Year <- format(TPP$Date, "%Y")

#adding election years
Electionyears<-c("1992","1996","2000","2004","2008","2012","2016","2020","2024")
Elections<-subset(TPP, Year %in% Electionyears)

#looking at values of violent/nonviolent crimes between the genders
table(Gender=TPP$Gender, Violence=TPP$Violence)

#barplot to compare values 
valuesbar<-c(165, 2098,308, 1889)
namesbar<-c("Female Violent", "Male Violent", 
            "Female Nonviolent", "Male Nonviolent")
barplot(valuesbar,
        names=namesbar,
        cex.names=.8,
        main="Frequency of Political Crimes By Gender",
        ylab="Frequency of Crime",
        xlab="Crime Type",
        col=c("rosybrown1","steelblue3","rosybrown1","steelblue3"))

#two separate barplots to look at females/males individually 
fvalues<-c(165, 308)
names<-c("Violent", "Nonviolent")

barplot(fvalues, 
        names=names,
        cex.names=.9,
        main="Female Political Crimes",
        ylab="Frequency of Crime",
        xlab="Crime Type",
        col="rosybrown1")

mvalues<-c(2098,1889)

barplot(mvalues,
        names=names,
        cex.names=.9,
        main="Male Political Crimes",
        ylab="Frequency of Crime",
        xlab="Crime Type",
        col="steelblue3")

#table to look at crime values only during election years
table(Gender=Elections$Gender, Violence=Elections$Violence)

#barplot of election year only crime values 
electionvalues<-c(48, 498,49, 401)

barplot(electionvalues,
        names=namesbar,
        cex.names=.8,
        main="Frequency of Political Crimes By Gender During Election Years",
        ylab="Frequency",
        xlab="Crime Type",
        col=c("rosybrown1","steelblue3","rosybrown1","steelblue3"))

#lineplot to visualize crime values across election years and gender
table(Year = Elections$Year, Gender = Elections$Gender, 
      Violence = Elections$Violence)

Femalenv<-c(0,5,4,2,0,7,4,11,16)
Malenv<-c(0,9,8,46,36,43,81,136,42)

Femalev<-c(0,5,1,2,4,11,5,16,4)
Malev<-c(8,75,56,37,46,59,98,80,39)

plot(x=Electionyears, y=Femalenv, 
     pch=20,
     col="rosybrown1",
     xlab="Year",
     ylab="Frequency of Crime",
     type="l",
     lwd=2,
     ylim=c(0,138),
     las=1,
     xaxt="n",
     main="Political Crimes by Gender during Election Years")
lines(x=Electionyears,
      y=Malenv, col="steelblue3", lwd=2)
lines(x=Electionyears,
      y=Femalev, col="plum4",lwd=2)
lines(x=Electionyears,
      y=Malev,col="olivedrab4",lwd=2)
axis(1, at = Electionyears, 
     labels=Electionyears, las=2)
legend("topleft",
       col=c("rosybrown1", "steelblue3","plum4","olivedrab4"), 
       c("Nonviolent Females","Nonviolent Males","Violent Females",
         "Violent Males"),
       cex = .7,
       lwd=2,
       bty="n")

#female only lineplot
plot(x=Electionyears, y=Femalenv, 
     pch=20,
     col="rosybrown1",
     xlab="Year",
     ylab="Frequency of Crime",
     type="l",
     lwd=2,
     ylim=c(0,18),
     las=1,
     xaxt="n",
     main="Political Crimes by Females during Election Years")
lines(x=Electionyears,
      y=Femalev, col="plum4",lwd=2)
axis(1, at = Electionyears, 
     labels=Electionyears, las=2)
legend("topleft",
       col=c("rosybrown1", "plum4"), 
       c("Nonviolent Females","Violent Females"),
       cex = .7,
       lwd=2,
       bty="n")

#male only line plot 
plot(x=Electionyears, y=Malenv, 
     pch=20,
     col="steelblue3",
     xlab="Year",
     ylab="Frequency of Crime",
     type="l",
     lwd=2,
     ylim=c(0,138),
     las=1,
     xaxt="n",
     main="Political Crimes by Males during Election Years")
lines(x=Electionyears,
      y=Malev,col="olivedrab4",lwd=2)
axis(1, at = Electionyears, 
     labels=Electionyears, las=2)
legend("topleft",
       col=c("steelblue3","olivedrab4"), 
       c("Nonviolent Males","Violent Males"),
       cex = .7,
       lwd=2,
       bty="n")

#All years lineplot 
table(Year = TPP$Year, Gender = TPP$Gender, 
            Violence = TPP$Violence)

Femalenv_general <- c(0,0,0,0,2,0,5,2,2,3,4,2,6,4,2,1,6,5,0,9,9,5,7,2,15,5,4,4,
                      52,73,11,11,31,9,16,2)
    
Malenv_general<-c(0,5,0,2,9,9,9,10,5,14,8,23,68,78,46,84,46,45,36,93,69,
                  57,43,39,44,58,81,76,201,238,136,69,84,57,42,5)

Femalev_general<-c(1,0,0,3,1,3,5,4,3,2,1,6,3,3,2,2,10,0,4,3,3,8,11,5,4,11,5,3,
                   8,11,16,9,2,9,4,0)

Malev_general<-c(15,8,8,44,31,45,75,75,68,102,56,85,64,44,37,20,26,41,46,59,66,
                 76,59,44,51,68,98,73,108,155,80,99,62,64,39,7)


#female only lineplot (all years)

years<-unique(TPP$Year)

plot(x=years, y=Femalenv_general, 
     pch=20,
     col="rosybrown1",
     xlab="Year",
     ylab="Frequency of Crime",
     type="l",
     lwd=2,
     ylim=c(0,75),
     las=1,
     xaxt="n",
     main="Political Crimes by Females since 1990")
lines(x=years,
      y=Femalev_general, col="plum4",lwd=2)
axis(1, at = years, 
     labels=years, las=2)
legend("topleft",
       col=c("rosybrown1", "plum4"), 
       c("Nonviolent Females","Violent Females"),
       cex = .7,
       lwd=2,
       bty="n")

#male only lineplot (all years)
plot(x=years, y=Malenv_general, 
     pch=20,
     col="steelblue3",
     xlab="Year",
     ylab="Frequency of Crime",
     type="l",
     lwd=2,
     ylim=c(0,240),
     las=1,
     xaxt="n",
     main="Political Crimes by Males since 1990")
lines(x=years,
      y=Malev_general,col="olivedrab4",lwd=2)
axis(1, at = years, 
     labels=years, las=2)
legend("topleft",
       col=c("steelblue3","olivedrab4"), 
       c("Nonviolent Males","Violent Males"),
       cex = .7,
       lwd=2,
       bty="n")

#combined lineplot for all years

plot(x=years, y=Femalenv_general, 
     pch=20,
     col="rosybrown1",
     xlab="Year",
     ylab="Frequency of Crime",
     type="l",
     lwd=2,
     ylim=c(0,240),
     las=1,
     xaxt="n",
     main="Political Crimes by Gender since 1990")
lines(x=years,
      y=Malenv_general, col="steelblue3", lwd=2)
lines(x=years,
      y=Femalev_general, col="plum4",lwd=2)
lines(x=years,
      y=Malev_general,col="olivedrab4",lwd=2)
axis(1, at = years, 
     labels=years, las=2)
legend("topleft",
       col=c("rosybrown1", "steelblue3","plum4","olivedrab4"), 
       c("Nonviolent Females","Nonviolent Males","Violent Females",
         "Violent Males"),
       cex = .7,
       lwd=2,
       bty="n")





