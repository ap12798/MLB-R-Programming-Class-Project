rm(list=ls())

############## This is the start of the Section that loads the ###############
##### data frames. Should go First. Before running be sure to load the #######
##### following files into your working directory: BaseBallHittingYYYY.csv ###
##### and BaseBallPitchingYYYY.csv where YYYY is all the files from 1980 to ###
##### to 2018. These are files to be accumlated into dfhitting and dfpitching##
##### by year and by player. Also load salaries-2016.csv which is the player ##
##### salaries from 1985 through 2015. Also new to load is FanGraphs Leaderboard.csv
##### which may also be used for the wOBP metric. Be sure all these files exist
##### in your working directory first #########################################


# Read Baseball Hitters and Pitchers Files. Will build dfhitting and then dfpitching  as accumulated file.
# Clear them first, so that exists outside of for next loop.

dfhitting<-as.data.frame(NULL)
dfpitching<-as.data.frame(NULL)
# Start reading and Storing in Data Frame dfhitting the Hitting Files. 
# From year:
begyear<-1980
# To Year:
endyear<-2018
#### When adding new files just change the begin year - 
#### or end year(must be in sequence).


# For Next loop to read correct file and store it in df. Then create and 
# Populate Year with this ones year, playerId with the playerId field in Name 
# Column, and Create New Name column with first part of Name Column -
# Missing any * or # signs at end in new Name1 column.
# The nested for loops: The first one is to preform the below for loop twice:
# Once for populating the dfhitting frame with hitters data, and once for 
# populating the dfpicthers frame with pitchers data.
# The url for both dfhitting and dfpitching frames 
# was from: https://www.baseball-reference.com/leaders/batting_avg_leagues.shtml
for (k in c("BaseBallHitting","BaseBallPitching"))
{for (j in begyear:endyear)
{ readfile=paste(k,j,".csv",sep="")
  df<-read.csv(readfile,stringsAsFactors = FALSE)
  df$Year<-j
  df$playerID<-NA
  df$Name1<-NA
  # Below is to split Name field into actual name and player ID separate values(\\ if the separator)
  x<-strsplit(df$Name,"[\\]+") # split df$Name field here!
  if (length(x)>0)
    for (i in 1:length(x))
    { df$playerID[i]<-x[[i]][2]
    df$Name1[i]<-x[[i]][1]
    # below is to remove * and # from end of Name1 fields
    if (substr(df$Name1[i],nchar(df$Name1[i]),nchar(df$Name1[i]))=="*")
      df$Name1[i]<-substr(df$Name1[i],1,nchar(df$Name1[i])-1)
    if (substr(df$Name1[i],nchar(df$Name1[i]),nchar(df$Name1[i]))=="#")
      df$Name1[i]<-substr(df$Name1[i],1,nchar(df$Name1[i])-1)
    }
  filename<-ifelse(k=="BaseBallHitting","dfhitting","dfpitching")
  # Now add df to dfhitting or dfpitching if not 1st year. If first year set dfhitting=df or dfpitching=df.
  if (j!=begyear)
    {if (filename=="dfhitting")
      {dfhitting<-rbind(dfhitting,df) } else {dfpitching<-rbind(dfpitching,df)}
     } else { if (filename=="dfhitting")
      { dfhitting<-df } else {dfpitching<-df} }
}
}

# Now read all the players salaries through 2016 and store in dfsalaries:
# This is from: http://www.seanlahman.com/baseball-archive/statistics/
dfsalaries<-read.csv("salaries-2016.csv",stringsAsFactors = FALSE)

#Now read the dfhitting wOBA metric file for all players for each season from:
# https://www.fangraphs.com/guts.aspx?type=cn 
dfROC<-read.csv("FanGraphs Leaderboard.csv",stringsAsFactors = FALSE)

### Now set the decade in a new Decade column for each of the frame dfsalaries,dfhiiting, and dfpitching
### May be used below in some of the plots 
# Create new Decade field for recording each salary record into a given decade based on the salary year.
dfsalaries$Decade<-NA
# Now populate it.
dfsalaries$Decade<-paste0(substr(dfsalaries$yearID,1,3),"0s")
# Create new Decade field for recording each dfhitting record into a given decade based on the salary year.
dfhitting$Decade<-NA
# Now populate it.
dfhitting$Decade<-paste0(substr(dfhitting$Year,1,3),"0s")
# Create new Decade field for recording each salary record into a given decade based on the salary year.
# Needed for later graphics.
dfpitching$Decade<-NA
# Now populate it.
dfpitching$Decade<-paste0(substr(dfpitching$Year,1,3),"0s")
# Get rid of all players with no at bats for the year(ptchers not hitting mostly) from dfhitting
dfhitting<-subset(dfhitting,AB>0)
#################### End of Data Load Section ######################################################
####################################################################################################


############ Start the Data Manipluation and Calculation Section Here #####
###########################################################################
###########################################################################

# Now calculate and store values in dfhitting for metrics 
dfhitting$RBOE <- (dfhitting$AB + dfhitting$BB - dfhitting$IBB + dfhitting$SF + dfhitting$HBP)
dfhitting$X1B <- dfhitting$H - dfhitting$X2B - dfhitting$X3B - dfhitting$HR
dfhitting$NIBB <- dfhitting$BB - dfhitting$IBB
dfhitting$wOBA <- ((.72*dfhitting$NIBB) + (.75*dfhitting$HBP) + (.9*dfhitting$X1B) + (.92*dfhitting$RBOE) + (1.24*dfhitting$X2B) + (1.56*dfhitting$X3B) + (1.95*dfhitting$HR))/(dfhitting$PA)

# wOBA <- dfROC[1,2]
# wOBAScale <- dfROC[1,3]
# RPA <- dfROC[1,12]





# Set up new average wOBA field in dfhitting which is the league average for that year.
dfhitting$wRC<-NA
# Now use dfhittingmetrics to update majors average wOBA to each hitting record
for (i in 1:nrow(dfhitting))
{ x<-which(dfROC$ï..Season==dfhitting$Year[i])
if (length(x)>0)
{
  wOBA <- dfROC[x[1],2]
  wOBAScale <- dfROC[x[1],3]
  RPA <- dfROC[x[1],12]
  dfhitting$wOBAROC[i]<-wOBA
  dfhitting$wOBAScale[i]<-wOBAScale
  dfhitting$RPA[i]<-RPA
  dfhitting$wRC[i] <- (((dfhitting$wOBA[i] - wOBA)/wOBAScale) + (RPA))*dfhitting$PA[i]
}
}

########### End of Data Manipulation and Calculation Section #####
############################################################################

########### Start of Data lotting Section so far ##############################
###### Note: This may require you install ggrepel and formattable as    #######
###### packages so far. I used them below in several of the plots!!!    #######
#### The four plots so far are: First Plot the average salaries for all #######
#### players for the years from 1985 to 2016. Next is the plot of player
####average salaries by League for 1985 to 2016. Next is the average salary ###
#### by decade, followed by the average salary be decade by League ############

# Now can subset any of dfhitting, dfpitching, and/or dfsalaries by Incontracts
# for evaluation for players in contracts frame! for these Incontracts=="Y"
library(ggplot2)
library(scales)
library(dplyr)
library(ggrepel)
library(formattable)
#Start pdf file. These plots are going into BaseBallSalaries.pdf currently! ####
pdf("BaseballSalaries.pdf")
#### Plot #1 #####
# group salaries frame by salary year.
dfsalaries<-group_by(dfsalaries,yearID)
# Summarize the average salary for all palyers by year.
summ0_sal<-summarize(dfsalaries,avg_sal=mean(salary,na.rm=T),
                     num_sal=n())
# plot it
p<-qplot(yearID,avg_sal,data=summ0_sal,geom="point",xlab="Salary Year",fill=I("black"),size=I(3), # Add a light blue fill
         color = I("black"),      # Make color of rectangles red
         alpha = I(0.9))        # Set 50% transparancy)
p <- p + scale_y_continuous(      # Switch x axis to log (base 10) scale
  name = "Average Salary",     # Set axis title
  labels = dollar,         # Label axis as dollors
  limits = c(0, 4400000)) # Set limits of axis
p <- p + ggtitle("Average Player Salary by Year") # Set overall plot title
p <- p + theme(                 # Change the theme
  plot.title = element_text(  # Alter the text of the plot title
    size = 30,              # Set the font size to 32
    face = "bold",
    colour="blue"))
p<-p+labs(shape = 'League',colour='League')
doll1<-round(summ0_sal[1,2])
doll2<-round(summ0_sal[nrow(summ0_sal),2])
rate1<-((log(doll2$avg_sal/doll1$avg_sal))/31)


line1<-sprintf("Salaries have grown from the 1985 average of %s",currency(doll1, digits = 0L))
line2<-sprintf("to a 2016 level of %s. That's an average annual",currency(doll2,digits=0L))
line3<-sprintf("growth rate of %s",percent(rate1))
p<-p+annotate(geom="text",label=line1,x=1995,y=4000000)
p<-p+annotate(geom="text",label=line2,x=1995,y=3800000)
p<-p+annotate(geom="text",label=line3 ,x=1995,y=3600000)
p<-p+geom_label_repel(aes(label = yearID),
                 box.padding   = 0.35, 
                 point.padding = 0.5,
                 segment.color = 'grey50')
p          
ungroup(df)
### Plot #2 Below ####
# Now group salaries by year by league(American or National)
dfsalaries<-group_by(dfsalaries,yearID,lgID)
# Summarize it for average salary by above grouping
summ0_sal<-summarize(dfsalaries,avg_sal=mean(salary,na.rm=T),
                    num_sal=n())
# Plot it
p<-qplot(yearID,avg_sal,data=summ0_sal,geom="point",xlab="Salary Year",shape = lgID,size=I(3), # Add a light blue fill
         color = lgID,      # Make color of rectangles red
         alpha = I(0.9))        # Set 50% transparancy)
p <- p + scale_y_continuous(      # Switch x axis to log (base 10) scale
  name = "Average Salary",     # Set axis title
  labels = dollar,         # Label axis as dollors
  limits = c(0, 4000000)) # Set limits of axis
p <- p + ggtitle("Average Player Salary by Year by League") # Set overall plot title
# p <- p + scale_x_discrete(      # Switch x axis to log (base 10) scale
#   name = "Salary Year")     # Set axis title
p <- p + theme(                 # Change the theme
  plot.title = element_text(  # Alter the text of the plot title
    size = 18,              # Set the font size to 32
    face = "bold",
    colour="blue"))
p<-p+labs(shape = 'League',colour='League')
p
ungroup(dfsalaries)

#### Plot #3 below #####


# Now group by Decade
dfsalaries<-group_by(dfsalaries,Decade)
# Summarize average salary by decade
summ_sal<-summarize(dfsalaries,avg_sal=mean(salary,na.rm=T),
                    num_sal=n())
summ_sal$avg_sal<-round(summ_sal$avg_sal)
# plot it
p<-qplot(Decade,avg_sal,data=summ_sal,geom="col",fill = I("lightblue"), # Add a light blue fill
         color = I("red"),      # Make color of rectangles red
         alpha = I(0.5))        # Set 50% transparancy)
p <- p + scale_y_continuous(      # Switch x axis to log (base 10) scale
  name = "Average Salary",     # Set axis title
  labels = dollar,         # Label axis as dollorsA
  limits = c(0, 4000000)) # Set limits of axis
p <- p + ggtitle("Average Player Salary by Decade") # Set overall plot title

p <- p + theme(                 # Change the theme
  plot.title = element_text(  # Alter the text of the plot title
    size = 24,              # Set the font size to 32
    face = "bold",
    colour="blue"))
p<-p+geom_label_repel(aes(label =currency(avg_sal,digits=0L)),
                      box.padding   = 0.35, 
                      point.padding = 0.5,
                      segment.color = 'grey50')
p
ungroup(dfsalaries)
#### Plot #4 now ####
# Group by League(American or National) by decade
dfsalaries<-group_by(dfsalaries,lgID,Decade)
# Summarize average salary by that.
summ1_sal<-summarize(dfsalaries,avg_sal=mean(salary,na.rm=T),
                    num_sal=n())
summ1_sal$avg_sal<-round(summ1_sal$avg_sal)
# Plot it
p<-qplot(Decade,avg_sal,data=summ1_sal,geom="col",fill = lgID, # Add a light blue fill
         color = I("red"),      # Make color of rectangles red
         alpha = I(0.5))        # Set 50% transparancy)
p <- p + scale_y_continuous(      # Switch x axis to log (base 10) scale
  name = "Average Salary",     # Set axis title
  labels = dollar,         # Label axis as dollors
  limits = c(0, 8000000)) # Set limits of axis
p <- p + ggtitle("Average Player Salary by Decade by League") # Set overall plot title

p <- p + theme(                 # Change the theme
  plot.title = element_text(  # Alter the text of the plot title
    size = 16,              # Set the font size to 32
    face = "bold",
    colour="blue"))
p<-p+labs(fill = 'League')
p<-p+geom_label_repel(aes(label =currency(avg_sal,digits=0L)),
                      box.padding   = 0.35,
                      point.padding = 0.5,
                      segment.color = 'grey50')
p
ungroup(dfsalaries)
dev.off()
##### End of Plots #######

