setwd("~Project1-RNotebook/data")

library(haven)
df_init<-read_sav("anes_pilot_2020ets_sav.sav")
df_init_c<-read.csv("anes_pilot_2020ets_csv.csv")


ggplot(data=df_init)+
  geom_bar(mapping=aes(x=marital1))


df_sex_vote<-df_init%>%filter(vote20jb %in% c("1. Donald Trump", "2. Joe Biden")) 
df_sex_vote<-df_init[,c("sex","vote20jb")]
df_sex_vote$vote20jb <- haven::as_factor(df_sex_vote$vote20jb)
df_sex_vote$sex <- haven::as_factor(df_sex_vote$sex)
df_sex_vote<-df_sex_vote%>%filter(vote20jb %in% c("1. Donald Trump", "2. Joe Biden")) 
df_sex_vote_t = df_sex_vote %>%
  group_by(vote20jb,sex) %>%
  summarise(
    n = n()
  ) 
ggplot(data = df_sex_vote_t, aes(x=sex, y=n, group = vote20jb)) +
  geom_col(aes(fill = vote20jb), position = "dodge") +
  geom_text(
    aes(label = n, y = n + 0.5),
    position = position_dodge(0.9),
    vjust = 0
  )

ggplot(data=df_sex_vote)+
  geom_bar(mapping=aes(x=sex),width=0.5)

df_mari_vote<-df_init[,c("marital1","vote20jb")]
df_mari_vote$vote20jb <- haven::as_factor(df_mari_vote$vote20jb)
df_mari_vote$marital1 <- haven::as_factor(df_mari_vote$marital1)
df_mari_vote<-df_mari_vote%>%filter(vote20jb %in% c("1. Donald Trump", "2. Joe Biden")) 

ggplot(data=df_mari_vote)+
  geom_bar(mapping=aes(x=marital1),width=0.5)

summary(df_mari_vote$marital1)

## 3.1 **Question2: What emotions is held by the highest percentage of supporters for Trump and Biden?**
df_emotion<-df_init[,c("hopeful","afraid","outraged","angry","happy","worried","proud",
                       "irritated","nervous","moneyorg","protest","online","persuade","vote20jb")]
# correlation plot of different emotions
library(ggplot2)  
library(RColorBrewer)  
library(reshape2) 
library(corrplot)
library(matlab)

mat <- round(cor(df_emotion), 1)
par(pin=c(9,9))
color<-colorRampPalette(c(brewer.pal(7,"Set1")[2],"white",brewer.pal(7,"Set1")[1]))(100)
corrplot.mixed(mat,order ="alphabet",pch.col = "black",bg = "grey80", lower.col = color, upper.col = color)

# plot the two group distributions
# split the set into two groups
df_trumpe <-  filter(df_emotion, 'vote20jb'==1)
df_bidene <-  filter(df_emotion, 'vote20jb'==2)

ggplot(data=df_trumpe)+ 
  geom_point(mapping = aes(x=proud , y=count(vote20jb)))

library(devtools)
devtools::install_github("dreamRs/esquisse")

library(haven)
library(ggplot2)
# Convert labelled vector to a factor
df_emotion$persuade <- haven::as_factor(df_emotion$persuade)
df_emotion$proud <- haven::as_factor(df_emotion$proud)
df_emotion$happy <- haven::as_factor(df_emotion$happy)
df_emotion$hopeful<- haven::as_factor(df_emotion$hopeful)
df_emotion$outraged<- haven::as_factor(df_emotion$outraged)
df_emotion$angry<- haven::as_factor(df_emotion$angry)
df_emotion$irritated<- haven::as_factor(df_emotion$irritated)
df_emotion$afraid <- haven::as_factor(df_emotion$afraid)
df_emotion$worried <- haven::as_factor(df_emotion$worried)
df_emotion$vote20jb <- haven::as_factor(df_emotion$vote20jb)

library(dplyr)
library(ggplot2)
dot_tibble<-df_emotion %>%
  group_by(vote20jb,proud) %>%
  summarise(
    count = n()
  )
ggplot(data=dot_tibble)+ 
  geom_point(mapping = aes(x=proud , y=count, color=vote20jb))


# df_emotion<-apply(df_emotion,2,as.factor)

p1<-df_emotion %>%
 filter(vote20jb %in% c("1. Donald Trump", "2. Joe Biden")) %>%
 ggplot() +
 aes(x = vote20jb, fill = hopeful) +
 geom_bar(position = "dodge") +
 scale_fill_brewer(palette = "Set2") +
 labs(title = "Hopeful") +
 theme_light()


p2<-df_emotion %>%
 filter(vote20jb %in% c("1. Donald Trump", "2. Joe Biden")) %>%
 ggplot() +
 aes(x = vote20jb, fill = afraid) +
 geom_bar(position = "dodge") +
 scale_fill_brewer(palette = "Set2") +
 labs(title = "Afraid") +
 theme_light()

p3<-df_emotion %>%
 filter(vote20jb %in% c("1. Donald Trump", "2. Joe Biden")) %>%
 ggplot() +
 aes(x = vote20jb, fill = outraged) +
 geom_bar(position = "dodge") +
 scale_fill_brewer(palette = "Set2") +
 labs(title = "Outraged") +
 theme_light()

p4<-df_emotion %>%
 filter(vote20jb %in% c("1. Donald Trump", "2. Joe Biden")) %>%
 ggplot() +
 aes(x = vote20jb, fill = angry) +
 geom_bar(position = "dodge") +
 scale_fill_brewer(palette = "Set2") +
 labs(title = "Angry") +
 theme_light()

p5<-df_emotion %>%
 filter(vote20jb %in% c("1. Donald Trump", "2. Joe Biden")) %>%
 ggplot() +
 aes(x = vote20jb, fill = happy) +
 geom_bar(position = "dodge") +
 scale_fill_brewer(palette = "Set2") +
 labs(title = "Happy") +
 theme_light()

p6<-df_emotion %>%
 filter(vote20jb %in% c("1. Donald Trump", "2. Joe Biden")) %>%
 ggplot() +
 aes(x = vote20jb, fill = worried) +
 geom_bar(position = "dodge") +
 scale_fill_brewer(palette = "Set2") +
 labs(title = "Worried") +
 theme_light()

p7<-df_emotion %>%
 filter(vote20jb %in% c("1. Donald Trump", "2. Joe Biden")) %>%
 ggplot() +
 aes(x = vote20jb, fill = proud) +
 geom_bar(position = "dodge") +
 scale_fill_brewer(palette = "Set2") +
 labs(title = "Proud") +
 theme_light()

p8<-df_emotion %>%
 filter(vote20jb %in% c("1. Donald Trump", "2. Joe Biden")) %>%
 ggplot() +
 aes(x = vote20jb, fill = irritated) +
 geom_bar(position = "dodge") +
 scale_fill_brewer(palette = "Set2") +
 labs(title = "Irritated") +
 theme_light()

p9<-df_emotion %>%
  ggplot() +
  aes(x = persuade, fill = vote20jb) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Persuade") +
  theme_light()

library(gridExtra)
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,nrow=4,ncol=2)


# **Question3: What do supporters of Trump and Biden differ in previous or other voting situations?**
# Sankey plot on voters' choice variations
library(ggalluvial)
library(ggplot2)
df_sankey<-df_init[,c('turnout16a',"vote20bs","cvote2020","vote20jb")]

# label the dataset
library(dplyr)
df_labelled <- df_sankey %>% mutate( Vote20bs = case_when(vote20bs == 1 ~ 'Donald Trump',
                                                            vote20bs == 2 ~ 'Bernie Sanders',
                                                            vote20bs == 3 ~ 'Someone Else',
                                                            vote20bs == 4 ~ 'Probably not vote'))
df_labelled <- df_labelled %>% mutate( Vote20jb = case_when(vote20jb == 1 ~ 'Donald Trump',
                                                           vote20jb == 2 ~ 'Joe Biden',
                                                           vote20jb == 3 ~ 'Someone Else',
                                                           vote20jb == 4 ~ 'Probably not vote'))
df_labelled <- df_labelled %>% mutate( Cvote2020 = case_when(cvote2020 == 1 ~ 'Democrat',
                                                             cvote2020 == 2 ~ 'Republican',
                                                             cvote2020 == 3 ~ 'Other',
                                                             cvote2020 == 4 ~ 'Won’t vote',
                                                           TRUE ~ 'Don’t know'))
df_labelled <- df_labelled %>% mutate( Turnout16a = case_when(turnout16a == 1 ~ 'Definitely voted',
                                                             vote20bs == 2 ~ 'Definitely did not vote',
                                                             vote20bs == 3 ~ 'Not completely sure',
                                                             vote20bs == 4 ~ 'NA: Form 2'))
df_labelled<-df_labelled[,c('Vote20bs','Vote20jb','Cvote2020','Turnout16a')]

df_labelled<-na.omit(df_labelled)
sank_tibble<-df_labelled %>%
  group_by(Vote20jb,Vote20bs,Cvote2020,Turnout16a) %>%
  summarise(
    n = n()
  )
ggplot(data = sank_tibble,
       aes(axis1 = Turnout16a, axis2 = Vote20bs, axis3 = Cvote2020,
           y = n, label = Vote20jb, figsize=750)) +
  scale_x_discrete(limits = c("Turnout16a","Vote20bs", "Cvote2020"), expand = c(.2, .05)) +
  xlab("Voting History") +
  geom_alluvium(aes(fill = Vote20jb)) +
  geom_stratum(alpha = 1) +
  theme_classic()+
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),size = 3.5) +
  theme_minimal() +
  ggtitle("Voting Sankey Chart",
          "stratified by voting history")

# starwars %>%
#   group_by(species) %>%
#   summarise(
#     n = n(),
#     mass = mean(mass, na.rm = TRUE)
#   )

###  race
library(dplyr)

df_race<-df_init[,c("race1_white","race1_black","race1_namer","race1_asian","race1_hpi","vote20jb")]
df_hisp<-df_init_c[,c("latin1","vote20jb")]

# race_tibble<-df_race %>%
#   group_by(race1_white,race1_black,race1_namer,race1_asian,race1_hpi,vote20jb) %>%
#   summarise(
#     count = n()
#   )

df_race1<-df_race%>%mutate( Ethnicity = case_when(race1_white == 1 ~ 'White',
                                              race1_black == 1 ~ 'Black',
                                              race1_asian == 1 ~ 'Asian',
                                              race1_namer == 1 ~ 'American Indian or Alaskan Native',
                                              TRUE ~ 'Native Hawaiian or other Pacific Islander'))
df_race1<-df_race1%>%mutate( VotingChoice = case_when(vote20jb == 1 ~ 'Donald Trump',
                                                  vote20jb == 2 ~ 'Joe Biden',
                                                  vote20jb == 3 ~ 'Someone else',
                                                  TRUE ~ 'Probably not vote'))
df_race1<-filter(df_race1,VotingChoice==c('Donald Trump','Joe Biden'))
df_race1<-df_race1[,c('Ethnicity','VotingChoice')]
race_tibble<-df_race1 %>%
  group_by(Ethnicity,VotingChoice) %>%
  summarise(
    count = n()
  )
# plot
race_tibble$Ethnicity <- factor(race_tibble$Ethnicity)
race_tibble<-melt(race_tibble,id.vars=c('Ethnicity','VotingChoice'))
ggplot(race_tibble, aes(value,Ethnicity,fill=VotingChoice)) +
  geom_line(aes(group = Ethnicity)) +
  geom_point(shape=21,size=3,colour="black")+
  scale_fill_manual(values=c("#00AFBB", "#FC4E07","#36BED9"))+
  theme(
    axis.title=element_text(size=13,face="plain",color="black"),
    axis.text = element_text(size=10,face="plain",color="black"),
    legend.title=element_text(size=12,face="plain",color="black"),
    legend.background = element_blank(),
    legend.position = c(0.85,0.12)
  )

#pie chart of hispanic
df_hisp$vote20jb <- haven::as_factor(df_hisp$vote20jb)
df_hisp$latin1 <- haven::as_factor(df_hisp$latin1)
summary(df_hisp$latin1)
summarise(group_by(df_hisp,vote20jb),count(latin1))

df_pie <- data.frame(value = c(369,2709), 
                 group = c('Latin/Hispanic','Not Latin/Hispanic'))
df_pie <-arrange(df_pie,desc(value))
df_pie$color<-rev(brewer.pal(nrow(df_pie), "Oranges"))
df_pie<-df_pie[c(2:nrow(df_pie),1),]
labs <- paste0(df_pie$group," \n(", round(df_pie$value/sum(df_pie$value)*100,2), "%)")
pie(df_pie$value,labels=labs, init.angle=90,col = df_pie$color,
    border="black")

######draw radar chart on different races of people and their different emotions
##radar setting prepare
coord_radar <- function (theta = "x", start = 0, direction = 1) 
{  theta <- match.arg(theta, c("x", "y"))
r <- if (theta == "x") 
  "y"
else "x"
ggproto("CoordRadar", CoordPolar, theta = theta, r = r, start = start, 
        direction = sign(direction),
        is_linear = function(coord) TRUE)}

# emotions
df_raceemo<-df_init[,c("latin1","race1_white","race1_black","race1_namer","race1_asian","race1_hpi",
                       "hopeful","afraid","outraged","angry","happy","worried","proud",
                       "irritated","vote20jb")]
# hispanics
df_hispemo<-df_raceemo%>%filter(latin1==1)
df_hispemo<-df_hispemo[,c("hopeful","afraid","outraged","angry","happy","worried","proud",
                                              "irritated","vote20jb")]
emotion_trump<-df_hispemo%>%filter(vote20jb==1)
emotion_biden<-df_hispemo%>%filter(vote20jb==2)
count_emotrump<-scale(colSums(emotion_trump[,c("hopeful","afraid","outraged","angry","happy","worried","proud","irritated")]))
count_emobiden<-scale(colSums(emotion_biden[,c("hopeful","afraid","outraged","angry","happy","worried","proud","irritated")]))
# plot radar chart
label_data<-data.frame(
  car=c("hopeful" , "afraid" ,"outraged" ,  "angry" , "happy" , "worried","proud", "irritated"),
  id=c(1:8) ,
  DonaldTrump=count_emotrump,
  JoeBiden=count_emobiden
)
AddRow<-c(NA,nrow(label_data)+1,label_data[1,ncol(label_data)-1],label_data[1,ncol(label_data)])
mydata<-rbind(label_data,AddRow)
myAngle<- 360- 360 * (label_data$id-1) /nrow(label_data)  
mydata<-melt(mydata,id=c("car", "id"))
g1<-ggplot(data=mydata,aes(x=id, y=value,group=variable,fill=variable)) + 
  geom_polygon(colour="black",alpha=0.1)+
  geom_point(size=4,shape=21,color = 'black')+
  coord_radar()+
  labs(title="Radar Chart of the Hispanics' Emotions")+
  #coord_polar() +
  scale_x_continuous(breaks =label_data$id,labels=label_data$car)+
  theme_bw() +
  ylim(-2,2)+
  theme(axis.text.x=element_text(size = 11,colour="black",angle = myAngle),
        axis.title=element_text(size=15,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        panel.grid.major = element_line(color="grey80"),
        axis.line = element_line(color="black"),
        axis.ticks =  element_line(color="black"))


# whites
df_hispemo<-df_raceemo%>%filter(race1_white==1)
df_hispemo<-df_hispemo[,c("hopeful","afraid","outraged","angry","happy","worried","proud",
                          "irritated","vote20jb")]
emotion_trump<-df_hispemo%>%filter(vote20jb==1)
emotion_biden<-df_hispemo%>%filter(vote20jb==2)
count_emotrump<-scale(colSums(emotion_trump[,c("hopeful","afraid","outraged","angry","happy","worried","proud","irritated")]))
count_emobiden<-scale(colSums(emotion_biden[,c("hopeful","afraid","outraged","angry","happy","worried","proud","irritated")]))
# plot radar chart
label_data<-data.frame(
  car=c("hopeful" , "afraid" ,"outraged" ,  "angry" , "happy" , "worried","proud", "irritated"),
  id=c(1:8) ,
  DonaldTrump=count_emotrump,
  JoeBiden=count_emobiden
)
AddRow<-c(NA,nrow(label_data)+1,label_data[1,ncol(label_data)-1],label_data[1,ncol(label_data)])
mydata<-rbind(label_data,AddRow)
myAngle<- 360- 360 * (label_data$id-1) /nrow(label_data)  
mydata<-melt(mydata,id=c("car", "id"))
g2<-ggplot(data=mydata,aes(x=id, y=value,group=variable,fill=variable)) + 
  geom_polygon(colour="black",alpha=0.1)+
  geom_point(size=4,shape=21,color = 'black')+
  coord_radar()+
  labs(title="Radar Chart of the Whites' Emotions")+
  #coord_polar() +
  scale_x_continuous(breaks =label_data$id,labels=label_data$car)+
  theme_bw() +
  ylim(-2,2)+
  theme(axis.text.x=element_text(size = 11,colour="black",angle = myAngle),
        axis.title=element_text(size=15,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        panel.grid.major = element_line(color="grey80"),
        axis.line = element_line(color="black"),
        axis.ticks =  element_line(color="black"))


# asians
df_hispemo<-df_raceemo%>%filter(race1_asian==1)
df_hispemo<-df_hispemo[,c("hopeful","afraid","outraged","angry","happy","worried","proud",
                          "irritated","vote20jb")]
emotion_trump<-df_hispemo%>%filter(vote20jb==1)
emotion_biden<-df_hispemo%>%filter(vote20jb==2)
count_emotrump<-scale(colSums(emotion_trump[,c("hopeful","afraid","outraged","angry","happy","worried","proud","irritated")]))
count_emobiden<-scale(colSums(emotion_biden[,c("hopeful","afraid","outraged","angry","happy","worried","proud","irritated")]))
# plot radar chart
label_data<-data.frame(
  car=c("hopeful" , "afraid" ,"outraged" ,  "angry" , "happy" , "worried","proud", "irritated"),
  id=c(1:8) ,
  DonaldTrump=count_emotrump,
  JoeBiden=count_emobiden
)
AddRow<-c(NA,nrow(label_data)+1,label_data[1,ncol(label_data)-1],label_data[1,ncol(label_data)])
mydata<-rbind(label_data,AddRow)
myAngle<- 360- 360 * (label_data$id-1) /nrow(label_data)  
mydata<-melt(mydata,id=c("car", "id"))
g3<-ggplot(data=mydata,aes(x=id, y=value,group=variable,fill=variable)) + 
  geom_polygon(colour="black",alpha=0.1)+
  geom_point(size=4,shape=21,color = 'black')+
  coord_radar()+
  labs(title="Radar Chart of the Asians' Emotions")+
  #coord_polar() +
  scale_x_continuous(breaks =label_data$id,labels=label_data$car)+
  theme_bw() +
  ylim(-2,2)+
  theme(axis.text.x=element_text(size = 11,colour="black",angle = myAngle),
        axis.title=element_text(size=15,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        panel.grid.major = element_line(color="grey80"),
        axis.line = element_line(color="black"),
        axis.ticks =  element_line(color="black"))

# blacks
df_hispemo<-df_raceemo%>%filter(race1_black==1)
df_hispemo<-df_hispemo[,c("hopeful","afraid","outraged","angry","happy","worried","proud",
                          "irritated","vote20jb")]
emotion_trump<-df_hispemo%>%filter(vote20jb==1)
emotion_biden<-df_hispemo%>%filter(vote20jb==2)
count_emotrump<-scale(colSums(emotion_trump[,c("hopeful","afraid","outraged","angry","happy","worried","proud","irritated")]))
count_emobiden<-scale(colSums(emotion_biden[,c("hopeful","afraid","outraged","angry","happy","worried","proud","irritated")]))
# plot radar chart
label_data<-data.frame(
  car=c("hopeful" , "afraid" ,"outraged" ,  "angry" , "happy" , "worried","proud", "irritated"),
  id=c(1:8) ,
  DonaldTrump=count_emotrump,
  JoeBiden=count_emobiden
)
AddRow<-c(NA,nrow(label_data)+1,label_data[1,ncol(label_data)-1],label_data[1,ncol(label_data)])
mydata<-rbind(label_data,AddRow)
myAngle<- 360- 360 * (label_data$id-1) /nrow(label_data)  
mydata<-melt(mydata,id=c("car", "id"))
g4<-ggplot(data=mydata,aes(x=id, y=value,group=variable,fill=variable)) + 
  geom_polygon(colour="black",alpha=0.1)+
  geom_point(size=4,shape=21,color = 'black')+
  coord_radar()+
  labs(title="Radar Chart of the Blacks' Emotions")+
  #coord_polar() +
  scale_x_continuous(breaks =label_data$id,labels=label_data$car)+
  theme_bw() +
  ylim(-2,2)+
  theme(axis.text.x=element_text(size = 11,colour="black",angle = myAngle),
        axis.title=element_text(size=15,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        panel.grid.major = element_line(color="grey80"),
        axis.line = element_line(color="black"),
        axis.ticks =  element_line(color="black"))

# namer
df_hispemo<-df_raceemo%>%filter(race1_namer==1)
df_hispemo<-df_hispemo[,c("hopeful","afraid","outraged","angry","happy","worried","proud",
                          "irritated","vote20jb")]
emotion_trump<-df_hispemo%>%filter(vote20jb==1)
emotion_biden<-df_hispemo%>%filter(vote20jb==2)
count_emotrump<-scale(colSums(emotion_trump[,c("hopeful","afraid","outraged","angry","happy","worried","proud","irritated")]))
count_emobiden<-scale(colSums(emotion_biden[,c("hopeful","afraid","outraged","angry","happy","worried","proud","irritated")]))
# plot radar chart
label_data<-data.frame(
  car=c("hopeful" , "afraid" ,"outraged" ,  "angry" , "happy" , "worried","proud", "irritated"),
  id=c(1:8) ,
  DonaldTrump=count_emotrump,
  JoeBiden=count_emobiden
)
AddRow<-c(NA,nrow(label_data)+1,label_data[1,ncol(label_data)-1],label_data[1,ncol(label_data)])
mydata<-rbind(label_data,AddRow)
myAngle<- 360- 360 * (label_data$id-1) /nrow(label_data)  
mydata<-melt(mydata,id=c("car", "id"))
g5<-ggplot(data=mydata,aes(x=id, y=value,group=variable,fill=variable)) + 
  geom_polygon(colour="black",alpha=0.1)+
  geom_point(size=4,shape=21,color = 'black')+
  coord_radar()+
  labs(title="Radar Chart of the Native Americans' Emotions")+
  #coord_polar() +
  scale_x_continuous(breaks =label_data$id,labels=label_data$car)+
  theme_bw() +
  ylim(-2,2)+
  theme(axis.text.x=element_text(size = 11,colour="black",angle = myAngle),
        axis.title=element_text(size=15,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        panel.grid.major = element_line(color="grey80"),
        axis.line = element_line(color="black"),
        axis.ticks =  element_line(color="black"))

# native hawaiian or pacific islanders
df_hispemo<-df_raceemo%>%filter(race1_hpi==1)
df_hispemo<-df_hispemo[,c("hopeful","afraid","outraged","angry","happy","worried","proud",
                          "irritated","vote20jb")]
emotion_trump<-df_hispemo%>%filter(vote20jb==1)
emotion_biden<-df_hispemo%>%filter(vote20jb==2)
count_emotrump<-scale(colSums(emotion_trump[,c("hopeful","afraid","outraged","angry","happy","worried","proud","irritated")]))
count_emobiden<-scale(colSums(emotion_biden[,c("hopeful","afraid","outraged","angry","happy","worried","proud","irritated")]))
# plot radar chart
label_data<-data.frame(
  car=c("hopeful" , "afraid" ,"outraged" ,  "angry" , "happy" , "worried","proud", "irritated"),
  id=c(1:8) ,
  DonaldTrump=count_emotrump,
  JoeBiden=count_emobiden
)
AddRow<-c(NA,nrow(label_data)+1,label_data[1,ncol(label_data)-1],label_data[1,ncol(label_data)])
mydata<-rbind(label_data,AddRow)
myAngle<- 360- 360 * (label_data$id-1) /nrow(label_data)  
mydata<-melt(mydata,id=c("car", "id"))
g6<-ggplot(data=mydata,aes(x=id, y=value,group=variable,fill=variable)) + 
  geom_polygon(colour="black",alpha=0.1)+
  geom_point(size=4,shape=21,color = 'black')+
  coord_radar()+
  labs(title="Radar Chart of the Native Hawaiian or Pacific Islanders' Emotions")+
  #coord_polar() +
  scale_x_continuous(breaks =label_data$id,labels=label_data$car)+
  theme_bw() +
  ylim(-2,2)+
  theme(axis.text.x=element_text(size = 11,colour="black",angle = myAngle),
        axis.title=element_text(size=15,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        panel.grid.major = element_line(color="grey80"),
        axis.line = element_line(color="black"),
        axis.ticks =  element_line(color="black"))

library(gridExtra)
grid.arrange(g1,g2,g3,g4,g5,g6,nrow=3,ncol=2)

### financial status
# ggplot(df, aes(x=SOD,y=tau,fill=Class)) + 
#   geom_point(shape=21,size=3,colour="black",stroke=0.25) +
#   scale_fill_manual(values=c("#E7298A","#66A61E","#E6AB02"))+
#   theme(
#     legend.background = element_blank(),
#     legend.position=c(0.8,0.15)
#   )
df_econ<-df_init[,c("finworry","wall7","covid1","covid2","vote20jb")]
library(haven)
library(ggplot2)
# Convert labelled vector to a factor
df_econ$covid1 <- haven::as_factor(df_econ$covid1)
df_econ$covid2 <- haven::as_factor(df_econ$covid2)
df_econ$vote20jb<- haven::as_factor(df_econ$vote20jb)
df_econ$finworry<- haven::as_factor(df_econ$finworry)
df_econ$wall7<- haven::as_factor(df_econ$wall7)


g1<-df_econ %>%
 filter(!(finworry %in% "9. Missing")) %>%
 filter(!(wall7 %in% "9. Missing")) %>%
 
    filter(!(covid1 %in% "9. Missing")) %>%
 filter(!(covid2 %in% "9. Missing")) %>%
 
    filter(vote20jb %in% c("1. Donald Trump", "2. Joe Biden")) %>%
 ggplot() +
 aes(x = covid1, fill = finworry) +
 geom_bar(width = 0.6) +
 scale_fill_brewer(palette = "Dark2") +
 labs(title = "COVID concerns of different financial status") +
 coord_flip() +
 theme_minimal()+
  theme(legend.position = "bottom")
g1

g2<-df_econ %>%
 filter(!(finworry %in% "9. Missing")) %>%
 filter(!(wall7 %in% "9. Missing")) %>%
 
    filter(!(covid1 %in% "9. Missing")) %>%
 filter(!(covid2 %in% "9. Missing")) %>%
 
    filter(vote20jb %in% c("1. Donald Trump", "2. Joe Biden")) %>%
 ggplot() +
 aes(x = covid2, fill = finworry) +
 geom_bar(width = 0.8) +
 scale_fill_brewer(palette = "Dark2") +
 labs(title = "econ-COVID concerns of different financial status") +
 coord_flip() +
 theme_minimal()+
  theme(legend.position = "bottom")
g2

library(gridExtra)
grid.arrange(g1,g2,nrow=1,ncol=2)

df_econ %>%
 filter(!(finworry %in% "9. Missing")) %>%
 filter(!(wall7 %in% "9. Missing")) %>%
 
    filter(!(covid1 %in% "9. Missing")) %>%
 filter(!(covid2 %in% "9. Missing")) %>%
 
    filter(vote20jb %in% c("1. Donald Trump", "2. Joe Biden")) %>%
 ggplot() +
 aes(x = wall7, fill = finworry) +
 geom_bar(width = 0.6) +
 scale_fill_brewer(palette = "Paired") +
 labs(title = "financial status & attitude on Trump's Wall Policy") +
 coord_flip() +
 theme_minimal()

df_econ %>%
 filter(!(finworry %in% "9. Missing")) %>%
 filter(!(wall7 %in% "9. Missing")) %>%
 
    filter(!(covid1 %in% "9. Missing")) %>%
 filter(!(covid2 %in% "9. Missing")) %>%
 
    filter(vote20jb %in% c("1. Donald Trump", "2. Joe Biden")) %>%
 ggplot() +
 aes(x = finworry, fill = vote20jb) +
 geom_bar(width = 0.6) +
 scale_fill_brewer(palette = "Paired") +
 labs(title = "financial status & voting choice") +
 coord_flip() +
 theme_minimal()

df_econ %>%
 filter(!(finworry %in% "9. Missing")) %>%
 filter(!(wall7 %in% "9. Missing")) %>%
 
    filter(!(covid1 %in% "9. Missing")) %>%
 filter(!(covid2 %in% "9. Missing")) %>%
 
    filter(vote20jb %in% c("1. Donald Trump", "2. Joe Biden")) %>%
 ggplot() +
 aes(x = vote20jb, fill = finworry) +
 geom_bar(width=0.5) +
 scale_fill_brewer(palette = "Paired") +
 labs(title = "financial status & voting choice") +
 coord_flip() +
 theme_minimal()





library(viridis)
ggplot(df_econ, aes(x=covid1,y=finworry,fill=vote20jb)) + 
  geom_point(shape=21,size=3,colour="black",stroke=0.25) +
  scale_fill_viridis(option = "viridis",discrete =TRUE)+
  theme(
    legend.background = element_blank(),
    legend.position=c(0.8,0.15)
  )
