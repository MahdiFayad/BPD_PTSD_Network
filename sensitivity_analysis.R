options(rgl.useNULL = TRUE)
library(Gmisc, quietly = T)
library(glue)
library(htmlTable)
library(grid)
library(qgraph)
library(ggplot2)
library(bootnet) 
library(ggthemes)
library(dplyr)
library(IsingFit)
library(networktools)
library(glmnet)
library(knitr)
library(kableExtra)

####################################################################################
############################### Sensitivity analysis ###############################
####################################################################################
df <-read.csv("data/border_ptsd_JCP.csv")
df <- df[df$TRAUMA==1,]
df <- df[df$FEAR==1,]

sensit_communities <- list("Borderline personality disorder"=1:9,
                           'Post-traumatic stress disorder'=10:28)

sensit_communities_clusters<-list("Borderline personality disorder"=1:9,'PTSD : Traumatic Exposure'=c(10,11),
                                   "PTSD : Intrusion"=c(12:16),"PTSD : Avoidance"=c(17:23),
                                   "PTSD : Cognitive Hyperarousal"=c(24:26),"PTSD : Physical Hyperarousal"=c(27,28))

names<-c('Marked reactivity of mood',
         'Chronic feelings of emptiness',
         'Identity disturbance',
         'Frantic efforts to avoid real or imagined abandonment',
         'Extremes of idealization and devaluation',
         'Impulsivity',
         'Inappropriate anger',
         'Transient paranoid ideation or severe dissociative symptoms',
         'Self harm or suicidal gestures',
         'Experienced, witnessed, or was confronted with traumatic event',
         'Response involved intense fear, helplessness, or horror',
         'Recurrent and intrusive distressing recollections of the event',
         'Recurrent distressing dreams of the event',
         'Acting or feeling as if the traumatic event were recurring',
         'Intense psychological distress at exposure to cues',
         'Physiological reactivity on exposure to cues',
         'Efforts to avoid thoughts, feelings, or conversations',
         'Efforts to avoid activities, places, or people ',
         'Inability to recall aspects of the trauma',
         'Diminished interest or participation in activities',
         'Feelings of detachment or estrangement ',
         'Restricted range of affect',
         'Sense of foreshortened future',
         'Difficulty falling or staying asleep',
         'Irritability or outburst of anger',
         'Difficulty concentrating',
         'Hypervigilance',
         'Exaggerated startle response')

labels<-c('BPD1',
          'BPD2',
          'BPD3',
          'BPD4',
          'BPD5',
          'BPD6',
          'BPD7',
          'BPD8',
          'BPD9',
          'PTSD1.1',
          'PTSD1.2',
          'PTSD2.1',
          'PTSD2.2',
          'PTSD2.3',
          'PTSD2.4',
          'PTSD2.5',
          'PTSD3.1',
          'PTSD3.2',
          'PTSD3.3',
          'PTSD3.4',
          'PTSD3.5',
          'PTSD3.6',
          'PTSD3.7',
          'PTSD4.1',
          'PTSD4.2',
          'PTSD4.3',
          'PTSD4.4',
          'PTSD4.5')


labels_for_plots <- c('MOODSWING', 'EMPTY','IDENTITY','ABANDONMENT', 'RELATINSTAB','IMPULSIVITY','ANGER','PARANOID', 'AUTOAGRESSIVITY',
                      'TRAUMA','FEAR',
                      'MEMORIES','NIGHTMARES','REVIVAL','PSYCH DISTRESS','STIM REACTIVITY','INTERNALAVOID','EXTERNALAVOID','AMNESIA',
                      'INTERESTREDUCT','DETACHMENT','AFFECTRESTRIC','FUTURE','DIFFSLEEPING','IRRITANGER','DIFFCONCENT','HYPERVIGILANCE','STARTLE')

sensit_Ising<-IsingFit(df, maximum=1, minimum=0,AND = TRUE, gamma=0.25)
sensit_q1 <- qgraph(sensit_Ising$weiadj,layout='spring',legend.mode='style2',
                    labels=labels,
                    minimum=0.25,
                    node.width=0.7,node.height=0.7,legend.cex=0.4,
                    groups=sensit_communities_clusters,
                    nodeNames = names, palette = 'colorblind')

sensit_bridge <- bridge(
  sensit_q1,
  communities=sensit_communities,
  useCommunities = 'all',
  directed=NULL
)
plot(sensit_bridge,order = 'value', zscore=F,  
     plotNA = F, include = c("Bridge Expected Influence (1-step)"),color =F)

