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


################################################################################
###################### SECOND NETWORK ##########################################
########################## PTSD ################################################
################################################################################
df <-read.csv("data/border_ptsd_JCP.csv")
ptsd <- subset(df,select=-c(MOODSWING,EMPTY,ABANDONMENT,IDENTITY,INTERPERSONALINSTAB,IMPULSIVITY,ANGER,PARANOID,AUTOAGRESSIVITY))

ptsd_communities <- list("Post-traumatic stress disorder"=1:19)
ptsd_communities_clusters <- list('Exposure'=c(1,2),
                                  "Intrusion"=c(3:7),"Avoidance"=c(8:14),"Cognitive Hyperarousal"=c(15:17),"Physical Hyperarousal"=c(18,19))

ptsd_names<-c('Experienced, witnessed, or was confronted with traumatic event',
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

ptsd_labels<-c('PTSD1.1',
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

ptsd_labels_for_plots <- c('TRAUMA','FEAR',
                           'MEMORIES','NIGHTMARES','REVIVAL','PSYCH DISTRESS','STIM REACTIVITY','INTERNALAVOID','EXTERNALAVOID','AMNESIA',
                           'INTERESTREDUCT','DETACHMENT','AFFECTRESTRIC','FUTURE','DIFFSLEEPING','IRRITANGER','DIFFCONCENT','HYPERVIGILANCE','STARTLE')

# Ising Mode, suited for the binary nature of the data
ptsd_Ising <-IsingFit(ptsd, maximum=1, minimum=0,AND = TRUE, gamma=0.25)

# qgraph network plotting
ptsd_q1 <- qgraph(ptsd_Ising$weiadj,layout='spring',legend.mode='style2',
                  labels=ptsd_labels, legend=F,
                  minimum=0.45, #cut = 0.8, 
                  #GLratio=0.0001,
                  node.width=0.7,node.height=0.7,legend.cex=0.5,
                  groups=ptsd_communities_clusters, label.scale.equal = T,
                  layoutOffset=c(-0.2,0.2),
                  nodeNames = ptsd_names, palette = 'colorblind')

# Plotting Expected Influence Indices for nodes in the network
centralityPlot(ptsd_q1, include=c("ExpectedInfluence"),
               labels = ptsd_names,
               orderBy = 'ExpectedInfluence')

# Non-parametric bootstrap
ptsd_non_parametric_bootstrap <-bootnet(ptsd, nBoots=1000, default="IsingFit",type="nonparametric", 
                                        labels=ptsd_labels,
                                        statistics=c('ExpectedInfluence','edge'),memorysaver = FALSE)

## Plot significant edges
plot(ptsd_non_parametric_bootstrap, c("edge"), plot = "difference",onlyNonZero = T,
     order = "mean",zscore=F)
plot(ptsd_non_parametric_bootstrap, plot = "interval", split0 = TRUE, order="sample", labels=T)

## Plot significant differences between indices
plot(ptsd_non_parametric_bootstrap,statistics = 'expectedInfluence',order = 'mean')

## Test for specific differences between indices
differenceTest(ptsd_non_parametric_bootstrap,measure = "expectedInfluence",x='PTSD1.1',y='PTSD1.2')

# Case-drop bootstrap
ptsd_case_bootstrap <-bootnet(ptsd, nBoots=1000, default="IsingFit", type="case",
                              statistics="ExpectedInfluence",
                              memorysaver = F,labels = ptsd_labels_for_plots )

summary(ptsd_case_bootstrap)
ptsd_stability <- corStability(ptsd_case_bootstrap)
plot(ptsd_case_bootstrap,statistics = 'ExpectedInfluence')

#Edge weights (lasso penalized ORs)
ptsd_summary <- summary(ptsd_non_parametric_bootstrap)
ptsd_summary_sorted <- ptsd_summary[order(-ptsd_summary$mean), ]
exp(ptsd_summary_sorted[c('mean','CIlower','CIupper')])

ptsd_LP_ORs <- summary(ptsd_non_parametric_bootstrap)
matrix_ptsd <-matrix(NA, nrow = length(ptsd_labels), ncol = length(ptsd_labels), dimnames = list(ptsd_labels, ptsd_labels))
for (i in 1:nrow(ptsd_LP_ORs)) {
  row <- ptsd_LP_ORs[i, ]
  matrix_ptsd[row$node1, row$node2] <- round(exp(row$mean),2)
  matrix_ptsd[row$node2, row$node1] <- round(exp(row$mean),2)
}
kable(matrix_ptsd, escape=F, format = "html", digits = 2) %>%
             kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
