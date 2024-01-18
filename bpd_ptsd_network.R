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

############################################################################
##################### THIRD NETWORK ########################################
###################### BPD - PTSD ##########################################
############################################################################
df <-read.csv("data/border_ptsd_JCP.csv")

communities <-list("Borderline personality disorder"=1:9,'Post-traumatic stress disorder'=10:28)
communities_clusters <-list("Borderline personality disorder"=1:9,'PTSD : Traumatic Exposure'=c(10,11),
                            "PTSD : Intrusion"=c(12:16),"PTSD : Avoidance"=c(17:23),"PTSD : Cognitive Hyperarousal"=c(24:26),"PTSD : Physical Hyperarousal"=c(27,28))

bpd_names<-c('Marked reactivity of mood',
             'Chronic feelings of emptiness',
             'Identity disturbance',
             'Frantic efforts to avoid real \nor imagined abandonment',
             'Extremes of idealization and devaluation',
             'Impulsivity',
             'Inappropriate anger',
             'Transient paranoid ideation \nor severe dissociative symptoms',
             'Self harm or suicidal gestures')

bpd_labels<-c('BPD1','BPD2','BPD3','BPD4','BPD5','BPD6','BPD7','BPD8','BPD9')


bpd_labels_for_plots <- c('MOODSWING', 'EMPTY','IDENTITY','ABANDONMENT', 'RELATINSTAB','IMPULSIVITY','ANGER','PARANOID', 'AUTOAGRESSIVITY')

ptsd_names<-c('Experienced, witnessed,\nor was confronted with\ntraumatic event',
              'Response involved\nintense fear, helplessness,\nor horror',
              'Recurrent and intrusive \ndistressing recollections of the event',
              'Recurrent distressing \ndreams of the event',
              'Acting or feeling as if \nthe traumatic event were recurring',
              'Intense psychological \ndistress at exposure to cues',
              'Physiological reactivity \non exposure to cues',
              'Efforts to avoid thoughts,\nfeelings, or conversations',
              'Efforts to avoid activities,\nplaces, or people ',
              'Inability to recall aspects\nof the trauma',
              'Diminished interest \nor participation in activities',
              'Feelings of detachment \nor estrangement ',
              'Restricted range of affect',
              'Sense of foreshortened future',
              'Difficulty falling or staying \nasleep',
              'Irritability or outburst \nof anger',
              'Difficulty concentrating',
              'Hypervigilance',
              'Exaggerated startle \nresponse')

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


bpd_ptsd_names<-c(bpd_names,ptsd_names)
bpd_ptsd_labels<-c(bpd_labels, ptsd_labels)
bpd_ptsd_labels_for_plots <- c(bpd_labels_for_plots,ptsd_labels_for_plots)

# Ising Mode, suited for the binary nature of the data
bpd_ptsd_Ising <-IsingFit(df, maximum=1, minimum=0,AND = TRUE, gamma=0.25)

q1 <- qgraph(bpd_ptsd_Ising$weiadj,layout='spring',legend.mode='style2', legend=T,
             labels=bpd_ptsd_labels,
             minimum=0.25, #GLratio=0.00001,label.cex=1.2,
             node.width=0.8,node.height=0.8,legend.cex=0.3,
             groups=communities_clusters, label.scale.equal = T,
             layoutOffset=c(-0.2,0.2),
             nodeNames = bpd_ptsd_names, palette='colorblind')
bridge <- bridge(
  q1,  communities=communities,
  useCommunities = 'all',
  directed=NULL
)

plot(bridge,order = 'value', zscore=F,  
     plotNA = F, include = c("Bridge Expected Influence (1-step)"),color =F)

# Non-parametric bootstrap
bpd_ptsd_non_parametric_bootstrap <-bootnet(df, nBoots=2, default="IsingFit",type="nonparametric", 
                                            labels=bpd_ptsd_labels, communities = communities,
                                            statistics=c('BridgeExpectedInfluence','edge'),memorysaver = FALSE)


## Plot significant differences between indices
plot(bpd_ptsd_non_parametric_bootstrap,statistics = "BridgeExpectedInfluence",order = 'mean',plot='difference')

pairs <- list()
for (element in bpd_labels){
  for (symptom in ptsd_labels){
    pairs <- append(pairs,paste(element,'--',symptom,sep = ''))
  }
}
plot(bpd_ptsd_non_parametric_bootstrap,split0=T, plot = "interval", order="sample", labels=T,subset=pairs)

## Plot significant edges
plot(bpd_ptsd_non_parametric_bootstrap, c("edge"), plot = "difference",onlyNonZero = T,
     order = "mean",zscore=F,subset=bpd_labels)

# Case-drop bootstrap
bpd_ptsd_case_bootstrap <-bootnet(df, nBoots=1000, default="IsingFit", type="case",
                                  statistics="BridgeExpectedInfluence", communities = communities,
                                  memorysaver = F,labels = bpd_ptsd_labels_for_plots )
summary(bpd_ptsd_case_bootstrap)
bpd_ptsd_stability <- corStability(bpd_ptsd_case_bootstrap)
plot(bpd_ptsd_case_bootstrap,statistics = 'BridgeExpectedInfluence')

#Edge weights (lasso penalized ORs)
bpd_ptsd_summary <- summary(bpd_ptsd_non_parametric_bootstrap)
bpd_ptsd_summary_sorted <- bpd_ptsd_summary[order(-bpd_ptsd_summary$mean), ]
print(exp(bpd_ptsd_summary_sorted[c('mean','CIlower','CIupper')]),n=100)

matrix_bpd_ptsd <-matrix(NA, nrow = length(bpd_ptsd_labels), ncol = length(bpd_ptsd_labels), dimnames = list(bpd_ptsd_labels, bpd_ptsd_labels))
for (i in 1:nrow(bpd_ptsd_LP_ORs)) {
  row <- bpd_ptsd_LP_ORs[i, ]
  matrix_bpd_ptsd[row$node1, row$node2] <- round(exp(row$mean),2)
  matrix_bpd_ptsd[row$node2, row$node1] <- round(exp(row$mean),2)
}

kable(matrix_bpd_ptsd[ptsd_labels,bpd_labels], escape=F, format = "html", digits = 2) %>%
             kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

