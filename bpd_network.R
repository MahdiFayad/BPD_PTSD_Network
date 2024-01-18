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
##################### FIRST NETWORK ########################################
########################## BPD #############################################
############################################################################
df <-read.csv("data/border_ptsd_JCP.csv")
bpd <- subset(df, select = -c(TRAUMA,	FEAR, 
                              MEMORIES,	NIGHTMARES,	FLASHBACKS,	PSYCHDISTRESS,	STIMREACTIVITY,	INTERNALAVOID,	EXTERNALAVOID,	
                              AMNESIA,	INTERESTREDUCT,	DETACHMENT,	AFFECTRESTRIC,	FUTURE,	
                              DIFFSLEEPING,	IRRITANGER,	DIFFCONCENT,	
                              HYPERVIGILANCE,	STARTLE))

bpd_communities <- list("Borderline personality disorder"=1:9)

bpd_names<-c('Marked reactivity of mood',
             'Chronic feelings of emptiness',
             'Identity disturbance',
             'Frantic efforts to avoid real or imagined abandonment',
             'Extremes of idealization and devaluation',
             'Impulsivity',
             'Inappropriate anger',
             'Transient paranoid ideation or severe dissociative symptoms',
             'Self harm or suicidal gestures')

bpd_labels<-c('BPD1','BPD2','BPD3','BPD4','BPD5','BPD6','BPD7','BPD8','BPD9')


bpd_labels_for_plots <- c('MOODSWING', 'EMPTY','IDENTITY','ABANDONMENT', 'RELATINSTAB','IMPULSIVITY','ANGER','PARANOID', 'AUTOAGRESSIVITY')

# Ising Mode, suited for the binary nature of the data
bpd_Ising <-IsingFit(bpd, maximum=1, minimum=0,AND = TRUE, gamma=0.25)

# qgraph network plotting
bpd_q1 <- qgraph(bpd_Ising$weiadj,layout='spring',legend.mode='style2',
                 labels=bpd_labels,
                 minimum=0.6, #cut = 0.8,
                 node.width=0.7,node.height=0.7,legend.cex=0.4,
                 groups=bpd_communities, label.scale.equal = T,
                 layoutOffset=c(-0.169,0.2),
                 nodeNames = bpd_names, palette = 'colorblind')

# Plotting Expected Influence Indices for nodes in the network
centralityPlot(bpd_q1, include=c("ExpectedInfluence"),
               labels = bpd_names,
               orderBy = 'ExpectedInfluence')

# Non-parametric bootstrap
bpd_non_parametric_bootstrap <-bootnet(bpd, nBoots=1000, default="IsingFit",type="nonparametric", 
                                       labels=bpd_labels,
                                       statistics=c('ExpectedInfluence','edge'),memorysaver = FALSE)

## Plot significant edges
plot(bpd_non_parametric_bootstrap, c("edge"), plot = "difference",onlyNonZero = T,
     order = "mean",zscore=F)
plot(bpd_non_parametric_bootstrap, plot = "interval", split0 = TRUE, order="sample", labels=T)

bpd_non_parametric_bootstrap$bootTable
## Plot significant differences between indices
plot(bpd_non_parametric_bootstrap,statistics = 'ExpectedInfluence',order = 'mean')
## Test for specific differences between indices
differenceTest(bpd_non_parametric_bootstrap,measure = "expectedInfluence",x='BPD1',y='BPD2')


# Case-drop bootstrap
bpd_case_bootstrap <-bootnet(bpd, nBoots=1000, default="IsingFit", type="case",
                             statistics="ExpectedInfluence",
                             memorysaver = F,labels = bpd_labels_for_plots )
summary(bpd_case_bootstrap)
bpd_stability <- corStability(bpd_case_bootstrap)
plot(bpd_case_bootstrap,statistics = 'ExpectedInfluence')

#Edge weights (lasso penalized ORs)
bpd_summary <- summary(bpd_non_parametric_bootstrap)
## Sorted edge weights
bpd_summary_sorted <- bpd_summary[order(-bpd_summary$mean), ]
##Lasso-penalized ORs of bootstrapped edge weights 
exp(bpd_summary_sorted[c('mean','CIlower','CIupper')])
bpd_LP_ORs <- summary(bpd_non_parametric_bootstrap)
matrix_bpd <- matrix(NA, nrow = length(bpd_labels), ncol = length(bpd_labels), dimnames = list(bpd_labels, bpd_labels))
for (i in 1:nrow(bpd_LP_ORs)) {
  row <- bpd_LP_ORs[i, ]
  matrix_bpd[row$node1, row$node2] <- round(exp(row$mean),2)
  matrix_bpd[row$node2, row$node1] <- round(exp(row$mean),2)
}
kable(matrix_bpd, escape=F, format = "html", digits = 2) %>%
             kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


