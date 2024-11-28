library(ggplot2)
library(ggthemes)
library(tidyverse)
library(readxl)
library(gridExtra)
library(grid)
library(ggrepel)
library(ggthemes)
library(ggpubr)

# Parameters - Base model
n_pop_lowcr <- 231880*22.9/100
n_pop_highcr <-	231880*15.6/100
teeth	<- 1 #Set as 1 for base case with low caries activity, and 7 for base case with high caries activity
teeth_scaries <-  1 #Teeth with secondary caries, Set as 1 for base case with low caries activity, and 7 for base case with high caries activity
high_risk <- 0 #Set as 1 for high caries activity group, 0 for low caries activity group
p_ga_naf	<- 0.066 #Base 0.066, Sensitivity analysis 0.0098, 0.0845
p_ga_sdf	<- 0.066 #Base 0.066, Sensitivity analysis 0.0098, 0.0845

#Filling
p_cariesarrested_filling	<- 0.8 #Base 0.8, Sensitivity analysis 0.72, 0.98
p_exo_filling	<- 0.14

# Filling under GA
p_cariesarrested_ga	<- 0.9 #Base 0.9
p_exo_ga	<- 0.14

# NaF
p_cariesarrested_naf	<- 0.209
p_exo_naf	<- 0.14

# SDF
p_cariesarrested_sdf	<- 0.5162 #Base 0.5162, Sensitivity analysis 0.439, 0.81
p_exo_sdf	<- 0.14

# Cost
c_consult	<- 85
c_review	<- 55
c_ga	<- 1444
c_filling_ga	<- 100
c_exo_ga	<- 80
c_filling	<- 130
c_exo	<- 90
c_naf	<- 24
c_sdf	<- 24
c_prophy <- 50

# NUHS Costs
# c_consult	<- 82
# c_review	<- 35
# c_ga	<- 609
# c_filling_ga	<- 95
# c_exo_ga	<- 90
# c_filling	<- 95
# c_exo	<- 90
# c_naf	<- 30
# c_sdf	<- 30
# c_prophy <- 55

# Getting ICERs
c_arr_filling <- c_consult + c_filling*teeth
c_resto_filling <- c_consult + c_filling*teeth + c_review + c_filling*teeth_scaries
c_exo_filling <- c_consult + c_filling*teeth + c_review + c_exo*teeth_scaries
c_filling_final <- (p_cariesarrested_filling*c_arr_filling + (1 - p_cariesarrested_filling)*(1 - p_exo_filling)*c_resto_filling + (1 - p_cariesarrested_filling)*(p_exo_filling)*c_exo_filling)*n_pop_lowcr*(1 - high_risk) + (p_cariesarrested_filling*c_arr_filling + (1 - p_cariesarrested_filling)*(1 - p_exo_filling)*c_resto_filling + (1 - p_cariesarrested_filling)*(p_exo_filling)*c_exo_filling)*n_pop_highcr*(high_risk)

c_arr_naf <- c_consult + c_review + c_naf*2
c_resto_naf <- c_consult + c_review + c_naf*2 + (c_filling*teeth_scaries*(1 - p_ga_naf) + (c_filling_ga*teeth_scaries + c_ga)*p_ga_naf)*high_risk + c_filling*teeth_scaries*(1 - high_risk)
c_exo_naf <- c_consult + c_review + c_naf*2 + (c_exo*teeth_scaries*(1 - p_ga_naf) + (c_exo_ga*teeth_scaries + c_ga)*p_ga_naf)*high_risk + c_exo*teeth_scaries*(1- high_risk)
c_naf_final <- (p_cariesarrested_naf*c_arr_naf + (1 - p_cariesarrested_naf)*(1 - p_exo_naf)*c_resto_naf + (1 - p_cariesarrested_naf)*(p_exo_naf)*c_exo_naf)*n_pop_lowcr*(1 - high_risk) + (p_cariesarrested_naf*c_arr_naf + (1 - p_cariesarrested_naf)*(1 - p_exo_naf)*c_resto_naf + (1 - p_cariesarrested_naf)*(p_exo_naf)*c_exo_naf)*n_pop_highcr*(high_risk) 

c_arr_sdf <- c_consult + c_review + c_sdf*2
c_resto_sdf <- c_consult + c_review + c_sdf*2 + (c_filling*teeth_scaries*(1 - p_ga_sdf) + (c_filling_ga*teeth_scaries + c_ga)*p_ga_sdf)*high_risk + c_filling*teeth_scaries*(1 - high_risk)
c_exo_sdf <- c_consult + c_review + c_sdf*2 + (c_exo*teeth_scaries*(1 - p_ga_sdf) + (c_exo_ga*teeth_scaries + c_ga)*p_ga_sdf)*high_risk + c_exo*teeth_scaries*(1 - high_risk)
c_sdf_final <- (p_cariesarrested_sdf*c_arr_sdf + (1 - p_cariesarrested_sdf)*(1 - p_exo_sdf)*c_resto_sdf + (1 - p_cariesarrested_sdf)*p_exo_sdf*c_exo_sdf)*n_pop_lowcr*(1 - high_risk) + (p_cariesarrested_sdf*c_arr_sdf + (1 - p_cariesarrested_sdf)*(1 - p_exo_sdf)*c_resto_sdf + (1 - p_cariesarrested_sdf)*(p_exo_sdf)*c_exo_sdf)*n_pop_highcr*(high_risk) 

c_arr_ga <- c_consult + c_ga + c_filling_ga*teeth
c_resto_ga <- c_consult + c_ga + c_filling_ga*teeth + c_review + c_filling*teeth_scaries
c_exo_ga <- c_consult + c_ga + c_filling_ga*teeth + c_review + c_exo*teeth_scaries
c_ga_final <- (p_cariesarrested_ga*c_arr_ga + (1 - p_cariesarrested_ga)*(1 - p_exo_ga)*c_resto_ga + (1 - p_cariesarrested_ga)*(p_exo_ga)*c_exo_ga)*n_pop_highcr*high_risk

exo_avoided_filling <- (1 - (1 - p_cariesarrested_filling)*p_exo_filling)*teeth_scaries*n_pop_lowcr*(1 - high_risk) + (1 - (1 - p_cariesarrested_filling)*p_exo_filling)*teeth_scaries*n_pop_highcr*high_risk
exo_avoided_ga <- (1 -(1 - p_cariesarrested_ga)*p_exo_ga)*teeth_scaries*n_pop_highcr*high_risk # + (1 - (1 - p_cariesarrested_ga)*p_exo_ga)*teeth_scaries*n_pop_lowcr*(1 - high_risk)
exo_avoided_naf <- (1 - (1 - p_cariesarrested_naf)*p_exo_naf)*teeth_scaries*n_pop_lowcr*(1 - high_risk) + (1 - (1 - p_cariesarrested_naf)*p_exo_naf)*teeth_scaries*n_pop_highcr*high_risk
exo_avoided_sdf <- (1 - (1 - p_cariesarrested_sdf)*p_exo_sdf)*teeth_scaries*n_pop_lowcr*(1 - high_risk) + (1 - (1 - p_cariesarrested_sdf)*p_exo_sdf)*teeth_scaries*n_pop_highcr*high_risk

cariescontrolled_filling <- p_cariesarrested_filling*teeth_scaries*n_pop_lowcr*(1 - high_risk) + p_cariesarrested_filling*teeth_scaries*n_pop_highcr*high_risk
cariescontrolled_ga <- p_cariesarrested_ga*teeth_scaries*n_pop_highcr*high_risk # + p_cariesarrested_ga*teeth_scaries*n_pop_lowcr*(1 - high_risk)
cariescontrolled_naf <- p_cariesarrested_naf*teeth_scaries*n_pop_lowcr*(1 - high_risk) + p_cariesarrested_naf*teeth_scaries*n_pop_highcr*high_risk
cariescontrolled_sdf <- p_cariesarrested_sdf*teeth_scaries*n_pop_lowcr*(1 - high_risk) + p_cariesarrested_sdf*teeth_scaries*n_pop_highcr*high_risk

df <- data.frame(
  tx = c("SDF", "Direct Restoration", "GA", "NaF"),
  averagecost= c(c_sdf_final/(ifelse(high_risk==1,n_pop_highcr ,n_pop_lowcr)), c_filling_final/(ifelse(high_risk==1,n_pop_highcr ,n_pop_lowcr)), c_ga_final/(ifelse(high_risk==1,n_pop_highcr ,n_pop_lowcr)), c_naf_final/(ifelse(high_risk==1,n_pop_highcr ,n_pop_lowcr))),
  cost = c(c_sdf_final, c_filling_final, c_ga_final, c_naf_final),
  cariescontrolled = c(cariescontrolled_sdf, cariescontrolled_filling, cariescontrolled_ga, cariescontrolled_naf),
  exoavoided = c(exo_avoided_sdf, exo_avoided_filling, exo_avoided_ga, exo_avoided_naf))

columns <- c("cost", "cariescontrolled", "exoavoided")

df <- df %>% arrange(cost)

for (col_name in columns){
  diff_col <- c(NA, diff(df[[col_name]]))
  new_col_name <- paste0("incr", col_name)
  df[[new_col_name]] <-diff_col
}

df <- df[, c("tx", "averagecost", "cost", "incrcost", "cariescontrolled", "incrcariescontrolled", "exoavoided", "increxoavoided")]

df2 <- data.frame(
  tx = c("Direct Restoration", "SDF"),
  cost = c(c_filling_final, c_sdf_final),
  cariescontrolled = c(cariescontrolled_filling, cariescontrolled_sdf),
  exoavoided = c(exo_avoided_filling, exo_avoided_sdf))

df2 <- df2 %>%
  mutate(tx = factor(tx, levels = c("Direct Restoration", "SDF"))) %>%
  arrange(tx)

for (col_name in columns){
  diff_col <- c(NA, diff(df2[[col_name]]))
  new_col_name <- paste0("incr", col_name)
  df2[[new_col_name]] <-diff_col
}

df$ICERcariescontrolled <- df$incrcost/df$incrcariescontrolled
df$ICERexoavoided <- df$incrcost/df$increxoavoided
df$NMBcariescontrolled <- 30*df$incrcariescontrolled - df$incrcost
df$NMBcariescontrolledperpax <- (30*df$incrcariescontrolled - df$incrcost)*(1-high_risk)/n_pop_lowcr + (30*df$incrcariescontrolled - df$incrcost)*high_risk/n_pop_highcr
df$NMBexoavoidedperpax <- (30*df$increxoavoided - df$incrcost)*(1-high_risk)/n_pop_lowcr + (30*df$increxoavoided - df$incrcost)*high_risk/n_pop_highcr 

df2$ICERcariescontrolled <- df2$incrcost/df2$incrcariescontrolled
df2$ICERexoavoided <- df2$incrcost/df2$increxoavoided
df2$NMBcariescontrolled <- 30*df2$incrcariescontrolled - df2$incrcost
df2$NMBcariescontrolledperpax <- (30*df2$incrcariescontrolled - df2$incrcost)*(1-high_risk)/n_pop_lowcr + (30*df2$incrcariescontrolled - df2$incrcost)*high_risk/n_pop_highcr
df2$NMBexoavoidedperpax <- (30*df2$increxoavoided - df2$incrcost)*(1-high_risk)/n_pop_lowcr + (30*df2$increxoavoided - df2$incrcost)*high_risk/n_pop_highcr 

df3$ICERcariescontrolled <- df3$incrcost/df3$incrcariescontrolled
df3$ICERexoavoided <- df3$incrcost/df3$increxoavoided
df3$NMBcariescontrolled <- 30*df3$incrcariescontrolled - df3$incrcost
df3$NMBcariescontrolledperpax <- (30*df3$incrcariescontrolled - df3$incrcost)*(1-high_risk)/n_pop_lowcr + (30*df3$incrcariescontrolled - df3$incrcost)*high_risk/n_pop_highcr
df3$NMBexoavoidedperpax <- (30*df3$increxoavoided - df3$incrcost)*(1-high_risk)/n_pop_lowcr + (30*df3$increxoavoided - df3$incrcost)*high_risk/n_pop_highcr 

numeric_cols <- sapply(df, is.numeric)
df[, numeric_cols] <- round(df[, numeric_cols])

numeric_cols <- sapply(df2, is.numeric)
df2[, numeric_cols] <- round(df2[, numeric_cols])

numeric_cols <- sapply(df3, is.numeric)
df3[, numeric_cols] <- round(df3[, numeric_cols])

# Test with different WTP values

df2 <- data.frame(
  tx = c("Direct Restoration", "SDF"),
  cost = c(c_filling_final, c_sdf_final),
  cariescontrolled = c(cariescontrolled_filling, cariescontrolled_sdf),
  exoavoided = c(exo_avoided_filling, exo_avoided_sdf))

df2 <- df2 %>%
  mutate(tx = factor(tx, levels = c("Direct Restoration", "SDF"))) %>%
  arrange(tx)
for (col_name in columns){
  diff_col <- c(NA, diff(df2[[col_name]]))
  new_col_name <- paste0("incr", col_name)
  df2[[new_col_name]] <-diff_col
}

## ------------------------------------------------------------------------------------------------
# Supplemental Appendix (varying WTP)
wtp_values <- c(0:5000)
nmb_list <- list()
# Loop over each WTP value
for (wtp in wtp_values) {
  # Calculate NMB for each row in the data frame
  df_nmb <- df2 %>%
    mutate(
      wtp = wtp,
  NMBcariescontrolledperpax = (wtp*df2$incrcariescontrolled - df2$incrcost)*(1-high_risk)/n_pop_lowcr + (wtp*df2$incrcariescontrolled - df2$incrcost)*high_risk/n_pop_highcr,
  NMBexoavoidedperpax =(wtp*df2$increxoavoided - df2$incrcost)*(1-high_risk)/n_pop_lowcr + (wtp*df2$increxoavoided - df2$incrcost)*high_risk/n_pop_highcr 
    )
  
  df_nmb <- df_nmb %>% subset(tx=="SDF") %>% select(wtp, NMBcariescontrolledperpax, NMBexoavoidedperpax)
  
  # Append the result to the list
  nmb_list[[as.character(wtp)]] <- df_nmb
}

  # Combine results to single dataframe
nmb_results <- do.call(rbind, nmb_list)
numeric_cols <- sapply(nmb_results, is.numeric)
nmb_results[, numeric_cols] <- round(nmb_results[, numeric_cols])

# Print the combined results
print("NMB Results for Different WTP Values:")
print(nmb_results)
write.xlsx(nmb_results, "file.xlsx")

## ------------------------------------------------------------------------------------------------
fig2 <- readxl::read_xlsx("Model_Children_Figure.xlsx", sheet="Fig2") #extract results on total costs and caries activity for base case
fig2$costs <- round(fig2$costs/1000)

  ggplot(data = fig2, aes(x = caries_control, y = costs), color = variable) +
  geom_line(aes(color = variable), linewidth = 1.5) +
  geom_point(shape=16, size=2) +
  geom_label_repel(aes(label = Option), point.padding = 0.1, box.padding = 0.5, direction="both", nudge_y = 1000, nudge_x = -50000, min.segment.length = 0, label.size = NA) +
  labs(title = "Cost Efficiency Frontier", y = "Costs ('000s)", x = "Active caries controlled", color="Caries Activity", size = 4) +
    theme_few() + 
    theme(text = element_text(size =15), legend.text=element_text(size=15)) + 
  scale_color_brewer(palette="Paired")
  
## ------------------------------------------------------------------------------------------------
fig3 <- readxl::read_xlsx("Model_Children_Figure.xlsx", sheet="Fig3") #extract results, varying number of teeth with caries at baseline
fig3$NMB <- round(fig3$NMB)

    ggplot(data = fig3, aes(x = baselinecaries, y = NMB, label=NMB), color = variable) +
      coord_cartesian(ylim = c(-50, 1000)) +
      geom_text_repel(data= subset(fig3, variable=="Caries control"), aes(label = NMB), nudge_y = -30, size = 5) +
      geom_text_repel(data= subset(fig3, variable=="Avoidance of extraction"), aes(label = NMB), nudge_y = 30, size = 5) +
      geom_line(aes(color = variable), linewidth = 1.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
      geom_point(shape=16, size=2) +
      labs(title = "Net Monetary Benefit of SDF (Relative to Direct Restorations Chairside), Per Child", y = "NMB", x = "Number of carious teeth at baseline", color="Outcome") +
      scale_x_continuous(breaks=seq(0,10,1)) + 
      theme_few() + 
      theme(text = element_text(size = 20), legend.text=element_text(size=20)) + 
      scale_color_brewer(palette="Paired") 
    
## ------------------------------------------------------------------------------------------------
# PSA

# Set seed
set.seed(1234)

# No of simulations
n_sim <- 10000

params <- readxl::read_xlsx("SDF%20Model_Children_PSA.xlsx", sheet="PSA_HighActivity")
params2 <- readxl::read_xlsx("SDF%20Model_Children_PSA.xlsx", sheet="PSA_LowActivity")
psa_sim <- function(n, input) {
  if (is.na(input["distribution"]) == TRUE) {
    result <- rep(pull(input["mean_value"]), n)
  }   
  else if (input["distribution"] == "uniform") {
    result <- sample(paste0(pull(input["lower"])):paste0(pull(input["upper"])), n, replace=TRUE)
  }     
  else if (input["distribution"] == "beta") {
    result <- rbeta(n, pull(input["alpha"]), pull(input["beta"]))
  }              
  else if (input["distribution"] == "gamma") {
    result <- rgamma(n, pull(input["alpha"]), pull(input["beta"]))
  }
  else if (input["distribution"] == "lognormal") {
    result <- rlnorm(n, pull(input["mu"]), pull(input["sigma"]))
  }
  result
}

psa_mat <- array(NA, dim=c(n_sim, nrow(params)))  
psa_mat2 <- array(NA, dim=c(n_sim, nrow(params2)))

for (i in 1:nrow(params)) {
  psa_mat[,i] <- psa_sim(n_sim, params[i,]) #(r,c)
}
for (i in 1:nrow(params2)) {
  psa_mat2[,i] <- psa_sim(n_sim, params2[i,]) #(r,c)
}

colMeans(psa_mat)
colMeans(psa_mat2)

psa <- data.frame(psa_mat) 
names(psa) <- params$parameter

psa2 <- data.frame(psa_mat2) 
names(psa2) <- params2$parameter

psa_list <- split(psa, 1:nrow(psa))
psa_list2 <- split(psa2, 1:nrow(psa2))

# Model
DCEA <- function(input){
  with(input,{                     # with function simplifies code, so var names will automatically be searched from the input, no need pd$var
    c_arr_filling <- c_consult + c_filling*teeth
    c_resto_filling <- c_consult + c_filling*teeth + c_review + c_filling*teeth_scaries
    c_exo_filling <- c_consult + c_filling*teeth + c_review + c_exo*teeth_scaries
    c_filling_final <- (p_cariesarrested_filling*c_arr_filling + (1 - p_cariesarrested_filling)*(1 - p_exo_filling)*c_resto_filling + (1 - p_cariesarrested_filling)*(p_exo_filling)*c_exo_filling)*n_pop_lowcr*(1 - high_risk) + (p_cariesarrested_filling*c_arr_filling + (1 - p_cariesarrested_filling)*(1 - p_exo_filling)*c_resto_filling + (1 - p_cariesarrested_filling)*(p_exo_filling)*c_exo_filling)*n_pop_highcr*(high_risk)
    
    c_arr_sdf <- c_consult + c_review + c_sdf*2
    c_resto_sdf <- c_consult + c_review + c_sdf*2 + (c_filling*teeth_scaries*(1 - p_ga_sdf) + (c_filling_ga*teeth_scaries + c_ga)*p_ga_sdf)*high_risk + c_filling*teeth_scaries*(1 - high_risk)
    c_exo_sdf <- c_consult + c_review + c_sdf*2 + (c_exo*teeth_scaries*(1 - p_ga_sdf) + (c_exo_ga*teeth_scaries + c_ga)*p_ga_sdf)*high_risk + c_exo*teeth_scaries*(1 - high_risk)
    c_sdf_final <- (p_cariesarrested_sdf*c_arr_sdf + (1 - p_cariesarrested_sdf)*(1 - p_exo_sdf)*c_resto_sdf + (1 - p_cariesarrested_sdf)*p_exo_sdf*c_exo_sdf)*n_pop_lowcr*(1 - high_risk) + (p_cariesarrested_sdf*c_arr_sdf + (1 - p_cariesarrested_sdf)*(1 - p_exo_sdf)*c_resto_sdf + (1 - p_cariesarrested_sdf)*(p_exo_sdf)*c_exo_sdf)*n_pop_highcr*(high_risk) 
    
    exo_avoided_filling <- (1 - (1 - p_cariesarrested_filling)*p_exo_filling)*teeth_scaries*n_pop_lowcr*(1 - high_risk) + (1 - (1 - p_cariesarrested_filling)*p_exo_filling)*teeth_scaries*n_pop_highcr*high_risk
    exo_avoided_sdf <- (1 - (1 - p_cariesarrested_sdf)*p_exo_sdf)*teeth_scaries*n_pop_lowcr*(1 - high_risk) + (1 - (1 - p_cariesarrested_sdf)*p_exo_sdf)*teeth_scaries*n_pop_highcr*high_risk
    
    cariescontrolled_filling <- p_cariesarrested_filling*teeth_scaries*n_pop_lowcr*(1 - high_risk) + p_cariesarrested_filling*teeth_scaries*n_pop_highcr*high_risk
    cariescontrolled_sdf <- p_cariesarrested_sdf*teeth_scaries*n_pop_lowcr*(1 - high_risk) + p_cariesarrested_sdf*teeth_scaries*n_pop_highcr*high_risk
    
    ICERcariescontrolled <- (c_sdf_final - c_filling_final)/(cariescontrolled_sdf - cariescontrolled_filling)
    ICERexoavoided <- (c_sdf_final - c_filling_final)/(exo_avoided_sdf - exo_avoided_filling)
    
    NMBcariescontrolled <- 30*(cariescontrolled_sdf - cariescontrolled_filling) - (c_sdf_final - c_filling_final)
    NMBexoavoided <- 30*(exo_avoided_sdf - exo_avoided_filling) - (c_sdf_final - c_filling_final)
    
    c(c_sdf_final - c_filling_final, 
      cariescontrolled_sdf - cariescontrolled_filling, 
      ICERcariescontrolled, 
      exo_avoided_sdf - exo_avoided_filling, 
      ICERexoavoided)
  })
}

# Apply PSA function onto each of the 10k datasets
result_psa <- sapply(psa_list, DCEA)
rownames(result_psa) <- c("IRC", "IRCC", "ICERCC", "IRE", "ICERE")  #IRC=incremental cost, IRCC=incremental caries controlled, IRE=incremental exo avoided, ICERCC=ICER caries controlled, ICERE=ICER of exo avoided

result_psa2 <- sapply(psa_list2, DCEA)
rownames(result_psa2) <- c("IRC", "IRCC", "ICERCC", "IRE", "ICERE")

# Transpose matrix
result_psa_df <- data.frame(t(result_psa))
result_psa_df2 <- data.frame(t(result_psa2))

# Get mean + 95% CI of incremental costs and incremental outcomes
test <- function(df, colname){
    paste0(round(mean(df[[colname]])), " (95% CI, ", round(t.test(df[[colname]])$conf.int[1]), " - ", round(t.test(df[[colname]])$conf.int[2]), ")")
}
variables <- c("IRC", "IRCC", "IRE")
for (i in variables){
print(i)
print(test(result_psa_df, i))
}
for (i in variables){
  print(i)
  print(test(result_psa_df2, i))
}

# Plot the 10,000 ICER values on cost-effectiveness plane
# x values = IRQ, y values = IRC
ggplot(data=result_psa_df, aes(x=IRCC, y=IRC)) +
  geom_point() +
  xlab("Incremental Caries Controlled") +
  ylab("Incremental Cost") +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0)

# Cost effectiveness acceptability curve (CEAC curve)
# x axis = willingness to pay, y axis = probability cost effective
WTP <- seq(0, 10000, 1)
# set up the matrix to hold data
CEAC_matrix <- array(NA, dim=list(length(WTP)*2, 4))  # *2 due to intervention and comparator
colnames(CEAC_matrix) <- c("WTP", "ProbCariesControl", "ProbExoAvoided", "Int") # Int indicates if intervention or comparator

CEAC_matrix[,1] <- rep(WTP,2) # input the WTP values twice, once for int once for com

WTP2 <- seq(0, 100, 1)

# set up the matrix to hold data
CEAC_matrix2 <- array(NA, dim=list(length(WTP2)*2, 4))  
colnames(CEAC_matrix2) <- c("WTP", "ProbCariesControl", "ProbExoAvoided", "Int") # Int indicates if intervention or comparator

CEAC_matrix2[,1] <- rep(WTP2,2) # input the WTP values twice, once for int once for com

# calculate probability that it is cost effective for each WTP
for (i in 1:length(WTP)) {
  CEAC_matrix[i,2] <- mean((result_psa_df$IRCC*WTP[i] -   
                              result_psa_df$IRC) > 0)
  CEAC_matrix[i+length(WTP),2] <- mean((result_psa_df$IRCC*WTP[i] -  
                                          result_psa_df$IRC) <= 0)
  CEAC_matrix[i,3] <- mean((result_psa_df$IRE*WTP[i] -   
                              result_psa_df$IRC) > 0)
  CEAC_matrix[i+length(WTP),3] <- mean((result_psa_df$IRE*WTP[i] -  
                                          result_psa_df$IRC) <= 0)
}

for (i in 1:length(WTP2)) {
  CEAC_matrix2[i,2] <- mean((result_psa_df2$IRCC*WTP2[i] -   
                              result_psa_df2$IRC) > 0)
  CEAC_matrix2[i+length(WTP2),2] <- mean((result_psa_df2$IRCC*WTP2[i] -  
                                          result_psa_df2$IRC) <= 0)
  CEAC_matrix2[i,3] <- mean((result_psa_df2$IRE*WTP[i] -   
                              result_psa_df2$IRC) > 0)
  CEAC_matrix2[i+length(WTP2),3] <- mean((result_psa_df2$IRE*WTP2[i] -  
                                          result_psa_df2$IRC) <= 0)
}

# label if it is intervention or comparator
CEAC_matrix[,4] <- c(rep(1, length(WTP)), rep(2, length(WTP)))
CEAC_matrix_df <- data.frame(CEAC_matrix)
CEAC_matrix_df$Int_fact <- factor(CEAC_matrix_df$Int,
                                  levels = c(1,2),
                                  labels = c("SDF", "Direct Restoration")) 

CEAC_matrix2[,4] <- c(rep(1, length(WTP2)), rep(2, length(WTP2)))
CEAC_matrix_df2 <- data.frame(CEAC_matrix2)
CEAC_matrix_df2$Int_fact <- factor(CEAC_matrix_df2$Int,
                                  levels = c(1,2),
                                  labels = c("SDF", "Direct Restoration")) 
# Plot CEAC curve
plot1 <- ggplot(data=CEAC_matrix_df, aes(x=WTP, y=ProbCariesControl, color=Int_fact)) +
  geom_line(linewidth = 2) +   
  labs(color = "Intervention") + 
  ylab("Probability") +
  ggtitle("CEAC for caries control (High Caries Activity)") +
  theme_classic() + 
  theme(text = element_text(size = 15), legend.text=element_text(size=20), legend.title=element_text(size=20)) + 
  scale_color_brewer(palette="Paired")

plot2 <- ggplot(data=CEAC_matrix_df, aes(x=WTP, y=ProbExoAvoided, color=Int_fact)) +
  geom_line(linewidth = 2) +   
  labs(color = "Intervention") + 
  ylab("Probability") + 
  ggtitle("CEAC for avoidance of extraction (High Caries Activity)") +
  theme_classic() + 
  theme(text = element_text(size = 15), legend.text=element_text(size=20), legend.title=element_text(size=20)) + 
  scale_color_brewer(palette="Paired")

plot3 <- ggplot(data=CEAC_matrix_df2, aes(x=WTP, y=ProbCariesControl, color=Int_fact)) +
  geom_line(linewidth = 2) +   
  labs(color = "Intervention") + 
  ylab("Probability") +
  ggtitle("CEAC for caries control (Low Caries Activity)") +
  theme_classic() + 
  theme(text = element_text(size = 15), legend.text=element_text(size=20), legend.title=element_text(size=20)) + 
  scale_color_brewer(palette="Paired")

plot4 <- ggplot(data=CEAC_matrix_df2, aes(x=WTP, y=ProbExoAvoided, color=Int_fact)) +
  geom_line(linewidth = 2) +   
  labs(color = "Intervention") + 
  ylab("Probability") +
  ggtitle("CEAC for avoidance of extraction (Low Caries Activity)") +
  theme_classic() + 
  theme(text = element_text(size = 15), legend.text=element_text(size=20), legend.title=element_text(size=20)) + 
  scale_color_brewer(palette="Paired")

ggarrange(plot1, plot2, plot3, plot4, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")