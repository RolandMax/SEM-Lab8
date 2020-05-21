# ---
#   title: "Lab 8 - Introduction to Mixture Models - Latent Class Analysis"
# author: "*Adam Garber*"
# subtitle: 'Structural Equation Modeling - Instructor: Karen Nylund-Gibson'
# date: "`r format(Sys.time(), '%B %d, %Y')`"
# output: 
#   prettydoc::html_pretty:
#   theme: architect
# highlight: github
# toc: yes
# ---


# `University of California, Santa Barbara`

# # ______________________________________________

# Lab preparation

# # ______________________________________________


## Data source:

# 1. The first example utilizes a dataset on undergraduate *Cheating* available from the `poLCA` package (Dayton, 1998): [$\color{blue}{\text{See documentation here}}$](https://cran.r-project.org/web/packages/poLCA/poLCA.pdf)
# 
# 2. The second examples utilizes the public-use dataset, *The Longitudinal Survey of American Youth* (**LSAY**):  [$\color{blue}{\text{See documentation here}}$](https://www.lsay.org/)
# 
# 3. The third examples utilizes the *Kindergarten Student Entrance Profile* (**KSEP**) (Quirk et al., 2011): [$\color{blue}{\text{See documentation here}}$](https://www.tandfonline.com/doi/full/10.1080/15377903.2010.540518?casa_token=hGkNrXYxjfAAAAAA:P4jjtIDXjHkU6mq8hvM9wsbQ-HgERqh0Z8cTfHIu4aeNUqThOtzKflpTwbWQpjFk9rT8At6-5Npw)

# # ______________________________________________

# Load packages
library(tidyverse)
library(haven)
library(glue)
library(MplusAutomation)
library(rhdf5)
library(here)
library(janitor)
library(gt)
library(semPlot)
library(reshape2)
library(cowplot)
library(filesstrings)


# ____________________________________

# Enumerate and plot mixtures

# Compare *k*-class models 1 through 6

# ____________________________________

## Example 1: Undergraduate Cheating behavior

# "Dichotomous self-report responses by 319 undergraduates to four
# questions about cheating behavior" (poLCA, 2016).

# ____________________________________

# Prepare data 
data(cheating)

cheating <- cheating %>% clean_names() 

df_cheat <-  cheating %>%                                  #
  dplyr::select(1:4) %>%                                   #
  dplyr::mutate_all(funs(.-1))                             #


# Run a quick LCA using `createMixtures`

createMixtures(classes = 1:4, filename_stem = "cheat",                                   
               rdata = df_cheat,                                                         
               ANALYSIS = "processors = 10; starts 500 100;",                                
               VARIABLE = "CATEGORICAL = lieexam-copyexam;",                                
               PLOT = "type = plot3; series = lieexam-copyexam(*);")      

files <- data.frame(names = list.files(path = here(), full.names = FALSE))

f_cheat <- files %>% dplyr::filter(., grepl('cheat', names))

for (i in 1:length(f_cheat)) { file.move(glue(here("{f_cheat[,i]}")), here("enum_mplus")) }


runModels(here("enum_mplus"), filefilter = "cheat")

 

# View model fit statistics with `mixtureSummaryTable()`
output_cheat <- readModels(here("enum_mplus"), filefilter = "cheat", quiet = TRUE)

gt(mixtureSummaryTable(output_cheat)) %>%                                                         #
  tab_header(                                                                                     #
    title = "Fit Indices") %>%                                                                    #
  tab_options(                                                                                    #
    table.width = pct(80)) %>%                                                                    #
  tab_footnote(                                                                                   #
    footnote = "Undergraduate Cheating Behavior",                                                 #
    location = cells_title())                                                                     #

 

# Extract and prepare plot data
   
# extract posterior probabilities 
plot1 <- as.data.frame(output_cheat[["cheat_4_class.out"]]                        
                       [["gh5"]][["means_and_variances_data"]]                
                       [["estimated_probs"]][["values"]]                      
                       [seq(2, 8, 2),]) #seq("from","to","by")                

# extract class size proportions
c_size <- as.data.frame(output_cheat[["cheat_4_class.out"]]                           
                        [["class_counts"]][["modelEstimated"]][["proportion"]])       

colnames(c_size) <- paste0("cs")

c_size <- c_size %>% mutate(cs = round(cs*100, 2))

#rename columns (classes) and "Var" (indicator names)
colnames(plot1) <- paste0("C", 1:4, glue(" ({c_size[1:4,]}%)"))
plot1 <- cbind(Var = paste0("U", 1:4), plot1)

# choose the order of indicators by changing to ordered factor
plot1$Var <- fct_inorder(plot1$Var)

#change dateframe from wide to long format
pd_long1 <- melt(plot1, id.vars = "Var") 
 

# Plot 4-class latent class posterior probability plot
ggplot(pd_long1, aes(Var, value, shape = variable,                                              #
                     colour = variable, lty = variable)) +                                       #
  geom_point(size = 4) + geom_line(aes(as.integer(Var))) +                                      #
  scale_x_discrete(labels = c("Lie Exam", "Lie Paper", "Fraud", "Copy Exam")) +                 #
  scale_y_continuous("Probability") +                                                           #
  scale_colour_viridis_d(end = .7) +                                                            #
  theme_cowplot() + labs(x=" ") +                                                               #
  theme(text=element_text(family="Times New Roman", size=12),                                   #
        legend.key.width = unit(.5, "line"),                                                    #
        legend.text = element_text(family="Times New Roman", size=12),                          #
        legend.title = element_blank(),                                                         #
        legend.position = "top")                                                                #
 

# save figure
ggsave(here("figures", "C4_Cheat_LCA_Plot.png"), dpi="retina", height=5, width=7, units="in")
 
# # ______________________________________________

## Example 2: Longitudinal Study of American Youth, **Science Attitudes** 

# # ______________________________________________

# Load data
   
lsay_data <- read_csv(here("data", "lca_lsay_sci.csv"), na = c("9999", "9999.00")) %>%  
  clean_names() %>%                                                                                  
  dplyr::select(1:5, Enjoy = ab39m, Useful = ab39t,                                  
                Logical = ab39u, Job = ab39w, Adult = ab39x)          


# View LCA indicators 

var_table <- tribble(
  ~"Name",      ~"Label",  ~"Values",                                   
  #--------------|--------------------------------|-----|,
  "Enjoy",   "I enjoy science "                   ,  "0 = Disagree, 1 = Agree",
  "Useful",  "Science useful in everday problems ",  "0 = Disagree, 1 = Agree",
  "Logical", "Science helps logical thinkng "     ,  "0 = Disagree, 1 = Agree",
  "Job",     "Need science for a good job "       ,  "0 = Disagree, 1 = Agree",
  "Adult",   "Will use science often as an adult ",  "0 = Disagree, 1 = Agree")

gt(var_table) %>% 
  tab_header(
    title = "LCA Indicators"  # Add a title
  ) %>%
  tab_options(
    table.width = pct(80)
  ) %>%
  tab_footnote(
    footnote = "Longitudinal Study of American Youth",
    location = cells_title())


 

# Run enumeration using `mplusObject` method

lca_k1_6  <- lapply(1:6, function(k) {
  lca_enum  <- mplusObject(
    
    TITLE = glue("Class {k}"), 
    
    VARIABLE = glue(
      "categorical = Enjoy-Adult; 
     usevar = Enjoy-Adult;
     classes = c({k}); "),
    
    ANALYSIS = 
      "estimator = mlr; 
    type = mixture;
    stseed = 5212020;
    starts = 200 100; 
    processors = 10;",
    
    OUTPUT = "sampstat residual tech11 tech14;",
    
    PLOT = 
      "type = plot3; 
    series = Enjoy-Adult(*);",
    
    usevariables = colnames(lsay_data),
    rdata = lsay_data)
  
  lca_enum_fit <- mplusModeler(lca_enum, 
                               dataout=glue(here("enum_mplus", "c_lca_lsay_Lab8.dat")),
                               modelout=glue(here("enum_mplus", "c{k}_lca_lsay_Lab8.inp")) ,
                               check=TRUE, run = TRUE, hashfilename = FALSE)
})

 

# Compare model fit for series of enumerated models
   
all_output <- readModels(here("enum_mplus"), filefilter = "lsay", quiet = TRUE)

enum_summary <- LatexSummaryTable(all_output,                                 
                                  keepCols=c("Title", "LL", "BIC", "aBIC",                      
                                             "BLRT_PValue", "T11_VLMR_PValue"),           
                                  sortBy = "Title")                                 

gt(enum_summary)  %>%                                                                     #
  tab_header(                                                                             #
    title = "Fit Indices" ) %>%                                                           #
  tab_options(                                                                            #
    table.width = pct(80)) %>%                                                            #
  tab_footnote(                                                                           #
    footnote = "Longitudinal Study of American Youth",                                    #
    location = cells_title())                                                             #
 

# Compare probability plots for $K = 1:6$ class solutions
   
model_results <- data.frame()

for (i in 1:length(all_output)) {
  
  temp <- all_output[[i]]$parameters$unstandardized %>%                                            #            
    mutate(model = paste(i, "-Class Model"))                                                       # 
  
  model_results <- rbind(model_results, temp)
}

rm(temp)

model_results <- model_results %>%                                                                 #
  filter(paramHeader == "Thresholds") %>%                                                          #
  dplyr::select(est, model, LatentClass, param) %>%                                                #
  mutate(prob = (1 / (1 + exp(est)))) %>%                                                          #
  mutate(param = as.factor(str_to_lower(str_sub(param, end = -3)))) %>%                            #
  dplyr::select(-est)                                                                              #

ggplot(model_results, aes(x = param, y = prob,                                                     # 
                          color = LatentClass, shape = LatentClass,                                               # 
                          group = LatentClass, lty = LatentClass)) +                                              # 
  geom_point() + geom_line() +                                                                     # 
  scale_colour_viridis_d(end = .8, direction = -1) +                                               # 
  facet_wrap(~ model, ncol = 2) +                                                                  # 
  labs(title = "LCA Posterior Probability Plot",                                                   # 
       x= "Science attitudes", y = "Probability") +                                                # 
  theme_minimal()                                                                                  #

ggsave(here("figures","Enum_LCA_facet_plot.png"),
       dpi=300, height=4, width=6, units="in")  
 

# ____________________________________

## Example 3 - Kindergarten Student Entrance Profile (**KSEP**)

# ____________________________________

ksep <- read_csv(here("data", "KSEP_sub_L6.18.csv"))

 
# Enumeration: Compare *k*-class models 1-6

lca_k1_6  <- lapply(1:6, function(k) {
  lca_enum  <- mplusObject(
    
    TITLE = glue("Class {k}"), 
    
    VARIABLE = glue(
      "categorical = seek_hlp-shapes; 
     usevar = seek_hlp-shapes;
     classes = c({k}); "),
    
    ANALYSIS = 
      "estimator = mlr; 
    type = mixture;
    stseed = 5212020;
    starts = 200 100; 
    processors = 10;",
    
    OUTPUT = "sampstat residual tech11 tech14;",
    
    PLOT = 
      "type = plot3; 
    series = seek_hlp-shapes(*);",
    
    usevariables = colnames(ksep),
    rdata = ksep)
  
  lca_enum_fit <- mplusModeler(lca_enum, 
                               dataout=glue(here("enum_ksep", "c_lca_ksep_Lab8.dat")),
                               modelout=glue(here("enum_ksep", "c{k}_lca_ksep_Lab8.inp")) ,
                               check=TRUE, run = TRUE, hashfilename = FALSE)
})

 

# Compare model fit for series of enumerated models
   
all_output <- readModels(here("enum_ksep"), quiet = TRUE)

enum_summary <- LatexSummaryTable(all_output,                       
                                  keepCols=c("Title", "LL", "BIC", "aBIC",            
                                             "BLRT_PValue", "T11_VLMR_PValue"),         
                                  sortBy = "Title")                                       

gt(enum_summary) %>%                                                            # 
  tab_header(                                                                   # 
    title = "Fit Indices") %>%                                                  # 
  tab_options(                                                                  # 
    table.width = pct(80)) %>%                                                  # 
  tab_footnote(                                                                 # 
    footnote = "Kindergarten Student Entrance Profile",                         #
    location = cells_title())                                                   # 
 

# Compare probability plots for $K = 1:6$ class solutions
model_results <- data.frame()

for (i in 1:length(all_output)) {
  
  temp <- all_output[[i]]$parameters$unstandardized %>%                   
    mutate(model = paste(i, "-Class Model"))                              
  
  model_results <- rbind(model_results, temp)
}

rm(temp)

model_results <- model_results %>%                                
  filter(paramHeader == "Thresholds") %>%                                         
  dplyr::select(est, model, LatentClass, param) %>%                             
  mutate(prob = (1 / (1 + exp(est)))) %>%                                               
  mutate(param = as.factor(str_to_lower(str_sub(param, end = -3)))) %>%                     
  dplyr::select(-est)                                                                 

model_results$param <- fct_inorder(model_results$param) 

ggplot(model_results, aes(x = param, y = prob,                                              
                          color = LatentClass, shape = LatentClass,                       
                          group = LatentClass, lty = LatentClass)) +                
  geom_point() + geom_line() +                                  
  scale_colour_viridis_d() +                                              
  facet_wrap(~ model, ncol = 2) +                                             
  labs(title = "Kindergarten Student Entrance Profile (KSEP)",              
       x= " ", y = "Probability") +                                           
  scale_x_discrete(labels = c("Seeks help", "Cooperative", "Impulse control","Repeats", "Separates",
                              "New activities", "Follows rules", "Name", "Writes", "Expressive", "Quantity", "Colors", "Shapes")) +         
  theme_minimal() + theme(panel.grid.major.y = element_blank(),                                      
                          axis.text.x = element_text(angle = -45, hjust = -.1))             

ggsave(here("figures","KSEP_LCA_facet_plot.png"),
       dpi=300, height=4, width=6, units="in")  
 

# Extraxt and prepare plot data
   
# extract posterior probabilities 
plot1 <- as.data.frame(all_output[["c4_lca_ksep_Lab8.out"]]                                      #
                       [["gh5"]][["means_and_variances_data"]]                               #       
                       [["estimated_probs"]][["values"]]                                     # 
                       [seq(2, 26, 2),]) #seq("from","to","by")                              #        

# extract class size proportions
c_size <- as.data.frame(all_output[["c4_lca_ksep_Lab8.out"]]
                        [["class_counts"]][["modelEstimated"]][["proportion"]])

colnames(c_size) <- paste0("cs")

c_size <- c_size %>% mutate(cs = round(cs*100, 2))

#rename columns (classes) and "Var" (indicator names)
colnames(plot1) <- paste0("C", 1:4, glue(" ({c_size[1:4,]}%)"))
plot1 <- cbind(Var = paste0("U", 1:13), plot1)

# choose the order of indicators by changing to ordered factor
plot1$Var <- fct_inorder(plot1$Var) 

#change dateframe fromw wide to long format
pd_long1 <- melt(plot1, id.vars = "Var") 
 

# Plot 4-class mixture
   

ggplot(pd_long1, aes(Var, value, shape = variable,          
                     colour = variable, lty = variable)) +                      
  geom_point(size = 4) + geom_line(aes(as.integer(Var))) +                    
  scale_x_discrete(labels = c("Seeks help", "Cooperative", "Impulse control", "Repeats",      
                              "Separates", "New activities", "Follows rules", "Name",       
                              "Writes", "Expressive", "Quantity", "Colors", "Shapes")) +    
  scale_y_continuous("Probability") +                                                 
  scale_colour_viridis_d(end = .9) +                                                  
  labs(title="Kindergarten Student Entrance Profile", x=" ") +                              
  theme_cowplot() +                                                                 
  theme(text=element_text(family="Times New Roman", size=12),                                 
        legend.key.width = unit(.5, "line"),                                                
        legend.text = element_text(family="Times New Roman", size=12),                      
        legend.title = element_blank(),                                                 
        axis.text.x = element_text(angle = -45, hjust = -.1, size=10),                
        legend.position = "top")                                                          

ggsave(here("figures","Class4_KSEP_LCA_plot.png"),             #    
       dpi=300, height=4, width=6, units="in")                 #
 

# # ______________________________________________

# References

# Drew A. Linzer, Jeffrey B. Lewis (2011). poLCA: An R Package for Polytomous Variable Latent Class Analysis. Journal of Statistical Software, 42(10), 1-29. URL http://www.jstatsoft.org/v42/i10/.
# 
# Hallquist, M. N., & Wiley, J. F. (2018). MplusAutomation: An R Package for Facilitating Large-Scale Latent Variable Analyses in Mplus. Structural equation modeling: a multidisciplinary journal, 25(4), 621-638.
# 
# Miller, J. D., Hoffer, T., Suchner, R., Brown, K., & Nelson, C. (1992). LSAY codebook. Northern Illinois University.
# 
# Muthén, B. O., Muthén, L. K., & Asparouhov, T. (2017). Regression and mediation analysis using Mplus. Los Angeles, CA: Muthén & Muthén.
# 
# Muthén, L.K. and Muthén, B.O. (1998-2017).  Mplus User’s Guide.  Eighth Edition. Los Angeles, CA: Muthén & Muthén
# 
# Quirk, M., Furlong, M., Lilles, E., Felix, E., & Chin, J. (2011). Preliminary development of a kindergarten school readiness assessment for Latino students. Journal of Applied School Psychology, 27(1), 77-102.
# 
# R Core Team (2017). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL http://www.R-project.org/
#   
#   Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686, https://doi.org/10.21105/joss.01686






