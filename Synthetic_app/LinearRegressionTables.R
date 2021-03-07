###############################################################################
######################### Linear Regression Summaries ######################### 
###############################################################################

# This function is for both Groups 
# Output is a HTML table put for one of the panels inside the Shiny App 


# Demographic Interplay

LinearRegressionTables <- function(dat, biomarker, title_biomarker){
  
  linear_regression <- lm(biomarker ~ Age+Sex+Education_binary+apoe4, data = dat)
  p_value_df <- as.data.frame(summary(linear_regression)$coefficients)
  new_pvalue <- p_value_df %>%
    mutate(across(where(is.numeric), ~signif(.,digits = 2)))
  rownames(new_pvalue) = c("Intercept","Age", "Sex (Male)", "Education Binary (under 12 years education)", "apoe4 (non-carrier")
  mytableout = htmlTable(
    new_pvalue, 
    caption = title_biomarker, 
    tfoot = paste("R-squared value =",signif(summary(linear_regression)$r.squared,2), "Adjusted R-squared value = ",
                  signif(summary(linear_regression)$adj.r.squared,2), sep = "   ")
  )
  mytableout %>%
    addHtmlTableStyle(col.rgroup = c("none", "#F7F7F7")) %>%
    htmlTable
  
}



# Biomarker interplay 

BiomarkerInterplay <- function(dat, biomarker1,biomarker2,biomarker3,title_biomarker1, title_biomarker2,title_biomarker3, 
                               title_caption){
  
  regression <- lm(biomarker1 ~ biomarker2+biomarker3, data = dat)
  df_pvalue <- as.data.frame(summary(regression)$coefficients)
  new_df <- df_pvalue %>%
    mutate(across(where(is.numeric), ~ signif(.,digits = 2)))
  rownames(new_df) = c("Intercept", paste0(title_biomarker2), paste0(title_biomarker3))
  mytableout = htmlTable(
    new_df, 
    caption = title_caption, 
    tfoot = paste("R-squared value =",signif(summary(regression)$r.squared,2), "Adjusted R-squared value = ",
                  signif(summary(regression)$adj.r.squared,2), sep = "   ")
  )
  mytableout %>%
    addHtmlTableStyle(col.rgroup = c("none", "#F7F7F7")) %>%
    htmlTable
  
}


LogisticSummary <- function(dat, xinput, caption_title){

  mylogit <- glm(xinput ~ Age+ Education_binary+Sex+apoe4, data = dat, family = "binomial")
  summary_pvals <- as.data.frame(summary(mylogit)$coefficients)
  new_sum <- summary_pvals %>%
    mutate(across(where(is.numeric), ~ signif(.,digits = 2)))
  rownames(new_sum) = c("Intercept","Age","Sex (Male)","Education Binary (under 12 years education)", "apoe4 (non-carrier)")
  mytableout = htmlTable (
    new_sum,
    caption = caption_title
  )
  mytableout %>%
    addHtmlTableStyle(col.rgroup = c("none", "#F7F7F7")) %>%
    htmlTable

}

