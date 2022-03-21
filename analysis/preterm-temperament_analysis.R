# Lucy Vanes
# 13/01/2022
# Analysis code for:
# Lovato, Vanes, et al.
# "Early childhood temperamental trajectories following very preterm birth and their association with parenting style"

library(ggplot2)
library(jtools)
library(interactions)

setwd("C:/Users/vanes/OneDrive/Documents/GitHub/preterm-temperament/data")

dat <- read.csv("Lovato_Vanes_et_al_TemperamentParentingData.csv", header=T, fileEncoding="UTF-8-BOM")
dat$id <- factor(dat$id)

#===============================================================
#   3.2. Association between infant and childhood temperament
#===============================================================

# overall multivariate model (p-threshold .05)
#==============================================
      
anova(lm(cbind(cbq4_neg_affect_raw, cbq4_surgency_raw, cbq4_effort_control_raw) ~ m3_easy + m3_diff + sex + ga + imd_score + age4, dat))
    # Analysis of Variance Table
    # 
    # Df  Pillai approx F num Df den Df    Pr(>F)    
    # (Intercept)   1 0.99389   9807.3      3    181 < 2.2e-16 ***
    #   m3_easy       1 0.04930      3.1      3    181 0.0270332 *  
    #   m3_diff       1 0.06968      4.5      3    181 0.0044079 ** 
    #   sex           1 0.10065      6.8      3    181 0.0002422 ***
    #   ga            1 0.01900      1.2      3    181 0.3232174    
    # imd_score     1 0.02788      1.7      3    181 0.1623538    
    # age4          1 0.03781      2.4      3    181 0.0720113 .  
    # Residuals   183                                             

    
# follow up univariate models (p-threshold .05/3=.017)
#=======================================================
      
summary(lm(cbind(cbq4_neg_affect_raw, cbq4_surgency_raw, cbq4_effort_control_raw) ~ m3_easy + m3_diff + sex + ga + imd_score + age4, dat))
    
    # Response cbq4_neg_affect_raw :
    #   
    #   
    # Coefficients:
    #   Estimate Std. Error t value Pr(>|t|)    
    # (Intercept)  3.789568   0.736443   5.146 6.83e-07 ***
    #   m3_easy     -0.063644   0.029219  -2.178  0.03068 *  
    #   m3_diff      0.154658   0.048660   3.178  0.00174 ** 
    #   sexmale     -0.013188   0.091247  -0.145  0.88524    
    # ga           0.025339   0.019470   1.301  0.19475    
    # imd_score    0.008193   0.003753   2.183  0.03029 *  
    #   age4        -0.042383   0.061277  -0.692  0.49003    

    
    # Response cbq4_surgency_raw :
    #   
    # 
    # Coefficients:
    #   Estimate Std. Error t value Pr(>|t|)    
    # (Intercept)  5.1490225  0.7491291   6.873 9.54e-11 ***
    #   m3_easy      0.0303557  0.0297227   1.021    0.308    
    # m3_diff     -0.0808652  0.0494982  -1.634    0.104    
    # sexmale     -0.3711191  0.0928188  -3.998 9.25e-05 ***
    #   ga           0.0145134  0.0198058   0.733    0.465    
    # imd_score   -0.0007198  0.0038175  -0.189    0.851    
    # age4        -0.0840662  0.0623324  -1.349    0.179    
    
    
    
    # Response cbq4_effort_control_raw :
    #   
    #  
    # Coefficients:
    #   Estimate Std. Error t value Pr(>|t|)    
    # (Intercept)  5.407018   0.554136   9.758   <2e-16 ***
    #   m3_easy     -0.038105   0.021986  -1.733   0.0848 .  
    # m3_diff      0.023823   0.036614   0.651   0.5161    
    # sexmale      0.082743   0.068659   1.205   0.2297    
    # ga          -0.008455   0.014650  -0.577   0.5646    
    # imd_score    0.003109   0.002824   1.101   0.2724    
    # age4        -0.116203   0.046108  -2.520   0.0126 *  


# Figure 1
#============

m0 <- lm(cbq4_neg_affect_raw ~ m3_easy + m3_diff + sex + ga + imd_score + age4, dat)


# png( "Figure1_new.jpeg",  width = 6, height = 6, units = 'in', res = 300)
effect_plot(m0, pred = m3_diff, interval = TRUE, plot.points = TRUE, point.size=1.5,
            point.alpha=0.4, point.color="black",
            x.label="Infant difficult temperament", y.label="Childhood Negative Affectivity (partial residuals)",
            partial.residuals = TRUE) +
              theme(axis.text=element_text(size=12, face="bold"),
              axis.title=element_text(size=16,face="bold"))
# dev.off()

#====================================================================================================
#   3.3. Influence of parenting style on the association between infant and childhood temperament 
#====================================================================================================

# Multivariate model comparisons  (p-threshold .05/6 = .008)
#============================================================
    
# Laxness
#=========
m1 <- lm(cbind(cbq4_neg_affect_raw, cbq4_surgency_raw, cbq4_effort_control_raw) ~ m3_easy + m3_diff + sex + ga + imd_score + age4, dat[which(!is.na(dat$parenting4_laxness_raw)),])
m2_a <- lm(cbind(cbq4_neg_affect_raw, cbq4_surgency_raw, cbq4_effort_control_raw) ~ m3_easy*parenting4_laxness_raw + m3_diff + sex + ga + imd_score + age4, dat[which(!is.na(dat$parenting4_laxness_raw)),])
m2_b <- lm(cbind(cbq4_neg_affect_raw, cbq4_surgency_raw, cbq4_effort_control_raw) ~ m3_easy + m3_diff *parenting4_laxness_raw + sex + ga + imd_score + age4, dat[which(!is.na(dat$parenting4_laxness_raw)),])


# model comparisons  (p-threshold .05/6 = .008)

anova(m1, m2_a)

# Model 1: cbind(cbq4_neg_affect_raw, cbq4_surgency_raw, cbq4_effort_control_raw) ~ 
#   m3_easy + m3_diff + sex + ga + imd_score + age4
# Model 2: cbind(cbq4_neg_affect_raw, cbq4_surgency_raw, cbq4_effort_control_raw) ~ 
#   m3_easy * parenting4_laxness_raw + m3_diff + sex + ga + imd_score + 
#   age4
# Res.Df Df Gen.var.  Pillai approx F num Df den Df   Pr(>F)   
# 1     94     0.28780                                           
# 2     92 -2  0.27283 0.20908   3.5412      6    182 0.002435 **

anova(m1, m2_b)

#Analysis of Variance Table
#
#Model 1: cbind(cbq4_neg_affect_raw, cbq4_surgency_raw, cbq4_effort_control_raw) ~ 
#  m3_easy + m3_diff + sex + ga + imd_score + age4
#Model 2: cbind(cbq4_neg_affect_raw, cbq4_surgency_raw, cbq4_effort_control_raw) ~ 
#  m3_easy + m3_diff * parenting4_laxness_raw + sex + ga + imd_score + 
#  age4
#Res.Df Df Gen.var.  Pillai approx F num Df den Df  Pr(>F)  
#1     94     0.28780                                         
#2     92 -2  0.27739 0.16291   2.6898      6    182 0.01588 *
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

    
# --- m2_a is winning model --- 
    
# follow-up (p-threshold .008/3 = .003)
#=======================================
summary(m2_a)
    
# Response cbq4_neg_affect_raw :
#   
#   Call:
#   lm(formula = cbq4_neg_affect_raw ~ m3_easy * parenting4_laxness_raw + 
#        m3_diff + sex + ga + imd_score + age4, data = dat[which(!is.na(dat$parenting4_laxness_raw)), 
#        ])
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.37345 -0.36738  0.07321  0.35557  1.03117 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                     6.733367   1.232660   5.462 3.97e-07 ***
#   m3_easy                        -0.719815   0.169324  -4.251 5.10e-05 ***
#   parenting4_laxness_raw         -1.083936   0.282305  -3.840 0.000226 ***
#   m3_diff                         0.158661   0.063788   2.487 0.014672 *  
#   sexmale                        -0.031057   0.115365  -0.269 0.788374    
# ga                              0.032109   0.023203   1.384 0.169751    
# imd_score                       0.008210   0.004809   1.707 0.091183 .  
# age4                            0.014121   0.070633   0.200 0.841986    
# m3_easy:parenting4_laxness_raw  0.202813   0.050545   4.013 0.000122 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.543 on 92 degrees of freedom
# (18 observations deleted due to missingness)
# Multiple R-squared:  0.2627,	Adjusted R-squared:  0.1986 
# F-statistic: 4.097 on 8 and 92 DF,  p-value: 0.0003274
# 
# 
# Response cbq4_surgency_raw :
#   
#   Call:
#   lm(formula = cbq4_surgency_raw ~ m3_easy * parenting4_laxness_raw + 
#        m3_diff + sex + ga + imd_score + age4, data = dat[which(!is.na(dat$parenting4_laxness_raw)), 
#        ])
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.68893 -0.39353 -0.00483  0.36622  1.32605 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                     6.087381   1.391005   4.376 3.18e-05 ***
#   m3_easy                        -0.088194   0.191075  -0.462  0.64548    
# parenting4_laxness_raw         -0.099468   0.318569  -0.312  0.75557    
# m3_diff                        -0.120746   0.071982  -1.677  0.09685 .  
# sexmale                        -0.352589   0.130184  -2.708  0.00806 ** 
#   ga                             -0.012672   0.026183  -0.484  0.62954    
# imd_score                      -0.000391   0.005427  -0.072  0.94273    
# age4                           -0.066190   0.079707  -0.830  0.40845    
# m3_easy:parenting4_laxness_raw  0.042305   0.057038   0.742  0.46016    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.6128 on 92 degrees of freedom
# (18 observations deleted due to missingness)
# Multiple R-squared:  0.1648,	Adjusted R-squared:  0.09218 
# F-statistic: 2.269 on 8 and 92 DF,  p-value: 0.02913
# 
# 
# Response cbq4_effort_control_raw :
#   
#   Call:
#   lm(formula = cbq4_effort_control_raw ~ m3_easy * parenting4_laxness_raw + 
#        m3_diff + sex + ga + imd_score + age4, data = dat[which(!is.na(dat$parenting4_laxness_raw)), 
#        ])
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.98718 -0.28373  0.03209  0.33177  0.84594 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                     7.296596   1.043120   6.995 4.17e-10 ***
#   m3_easy                        -0.379700   0.143288  -2.650  0.00948 ** 
#   parenting4_laxness_raw         -0.577973   0.238896  -2.419  0.01752 *  
#   m3_diff                         0.024464   0.053979   0.453  0.65147    
# sexmale                         0.085116   0.097626   0.872  0.38556    
# ga                             -0.006931   0.019635  -0.353  0.72489    
# imd_score                       0.001738   0.004070   0.427  0.67032    
# age4                           -0.093382   0.059772  -1.562  0.12165    
# m3_easy:parenting4_laxness_raw  0.093296   0.042773   2.181  0.03172 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4595 on 92 degrees of freedom
# (18 observations deleted due to missingness)
# Multiple R-squared:  0.1484,	Adjusted R-squared:  0.07436 
# F-statistic: 2.004 on 8 and 92 DF,  p-value: 0.05445


# Simple slope analysis
##====================

library(reghelper)
m2a_1 <- lm(cbq4_neg_affect_raw ~ m3_easy * parenting4_laxness_raw + m3_diff + sex + ga + imd_score + age4, dat)

m <- mean(dat$parenting4_laxness_raw, na.rm=T)
sd <- sd(dat$parenting4_laxness_raw, na.rm=T)

m_min1sd <- m-sd
m_1sd <- m+sd

m_min15sd <- m - 1.5*sd
m_15sd <- m + 1.5*sd

# +/- 1SD of parenting:
simple_slopes(m2a_1,levels=list(parenting4_laxness_raw=c(m_min1sd, m_1sd, 'sstest')))

# +/- 1.5SD of parenting:
simple_slopes(m2a_1,levels=list(parenting4_laxness_raw=c(m_min15sd, m_15sd, 'sstest')))

# Figure 2
#============
# png( "Figure2_new.jpeg",  width = 6, height = 6, units = 'in', res = 300)

interact_plot(m2a_1, pred = m3_easy, modx = parenting4_laxness_raw, modx.values = c(m_min15sd, m_min1sd,m, m_1sd,m_15sd), interval = TRUE, plot.points = TRUE, point.size=1.5,
              point.alpha=0.9, point.color="black",x.label="Infant easy temperament", 
              y.label="Childhood Negative Affectivity (partial residuals)", modx.labels =c("- 1.5 SD", "- 1 SD","Mean", "+ 1 SD", "+ 1.5 SD"),
              legend.main = "Parental Laxness", partial.residuals = TRUE, colors = "blue")  +
  theme(axis.text=element_text(size=12, face="bold"),
        axis.title=element_text(size=16,face="bold"),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14))
# dev.off()
  
# Overreactivity
#===================
m1 <- lm(cbind(cbq4_neg_affect_raw, cbq4_surgency_raw, cbq4_effort_control_raw) ~ m3_easy + m3_diff + sex + ga + imd_score + age4, dat[which(!is.na(dat$parenting4_laxness_raw)),])
m2_a <- lm(cbind(cbq4_neg_affect_raw, cbq4_surgency_raw, cbq4_effort_control_raw) ~ m3_easy*parenting4_overreactivity_raw + m3_diff + sex + ga + imd_score + age4, dat[which(!is.na(dat$parenting4_laxness_raw)),])
m2_b <- lm(cbind(cbq4_neg_affect_raw, cbq4_surgency_raw, cbq4_effort_control_raw) ~ m3_easy + m3_diff *parenting4_overreactivity_raw + sex + ga + imd_score + age4, dat[which(!is.na(dat$parenting4_laxness_raw)),])

# model comparisons (p-threshold .05/6 = .008)

anova(m1,m2_a)
# Analysis of Variance Table
# 
# Model 1: cbind(cbq4_neg_affect_raw, cbq4_surgency_raw, cbq4_effort_control_raw) ~ 
#   m3_easy + m3_diff + sex + ga + imd_score + age4
# Model 2: cbind(cbq4_neg_affect_raw, cbq4_surgency_raw, cbq4_effort_control_raw) ~ 
#   m3_easy * parenting4_overreactivity_raw + m3_diff + sex + 
#   ga + imd_score + age4
# Res.Df Df Gen.var.   Pillai approx F num Df den Df Pr(>F)
# 1     94     0.28780                                       
# 2     92 -2  0.28495 0.091539   1.4549      6    182 0.1961

anova(m1,m2_b)
# Analysis of Variance Table
# 
# Model 1: cbind(cbq4_neg_affect_raw, cbq4_surgency_raw, cbq4_effort_control_raw) ~ 
#   m3_easy + m3_diff + sex + ga + imd_score + age4
# Model 2: cbind(cbq4_neg_affect_raw, cbq4_surgency_raw, cbq4_effort_control_raw) ~ 
#   m3_easy + m3_diff * parenting4_overreactivity_raw + sex + 
#   ga + imd_score + age4
# Res.Df Df Gen.var.  Pillai approx F num Df den Df  Pr(>F)  
# 1     94     0.28780                                         
# 2     92 -2  0.28257 0.11395   1.8326      6    182 0.09501 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# --- adding overreactivity does not significantly improve the model ---


# Verbosity
#==============
m1 <- lm(cbind(cbq4_neg_affect_raw, cbq4_surgency_raw, cbq4_effort_control_raw) ~ m3_easy + m3_diff + sex + ga + imd_score + age4, dat[which(!is.na(dat$parenting4_laxness_raw)),])
m2_a <- lm(cbind(cbq4_neg_affect_raw, cbq4_surgency_raw, cbq4_effort_control_raw) ~ m3_easy*parenting4_verbosity_raw + m3_diff + sex + ga + imd_score + age4, dat[which(!is.na(dat$parenting4_laxness_raw)),])
m2_b <- lm(cbind(cbq4_neg_affect_raw, cbq4_surgency_raw, cbq4_effort_control_raw) ~ m3_easy + m3_diff *parenting4_verbosity_raw + sex + ga + imd_score + age4, dat[which(!is.na(dat$parenting4_laxness_raw)),])

# model comparisons (p-threshold .05/6 = .008)

anova(m1, m2_a)

# Analysis of Variance Table
# 
# Model 1: cbind(cbq4_neg_affect_raw, cbq4_surgency_raw, cbq4_effort_control_raw) ~ 
#   m3_easy + m3_diff + sex + ga + imd_score + age4
# Model 2: cbind(cbq4_neg_affect_raw, cbq4_surgency_raw, cbq4_effort_control_raw) ~ 
#   m3_easy * parenting4_verbosity_raw + m3_diff + sex + ga + 
#   imd_score + age4
# Res.Df Df Gen.var.   Pillai approx F num Df den Df Pr(>F)
# 1     94     0.28780                                       
# 2     92 -2  0.28683 0.072869    1.147      6    182  0.337

anova(m1, m2_b)

# Analysis of Variance Table
# 
# Model 1: cbind(cbq4_neg_affect_raw, cbq4_surgency_raw, cbq4_effort_control_raw) ~ 
#   m3_easy + m3_diff + sex + ga + imd_score + age4
# Model 2: cbind(cbq4_neg_affect_raw, cbq4_surgency_raw, cbq4_effort_control_raw) ~ 
#   m3_easy + m3_diff * parenting4_verbosity_raw + sex + ga + 
#   imd_score + age4
# Res.Df Df Gen.var.   Pillai approx F num Df den Df Pr(>F)
# 1     94     0.28780                                       
# 2     92 -2  0.28551 0.086278   1.3675      6    182 0.2299

# --- adding verbosity does not significantly improve the model ---


