# Piecewise SEM
renv::restore()
rm(list = ls())
library(piecewiseSEM)
library(ggplot2)

# read the pointdata
pointdata_init<-readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTZ5ObFosqVNTOMG6P87CCcqE4oiXYkuSQixNGR6qex1E4TbvT4wDsn6w5zC-hOVIOQRifzsbZXxuss/pub?gid=964721627&single=true&output=csv")
pointdata <- pointdata_init |> # Remove rows with missing values
  na.omit() |>   # keep complete cases
  dplyr:: filter(woody>0, woody<20)   # remove 2 extreme values and avoid interpolated negative values

# note that you should not standardize your data for a PicewiseSEM as then eg logistic regression cannot be used

# Check for missing values
sum(is.na(pointdata))
colSums(is.na(pointdata))

  
psych::pairs.panels(pointdata,stars = T, ellipses = F)


# Define the models
# Model 1: woody predicted by burnfreq, rainfall, dist2river, elevation and hills
model_woody <- lm(woody ~  burnfreq + dist2river + rainfall + elevation + hills, 
             data = pointdata)
summary(model_woody)
p1<-ggplot(data=pointdata,aes(x=burnfreq,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p1

p2<-ggplot(data=pointdata,aes(x=dist2river,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
#              method.args=list(family=Gamma(link="log")),
              formula= y~x,
              se=T) 
p2

p3<-ggplot(data=pointdata,aes(x=rainfall,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p3


p4<-ggplot(data=pointdata,aes(x=elevation,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p4

# Hills is yes or no > glm
p5<-ggplot(data=pointdata,aes(y=woody,x=hills))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),  # close to glm.nb
              formula= y~x,
              se=T)
p5


# Model_burnfreq: burning frequency predicted by dist2river and rainfall
model_burnfreq_init <- glm(burnfreq ~ dist2river + rainfall, 
              family=poisson, 
              data = pointdata)
# Calculate dispersion statistic
dispersion_stat <- summary(model_burnfreq_init)$deviance / summary(model_burnfreq_init)$df.residual
dispersion_stat
# If 𝜙≈1 : No evidence of overdispersion → Poisson is appropriate. (mean≈variance)
# If 𝜙>1 : Overdispersion is present → Consider quasi-Poisson or negative binomial.
# If 𝜙<1 : Underdispersion (less common) → Investigate the data further.

### there is overdispersion so use a quasi-Poisson

library(MASS)
model_burnfreq <- MASS::glm.nb(burnfreq ~ dist2river + rainfall, 
              data = pointdata)
summary(model_burnfreq)

p6<-ggplot(data=pointdata,aes(y=burnfreq,x=dist2river))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),  # close to glm.nb
              formula= y~x,
              se=T)
p6

p7<-ggplot(data=pointdata,aes(y=burnfreq,x=rainfall))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),
              formula= y~x,
              se=T)
p7

# model_dist2river: predicted by rainfall and elevation and hills
model_dist2river <- lm(dist2river ~  rainfall + elevation + hills, 
                  data = pointdata)
summary(model_dist2river)
p8<-ggplot(data=pointdata,aes(x=rainfall,y=dist2river))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p8


p9<-ggplot(data=pointdata,aes(x=elevation,y=dist2river))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p9

p10<-ggplot(data=pointdata,aes(x=hills,y=dist2river))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p10

# model_rainfall: rainfall predicted by elevation and hills
model_rainfall <- lm(rainfall ~  elevation + hills, 
                  data = pointdata)
summary(model_rainfall)
p11<-ggplot(data=pointdata,aes(x=elevation,y=rainfall))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p11

p12<-ggplot(data=pointdata,aes(x=hills,y=rainfall))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p12

# combine the figures
library(patchwork)
allplots<-p1+p2+p3+p4+p5+p6+p7+p8+p9+p10+p11+p12+
  patchwork::plot_layout(ncol=4) +
  patchwork::plot_annotation(title="Relations in the model")
allplots

####### Combine all models into a single piecewise SEM
psem_model <- piecewiseSEM::psem(model_woody,
                                 model_burnfreq,
                                 model_rainfall,
                                 model_dist2river)

# Summarize the SEM results
summary(psem_model)


############################

# PLOT TRIES #

###########################

# Simplest option (time)
plot(psem_model)








##### Try some adjustments
p<-plot(psem_model)

p + theme(
  text = element_text(size = 14),      # Base text size
  axis.title.x = element_text(size = 12),  # X-axis title size
  axis.title.y = element_text(size = 12),  # Y-axis title size
  plot.title = element_text(size = 16, face = "bold")  # Plot title size and bold style
)

########### Try to manually do something
#### Not well . . . 


library(ggplot2)
library(ggforce)  # For drawing arrows
library(patchwork)

# Create a custom plot for SEM
plot_model <- ggplot() +
  # Nodes (for each model variable)
  geom_point(aes(x = 1, y = 5), size = 5, color = "purple") +
  geom_point(aes(x = 2, y = 3), size = 5, color = "yellow") +
  geom_point(aes(x = 3, y = 6), size = 5, color = "lightblue") +
  geom_point(aes(x = 4, y = 8), size = 5, color = "darkblue") +
  geom_point(aes(x = 5, y = 6), size = 5, color = "red") +
  geom_point(aes(x = 6, y = 4), size = 5, color = "brown") +
  # Arrows representing relationships
  geom_mark_ellipse(aes(x = 1, y = 5, label = "elevation"), label.padding = unit(0.5, "lines")) +
  geom_mark_ellipse(aes(x = 2, y = 5, label = "hills"), label.padding = unit(0.5, "lines")) +
  geom_mark_ellipse(aes(x = 3, y = 5, label = "rainfall"), label.padding = unit(0.5, "lines")) +
  geom_mark_ellipse(aes(x = 1, y = 5, label = "dist2river"), label.padding = unit(0.5, "lines")) +
  geom_mark_ellipse(aes(x = 1, y = 5, label = "burnfreq"), label.padding = unit(0.5, "lines")) +
  geom_mark_ellipse(aes(x = 1, y = 5, label = "woody"), label.padding = unit(0.5, "lines")) +
  # Arrows between nodes
  geom_segment(aes(x = 1, y = 5, xend = 2, yend = 5), arrow = arrow(length = unit(0.2, "inches"))) +
  geom_segment(aes(x = 1, y = 5, xend = 3, yend = 5), arrow = arrow(length = unit(0.2, "inches"))) +
  # Adjust plot limits and themes
  theme_void() +
  theme(legend.position = "none")

plot_model

####################################



