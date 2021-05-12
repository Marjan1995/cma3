library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times
#Task 1
caro60 <- caro60 %>%
mutate(
nMinus3 = sqrt((lag(E,2)-E)^2+(lag(N,2)-N)^2),   
nMinus2 = sqrt((lag(E,2)-E)^2+(lag(N,2)-N)^2),   
nMinus1 = sqrt((lag(E,1)-E)^2+(lag(N,1)-N)^2),  
nPlus1  = sqrt((E-lead(E,1))^2+(N-lead(N,1))^2), 
nPlus2  = sqrt((E-lead(E,2))^2+(N-lead(N,2))^2), 
nPlus3  = sqrt((E-lead(E,2))^2+(N-lead(N,2))^2))
caro60 <- caro60 %>%
rowwise() %>%
mutate(
stepMean = mean(c(nMinus2, nMinus1,nPlus1,nPlus2))
) %>%
ungroup()

#Task 2
ggplot(caro60, aes(x= stepMean)) +
theme_set(theme_minimal()) +
geom_histogram()
summary(caro60)
caro60 <- caro60 %>%
ungroup() %>%
mutate(static = stepMean < 3.6976)
caro60_filter <- caro60 %>%
filter(!static)

#Task 3
caro60 %>%
ggplot(aes(x= E, y= N))  +
geom_path() +
geom_point(aes(colour=static)) + 
coord_equal(1) + 
labs(color = "Segments") + 
theme_set(theme_minimal()) +
theme(legend.position = "bottom")

#Task 4
rle_id <- function(vec){
x <- rle(vec)$lengths
as.factor(rep(seq_along(x), times=x))
}
caro60 <- caro60 %>%
mutate(
segment_id = rle_id(static)
)%>%
group_by(segment_id)%>%
mutate(
segment_length = n(),
segment_time = difftime(max(DatetimeUTC), min(DatetimeUTC))
)%>%
ungroup()
caro1 <- filter(caro60, !static)
caro2 <- filter(caro1, segment_time > 300)
ggplot(caro1, aes(x = E, y = N, colour = segment_id), alpha = 0.5)+
labs(title = "Trajectories")+
geom_path()+
geom_point()+
coord_fixed()
ggplot(caro2, aes(x = E, y = N, colour = segment_id), alpha = 0.5)+
labs(title = "Trajectories over 5 minutes in length")+
geom_path()+
geom_point()+
coord_fixed()

