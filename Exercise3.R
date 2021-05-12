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

#Task 5
library(SimilarityMeasures)
library(patchwork)
ggplot(data = pedestrian, mapping = aes(E, N))+
  geom_point(col = pedestrian$TrajID)+
  geom_path(col = pedestrian$TrajID)+
  facet_wrap(~TrajID, nrow = 2)+
  labs(title = "Different Trajectories")

#Task 6
df <- filter(pedestrian, TrajID == 1)
traj <- append(df$E, df$N)
matrix1 <- matrix(traj, length(traj)/2)
df <- filter(pedestrian, TrajID == 2)
traj <- append(df$E, df$N)
matrix2 <- matrix(traj, length(traj)/2)
df <- filter(pedestrian, TrajID == 3)
traj <- append(df$E, df$N)
matrix3 <- matrix(traj, length(traj)/2)
df <- filter(pedestrian, TrajID == 4)
traj <- append(df$E, df$N)
matrix4 <- matrix(traj, length(traj)/2)
df <- filter(pedestrian, TrajID == 5)
traj <- append(df$E, df$N)
matrix5 <- matrix(traj, length(traj)/2)
df <- filter(pedestrian, TrajID == 6)
traj <- append(df$E, df$N)
matrix6 <- matrix(traj, length(traj)/2)
dtw <- DTW(matrix1, matrix2, -1)%>%
append(DTW(matrix1, matrix3, -1))%>%
append(DTW(matrix1, matrix4, -1))%>%
append(DTW(matrix1, matrix5, -1))%>%
append(DTW(matrix1, matrix6, -1))
pd <- 10
edit_dist <- EditDist(matrix1, matrix2, pd)%>%
append(EditDist(matrix1, matrix3, pd))%>%
append(EditDist(matrix1, matrix4, pd))%>%
append(EditDist(matrix1, matrix5, pd))%>%
append(EditDist(matrix1, matrix6, pd))
frechet_dist <- Frechet(matrix1, matrix2)%>%
append(Frechet(matrix1, matrix3))%>%
append(Frechet(matrix1, matrix4))%>%
append(Frechet(matrix1, matrix5))%>%
append(Frechet(matrix1, matrix6))
lcss_dist <- Frechet(matrix1, matrix2)%>%
append(LCSS(matrix1, matrix3, 2, 2, 0.5))%>%
append(LCSS(matrix1, matrix4, 2, 2, 0.5))%>%
append(LCSS(matrix1, matrix5, 2, 2, 0.5))%>%
append(LCSS(matrix1, matrix6, 2, 2, 0.5))
trajectories <- c("2","3","4","5","6")
distance_measures <- data.frame(trajectories, dtw, edit_dist, frechet_dist, lcss_dist)
p1 <- ggplot(distance_measures, aes(x=trajectories, y=dtw, fill = trajectories)) + geom_bar(stat="identity", show.legend = FALSE) + ggtitle('Distance Time Warping')
p2 <- ggplot(distance_measures, aes(x=trajectories, y=lcss_dist, fill = trajectories)) + geom_bar(stat="identity", show.legend = FALSE) + ggtitle('Longest Common Shared Segment')
p3 <- ggplot(distance_measures, aes(x=trajectories, y=edit_dist, fill = trajectories)) + geom_bar(stat="identity", show.legend = FALSE) + ggtitle("Edit Distance")
p4 <- ggplot(distance_measures, aes(x=trajectories, y=frechet_dist, fill = trajectories)) + geom_bar(stat="identity", show.legend = FALSE) + ggtitle('Frechet Distance')
wrapped <- wrap_plots(p1,p2,p3,p4)
wrapped + plot_annotation(
title = 'Comparison of different Similarity Measures',
subtitle = 'Trajectory 1 vs. Trajectory 2-6'
)
