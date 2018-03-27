require(tidyverse)

# prereq ------------------------------------------------------------------



name = 'Circle_lat_10'
size_x <- 10
size_y <- 10
circle_r <- 5
eps = 0.5


# functions ---------------------------------------------------------------

check_if_circle <- function(x,  y, cen_x = size_x/2,cen_y = size_y/2, R = circle_r, epsilon = eps) {
  (x - cen_x)^2 + (y - cen_y)^2 <= (R + epsilon)^2
}
# create lat --------------------------------------------------------------


df_points <- expand.grid(coor_x = c(0:size_x ),coor_y = c(0:size_y)) %>% 
  as.tibble() %>% 
  mutate(name = c(1:n()))

df_points %>% 
  ggplot(aes(coor_x,coor_y)) +
  geom_point() +
  geom_label(aes(label = name))


# filter points -----------------------------------------------------------


df_circle <- df_points %>% 
  filter(check_if_circle(coor_x,coor_y)) %>% 
  mutate(name = c(1:n()))

df_circle %>% 
  ggplot(aes(coor_x,coor_y)) +
  geom_point() +
  geom_label(aes(label = name))



# create edges -------------------------------------------------------------
df_edge <-NULL
res <- dist(df_circle[,c(1,2)], method = "manhattan") %>% 
  as.matrix()
res[upper.tri(res)] <- 0

for(i in c(1:NCOL(res))){
  tmp <- names(res[,i])[res[,i] == 1]
  tmp <- tibble(source = rep(i,NROW(tmp)),target = as.numeric(tmp))
  df_edge <- rbind(df_edge,tmp)
}
df_all <-  df_edge %>% 
  drop_na() %>% 
  left_join(df_circle,by = c('source' = 'name')) %>% 
  arrange(source)

df_conn <- inner_join(df_all, mutate(df_circle, target = name), by = 'target')

df_circle %>% 
  ggplot(aes(coor_x,coor_y)) +
  geom_point() +
  geom_segment(data = df_conn, 
               mapping = aes(x = coor_x.x,
                             y = coor_y.x,
                             xend = coor_x.y,
                             yend = coor_y.y))+
  geom_label(aes(label = name)) +
  theme_bw()


# write in xml ------------------------------------------------------------

beg <- str_c('<GRAPH name="',name,'" dimension="2" vertices="',NROW(df_circle),'" edges="',NROW(df_conn),'">\n\n')

for ( i in c(1:NROW(df_circle))){
  beg <- str_c(beg,'<VERTEX id="',i,'" type="0"> \n    <COORDINATE>',
               df_circle$coor_x[i],' ',df_circle$coor_y[i],'</COORDINATE>\n</VERTEX>\n')
}
for (i in c(1:NROW(df_edge))){
  beg <- str_c(beg, 
               '<EDGE source="',df_edge$source[i],
               '" target="',df_edge$target[i],
               '" id="',i,'" type="0"/>\n')
}
beg <- str_c(beg,'</GRAPH>')

cat(beg)

writeLines(beg,'res_lat/res.txt')
