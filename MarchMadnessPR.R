library(tidyverse)
library(hoopR)
library(igraph)

season <- load_mbb_schedule()

season %>% arrange(date) %>% view()

season %>% filter(groups_short_name=="Am. East"|groups_short_name=="American"|
                    groups_short_name=="ASUN"|groups_short_name=="A 10"|
                    groups_short_name=="ACC"|groups_short_name=="Big 12"|
                    groups_short_name=="Big East"|groups_short_name=="Big Sky"|
                    groups_short_name=="Big Ten"|groups_short_name=="Big West"|
                    groups_short_name=="CAA"|groups_short_name=="C-USA"|
                    groups_short_name=="Horizon"|groups_short_name=="Ivy"|
                    groups_short_name=="MAAC"|groups_short_name=="MAC"|
                    groups_short_name=="MEAC"|groups_short_name=="MVC"|
                    groups_short_name=="Mountain West"|groups_short_name=="NEC"|
                    groups_short_name=="OVC"|groups_short_name=="Pac-12"|
                    groups_short_name=="Patriot"|groups_short_name=="SEC"|
                    groups_short_name=="Southern"|groups_short_name=="Southland"|
                    groups_short_name=="SWAC"|groups_short_name=="Summit"|
                    groups_short_name=="Sun Belt"|groups_short_name=="WCC"|
                    groups_short_name=="WAC") %>%
  filter(conference_competition=FALSE) %>% view()

alpha_outcomes <- season %>%
  drop_na(home_winner, away_winner) %>%
  summarize(from = if_else(away_winner, home_display_name, away_display_name), 
            to = if_else(home_winner, home_display_name, away_display_name),
            weight = 2)

one_outcomes <- season %>%
  drop_na(home_winner, away_winner) %>%
  summarize(from = if_else(home_winner, home_display_name, away_display_name),
            to = if_else(away_winner, home_display_name, away_display_name),
            weight = 1)

pr_edges <- rbind(alpha_outcomes, one_outcomes)

losers <- alpha_outcomes %>%
  distinct(from) %>%
  rename(label = from)

winners <- alpha_outcomes %>%
  distinct(to) %>%
  rename(label = to)

nodes <- full_join(losers, winners, by = "label") %>%
  rename(team = label)

pr_igraph <- graph_from_data_frame(d = pr_edges, vertices = nodes,
                                                 directed = TRUE)

plot(pr_igraph,
     vertex.size = 5,
     vertex.label.cex = 0.3,
     edge.arrow.size = 0.2)

pr_df <- page_rank(pr_igraph)
view(pr_df[["vector"]])





