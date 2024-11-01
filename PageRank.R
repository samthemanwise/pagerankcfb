library(tidyverse)
library(network)
library(cfbfastR)

# API key for cfbfastR access
Sys.setenv(CFBD_API_KEY = "2FWP5DlOlflwSogFGVVkGIU3qi3dySMa0njW4iG1fm65v9LWjKxbCWkD0uGWT0SO")

cfbd_game_info(2019) %>% view()

outcomes <- cfbd_game_info(2019) %>% 
  drop_na(home_points, away_points) %>%
  summarize(loser = if_else(home_points < away_points, home_team, away_team), 
            winner = if_else(home_points > away_points, home_team, away_team))
view(outcomes)

# node list
losers <- outcomes %>%
  distinct(loser) %>%
  rename(label = loser)

winners <- outcomes %>%
  distinct(winner) %>%
  rename(label = winner)

nodes <- full_join(losers, winners, by = "label") %>%
  rename(team = label)
view(nodes)

# edge list
outcomes

edges <- outcomes %>% 
  rename(from = loser) %>% 
  rename(to = winner)
view(edges)

# network
games_network <- network(edges, vertex.attr = nodes, 
                         vertex.attrnames = nodes,
                         mode = "circle",
                         multiple = TRUE)

plot(games_network)

# igraph
detach(package:network)
library(igraph)

games_igraph <- graph_from_data_frame(d = edges,
                                       vertices = nodes,
                                       directed = TRUE)

plot(games_igraph,
     vertex.size = 10,
     vertex.label.cex = 0.3,
     edge.arrow.size = 0.2)

# pagerank

df <- page_rank(games_igraph)
cfb_pr <- as_tibble(df[["vector"]])
cfb_pr <- cfb_pr %>% arrange(desc(value)) %>% head(25)

cfb_names <- tibble(school = c("LSU", "Kansas State", "Oklahoma", "Ohio State",
                               "Georgia", "Memphis", "Temple", "Baylor",
                               "BYU", "Oregon", "Wisconsin", "West Virginia",
                               "Auburn", "Texas", "Notre Dame", "Oklahoma State",
                               "Cincinnati", "Michigan", "Arizona State",
                               "Penn State", "Florida", "Boise State", "Utah",
                               "Cornell", "Dartmouth"))

pr_rankings_2019 <- cfb_names %>% 
  rowid_to_column("pr_rank")
view(pr_rankings_2019)

# ap rankings
ap_rankings_2019 <- cfbd_rankings(year = 2019, week = 14) %>%
  filter(poll == "AP Top 25") %>%
  select(school, rank) %>%
  rename(ap_rank = rank)
view(ap_rankings_2019)

# join rankings

join_rankings_2019 <- full_join(ap_rankings_2019, pr_rankings_2019, by="school")
view(join_rankings_2019)

# just FBS pagerank

fbs_outcomes <- cfbd_game_info(2019) %>%
  filter(home_division == "fbs" & away_division == "fbs") %>%
  drop_na(home_points, away_points) %>%
  summarize(loser = if_else(home_points < away_points, home_team, away_team), 
            winner = if_else(home_points > away_points, home_team, away_team))
view(fbs_outcomes)

# FBS node list
fbs_losers <- fbs_outcomes %>%
  distinct(loser) %>%
  rename(label = loser)

fbs_winners <- fbs_outcomes %>%
  distinct(winner) %>%
  rename(label = winner)

fbs_nodes <- full_join(fbs_losers, fbs_winners, by = "label") %>%
  rename(team = label)
view(fbs_nodes)

# FBS edge list
fbs_outcomes

fbs_edges <- fbs_outcomes %>% 
  rename(from = loser) %>% 
  rename(to = winner)
view(fbs_edges)

# FBS igraph
fbs_games_igraph <- graph_from_data_frame(d = fbs_edges,
                                      vertices = fbs_nodes,
                                      directed = TRUE)

plot(fbs_games_igraph,
     vertex.size = 5,
     vertex.label.cex = 0.25,
     edge.arrow.size = 0.1)

# FBS pagerank

fbs_df <- page_rank(fbs_games_igraph)
view(fbs_df[["vector"]])
fbs_cfb_pr <- as_tibble(fbs_df[["vector"]])
fbs_cfb_pr <- fbs_cfb_pr %>% arrange(desc(value)) %>% head(25)
view(fbs_cfb_pr)

fbs_cfb_names <- tibble(school = c("Ohio State", "LSU", "Kansas State", "Oklahoma",
                               "Georgia", "Temple", "Memphis", "Wisconsin",
                               "BYU", "Oregon", "Notre Dame", "Michigan",
                               "Baylor", "Auburn", "Penn State", "Cincinnati",
                               "Georgia Southern", "Minnesota", "Utah",
                               "Texas", "South Carolina", "USC", "Iowa",
                               "Clemson", "Florida"))

fbs_pr_rankings_2019 <- fbs_cfb_names %>% 
  rowid_to_column("fbs_pr_rank")
view(fbs_pr_rankings_2019)

# FBS join rankings
fbs_join_rankings_2019 <- full_join(ap_rankings_2019, fbs_pr_rankings_2019,
                                    by="school")
view(fbs_join_rankings_2019)


# using score differential as weight

sd_fbs_outcomes <- cfbd_game_info(2019) %>%
  filter(home_division == "fbs" & away_division == "fbs") %>%
  drop_na(home_points, away_points) %>%
  summarize(loser = if_else(home_points < away_points, home_team, away_team), 
            winner = if_else(home_points > away_points, home_team, away_team),
            score_diff = if_else(home_points < away_points, 
                                 away_points-home_points,
                                 home_points-away_points))
view(sd_fbs_outcomes)

sd_fbs_edges <- sd_fbs_outcomes %>% 
  rename(from = loser) %>% 
  rename(to = winner) %>%
  rename(weight = score_diff)
view(sd_fbs_edges)

# score diffgraph

sd_fbs_games_igraph <- graph_from_data_frame(d = sd_fbs_edges,
                                          vertices = fbs_nodes,
                                          directed = TRUE)

plot(sd_fbs_games_igraph,
     vertex.size = 5,
     vertex.label.cex = 0.25,
     edge.arrow.size = 0.1)

# score diff weighted pagerank

sd_fbs_df <- page_rank(sd_fbs_games_igraph)
view(sd_fbs_df[["vector"]])
sd_fbs_cfb_pr <- as_tibble(sd_fbs_df[["vector"]])
sd_fbs_cfb_pr <- sd_fbs_cfb_pr %>% arrange(desc(value)) %>% head(25)
view(sd_fbs_cfb_pr)

sd_fbs_cfb_names <- tibble(school = c("Ohio State", "LSU", "Kansas State", "Oklahoma",
                                   "Baylor", "Oregon", "Auburn", "Georgia",
                                   "Wisconsin", "Michigan", "Utah", "Oklahoma State",
                                   "Florida", "Clemson", "Notre Dame", "Temple",
                                   "Memphis", "UCF", "BYU",
                                   "Navy", "Georgia Southern", "Washington", "Cincinnati",
                                   "Penn State", "Appalachian State"))

sd_fbs_pr_rankings_2019 <- sd_fbs_cfb_names %>% 
  rowid_to_column("sd_pr_rank")
view(sd_fbs_pr_rankings_2019)

# sd FBS join rankings
sd_fbs_join_rankings_2019 <- full_join(ap_rankings_2019, sd_fbs_pr_rankings_2019,
                                    by="school")
view(sd_fbs_join_rankings_2019)

# all joint rankings

join <- full_join(join_rankings_2019, fbs_pr_rankings_2019, by = "school")

all_join_rankings_2019 <- full_join(join, sd_fbs_pr_rankings_2019,
                                    by = "school")
view(all_join_rankings_2019)

# Kansas state high in PR, upset value?
# try 2007, Standford upset against USC

fbs_outcomes_2007 <- cfbd_game_info(2007) %>%
  filter(home_division == "fbs" & away_division == "fbs") %>%
  drop_na(home_points, away_points) %>%
  summarize(loser = if_else(home_points < away_points, home_team, away_team), 
            winner = if_else(home_points > away_points, home_team, away_team))

fbs_losers_2007 <- fbs_outcomes_2007 %>%
  distinct(loser) %>%
  rename(label = loser)

fbs_winners_2007 <- fbs_outcomes_2007 %>%
  distinct(winner) %>%
  rename(label = winner)

fbs_nodes_2007 <- full_join(fbs_losers_2007, fbs_winners_2007, by = "label") %>%
  rename(team = label)
view(fbs_nodes_2007)

fbs_edges_2007 <- fbs_outcomes_2007 %>% 
  rename(from = loser) %>% 
  rename(to = winner)
view(fbs_edges_2007)

fbs_games_igraph_2007 <- graph_from_data_frame(d = fbs_edges_2007,
                                          vertices = fbs_nodes_2007,
                                          directed = TRUE)

plot(fbs_games_igraph_2007,
     vertex.size = 10,
     vertex.label.cex = 0.3,
     edge.arrow.size = 0.2)

# 2007 pagerank

fbs_df_2007 <- page_rank(fbs_games_igraph_2007)
view(fbs_df_2007[["vector"]])

fbs_cfb_pr_2007 <- as_tibble(fbs_df_2007[["vector"]])
fbs_cfb_pr_2007 <- fbs_cfb_pr_2007 %>% arrange(desc(value)) %>% head(25)
view(fbs_cfb_pr_2007)

fbs_cfb_names_2007 <- tibble(school = c("Oklahoma", "LSU", "Missouri", "Oregon",
                                   "Colorado", "Tennessee", "Illinois", "Kentucky",
                                   "USC", "Virginia Tech", "Texas Tech",
                                   "Boston College", "South Florida", "Arkansas",
                                   "West Virginia", "UCLA", "Georgia", "Arizona State",
                                   "Cincinnati", "Florida", "Oregon State", 
                                   "South Carolina", "Pittsburgh", "Florida State",
                                   "Ohio State"))

fbs_pr_rankings_2007 <- fbs_cfb_names_2007 %>% 
  rowid_to_column("fbs_pr_rank")
view(fbs_pr_rankings_2007)

ap_rankings_2007 <- cfbd_rankings(year = 2007, week = 14) %>%
  filter(poll == "AP Top 25") %>%
  select(school, rank) %>%
  rename(ap_rank = rank)

fbs_join_rankings_2007 <- full_join(ap_rankings_2007, fbs_pr_rankings_2007,
                                    by="school")
view(fbs_join_rankings_2007)

# seems to value wins against higher ranked teams more than losses against 
# unranked teams
# could be because the value of a win against a high rank(link from higher PR site)
# gives a lot more than a loss of a win against an unranked team
# (missing out on link from lower PR site)
# good example is 2007 Colorado, 6-7 record (5 losses coming from unranked teams)
# beat 3rd rank Oklahoma week 5, ended up with pr rank of 5, compared to unranked
# in ap rankings
# same with 2019 Kansas State, 8-5 record, unranked in ap end of season, week 7 beat
# 5th ranked Oklahoma, pr rank of 3rd
# How to properly weigh upsets?
# Lower damping factor("upset factor")? Reverse edges from winners to losers?
# Dream is to have better rankings than AP
# can go through the iterations, AP to Pagerank to next iteration, find best form rankings

# using (1,alpha) instead of (0,1)

alpha_outcomes_2007 <- cfbd_game_info(2007) %>%
  filter(home_division == "fbs" & away_division == "fbs") %>%
  drop_na(home_points, away_points) %>%
  summarize(from = if_else(home_points < away_points, home_team, away_team), 
            to = if_else(home_points > away_points, home_team, away_team),
            weight = 2)

one_outcomes_2007 <- cfbd_game_info(2007) %>%
  filter(home_division == "fbs" & away_division == "fbs") %>%
  drop_na(home_points, away_points) %>%
  summarize(from = if_else(home_points > away_points, home_team, away_team),
            to = if_else(home_points < away_points, home_team, away_team),
            weight = 1)

alpha_edges_2007 <- rbind(alpha_outcomes_2007, one_outcomes_2007)

fbs_nodes_2007

alpha_games_igraph_2007 <- graph_from_data_frame(d = alpha_edges_2007,
                                               vertices = fbs_nodes_2007,
                                               directed = TRUE)

plot(alpha_games_igraph_2007,
     vertex.size = 10,
     vertex.label.cex = 0.3,
     edge.arrow.size = 0.2)

# Pagerank

alpha_df_2007 <- page_rank(alpha_games_igraph_2007)
view(alpha_df_2007[["vector"]])

alpha_cfb_names_2007 <- tibble(school = c("LSU", "Oklahoma", "Missouri", "Virginia Tech",
                                        "Tennessee", "Boston College", "Florida", "West Virginia",
                                        "Oregon", "Georgia", "USC",
                                        "Arizona State", "Ohio State", "Illinois",
                                        "Virginia", "South Florida", "UCLA", "Michigan",
                                        "Florida State", "Clemson", "Texas", 
                                        "Penn State", "Cincinnati", "Oregon State",
                                        "Wake Forest"))

alpha_pr_rankings_2007 <- alpha_cfb_names_2007 %>% 
  rowid_to_column("alpha_pr_rank")

all_join_rankings_2007 <- full_join(fbs_join_rankings_2007, alpha_pr_rankings_2007,
                                    by="school")
view(all_join_rankings_2007)

# HITS(Hubs and Authorities)
library('centiserve')
hub_2007 <- hub_score(alpha_games_igraph_2007)
view(hub_2007[["vector"]])

auth_score_2007 <- authority_score(alpha_games_igraph_2007)
view(auth_score_2007[["vector"]])

eigen_2007 <- eigen_centrality(alpha_games_igraph_2007)
view(eigen_2007[["vector"]])

salsa_2007 <- salsa(alpha_games_igraph_2007, score = "authority")
view(salsa_2007)

# (1,alpha) + score diffential

sd_fbs_outcomes_2007 <- cfbd_game_info(2007) %>%
  filter(home_division == "fbs" & away_division == "fbs") %>%
  drop_na(home_points, away_points) %>%
  summarize(from = if_else(home_points < away_points, home_team, away_team), 
            to = if_else(home_points > away_points, home_team, away_team),
            weight = if_else(home_points < away_points, 
                                 away_points-home_points,
                                 home_points-away_points))
one_outcomes_2007

sd_alpha_edges_2007 <- rbind(sd_fbs_outcomes_2007, one_outcomes_2007)

sd_alpha_games_igraph_2007 <- graph_from_data_frame(d = sd_alpha_edges_2007,
                                                 vertices = fbs_nodes_2007,
                                                 directed = TRUE)

sd_alpha_df_2007 <- page_rank(sd_alpha_games_igraph_2007)
view(sd_alpha_df_2007[["vector"]])





