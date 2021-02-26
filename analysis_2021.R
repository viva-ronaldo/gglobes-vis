#Analysis for new Golden Globes post

library(dplyr)
library(ggplot2)
library(magrittr)
library(waffle)
library(piecewiseSEM)
library(tidyr)
library(broom)
library(gt)
library(caret)
library(Metrics)

setwd('~/projects/gglobes_vis/')
noms_filepath <- 'data/gglobes_data_wikipedia_1972to2020.csv'
nominations <- read.csv(noms_filepath)
nominations$category[nominations$category=='Show Drama'] <- 'TV Drama'
nominations$category[nominations$category=='Show Comedy'] <- 'TV Comedy'
nominations$result <- ifelse(nominations$won==1, 'Won', 'Lost')
nominations$cat_group <- ifelse(grepl('TV',nominations$category), 'series', 'acting')
nominations$candidate <- ifelse(nominations$cat_group=='series', nominations$show, nominations$performer)

#TODO add these to show_data
# [1] "A Year in the Life"                      "Alls Fair"                              
# [3] "America"                                 "Arnie"                                  
# [5] "Blind Ambition"                          "Carol & Company"                        
# [7] "Civil Wars"                              "Donny and Marie"                        
# [9] "General Hospital"                        "Harold Robbins 79 Park Avenue"          
# [11] "House Calls"                             "Love, Sidney"                           
# [13] "Nothing Sacred"                          "Notorious Woman"                        
# [15] "Private Benjamin"                        "Pros and Cons"                          
# [17] "Sammy and Company"                       "Sirotas Court"                          
# [19] "Sister Kate"                             "The Homecoming: A Christmas Story"      
# [21] "The New Bill Cosby Show"                 "The Scarlett OHara War"                 
# [23] "The Slap Maxwell Story"                  "The Tonight Show Starring Johnny Carson"
# [25] "The Tony Randall Show"                   "Wheels"

show_data <- read.csv('data/imdb_data_gglobesnominees_wikipedianames_1972to2020.csv')
#nrow(show_data)  #1924
#show_data %>% select(show,season_start_year) %>% filter(!duplicated(.)) %>% nrow()  #1851
#Sometimes there are two seasons in the same year; combine in this case
show_data <- show_data %>% group_by(show, season_start_year) %>% 
    summarise(season=first(season),
              av_rating = round(weighted.mean(av_rating, num_eps),2),
              av_num_votes = round(weighted.mean(av_num_votes, num_eps),0),
              num_eps = sum(num_eps), .groups='drop')
#Not many missing now that I am using y-1 and y-2 averages
#UK Office 04 - could refer to both series; 'The Office' in show_data refers to US Office (starts 05) -> just enter values manually here
#Corrected V's Closet s1 in file to start in 1997;  
show_data <- rbind(show_data,
                   data.frame(show='The Office', season_start_year=2003, season=2, av_rating=8.6, av_num_votes=1150, num_eps=6))
#The rest were not found in IMDB search                   


#Awards will be given out the year after the season started TODO or should it be two years after?
nominations$year_minus1 <- nominations$year - 1
nominations$year_minus2 <- nominations$year - 2

#Calculate nom and win history - performers independent of show and category (rarely matters)

nominations <- nominations %>% arrange(year) %>% 
    group_by(performer) %>% mutate(prev_noms = lag(row_number(), default=0),
                                   prev_wins = lag(cumsum(won), default=0)) %>% ungroup() %>%
    group_by(show, category) %>% mutate(prev_noms_show = lag(row_number(), default=0),
                              prev_wins_show = lag(cumsum(won), default=0)) %>% ungroup() %>%
    mutate(prev_noms = ifelse(cat_group=='acting', prev_noms, prev_noms_show),
           prev_wins = ifelse(cat_group=='acting', prev_wins, prev_wins_show)) %>%
    select(-prev_noms_show, -prev_wins_show) %>%
    mutate(prev_win_rate = ifelse(prev_noms > 0, prev_wins/prev_noms, 0.2))  #default win rate to 0.2 for first time nominees

#Increase numbers to cover 1969-1971
#prev_actor_drama_noms <- list('Peter Graves' = 2, 'Robert Young' = 2, 'Mike Connors' = 2, 'Chad Everett' = 1, 'Robert Wagner' = 1)
#prev_actor_drama_wins <- list('Peter Graves' = 1, 'Mike Connors' = 1)
#prev_actor_comedy_noms <- list('Flip Wilson' = 1, 'Herschel Bernardi' = 1)
#prev_actor_comedy_wins <- list('Flip Wilson' = 1)
#prev_actress_drama_noms <- list('Peggy Lipton' = 2, 'Denise Nicholas' = 2)
#prev_actress_drama_wins <- list('Peggy Lipton' = 1)
#prev_actress_comedy_noms <- list('Carol Burnett' = 2, 'Lucille Ball' = 1, 'Shirley Jones' = 1)
#prev_actress_comedy_wins <- list('Carol Burnett' = 1, 'Mary Tyler Moore' = 1)
#prev_tv_drama_noms <- list('Marcus Welby, M.D.' = 2, 'Medical Center' = 1, 'Mod Squad' = 2, 'Mannix' = 1)
#prev_tv_drama_wins <- list('Marcus Welby, M.D.' = 1, 'Medical Center' = 1)
#prev_tv_comedy_noms <- list('The Carol Burnett Show' = 2, 'The Partridge Family' = 1)
#prev_tv_comedy_wins <- list('The Carol Burnett Show' = 1)

nominations <- nominations %>% mutate(
    prev_wins = ifelse(category=='Actor Drama' & performer %in% c('Peter Graves','Mike Connors'), prev_wins+1, prev_wins),
    prev_wins = ifelse(category=='Actor Comedy' & performer %in% c('Flip Wilson'), prev_wins+1, prev_wins),
    prev_wins = ifelse(category=='Actress Drama' & performer %in% c('Peggy Lipton'), prev_wins+1, prev_wins),
    prev_wins = ifelse(category=='Actress Comedy' & performer %in% c('Carol Burnett','Mary Tyler Moore'), prev_wins+1, prev_wins),
    prev_wins = ifelse(category=='TV Drama' & show %in% c('Marcus Welby, M.D.','Medical Center'), prev_wins+1, prev_wins),
    prev_wins = ifelse(category=='TV Comedy' & show %in% c('The Carol Burnett Show'), prev_wins+1, prev_wins),
    prev_noms = ifelse(category=='Actor Drama' & performer %in% c('Peter Graves','Robert Young','Mike Connors'), prev_noms+2, prev_noms),
    prev_noms = ifelse(category=='Actor Drama' & performer %in% c('Chad Everett','Robert Wagner'), prev_noms+1, prev_noms),
    prev_noms = ifelse(category=='Actor Comedy' & performer %in% c('Flip Wilson','Herschel Bernardi'), prev_noms+1, prev_noms),
    prev_noms = ifelse(category=='Actress Drama' & performer %in% c('Peggy Lipton','Denise Nicholas'), prev_noms+2, prev_noms),
    prev_noms = ifelse(category=='Actress Comedy' & performer %in% c('Carol Burnett'), prev_noms+2, prev_noms),
    prev_noms = ifelse(category=='Actress Comedy' & performer %in% c('Lucille Ball','Shirley Jones'), prev_noms+1, prev_noms),
    prev_noms = ifelse(category=='TV Drama' & show %in% c('Marcus Welby, M.D.','Mod Squad'), prev_noms+2, prev_noms),
    prev_noms = ifelse(category=='TV Drama' & show %in% c('Medical Center','Mannix'), prev_noms+1, prev_noms),
    prev_noms = ifelse(category=='TV Comedy' & show %in% c('The Carol Burnett Show'), prev_noms+2, prev_noms),
    prev_noms = ifelse(category=='TV Comedy' & show %in% c('The Partridge Family'), prev_noms+1, prev_noms)
)

nominations %<>% mutate(prev_losses = prev_noms - prev_wins,
                                      prev_noms_without_win = ifelse(prev_wins==0, prev_noms, 0))

nominations %<>% mutate(surname = purrr::map(performer, function(n) strsplit(n,' ')[[1]]),
                                      surname = purrr::map_chr(surname, function(n) if (length(n)==0) '' else n[length(n)]))

#Join on IMDB data for previous year and two years ago, and average whatever is available
#Don't worry about weighting by number of episodes - don't know what fraction are in the previous calendar year
joined <- nominations %>% left_join(show_data %>% select(show,season,season_start_year,av_rating,av_num_votes),
                                    by=c('show',c('year_minus1'='season_start_year'))) %>%
    left_join(show_data %>% select(show,season,season_start_year,av_rating,av_num_votes),
              by=c('show', c('year_minus2'='season_start_year'))) %>%
    #average an NA and a real value by changing the NA to take the other value, then average
    mutate(av_rating.x = ifelse(is.na(av_rating.x), av_rating.y, av_rating.x),
           av_rating.y = ifelse(is.na(av_rating.y), av_rating.x, av_rating.y),
           av_rating = 0.5*(av_rating.x + av_rating.y),
           av_num_votes.x = ifelse(is.na(av_num_votes.x), av_num_votes.y, av_num_votes.x),
           av_num_votes.y = ifelse(is.na(av_num_votes.y), av_num_votes.x, av_num_votes.y),
           av_num_votes = 0.5*(av_num_votes.x + av_num_votes.y),
           earlier_season_awarded = ifelse(is.na(season.y), season.x, season.y),
           later_season_awarded = ifelse(is.na(season.x), season.y, season.x)) %>%
    select(-av_rating.x, -av_rating.y, -av_num_votes.x, -av_num_votes.y, 
           -season.x, -season.y, -year_minus1, -year_minus2)
#later_season_awarded should be the more reliable to use

#add IMDB rating relative to other nominees
joined %<>% group_by(category,year) %>% mutate(rel_rating = av_rating - mean(av_rating,na.rm=T)) %>% ungroup()

#Add show nominated or not alongside actors
perf_nom_with_show <- joined %>% filter(cat_group=='acting') %>% select(year,performer,category,show) %>%
    left_join(joined %>% filter(cat_group=='series') %>% select(year,show) %>% mutate(check=1) %>% filter(!duplicated(.)),
              by=c('year','show')) %>%
    mutate(show_also_nominated = ifelse(!is.na(check), 1, 0)) %>% select(year,performer,category,show_also_nominated)
#table(perf_nom_with_show$show_also_nominated)  #55% show nominated along with the actor
joined <- left_join(joined, perf_nom_with_show, by=c('year','performer','category'))

#Add number of nominations per year
joined %<>% group_by(year, show) %>% mutate(total_noms_this_year=n()) %>% ungroup()
#joined %>% group_by(total_noms_this_year) %>% summarise(n=n(), frac_won=mean(won))  #10% win chance if 1 nom, ~25% for any more

#Add won last year in same category
joined <- joined %>% left_join(
        joined %>% filter(won==1) %>% select(year,category,last_year_winner=candidate) %>% mutate(year = year+1),
        by = c('category','year')) %>%
    mutate(won_category_last_year = ifelse(!is.na(last_year_winner) & last_year_winner == candidate, TRUE, FALSE))
#joined %>% group_by(won_category_last_year) %>% summarise(n=n(), frac_won=mean(won))  #22% if won last year vs 20% not

#Add 'won previous year Emmy'
emmy_winners <- read.csv('data/emmy_winners_1971to2019.csv')
emmy_winners$category[emmy_winners$category=='Show Drama'] <- 'TV Drama'
emmy_winners$category[emmy_winners$category=='Show Comedy'] <- 'TV Comedy'
joined <- joined %>% mutate(year_minus1 = year - 1) %>% 
    left_join(emmy_winners, by=c('category',c('year_minus1'='year'))) %>% 
    mutate(won_emmy_prev_year = ifelse(winner == show | winner == performer, TRUE, FALSE),
           won_emmy_prev_year = ifelse(is.na(won_emmy_prev_year), FALSE, won_emmy_prev_year)) %>% 
    select(-winner, -year_minus1) %>% data.frame()
#and for 2021 this is true for S's C, Levy, O'Hara
#Emmy winners win 27% time vs 19% for no Emmy win for acting; 38% vs 16% for shows
#joined %>% mutate(decade = round(year,-1)) %>% group_by(decade, won_emmy_prev_year) %>% summarise(n=n(), frac_won=mean(won)) %>%
#    filter(n > 20) %>%
#    ggplot() + geom_point(aes(decade, frac_won, colour=won_emmy_prev_year))
#The link has perhaps got slightly stronger in the last 20 years

#In recent decades the link with IMDB ratings has not got stronger; nothing obvious with num votes;
#  total noms was less correlated ~1985-2005., now going up.
#  won last year was correlated in 1970-85 but now -ve corr if anything
#  emmy correlation no obvious trend
#Correlation with season is stronger than with prev_noms
joined %>% mutate(decade = round(year,-1)) %>% group_by(decade) %>% 
    summarise(n=n(),
              av_rating_corr = cor(av_rating, won, use='complete.obs'),
              av_num_votes_corr = cor(av_num_votes, won, use='complete.obs'),
              total_noms_corr = cor(total_noms_this_year, won, use='complete.obs'),
              won_last_year_corr = cor(ifelse(won_category_last_year,1,0), won, use='complete.obs'),
              won_emmy_corr = cor(ifelse(won_emmy_prev_year,1,0), won, use='complete.obs'),
              season_corr = cor(later_season_awarded, won, use='complete.obs'),
              prev_noms_corr = cor(prev_noms, won, use='complete.obs')) %>% data.frame()


#Done preparing data. Note there are a few years with multiple winners: Actor Comedy 1989 has 3,
#  Actor Comedy 1978, Actress Comedy 1974,1986, Actress Drama 1982,1991, Actor Drama 1976, TV Comedy 1980 have 2

#----

#Key results
#- IMDB ratings for winning show are only a bit higher than other nominees (above average only ~60% of time)
#- IMDB ratings for acting are even less predictive
#Shows
#- Winning shows tend to have more ratings, but only pre 2010 - probably classic shows being watched
#    and rated after the award years.
#- Show win frac decreases over time, especially from s3 onwards
#- s2 win frac is doubled from 25% to 50% if s1 won the previous year
#Acting
#- 54% of acting winners have zero prev nominations, vs 41% of nominations in general that are first timers,
#    so being a first timer makes winning more likely. Some of this is because s1 wins more often,
#    but it is also seen for s2+ - having zero prev noms increases win frac from 15% to 20%.
#- The show also being nominated (happens in about half of cases) increases actor win frac from 10% to 25%
#- Win frac decreases with increasing prev noms, but this seems to be mostly due to season increasing
#- Alphabetical surname order seems to affect actor win frac - being in p1 or p5 increases
#    win prob by relative 25%, and being p3 decreases by relative 25%.
#- Name length order is much weaker effect (not significant; slightly favours longest and shortest names)

joined %>% filter(category %in% c('TV Drama','TV Comedy')) %>% 
    mutate(category = ifelse(category=='TV Comedy','TV Musical/Comedy', category)) %>%
    ggplot() + 
    geom_point(aes(year, av_rating, colour=result), alpha=0.3) +
    geom_smooth(aes(year, av_rating, colour=result, group=result), se=FALSE) +
    facet_wrap(~category) +
    scale_colour_manual(values = c('Won'='red', 'Lost'='black')) + 
    scale_x_continuous(breaks = c(1972, 1980, 1990, 2000, 2010, 2020)) +
    labs(x=NULL, y='Mean IMDb rating',
         title='IMDb rating usually does not tell us who will win',
         subtitle='Golden Globe awards 1972-2020') +
    theme_light() + 
    theme(legend.position = c(0.9,0.20), legend.box.margin = margin(0,0,0,0), 
          legend.box.background=element_rect(), legend.title = element_blank(),
          strip.background = element_rect(fill='mediumpurple'))
ggsave('plots/gglobes_showvsimdbrating_redblack.png', width=7, height=4)
#Does the highest rated show usually win? No, sometimes the lowest rated wins

joined %>% #filter(category %in% c('TV Drama','TV Comedy')) %>%
    filter(!is.na(av_num_votes)) %>%
    group_by(category, won) %>% summarise(n=n(), mean_rating=mean(av_rating)) 
#Small difference (+0.04) in rating for Comedy; winner rating higher by 0.11 for Drama
#0-0.1 differences for the actor categories

joined %>%
    filter(!is.na(av_num_votes), won==1) %>%
    ggplot() + geom_histogram(aes(rel_rating), binwidth=0.1) + 
    geom_vline(xintercept=0, linetype=2) + 
    facet_wrap(~category) +
    labs(x='Winner rating minus average of other nominee ratings', y='Count')
#Looking at each year, shows that TV Drama winner is only higher rated about half the time
#Actor Drama looks consistently higher
joined %>%
    filter(!is.na(av_num_votes), won==1) %>%
    group_by(category) %>% summarise(n=n(),
                                     frac_winner_higher = mean(rel_rating > 0, na.rm=T),
                                     mean_diff = mean(rel_rating, na.rm=T))
#winner is higher rated in 54% cases overall; 56% TV Drama, 55% TV Comedy; 54% acting
#mean diff 0.05 for series, 0.05 for acting
    
ggplot(joined %>% filter(category %in% c('TV Drama','TV Comedy'))) + 
    geom_point(aes(year, av_num_votes, colour=result), alpha=0.3) +
    geom_smooth(aes(year, av_num_votes, colour=result, group=result), method='loess', se=FALSE) +
    #facet_wrap(~category) +
    scale_colour_manual(values = c('Won'='red', 'Lost'='black')) +
    scale_y_log10()
#Does the most rated i.e. most popular show usually win? On average yes, moreso for Drama
#But the signal disappears from 2010 - so this could be classic shows getting votes after the award year

# glm(won ~ av_rating + av_num_votes, 
#     data=subset(joined, grepl('TV', category)), family='binomial') %>%
#     summary()
# #positive coefficient for rating, p=0.13
# glm(won ~ I(year >= 2000):(av_rating + av_num_votes), 
#     data=subset(joined, grepl('TV', category)), family='binomial') %>%
#     summary()
# #splitting by year before/after 2000, av_rating effect doesn't change much;
# #  av_num_votes is weakly +ve before 2000 and weakly -ve after
# #  (could be that people have gone back and watched the old winning shows more than the losing ones)
# glm(won ~ av_rating + av_num_votes + season, 
#     data=subset(joined, grepl('TV', category)), family='binomial') %>%
#     summary()
# #season is much more important - smaller is better
# 
# glm(won ~ av_rating + av_num_votes, 
#     data=subset(joined, !grepl('TV', category)), family='binomial') %>%
#     summary()
# #For acting, no significant coefs
# glm(won ~ I(year >= 2000):(av_rating + av_num_votes), 
#     data=subset(joined, !grepl('TV', category)), family='binomial') %>%
#     summary()
#Nothing for before/after 2000

#Most or least experienced in terms of previous noms
joined %>% 
    group_by(category,year) %>% summarise(min_prev_noms=min(prev_noms),
                                          max_prev_noms=max(prev_noms),
                                          winner_prev_noms=prev_noms[which(won==1)],
                                          .groups='drop_last') %>%
    summarise(n_years=n(), 
              frac_fewest_prev_noms_wins = mean(winner_prev_noms==min_prev_noms),
              frac_zero_prev_noms_wins = mean(winner_prev_noms==0),
              frac_most_prev_noms_wins = mean(winner_prev_noms==max_prev_noms))
#50-65% of time the winner has fewest or joint fewest prev noms, which is usually 0 (Actor Comedy lower at 45%)
#15-20% of time the person with most prev noms wins, which is usually 2 or more
joined %>% group_by(cat_group) %>% summarise(n=n(), frac_zero_prev_noms = mean(prev_noms==0))
#53% winners have zero prev noms, vs 39,43% of noms overall are first timers
joined %>% filter(later_season_awarded>1) %>% group_by(cat_group, first_nom=(prev_noms==0)) %>% summarise(n=n(), win_frac=mean(won))
#s2+ acting and shows are also (5-10%) more likely to win if have zero previous noms - not just a s1 effect
joined %>% 
    filter(!is.na(later_season_awarded),
           later_season_awarded >= 2) %>%
    mutate(first_nom = ifelse(prev_noms==0,'Yes','No')) %>%
    group_by(category,first_nom) %>% 
    summarise(n=n(), win_frac=mean(won), .groups='drop') %>%
    ggplot() + 
    geom_hline(yintercept=0.2, linetype=2) +
    geom_path(aes(category, win_frac, group=category), size=1, colour='grey') +
    geom_point(aes(category, win_frac, colour=first_nom), size=4) +
    ylim(0,0.3) +
    #facet_wrap(~season, scales='free') +
    #expand_limits(y=0) +
    coord_flip() +
    labs(y='Win fraction', x=NULL, colour='First nomination?', title='Nominations for season two onwards') +
    theme_light()
#Combine no previous noms and s1 winning, for s2 onwards
joined %>% filter(!is.na(later_season_awarded), later_season_awarded >= 2) %>%
    mutate(any_prev_noms_grp = ifelse(prev_noms==0, 'No',
                                      ifelse(won_category_last_year, 'Yes; won last year', 'Yes; didn\'t win last year')),
           `Award type` = ifelse(cat_group=='acting', 'Acting', 'Series')) %>%
    group_by(`Award type`, any_prev_noms_grp) %>% 
    summarise(n=n(), win_frac=mean(won==1), .groups='drop') %>%
    filter(n>50) %>% select(-n) %>% 
    pivot_wider(names_from = 'any_prev_noms_grp', values_from='win_frac') %>%
    select(`Award type`,`Yes; didn't win last year`,No,`Yes; won last year`) %>%
    gt() %>%
    data_color(
        columns = vars(`Yes; didn't win last year`, No, `Yes; won last year`), 
        colors = scales::col_numeric(palette = c('grey','white','red'), domain = c(0.08,0.32)) 
    ) %>%
    fmt_percent(
        columns = vars(`Yes; didn't win last year`,No,`Yes; won last year`), decimals=0
    ) %>%
    cols_width(
        1 ~ px(110),
        2:4 ~ px(100)
    ) %>%
    tab_spanner(label = 'Any previous nominations?', columns=vars(`Yes; didn't win last year`,No,`Yes; won last year`)) %>%
    tab_header(title = 'Win percentages for nominations for seasons two and above',
               subtitle = 'Golden Globe awards 1972-2020') %>%
    tab_options(
        heading.align = "left",
        heading.title.font.size = 15,
        column_labels.font.size = 15)
#(save screenshot)
#Overall, if first nom, 0.19; if not, 0.22 if won last year, vs 0.13 if didn't.
#For series, didn't win last year/first nom/won last year is win frac 0.1/0.2/0.3,
#  but for acting it is 0.14/0.19/0.19, i.e. winning last year is not better than first nom.
joined %>% 
    filter(!is.na(later_season_awarded), later_season_awarded >= 2) %>%
    mutate(any_prev_noms_grp = ifelse(prev_noms==0, 'No',
                                      ifelse(won_category_last_year, 'Yes; won last year', 'Yes; didn\'t win last year'))) %>%
    group_by(category, any_prev_noms_grp) %>% 
    summarise(n=n(), win_frac=mean(won), .groups='drop') %>%
    ggplot() + 
    geom_hline(yintercept=0.2, linetype=2) +
    geom_path(aes(category, win_frac, group=category), size=1, colour='grey') +
    geom_point(aes(category, win_frac, colour=any_prev_noms_grp), size=4) +
    ylim(0,0.3) +
    #facet_wrap(~season, scales='free') +
    #expand_limits(y=0) +
    coord_flip() +
    labs(y='Win fraction', x=NULL, colour='Any previous nomination?', title='Nominations for season two onwards') +
    theme_light()

#Show being nominated, for actor winning
joined %>% filter(cat_group=='acting') %>% 
    mutate(show_also_nominated = ifelse(show_also_nominated==1,'Yes','No')) %>%
    group_by(category,show_also_nominated) %>% summarise(n=n(), win_frac=mean(won), .groups='drop') %>%
    ggplot() + 
    geom_path(aes(category, win_frac, group=category), size=0.8, colour='grey') +
    geom_point(aes(category, win_frac, colour=show_also_nominated), size=4) +
    expand_limits(y=0) +
    coord_flip() +
    labs(y='Win fraction', x=NULL, colour='Show also nominated?') +
    theme_light()
#much more likely to win if show was also nominated - 25% vs 10%; applies to all acting categories
#Each category is roughly 50-50 for show_also_nominated so don't need to plot size
#Alternative plot - tried dotplot but seems to be a bug in the yaxis range; instead use waffle (installed 1.0.1 from github rather than 0.7.0 from CRAN)
joined %>% filter(cat_group=='acting') %>%
    mutate(show_also_nominated = ifelse(show_also_nominated==1,'Yes','No')) %>%
    count(category, show_also_nominated, result) %>%
    ggplot(aes(fill = result, values = n/5)) +
    geom_waffle(n_rows = 5, size = 0.33, colour = "white", flip = FALSE) + 
    facet_grid(category~show_also_nominated) +
    scale_fill_discrete(name='Acting nom.') +
    scale_x_discrete() +  #turns off the x axis values
    scale_y_continuous(expand=c(0,0),  #reduces padding between plots a bit
                       labels = function(x) x*25) + #make this the same as n_rows * (any divisor on values=n/?)
    coord_equal() + #need this to make the units squares
    labs(subtitle = 'Each square = 5 cases', x='Show also nominated?') +
    ggthemes::theme_tufte(base_size = 11)
#Or change the facet plot to make it like a stacked bar plot (nrow=1 in facet, strip at bottom)
#Probably USE THIS ONE
joined %>% filter(cat_group=='acting') %>%
    mutate(show_also_nominated = ifelse(show_also_nominated==1,'Yes','No')) %>%
    count(show_also_nominated, result) %>% 
    mutate(result = ifelse(result=='Won', '5 wins', '5 losses')) %>%
    ggplot(aes(fill = result, values = n/5)) +
    geom_waffle(n_rows = 10, size = 0.33, colour = "white", flip = TRUE) + 
    facet_wrap(~show_also_nominated, nrow=1, strip.position = 'bottom') +
    scale_x_discrete() + 
    scale_y_continuous(labels = function(x) x*50,  #make this the same as n_rows * (any divisor on values=n/?)
                       expand = c(0,0)) +
    ggthemes::scale_fill_tableau(name='Acting nom.') +
    coord_equal() + 
    labs(x='Show also nominated?', y='Count', 
         title='Awards for acting are more likely when the show is also nominated',
         subtitle='Golden Globe lead actor/actress awards, 1972-2020') +
    theme_minimal() +
    theme(panel.grid = element_blank(), axis.ticks.y = element_line(), #marks the y ticks but no axis line
          plot.title = element_text(hjust=0, size=12),
          plot.subtitle = element_text(hjust=0, size=9)) +
    guides(fill = guide_legend(reverse=TRUE))
ggsave('plots/gglobes_showalsonominated_waffleplot.png', width=7, height=4)
#Or use iron method - would need two columns though
#need to specify colours because it fills in the last row with squares, need to make them white by not specifying a colour
iron(
    waffle(c('Lost'=104,'Won'=16), title='Actor Comedy show not nominated', size=0.5),
    waffle(c('Lost'=108,'Won'=6), title='Actress Comedy show not nominated', size=0.5),
    waffle(c('Lost'=88,'Won'=9), title='Actor Drama show not nominated', size=0.5),
    waffle(c('Lost'=104,'Won'=16), title='Actress Drama show not nominated', size=0.5)
)

#----
#Overall win frac is ~20%

#i) win frac by season vs nom frac estimated
joined %>% filter(cat_group == 'series') %>%
    group_by(earlier_season_awarded) %>% summarise(n_noms=n(), win_frac=mean(won))
#29% win for s1, 21% s2, then down to 8%; low numbers for s7+
joined %>% filter(cat_group == 'series') %>%
    group_by(earlier_season_awarded) %>% summarise(n_noms=n(), win_frac=mean(won)) %>% 
    filter(n_noms >= 8) %>%
    ggplot() + 
    geom_hline(yintercept=0.2, linetype=2) +
    geom_col(aes(earlier_season_awarded, win_frac), width=0.04) +
    geom_point(aes(earlier_season_awarded, win_frac, size=n_noms)) +
    scale_x_continuous(breaks=seq(1,9))
#Drama peaks at s2, Comedy s1

#Shouldn't have any s1 with prev_wins>=1: have several including Kominsky 2020, Maisel 2019, Crown 2018:
#  some of these should be using just year-1 because the season came out in full in late year-1.
#  Others got awards midway through s1 airing.
#Have to exclude as it will look too strange.
joined %>% filter(cat_group == 'series') %>%
    #filter(!(prev_wins>=1 & earlier_season_awarded==1)) %>%
    group_by(later_season_awarded, 
             `Has Won Previously` = ifelse(prev_wins>=1,'Yes','No')) %>% 
    summarise(n_noms=n(), win_frac=mean(won)) %>% 
    filter(n_noms >= 8) %>%
    ggplot() + 
    geom_hline(yintercept=0.2, linetype=2) +
    geom_col(aes(later_season_awarded, win_frac), width=0.04) +
    geom_point(aes(later_season_awarded, win_frac, size=n_noms)) +
    facet_wrap(~`Has Won Previously`, labeller = label_both) +
    scale_x_continuous(breaks=seq(1,9)) + ylim(0,0.5)
#If s1 wins, the next year has a nearly 50% chance of winning - many of these are awarding first midway through s1
#If s1 didn't win, s2 win frac is only 25%
#Win frac declines over time regardless of s1 winning

#Nomination dist by season 
joined %>% filter(later_season_awarded<=10) %>% 
    ggplot() + stat_count(aes(later_season_awarded, group=result, fill=result), position='stack') +
    facet_wrap(~cat_group) +
    scale_x_continuous(breaks=seq(1,10)) +
    labs(x='Season', y='Number of nominations', fill=NULL, title='Nominations and wins are most likely for Season One') +
    theme_bw()
#Win frac notably higher for acting in s1; otherwise both noms and wins decline with increasing season
#Combine with overall show season mix - use all nominated shows that have finished
#  to get mix of seasons we can expect from 'quality' shows (excluded those never nominated)
show_data %>% group_by(show) %>% summarise(show_start_year = min(season_start_year),
                                           show_length = max(season)) %>% 
    filter(show_start_year <= 2010) %>%
    ggplot() + stat_count(aes(show_length))
#Get a peak at one season but then 4-7 seasons is more common
#The one seasoners are mostly older, e.g. From Here to Eternity (1979), Hooperman (1987), My So-Called Life (1994)
#NB, this method shows length in terms of seasons with rated episodes; underestimates e.g. Hooperman (3),
#  Young IJ Chronicles (2), New DVD show (3)
#TODO get show lengths properly

#Given these lengths, what mix of seasons do we expect in a given year? s1 is present in all shows;
#  s2 for those with length >=2, etc.
expected_seasons <- show_data %>% group_by(show) %>% summarise(show_start_year = min(season_start_year),
                                           show_length = max(season), .groups='drop') %>% 
    filter(show_start_year <= 2010) %>%
    summarise(num_each_season = purrr::map(1:10, function(i) sum(show_length >= i))) %>% unlist() %>% as.numeric() %>%
    data.frame(season = seq(10), num_cases = .) %>% mutate(frac_expected = num_cases/sum(num_cases))
#expected_seasons <- rbind(
#    expected_seasons %>% mutate(cat_group='acting') %>% mutate(num_expected=frac_expected*sum(joined$cat_group=='acting')),
#    expected_seasons %>% mutate(cat_group='series') %>% mutate(num_expected=frac_expected*sum(joined$cat_group=='series'))
#)
expected_seasons %>%
    ggplot() + geom_point(aes(season, num_cases)) + scale_x_continuous(breaks=seq(1,10))# + #ylim(0,0.2) +
    #facet_wrap(~cat_group)
#Expect 19% s1s, 16% s2s, to 2% s10s, in nominations
#In noms we see 24% s1, 18% s2, to 1% s10

#Plot stacked bar with two lines overlaid, showing expect num noms and num wins
joined %>% filter(later_season_awarded<=10) %>% 
    ggplot() + stat_count(aes(later_season_awarded, group=result, fill=result), position='stack') +
    geom_line(data = expected_seasons, aes(x=season, y=num_cases), size=1) + 
    geom_line(data = expected_seasons, aes(x=season, y=num_cases*0.2), size=1, colour='yellow') + 
    #facet_wrap(~cat_group) +
    scale_x_continuous(breaks=seq(1,10)) +
    scale_fill_manual(values=c('Lost'='darkgrey','Won'='red2')) +
    labs(x='Season', y='Number of nominations', fill=NULL, 
         title='Season ones are over-represented in nominations and wins',
         subtitle='Golden Globe TV series and lead actor/actress awards, 1972-2020\nBlack, yellow lines show expected distribution of seasons for nominations, wins') +
    theme_light() +
    theme(legend.title=element_blank(), legend.position=c(0.9,0.6),
          legend.box.background = element_rect(),
          plot.subtitle=element_text(size=8))
ggsave('plots/gglobes_seasonnominations_vs_expected_redgreyyellow.png', width=7, height=4)

#ii) win frac by prev nominations 
joined %>% filter(cat_group == 'acting') %>% 
    group_by(prev_noms) %>% summarise(n_cases=n(), win_frac=mean(won)) %>%
    filter(n_cases >= 8) %>%
    ggplot() + 
    geom_col(aes(prev_noms, win_frac), width=0.04) +
    geom_point(aes(prev_noms, win_frac, size=n_cases)) +
    geom_hline(yintercept = c(0.12,0.30), linetype=2) + 
    geom_hline(yintercept = 0.2, linetype=1) +
    scale_x_continuous(breaks=seq(0,7)) + 
    theme_bw()
#TODO calculate 90% lims properly
#Win frac is higher for 0 prev noms but not anomalously so. win frac drops for 2-4 prev noms
#  and is below 10% line for 4 prev noms. prev_noms=0 will be linked with season being 1.
#The prev_noms=6-7 cases mostly have 2-3 wins already -> serial winners
joined %>% filter(cat_group == 'acting') %>% 
    group_by(prev_noms, later_season_awarded) %>% summarise(n_cases=n(), win_frac=mean(won)) %>%
    filter(n_cases >= 5, later_season_awarded >= 3, later_season_awarded <= 6) %>%
    ggplot() + 
    geom_col(aes(prev_noms, win_frac), width=0.04) +
    geom_point(aes(prev_noms, win_frac, size=n_cases)) +
    facet_wrap(~later_season_awarded) +
    theme_bw()
#But separating by season, pattern is less clear -> prev nom pattern mostly driven by season
#Not much pattern for prev_wins either -> s3-6 win doesn't depend on number of prev noms or wins?
#Maybe an increased win frac for s5-6 with zero prev wins

#iii) win frac by previous wins
joined %>% filter(cat_group == 'acting') %>% 
    group_by(prev_wins) %>% summarise(n_cases=n(), win_frac=mean(won), .groups='drop') %>%
    filter(n_cases >= 10) %>%
    ggplot() + 
    geom_col(aes(prev_wins, win_frac), width=0.02) +
    geom_point(aes(prev_wins, win_frac, size=n_cases)) +
    geom_hline(yintercept = c(0.12,0.30), linetype=2) + 
    geom_hline(yintercept = 0.2, linetype=1) +
    scale_x_continuous(breaks=seq(0,7)) + 
    theme_bw()
#chance of winning drops after winning the first time, but then rises for 2-3 prev wins
#  suggesting serial winners (though low for 4 prev wins, 1 out of 8)
#The drop from 0 to 1 is not just a s1 effect - seen if I limit to s2+ too.
joined %>% 
    mutate(prev_wins_grp = ifelse(prev_wins >= 2, '2+', as.character(prev_wins))) %>%
    group_by(cat_group, prev_wins_grp) %>% summarise(n_cases=n(), win_frac=mean(won), .groups='drop') %>%
    filter(n_cases >= 10) %>%
    mutate(conf95=qbinom(0.95,n_cases,0.2)/n_cases, conf05=qbinom(0.05,n_cases,0.2)/n_cases) %>%
    ggplot() + 
    #geom_hline(yintercept = c(0.12,0.30), linetype=3, size=0.6) + 
    #geom_hline(yintercept = 0.2, linetype=2, size=1.0) +
    geom_linerange(aes(prev_wins_grp,ymin=conf05,ymax=conf95,colour=cat_group), alpha=0.3, size=2, position=position_dodge(width=0.5)) +
    geom_point(aes(prev_wins_grp,win_frac,colour=cat_group,size=n_cases), position=position_dodge(width=0.5)) +
    #geom_col(aes(prev_wins_grp,win_frac,colour=cat_group), width=0.02, position=position_dodge(width=0.5)) +
    annotate('text', x=3.6, y=0.26, label='Acting categories', hjust=-0.2, colour='seagreen', size=3.5) +
    annotate('text', x=3.6, y=0.24, label='Series categories', hjust=-0.2, colour='navy', size=3.5) +
    coord_cartesian(clip='off') +
    expand_limits(y=0) +
    scale_colour_manual(values = c('acting'='seagreen', 'series'='navy')) +
    guides(colour='none') +
    labs(x='Number of previous wins', y='Fraction of nominations won', size='Number of cases',
         title='It is more difficult than you\'d think to win a second acting award',
         subtitle='Golden Globe awards 1972-2020; bars show 5-95% win fraction likelihood ranges') +
    theme_minimal() +
    theme(plot.subtitle = element_text(size=8), legend.position='right',
          panel.grid.major.x = element_blank())
#Confidence bounds here are different for each x value because there are 
#  different numbers of cases involved. For each n_cases the 5-95% range
#  is qbinom(c(0.05,0.95),n_cases,0.2)/n_cases
ggsave('plots/gglobes_wins_vs_prevwins_wconflines.png', width=6, height=4)
#

#iv) win frac by previous losing noms
joined %>% filter(cat_group == 'acting') %>% 
    group_by(prev_losses) %>% summarise(n_cases=n(), win_frac=mean(won)) %>%
    filter(n_cases >= 5) %>%
    ggplot() + 
    geom_col(aes(prev_losses, win_frac), width=0.02) +
    geom_point(aes(prev_losses, win_frac, size=n_cases)) +
    geom_hline(yintercept = c(0.12,0.30), linetype=2) + 
    geom_hline(yintercept = 0.2, linetype=1) +
    scale_x_continuous(breaks=seq(0,7)) + 
    theme_bw()
#chance of winning declines with 2-3 previous losses, but then increases for 4-5
#  previous losses, suggesting compensatory voting (though have a few 6-7 prev losses with 0% win)
#BUT some of these have already won as well as lost
joined %>% filter(cat_group == 'acting') %>% 
    group_by(prev_losses, has_won=prev_wins>0) %>% summarise(n_cases=n(), win_frac=mean(won)) %>%
    filter(n_cases >= 7) %>%
    ggplot() + 
    #geom_col(aes(prev_losses, win_frac), width=0.02) +
    geom_point(aes(prev_losses, win_frac, size=n_cases)) +
    geom_hline(yintercept = c(0.12,0.30), linetype=2) + 
    geom_hline(yintercept = 0.2, linetype=1) +
    facet_wrap(~has_won, labeller=label_both) +
    scale_x_continuous(breaks=seq(0,7)) + 
    theme_bw()
#win rates are low for those with 1 win and 0-3 losses but increase at 1 win and 4-5 losses
#for those without a win, win frac does NOT increase with more losses
#This suggests a pattern of people winning early and then again later: applies to Hamm, Burnett (x2),
#  Fox (x3), Danson, Gless, Lansbury, Grammer, Alda (x4)
#  e.g. Hamm won s1, lost s2-5, won s6; Burnett won s5 (and once before), lost s6-9, won s10-11;
#    Danson lost 4 times then won twice; Gless won 1/4 for one show then 1/2 for another

#v) win frac by alphabetical name
#TODO put pos5 into pos4 when there are 6 candidates

#joined %>% filter(grepl('TV', category)) %>%
#    count(year, category) %>% count(category, n)
#1 in 5 years have 6 nominees -> combine positions 5 and 6 below
joined %>% filter(cat_group == 'series') %>%
    arrange(year, category, show) %>% 
    group_by(category, year) %>% summarise(winner_alph_pos = min(which(won==1),5)) %>%
    group_by(category) %>% count(winner_alph_pos) %>%
    ggplot() + geom_col(aes(winner_alph_pos, n)) + facet_wrap(~category)
#Doesn't look like there is a signal in shows

# joined %>% filter(cat_group == 'series') %>%
#     arrange(year, category, show) %>% 
#     group_by(year, category) %>% mutate(alph_pos = pmin(rank(show),5)) %>% ungroup() %>%
#     glm(won ~ factor(alph_pos), data=., family='binomial') %>%
#     summary()
#position 1 is in intercept, then pos 2 is -ve, and others weakly -ve -> some leaning to pos 1

joined %>% filter(cat_group == 'acting') %>%
    arrange(year, category, performer) %>% 
    group_by(category, year) %>% summarise(winner_alph_pos = min(which(won==1),5)) %>%
    group_by(category) %>% count(winner_alph_pos) %>%
    ggplot() + geom_col(aes(winner_alph_pos, n)) + facet_wrap(~category)
#Going by performer FIRST name, not a clear trend
joined %>% filter(cat_group == 'acting') %>%
    arrange(year, category, surname) %>% 
    group_by(category, year) %>% summarise(winner_alph_pos = min(which(won==1),5)) %>%
    group_by(category) %>% count(winner_alph_pos) %>%
    ggplot() + geom_col(aes(winner_alph_pos, n)) + facet_wrap(~category)
#Going by performer LAST name, there is more preference for pos 1 and 5, in Actors anyway
#qbinom(c(0.1,0.9),196,0.2) #From 196 trials with p=0.2, 10-90% conf range is 32-46 wins, or 0.163 to 0.234; 95% range is 30-49
joined %>% filter(cat_group == 'acting') %>%
    arrange(year, category, surname) %>% 
    group_by(category, year) %>% summarise(winner_alph_pos = min(which(won==1),5), .groups='drop') %>%
    count(winner_alph_pos) %>% mutate(frac = n/sum(n)) %>%
    ggplot() + 
    geom_col(aes(winner_alph_pos, n), fill='skyblue', width=0.7) +
    geom_hline(yintercept=0.2*196, linetype=2, size=1.2) +
    geom_hline(yintercept=30, linetype=3, size=0.8) +
    geom_hline(yintercept=49, linetype=3, size=0.8) +
    labs(x='Winner surname alphabetical position', y='Number of wins', 
         title='Names of winners in acting categories tend to come first or last alphabetically',
         subtitle='Golden Globe lead actor/actress awards, 1972-2020; lines show expected wins and 5-95% likelihood range') +
    theme_minimal() + 
    theme(plot.subtitle = element_text(size=8), plot.title = element_text(size=11))
ggsave('plots/gglobes_wins_vs_alphabeticalposition_blue_wconflines.png', width=6, height=4)

joined %>% filter(cat_group == 'acting') %>%
    arrange(year, category, surname) %>% 
    group_by(year, category) %>% mutate(alph_pos = pmin(rank(surname),5)) %>% ungroup() %>%
    glm(won ~ factor(alph_pos), data=., family='binomial') %>%
    summary()
#pos 1 is in intercept, -ve coefs for 2,4, especially 3, and not signif for 5.
#
#The overall effect may be dominated by Actor Comedy pos 1s: 6 Aldas, 3 Baldwins, 3 Foxs, 8 others
#Alda was always first on list so we can't tell if he won on merit or was helped by alphabetical order
joined %>% filter(cat_group == 'acting') %>%
    group_by(year,category) %>% filter(!any(performer=='Alan Alda')) %>% ungroup() %>%
    arrange(year, category, surname) %>% 
    group_by(year, category) %>% mutate(alph_pos = pmin(rank(surname),5)) %>% ungroup() %>%
    glm(won ~ factor(alph_pos), data=., family='binomial') %>%
    summary()
#If we exclude Alda, significance weakens by pos 3 is still significantly -ve
joined %>% filter(cat_group == 'acting') %>%
    arrange(year, category, surname) %>% 
    group_by(category, year) %>% summarise(winner_alph_pos = min(which(won==1),5)) %>%
    count(winner_alph_pos) %>%
    ggplot() + geom_col(aes(winner_alph_pos, n), width=0.7, fill='skyblue3') +
    geom_hline(yintercept = joined %>% filter(cat_group == 'acting') %>% 
                   summarise(exp_wins = (n_distinct(year) * 4)/5) %>% pull(exp_wins),
               linetype=1) +
    theme_bw()
#There does seem to be an effect because it varies smoothly from p1 to p5. p1 and p5 are over-represented
#  by ~10 noms each compared to expected 40, so being p1/5 increases win frac by *1.25; 
#  p3 under-rep by more than 10, so being p3 affects win frac by *0.75.

#vi) win frac by length of name rank (shortest name in pos 1; show length to break ties)
#**Original pos 1 favoured result MAY HAVE BEEN A BUG, if ranking favours pos1 in case of a tie in name length**

joined %>% filter(cat_group == 'acting') %>%
    mutate(performer_name_length = purrr::map_int(performer, nchar),
           show_name_length = purrr::map_int(show, nchar)) %>%
    arrange(year, category, performer_name_length, show_name_length) %>% 
    group_by(category, year) %>% summarise(winner_nchar_pos = min(which(won==1),5)) %>%
    group_by(category) %>% count(winner_nchar_pos) %>%
    ggplot() + geom_col(aes(winner_nchar_pos, n)) + facet_wrap(~category)
#no signal for pos1 now
joined %>% filter(cat_group == 'acting') %>%
    mutate(performer_name_length = purrr::map_int(performer, nchar),
           show_name_length = purrr::map_int(show, nchar)) %>%
    arrange(year, category, performer_name_length, show_name_length) %>% 
    group_by(year, category) %>% summarise(winner_nchar_pos = min(which(won==1),5), .groups='drop') %>%
    count(winner_nchar_pos) %>%
    ggplot() + geom_col(aes(winner_nchar_pos, n))
#overall shortest and longest names are top - perhaps these are the most memorable?
joined %>% filter(cat_group == 'acting') %>%
    mutate(performer_name_length = purrr::map_int(performer, nchar),
           show_name_length = purrr::map_int(show, nchar)) %>%
    arrange(year, category, performer_name_length, show_name_length) %>% 
    group_by(year, category) %>% mutate(nchar_pos_1or5 = pmin(rank(performer_name_length),5) %in% c(1,5)) %>% ungroup() %>%
    glm(won ~ later_season_awarded + nchar_pos_1or5, data=., family='binomial') %>% summary()
#Being name length 1 or 5 is not a significant coef

#vii) Model some of that for performers and shows
#Do properly by converting into wide and using multinomial model
#Filter to cases with 5 noms, 1 winner
# traintest_df_wide <- joined %>% 
#     sample_frac(1) %>% arrange(year, category) %>%  #jumble order within year-category so that nom_slot=1 doesn't always win
#     mutate(candidate = ifelse(cat_group=='acting', performer, show)) %>%
#     group_by(category,year) %>% mutate(nom_slot = seq(n())) %>% 
#     filter(n()==5, sum(won)==1) %>% ungroup() %>%
#     select(category, year, nom_slot, candidate, later_season_awarded, 
#            prev_noms, prev_wins, av_rating, av_num_votes, rel_rating, show_also_nominated, won) %>% 
#     pivot_wider(names_from = nom_slot, 
#                 values_from = c(candidate, later_season_awarded, prev_noms, prev_wins,
#                                 av_rating, av_num_votes, rel_rating, show_also_nominated, won)) %>%
#     mutate(won = ifelse(won_1==1, '1',
#                         ifelse(won_2==1, '2',
#                                ifelse(won_3==1, '3',
#                                       ifelse(won_4==1, '4', '5'))))) %>% 
#     select(-won_1,-won_2,-won_3,-won_4,-won_5) %>%
#     data.frame()
# multinom(won ~ -1 + later_season_awarded_1 + later_season_awarded_2 + later_season_awarded_3 +
#              later_season_awarded_4 + later_season_awarded_5, data=traintest_df_wide)

#Maybe better to do row-wise but then normalise the predictions within each year-category
winners_ref_df <- joined %>% 
    group_by(year, category) %>% filter(sum(won)==1) %>% ungroup() %>%
    filter(won==1) %>% select(category, year, winner=candidate, won)
joined_singlewinnersonly <- joined %>% group_by(year, category) %>% filter(sum(won)==1) %>% ungroup() %>% 
    mutate(candidate = ifelse(cat_group=='series', show, performer)) %>%
    mutate(candidate_name_length = purrr::map_int(candidate, nchar)) %>%
    group_by(year, category) %>% 
    mutate(alph_pos = pmin(rank(surname, ties.method='first'),5),
           alph_pos_1or5 = alph_pos %in% c(1,5),
           name_length_pos = pmin(rank(candidate_name_length, ties.method='first'),5),
           name_length_pos_234 = name_length_pos %in% c(2,3,4),
           name_length_pos_1or5 = name_length_pos %in% c(1,5)) %>% ungroup() %>%
    mutate(av_rating = ifelse(is.na(av_rating), 7.9, av_rating),
           av_num_votes = ifelse(is.na(av_num_votes), 0, av_num_votes)) %>%
    filter(!is.na(later_season_awarded)) %>%
    data.frame()

model_series <- glm(won ~ later_season_awarded + prev_wins + I(prev_wins==1) + prev_noms + 
                      av_rating + #av_num_votes + 
                      won_emmy_prev_year + 
                      won_category_last_year + 
                      total_noms_this_year,
                  data = subset(joined_singlewinnersonly, cat_group=='series'), 
                  family='binomial')
summary(model_series)
#-ve for prev_noms (not season), +ve emmy, won last year, and total noms
#For 1995 onwards, similar structure 
rmse(subset(joined_singlewinnersonly, cat_group=='series')$won,
     predict(model_series, subset(joined_singlewinnersonly, cat_group=='series'), type='response'))
#rmse=0.36

#Do train/test - can't easily do accuracy because would have to split taking all 5 from a year-category
test_rmses <- c()
for (i in seq(10)) {
    train_ids <- createDataPartition(subset(joined_singlewinnersonly, cat_group=='series')$won, 
                                      p = 0.75, list = FALSE)
    tmp_series_cv <- glm(won ~ later_season_awarded + prev_wins + I(prev_wins==1) + prev_noms + 
                          av_rating + #av_num_votes + 
                          won_emmy_prev_year + 
                          won_category_last_year + 
                          total_noms_this_year,
                      data = subset(joined_singlewinnersonly, cat_group=='series')[train_ids,], 
                      family='binomial')
    tmp_rmse <- rmse(subset(joined_singlewinnersonly, cat_group=='series')[-train_ids,]$won,
                     predict(tmp_series_cv, subset(joined_singlewinnersonly, cat_group=='series')[-train_ids,], type='response'))
    test_rmses <- c(test_rmses, tmp_rmse)
}
mean(test_rmses)  #0.37 - similar to train

model_acting <- glm(won ~ later_season_awarded + prev_wins + I(prev_wins==1) + prev_noms + 
                      #prev_win_rate +
                      alph_pos_1or5 + 
                      #name_length_pos_1or5 + 
                      #av_rating +
                      show_also_nominated + 
                      total_noms_this_year +
                      won_emmy_prev_year,
                      #won_category_last_year,
                  data = subset(joined_singlewinnersonly, cat_group=='acting'), 
                  family='binomial')
summary(model_acting)
#-ve for season (not prev noms), -ve for prev wins = 1, -ve for alph234, length234, +ve for show also nominated, -ve for total noms, +ve for emmy
#Show also nominated is more important than total noms, but both contribute (+/- here would mean it is good for show to be nominated, but not other actors)
#For 1995 onwards, very similar structure
rmse(subset(joined_singlewinnersonly, cat_group=='acting')$won,
     predict(model_acting, subset(joined_singlewinnersonly, cat_group=='acting'), type='response'))
#rmse=0.37

test_rmses <- c()
for (i in seq(10)) {
    train_ids <- createDataPartition(subset(joined_singlewinnersonly, cat_group=='series')$won, 
                                     p = 0.75, list = FALSE)
    tmp_acting_cv <- glm(won ~ later_season_awarded + prev_wins + I(prev_wins==1) + prev_noms + 
                          alph_pos_1or5 + 
                          #name_length_pos_1or5 + 
                          show_also_nominated + 
                          total_noms_this_year +
                          won_emmy_prev_year,
                      data = subset(joined_singlewinnersonly, cat_group=='acting')[train_ids,], 
                      family='binomial')
    tmp_rmse <- rmse(subset(joined_singlewinnersonly, cat_group=='acting')[-train_ids,]$won,
                     predict(tmp_acting_cv, subset(joined_singlewinnersonly, cat_group=='acting')[-train_ids,], type='response'))
    test_rmses <- c(test_rmses, tmp_rmse)
}
mean(test_rmses)  #0.38 - similar to train

joined_singlewinnersonly$pred_acting <- predict(model_acting, joined_singlewinnersonly, type='response')
joined_singlewinnersonly$pred_series <- predict(model_series, joined_singlewinnersonly, type='response')
joined_singlewinnersonly$pred <- ifelse(joined_singlewinnersonly$cat_group=='acting', 
                                        joined_singlewinnersonly$pred_acting, 
                                        joined_singlewinnersonly$pred_series)
joined_singlewinnersonly <- joined_singlewinnersonly %>% group_by(year,category) %>% 
    mutate(pred = ifelse(is.na(pred), 0.2, pred),
           pred = pred/sum(pred)) %>% ungroup()
#Could take top pred each time and get accuracy
winners_ref_df %>% left_join(
    joined_singlewinnersonly %>% group_by(year,category) %>% slice_max(order_by=pred, n=1, with_ties=FALSE) %>%
        select(year, category, pred_winner=candidate, pred),
    by=c('year','category')
) %>% group_by(category) %>% summarise(n_years = n(), mean_pred = mean(pred), accuracy = mean(winner==pred_winner))
#Training acc this way (separate models, acting including show_also_nominated) is ~0.48.
#Based on the rmse comparisons above, test acc may be only slightly worse, say 0.45, which is Brier=0.20.

tidy(model_series) %>% select(term, series_coef=estimate, series_pval=p.value) %>% 
    full_join(tidy(model_acting) %>% select(term, acting_coef=estimate, acting_pval=p.value),
              by='term') %>%
    filter(term != '(Intercept)') %>%
    mutate(series_coef = ifelse(term=='av_num_votes', series_coef*1000, series_coef),
           acting_coef = ifelse(term=='av_num_votes', acting_coef*1000, acting_coef),
           term = ifelse(term=='av_num_votes', 'IMDB_votes_000s', term),
           term = ifelse(term=='I(prev_wins == 1)TRUE','has_one_previous_win',term),
           term = ifelse(term=='later_season_awarded','per_season',term),
           term = ifelse(term=='won_emmy_prev_yearTRUE','won_Emmy_previous_year',term),
           term = ifelse(term=='won_category_last_yearTRUE','won_GG_previous_year',term),
           term = ifelse(term=='alph_pos_1or5TRUE','first_or_last_name_alphabetically',term),
           term = ifelse(term=='name_length_pos_1or5TRUE','longest_or_shortest_name',term),
           term = ifelse(term=='prev_wins','per_number_of_previous_wins',term),
           term = ifelse(term=='prev_noms','per_number_of_previous_noms',term),
           term = ifelse(term=='av_rating','per_average_IMDB_rating',term)
           ) %>%
    mutate(series_rel_effect = (1/(1+exp(-(-1.38+series_coef)))) - (1/(1+exp(1.38))),
           acting_rel_effect = (1/(1+exp(-(-1.38+acting_coef)))) - (1/(1+exp(1.38)))) %>%
    select(term, series_rel_effect, acting_rel_effect) %>%
    gt::gt() %>%
    fmt_number(
        columns = vars(series_rel_effect, acting_rel_effect), decimals=1
    ) %>%
    fmt(
        columns = vars(series_rel_effect, acting_rel_effect), fns = function(x) ifelse(abs(x)>=0.01, sprintf('%+.3f',x), '~')
    ) %>%
    data_color(
        columns = vars(series_rel_effect, acting_rel_effect),   #this applies to the numeric values in the data frame
        colors = scales::col_numeric(palette = c('red3','white','green'), domain = c(-0.5,+0.5)) 
    )

# joined %>% filter(cat_group == 'acting') %>%
#     arrange(year, category, surname) %>% 
#     group_by(year, category) %>% mutate(alph_pos = pmin(rank(surname),5),
#                                         alph_pos_234 = alph_pos %in% c(2,3,4)) %>% ungroup() %>%
#     #mutate(performer_name_length = purrr::map_int(performer, nchar),
#     #       show_name_length = purrr::map_int(show, nchar)) %>%
#     #arrange(year, category, performer_name_length, show_name_length) %>% 
#     #group_by(year, category) %>% mutate(nchar_pos_1or5 = pmin(rank(performer_name_length),5) %in% c(1,5)) %>% ungroup() %>%
#     glm(won ~ prev_noms + I(prev_wins==1) + alph_pos_234 + later_season_awarded,
#         data=., family='binomial') %>%
#     summary()
# #prev_noms+prev_wins+alph_pos_234 gives resid 1003.2; -ve for prev_noms, -ve for pos 234
# #prev_noms + I(prev_wins==1) + alph_pos_234 gives resid 991.77; -ve for prev_wins==1 and pos 234
# #prev_noms + I(prev_wins==1) + alph_pos_234 + season may be slightly better (different null deviance so can't compare);
# #  -ve for season but it is not too dominant compared to the others
# 
# #But really should be grouping by category-year and predicting the winner class, e.g. with nnet::multinom
# #TODO https://www.r-bloggers.com/2020/05/multinomial-logistic-regression-with-r/
# 
# joined %>% filter(cat_group == 'series') %>%
#     arrange(year, category, surname) %>%
#     group_by(year, category) %>% mutate(alph_pos = pmin(rank(surname),5),
#                                         alph_pos_234 = alph_pos %in% c(2,3,4)) %>% ungroup() %>%
#     glm(won ~ season + prev_noms*prev_wins,
#         data=., family='binomial') %>%
#     summary()
# #season+prev_noms+prev_wins: season, prev_noms signif -ve, prev_wins signif +ve
# #season+prev_noms*prev_wins: better; prev_wins +ve and interaction -ve
# 
# #viii) Model with series ratings
# joined %>% filter(cat_group == 'series') %>%
#     arrange(year, category, surname) %>%
#     group_by(year, category) %>% mutate(alph_pos = pmin(rank(surname),5),
#                                         alph_pos_234 = alph_pos %in% c(2,3,4)) %>% ungroup() %>%
#     glm(won ~ season + rel_rating + av_num_votes,
#         data=., family='binomial') %>%
#     summary()
# #season + rel_rating + av_num_votes gives 487.61->452.88; -ve season dominates, rel_rating signif +ve
# #  For year>=2000, similar
# #  For year<2000, av_num_votes is signif +ve - probably 'classic' shows being watched and rated later
# 
# #Acting
# joined %>% filter(cat_group == 'acting') %>%
#     arrange(year, category, surname) %>% 
#     group_by(year, category) %>% mutate(alph_pos = pmin(rank(surname),5),
#                                         alph_pos_234 = alph_pos %in% c(2,3,4)) %>% ungroup() %>%
#     glm(won ~ prev_noms + I(prev_wins==1) + alph_pos_234 + season + rel_rating + av_num_votes + show_also_nominated,
#         data=., family='binomial') %>%
#     summary()
# #rel_rating somewhat signif +ve; show_also_nominated very important
# #Win by having show also nominated, show rated higher than others, low season, not having 1 previous win, and not being in pos 234
# 
# #piecewiseSEM functions throw an error when using the full pipe syntax, so save data frame temporarily
# tmp <- joined %>% filter(cat_group == 'acting') %>%
#     arrange(year, category, surname) %>% 
#     group_by(year, category) %>% mutate(alph_pos = pmin(rank(surname),5),
#                                         alph_pos_234 = alph_pos %in% c(2,3,4)) %>% ungroup()
# glm(won ~ prev_noms + I(prev_wins==1) + season + alph_pos_234 + rel_rating + av_num_votes + show_also_nominated,
#     data=tmp, family='binomial') %>% piecewiseSEM::rsquared()
# #current best R2 = 0.19

#Set up 2021 rows -----
noms_2021 <- read.csv('data/gglobes_data_2021pending.csv', stringsAsFactors = FALSE) %>%
    mutate(cat_group = ifelse(category %in% c('Show Drama','Show Comedy'), 'series', 'acting'),
           candidate = ifelse(cat_group=='series', show, performer))
latest_prev_reference <- joined %>% select(year,category,cat_group,show,performer,prev_noms,prev_wins,
                                           prev_losses,prev_noms_without_win,won) %>%
    mutate(candidate = ifelse(cat_group=='series', show, performer)) %>%
    arrange(year,category) %>% group_by(candidate) %>% slice_tail(1) %>% ungroup() %>%
    mutate(prev_noms = prev_noms + 1,
           prev_wins = prev_wins + won,
           prev_losses = prev_losses + (1-won),
           prev_noms_without_win = ifelse(prev_wins==0, prev_noms, 0)) %>%
    select(candidate, prev_noms, prev_wins, prev_losses, prev_noms_without_win) 
noms_2021 <- left_join(noms_2021, 
                       latest_prev_reference, by='candidate') %>%
    mutate(prev_noms = ifelse(is.na(prev_noms),0,prev_noms),
           prev_wins = ifelse(is.na(prev_wins),0,prev_wins),
           prev_losses = ifelse(is.na(prev_losses),0,prev_losses),
           prev_noms_without_win = ifelse(is.na(prev_noms_without_win),0,prev_noms_without_win)) %>%
    mutate(won_emmy_prev_year = ifelse(show=="Schitt's Creek", TRUE, FALSE),  #applies to 3 2021 noms
           won_category_last_year = ifelse(performer %in% c('Ramy Youssef','Olivia Colman'), TRUE, FALSE),
           year_minus1 = year - 1,
           year_minus2 = year - 2)

show_data_2021 <- read.csv('data/imdb_data_gglobesnominees_2021pending.csv', stringsAsFactors = FALSE)%>% 
    group_by(show, season_start_year) %>% 
    summarise(season=first(season),
              av_rating = round(weighted.mean(av_rating, num_eps),2),
              av_num_votes = round(weighted.mean(av_num_votes, num_eps),0),
              num_eps = sum(num_eps), .groups='drop')

noms_2021 %<>% left_join(show_data_2021 %>% select(show,season,season_start_year,av_rating,av_num_votes),
                                    by=c('show',c('year_minus1'='season_start_year'))) %>%
    left_join(show_data %>% select(show,season,season_start_year,av_rating,av_num_votes),
              by=c('show', c('year_minus2'='season_start_year'))) %>%
    #average an NA and a real value by changing the NA to take the other value, then average
    mutate(av_rating.x = ifelse(is.na(av_rating.x), av_rating.y, av_rating.x),
           av_rating.y = ifelse(is.na(av_rating.y), av_rating.x, av_rating.y),
           av_rating = 0.5*(av_rating.x + av_rating.y),
           av_num_votes.x = ifelse(is.na(av_num_votes.x), av_num_votes.y, av_num_votes.x),
           av_num_votes.y = ifelse(is.na(av_num_votes.y), av_num_votes.x, av_num_votes.y),
           av_num_votes = 0.5*(av_num_votes.x + av_num_votes.y),
           earlier_season_awarded = ifelse(is.na(season.y), season.x, season.y),
           later_season_awarded = ifelse(is.na(season.x), season.y, season.x)) %>%
    select(-av_rating.x, -av_rating.y, -av_num_votes.x, -av_num_votes.y, 
           -season.x, -season.y, -year_minus1, -year_minus2)

noms_2021 %<>% 
    mutate(surname = purrr::map(performer, function(n) strsplit(n,' ')[[1]]),
           surname = purrr::map_chr(surname, function(n) if (length(n)==0) '' else n[length(n)])) %>%
    group_by(category) %>%
    mutate(candidate_name_length = purrr::map_int(candidate, nchar)) %>%
    group_by(category) %>% 
    mutate(alph_pos = pmin(rank(surname, ties.method='first'),5),
           alph_pos_1or5 = alph_pos %in% c(1,5),
           name_length_pos = pmin(rank(candidate_name_length, ties.method='first'),5),
           name_length_pos_1or5 = name_length_pos %in% c(1,5)) %>% ungroup() %>%
    data.frame()

noms_2021$show_also_nominated <- ifelse(noms_2021$cat_group == 'series', NA,
                                        ifelse(noms_2021$show %in% subset(noms_2021, cat_group=='series')$show, 1, 0))
noms_2021 %<>% group_by(show) %>% mutate(total_noms_this_year=n()) %>% ungroup()
#table(noms_2021$show_also_nominated, useNA='ifany')
#13/20 get this benefit - Odenkirk, Pacino, Rhys, Cheadle, Youssef, Comer, Levy don't

noms_2021$pred <- NA
noms_2021$pred[noms_2021$cat_group=='acting'] <- predict(model_acting, subset(noms_2021, cat_group=='acting'), type='response')
noms_2021$pred[noms_2021$cat_group=='series'] <- predict(model_series, subset(noms_2021, cat_group=='series'), type='response')
noms_2021 <- noms_2021 %>% group_by(category) %>% mutate(pred = pred/sum(pred)) %>% ungroup() %>% data.frame()
noms_2021 %>% arrange(-pred) %>% group_by(category) %>% 
    summarise(predictions = sprintf('%s: %s', category[1], paste(candidate,round(pred,2), collapse=': ')), .groups='drop') %>% 
    pull(predictions)

#Full model including Emmy and total noms:

#Show Comedy: none nominated before;
#  Schitt's Creek 0.36 due to emmy win, 3 total noms, and 0 prev noms, despite s6; The Great second
#In approx order of feature importance:
#subset(noms_2021, category=='Show Comedy') %>% select(show, won_emmy_prev_year, won_category_last_year, total_noms_this_year, av_rating, later_season_awarded, prev_noms, prev_wins, pred) %>% arrange(-pred)

#Show Drama: The Crown 3 noms, 1 win; others new
#  Ozark 0.39 due to first nom, 3 noms this year, good rating (Mandalorian similar but no other noms this year)
#    (The Crown 0.06 due to 3 prev noms, despite 4 noms this year)
#  Shows the importance of prev_noms rather than season in this model

#Actor Comedy: Cheadle 3 prev noms, 1 win; Youssef 1 nom 1 win; others new
#  Sudeikis (0.39) because show also nominated, s1, no prev noms (Levy s6)
#In approx order of feature importance:
#subset(noms_2021, category=='Actor Comedy') %>% select(performer, show_also_nominated, alph_pos, prev_wins, won_emmy_prev_year, total_noms_this_year, later_season_awarded, pred) %>% arrange(-pred)

#Actor Drama: Bateman, Odenkirk, Rhys 2-3 noms, 0 wins
#  Slightly Rhys (0.30), because alph5, season 1, no prev wins; could be any except Odenkirk 
#    (because show not nominated, s5, and 3 prev noms, despite highest av_rating (not in model))

#Actress Comedy: none nominated before
#  Collins 0.34, O'Hara third 0.20 (Collins show is nominated, s1, 0 prev noms, despite O'Hara emmy win; both favourable alph)

#Actress Drama: 1 win for Colman, Linney; 1 nom for Comer
#  Paulson (0.59) because show also nominated, name alpha 5, s1
#  Colman 0.10 because has 1 prev win, and winning last year isn't in model 
#    (win rate for people nominated and winning once is only 14%)
#  If I change Colman to (effectively) s2 it increases her prob only to 0.12.

#Not much money on BetFair but favours Crown/Ozark, Rhys, Levy/Sudeikis, strongly O'Hara, strongly Colman
#Paddy Power favours Schitt's Creek (50%, Ted Lasso second), The Crown (60%, Ozark second),
#  Levy (>50%, Sudeikis second), Rhys (45%, Bateman second),
#  O'Hara (70%, Collins 5th), Colman (60%, Paulson 4th)
#https://www.paddypower.com/special-bets/golden-globes
