# Reproduktionszahl Thüringen
require(EpiEstim)

names(dftsI)
OurTools::Table(dftsI$county)


names(RKI_df)

dftsI_TH <- dftsI %>% filter(county == "SK Leipzig" | Bundesland == "Thüringen" )  %>%
  group_by(county) %>% 
  mutate(idx = which(infected != 0)[1] - 1) %>%
  filter(id >= idx) %>%
  group_by(county) %>%
  mutate(id = row_number()) %>%
  select(-idx) %>%
  rename(i = infected)


fltcounty <- "SK Gera"
dfestR <- dftsI_TH %>% filter(county == fltcounty )

dfestdat <- dfestR %>% select(Datum, id)

#------------------------------------------------------------------------------------
# Reproduktionszahl für 7 Tage Intervalle
#------------------------------------------------------------------------------------
T <- length(dfestR$i)
t_start <- seq(2, T-6) # starting at 2 as conditional on the past observations
t_end <- t_start + 6 # adding 6 to get 7-day windows as bounds included in window
# Mit SI Distribution
estR7 <- estimate_R(dfestR$i,
                    method="non_parametric_si",
                    config = make_config(list(
                      t_start = t_start,
                      t_end = t_end,
                      si_distr = dfestR$ip))
)

dfestR7 <- estR7$R %>% left_join(dfestdat)


#p<-plot(res_parametric_si, legend = FALSE,what = "R", options_R = list(col = "darkred",size = 2));p
p<-plot(estR7, legend = FALSE,what = "incid", options_R = list(col = "darkred",size = 2));p
p<-plot(estR7, legend = FALSE,what = "R", options_R = list(col = "darkred",size = 2));p

p + theme_minimal() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25,face = "bold")
  ) +
  labs(y = "Reproduktionszahl, gesch\u00E4tzt",
       title = paste0("Reproduktionszahl in ",fltcounty," - 7 Tages Intervall"),
       x = "Tage seit erstem Fall") +
  scale_x_continuous(breaks = seq(min(dfestdat$id),max(dfestdat$id), 7),
                     label = format(dfestdat$Datum[seq(min(dfestdat$id),max(dfestdat$id), 7)], format="%d-%m"))
