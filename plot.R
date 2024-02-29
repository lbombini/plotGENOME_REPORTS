library(ggplot2)
library(tidyr)

source("data_prep.R")
if (!file.exists("plots")) {
  dir.create("plots")
}

df <- get_all_genomes()
df_balanced <- get_balanced(df)

year_sum <- get_yearly_summary(df)

#Color palette
palette=c("#003f5c",
          "#2f4b7c",
          "#665191",
          "#a05195",
          "#d45087",
          "#f95d6a",
          "#ff7c43",
          "#ffa600")

# genomeVSgenes
ggplot(df_balanced, aes(x=Size, y=Genes, color=Domen)) +
  geom_point(size=1.6) +
  scale_color_manual(name = NULL,
                     values = palette[c(3, 6, 8)]) +
  ylim(0, 20000) +
  xlim(0, 75) +
  labs(x = "Genome Size, Mb",
       y = "Number of Genes") +
  theme_minimal(base_size = 14)
ggsave('plots/genomeVSgenes.png', width = 7, height = 4)

#genomes-sequenced
endpoints <- filter(year_sum, Year == max(Year))
xlim <- max(endpoints$Year) + 1
ylim <- round(max(endpoints$Genomes_cumsum)) * 1.04
ggplot(year_sum, aes(x=Year, y=Genomes_cumsum, color=Domen)) +
  geom_line() +
  scale_color_manual(name = NULL,
                     values = palette[c(3, 6, 8)]) +
  labs(x = NULL,
       y = "Genomes sequenced") +
  theme_minimal(base_size = 16) +
  scale_x_continuous(breaks = seq(min(year_sum$Year), max(year_sum$Year), by = 5)) +
  geom_text(data = endpoints,
              label = endpoints$Genomes_cumsum,
              vjust = -1,
            size = 4) +
  theme(axis.text.x = element_text(angle = 45)) +
  expand_limits(x = xlim, y = ylim)

ggsave('plots/genomes-sequenced.png', width = 7, height = 4)
