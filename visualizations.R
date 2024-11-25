library(dplyr)
library(ggplot2)
library(patchwork)
library(jsonlite)

repos_raw <- read.csv("outputs/repos_2024-11-22.csv")
repos <- repos_raw %>%
  mutate(created_at = as.POSIXct(created_at),
         year = cut(created_at, breaks = "1 year")) %>%
  mutate(year = format(as.POSIXct(year), "%Y")) %>%
  filter(language != "") %>%
  mutate(license = ifelse(license == "", "None", license))

language_lines <- lapply(repos$language_lines,
                         function(x) {
                           as.data.frame(fromJSON(x), check.names = FALSE)
                         }) %>%
  do.call(bind_rows, .)

language_lines_sum <- sort(colSums(language_lines, na.rm = TRUE), decreasing = TRUE) %>%
  data.frame(language = names(.), cnt = ., pct = ./sum(.), row.names = NULL)
language_lines_top25 <- bind_rows(slice(language_lines_sum, 1:25),
                                  slice(language_lines_sum, 26:n()) %>%
                                    summarize(cnt = sum(cnt), pct = sum(pct)) %>%
                                    mutate(language = "Other")
                                  ) %>%
  #mutate(language = gsub(" ", "\n", language)) %>%
  mutate(language = factor(language, levels = language))

# find most common languages
languages <- sort(table(repos$language), decreasing = TRUE)
# find most common licenses
licenses <- sort(table(repos$license), decreasing = TRUE)

repos <- repos %>%
  mutate(language_clean = if_else(language %in% names(languages)[1:7],
                                  language, "Other"),
         license_clean = if_else(license %in% names(licenses)[1:7],
                                 license, "Other"))
repos <- repos %>%
  mutate(language_clean = factor(language_clean, levels = c(names(languages)[1:7], "Other")),
         license_clean = factor(license_clean, levels = c(names(licenses)[!names(licenses) %in% c("Other", "None")][1:5], "Other", "None")))

g1 <- ggplot(repos) +
  geom_bar(aes(x = year, fill = language_clean), show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  xlab("Year") +
  scale_y_continuous("# of GitHub Repositories", expand = expansion()) +
  theme_classic(base_size = 14) +
  theme(axis.text = element_text(color = "black"))

language_sum <- repos %>%
  count(language = language_clean) %>%
  mutate(pct = n / sum(n))

g2 <- ggplot(language_sum, aes(x = "", y = rev(pct), fill = rev(language))) +
  geom_col() +
  geom_text(aes(x = 1.25, y = rev(cumsum(rev(pct)) - rev(pct)/2),
                label = scales::percent(pct, 0.1))) +
  coord_polar("y", start = 0) +
  scale_fill_brewer(NULL, palette = "Set1") +
  theme_void(base_size = 14) +
  guides(fill = guide_legend(position = "bottom", direction = "vertical",
                             ncol = 2)) +
  theme(plot.margin = margin(), legend.margin = margin(b = 30))

free(g1) + g2 +
  plot_layout(widths = c(2.5, 1))

ggsave("figures/languages.pdf", width = 12, height = 5)
ggsave("figures/languages.png", width = 12, height = 5)

g3 <- ggplot(repos) +
  geom_bar(aes(x = year, fill = license_clean), show.legend = FALSE) +
  scale_fill_brewer(palette = "Dark2") +
  xlab("Year") +
  scale_y_continuous("# of GitHub Repositories", expand = expansion()) +
  theme_classic(base_size = 14) +
  theme(axis.text = element_text(color = "black"))

license_sum <- repos %>%
  count(license = license_clean) %>%
  mutate(pct = n / sum(n))

g4 <- ggplot(license_sum, aes(x = "", y = rev(pct), fill = rev(license))) +
  geom_col() +
  geom_text(aes(x = 1.25, y = rev(cumsum(rev(pct)) - rev(pct)/2),
                label = scales::percent(pct, 0.1))) +
  coord_polar("y", start = 0) +
  scale_fill_brewer(NULL, palette = "Dark2") +
  theme_void(base_size = 14) +
  guides(fill = guide_legend(position = "bottom", direction = "vertical",
                             ncol = 1)) +
  theme(plot.margin = margin(t = -20), legend.margin = margin(t = -20, b = 0))

free(g3) + g4 +
  plot_layout(widths = c(2.5, 1))

ggsave("figures/licenses.pdf", width = 12, height = 5)
ggsave("figures/licenses.png", width = 12, height = 5)

ggplot(language_lines_top25, aes(x = language, fill = language)) +
  geom_col(aes(y = pct * 100), show.legend = FALSE) +
  geom_text(aes(y = pct * 100 + 1, label = scales::percent(pct, 0.1)), size = 3) +
  scale_x_discrete(NULL) +
  scale_y_continuous("% Lines of Code", breaks = seq(0, 30, 5), expand = expansion(add = c(0, 1))) +
  scale_fill_viridis_d(option = "plasma", begin = 1, end = 0) +
  theme_classic(base_size = 14) +
  theme(axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggsave("figures/languages_by_line.pdf", width = 12, height = 5)
ggsave("figures/languages_by_line.png", width = 12, height = 5)
