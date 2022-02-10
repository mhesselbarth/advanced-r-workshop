library(cowplot)
library(googlesheets4)
library(tidyselect)
library(tidyverse)

# get googlesheet with results
survey_df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1VbfapXDMQYxzU2l0U5Slq3Q4XLz7P7eiNkNZLRvaUp4/edit?usp=drive_web&ouid=106897516412995049455")

# rename columns
names(survey_df) <- c("timestamp", "email", "name", "research", "experience", 
                      "topics.general", "topics.tidy", "topics.viz", "topics.spat", 
                      "topics.pkg", "topics.mrkdwn", "topics.hpc", "topics.bug", 
                      "suggestions", "teach", "os", "expectations")

# get experience figure 
gg_experience <- dplyr::mutate(survey_df, experience = factor(experience, levels = 1:5)) %>% 
  ggplot(aes(x = experience)) + 
  geom_bar(fill = NA, col = "black") + 
  geom_text(stat = "count", aes(label = paste0("n=",..count..)), vjust = 2) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "How would you describe your current R programming experience?", 
       y = "") + 
  theme_classic()

tidyr::pivot_longer(data = survey_df, cols = tidyselect::starts_with("topics."), 
                    names_to = "topics") %>%
  dplyr::mutate(value_num = dplyr::case_when(value == "First choice" ~ 3, 
                                             value == "Second choice" ~ 2, 
                                             value == "Third choice" ~ 1)) %>% 
  dplyr::group_by(topics) %>% 
  dplyr::summarise(value_sum = sum(value_num, na.rm = TRUE)) %>% 
  dplyr::arrange(-value_sum)

gg_os <- dplyr::mutate(survey_df, os = factor(os)) %>% 
  ggplot(aes(x = os)) + 
  geom_bar(fill = NA, col = "black") + 
  geom_text(stat = "count", aes(label = paste0("n=",..count..)), vjust = 2) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "What operating system are you using?", 
       y = "") + 
  theme_classic()

cowplot::plot_grid(gg_experience, gg_os, nrow = 2)

# pull email addresses only
paste(survey_df$email, collapse = ", ")

