library(tidyverse)
library(ggtext)
library(cowplot)
library(extrafont)
fonttable <- fonttable()
loadfonts(device = "win", quiet = TRUE) ## to load

# Prepare the data -----
# source: U.S. Treasury Inspector General for Tax Administration (TIGTA) 02/03/2020 Audit Report: Complexity and Insufficient Oversight of the Free File Program Result in Low Taxpayer Participation
# link to report: https://www.treasury.gov/tigta/auditreports/2020reports/202040009fr.pdf
# total number of taxpayers eligible for free online filing using the IRS Free File program: 104 million (out of 154 million total taxpayers)
# 34,508,312 non-Free File tax returns filed using the members’ software in Processing Year 2019/Tax Year 2018
eligible_filers_2018 <- tribble(
  ~category, ~count, ~file_type, ~label,
  "other means of filing", 67, "other", 1,
  "irs free file program", 2.5, "online", 1,
  "commercial website - paid a fee (estimated)", 14.1, "online", 1,
  "commercial website - free (estimated)", 20.4, "online", 1
) %>%
  mutate(pct = prop.table(count))

# From the IRS Data Book 2019, published June 2020 (https://www.irs.gov/pub/irs-pdf/p55b.pdf):
# online filers: 57,303,192 (includes those not eligible for IRS Free File program)
# used IRS Free File program: 2,811,883

# Theme -----
theme_set(theme_minimal())
theme <- theme_update(text = element_text(family = "IBM Plex Sans", size = 13),
                      title = element_text(size = 24, margin = margin(t = 0, r = 100, b = 0, l = 0)),
                      plot.title = element_markdown(size = 25, lineheight = 1.2),
                      plot.subtitle = element_markdown(size = 24, lineheight = 1.2),
                      plot.caption = element_text(size = 12, color = "gray50"),
                      axis.text = element_text(size = 20),
                      axis.text.y = element_blank(),
                      axis.title.y = element_blank(),
                      axis.title.x = element_text(color = "gray60", hjust=1),
                      axis.line = element_blank(),
                      panel.grid = element_blank(),
                      plot.margin = margin(40),
                      plot.background = element_rect(fill = "#F7F7F7", color = "#F7F7F7"))


# Plots ----
# Of the 37 million online filers last year, only 37 million of them filed online
eligible_taxpayers <- eligible_filers_2018 %>%
  group_by(file_type) %>%
  summarise(sum = sum(count)) %>%
  summarise(file_type, sum, pct_file_type = prop.table(sum)) %>%
  mutate(label= 1) %>%
  ggplot(aes(label, sum,  fill = file_type)) +
  labs(title = "<b>Last year, only 37 million of the 104 million taxpayers eligible for the <span style = 'color:#FF9F1C;'>IRS's Free File<br>Program</span> filed <span style = 'color:#E71D36;'>online</span></b>",
       y = "Number of eligible taxpayers") +
  geom_col(show.legend = FALSE, width = 1) +
  geom_text(aes(label = scales::percent(pct_file_type)),
            color = "white",
            fontface = "bold",
            position = position_stack(vjust = 0.5), family = "IBM Plex Sans", size = 6) +
  scale_fill_manual(values = c("#E71D36","gray70"))+
  scale_y_continuous(labels = scales::label_number(suffix = "M"), expand = expansion(0,0)) +
  coord_flip()

# Breakdown of online filers - only 2.5 million used the Free File Program
online_filers <- eligible_filers_2018 %>%
  filter(file_type == "online") %>% ## filter to only include online filers
  ggplot(aes(label,count,  fill = fct_reorder(category,count))) +
  labs(title = "<b>Of the 37 million eligible <span style = 'color:#E71D36;'>online</span> filers, 34.5 million filed through a <span style = 'color:#5e548e;'>commercial<br>website</span> and only 2.5 million filed through the <span style = 'color:#FF9F1C;'>Program</span></b>",
       subtitle = "An estimated 14.1 million still paid a fee to prepare and e-file their Federal return.",
       y = "Number of online filers") +
  geom_col(show.legend = FALSE, width = 1) +
  ## annotations
  annotate("text", x = 1.7, y = 20.4, label = "Paid a fee", hjust = 0, family = "IBM Plex Sans",fontface = "bold", color="#5e548e", size = 7) +
  annotate("text", x = 1.7, y = 0, label = "Free through a commercial site", hjust = 0, family = "IBM Plex Sans",fontface = "bold", color="#9f86c0", size = 7) +
  annotate("text", x = 0.75, y = 10.5, label = "of all 104M eligible taxpayers", hjust = 0.5, family = "IBM Plex Sans", fontface = "bold", color="white", size = 6) +
  ## percent annotations
  geom_text(aes(label = scales::percent(pct)), color = "white", fontface = "bold", position = position_stack(vjust = 0.5), family = "IBM Plex Sans", size = 6) +
  scale_fill_manual(values = c("#FF9F1C","#5e548e","#9f86c0", "#2EC4B6"))+
  scale_x_continuous(limits = c(0.5,1.8)) +
  scale_y_continuous(labels = scales::label_number(suffix = "M"), expand = expansion(0,0)) + ## add "M" to indicate counts are in millions
  coord_flip()

# Plot title
t1 <- ggplot() +
  labs(title = "If you make <b>$69,000</b> or less a year, you can file your federal taxes<br>online <b>for free</b> through the <b><span style = 'color:#FF9F1C;'>IRS's Free File Program</span></b>.") +
  theme(axis.line = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_markdown(size = 32))

# 2nd text block
# sources:
# ProPublica - https://www.propublica.org/article/turbotax-and-others-charged-at-least-14-million-americans-for-tax-prep-that-should-have-been-free-audit-finds, https://www.propublica.org/series/the-turbotax-trap
# IRS - https://www.irs.gov/newsroom/irs-free-file-use-soars-taxpayers-still-have-time-to-do-their-taxes-for-free
t2 <- ggplot() +
  labs(title = "<b><span style = 'color:#304154;'>ProPublica</span></b> reports that the tax prep industry made around <b>$1 billion</b> in revenue <br>from people who were eligible to file for free.
       Although the IRS reported that as of <br>April 2020, the use of <b><span style = 'color:#FF9F1C;'>Free File</span></b> has increased by 16%, that is still only 2.9 million <br>of the currently more than 104 million eligible taxpayers.<br><br>
       The efforts of the tax prep industry to deceive customers and lobby against free <br>and easy tax filing, in addition to the complexity and obscurity of the <b><span style = 'color:#FF9F1C;'>Free File <br>Program</span></b>, result in many taxpayers not utilizing the product.<br>
       <b>So spread the word and file for free at <span style = 'color:#FF9F1C;'>irs.gov/freefile</span></b>.",
       caption = "Sources: U.S. Treasury Inspector General for Tax Administration, ProPublica, IRS") + ## sources for entire infographic
  theme(plot.title = element_markdown(size = 26, lineheight = 1.5, color = "black"))

# Combine plots + save the final plot
plot_grid(t1, eligible_taxpayers, online_filers, t2, ncol = 1, rel_heights = c(0.04,0.1,0.15,0.2)) +
  theme(plot.margin = margin(50,50,50,50), plot.background = element_rect(fill = "#F7F7F7", color = "#F7F7F7"))
ggsave("IRS_free_file_program.png", device = "png", type = "cairo", width = 15, height = 15, dpi = 300)

