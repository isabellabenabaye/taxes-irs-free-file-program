library(tidyverse)
library(hrbrthemes)
library(ggtext)
library(extrafont)
fonttable <- fonttable()
loadfonts(device = "win", quiet = TRUE) ## to load

# Prepare the data -----
# source: U.S. Treasury Inspector General for Tax Administration (TIGTA) 02/03/2020 Audit Report: Complexity and Insufficient Oversight of the Free File Program Result in Low Taxpayer Participation
# link to report: https://www.treasury.gov/tigta/auditreports/2020reports/202040009fr.pdf
# total number of taxpayers eligible for free online filing using the IRS Free File program: 104 million (out of 154,011,950)
# 34,508,312 non-Free File tax returns filed using the members’ software in Processing Year 2019
online_eligible_filers_2018 <- tribble(
  ~category, ~count, ~label,
  "irs free file program", 2.5, 1,
  "commercial website - paid a fee (estimated)", 14.1, 1,
  "commercial website - free (estimated)", 20.4, 1
) %>%
  mutate(pct = prop.table(count))

returns_examined <- tribble(
  ~year, ~percentage_covered,
  2010, 1.01,
  2011, 0.89,
  2012, 0.81,
  2013, 0.64,
  2014, 0.58,
  2015, 0.57,
  2016, 0.49,
  2017, 0.33,
  2018, 0.15
)

# Percentage of returns examined/audited: 2010-2018
# source: IRS Data Book 2019
returns_examined %>%
  ggplot(aes(year, percentage_covered)) +
  geom_line() +
  scale_y_continuous(limits = c(0,1.1), expand = expansion(0,0)) +
  theme_ipsum(base_family = "IBM Plex Sans")

# IRS Data Book
# online filers: 57,303,192 (may include those not eligible for IRS Free File program)
# used IRS Free File program: 2,811,883

theme_set(theme_minimal())
theme <- theme_update(text = element_text(family = "IBM Plex Sans", size = 13),
                      title = element_text(size = 24, margin = margin(t = 0, r = 100, b = 0, l = 0)),
                      plot.title = element_markdown(face = "bold", size = 26, lineheight = 1.2),
                      plot.caption = element_text(size = 12, color = "gray50"),
                      axis.text = element_text(size = 20),
                      axis.text.y = element_text(size = 18),
                      axis.line.x = element_line(color = "gray60"),
                      axis.line.y = element_blank(),
                      panel.grid = element_blank(),
                      panel.grid.major.y = element_line(color = "gray90"),
                      legend.text = element_text(size = 20, face = "bold"),
                      plot.margin = margin(70, 80, 30, 35),
                      plot.background = element_rect(fill = "#F7F7F7", color = "#F7F7F7"),
                      legend.position = c(.15, .51),
                      legend.direction = "horizontal")

# Of the 37 million online filers last year, only 2.5 million of them utilized the IRS Free File program.
online_eligible_filers_2018 %>%
  ggplot(aes(label,count,  fill = fct_reorder(category,count))) +
  labs(title = "Of the 104 million taxpayers eligible for the <span style = 'color:#FF9F1C;'>IRS's Free File Program</span> last year, 37 million filed online:<br>34.5 through a <span style = 'color:#E71D36;'>commercial website</span> and only 2.5 million through the <span style = 'color:#FF9F1C;'>Program</span>",
       subtitle = "An estimated 14.1 million still paid a fee to prepare and e-file their Federal return.",
       y = "Number of online filers") +
  geom_col(show.legend = FALSE, width = 1) +

  #annotate("text", x = 1.7, y = 34, label = "Filed using IRS's Free File Program", hjust = 1, family = "IBM Plex Sans", color="#FF9F1C",size = 6.4) +
  #geom_curve(aes(x = 1.7, y = 34, xend= 1.5, yend = 35.7),
   #          arrow = arrow(length = unit(0.07, "inch")),
    #         curvature=-0.3, color="#FF9F1C") +
  annotate("text", x = 1.6, y = 20.4, label = "Paid a fee", hjust = 0, family = "IBM Plex Sans", color="#E71D36",size = 6.4) +

  annotate("text", x = 1.6, y = 0, label = "Free through a commercial site", hjust = 0, family = "IBM Plex Sans", color="#F17E8C",size = 6.4) +

  scale_fill_manual(values = c("#FF9F1C","#E71D36","#F17E8C"))+
  scale_y_continuous(labels = scales::label_number(suffix = "M"), expand = expansion(0,0)) +
  coord_flip() +
  theme(panel.grid = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(face = "italic", hjust=1))

# As of April 2020, the usage of the Free File Program has increased by 26%, but that is still only ___M/% of
