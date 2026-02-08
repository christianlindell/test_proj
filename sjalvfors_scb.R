library(tidyverse)
library(pxweb)

# d <- pxweb_interactive("api.scb.se")

# PXWEB query
pxweb_query_list <-
  list(
    "Region" = c("*"),
    "Kon" = c("1+2"),
    "Alder" = c("20-64"),
    "Fodelseregion" = c("samt", "in", "ut", "ute", "utee"),
    "ContentsCode" = c("000007X7", "000007X5", "000007X3"),
    "Tid" = c("*")
  )

# Download data
px_data <-
  pxweb_get(
    url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/HE/HE0000/HE0000Tab04",
    query = pxweb_query_list
  )


# Convert to data.frame
df <-
  bind_cols(
    as.data.frame(
      px_data,
      column.name.type = "text",
      variable.value.type = "code"
    )[1],
    as.data.frame(
      px_data,
      column.name.type = "text",
      variable.value.type = "text"
    )
  ) |>
  janitor::clean_names() |>
  tibble::tibble() |> 
  mutate(ar = as.integer(ar))


dftot <- df |>
  rename(region_kod = 1, region = 2) |>
  filter(fodelseregion == "samtliga födelseregioner") |>
  filter(nchar(region_kod) == 8)

dfej_stud <- dftot |> 
  filter(ar == max(ar, na.rm = TRUE)) |>  
   filter(andel_studerande_varnpliktiga < 35) |> 
  select(region_kod)

dftot_ej_stud <- dftot |> 
  right_join(dfej_stud) |> 
  mutate(andel_sjalvforsorjande = antal_sjalvforsorjande / antal_personer_totalt)

dfurval_2013_23 <- dftot_ej_stud |> 
  filter(ar %in% c(min(ar), max(ar))) |> 
  select(region_kod, region, ar, antal_personer_totalt, andel_sjalvforsorjande) |> 
  arrange(ar, andel_sjalvforsorjande) |> 
    pivot_wider(
    id_cols = c(region_kod, region),
    names_from = ar,
    values_from = c(antal_personer_totalt, andel_sjalvforsorjande),
    names_glue = "{.value}_{ar}"
  ) |> 
  mutate(percentilgrupp_30 = ntile(andel_sjalvforsorjande_2013, 30))

dfpercentilgrp_2013 <- dfurval_2013_23 |> 
  select(region, region_kod, percentilgrp_2013 = percentilgrupp_30)



dfpercentiler <- dftot_ej_stud  |> 
select(region, region_kod, ar, antal_sjalvforsorjande, antal_personer_totalt) |> 
  mutate(andel_sjalvforsorjande = antal_sjalvforsorjande / antal_personer_totalt) |> 
  mutate(.by = c(ar), percentilgrupp_30 = ntile(andel_sjalvforsorjande, 30)) |> 
  left_join(dfpercentilgrp_2013)


dfpercentiler_summa <- dfpercentiler |> 
  mutate(antal_reg = 1) |> 
  select(-c(andel_sjalvforsorjande, region, region_kod, percentilgrp_2013)) |> 
  summarise(.by = c(ar, percentilgrupp_30), across(everything(), ~sum(.x, na.rm = TRUE))) |> 
  arrange(ar, percentilgrupp_30) |> 
  mutate(andel_sjalvforsorjande = antal_sjalvforsorjande / antal_personer_totalt)


# dfpercentiler_summa <- dfpercentiler |> 
dfpercentiler_summa


diasjalvfors <- dfpercentiler_summa %>% 
  mutate(percentilgrupp_30 = as.character(percentilgrupp_30)) %>% 
  mutate(ar = as.numeric(ar)) %>% 
  ggplot(aes(ar, andel_sjalvforsorjande, colour = percentilgrupp_30)) +
  geom_line() +
  scale_color_viridis_d() +
  theme_bw() +
  guides(colour = "none") +
  scale_y_continuous(breaks = scales::breaks_pretty(10), labels = scales::percent_format()) +
  scale_x_continuous(breaks = scales::breaks_pretty(10)) +
  labs(
    title = str_wrap("Andel självförsörjande av befolkningen 20-64 år, 2013 -2023 per grupp av stadsdelar (RegSO)", 60 ),
    subtitle = str_wrap("Varje år har stadsdelarna grupperats i 30 grupper baserat på andelen självförsörjande. Varje grupp omfattar 111 stadsdelar. Linjen längst ned visar självförsörjningsgrad för de 111 stadsdelar som har lägst andel självförsörjande. Studentområden har räknats bort", 70),
    x = NULL,
    y = NULL,
    caption = "Källa: SCB"
  ) +
  annotate(
    "text",
    x = 2013, y = 0.91,
    label = "Den 30-del av stadsdelarna\nsom har högst självförsörjningsgrad",
    hjust = 0,
    size = 3
  ) +
  annotate("segment",
           x = 2014.5, xend = 2015,
           y = 0.88, yend = 0.87,
           arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate(
    "text",
    x = 2013, y = 0.455,
    label = "Den 30-del av stadsdelarna\nsom har lägst självförsörjningsgrad",
    hjust = 0,
    size = 3
  ) +
  annotate("segment",
           x = 2014.5, xend = 2015,
           y = 0.43, yend = 0.422,
           arrow = arrow(length = unit(0.2, "cm"))
  )

ggsave(plot = diasjalvfors, filename = "dia_sjalvfors_per_regso30perc_annoterad2.svg", units = "cm", width = 15, height = 15)
ggsave(plot = diasjalvfors, filename = "dia_sjalvfors__per_regso30perc2_annoterad.png", dpi = 300, units = "cm", width = 15, height = 15)



