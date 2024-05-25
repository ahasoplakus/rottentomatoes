library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(rvest)
library(gt)
library(gtExtras)

get_img_src <- function(pg) {
  pg |>
    html_elements("a > div > img") |>
    html_attrs() |>
    keep(\(x) x[["class"]] == "article_poster") |>
    map_chr(\(x) x[["src"]])
}

get_dir_names <- function(pg) {
  pg |>
    html_elements("div.info.director") |>
    html_text2() |>
    str_remove(pattern = "Directed By: ") |>
    str_replace_all(" ", "_")
}

get_cast <- function(pg) {
  pg |>
    html_elements("div.info.cast") |>
    html_text2() |>
    str_remove(pattern = "Starring: ")
}

get_imdb_movies <- function(pg) {
  name <- pg |>
    html_elements(".ipc-title__text") |>
    html_text2()

  ratings <- pg |>
    html_elements(".ipc-metadata-list-summary-item__tc") |>
    html_elements("div") |>
    html_text2()

  ratings <-
    map_chr(seq(4, by = 4, length.out = length(ratings) / 4), \(x) ratings[x]) |>
    str_sub(1, 3) |>
    as.numeric()

  data.frame(name = name[3:252], imdb = ratings) |>
    mutate(name = str_squish(str_remove(name, paste0(row_number(), ". "))),
    imdb = round(imdb))
}

rot <- "https://editorial.rottentomatoes.com/guide/best-movies-of-all-time/"
imdb <- "https://www.imdb.com/chart/top/"

html <-map(c(rot, imdb), read_html) |>
  set_names("tomato", "imdb")

rt <- html$tomato |>
  html_elements("table") |>
  html_table() |>
  bind_rows() |>
  mutate(across(everything(), \(x) str_squish(x))) |>
  select("rank" = "X1", "X2") |>
  separate_wider_delim(X2, delim = "% ", names = c("score", "name")) |>
  separate_wider_delim(name, delim = " (", names = c("name", "year")) |>
  mutate(year = str_extract(year, "\\d+")) |>
  mutate(across(c(1, 2, 4), as.numeric)) |>
  left_join(get_imdb_movies(html$imdb), by = "name") |>
  slice_head(n = 50)

rt |>
  gt(groupname_col = "rank") |>
  text_transform(
    locations = cells_body(columns = logo),
    fn = \(x) web_image(url = x, height = 175)
  ) |>
  fmt_pct_extra(columns = score) |>
  gt_fa_rating(
    column = imdb,
    max_rating = 10,
    width = "20px"
  ) |>
  cols_width(
    rank ~ px(25),
    logo ~ px(180),
    c("name", "dir", "cast") ~ px(180),
    imdb ~ px(200),
    everything() ~ px(100)
  ) |>
  cols_move(
    "name",
    after = "logo"
  ) |>
  cols_move_to_end(c("score", "imdb")) |>
  cols_label(
    "rank" ~ "",
    "logo" ~ "",
    "name" ~ "Movie",
    "score" ~ with_tooltip(
      html(local_image(filename = "apple.png", height = 25)),
      "Critic Score"
    ),
    "dir" ~ "Directed By",
    "cast" ~ "Starring",
    "year" ~ "Year",
    "imdb" ~ with_tooltip(
      html(local_image(filename = "imdb.png", height = 20)),
      "IMDb Ratings"
    )
  ) |>
  tab_style(
    style = cell_text(
      align = "center",
      font = google_font("Chivo"),
      color = "#fdde55",
      weight = "bold"
    ),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_body(columns = rank)
  ) |>
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = -rank)
  ) |>
  tab_style(
    style = cell_fill(color = "#222831"),
    locations = cells_body()
  ) |>
  tab_style(
    style = cell_fill(color = "#222831"),
    locations = cells_row_groups()
  ) |>
  tab_style(
    style = cell_text(color = "tomato", font = google_font("Poppins")),
    locations = cells_body(columns = -score)
  ) |>
  tab_style(
    style = cell_text(color = "#65b741", font = google_font("Poppins")),
    locations = cells_body(columns = score)
  ) |>
  tab_style(
    style = cell_text(color = "#fdde55", font = google_font("Poppins")),
    locations = cells_row_groups()
  ) |>
  tab_style(
    style = cell_fill(color = "#a91d3a"),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_borders(color = "#a91d3a", sides = "bottom", weight = px(2)),
    locations = cells_column_labels()
  ) |>
  tab_options(
    data_row.padding = px(10),
    table.font.size = 15,
    row_group.font.size = 18,
    row_group.border.bottom.style = "hidden",
    row_group.border.top.color = "#597e52",
    column_labels.font.size = 20,
    table.border.top.color = "tomato",
    table.border.right.color = "tomato",
    table.border.left.color = "tomato"
  ) |>
  opt_css(
    css = "
    .gt_table {
      width: max-content !important;
    }
    abbr > img {
       vertical-align: -0.45em !important;
    }
    div > svg {
       width: 14px !important;
    }
    tr > td > img {
       border-radius: 8px !important;
    }
    "
  )
