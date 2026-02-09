recode_mrot_items <- \(df) {
  df |>
    separate(item, into = c("task_code", "item_group", "item_entry", "angle")) |>
    unite("item", "item_group", "item_entry", sep = " ", remove = FALSE) |>
    mutate(itemtype = itemtype |> fct_relevel("Rasch"),
           item_group = item_group |> as_factor(),
           item_entry = item_entry |> as_factor(),
           angle = angle |> as.numeric())
}

mrot_2pl_f1_scalar <- read_rds("02_scoring_outputs/model_registry/mrot/multigroup_site/all_items/mrot_2pl_f1_scalar.rds")
mrot_rasch_f1_scalar <- read_rds("02_scoring_outputs/model_registry/mrot/multigroup_site/all_items/mrot_rasch_f1_scalar.rds")

# recon_model <- \(mod_rec) multipleGroup(data = mod_rec@data, group = mod_rec@groups, pars = mod_vals, TOL = NaN)
mod_2pl <- model_from_record(mrot_2pl_f1_scalar)
mod_rasch <- model_from_record(mrot_rasch_f1_scalar)

coef_2pl <- coef(mod_2pl, simplify = TRUE, IRTpars = TRUE)[[1]]$items |> as_tibble(rownames = "item") |> mutate(itemtype = "2PL")
coef_rasch <- coef(mod_rasch, simplify = TRUE, IRTpars = TRUE)[[1]]$items |> as_tibble(rownames = "item") |> mutate(itemtype = "Rasch")
coef_mrot <- bind_rows(coef_rasch, coef_2pl) |>
  rename(difficulty = b) |>
  # select(-g, -u) |>
  # mutate(difficulty = -d / a1) |>
  mutate(item = str_remove(item, "-[0-9]+$")) |>
  distinct() |>
  recode_mrot_items() |>
  mutate(interpolated = FALSE)

poly_items <- expand_grid(
  itemtype = c("Rasch", "2PL"),
  item = c(
    "mrot_2d_polygon1_040",
    "mrot_2d_polygon1_080",
    "mrot_2d_polygon1_120",
    "mrot_2d_polygon1_160",
    "mrot_2d_polygon1_200",
    "mrot_2d_polygon1_240",
    "mrot_2d_polygon1_280",
    "mrot_2d_polygon1_320",
    "mrot_2d_polygon2_040",
    "mrot_2d_polygon2_080",
    "mrot_2d_polygon2_120",
    "mrot_2d_polygon2_160",
    "mrot_2d_polygon2_200",
    "mrot_2d_polygon2_240",
    "mrot_2d_polygon2_280",
    "mrot_2d_polygon2_320"
  )
) |> recode_mrot_items() |>
  mutate(angle = map_dbl(angle, \(a) min(a, 360 - a)))

contrasts(coef_mrot$item_group) <- contr.sum(n_distinct(coef_mrot$item_group))
mrot_grid <- expand_grid(item_group = unique(coef_mrot$item_group), angle = unique(poly_items$angle))

param_fits <- coef_mrot |>
  nest(coefs = -itemtype) |>
  mutate(param_mod = map(coefs, \(cr) lm(difficulty ~ angle * item_group, data = cr)),
         param_fits = map(param_mod, \(pmod) {
           broom::augment(pmod, newdata = mrot_grid) |>
             group_by(angle) |>
             summarise(difficulty = mean(.fitted))
         })) |>
         # param_coef = map(param_mod, \(mod) broom::tidy(mod))) |>
  select(itemtype, param_fits) |>
  unnest(param_fits)

poly_fits <- poly_items |> left_join(param_fits) |> mutate(interpolated = TRUE)

coef_combined <- coef_mrot |> bind_rows(poly_fits) |>
  mutate(item = item |> fct_relevel("2d rabbit", "2d duck"))

ggplot(coef_combined |> filter(itemtype == "Rasch"),
       aes(x = angle, y = difficulty, color = item)) +
  # facet_wrap(vars(itemtype)) +
  geom_point(aes(shape = interpolated)) +
  geom_line(aes(group = item, linetype = interpolated)) +
  # geom_abline(intercept = intercept, slope = slope, linetype = "dotted") +
  scale_colour_ptol() +
  scale_shape_manual(values = c(16, 1)) +
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Estimated", "Interpolated")) +
  guides(colour = guide_legend(reverse = TRUE), shape = "none", linetype = guide_legend(title = NULL)) +
  labs(x = "Angle", y = "Difficulty (from 1PLg model)", color = "Item type")
ggsave("mrot_coefs.png", width = 7, height = 4.5)

# ggplot(coef_mrot, aes(x = angle, y = a, color = item)) +
#   facet_wrap(vars(itemtype)) +
#   geom_point(aes(shape = interpolated)) +
#   geom_line(aes(group = item, linetype = interpolated)) +
#   # geom_abline(intercept = intercept, slope = slope, linetype = "dotted") +
#   scale_colour_ptol() +
#   scale_shape_manual(values = c(16, 1)) +
#   scale_linetype_manual(values = c("solid", "dashed"), labels = c("estimated", "interpolated")) +
#   guides(colour = guide_legend(reverse = TRUE), shape = "none", linetype = guide_legend(title = NULL)) +
#   labs(x = "Angle", y = "Difficulty", color = "Item type")

theta <- seq(-5, 5, by = .01)

item_info <- coef_combined |>
  filter(itemtype == "Rasch") |>
  select(item_group, item, angle, difficulty, interpolated) |>
  expand_grid(theta) |>
  mutate(g = 0.5,
         p = exp(theta - difficulty) / (1 + exp(theta - difficulty)),
         p_g = g + (1 - g) * p,
         # info = p * (1 - p),
         info = (1 / (1 - g)) ^ 2 * (p_g - g) ^2 / p_g * (1 - p_g))

ggplot(item_info |> mutate(angle = angle |> as_factor()), aes(x = theta, y = info)) +
  facet_wrap(vars(item), nrow = 1) +
  geom_vline(xintercept = 0, colour = "#EBEBEBFF") +
  geom_line(aes(colour = angle, linetype = interpolated)) +
  scale_colour_viridis_d(direction = -1) +
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Estimated", "Interpolated")) +
  guides(linetype = "none") +
  labs(x = "Theta", y = "Information (from 1PLg model)", colour = "Angle") +
  theme(legend.position = "top")
ggsave("mrot_item_info.png", width = 10, height = 3)

item_info_blocks <- item_info |>
  mutate(block = case_when(
    item_group == "3d" ~ "3d",
    str_detect(item, "polygon") ~ "2d polygons",
    .default = "2d animals")) |> #filter(theta == -2) |> View()
  group_by(block, theta) |>
  summarise(info = mean(info), n = n(), items = list(item)) |>
  ungroup()

block_compare <- item_info_blocks |>
  select(block, theta, info) |>
  pivot_wider(names_from = block, values_from = info)

cross_animal_poly <- block_compare |> filter(`2d polygons` >= `2d animals`) |> pull(theta) |> min()
cross_poly_shape <- block_compare |> filter(`3d` >= `2d polygons`) |> pull(theta) |> min()
thresholds <- tibble(theta = c(cross_animal_poly, cross_poly_shape))

# thresholds <- item_info_blocks |>
#   group_by(block) |>
#   filter(info == max(info))

ggplot(item_info_blocks, aes(x = theta, y = info)) +
  geom_vline(aes(xintercept = theta), data = thresholds, colour = "lightgrey") +
  geom_label(aes(label = theta), y = Inf, data = thresholds, vjust = 1, size = 3) +
  geom_line(aes(colour = block)) +
  scale_colour_ptol() +
  labs(x = "Theta", y = "Item information (block average)", colour = "Block") +
  theme(legend.position = "top")
ggsave("mrot_info_blocks.png", width = 7, height = 4.5)

# item_name <- "mrot_2d_rabbit_020-1"
# i1 <- extract.item(mod_rasch, item = item_name, group = 1)
# rasch_info <- iteminfo(i1, -4)

get_item_info <- \(mod, item_name, thetas) {
  item_obj <- extract.item(mod, item = item_name, group = 1) # same regardless of group
  tibble(theta = thetas, info = iteminfo(item_obj, thetas), item = item_name)
}

get_info <- \(mod, thetas) {
  items <- extract.mirt(mod, "itemnames") |> unique()
  map(items, \(i) get_item_info(mod, i, thetas)) |> list_rbind()
}

info_2pl <- get_info(mod_2pl, thetas) |> mutate(itemtype = "2PL")
info_rasch <- get_info(mod_rasch, thetas) |> mutate(itemtype = "Rasch")

info_combined <- bind_rows(info_2pl, info_rasch) |>
  mutate(item = str_remove(item, "-[0-9]+$")) |>
  distinct() |>
  separate(item, into = c("task_code", "item_group", "item_entry", "angle")) |>
  unite("item", "item_group", "item_entry", sep = " ", remove = FALSE) |>
  mutate(itemtype = itemtype |> fct_relevel("Rasch"))

ggplot(info_combined, aes(x = theta, y = info)) +
  facet_grid(vars(itemtype), vars(item), scales = "free") +
  geom_vline(xintercept = 0, colour = "#EBEBEBFF") +
  geom_line(aes(colour = angle)) +
  scale_colour_viridis_d(direction = -1) +
  labs(x = "Theta", y = "Item information") +
  theme(legend.position = "top")
  # theme(panel.grid.major.x = element_line(colour = "lightgrey"))
ggsave("mrot_info.png", width = 10, height = 5)

info_blocked <- info_combined |>
  group_by(itemtype, item_group, theta) |>
  summarise(info = sum(info), n = n()) |>
  ungroup()

ggplot(info_blocked, aes(x = theta, y = info)) +
  facet_grid(vars(itemtype)) +
  geom_vline(xintercept = 0, colour = "#EBEBEBFF") +
  # geom_line(aes(colour = angle)) +
  geom_line(aes(colour = item_group)) +
  scale_colour_ptol() +
  # scale_colour_viridis_d(direction = -1) +
  labs(x = "Theta", y = "Item information (block sum)", colour = "Block") +
  theme(legend.position = "top")
ggsave("mrot_info_blocks.png", width = 7, height = 5)
