data_filtered <- sds_collapsed |> rlevante:::dedupe_items() |> #|> rlevante:::remove_no_var_items()
  mutate(match_k = as.integer(str_sub(item_group, 1, 1))) |>
  group_by(item_inst) |> #summarise(nc = n_distinct(correct)) |> count(match_k, nc)
  filter(match_k %in% c(2, 3) & n_distinct(correct) == match_k + 1 |
           match_k == 4 & n_distinct(correct) == match_k) |>
  # filter(n_distinct(correct) > 1) |>
  ungroup() |>
  arrange(item_uid)
  
data_prepped <- rlevante:::to_mirt_shape(data_filtered)

# itemtype <- 
model_str <- generate_model_str_numeric(data_filtered, data_prepped,
                                        item_type = "Rasch", f = 1)
                                        # priors = priors)

sds_pcm_pars <- mirt(data_prepped, itemtype = "Rasch", pars = "vals") |> as_tibble()
sds_pcm_pars |> filter(str_detect(name, "^d")) |> distinct(name)


# sds_pcm <- mirt(data_prepped, itemtype = "Rasch")
sds_pcm <- mirt(data_prepped, itemtype = "Rasch", model = mirt.model(model_str)) #, SE = TRUE)
sds_pcm_coefs <- coef(sds_pcm, IRTpars = TRUE) |>
  discard_at("GroupPars") |>
  map(as_tibble) |>
  # map(\(co) co |> as_tibble(rownames = "stat")) |>
  bind_rows(.id = "item") |>
  # select(item, starts_with("d"))
  select(item, starts_with("b"))

sds_pcm_coefs |>
  mutate(ordered_1_2 = b1 < b2, ordered_2_3 = b2 < b3) |>
  # filter(!ordered_1_2)
  count(ordered_1_2, ordered_2_3)

sds_par_vals <- sds_pcm_coefs |>
  rename(item_inst = item) |>
  left_join(data_filtered |> distinct(item_inst, item_uid, item_group)) |>
  select(-item_inst) |>
  distinct() |>
  pivot_longer(cols = starts_with("b"), names_to = "par", values_to = "val") |>
  filter(!is.na(val))

ggplot(sds_par_vals, aes(x = val, y = item_uid)) +
  facet_grid(vars(item_group), scales = "free", space = "free") +
  geom_point(aes(colour = par))

pdf("sds_gpcm_trace.pdf", width = 8, height = 5)
plot(sds_pcm, type = "trace", main = "",
     # which.items = which(str_detect(colnames(data_prepped), "2match")),
     which.items = which(str_detect(colnames(data_prepped), "-1$")),
     auto.key=list(points=FALSE,lines=TRUE, columns=4))
dev.off()

pdf("sds_gpcm_infotrace.pdf", width = 8, height = 5)
plot(sds_pcm, type = "infotrace", main = "",
     # which.items = which(str_detect(colnames(data_prepped), "2match")),
     which.items = which(str_detect(colnames(data_prepped), "-1$")),
     auto.key=list(points=FALSE,lines=TRUE, columns=4))
dev.off()

# base_items <- which(str_detect(colnames(data_prepped), "-1$"))
# base_items <- str_detect(colnames(data_prepped), "-1$")
base_items <- colnames(data_prepped) |> keep(\(n) str_detect(n, "-1"))
theta <- matrix(seq(-6,6, by = .1))
sds_pcm_info <- base_items |>
  map(\(item) tibble(theta = theta[,1], info = iteminfo(extract.item(sds_pcm, item), theta), item = item)) |>
  list_rbind() |>
  mutate(item_group = str_extract(item, "[0-9]match"))

# sds_pcm_info_cats <- base_items |>
#   map(\(item) tibble(theta = theta[,1], info = iteminfo(extract.item(sds_pcm, item), theta, total.info = FALSE), item = item)) |>
#   list_rbind() |>
#   mutate(item_group = str_extract(item, "[0-9]match"))

ggplot(sds_pcm_info, aes(x = theta, y = info, colour = item_group)) +
  # facet_wrap(vars(item_group)) +
  geom_line(aes(group = item)) +
  scale_colour_ptol() +
  labs(x = "Theta", y = "Item information", colour = "Item type")
ggsave("sds_gpcm_info.png", width = 8, height = 5)

sds_pcm_prob <- probtrace(sds_pcm, theta) |> as_tibble() |> mutate(theta = theta[,1], .before = everything()) |>
  pivot_longer(cols = -theta, names_to = c("item", "category"), names_pattern = "(.*?)\\.(.*)", values_to = "p")

sds_pcm_prob |>
  filter(str_detect(item, "-1$")) |>
  mutate(item_group = str_extract(item, "[0-9]match")) |>
ggplot(aes(x = theta, y = p, colour = category)) +
  facet_wrap(vars(item), ncol = 4) +
  geom_line(aes(group = category)) +
  scale_colour_ptol() +
  labs(x = "Theta", y = "Probability", colour = "Response") +
  theme(legend.position = "top")
ggsave("sds_gpcm_occ.png", width = 10, height = 6)


sds_bytrial <- model_from_record(sds_rasch_f1_scalar)

bytrial_items <- extract.mirt(sds_bytrial, "itemnames") |> keep(\(n) str_detect(n, "-1$") & str_detect(n, "match"))
bytrial_group <- extract.mirt(sds_bytrial, "groupNames")[[1]]
sds_bytrial_group <- extract.group(sds_bytrial, bytrial_group)

bytrial_info <- bytrial_items |>
  map(\(item) tibble(theta = theta[,1], info = iteminfo(extract.item(sds_bytrial_group, item), theta), item = item)) |>
  list_rbind() |>
  mutate(item = item |> str_remove("-1$")) |>
  separate(item, c("task", "block", "trial_type", "trial_status"), remove = FALSE)
  
ggplot(bytrial_info, aes(x = theta, y = info, colour = trial_type)) +
  facet_wrap(vars(block)) +
  geom_line(aes(group = item, linetype = trial_status)) +
  scale_colour_ptol() +
  labs(x = "Theta", y = "Item information", colour = "Item type")
ggsave("sds_bytrial_info.png", width = 8, height = 3)

bytrial_info |> filter(trial_type == "choice1") |>
ggplot(aes(x = theta, y = info, colour = block)) +
  # facet_wrap(vars(block)) +
  geom_line(aes(group = item)) +
  scale_colour_ptol() +
  labs(x = "Theta", y = "Item information on choice1", colour = "Item type")
ggsave("sds_bytrial_info_choice1.png", width = 6, height = 4)

