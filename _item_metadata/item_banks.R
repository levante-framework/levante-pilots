library(googlesheets4)

item_bank_id <- "1MlU4eOd45XVMg7HrnTDGZ3rv1cfNjvjpdc8e_edQqQk"

corpus_sheets <- c(
  "vocab",
  "mental-rotation",
  "adult-reasoning",
  "TROG-full",
  "matrix-reasoning-Mars-IB-v2",
  "matrix-reasoning-Mars-IB-retest",
  "math",
  "math_retest",
  "theory-of-mind-v2",
  "theory-of-mind_retest-A",
  "CO_theory-of-mind_retest-A",
  "theory-of-mind_retest-B",
  "CO_theory-of-mind_retest-B",
  "child-survey",
  "same-different-selection-v5"
)

corpus_list <- corpus_sheets |> set_names() |> map(\(s) read_sheet(item_bank_id, sheet = s, na = c("", "NA"), col_types = "c"))

num_cols <- c("block_index", "chance_level", "d", "time_limit", "a", "g", "u",
              "difficulty", "trial_num", "required_selections", "num_cards", "num_dimensions")

corpus_df <- corpus_list |>
  bind_rows(.id = "item_bank_sheet") |>
  mutate(across(all_of(num_cols), as.numeric)) |>
  group_by(item_bank_sheet) |>
  mutate(row = row_number(), .after = item_bank_sheet) |>
  ungroup()

write_csv(corpus_df, here("_item_metadata/item_banks.csv"), na = "")
