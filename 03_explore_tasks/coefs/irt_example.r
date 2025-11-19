source(here("02_score_data/01_scoring_prep/scoring_prep_helpers.R"), chdir = TRUE)

trial_data <- read_rds(here(glue("trial_data.rds")))
# trial_data needs to have
# item_task ("hf" or whatever)
# site (or whatever group argument should be)
# rt_numeric (milliseconds)
# correct (true/false)
# run_id
# item_uid
# item_group (hearts, flowers, heartsflowers)
# item (heart, flower)
# trial_number (sequential index 1 through k per run)
# chance (e.g. 0.25)

trial_data_filtered <- trial_data |>
  recode_hf()

# trial_data_filtered should have item_uid values for hf:
# hearts_heart_start         
# hearts_heart_stay          
# flowers_flower_start       
# flowers_flower_stay        
# heartsflowers_heart_start  
# heartsflowers_flower_start 
# heartsflowers_heart_stay   
# heartsflowers_flower_switch
# heartsflowers_heart_switch 
# heartsflowers_flower_stay 

task_data_nested <- trial_data_filtered |>
  nest(data = -c(item_task, site))

source(here("02_score_data/02_fit_irt/irt_helpers.R"))
source(here("02_score_data/02_fit_irt/irt_modular.R"), chdir = TRUE)
regdir <- here("02_scoring_outputs", "model_registry")

# define set of multigroup models (Rasch/2PL x invariance)
models_multigroup <- tribble(
  ~nfact, ~itemtype, ~invariance,
  1,   "Rasch", "configural",
  1,   "Rasch", "scalar",
  1,     "2PL", "configural",
  1,     "2PL", "metric",
  1,     "2PL", "scalar",
)

# define set of pooled models (Rasch/2PL)
models_pooled <- tribble(
  ~nfact, ~itemtype,
  1,   "Rasch",
  1,   "2PL"
)

# fit models for a single site
fit_task_models_pooled(task_data = task_data_nested,
                       models = models_pooled,
                       task = "hf", # or "mg", values in item_task column
                       subset_var = site,
                       subset_val = "siteA",
                       registry_dir = regdir)

# fit all multigroup IRT models to task_data_irt
fit_task_models_multigroup(task_data = task_data_nested,
                           models = models_multigroup,
                           task = "hf", # or "mg", values in item_task column
                           group = site, # or school or whatever
                           registry_dir = regdir)
