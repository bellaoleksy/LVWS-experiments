## Loch Vale Experiments Repository

A repository for the Sky Pond NDS experiment and the TempxNewtz incubation experiment

## Files
* data_clean - folder containing raw data for individual datasets
* figures - a place to put current figures
* figures_archived - a place for old, ugly figs
* archived_Rdocs - folder containing old .R and Rmarkdown files
* `Incubation_Statistics_181004.Rmd` - statistical analyses for TempxNewtz incubation experiment

## Data overview

- `TxN_withdilutions.csv` - clean benthic chlorophyll a & b (UV-Vis)
- `waterchem_warmingxnutrients_171206.csv` - most recent incubation data with water chemistry responses
- `incubationdata_GPP_180523.csv` - most recent file containing GPP/ER assays
- `waterchem&bentho_171206_test` - a file worth revisiting - contains the benthotorch measurements corresponding to each tempxnewt combinations

##Incubation Experiment
*Driver vars:

  - temperature (8, 12, 16)
  - nutrients (control, +n, +p, +np)

*Response vars: 

  - change in nutrients (DOC, NO3, TDN, PO4, NH4)
  - GPP & ER
  
*Covariates

  - starting benthotorch reading??

  
