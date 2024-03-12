#############################
### 0. Create Directories ###
#############################

# create data directory
data_dir <- dir.create("data")

# designate subdirectories
data_subdirectories <- c("a_raw_data",
                         "b_intermediate_data",
                         "c_submodel_data",
                         "d_suitability_data",
                         "e_rank_data",
                         "f_sensitivity_data",
                         "g_uncertainty_data",
                         "zz_miscellaneous")

# designate submodel directories
data_submodels <- c("national_security",
                    "industry_navigation",
                    "fisheries_aquaculture",
                    "natural_resources",
                    "cultural_resources",
                    "metocean_other")

#####################################

# create sub-directories within data directory
for (i in 1:length(data_subdirectories)){
  subdirectories <- dir.create(paste0("data/", data_subdirectories[i]))
}

# create submodels within raw data directory
for (i in 1:length(data_submodels)){
  submodels <- dir.create(paste0("data/a_raw_data/", data_submodels[i]))
}

#####################################

# create code directory
code_dir <- dir.create("code")

# designate submodel code subdirectories
code_submodels <- c("00_national_security",
                    "00_industry_navigation",
                    "00_fisheries_aquaculture",
                    "00_natural_resources",
                    "00_cultural_resources",
                    "00_metocean_other")

# create submodels within code directory
for (i in 1:length(code_submodels)){
  submodels_code <- dir.create(paste0("code/", code_submodels[i]))
}

#####################################

# create figure directory
figure_dir <- dir.create("figure")

#####################################
#####################################

# delete directory (if necessary)
### ***Note: change directory path for desired directory
#unlink("data/a_raw_data", recursive = T)
