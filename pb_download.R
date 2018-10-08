library(piggyback)


# Download files on piggyback

# Boundaries
pb_download("Datasets/Boundaries/UK_2011_Census_Boundaries__MSOA/UK_2011_Census_Boundaries__MSOA.shp", repo= 'PanosTsole87/odmsim/', tag = "v1.0.0")     # Boundaries of UK MSOAs
pb_download("Datasets/Boundaries/UK_2011_Census_Boundaries__Euro_Region/UK_2011_Census_Boundaries__Euro_Region.shp", repo= 'PanosTsole87/odmsim/', tag = "v1.0.0")     # Boundaries of UK Regions
pb_download("Datasets/Boundaries/England_msoa_2011shp/england_msoa_2011.shp", repo= 'PanosTsole87/odmsim/', tag = "v1.0.0")     # Boundaries of England MSOAs 

# Microdata
pb_download("Datasets/Microdata/2011CensusMicrodataTeachingFile.csv", repo= 'PanosTsole87/odmsim/', tag = "v1.0.0") #Initial Microdata file

# Sociodemographic files
pb_download("Datasets/Sociodemographic/Data_AGE_ECOACT_OCCUP_SEX_UNIT.csv", repo= 'PanosTsole87/odmsim/', tag = "v1.0.0") #Initial Age-Sex Census file
pb_download("Datasets/Sociodemographic/Data_AGE_ECOACT_HRSWRK_OCCUP_UNIT.csv", repo= 'PanosTsole87/odmsim/', tag = "v1.0.0") #Initial Hours of Work Census file
pb_download("Datasets/Sociodemographic/Occupation.csv", repo= 'PanosTsole87/odmsim/', tag = "v1.0.0") #Initial Occupation Census file

# OD files
pb_download("Datasets/OD/wu01ew_v2.csv", repo= 'PanosTsole87/odmsim/', tag = "v1.0.0") #initial OD per Sex file before cleaning
pb_download("Datasets/OD/wu02ew_v2.csv", repo= 'PanosTsole87/odmsim/', tag = "v1.0.0") #initial OD per Age file before cleaning
pb_download("Datasets/OD/wu03ew_v2.csv", repo= 'PanosTsole87/odmsim/', tag = "v1.0.0") #initial OD per Mode file before cleaning

pb_download("Datasets/OD/ODsex.csv", repo= 'PanosTsole87/odmsim/', tag = "v1.0.0") #Final OD per Sex file after cleaning
pb_download("Datasets/OD/OD_sexage.csv", repo= 'PanosTsole87/odmsim/', tag = "v1.0.0") #Final OD per Sex-Age file after cleaning
pb_download("Datasets/OD/ODmode_final.csv", repo= 'PanosTsole87/odmsim/', tag = "v1.0.0") #Final OD per Mode file after cleaning
pb_download("Datasets/OD/OD_SexAge_Mode.csv", repo= 'PanosTsole87/odmsim/', tag = "v1.0.0") #Final OD per Sex-Age-Mode file after cleaning

# First IPF
pb_download("Datasets/IPF/weights.csv", repo= 'PanosTsole87/odmsim/', tag = "v1.0.0")     # real numbered weights
pb_download('Datasets/IPF/weights_int.csv', repo= 'PanosTsole87/odmsim/', tag = 'v1.0.0') # integerised weights after TRS
pb_download("Datasets/IPF/ints_df.csv", repo= 'PanosTsole87/odmsim/', tag = "v1.0.0")     # individuals in Spatial Microdata
pb_download("Datasets/IPF/ints_agg.csv", repo= 'PanosTsole87/odmsim/', tag = "v1.0.0")     # Aggregated individuals in Spatial Microdata
pb_download("Datasets/Sociodemographic/cons.csv", repo= 'PanosTsole87/odmsim/', tag = "v1.0.0")     # Sociodemographic Final constraint variables used in the 1st IPF

# Second IPF
pb_download("Datasets/IPF/ints_OD_df.csv", repo= 'PanosTsole87/odmsim/', tag = "v1.0.0") #Integerised individuals with OD after 2nd IPF
pb_download("Datasets/IPF/agg_ints_OD_df.csv", repo= 'PanosTsole87/odmsim/', tag = "v1.0.0") #Aggregated Integerised individuals with OD after 2nd IPF
pb_download("Datasets/IPF/ints_OD_agg1.csv", repo= 'PanosTsole87/odmsim/', tag = "v1.0.0") #Aggregated Integerised individuals with OD after 2nd IPF - Updated Version

# Mode imputation
pb_download("Datasets/IPF/ints_OD_SexAge_Mode.csv", repo= 'PanosTsole87/odmsim/', tag = "v1.0.0") #Integerised individuals with OD and Mode after 2nd IPF


# List all existing files on piggyback
pb_list()






