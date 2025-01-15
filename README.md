# Westphalia-II

This project aims to analyze the relationship between climate variability and stress on the international state system. 

Predictor (independent) variables are defined as absolute deviation in current (1982-2023) annual temperature and precipitation measurements from a baseline time period of 1950-1980. See temp_indermediate_files for original temperature data imported from ArcGIS Pro (raw data files downloaded from Copernicus Climate Data Store too large for import), and subsequent manipulations as the data was processed in R. See precip_intermediate_files for manipulations as the raw precipitation data (in raw_data_files) was processed in R. For information on data sources, see predictor_var_source_information.

The outcome (dependent) variable is defined as a state's problem solving capacity, determined by a novel index of seven indicators: credibility, basic administration, monopoly on the use of force, taxation, banking system, welfare regime, and access to basic water services.

final_indicator_files houses individual .csv files for each predictor after they were calculated in R, and added to values_for_regression.csv along with final state problem solving scores. 

Our original list of 179 states was updated and trimmed to include only the 163 for which we obtained data, and these lists can be seen in their respective .csv files. 
