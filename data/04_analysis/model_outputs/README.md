## Outputs from mixed-modeling of overlap


### Parameters:
- `elevation_change`: the "height" of the mountain, meaning different in altitude between low and high elevation sites (continuous covariate)  
- `latitude`: absolute value of the approximate latitude of the mountain (continuous covariate)    
- `micro`: the microhabitat represented (categorical, fixed effect; removed when micros analyzed separately)  
- `macro`: the macrohabitat (biome) represented (categorical, fixed effect)  
- `foliage`: "leaf-on" or "leaf-off", representing whether foliage is on average present during the day/month of the year (categorical, fixed effect)  
- `veg_structure`: an index representing coarse resolution (1km2) canopy height and tree density (continuous covariate)  
- `snow_depth`: average depth of snow for the day/month of the year, extracted from local NOAA gauges or from weather station queried (continuous covariate)  
- `site`: unique flag for each mountain (categorical, random effect)  


global model:

overlap ~ elevation_change + latitude + micro + macro + foliage + veg_structure + snowdepth + (1|site)


Distribution of overlap:

TAD (_Thermal Absolute Distance_):  
Gaussian (normal)  
min:  -7.362095   max:  24.30717   
median:  6.800833   
mean:  7.329672   
estimated sd:  6.79768   
estimated skewness:  0.3060841   
estimated kurtosis:  2.814885   

Model structure:
Guassian linear mixed model (_Gaussian because TAD overlap scores are normally distributed_)  

- modeling performed at daily and monthly resolution  
- moderling performed for all micros combined as well as each micro separately  



Data corrections and adjustments:
- Vegetation structure indices (veg_structure) are not temporally explicit, i.e. there is only one value for each mountain 
(avg of values from low and high sites), across the whole timeseries sampled. To accomodate for seasonal flux of foliage cover, 
in deciduous systems during leaf-off days (from pheno data), veg_structure value were reduced proportionate to estimates (from site photos) 
of coniferous vs deciduous cover for each site. As in, photos of Mt Mitchell suggest ~30% coniferous cover. 
So, the veg_structure value for NC was multiplied by 0.2.
- All continuous covariates were scaled from 0 to 1.  

### Some thoughts and reflections from model results


At monthly resolution, height (micro) consistently has high magnitude coefficient and signaficant effect (for elevCorr, for positive-transformed, for both TAD and d-scores). Foliage cover is next most important. After that floats between veg structure, latitude, foliage:veg, and maybe snow.  

At daily resolution, height consistently is NOT important (for elevCorr, for positive-transformed, for both TAD and d-scores). Foliage cover and snowdepth appear to be the most important parameters in many of the daily models