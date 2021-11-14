
## Notes on building mixed-models in R
_Stemming from reviewing package docs, forums, and convos with A. Baecher_  


### Describing the model 

- If the distribution of the response variable is not gaussian, then a GLMM is more appropriate than an LMM. In which case use glmer rather than lmer
- Because some of the independents are correlated with each other (e.g. micro with biome, because canopy only in forest) it might be appropriate to nest
- In lmer(), fixed effects are listed as themselves after the ~, and random effects are within parans with a matrix (which can be == 1) on left-hand of |
	- e.g. (response ~ fixed1 + fixed2 + (1|random1) + (1|random2) + (1|random1/nestedrandom3))
- All environmental covariates are fixed, and site is random
- A random effect has to be categorical, and has to be represent some broader gradient of sorts. Think of a random effect as a sample from a population  

### Questions for A. Baecher

1. "We modeled Y at the site level" does this mean site is a random effect?  
YES. Think of each level is a different intercept, through which observations can be strung together  

2. Model selection: AIC is what ecologists tend to do...any point in interpreting the p-value?  

The p-value in anova.glm() appears to effectively be like th AIC_delta in the other function. Let's go with the function that Baecher used, aictab()  

3. Q: I could avg each year to get avg overlap per day/month for the time series, OR use a full time series and have year-of-sample as a random effect, right?
A: Not really. You can string together years from the same site, but you can't really string together different observations within the same year....so therefore a year doesn't really make sense as an intercept for the model, and therefore wouldn't really be an appropriate random effect.  


### Some notes on syntax:  
- Adding things with a + makes it a covariate. If it's not continuous, it is treated as a fixed effect. 
- If it's in parans with a 1 (1 | x), it's a random effect. NOT everything is appropriate as a random effect.
- This is bad syntax
`linear_latitude1 <- lmer(TAD_elevCorr ~ (1 | latitude:micro), data = mountains)`  

- This is just looking at nesting of micro in latitude  
`linear_latitude2 <- lmer(TAD_elevCorr ~ latitude + (1 | latitude:micro), data = mountains)`  

- This is nesting of micro in latitude, and latitude by itself  
`linear_latitudeMicro <- lmer(TAD_elevCorr ~ latitude + (1 | latitude/micro), data = mountains)`  

- This is exact same as above: nesting of micro in latitude, and latitude by itself  
`linear_latitude4 <- lmer(TAD_elevCorr ~ latitude + (1 | latitude) + (1|latitude:micro), data = mountains)`  


### Data corrections and adjustments:
- Vegetation structure indices (veg_structure) are not temporally explicit, i.e. there is only one value for each mountain (avg of values from low and high sites), across the whole timeseries sampled. To accomodate for seasonal flux of foliage cover, in deciduous systems during leaf-off days (from pheno data), veg_structure value were reduced proportionate to estimates (from site photos) of coniferous vs deciduous cover for each site. As in, photos of Mt Mitchell suggest ~30% coniferous cover. So, the veg_structure value for NC was multiplied by 0.2.
- All continuous covariates were scaled from 0 to 1.
