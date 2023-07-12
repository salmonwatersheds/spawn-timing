# Estimating spawn timing distributions by CU

## Data
The data used to fit the model are from NuSEDS:

* *Fish arrival*: the period when the fish first arrive in the water body until >5% of the fish have arrived in the stream.

* *Spawning start date*: the period when fish are beginning to pair on the spawning grounds, schools of fish may be holding (in pools or at the mouth of the stream/river) and there are very few, if any, carcasses or redds. Fish are generally in the lower sections of the normal spawning area and may be bright with no fungus and have no white-coloured, eroded fins.

* *Spawning peak date*: the period when the majority of the fish present are paired and actively spawning with few fish holding. The fish may have fungus or white-coloured, eroded fins. A significant proportion of the spawning grounds should have evidence of redds and the fish should generally be distributed throughout the spawning area.

* *Spawning end date*: the period when very few fish are on the spawning grounds, few unspawned fish are holding and there are lots of carcasses. The remaining fish will likely occupy the upper reaches of the spawning area.

## Start/End data summaries
To estimate the spawning window for each CU, the simplest approach does not fit a timing curve or make assumptions about the shape of this curve, but simply averages data among years for a given location and among locations within a given CU. At each stage, data quality is noted based on the number of years of data for which the start or end date is within the sampling dates for that population (under the assumption that if a start date is outside the sampling dates, there was some guesswork involved in estimating that start date and it is less reliable). See `start-end-data-summaries.R` for details.

## Simple model

The simple model does not include any covariates, but simply estimates the mean and standard deviation in the spawn timing distribution for each Conservation Unit (CU), drawing from regional hyper-parameters. Mean arrival time (but not the standard deviation) is also estimated in the same way.

For a timing observations of population $i$ in year $t$, the likelihood is calculated assuming the following distributions:

$$ \text{arrival}_{i,t} \sim \mathcal{N} \left( \alpha_{CU_i} \, \upsilon_1 \right)$$

$$ \text{start}_{i,t} \sim \mathcal{N} (\mu_{CU_i} - q \times \sigma_{CU_i} \, \upsilon_1) $$

$$ \text{peak}_{i,t} \sim \mathcal{N} (\mu_{CU_i} \, \upsilon_1) $$

$$ \text{end}_{i,t} \sim \mathcal{N} (\mu_{CU_i} + q \times \sigma_{CU_i} \, \upsilon_1) $$

where $q$ is 97.5% quantile of the standard normal distribution (i.e., 1.96) such that the start and end dates are assumed to be the 2.5 and 97.5 quantiles of the spawn timing distribution, allowing for the estimation of $sigma_{CU_i}$. 

The CU-specific parameters, $\mu_{CU_i}$, $\sigma_{CU_i}$, and $\alpha_{CU_i}$ are drawn from regional hyperparameters:

$$\mu_{CU_i} \sim \mathcal{N} \left(\mu_{R_i} \, \upsilon_2 \right)$$

$$\sigma_{CU_i} \sim \mathcal{N} \left(\sigma_{R_i} \, \upsilon_2 \right)$$

$$\alpha_{CU_i} \sim \mathcal{N} \left(\alpha_{R_i} \, \upsilon_2 \right)$$

The variances in the distributions $\upsilon_1$ and $\upsilon_2$ are not estimated from the data but assumed to be some small number such that the fit of the CU-specific parameters is given greater weight than the regional hyperparameters ($\upsilon_1 = 0.1 < \upsilon_2 = 1$).

The spawn timing distributions for each CU can then be taken from the fitted parameters $\mu_{CU}$ and $\sigma_{CU}$.

## Covariates model

Given that spawn timing information is not available for all CUs, we aim to use the fitted model to predict spawn timing for those CUs with no data. In the simple model, this could be achieved by assuming the spawn timing of the regional hyperparameters when no CU-specific data exist. However, the regions are large areas that may encompass a wide range of spawn timing even within species, and thus the regional hyperparameters may not be very meaningful.

To increase the accuracy of our predictions of spawn timing to data-deficient CUs, we also fitted a model that included several covariates known to influence spawn timing:
* latitude, with the hypothesis that populations at higher latitudes spawn earlier;
* elevation, with the hypothesis that populations at higher elevations spawn later;
*distance to ocean, with the hypothesis that populations further from ocean entry will spawn later.

These covariates were calculated for the latitude and longitude associated with each NuSEDS population, usually corresponding to the mouth of the river or stream where spawners are counted. 

The model had a similar structure to the simple model described above, except that the means for the distributions of arrival, start, peak, and end of spawning were a linear function of the covariates, as well as the hierarchical components. The likelihood distributions were:
$$ \text{arrival}_{i,t} \sim \mathcal{N} \left( \bf{\beta_1 X} +  \alpha_{CU_i} \, \upsilon_1 \right)$$

$$ \text{start}_{i,t} \sim \mathcal{N} (\bf{\beta_2 X_i} + \mu_{CU_i} - q \times \sigma_{CU_i} \, \upsilon_1) $$

$$ \text{peak}_{i,t} \sim \mathcal{N} (\bf{\beta_2 X_i} + \mu_{CU_i} \, \upsilon_1) $$

$$ \text{end}_{i,t} \sim \mathcal{N} (\bf{\beta_2 X_i} + \mu_{CU_i} + q \times \sigma_{CU_i} \, \upsilon_1) $$

where $\bf{\beta_1}$ is a vector of slopes describing the relationship between arrival time and covariates $\bf{X_i}$ for stream $i$, and $\bf{\beta_2}$ is a vector of slopes describing the relationship between peak spawn time and covariates $\bf{X_i}$.

### Predicting to data-deficient CUs
The timing covariates $\bf{X}$ are for specific NuSEDS populations and not CUs. So how do we use the model above to predict to CUs?

1. Use the mean latitude, elevation, and distance to ocean across all spawning locations in the CU?
2. Use the latitude corresponding to the centroid of the CU (and what for elevation and distance to ocean?)

Either of these options may be inaccurate for  CUs that spawn over large geographic areas (e.g., pink and chum CUs), but in those cases in may not make sense to summarize average timing since there can be considerable variability wihtin a CU?? 

For the CUs that DO have data, it seems most accurate to use the simple model which estimates the average arrival and spawn timing of the CU directly, without having to make such assumptions.
