# Seabird Risk Assessment [![Build Status](https://travis-ci.org/dragonfly-science/seabird-risk-assessment.svg?branch=master)](https://travis-ci.org/dragonfly-science/seabird-risk-assessment)

This is an example of carrying out a Seabird Risk Assessment, following the
Spatially Explicit Fisheries Risk Assessment framework [(SEFRA; see Chapter 3 of
this
PDF)](https://www.mpi.govt.nz/dmsdocument/16339-aquatic-environment-and-biodiversity-annual-review-aebar-2016-a-summary-of-environmental-interactions-between-fisheries-and-the-aquatic-environment).
Within the SEFRA framework, seabird fatalities in fisheries are assumed to be
proportional to both the fishing effort, and the seabird density (birds per
square kilometer). The constant of proportionality is the vulnerability, and may
be estimated from observer data.

The fisheries and observer data used here are simulated. This project acts as a
template for carrying out a risk assessment analysis, showing the format of
input files and the process needed for carrying out the assessment.  The
assessment is carried out on simulated on seabird captures, in commercial
longline fisheries in the southern hemisphere, assuming a known vulnerability.
Only the great albatrosses group  (_Diomedea_) are included.

The risk assessment prepares input data, runs a Bayesian model,
and carries out simple analysis and display of the results.

## Process

To run this test example, clone the project from Github (`git clone https://github.com/seabird-risk-assessment/seabird-risk-assessment.git`).
If you have [docker](https://www.docker.com) installed on an Ubuntu system, you can then run `make` to run the
test project. A docker image containing all the necessary software is available from [DockerHub](https://hub.docker.com/r/dragonflyscience/seabird-risk-assessment/).

Otherwise, install the  following software: 

### Software requirements

* [R](https://cran.r-project.org/), with the packages `data.table`, `lubridate`, `sf`, `rjags`, `coda`,
  `stringr`, `ggplot2` to process the data, orchestrate the model, and to get the
  results; and `rmarkdown`, `knitr`, `kableExtra`, and `shinystan` for the viewing the results and generating the
  report.  
     * ggpplot2: Currently, a development version of `ggplot2` is required to plot the maps. 
     First, install the `devtools` package, then run `devtools::install_github("tidyverse/ggplot2")`
     * sf: The current vesrion of `sf` has a bug that prevents the maps from being generated. Install
     the development version to avoid this bug: `devtools::install_github("r-spatial/sf")`
* [Stan](http://mc-stan.org/), to run the Bayesian model
* [GNU make](https://www.gnu.org/software/make/), is not necessary but
  highly recommended to run the whole project at once. Windows users
  can get GNU make along with other common programs part the UNIX-based
  environment by installing the R package
  [Rtools](https://cran.r-project.org/bin/windows/Rtools/). The
  installation guide can be found [here](https://github.com/stan-dev/rstan/wiki/Install-Rtools-for-Windows).
* [Pandoc](https://pandoc.org) is required to compile the
  report. This should be pre-installed with R-studio.

This project has been tested in Ubuntu 16.04.

### Running the risk assessment

If you have `GNU make` installed, you can run the entire project by
running `make` in the base directory of the project. This can be run from within the docker image. Before running `make` ensure that the environment variable `RUN` is not set. In a Linux system (such as within the docker container), this can be achieved by running `RUN= make`. 

The relationship
between the  individual scripts is shown in 
[build diagram](graph-makefile.pdf). If you don't have `make` available you
can follow the dependencies shown in that digram.

When the assessment is complete, there will be a report `model-report.html` in each of the model directories, e.g, in the `model/stan` directory.

## Data

### Spatial grid

A spatial grid is provided in the
[input-data/southern-hemisphere-5-degree](input-data/southern-hemisphere-5-degree)
directory. This grid is 5-degree by 5-degree, covering the southern hemisphere.
It is provided as a shapefile (in WGS84 latitude-longitude projection) that can
be loaded into GIS tools, or queried from software such as R.

The grid has been clipped to coastlines used by the American Bird Conservancy in
the development of their seabird distributions. For each cell, the shape file
has the following fields:

* grid_id	: integer index, unique for each grid cell
* lon		: longitude of the center of the cell
* lat		: latitude of the center of the cell
* area		: area of each cell in kilometers squared

Note that a grid is not necessarily required by the SEFRA method, however it is
convenient to have a common resolution for preparing the data.


### Fisheries effort data

Observed and total fishing effort. 

For both observed and total fishing effort, a csv file
([input-data/fishing-effort-observed.csv](input-data/fishing-effort-observed.csv)
or
[input-data/fishing-effort-total.csv](input-data/fishing-effort-total.csv)
respectively) is provided with the following fields:

* fishery_group : Fishery group; single fishery or group of fisheries. Fisheries of
  the same group are assumed to share the same vulnerability to capture.
* year			: 4-digit year
* month			: 1 to 12 (month of the year)
* grid_id		: index to the spatial grid
* effort		: annual effort (the effort measure may be different between
  fisheries, for surface longline fisheries, number of hooks is a
  standard measure)


In
this example, the fishery group was taken as the flag state of the countries
with the highest fishing effort, with smaller fisheries grouped as `other`.
The data may include other columns (such as RFMO, flag state, data source). 

The fishing effort was based on publicly available data from RFMOs, but has been
randomised, and so does not represent actual fishing effort. Five percent of the
fishing effort in each cell was assumed to be observed.

### Seabird data

#### Studied species and groupings

To identify seabird species, use the three-letter FAO codes from the
[ASFIS database](http://www.fao.org/fishery/collection/asfis/en).  An
extract from this data for seabird species is in
[data/birds/fao_birds.csv](data/birds/fao_birds.csv). 
The codes are
intended to allow captures to be linked to the corresponding taxonomic
data.  The FAO do not include all seabirds, and so custom codes may be
needed.

The list of species considered in the study is in
[input-data/species-list.csv](input-data/species-list.csv), with the
code, scientific name, and common name of each taxon. This is also
where the species groups are defined, with taxa of the same group
sharing the same `vul_group` code. Taxa of the same group are assumed
to have the same vulnerability to capture.

Some seabird captures are not recorded at the species level but at a
higher taxonomic level, because of identification issues.  
These  groups of species  feature in the species list, with the hierarchy between taxa 
defined by specifying the parent code of the higher taxonomic
level.  Currently, only one group of unidentified species is used in
the model. The column `base_species` indicates the taxa for which the
estimation of captures and the reporting are carried out (`TRUE` for
reported taxa, `FALSE` otherwise)


#### At-sea distributions

A polygon shapefile for each taxon
(e.g. [input-data/species-distributions/Diomedea_amsterdamensis.shp](input-data/species-distributions/Diomedea_amsterdamensis.shp)),
in the ESRI format, with the same geometries as in the generic grid,
and with the following attributes:
* grid_id : index to the spatial grid
* species_code : code of the taxon
* taxa : scientific name of the taxon
* density : bird density within each grid polygon, normalised so that
  the product of density and the area (in km^2) of each polygon sums
  to one across all polygons. When multiplied by the total population
  size, the density is in birds per km^2

These shapefiles are from the [American Bird Conservancy](http://www.fisheryandseabird.info/).

#### Demographic parameters

A csv file ([input-data/demographic-parameters-processed.csv](input-data/demographic-parameters-processed.csv)),
containing the mean and uncertainty of key demographic parameters used
in the study for each taxa.

The ields are:
* species : species code
* parameter : one of:
	* survival_current		: Current adult annual survival rate, including human-caused mortality
	* survival_optimal		: Optimal adult annual survival rate, which would occur in
	  absence of human-caused mortality. Used for the Population
	  Sustainability Threshold
	* age_first_breeding	: Mean age at first reproduction
	* population_total		: Total number of individuals (typically derived from the number of annual breeding
  pairs)
* mean, sd : mean and standard deviation of the parameter
* mean_log, sd_log : mean and standard deviation of the parameter in
  the log-scale (for log-normal distributions)
* min, max : minimum and maximum value used for truncating the generated
  distributions, to keep the parameters within plausible range.
  

#### Observed captures

A csv file
([input-data/observed-captures.csv](input-data/observed-captures.csv))
with the following fields:

* species		: code for each seabird species
* species_group : code for each seabird species group
* fishery_group : fishery group
* taxa          : scientific name of the species
* year			: 4-digit year of capture
* month			: 1 to 12 (month of the year)
* grid_id		: index to the spatial grid
* captures		: total number of captures (live and dead)
* alive			: number of observed live captures
* dead			: number of observed dead captures

To generate the seabird bycatch data, the
total number of incidents was drawn randomly from a Poisson
distribution, with a mean depending on the overlap between the
fisheries and the species, and the vulnerability coefficients.

The vulnerability intercept was set to 0.05, and for the fishery
group, species group, and the interaction, the vulnerability values
were drawn randomly from a log-normal distribution of mean 0 and
standard deviation of 0.5 (on the log scale). 

From the number of incidents, the number of observable captures, then
the number of live and dead captures, were drawn randomly from a
binomial distribution, assuming a probability of of a capture being
observable was taken to be 0.5, and the probability that a capture was released alive
of 0.3. A proportion (30%) of observable captures were assumed to
be unidentified, and were as unidentified wandering albatross.

The vulnerability and parameter values used to simulate the data can
be found in the folder [test-data](test-data/).


## Model

The Bayesian model to estimate the vulnerability to capture and the
number of annual potential fatalities is currently coded in the 
open source software [Stan](http://mc-stan.org/).

The estimation of vulnerability and the predictions of annual
potential fatalities are carried out simultaneously. The estimation is
done on the observed fishing effort aggregated by species, species
group, and fishery group, summing the fishing effort and the number of
captures over the period 2004 to 2014.  The predictions are carried out on
observed fishing effort (to compare observed and predicted captures),
as well as on the total fishing effort, aggregated by species, species
group, fishery group, grid cell, and month. The aggregation for the
predictions are on data between 2012 and 2014, to reflect recent
fishing effort and distribution, taking the annual average of fishing
effort, so that the predicted captures are also annual.  The periods
of the data used in the aggregation for the estimation or the
prediction stages are specified in the file [parameters.r](parameters.r).

From the MCMC output of the model, a posterior sample of the annual
potential fatalities is returned for each stratum, which can be
summed over strata to get for example the total number of annual
potential fatalities by species, or by fishery group.

The aggregation levels of the data to be used in the model are flexible
and may be changed in the script that prepares the model data
[model/jags/prepare-model-data.r](model/jags/prepare-model-data.r).


## Report

Results are reported and summarised in a R Markdown document in
[report/report.Rmd](report/report.Rmd), generating an HTML report that
can be viewed in any web browser after being compiled by the R package
[knitr](https://yihui.name/knitr).

In particular, the document reports on:
* the diagnostics of the model, including the comparison of observed
  and predicted captures
* the estimated annual potential fatalities (APF) by species
* the spatial distribution of APF
* the comparison of the model results with the fixed parameters that
  were used to simulate the data


## An open project

This project is licensed under the open-source MIT licence, which permits re-use
or adaptation of this code. The seabird distributions provided by the American
Bird Conservancy are not covered by this licence.

You are welcome to make contributions to this project. Note however,
that this is a test risk assessment. Please do not
commit any fisheries or bycatch data to this project.


