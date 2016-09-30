<div id="devex-badge">
<a rel="Delivery" href="https://github.com/BCDevExchange/docs/blob/master/discussion/projectstates.md"><img alt="In production, but maybe in Alpha or Beta. Intended to persist and be supported." style="border-width:0" src="http://bcdevexchange.org/badge/3.svg" title="In production, but maybe in Alpha or Beta. Intended to persist and be supported." /></a>
</div>
---


# Status of Invasive Species in B.C.

A set of R scripts to populate an indicator on status of invasive species in B.C. These scripts reproduce the graphs and maps published on [Environmental Reporting BC](http://www.env.gov.bc.ca/soe/indicators/plants-and-animals/invasive-species.html) in September 2015.

### Usage

#### Data
The data used for the indicator is available from the [BC Data Catalogue](https://catalogue.data.gov.bc.ca/dataset?download_audience=Public):

- [BC Species and Ecosystems Conservation Status Information](https://catalogue.data.gov.bc.ca/dataset/d3651b8c-f560-48f7-a34e-26b0afc77d84) is available under the
[Open Government License - BC](http://www2.gov.bc.ca/gov/content/governments/about-the-bc-government/databc/open-data/open-government-license-bc)
- [Aquatic Invasive Species of British Columbia](https://catalogue.data.gov.bc.ca/dataset/d9613096-b2fe-43b4-9be1-d82a3b805082) is available under the [Open Government License - BC](http://www2.gov.bc.ca/gov/content/governments/about-the-bc-government/databc/open-data/open-government-license-bc)
- [Invasive Alien Plant Site](https://catalogue.data.gov.bc.ca/dataset/10ecf9ad-1555-4043-834a-f5d24a506d59) is available under the [Access Only](http://www2.gov.bc.ca/gov/content?id=1AAACC9C65754E4D89A118B875E0FBDA) license.


####Code
There are 3 sets of R scripts that are required for the indicator. The 'aquatics' and 'plants' scripts need to be run in order:

(1) species_counts.R

(2) aquatics
- 01_load.R
- 02_clean.R
- 03_analysis.R
- 04_output.R

(3) plants
- 01_load.R
- 02_analysis.R
- 03_output.R

The `run_all.R` script can be `source`ed to run it all at once.

Most packages used in the analysis can be installed from CRAN using `install.packages()`, but you will need to install [envreportutils](https://github.com/bcgov/envreportutils) and [bcmaps](https://github.com/bcgov/bcmaps) using devtools:


```r
install.packages("devtools") # If you don't already have it installed

library(devtools)
install_github("bcgov/envreportutils")
install_github("bcgov/bcmaps")
```
[Mapshaper](https://github.com/mbloch/mapshaper)  is used to simplify polygons for display of results. Mapshaper is a Node library, so you need to have [Node](https://nodejs.org/download/) installed first, then install mapshaper on the command line with: ['npm install -g mapshaper'](https://github.com/mbloch/mapshaper/wiki/Command-Reference).


### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/<repo-name>/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

    Copyright 2016 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at 

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
