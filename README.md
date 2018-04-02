Houston Crime Explorer
----------------------

This app is not available online, yet, but you can run this app yourself.

To run the app, you will need to:

<ol>
<li>
[Install R](https://cran.r-project.org/)
</li>
<li>
[Install RStudio](https://www.rstudio.com/products/rstudio/download/#download)
</li>
<li>
[Install Git](https://git-scm.com/downloads)
</li>
<li>
Clone this project: `git clone https://github.com/seasmith/HoustonCrimeViewer HoustonCrimeViewer`
</li>
<li>
Install `devtools` and `packrat`: `install.packages(c("devtools", "packrat"))`
</li>
<li>
Install requisite packages: `packrat::install_deps()` (the `DESCRIPTION` file exists for this purpose, this repo is **not** a package)
</li>
<li>
Execute `shiny::runApp()`
</li>
</ol>
Please create an issue if something does not go as planned.

Future Versions
---------------

### Definitive Additions

Some things which **will** eventually be added:

-   `plotly` maps.
    -   Lasso-selection
    -   Multi-selection
    -   Satellite imagery
-   Start- and end-year range selection.
    -   Currently, the server gets hung up when the slider is used. May have to resort to using another dropdown menu.
-   School district polygons. This may make the map look a little messy, so this may be placed in its own separate map or as a static report per school district.
-   Toggle certain beats that skew the crime rates. Right now, this is pre-set (the grayed-out polygons).
-   Select which variable to map. Currently `rate` is used, but others may include:
    -   `n_offenses` (number of offenses)
    -   `n_offenses / pop_den` (number of offenses divided by population density)

### Probable Additions

Some things which **may** be added:

-   Address input. Ideally, a user should be able to enter their address to highlight the police beat in which the live.
-   Geocoding all crimes. There are hundreds of thousands of unique block ranges (block ranges are the addressing method used by the Houston Police Department). Focus may be prioritized on recent years, starting with 2017.
-   Census data. This may include income, age, race, and other data available through the census.
-   Vega/Vega-lite graphics.

Data sources
------------

| Type            | Source                                                                                     |
|-----------------|--------------------------------------------------------------------------------------------|
| Crime data      | [City of Houston](http://www.houstontx.gov/police/cs/crime-stats-archives.htm)             |
| Police beats    | [City of Houston](https://cohgis-mycity.opendata.arcgis.com/datasets/houston-police-beats) |
| Population data | [NASA SEDAC](http://sedac.ciesin.columbia.edu/data/collection/gpw-v4)                      |
