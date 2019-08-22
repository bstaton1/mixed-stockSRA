# Age Composition Data Preparation

This subdirectory stores the code and data used to summarize the age data from raw format to annual proportional contributions of adults of each age (4, 5, 6, and 7) to the run for each population/year with age data. Only weir-monitored populations have had age composition data collected. All data can be found on the [AYKDBMS](<http://www.adfg.alaska.gov/CommFishR3/WebSite/AYKDBMSWebsite/Default.aspx>), managed by the Alaska Department of Fish and Game.

A temporally-weighted average age composition was calculated for each population/year combination. If different ages return at different parts of the season and age sampling is not spread evenly across the season, this could introduce biases. To correct for this possibility, the age samples were divided into two-week strata, and then averaged weighted by the number of fish that passed the weir in each stratum. 

An optional shell script is provided (`0a-run-analysis.sh`) which when executed will run the whole data preparation analysis. Otherwise, the R scripts can be executed in sequence.

