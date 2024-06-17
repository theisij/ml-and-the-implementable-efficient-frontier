# Overview
This repository contains the code used for the paper [Machine Learning and the Implementable Efficient Frontier](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4187217) by Jensen, Kelly, Malamud, and Pedersen (2024). Please cite this paper if you are using the code:
```
@article{JensenKellyMalamudPedersen2024,
	author = {Jensen, Theis Ingerslev and Kelly, Bryan and Malamud, Semyon and Pedersen, Lasse Heje},
	title = {Machine Learning and the Implementable Efficient Frontier},
	year = {2024}
}
```

Please send questions about the code to Theis I. Jensen at [theis.jensen@yale.edu](mailto:theis.jensen@yale.edu)


# How to run the code
To run the code, clone this repo to your local computing environment, and follow the steps explained below. We note that replicating our analysis requires substantial computational resources, and the code is set up to be executed on a high-performance computing cluster with a SLURM scheduler. 

## Data
You need six data sets to run the code. 
- `usa.csv`
    - What: Stock return and firm characteristics at a monthly frequency from the paper [Is There a Replication Crisis in Finance?](https://onlinelibrary.wiley.com/doi/10.1111/jofi.13249) by Jensen, Kelly, and Pedersen (2023)
    - Where: Download from [WRDS](https://wrds-www.wharton.upenn.edu/pages/get-data/contributed-data-forms/global-factor-data/). To get US data, make require that the column `excntry` is equal to "USA"
- `usa_daily`
    - What: Stock returns at a daily frequency
    - Where: The file can be generated by following the instructions from the [GitHub repository](https://github.com/bkelly-lab/ReplicationCrisis/tree/master/GlobalFactors) from "Is There a Replication Crisis in Finance.'' Alternatively, you can request the data from us by sending an email to [theis.jensen@yale.edu](mailto:theis.jensen@yale.edu)
- `Factor Details.xlsx`
    - What: Information about factor characteristics from "Is There a Replication Crisis in Finance"
    - Where: Download from [GitHub/bkelly-lab/ReplicationCrisis/GlobalFactors/Factor Details.xlsx](https://github.com/bkelly-lab/ReplicationCrisis/blob/master/GlobalFactors/Factor%20Details.xlsx)
- `Cluster Labels.csv`
    - What: Information about factor characteristics from "Is There a Replication Crisis in Finance"
    - Where: Download from [GitHub/bkelly-lab/ReplicationCrisis/GlobalFactors/Cluster Labels.csv](https://github.com/bkelly-lab/ReplicationCrisis/blob/master/GlobalFactors/Cluster%20Labels.csv)
- `market_returns.csv`
    - What: Market returns from "Is There a Replication Crisis in Finance"
    - Where: Available on [Dropbox](https://www.dropbox.com/sh/xq278bryrj0qf9s/AABUTvTGok91kakyL07LKyQoa?dl=0)
- `ff3_m.csv`
    - What: The Fama-French 3-factor model data (used to get risk-free rate)
    - Where: Download at [https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip](https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip)
- `short_fees`
    - What: Short-selling fees. You can run the vast majority of the code without this data set (the exception being `6 - Short selling fees.R`)
    - Where: Based on the Markit Securities Finance Analytics - American Equities data base, which can be downloaded from [WRDS](https://wrds-www.wharton.upenn.edu/pages/get-data/markit/markit-securities-finance-analytics-equities/american-equities/)


These data sets should be saved in the `Data` folder, with the exact names used above. 

Running 