# DFS Landscape Dashboard

# To-do

## Current status

This application is relatively simple on the UI side, but fairly complex on the data processing front. `global.R` reads in a very large excel spreadsheet (20 tabs of data in various forms), and combines them all into one "tidy" / "long" dataset. The code in `app.R` serves to allow for visualizatoin of these data. Most of the pending items pertain to (a) dealing with units and currencies and (b) improving visualizations. 

## Tasks

- Replace all the stuff from the finstats file with the finstats 2018 tab here:   https://drive.google.com/drive/folders/15H_r9uz7tecHQaD2rnqPAhXwmbBDoOWA 
- Use unit / unit2 columns from glossary tab in order to match each value with a "unit"
- Incorporate currencies' conversions to put everything in USD.
- Implement download functionality (skeleton already there, but needs to actual generate plot)
- Country analysis custom tab: add possibility for comparative benchmarks
- Country analysis custom tab: improve chart types
- Incorporate new files as per Oleksiy's instructions: https://figssa.slack.com/messages/C8ZP5V2PQ/p1522091189000384


## Developer's guide 

### Data set up

- Download `18-02-17 Africa DFS landscape data tool.xlsx` from google drive and save in the `data` folder (https://drive.google.com/drive/folders/1v7uRVYzbkpthsiUHxJ2exRjStc9-r9iu)
- Run app.
