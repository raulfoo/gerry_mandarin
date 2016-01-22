# gerry_mandarin

Tested using R 3.2.1 on Mac OS 10.10.5

This repo structures the code base for testing election districting schemes against actual, precinct level vote returns in past elections.

To run the demo:
   Execute the master script in Scripts/boundary_vote_splitter_utility.R
   By default, it will compare the vote returns used by the 111th congress to the districts produced by a split line
   (be sure you have the correct working directory set for your environment on line 2! It will default to assuming you cloned to your desktop)
   
I have provided a test dataset using split line districting algorithm in the 2012 ohio election.
The json for this is alrady in Jobs/Pending/93235530bef043fe99455b62e685a741.json

To create your own:
  1. Place a correctly projected set of shapefiles (one with election data, one with districting boundaries) in the Input/ folder
  2. Create a json in the Jobs/Pending folder with the same format as the example
  3. In that json, reference your shapefile names and the columns denoting democratic and republican precinct votes
  4. Scripts/boundary_vote_splitter_utility.R
  
Outputs will be placed in Output/outputPNG/[districtFileName_x_votingFileName]