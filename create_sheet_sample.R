# Run once on set up
googlesheets4::gs4_create(name = "baby-log-test2", 
                          # Create a sheet called main for all data to 
                          # go to the same place
                          sheets = "main")