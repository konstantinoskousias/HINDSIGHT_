#-------------------- Testing function for HINDSIGHT++ --------------------#

source('hindsightpp.R') # Load main function

# We use 'pollution.csv; for testing purposes
files = c('../pollution.csv') # Input files (separated by comma)

res = hindsightpp(files, 
                  nfeatures = 6, nlags = 5, msteps = 1, fs_method = 'rfe',
                  network_type = 'Vanilla', act = 3, opt = 2, 
                  split = 0.8, valsplit = 0.2, search_type = "Manual", niter = 50, 
                  units1 = 256, units2 = 128, units3 = 64, lr = 0.001, nepochs = 50, bs = 8, nlayers = 2)
