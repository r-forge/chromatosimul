## Define chromatoSimul class par slot contains or the parameters needed to
## generate simulated data result slot contains final retentime, profile matrix,
## could be used for graphic check after you generate different set of NetCDF files.

setClass('chromatoSim',
         representation(par='list',
                        result='list',
                        partips='list'
                        ),
         prototype(par=list(
                     dir='./',
                     model='',
                     int_range = c(300,300000),
                     back_sd=0,
                     rep=2,
                     rep_sd=0,
                     common=5,
                     diff_low=5,
                     diff_zero=5,
                     low=0.2,
                     low_sd=0,
                     mz_range = c(50,800),
                     npeaks_mean=100,
                     npeaks_sd=0.1,
                     rt_range = c(215,3600),
                     rt_diff =  0.5,
                     rt_shift_sd= 5,
                     span=15,
                     span_sd=0.1,
                     sigma=1.5,
                     sigma_sd=0.1,
                     tau_mean = 0,
                     tau_sd = 0.1,
                     missing=NULL
                     ),
                   ## this partips is used for GUI.
                   partips=list(
                      dir="Specify the output directory",
                     model='ncdf Model you want to simulate from',
                     int_range = 'Intensity range for simulated profile matrix',
                     back_sd='Background noise: standard deviation for normal distribution',
                     rep='How many replicate do you want for each treatment',
                     rep_sd='If not 0, will add noise for each replicate',
                     common='How many metabolites exist in all the treatments.',
                     diff_low='How many metabolites are have level change between treatments',
                     diff_zero='How many metabolites exists only in each treatment',
                     low='How low you want for those metabolites which have level change between treatments,value(0-1)',
                     low_sd='If you want to add noise to level change, change this value',
                     mz_range = 'Mz ration range for simulated data',
                     npeaks_mean='How many peaks in each spectrum, this parameter set mean value for a normal distribution,used for simulation',
                     npeaks_sd='See npeaks_mean, this set standard deviation',
                     rt_range = 'Retention time range',
                     rt_diff =  'Retention time between each scan',
                     rt_shift_sd= 'Used for simulation',
                     span='Span',
                     span_sd='Span standard deviation',
                     sigma='Sigma',
                     sigma_sd='Sigma standard deviation',
                     tau_mean = 'Tau mean',
                     tau_sd = 'Tau standard deviation',
                     missing='Missing value'
                     ),
                   result=list(rt=list(),
                     codeing=list(),
                     prof=list()))
         )


