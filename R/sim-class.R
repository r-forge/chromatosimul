## Define chromatoSimul class par slot contains or the parameters needed to
## generate simulated data result slot contains final retentime, profile matrix,
## could be used for graphic check after you generate different set of NetCDF files.

setClass('chromatoSimul',
         representation(par='list',
                        result='list'
                        ),
         prototype(par=list(dir='./',
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
                   result=list(rt=list(),
                     codeing=list(),
                     prof=list()))
         )


