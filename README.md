# UGWP
This repository contains the "Unified Gravity Wave Physics (UGWP)" parameterizations developed by NOAA/GSL.

The following is a brief overview on using the suite:

 - In the init_atmosphere core, there is a new static pre-processing option,
   'config_native_gwd_gsl_static', that is used to generate a new static file
   used by the Unified Gravity Wave Physics. The static file is written by the
   ugwp_oro_data stream, and requires two new datasets in the geographical
   dataset directory: 'topo_ugwp_30s' and 'topo_ugwp_2.5m'.

 - In the atmosphere core, there are several new options when config_gwdo_scheme
   = 'bl_ugwp_gwdo': config_ngw_scheme, config_knob_ugwp_tauamp, and
   config_ugwp_diags.

 - If config_ngw_scheme = true, an additional input file, ugwp_limb_tau.nc is
   needed. This namelist options activates the non-stationary gravity wave drag
   scheme.

 - If config_ugwp_diags = true, the diag_ugwp output stream will be written.



#REFERENCES
[Toy 2021](https://ufs.epic.noaa.gov/wp-content/uploads/2021/07/UGWP_in_UFS_July_2021-1.pdf)
[Toy et al, 2025](https://doi.org/10.25923/v2kx-8e10)
