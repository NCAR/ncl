;-----------------------------------------------------------------
;  NCL User Guide Example:   NCL_histograms.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

;-- generate a 2D data set (gsn_histogram will go into compare mode)
  data = new((/2,1000/),float)
  data(0,:) = random_uniform(0,500.,1000)
  data(1,:) = random_uniform(0,500.,1000)

  xint = ispan(0,500,25)

;-- open workstation
  wks = gsn_open_wks("png","plot_histograms")           ;-- open workstation
  gsn_define_colormap(wks,"rainbow")                   ;-- choose colormap

  res                                   =  True
  res@gsnHistogramBarWidthPercent       =  70.
  res@gsnHistogramClassIntervals        =  xint
  res@tmXBLabelAngleF                   =  325.        ;-- change label angle
  res@tmLabelAutoStride                 =  True        ;-- prevent label overlap

  res@tiMainString                      = "NCL UG Example: Histograms"

  plot=gsn_histogram(wks,data,res)                     ;-- create histogram

end
