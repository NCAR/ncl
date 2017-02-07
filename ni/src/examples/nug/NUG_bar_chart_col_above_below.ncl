;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_bar_chart_col_above_below.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
   low  = -1.0
   high =  1.0
   n    = 24

   x = fspan(1.0,  12.0,  n)
   y = random_uniform(low, high, n)

   wks = gsn_open_wks("png","NUG_bar_chart_col_above_below")

   res                       =  True
   res@gsnDraw               =  True
   res@gsnFrame              =  False
   res@gsnXYBarChart         =  True
   res@gsnXYBarChartBarWidth =  0.25

   res@trXMinF               =  0.0              ;-- x-axis min value
   res@trXMaxF               = 13.0              ;-- x-axis max value
   res@trYMinF               = -1.0              ;-- y-axis min value
   res@trYMaxF               =  1.0              ;-- y-axis max value

   res@tmXBMode              = "Explicit"        ;-- explicit labels
   res@tmXBValues            =  fspan(1,12,n)
   res@tmXBLabels            =  (/"Jan","","Feb","","Mar","","Apr","","May","","Jun","",\
                                  "Jul","","Aug","","Sep","","Oct","","Nov","","Dec",""/)
   res@tmXBLabelFontHeightF  =  0.015
   res@tiMainString          = "NCL Doc Example: bar chart coloring above/below"

   res@gsnYRefLine           =  0.              ; reference line   
   res@gsnXYBarChart         =  True            ; create bar chart 
   res@gsnAboveYRefLineColor = "red"           ; above ref line fill red
   res@gsnBelowYRefLineColor = "blue"          ; below ref line fill blue

   plot = gsn_csm_xy(wks, x, y, res)

   frame(wks)      
end
