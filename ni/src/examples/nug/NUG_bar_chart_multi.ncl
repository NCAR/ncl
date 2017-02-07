;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_bar_chart_multi.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
   low  =  0.0
   high =  1.0
   n    = 12

   x  = fspan(0.5,  11.5,  n)
   y1 = random_uniform(low, high, n)
   y2 = random_uniform(low, high, n)
   y3 = random_uniform(low, high, n)

   wks = gsn_open_wks("png","NUG_bar_chart_multi")

   res                       =  True
   res@gsnDraw               =  True
   res@gsnFrame              =  False
   res@gsnXYBarChart         =  True
   res@gsnXYBarChartBarWidth =  0.25

   res@trXMinF               =  0.0              ;-- x-axis min value
   res@trXMaxF               = 12.5              ;-- x-axis max value
   res@trYMinF               =  0.0              ;-- y-axis min value
   res@trYMaxF               =  1.0              ;-- y-axis max value

   res@tmXBMode              = "Explicit"        ;-- explicit labels
   res@tmXBValues            =  ispan(1,12,1)-0.25
   res@tmXBLabels            = (/"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep", \
                                 "Oct","Nov","Dec"/)
   res@tmXBLabelFontHeightF  =  0.015
   res@tiMainString          = "NCL Doc Example: bar chart of multi data sets"

   res@gsnXYBarChartColors   = "blue"
   plots1 = gsn_csm_xy(wks, x, y1, res)

   res@gsnXYBarChartColors   = "gold"
   plots2 = gsn_csm_xy(wks, x+0.25, y2, res)

   res@gsnXYBarChartColors   = "green"
   plots3 = gsn_csm_xy(wks, x+0.5, y3, res)

   frame(wks)      
end
