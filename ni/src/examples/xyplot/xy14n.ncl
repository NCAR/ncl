;
;      $Id: xy14n.ncl,v 1.2 1995-05-15 15:29:28 scheitln Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       xy14n.ncl
;;
;;  Author:     Fred Clare
;;              National Center for Atmospheric Research
;;              PO 3000, Boulder, Colorado
;;
;;  Date:       Thu May 11 15:19:44 MDT 1995
;;
;; Description:  This example demonstrates the data manipulation
;;               capabilities of NCL in conjunction with XyPlot.
;;

;
; Begin NCL script.
;
begin

; 
; Define temperature/rainfall data for a 25-hour period in
; an NCL variable.  In a real-world NCL script these values
; would probably be read in, either directly or as part of
; a data file like a netCDF or HDF file.
;
; The temperature data is hourly data in degrees Celcius and
; the rainfall data is hourly data in millimeters.
;

tr_data = (/                                                     \
            (/                                                   \
               9.0,  8.3,  7.4,  6.2,  5.6,  3.4,  2.7,  1.2,    \
               3.7,  5.8,  5.9,  4.5,  7.4, 11.6, 10.4,  9.2,    \
               7.8, 10.2, 12.1, 11.4,  9.0,  8.9,  8.5,  7.4,    \
               6.6                                               \
            /),                                                  \
                                                                 \
            (/   0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, \ 
                 0.00, 0.00, 0.25, 0.63, 0.10, 0.00, 0.53, 3.75, \
                 3.10, 0.52, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, \
                 0.00                                            \
            /)                                                   \
          /)
     

;
; Set up an X11 output workstation.
;

xworkid = create "simple" xWorkstationClass defaultapp
    "wkPause" : "True"
end create

;
; Create X-axis values using the NCL "new" and "do" statements.
;

nhours = 25
x_array = new(nhours,float)
do i=0,nhours-1
  x_array(i) = i
end do

;
; Picture 1:
;
; Convert the temperatures to degrees Fahrenheit and plot as an XyPlot
; object in the X11 window.  Note the usage of NCL algebraic operators
; and array selection capabilities.
;

dataid = create "xyData" coordArraysClass defaultapp
    "caYArray": (9./5.)*tr_data(0,:)+32.
    "caXArray": x_array
end create

plotid = create "xyPlot" xyPlotClass xworkid
    "xyCoordData": dataid

; X-axis resources
    "tmXBMode": "MANUAL"
    "tmXBTickStartF": 0.
    "tmXBTickEndF": 24.
    "tmXBTickSpacingF": 4.
    "tmXBLabelFont": 22
    "tmXBLabelFontHeightF": 0.024

; Y-axis resources.  The title precision is changed to "2" so that integer
; values will be used (the temperatures are in a range requiring only
; two digits).
    "tmYLMode": "MANUAL"
    "tmYLTickStartF": 30.
    "tmYLTickEndF": 60.
    "tmYLTickSpacingF": 5.
    "tmYLAutoPrecision": "False"
    "tmYLPrecision": 2
    "tmYLLabelFont": 22
    "tmYLLabelFontHeightF": 0.024
    "tmYLMinorPerMajor": 4

;
; Specify the Y-axis range precisely.
    "trYMinF": 30.
    "trYMaxF": 60.

; Supply titles
    "tiMainFont": 26
    "tiMainFontHeightF": .03
    "tiMainString": "Hourly Temperatures"
    "tiXAxisFont": 22
    "tiXAxisString": "Hours Since Midnight"
    "tiYAxisFont": 22
    "tiYAxisString": "Temperature (degrees F)"
    "tiYAxisOffsetXF": 0.1
end create

draw(plotid)
frame(xworkid)

;
; Picture 2:
;
; Convert the rainfall data to inches and plot in the X11 window. 
; Note how easy it is to select the rainfall data from the original
; two-dimensional data array.
;

mm2inch = 1./25.4
dataid = create "xyData" coordArraysClass defaultapp
    "caYArray": mm2inch*tr_data(1,:)
    "caXArray": x_array
end create

plotid = create "xyPlot" xyPlotClass xworkid
    "xyCoordData": dataid

; X-axis resources
    "tmXBMode": "MANUAL"
    "tmXBTickStartF": 0.
    "tmXBTickEndF": 24.
    "tmXBTickSpacingF": 4.
    "tmXBLabelFont": 22
    "tmXBLabelFontHeightF": 0.024

; Y-axis resources.
    "tmYLMode": "MANUAL"
    "tmYLTickStartF": 0.
    "tmYLTickEndF": 0.15
    "tmYLTickSpacingF": 0.03
    "tmYLAutoPrecision": "False"
    "tmYLPrecision": 2
    "tmYLLabelFont": 22
    "tmYLLabelFontHeightF": 0.024
    "tmYLMinorPerMajor": 4

; Specify the Y-axis range precisely.
    "trYMinF": 0.
    "trYMaxF": 0.15

; Supply titles
    "tiMainFont": 26
    "tiMainFontHeightF": .03
    "tiMainString": "Hourly Rainfall"
    "tiXAxisFont": 22
    "tiXAxisString": "Hours Since Midnight"
    "tiYAxisFont": 22
    "tiYAxisString": "Rainfall in Inches"
    "tiYAxisOffsetXF": 0.1
end create

draw(plotid)
frame(xworkid)

;
; Picture 3:
;
; For each hourly temperature reading, plot the number of standard
; deviations from the mean the reading represents.  Note the use of the
; NCL exponentiation operator (^) and the sqrt function.
;

t_mean = 0.
do i=0,nhours-1
  t_mean = t_mean+(9./5.)*tr_data(0,i)+32.
end do
t_mean = t_mean/nhours

std_dev = 0.
do i=0,nhours-1
  std_dev = std_dev+( ((9./5.)*tr_data(0,i)+32.) - t_mean)^2
end do
std_dev = sqrt(std_dev/(nhours-1))


dataid = create "xyData" coordArraysClass defaultapp
    "caYArray": ((9./5.)*tr_data(0,:)+32. - t_mean)/std_dev
    "caXArray": x_array
end create

plotid = create "xyPlot" xyPlotClass xworkid

; X-axis resources
    "tmXBMode": "MANUAL"
    "tmXBTickStartF": 0.
    "tmXBTickEndF": 24.
    "tmXBTickSpacingF": 4.
    "tmXBLabelFont": 22
    "tmXBLabelFontHeightF": 0.024

; Y-axis resources.  
    "tmYLMode": "EXPLICIT"
    "tmYLValues": (/2.,1.,0.,-1.,-2./)
    "tmYLLabels": (/"2:F33:s",":F33:s",":F22:MEAN","-:F33:s","-2:F33:s"/)
    "tmYLTickSpacingF": 1.
    "tmYLLabelFont": 22
    "tmYLLabelFontHeightF": 0.024
    "tmYMajorGrid": "True"
    "tmYLMinorOn": "False"
    "tmYRMinorOn": "False"

;
; Specify the Y-axis range precisely.
    "trYMinF": -2.5
    "trYMaxF": 2.5

; Supply titles
    "tiMainFont": 26
    "tiMainFontHeightF": .03
    "tiMainString": "Temperature Deviations From Mean"
    "tiXAxisFont": 22
    "tiXAxisString": "Hours Since Midnight"
    "tiYAxisString": "Deviations"
    "tiYAxisOffsetXF": 0.1
    "tiYAxisFont": 22
end create

;
; Specify the use of filled circle markers.
;

dataspec = NhlAddData(plotid,"xyCoordData",dataid)
setvalues dataspec
      "xyMarkLineMode": "MARKERS"
      "xyMonoMarkLineMode": "True"
      "xyMarker": 16
      "xyMonoMarkerSize": True
      "xyMarkerSizeF": .02
end setvalues
 
draw(plotid)
frame(xworkid)

;
; Picture 4:
;
; Illustrate the use of being able to easily pick out subsections
; of data arrays.
;

dataid = create "xyData" coordArraysClass defaultapp
    "caYArray": (9./5.)*tr_data(0,2:24:6)+32.
    "caXArray": x_array(2:24:6)
end create

plotid = create "xyPlot" xyPlotClass xworkid
    "xyCoordData": dataid

; X-axis resources
    "tmXBMode": "MANUAL"
    "tmXBTickStartF": 2.
    "tmXBTickEndF": 22.
    "tmXBTickSpacingF": 6.
    "tmXBLabelFont": 22
    "tmXBLabelFontHeightF": 0.024
    "tmXBMinorPerMajor": 0

; Y-axis resources.
    "tmYLMode": "MANUAL"
    "tmYLTickStartF": 30.
    "tmYLTickEndF": 60.
    "tmYLTickSpacingF": 5.
    "tmYLAutoPrecision": "False"
    "tmYLPrecision": 2
    "tmYLLabelFont": 22
    "tmYLLabelFontHeightF": 0.024
    "tmYLMinorPerMajor": 4
;
; Specify the Y-axis range precisely.
    "trYMinF": 30.
    "trYMaxF": 60.

; Supply titles
    "tiMainFont": 26
    "tiMainString": "Temperatures at Six Hour Intervals"
    "tiMainFontHeightF": .03
    "tiXAxisFont": 22
    "tiXAxisString": "Hours Since Midnight"
    "tiYAxisFont": 22
    "tiYAxisString": "Temperature (degrees F)"
    "tiYAxisOffsetXF": 0.1
end create

draw(plotid)
frame(xworkid)

;
; End NCL script.
;
end
