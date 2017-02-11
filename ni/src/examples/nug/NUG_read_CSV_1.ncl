;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_read_CSV_1.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl" 
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
  diri = "$NCARG_ROOT/lib/ncarg/data/nug/"
  fili = "Test_6h.csv"

  if (.not. fileexists(diri+fili)) then
     print("")
     print("You don't have the necessary data for this script. You can download it from:​")
     print("")
     print("http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+fili)
     print("")
     print("or use the wget command:")
     print("")
     print("wget http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+fili)
     print("")
     exit
  end if
  
;-- read in file as array of strings so we can parse each line

  delim  = ";"

  data   = asciiread(diri+fili, -1, "string")
  scount = str_fields_count(data(0),delim)

;-- read 6h values

  nl  = dimsizes(data)
  val = tofloat(str_split_csv(data,delim,0))
  print("Val: " + val)

;-- 4 timesteps, interval 6h
  x = ispan(0,18,6)

  wks = gsn_open_wks("png","NUG_read_CSV_1")

  res                        =  True
  res@tiMainString           = "NCL Doc Example: Read CSV data (delimiter = ;)"
  res@xyLineThicknessF       =   5
  res@trYMinF                =   0.0
  res@trYMaxF                =  12.0
  res@trXMinF                =   0
  res@trXMaxF                =  18
  res@xyMonoDashPattern      = 0   ;-- make lines all solid
  res@xyLineColors           = (/"blue", "red", "green", "black", "orange"/)

  plot = gsn_csm_xy(wks, x, val, res)
end

