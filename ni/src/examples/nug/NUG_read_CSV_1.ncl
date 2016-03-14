;-----------------------------------------------------------------
;  NCL User Guide Example:   NCL_read_CSV_1.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_csm.ncl"

begin
  diri = "./"
  fili = "Test_6h.csv"
  
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

  wks = gsn_open_wks("png","plot_csv")

  res                        =  True
  res@gsnDraw                =  False
  res@gsnFrame               =  False
  res@tiMainString           = "NCL UG Example: Read CSV data (delimiter = ;)"
  res@xyLineThicknessF       =   5
  res@trYMinF                =   0.0
  res@trYMaxF                =  12.0
  res@trXMinF                =   0
  res@trXMaxF                =  18

  plot = new(nl,graphic)
  color = (/"blue", "red", "green", "black", "orange"/)

  do l=0,nl-1
    y = val(l,:)
    res@xyLineColor            =  color(l)
    plot(l) = gsn_csm_xy(wks, x, y, res)
  end do
  draw(plot)
  frame(wks)
end

