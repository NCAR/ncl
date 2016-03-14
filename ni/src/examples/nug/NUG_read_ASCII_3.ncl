;-----------------------------------------------------------------
;  NCL User Guide Example:   NCL_read_ASCII_3.ncl
;
;  KMF
;-----------------------------------------------------------------
begin

  diri   = "./"
  fname  = "asc3.txt"
  data   =  asciiread(diri+fname,-1,"string")
  year   =  tofloat(str_get_cols(data, 1,4))
  month  =  tofloat(str_get_cols(data,5,6))
  day    =  tofloat(str_get_cols(data,7,8))
  hour   =  tofloat(str_get_cols(data,9,10))
  minute =  tofloat(str_get_cols(data,11,12))
  
;-- read the station name (field 18)

;  sta    =  str_get_cols(data,99,101)   ;-- you must know the digit column numbers
  sta    =  str_get_field(data,18," ")   ;-- you must know the field number

  print("Year: "+year+"  month: "+month+"  day: "+day+"  hour: "+hour+\
  "  minute: "+minute)
  print("Data:")
  print(""+sta)
  
end

