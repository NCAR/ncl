;-----------------------------------------------------------------
;  NCL User Guide Example:   NCL_read_ASCII_1.ncl
;
;  KMF
;-----------------------------------------------------------------
begin
; Read data into a one-dimensional int array of length 14:

  data = asciiread("asc1.txt",14,"integer")

  npts = dimsizes(data)   ; should be 14
  print("Number of values: "+npts)
  print(data)             ; print the values

end
