;-----------------------------------------------------------------
;  NCL User Guide Example:   NCL_read_ASCII_2.ncl
;
;  KMF
;-----------------------------------------------------------------
begin
;
; To read this data into a 2D array dimensioned 17 x 2 
; (17 rows by 2 columns), use: 

  data = asciiread("asc2.txt",(/17,2/),"float")
  print(data)     ; Print the values
  
end
