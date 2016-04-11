;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_read_ASCII_2.ncl
;
;  KMF
;-----------------------------------------------------------------
begin
;
; To read this data into a 2D array dimensioned 17 x 2 
; (17 rows by 2 columns), use: 

  diri = "$NCARG_ROOT/lib/ncarg/data/nug/"
  fili = "asc2.txt"
  
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
  
  data = asciiread(diri+fili,(/17,2/),"float")
  print(data)     ; Print the values
  
end
