;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_write_ASCII_1.ncl
;
;  KMF
;-----------------------------------------------------------------
begin

;-- Generate a dummy 2 x 3 x 4 array

  data = random_uniform(-5,5,(/2,3,4/)) 

;-- Write it to a file

  asciiwrite("file1.txt",data)

end
