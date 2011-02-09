load "$NCARGTEST/nclscripts/functions.ncl"

begin
  i_2_31    = tolong(2^31)     ; make a long so it prints better
  i_2_31_m1 = i_2_31-1
  i_2_63_m1 = tolong(2^63)-1

  l = new(i_2_31,long)

;---Put some values in l
;  l(ispan(500,1000,1l)) = ispan(500,1000,1)
  l(5) = 50l

end
