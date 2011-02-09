load "$NCARGTEST/nclscripts/functions.ncl"

;---Tests for different types of input to ispan.

begin 
;---The int, long, and int64 output values
  ipos_out   = (/1,2,3,4,5,6,7,8,9,10/)
  ineg_out   = ipos_out(::-1)
  lpos_out   = (/1,2,3,4,5,6,7,8,9,10l/)
  lneg_out   = lpos_out(::-1)

;---There's a bug that doesn't allow you to mix ints and int64s.
;---https://vets.development.ucar.edu/jira/browse/NCL-784
;  i64pos_out = (/1,2,3,4,5,6,7,8,9,10q/)

  i64pos_out = new(10,int64)
  i64pos_out = (/1,2,3,4,5,6,7,8,9,10/)
  i64neg_out = i64pos_out(::-1)

;---Byte
  bbeg = 1b
  bend = 10b
  bspc = 1b

;---Short
  sbeg = 1h
  send = 10h
  sspc = 1h

;---Int
  ibeg = 1
  iend = 10
  ispc = 1

;---Long
  lbeg = 1l
  lend = 10l
  lspc = 1l

;---Int64
  i64beg = 1q
  i64end= 10q
  i64spc = 1q

;---Int8
  i8beg = 1c
  i8end = 10c
  i8spc = 1c

;---Test byte input to "ispan"
  bpos    = ispan( bbeg, bend, bspc)
  bneg    = ispan( bend, bbeg, bspc)

  check_type("ispan",bpos,"integer")
  check_type("ispan",bneg,"integer")
  check_values("ispan","byte pos",bpos,ipos_out,True,False,0)
  check_values("ispan","byte neg",bneg,ineg_out,True,False,0)

;---Test short input to "ispan"
  spos    = ispan( sbeg, send, sspc)
  sneg    = ispan( send, sbeg, sspc)

  check_type("ispan",spos,"integer")
  check_type("ispan",sneg,"integer")
  check_values("ispan","short pos",spos,ipos_out,True,False,0)
  check_values("ispan","short neg",sneg,ineg_out,True,False,0)

;---Test integer input to "ispan"
  ipos    = ispan( ibeg, iend, ispc)
  ineg    = ispan( iend, ibeg, ispc)

  check_type("ispan",ipos,"integer")
  check_type("ispan",ineg,"integer")
  check_values("ispan","int pos",ipos,ipos_out,True,False,0)
  check_values("ispan","int neg",ineg,ineg_out,True,False,0)

;---Test long input to "ispan"
  lpos    = ispan( lbeg, lend, lspc)
  lneg    = ispan( lend, lbeg, lspc)

  check_type("ispan",lpos,"long")
  check_type("ispan",lneg,"long")
  check_values("ispan","long pos",lpos,lpos_out,True,False,0)
  check_values("ispan","long neg",lneg,lneg_out,True,False,0)

;---Test int64 input to "ispan"
  i64pos    = ispan( i64beg, i64end, i64spc)
  i64neg    = ispan( i64end, i64beg, i64spc)

  check_type("ispan",i64pos,"int64")
  check_type("ispan",i64neg,"int64")
  check_values("ispan","int64 pos",i64pos,i64pos_out,True,False,0)
  check_values("ispan","int64 neg",i64neg,i64neg_out,True,False,0)

;---Test int8 input to "ispan"
; Something is wrong with int8 conversion, so comment out for now.
;---https://vets.development.ucar.edu/jira/browse/NCL-783
  print("ispan: int8 pos test unsuccessful")
  print("ispan: int8 neg test unsuccessful")

;  i8pos_out = (/1,2,3,4,5,6,7,8,9,10c/)
;  i8neg_out = i8pos_out(::-1)
;  i8pos     = ispan( i8beg, i8end, i8spc)
;  i8neg     = ispan( i8end, i8beg, i8spc)
;  check_type("ispan",i8pos,"integer")
;  check_type("ispan",i8neg,"integer")
;  check_values("ispan","int8 pos",i8pos,i8pos_out,True,False,0)
;  check_values("ispan","int8 neg",i8neg,i8neg_out,True,False,0)

;---Test byte, short input to ispan
  bspos = ispan( bbeg, send, sspc)
  bsneg = ispan( bend, sbeg, bspc)

  check_type("ispan",bspos,"integer")
  check_type("ispan",bsneg,"integer")
  check_values("ispan","byte/short pos",bspos,ipos_out,True,False,0)
  check_values("ispan","byte/short neg",bsneg,ineg_out,True,False,0)

;---Test byte, short, int input to ispan
  bsipos = ispan( bbeg, send, ispc)
  bsineg = ispan( iend, sbeg, sspc)

  check_type("ispan",bspos,"integer")
  check_type("ispan",bsneg,"integer")
  check_values("ispan","byte/short/int pos",bsipos,ipos_out,True,False,0)
  check_values("ispan","byte/short/int neg",bsineg,ineg_out,True,False,0)

;---Test short, int, long input to ispan
  silpos = ispan( lbeg, send, ispc)
  silneg = ispan( lend, ibeg, sspc)

  check_type("ispan",silpos,"long")
  check_type("ispan",silneg,"long")
  check_values("ispan","short/int/long pos",silpos,lpos_out,True,False,0)
  check_values("ispan","short/int/long neg",silneg,lneg_out,True,False,0)

;---Test int, long, int64 input to ispan
  ili64pos = ispan( lbeg, iend,   i64spc)
  ili64neg = ispan( lend, i64beg, ispc)

  check_type("ispan",ili64pos,"int64")
  check_type("ispan",ili64neg,"int64")
  check_values("ispan","int/long/int64 pos",ili64pos,i64pos_out,True,False,0)
  check_values("ispan","int/long/int64 neg",ili64neg,i64neg_out,True,False,0)

;---Test float input, should get an error.
  x = ispan( 1., 10., 1)

end
