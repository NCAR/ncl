      double precision function ftstoredp(x)
c
      double precision x
c
c                                     coded by russell lynch
c                              from fitpack -- april 8, 1991
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this function forces the placement of the value of its input
c to be placed in a double precision storage cell consistant
c with fortran 66. it is useful for comparing values nearly
c equal with respect to the precision of the computer.
c
c on input--
c
c   x is a double precision variable.
c
c on output--
c
c   ftstoredp is a double precision version of ftstore.
c
c and x is unaltered.
c
c this function references module strhlp and requires a
c common block named dvcom.
c
c-------------------------------------------------------------
c
c
      common/dvcom/vc
      double precision vc
      call strhlpdp(x)
      ftstoredp = vc
      return
      end
