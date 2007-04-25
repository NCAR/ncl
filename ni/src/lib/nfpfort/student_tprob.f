C NCLFORTSTART
      subroutine stuprobt (t,df,tmsg,result)
      implicit none
      double precision t,df,tmsg,result

c NCL: alpha = student_probt (t,df)
C NCLEND
      double precision dbetaislatec

      if (t.ne.tmsg .and. df.gt.0) then
          result = dbetaislatec(df/(df+t**2), df*0.5d0, 0.5d0)
      else
          result = tmsg
      end if

      return
      end
