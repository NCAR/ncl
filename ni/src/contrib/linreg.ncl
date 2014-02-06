;----------------------------------------------------------------------
; "linreg" is a regression function that returns a lot more information
; than "regcoef" and "regline". The developer plans to extend this 
; function or creat a new one to accommodate multiple explanatory 
; variables.
;----------------------------------------------------------------------
undef("linreg")
function linreg(x[*]:numeric, y[*]:numeric)
local nx, ny, num_x_real, num_y_real, common, nxy_real, x_in, y_in, \
      a, b, f, e, dof, sse, se2, mse, sst, ssr, msr, F, r2, r2_adj,  \
      se_a, se_b, t_a, t_b, p_a, p_b, out
begin

; Get the lengths of the x and y arrays.
 nx = dimsizes(x)
 ny = dimsizes(y)

; Check if those lengths match.
 if(nx.ne.ny)then
  print("linreg: Error: x and y must be the same length!")
  exit
 end if

; Create a logical array of the common non-missing values.
 common = .not.ismissing(x) .and. .not.ismissing(y)

; Find out how many common non-missing values are present.
 nxy_real = num(common)

; If we have less than three common observations, then the regression
; cannot proceed.
 if(nxy_real .lt.3)then
  print("linreg: Error: Less than three common observations detected!")
  exit
 end if

; Create arrays that only have the common observations in x and y.
 if(isatt(x, "_FillValue"))then
   x_in = x(ind(common))
 else
   x_in = x(ind(common))
   x_in@_FillValue = default_fillvalue(typeof(x))
 end if

 if(isatt(y, "_FillValue"))then
   y_in = y(ind(common))
 else
   y_in = y(ind(common))
   y_in@_FillValue = default_fillvalue(typeof(y))
 end if

; Compute the slope (b) and intercept (a).
 b = (nxy_real*sum(x_in*y_in) - sum(x_in)*sum(y_in))/(nxy_real*sum(x_in^2) - sum(x_in)^2)
 a = avg(y_in) - b*avg(x_in)

; Calculate the fitted values and the residuals.
 f = a + b*x
 e = y - f

; Calculate the degrees of freedome, sum of the squared errors, etc.
 dof = nxy_real - 2
 sse = sum(e^2)
 se2 = sse/dof
 se  = sqrt(se2)
 mse = se2
 sst = sum((y_in - avg(y_in))^2)
 ssr = sum((f - avg(y))^2)
 msr = ssr
 F = msr/mse

; Calculate the coefficients of determination.
 r2 = ssr/sst
 r2_adj = 1 - (1 - r2)*((nxy_real - 1)/(nxy_real - 1 - 1))

; Calculate the standard errors.
 se_a = sqrt(se2*sum(x_in^2)/(nxy_real*sum( (x_in - avg(x_in))^2 )))
 se_b = sqrt(se2/sum( (x_in - avg(x_in))^2 ))

; Calculate the t-statistics.
 t_a = a/se_a
 t_b = b/se_b

; Calculate the p-values.
 p_a = betainc(dof/(dof+t_a^2), dof/2.0, 0.5)
 p_b = betainc(dof/(dof+t_b^2), dof/2.0, 0.5)

 out = True
 out@x = x
 out@y = y
 out@a = a
 out@b = b
 out@f = f
 out@e = e
 out@dof = dof
 out@se = se
 out@se2 = se2
 out@F = F
 out@r2 = r2
 out@r2_adj = r2_adj
 out@se_a = se_a
 out@se_b = se_b
 out@t_a = t_a
 out@t_b = t_b
 out@p_a = p_a
 out@p_b = p_b

 return(out)

end
