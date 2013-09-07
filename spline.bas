' Copyright (c) 2013, Heinrich Schuchardt <xypron.glpk@gmx.de>
' All rights reserved.
'
' Redistribution and use in source and binary forms are permitted
' provided that the above copyright notice and this paragraph are
' duplicated in all such forms.
' THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
' IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
' WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.

'
' Continous
' Interpolates exactly up to x ^ 3
' Needs 2 intervals
' Uses polynoms up to order 3
' 
FUNCTION Spline032D(t AS DOUBLE) AS DOUBLE
  DIM x AS DOUBLE
  x = abs(t)
  IF x < 1# THEN
    x =  1# - x
    Spline032D = - 1# / 2# * x ^ 3 + 1# / 2# * x ^ 2 + x 
  ELSEIF x < 2# THEN
    x =  2# - x
    Spline032D =  1# / 6# * x ^ 3  - 1# / 6# * x 
  ELSE
    Spline032D = 0#
  END IF
END FUNCTION
'
' Continous
' Interpolates exactly up to x ^ 2
' Needs 2 intervals
' Uses polynoms up to order 2
' 
FUNCTION Spline022C(t AS DOUBLE) AS DOUBLE
  DIM x AS DOUBLE
  x = abs(t)
  IF x < 1# THEN
    x =  1# - x
    Spline022C = - 1# / 4# * x ^ 2 + 5# / 4# * x 
  ELSEIF x < 2# THEN
    x =  2# - x
    Spline022C =  1# / 4# * x ^ 2 - 1# / 4# * x 
  ELSE
    Spline022C = 0#
  END IF
END FUNCTION
'
' Continous
' Interpolates exactly up to x ^ 1
' Needs 1 intervals
' Uses polynoms up to order 1
' 
FUNCTION Spline011B(t AS DOUBLE) AS DOUBLE
  DIM x AS DOUBLE
  x = abs(t)
  IF x < 1# THEN
    x =  1# - x
    Spline011B =  x 
  ELSE
    Spline011B = 0#
  END IF
END FUNCTION
'
' 1-times continuously differentiable
' Interpolates exactly up to x ^ 4
' Needs 3 intervals
' Uses polynoms up to order 4
' 
FUNCTION Spline143E(t AS DOUBLE) AS DOUBLE
  DIM x AS DOUBLE
  x = abs(t)
  IF x < 1# THEN
    x =  1# - x
    Spline143E =  1# / 24# * x ^ 4 - 17# / 12# * x ^ 3 + 41# / 24# * x ^ 2 + 2# / 3# * x 
  ELSEIF x < 2# THEN
    x =  2# - x
    Spline143E = - 1# / 16# * x ^ 4 + 17# / 24# * x ^ 3 - 9# / 16# * x ^ 2 - 1# / 12# * x 
  ELSEIF x < 3# THEN
    x =  3# - x
    Spline143E =  1# / 48# * x ^ 4 - 1# / 8# * x ^ 3 + 5# / 48# * x ^ 2  
  ELSE
    Spline143E = 0#
  END IF
END FUNCTION
'
' 1-times continuously differentiable
' Interpolates exactly up to x ^ 3
' Needs 3 intervals
' Uses polynoms up to order 3
' 
FUNCTION Spline133D(t AS DOUBLE) AS DOUBLE
  DIM x AS DOUBLE
  x = abs(t)
  IF x < 1# THEN
    x =  1# - x
    Spline133D = - 4# / 3# * x ^ 3 + 5# / 3# * x ^ 2 + 2# / 3# * x 
  ELSEIF x < 2# THEN
    x =  2# - x
    Spline133D =  7# / 12# * x ^ 3 - 1# / 2# * x ^ 2 - 1# / 12# * x 
  ELSEIF x < 3# THEN
    x =  3# - x
    Spline133D = - 1# / 12# * x ^ 3 + 1# / 12# * x ^ 2  
  ELSE
    Spline133D = 0#
  END IF
END FUNCTION
'
' 1-times continuously differentiable
' Interpolates exactly up to x ^ 2
' Needs 2 intervals
' Uses polynoms up to order 3
' 
FUNCTION Spline122D(t AS DOUBLE) AS DOUBLE
  DIM x AS DOUBLE
  x = abs(t)
  IF x < 1# THEN
    x =  1# - x
    Spline122D = - 3# / 2# * x ^ 3 + 2# * x ^ 2 + 1# / 2# * x 
  ELSEIF x < 2# THEN
    x =  2# - x
    Spline122D =  1# / 2# * x ^ 3 - 1# / 2# * x ^ 2  
  ELSE
    Spline122D = 0#
  END IF
END FUNCTION
'
' 1-times continuously differentiable
' Interpolates exactly up to x ^ 1
' Needs 2 intervals
' Uses polynoms up to order 3
' 
FUNCTION Spline112D(t AS DOUBLE) AS DOUBLE
  DIM x AS DOUBLE
  x = abs(t)
  IF x < 1# THEN
    x =  1# - x
    Spline112D = - 3# / 2# * x ^ 3 + 2# * x ^ 2 + 1# / 2# * x 
  ELSEIF x < 2# THEN
    x =  2# - x
    Spline112D =  1# / 2# * x ^ 3 - 1# / 2# * x ^ 2  
  ELSE
    Spline112D = 0#
  END IF
END FUNCTION
'
' 2-times continuously differentiable
' Interpolates exactly up to x ^ 4
' Needs 3 intervals
' Uses polynoms up to order 5
' 
FUNCTION Spline243F(t AS DOUBLE) AS DOUBLE
  DIM x AS DOUBLE
  x = abs(t)
  IF x < 1# THEN
    x =  1# - x
    Spline243F =  25# / 12# * x ^ 5 - 31# / 6# * x ^ 4 + 11# / 4# * x ^ 3 + 2# / 3# * x ^ 2 + 2# / 3# * x 
  ELSEIF x < 2# THEN
    x =  2# - x
    Spline243F = - 25# / 24# * x ^ 5 + 61# / 24# * x ^ 4 - 11# / 8# * x ^ 3 - 1# / 24# * x ^ 2 - 1# / 12# * x 
  ELSEIF x < 3# THEN
    x =  3# - x
    Spline243F =  5# / 24# * x ^ 5 - 1# / 2# * x ^ 4 + 7# / 24# * x ^ 3   
  ELSE
    Spline243F = 0#
  END IF
END FUNCTION
'
' 2-times continuously differentiable
' Interpolates exactly up to x ^ 3
' Needs 3 intervals
' Uses polynoms up to order 4
' 
FUNCTION Spline233E(t AS DOUBLE) AS DOUBLE
  DIM x AS DOUBLE
  x = abs(t)
  IF x < 1# THEN
    x =  1# - x
    Spline233E = - 1# / 6# * x ^ 4 - x ^ 3 + 3# / 2# * x ^ 2 + 2# / 3# * x 
  ELSEIF x < 2# THEN
    x =  2# - x
    Spline233E =  1# / 4# * x ^ 4 + 1# / 12# * x ^ 3 - 1# / 4# * x ^ 2 - 1# / 12# * x 
  ELSEIF x < 3# THEN
    x =  3# - x
    Spline233E = - 1# / 12# * x ^ 4 + 1# / 12# * x ^ 3   
  ELSE
    Spline233E = 0#
  END IF
END FUNCTION
'
' 2-times continuously differentiable
' Interpolates exactly up to x ^ 2
' Needs 2 intervals
' Uses polynoms up to order 5
' 
FUNCTION Spline222F(t AS DOUBLE) AS DOUBLE
  DIM x AS DOUBLE
  x = abs(t)
  IF x < 1# THEN
    x =  1# - x
    Spline222F =  3# * x ^ 5 - 15# / 2# * x ^ 4 + 9# / 2# * x ^ 3 + 1# / 2# * x ^ 2 + 1# / 2# * x 
  ELSEIF x < 2# THEN
    x =  2# - x
    Spline222F = - x ^ 5 + 5# / 2# * x ^ 4 - 3# / 2# * x ^ 3   
  ELSE
    Spline222F = 0#
  END IF
END FUNCTION
'
' 2-times continuously differentiable
' Interpolates exactly up to x ^ 1
' Needs 2 intervals
' Uses polynoms up to order 4
' 
FUNCTION Spline212E(t AS DOUBLE) AS DOUBLE
  DIM x AS DOUBLE
  x = abs(t)
  IF x < 1# THEN
    x =  1# - x
    Spline212E = - 1# / 2# * x ^ 4 - 1# / 2# * x ^ 3 + 3# / 2# * x ^ 2 + 1# / 2# * x 
  ELSEIF x < 2# THEN
    x =  2# - x
    Spline212E =  1# / 2# * x ^ 4 - 1# / 2# * x ^ 3   
  ELSE
    Spline212E = 0#
  END IF
END FUNCTION
'
' 3-times continuously differentiable
' Interpolates exactly up to x ^ 4
' Needs 3 intervals
' Uses polynoms up to order 7
' 
FUNCTION Spline343H(t AS DOUBLE) AS DOUBLE
  DIM x AS DOUBLE
  x = abs(t)
  IF x < 1# THEN
    x =  1# - x
    Spline343H = - 35# / 6# * x ^ 7 + 245# / 12# * x ^ 6 - 145# / 6# * x ^ 5 + 113# / 12# * x ^ 4 - 1# / 6# * x ^ 3 + 2# / 3# * x ^ 2 + 2# / 3# * x 
  ELSEIF x < 2# THEN
    x =  2# - x
    Spline343H =  35# / 12# * x ^ 7 - 245# / 24# * x ^ 6 + 145# / 12# * x ^ 5 - 19# / 4# * x ^ 4 + 1# / 12# * x ^ 3 - 1# / 24# * x ^ 2 - 1# / 12# * x 
  ELSEIF x < 3# THEN
    x =  3# - x
    Spline343H = - 7# / 12# * x ^ 7 + 49# / 24# * x ^ 6 - 29# / 12# * x ^ 5 + 23# / 24# * x ^ 4    
  ELSE
    Spline343H = 0#
  END IF
END FUNCTION
'
' 3-times continuously differentiable
' Interpolates exactly up to x ^ 3
' Needs 3 intervals
' Uses polynoms up to order 6
' 
FUNCTION Spline333G(t AS DOUBLE) AS DOUBLE
  DIM x AS DOUBLE
  x = abs(t)
  IF x < 1# THEN
    x =  1# - x
    Spline333G =  7# / 24# * x ^ 6 - 1# / 4# * x ^ 5 - 19# / 24# * x ^ 4 - 1# / 6# * x ^ 3 + 5# / 4# * x ^ 2 + 2# / 3# * x 
  ELSEIF x < 2# THEN
    x =  2# - x
    Spline333G = - 7# / 16# * x ^ 6 + x ^ 5 - 3# / 8# * x ^ 4 + 1# / 12# * x ^ 3 - 3# / 16# * x ^ 2 - 1# / 12# * x 
  ELSEIF x < 3# THEN
    x =  3# - x
    Spline333G =  7# / 48# * x ^ 6 - 3# / 8# * x ^ 5 + 11# / 48# * x ^ 4    
  ELSE
    Spline333G = 0#
  END IF
END FUNCTION
'
' 3-times continuously differentiable
' Interpolates exactly up to x ^ 2
' Needs 2 intervals
' Uses polynoms up to order 7
' 
FUNCTION Spline322H(t AS DOUBLE) AS DOUBLE
  DIM x AS DOUBLE
  x = abs(t)
  IF x < 1# THEN
    x =  1# - x
    Spline322H = - 9# * x ^ 7 + 63# / 2# * x ^ 6 - 75# / 2# * x ^ 5 + 15# * x ^ 4  + 1# / 2# * x ^ 2 + 1# / 2# * x 
  ELSEIF x < 2# THEN
    x =  2# - x
    Spline322H =  3# * x ^ 7 - 21# / 2# * x ^ 6 + 25# / 2# * x ^ 5 - 5# * x ^ 4    
  ELSE
    Spline322H = 0#
  END IF
END FUNCTION
'
' 3-times continuously differentiable
' Interpolates exactly up to x ^ 1
' Needs 2 intervals
' Uses polynoms up to order 6
' 
FUNCTION Spline312G(t AS DOUBLE) AS DOUBLE
  DIM x AS DOUBLE
  x = abs(t)
  IF x < 1# THEN
    x =  1# - x
    Spline312G =  3# / 4# * x ^ 6 - 3# / 2# * x ^ 5   + 5# / 4# * x ^ 2 + 1# / 2# * x 
  ELSEIF x < 2# THEN
    x =  2# - x
    Spline312G = - 3# / 4# * x ^ 6 + 2# * x ^ 5 - 5# / 4# * x ^ 4    
  ELSE
    Spline312G = 0#
  END IF
END FUNCTION
'
' 4-times continuously differentiable
' Interpolates exactly up to x ^ 3
' Needs 4 intervals
' Uses polynoms up to order 6
' 
FUNCTION Spline434G(t AS DOUBLE) AS DOUBLE
  DIM x AS DOUBLE
  x = abs(t)
  IF x < 1# THEN
    x =  1# - x
    Spline434G =  97# / 672# * x ^ 6 + 151# / 672# * x ^ 5 - 431# / 336# * x ^ 4 - 1# / 336# * x ^ 3 + 829# / 672# * x ^ 2 + 153# / 224# * x 
  ELSEIF x < 2# THEN
    x =  2# - x
    Spline434G = - 237# / 1120# * x ^ 6 + 221# / 672# * x ^ 5 + 83# / 336# * x ^ 4 - 1# / 21# * x ^ 3 - 37# / 168# * x ^ 2 - 27# / 280# * x 
  ELSEIF x < 3# THEN
    x =  3# - x
    Spline434G =  43# / 672# * x ^ 6 - 167# / 1120# * x ^ 5 + 11# / 336# * x ^ 4 + 11# / 336# * x ^ 3 + 11# / 672# * x ^ 2 + 11# / 3360# * x 
  ELSEIF x < 4# THEN
    x =  4# - x
    Spline434G =  11# / 3360# * x ^ 6 - 11# / 3360# * x ^ 5     
  ELSE
    Spline434G = 0#
  END IF
END FUNCTION
'
' 4-times continuously differentiable
' Interpolates exactly up to x ^ 2
' Needs 2 intervals
' Uses polynoms up to order 9
' 
FUNCTION Spline422J(t AS DOUBLE) AS DOUBLE
  DIM x AS DOUBLE
  x = abs(t)
  IF x < 1# THEN
    x =  1# - x
    Spline422J =  30# * x ^ 9 - 135# * x ^ 8 + 231# * x ^ 7 - 357# / 2# * x ^ 6 + 105# / 2# * x ^ 5   + 1# / 2# * x ^ 2 + 1# / 2# * x 
  ELSEIF x < 2# THEN
    x =  2# - x
    Spline422J = - 10# * x ^ 9 + 45# * x ^ 8 - 77# * x ^ 7 + 119# / 2# * x ^ 6 - 35# / 2# * x ^ 5     
  ELSE
    Spline422J = 0#
  END IF
END FUNCTION
'
' 4-times continuously differentiable
' Interpolates exactly up to x ^ 1
' Needs 2 intervals
' Uses polynoms up to order 7
' 
FUNCTION Spline412H(t AS DOUBLE) AS DOUBLE
  DIM x AS DOUBLE
  x = abs(t)
  IF x < 1# THEN
    x =  1# - x
    Spline412H =  3# * x ^ 7 - 19# / 2# * x ^ 6 + 21# / 2# * x ^ 5 - 5# * x ^ 4  + 3# / 2# * x ^ 2 + 1# / 2# * x 
  ELSEIF x < 2# THEN
    x =  2# - x
    Spline412H = - x ^ 7 + 5# / 2# * x ^ 6 - 3# / 2# * x ^ 5     
  ELSE
    Spline412H = 0#
  END IF
END FUNCTION
