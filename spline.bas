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
DEFDBL A-Z
FUNCTION Fine032D(t)
  x = abs(t)
  SELECT CASE x
    CASE < 1#
      x =  1# - x
      Fine032D = -       1# /       2# *x ^ 3+       1# /       2# *x ^ 2+x
    CASE < 2#
      x =  2# - x
      Fine032D =        1# /       6# *x ^ 3-       1# /       6# *x
    CASE ELSE
      Fine032D = 0#
  END SELECT
END FUNCTION
'
' Continous
' Interpolates exactly up to x ^ 2
' Needs 2 intervals
' Uses polynoms up to order 2
' 
DEFDBL A-Z
FUNCTION Fine022C(t)
  x = abs(t)
  SELECT CASE x
    CASE < 1#
      x =  1# - x
      Fine022C = -       1# /       4# *x ^ 2+       5# /       4# *x
    CASE < 2#
      x =  2# - x
      Fine022C =        1# /       4# *x ^ 2-       1# /       4# *x
    CASE ELSE
      Fine022C = 0#
  END SELECT
END FUNCTION
'
' Continous
' Interpolates exactly up to x ^ 1
' Needs 1 intervals
' Uses polynoms up to order 1
' 
DEFDBL A-Z
FUNCTION Fine011B(t)
  x = abs(t)
  SELECT CASE x
    CASE < 1#
      x =  1# - x
      Fine011B = x
    CASE ELSE
      Fine011B = 0#
  END SELECT
END FUNCTION
'
' 1-times continuously differentiable
' Interpolates exactly up to x ^ 4
' Needs 3 intervals
' Uses polynoms up to order 4
' 
DEFDBL A-Z
FUNCTION Fine143E(t)
  x = abs(t)
  SELECT CASE x
    CASE < 1#
      x =  1# - x
      Fine143E =        1# /      24# *x ^ 4-      17# /      12# *x ^ 3+      41# /      24# *x ^ 2+       2# /       3# *x
    CASE < 2#
      x =  2# - x
      Fine143E = -       1# /      16# *x ^ 4+      17# /      24# *x ^ 3-       9# /      16# *x ^ 2-       1# /      12# *x
    CASE < 3#
      x =  3# - x
      Fine143E =        1# /      48# *x ^ 4-       1# /       8# *x ^ 3+       5# /      48# *x ^ 2
    CASE ELSE
      Fine143E = 0#
  END SELECT
END FUNCTION
'
' 1-times continuously differentiable
' Interpolates exactly up to x ^ 3
' Needs 3 intervals
' Uses polynoms up to order 3
' 
DEFDBL A-Z
FUNCTION Fine133D(t)
  x = abs(t)
  SELECT CASE x
    CASE < 1#
      x =  1# - x
      Fine133D = -       4# /       3# *x ^ 3+       5# /       3# *x ^ 2+       2# /       3# *x
    CASE < 2#
      x =  2# - x
      Fine133D =        7# /      12# *x ^ 3-       1# /       2# *x ^ 2-       1# /      12# *x
    CASE < 3#
      x =  3# - x
      Fine133D = -       1# /      12# *x ^ 3+       1# /      12# *x ^ 2
    CASE ELSE
      Fine133D = 0#
  END SELECT
END FUNCTION
'
' 1-times continuously differentiable
' Interpolates exactly up to x ^ 2
' Needs 2 intervals
' Uses polynoms up to order 3
' 
DEFDBL A-Z
FUNCTION Fine122D(t)
  x = abs(t)
  SELECT CASE x
    CASE < 1#
      x =  1# - x
      Fine122D = -       3# /       2# *x ^ 3+       2# *x ^ 2+       1# /       2# *x
    CASE < 2#
      x =  2# - x
      Fine122D =        1# /       2# *x ^ 3-       1# /       2# *x ^ 2
    CASE ELSE
      Fine122D = 0#
  END SELECT
END FUNCTION
'
' 1-times continuously differentiable
' Interpolates exactly up to x ^ 1
' Needs 2 intervals
' Uses polynoms up to order 3
' 
DEFDBL A-Z
FUNCTION Fine112D(t)
  x = abs(t)
  SELECT CASE x
    CASE < 1#
      x =  1# - x
      Fine112D = -       3# /       2# *x ^ 3+       2# *x ^ 2+       1# /       2# *x
    CASE < 2#
      x =  2# - x
      Fine112D =        1# /       2# *x ^ 3-       1# /       2# *x ^ 2
    CASE ELSE
      Fine112D = 0#
  END SELECT
END FUNCTION
'
' 2-times continuously differentiable
' Interpolates exactly up to x ^ 4
' Needs 3 intervals
' Uses polynoms up to order 5
' 
DEFDBL A-Z
FUNCTION Fine243F(t)
  x = abs(t)
  SELECT CASE x
    CASE < 1#
      x =  1# - x
      Fine243F =       25# /      12# *x ^ 5-      31# /       6# *x ^ 4+      11# /       4# *x ^ 3+       2# /       3# *x ^ 2+       2# /       3# *x
    CASE < 2#
      x =  2# - x
      Fine243F = -      25# /      24# *x ^ 5+      61# /      24# *x ^ 4-      11# /       8# *x ^ 3-       1# /      24# *x ^ 2-       1# /      12# *x
    CASE < 3#
      x =  3# - x
      Fine243F =        5# /      24# *x ^ 5-       1# /       2# *x ^ 4+       7# /      24# *x ^ 3
    CASE ELSE
      Fine243F = 0#
  END SELECT
END FUNCTION
'
' 2-times continuously differentiable
' Interpolates exactly up to x ^ 3
' Needs 3 intervals
' Uses polynoms up to order 4
' 
DEFDBL A-Z
FUNCTION Fine233E(t)
  x = abs(t)
  SELECT CASE x
    CASE < 1#
      x =  1# - x
      Fine233E = -       1# /       6# *x ^ 4-x ^ 3+       3# /       2# *x ^ 2+       2# /       3# *x
    CASE < 2#
      x =  2# - x
      Fine233E =        1# /       4# *x ^ 4+       1# /      12# *x ^ 3-       1# /       4# *x ^ 2-       1# /      12# *x
    CASE < 3#
      x =  3# - x
      Fine233E = -       1# /      12# *x ^ 4+       1# /      12# *x ^ 3
    CASE ELSE
      Fine233E = 0#
  END SELECT
END FUNCTION
'
' 2-times continuously differentiable
' Interpolates exactly up to x ^ 2
' Needs 2 intervals
' Uses polynoms up to order 5
' 
DEFDBL A-Z
FUNCTION Fine222F(t)
  x = abs(t)
  SELECT CASE x
    CASE < 1#
      x =  1# - x
      Fine222F =        3# *x ^ 5-      15# /       2# *x ^ 4+       9# /       2# *x ^ 3+       1# /       2# *x ^ 2+       1# /       2# *x
    CASE < 2#
      x =  2# - x
      Fine222F = -x ^ 5+       5# /       2# *x ^ 4-       3# /       2# *x ^ 3
    CASE ELSE
      Fine222F = 0#
  END SELECT
END FUNCTION
'
' 2-times continuously differentiable
' Interpolates exactly up to x ^ 1
' Needs 2 intervals
' Uses polynoms up to order 4
' 
DEFDBL A-Z
FUNCTION Fine212E(t)
  x = abs(t)
  SELECT CASE x
    CASE < 1#
      x =  1# - x
      Fine212E = -       1# /       2# *x ^ 4-       1# /       2# *x ^ 3+       3# /       2# *x ^ 2+       1# /       2# *x
    CASE < 2#
      x =  2# - x
      Fine212E =        1# /       2# *x ^ 4-       1# /       2# *x ^ 3
    CASE ELSE
      Fine212E = 0#
  END SELECT
END FUNCTION
'
' 3-times continuously differentiable
' Interpolates exactly up to x ^ 4
' Needs 3 intervals
' Uses polynoms up to order 7
' 
DEFDBL A-Z
FUNCTION Fine343H(t)
  x = abs(t)
  SELECT CASE x
    CASE < 1#
      x =  1# - x
      Fine343H = -      35# /       6# *x ^ 7+     245# /      12# *x ^ 6-     145# /       6# *x ^ 5+     113# /      12# *x ^ 4-       1# /       6# *x ^ 3+       2# /       3# *x ^ 2+       2# /       3# *x
    CASE < 2#
      x =  2# - x
      Fine343H =       35# /      12# *x ^ 7-     245# /      24# *x ^ 6+     145# /      12# *x ^ 5-      19# /       4# *x ^ 4+       1# /      12# *x ^ 3-       1# /      24# *x ^ 2-       1# /      12# *x
    CASE < 3#
      x =  3# - x
      Fine343H = -       7# /      12# *x ^ 7+      49# /      24# *x ^ 6-      29# /      12# *x ^ 5+      23# /      24# *x ^ 4
    CASE ELSE
      Fine343H = 0#
  END SELECT
END FUNCTION
'
' 3-times continuously differentiable
' Interpolates exactly up to x ^ 3
' Needs 3 intervals
' Uses polynoms up to order 6
' 
DEFDBL A-Z
FUNCTION Fine333G(t)
  x = abs(t)
  SELECT CASE x
    CASE < 1#
      x =  1# - x
      Fine333G =        7# /      24# *x ^ 6-       1# /       4# *x ^ 5-      19# /      24# *x ^ 4-       1# /       6# *x ^ 3+       5# /       4# *x ^ 2+       2# /       3# *x
    CASE < 2#
      x =  2# - x
      Fine333G = -       7# /      16# *x ^ 6+x ^ 5-       3# /       8# *x ^ 4+       1# /      12# *x ^ 3-       3# /      16# *x ^ 2-       1# /      12# *x
    CASE < 3#
      x =  3# - x
      Fine333G =        7# /      48# *x ^ 6-       3# /       8# *x ^ 5+      11# /      48# *x ^ 4
    CASE ELSE
      Fine333G = 0#
  END SELECT
END FUNCTION
'
' 3-times continuously differentiable
' Interpolates exactly up to x ^ 2
' Needs 2 intervals
' Uses polynoms up to order 7
' 
DEFDBL A-Z
FUNCTION Fine322H(t)
  x = abs(t)
  SELECT CASE x
    CASE < 1#
      x =  1# - x
      Fine322H = -       9# *x ^ 7+      63# /       2# *x ^ 6-      75# /       2# *x ^ 5+      15# *x ^ 4+       1# /       2# *x ^ 2+       1# /       2# *x
    CASE < 2#
      x =  2# - x
      Fine322H =        3# *x ^ 7-      21# /       2# *x ^ 6+      25# /       2# *x ^ 5-       5# *x ^ 4
    CASE ELSE
      Fine322H = 0#
  END SELECT
END FUNCTION
'
' 3-times continuously differentiable
' Interpolates exactly up to x ^ 1
' Needs 2 intervals
' Uses polynoms up to order 6
' 
DEFDBL A-Z
FUNCTION Fine312G(t)
  x = abs(t)
  SELECT CASE x
    CASE < 1#
      x =  1# - x
      Fine312G =        3# /       4# *x ^ 6-       3# /       2# *x ^ 5+       5# /       4# *x ^ 2+       1# /       2# *x
    CASE < 2#
      x =  2# - x
      Fine312G = -       3# /       4# *x ^ 6+       2# *x ^ 5-       5# /       4# *x ^ 4
    CASE ELSE
      Fine312G = 0#
  END SELECT
END FUNCTION
'
' 4-times continuously differentiable
' Interpolates exactly up to x ^ 3
' Needs 4 intervals
' Uses polynoms up to order 6
' 
DEFDBL A-Z
FUNCTION Fine434G(t)
  x = abs(t)
  SELECT CASE x
    CASE < 1#
      x =  1# - x
      Fine434G =       97# /     672# *x ^ 6+     151# /     672# *x ^ 5-     431# /     336# *x ^ 4-       1# /     336# *x ^ 3+     829# /     672# *x ^ 2+     153# /     224# *x
    CASE < 2#
      x =  2# - x
      Fine434G = -     237# /    1120# *x ^ 6+     221# /     672# *x ^ 5+      83# /     336# *x ^ 4-       1# /      21# *x ^ 3-      37# /     168# *x ^ 2-      27# /     280# *x
    CASE < 3#
      x =  3# - x
      Fine434G =       43# /     672# *x ^ 6-     167# /    1120# *x ^ 5+      11# /     336# *x ^ 4+      11# /     336# *x ^ 3+      11# /     672# *x ^ 2+      11# /    3360# *x
    CASE < 4#
      x =  4# - x
      Fine434G =       11# /    3360# *x ^ 6-      11# /    3360# *x ^ 5
    CASE ELSE
      Fine434G = 0#
  END SELECT
END FUNCTION
'
' 4-times continuously differentiable
' Interpolates exactly up to x ^ 2
' Needs 2 intervals
' Uses polynoms up to order 9
' 
DEFDBL A-Z
FUNCTION Fine422J(t)
  x = abs(t)
  SELECT CASE x
    CASE < 1#
      x =  1# - x
      Fine422J =       30# *x ^ 9-     135# *x ^ 8+     231# *x ^ 7-     357# /       2# *x ^ 6+     105# /       2# *x ^ 5+       1# /       2# *x ^ 2+       1# /       2# *x
    CASE < 2#
      x =  2# - x
      Fine422J = -      10# *x ^ 9+      45# *x ^ 8-      77# *x ^ 7+     119# /       2# *x ^ 6-      35# /       2# *x ^ 5
    CASE ELSE
      Fine422J = 0#
  END SELECT
END FUNCTION
'
' 4-times continuously differentiable
' Interpolates exactly up to x ^ 1
' Needs 2 intervals
' Uses polynoms up to order 7
' 
DEFDBL A-Z
FUNCTION Fine412H(t)
  x = abs(t)
  SELECT CASE x
    CASE < 1#
      x =  1# - x
      Fine412H =        3# *x ^ 7-      19# /       2# *x ^ 6+      21# /       2# *x ^ 5-       5# *x ^ 4+       3# /       2# *x ^ 2+       1# /       2# *x
    CASE < 2#
      x =  2# - x
      Fine412H = -x ^ 7+       5# /       2# *x ^ 6-       3# /       2# *x ^ 5
    CASE ELSE
      Fine412H = 0#
  END SELECT
END FUNCTION
