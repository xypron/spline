' Copyright (c) 2013, Heinrich Schuchardt <xypron.glpk@gmx.de>
' All rights reserved.
'
' Redistribution and use in source and binary forms are permitted
' provided that the above copyright notice and this paragraph are
' duplicated in all such forms.
' THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
' IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
' WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.

DECLARE FUNCTION test(x AS DOUBLE) AS DOUBLE
DECLARE FUNCTION Spline133D(t AS DOUBLE) AS DOUBLE

PRINT "Testing Spline133D"

DIM d AS DOUBLE
DIM dmax AS DOUBLE
DIM i AS INTEGER
DIM j AS INTEGER
DIM t AS DOUBLE
DIM x AS DOUBLE
DIM y AS DOUBLE
DIM y_ AS DOUBLE
DIM s(10) AS DOUBLE

FOR i = 0 TO 10
  s(i) = test(i - 5)
NEXT i

RANDOMIZE TIMER, 3
dmax = 0
FOR i = 1 to 100
  x = 2# * RND(1) - 1#
  y = test(x)
  y_ = 0
  FOR j = -5 to 5
    t = x - j
    y_ = y_ + Spline133D(t) * s(j + 5)
  NEXT j
  d = ABS(y - y_)
  IF d > dmax THEN
    dmax = d
    IF dmax > 1E-10 THEN
      PRINT "Expected "; y
      PRINT "Actual "; y_
      END 1
    END IF
  END IF
NEXT i
PRINT "Maximum error ", dmax


FUNCTION test(x AS DOUBLE) AS DOUBLE
  test = 3.7 * x ^ 3 + 1.3 * x ^ 2 - 3.1 * x + 4.3
END FUNCTION