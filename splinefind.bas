' Copyright (c) 2013, Heinrich Schuchardt <xypron.glpk@gmx.de>
' All rights reserved.
'
' Redistribution and use in source and binary forms are permitted
' provided that the above copyright notice and this paragraph are
' duplicated in all such forms.
' THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
' IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
' WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.

DECLARE FUNCTION Kuerzen (i as INTEGER) AS INTEGER
DECLARE FUNCTION Gauss () AS DOUBLE
DECLARE FUNCTION Gauss2 () AS DOUBLE
DECLARE SUB Help ()
DECLARE SUB SpeicherLoesung ()
DECLARE SUB ZeigLoesung ()
DECLARE SUB matrix (Text AS STRING)
DECLARE SUB Matrixc (s AS INTEGER, z AS INTEGER)
DECLARE SUB MatrixZeile (Zeile AS INTEGER)
DECLARE SUB MatrixKorpus ()
DECLARE SUB MatrixKopf ()
DECLARE FUNCTION idx (iExp AS INTEGER, iInt AS INTEGER) AS INTEGER

CONST cExp = 15 ' höchster betrachteter Exponent
CONST cnInt = 4 ' maximale Anzahl Intervalle
DIM SHARED anzZeilen AS INTEGER
DIM SHARED AnzSpalten AS INTEGER

DIM SHARED nGrd AS INTEGER ' Grad der Interpolation
DIM SHARED nExp AS INTEGER ' höchster betrachteter Exponent
DIM SHARED nint AS INTEGER ' maximale Anzahl Intervalle
DIM SHARED nc   AS INTEGER ' nC * stetig differenzierbar

REDIM SHARED txtExp(0) AS STRING
REDIM SHARED a(0, 0) AS DOUBLE
REDIM SHARED b(0)  AS DOUBLE
REDIM SHARED Nenner(0)  AS DOUBLE
REDIM SHARED zaehler(0) AS DOUBLE
DIM PascalTriangle(cExp, cExp) AS INTEGER
DIM Ident(cExp, cExp) AS INTEGER

DIM i AS INTEGER
DIM j AS INTEGER
DIM nExpMax AS INTEGER
DIM Zeile AS INTEGER
DIM iGrd AS INTEGER
DIM iExp AS INTEGER
DIM iInt AS INTEGER
DIM x AS DOUBLE
DIM iC AS INTEGER
DIM iFlag as INTEGER

IF LEN(COMMAND$(1)) = 0 THEN
  help
  END
ENDIF

OPEN COMMAND$(1) FOR OUTPUT AS 1
PRINT #1, "' Copyright (c) 2013, Heinrich Schuchardt <xypron.glpk@gmx.de>"
PRINT #1, "' All rights reserved."
PRINT #1, "'"
PRINT #1, "' Redistribution and use in source and binary forms are permitted"
PRINT #1, "' provided that the above copyright notice and this paragraph are"
PRINT #1, "' duplicated in all such forms."
PRINT #1, "' THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR"
PRINT #1, "' IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED"
PRINT #1, "' WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE."
PRINT #1, ""

PascalTriangle(0, 0) = 1
Ident(0, 0) = 1
FOR i = 1 TO cExp
  PascalTriangle(i, 0) = 1
  Ident(i, i) = 1
  FOR j = 1 TO i
    PascalTriangle(i, j) = PascalTriangle(i - 1, j) - PascalTriangle(i - 1, j - 1)
  NEXT j
NEXT i
CLS
PRINT "[PascalTriangle]"
FOR i = 0 TO cExp
  FOR j = 0 TO cExp
    PRINT USING "#####"; PascalTriangle(i, j);
  NEXT j
  PRINT
NEXT i

FOR nc = 0 TO 4 'nC * stetig differenzierbar
  nExpMax = cExp
  FOR nGrd = 4 TO 1 STEP -1 'Grad der Interpolation
    FOR nint = 1 TO cnInt 'maximale Anzahl Intervalle
      FOR nExp = nGrd TO nExpMax 'höchster betrachteter Exponent
        AnzSpalten = idx(nExp, nint - 1)
        anzZeilen = (nGrd + 1) * (nExp + 1) + (nc + 1) * (nint + 1) + nint
        REDIM Nenner(AnzSpalten)
        REDIM zaehler(AnzSpalten)
        REDIM a(AnzSpalten, anzZeilen)
        REDIM b(anzZeilen)
        REDIM txtExp(idx(nExp, nint)) AS STRING

        PRINT "Spline"; CHR$(48 + nc); CHR$(48 + nGrd); CHR$(48 + nint); CHR$(65 + nExp)
        'PRINT "[Bezeichnung]"
        FOR i = 0 TO nint - 1
          FOR j = 0 TO nExp
            txtExp(idx(j, i)) = CHR$(65 + i) + CHR$(48 + j)
          NEXT j
        NEXT i
        'MatrixKopf

        Zeile = 0
        'PRINT "[Interpolation]"
        FOR iGrd = 0 TO nGrd
          'PRINT "x ^"; iGrd
          FOR iExp = 0 TO nExp

            'Alle Aij auf 0
            FOR j = 0 TO nExp
              FOR iInt = 0 TO nint - 1
                a(idx(j, iInt), Zeile) = 0
              NEXT iInt
            NEXT j

            FOR j = 0 TO nint - 1
              FOR i = 0 TO nExp
                x = (j + 1) ^ iGrd
                a(idx(i, j), Zeile) = a(idx(i, j), Zeile) + x * Ident(i, iExp)
                x = (-j) ^ iGrd
                a(idx(i, j), Zeile) = a(idx(i, j), Zeile) + x * PascalTriangle(i, iExp)
              NEXT i
            NEXT j

            IF iGrd = iExp THEN
              b(Zeile) = 1
            ELSE
              b(Zeile) = 0
            END IF

            'MatrixZeile (Zeile)
            Zeile = Zeile + 1
          NEXT iExp
        NEXT iGrd

        'PRINT "[Stetigkeit]"
        FOR iC = 0 TO nc
          'PRINT "x ^"; iC
          FOR iInt = 0 TO nint
            FOR j = 0 TO nExp
              FOR i = 0 TO nint - 1
                a(idx(j, i), Zeile) = 0
              NEXT i
              SELECT CASE iInt
                CASE 0
                  IF 0 <> iC MOD 2 THEN
                    a(idx(j, iInt), Zeile) = PascalTriangle(j, iC)
                  END IF
                CASE nint
                  a(idx(j, iInt - 1), Zeile) = Ident(iC, j)
                CASE ELSE
                  a(idx(j, iInt - 1), Zeile) = -Ident(iC, j)
                  a(idx(j, iInt), Zeile) = ABS(PascalTriangle(j, iC))
              END SELECT
            NEXT j
            
            'MatrixZeile (Zeile)
            Zeile = Zeile + 1
          NEXT iInt
        NEXT iC

        'PRINT "[Nullstellen]"
          FOR iInt = 0 TO nint - 1
            FOR j = 0 TO nExp
              FOR i = 0 TO nint - 1
                 a(idx(j, i), Zeile) = 0
              NEXT i
            NEXT j
            a(idx(0, iInt), Zeile) = 1
           
            'MatrixZeile (Zeile)
            Zeile = Zeile + 1
          NEXT iInt
        


        anzZeilen = Zeile
        'Matrix "Ausgang"
        iflag = Gauss2
        'iFlag = -1
          IF iflag = -1 THEN
            iflag = Gauss
          END IF
        IF iflag THEN
          EXIT FOR
        END IF

      NEXT nExp

      IF iflag = -1 THEN
        EXIT FOR
      END IF

    NEXT nint

  NEXT nGrd
NEXT nc
CLOSE

FUNCTION Gauss AS DOUBLE
  DIM i AS INTEGER
  DIM iflag AS INTEGER
  DIM j AS INTEGER
  DIM k AS INTEGER
  DIM amax AS DOUBLE
  DIM zPivot AS DOUBLE
  
  IF anzZeilen < AnzSpalten THEN
    COLOR 12
    PRINT "Unterbestimmt"
    COLOR 7
    EXIT FUNCTION
  END IF
  FOR i = 0 TO AnzSpalten
    'Zeilepivotisierung
    'Suche größte Zeile
    amax = a(i, i)
    zPivot = i
    FOR k = i + 1 TO anzZeilen
      IF ABS(a(i, k)) > amax THEN
        amax = ABS(a(i, k))
        zPivot = k
      END IF
    NEXT k
    IF amax <> 0 THEN
      FOR j = i TO AnzSpalten
        SWAP a(j, i), a(j, zPivot)
      NEXT j
      SWAP b(i), b(zPivot)
      ' Matrix "Pivot"

      IF 0 = Kuerzen(i) THEN
        PRINT "Finde keine rationale Lösung."
        iflag = 2
        EXIT FUNCTION
      END IF
      
      FOR k = i + 1 TO anzZeilen
        FOR j = i + 1 TO AnzSpalten
          a(j, k) = a(i, i) * a(j, k) - a(j, i) * a(i, k)
        NEXT j
        b(k) = a(i, i) * b(k) - a(i, k) * b(i)
        a(i, k) = 0
      NEXT k
    END IF
    'Matrix "Gaussschritt"
  NEXT i
      IF 0 = Kuerzen(0) THEN
        PRINT "Finde keine rationale Lösung."
        iflag = 2
        EXIT FUNCTION
      END IF
  'Matrix "Ende"

  'Ergebnis bestimmen
  FOR i = anzZeilen TO AnzSpalten + 1 STEP -1
    IF b(i) THEN
      COLOR 12
      PRINT "keine Lösung"
      Gauss = 0
      COLOR 7
      EXIT FUNCTION
    END IF
  NEXT i
  FOR i = AnzSpalten TO 0 STEP -1
    Nenner(i) = a(i, i)
    zaehler(i) = b(i)
    IF b(i) / 101 = INT(b(i) / 101) AND b(i) <> 0 THEN STOP
    SELECT CASE Nenner(i)
      CASE IS < 0
        Nenner(i) = -Nenner(i)
        zaehler(i) = -zaehler(i)
      CASE 0
        COLOR 12
        PRINT "mehrdeutige Lösung"
        Gauss = 2
        COLOR 7
        EXIT FUNCTION
    END SELECT
    FOR j = i - 1 TO 0 STEP -1
      FOR k = j TO i - 1
        a(k, j) = a(k, j) * Nenner(i)
      NEXT k
      b(j) = b(j) * Nenner(i) - a(i, j) * zaehler(i)
      a(i, j) = 0
    NEXT j
      IF 0 = Kuerzen(0) THEN
        PRINT "Finde keine rationale Lösung."
        iflag = 2
        EXIT FUNCTION
      END IF
  NEXT i
  SpeicherLoesung
  Gauss = -1
END FUNCTION

FUNCTION Gauss2 AS DOUBLE
  DIM i AS INTEGER
  DIM iflag AS INTEGER
  DIM j AS INTEGER
  DIM k AS INTEGER
  DIM l AS INTEGER
  DIM amax AS DOUBLE
  DIM p AS DOUBLE
  DIM zPivot AS INTEGER
  DIM aa(AnzSpalten, anzZeilen) AS DOUBLE
  DIM bb(anzZeilen) AS DOUBLE
  FOR i = 0 TO anzZeilen
    FOR j = 0 TO AnzSpalten
      aa(j, i) = a(j, i)
    NEXT j
    bb(i) = b(i)
  NEXT i

  IF anzZeilen < AnzSpalten THEN
    COLOR 12
    PRINT "Unterbestimmt"
    COLOR 7
    EXIT FUNCTION
  END IF
  FOR i = 0 TO AnzSpalten
    'Zeilepivotisierung
    'Suche größte Zeile
    amax = aa(i, i)
    zPivot = i
    FOR k = i + 1 TO anzZeilen
      IF ABS(aa(i, k)) > amax THEN
        amax = ABS(aa(i, k))
        zPivot = k
      END IF
    NEXT k
    IF amax <> 0 THEN
      FOR j = i TO AnzSpalten
        SWAP aa(j, i), aa(j, zPivot)
      NEXT j
      SWAP bb(i), bb(zPivot)
      ' Matrix "Pivot"
  p = aa(i, i)
  FOR k = i TO anzZeilen
    FOR l = i TO AnzSpalten
      aa(l, k) = aa(l, k) / p
    NEXT l
    bb(k) = bb(k) / p
  NEXT k
     
      FOR k = i + 1 TO anzZeilen
        FOR j = i + 1 TO AnzSpalten
          aa(j, k) = aa(j, k) - aa(j, i) * aa(i, k)
        NEXT j
        bb(k) = bb(k) - aa(i, k) * bb(i)
        aa(i, k) = 0
      NEXT k
    END IF
    'Matrix "Gaussschritt"
  NEXT i
      IF 0 = Kuerzen(0) THEN
        PRINT "Finde keine rationale Lösung."
        iflag = 2
        EXIT FUNCTION
      END IF
  'Ergebnis bestimmen
  FOR i = anzZeilen TO AnzSpalten + 1 STEP -1
    IF ABS(bb(i)) >= .0000001 THEN
      COLOR 11
      PRINT "keine Lösung"
      Gauss2 = 0
      COLOR 7
      EXIT FUNCTION
    END IF
  NEXT i
  IF ABS(a(AnzSpalten, AnzSpalten)) < .0000001 THEN
      COLOR 11
      PRINT "mehrdeutige Lösung"
      Gauss2 = 0
      COLOR 7
      'EXIT FUNCTION
    
  END IF

  Gauss2 = -1
END FUNCTION

FUNCTION idx (iExp AS INTEGER, iInt AS INTEGER) AS INTEGER
  idx = iExp + iInt * (nExp + 1)
END FUNCTION

FUNCTION Kuerzen (i AS INTEGER) AS INTEGER
  DIM amax AS DOUBLE
  DIM c AS DOUBLE
  DIM iflag AS INTEGER
  DIM j AS DOUBLE
  DIM k AS INTEGER
  DIM l AS INTEGER
  
  ' Kürzen
  FOR k = i TO anzZeilen
    DO
      iflag = 0
      j = 2
      amax = 0#
      FOR l = i TO AnzSpalten
        c = ABS(a(l, k))
        IF c > amax THEN
          amax = c
          iflag = -1
        END IF
      NEXT l
      c = ABS(b(k))
      IF c > amax THEN
        amax = c
        iflag = -1
      END IF
      
        FOR l = i TO AnzSpalten
          c = ABS(a(l, k))
          IF c > 0 AND c < amax THEN
            amax = c
          END IF
        NEXT l
        c = ABS(b(k))
        IF c > 0 AND c < amax THEN
          amax = c
        END IF
        IF amax <= 1 THEN
          iflag = 0
        END IF
      IF iflag = 0 THEN EXIT DO
      DO
        iflag = -1
        FOR l = i TO AnzSpalten
          IF INT(a(l, k) / j) <> a(l, k) / j THEN
            iflag = 0
          END IF
        NEXT l
        IF INT(b(k) / j) <> b(k) / j THEN
          iflag = 0
        END IF
        IF iflag THEN
            FOR l = i TO AnzSpalten
              a(l, k) = a(l, k) / j
            NEXT l
            b(k) = b(k) / j
            EXIT DO
        END IF
        DO WHILE INT(amax / j) = amax / j
          amax = amax / j
        LOOP
        IF amax >= 1E+14 THEN
          Kuerzen = 0
          EXIT FUNCTION
        END IF
        SELECT CASE j
          CASE IS >= amax
            iflag = 0
            EXIT DO
          CASE 2
            j = j + 1
          CASE IS > SQR(amax) + 1
            j = amax
          CASE IS < amax
            j = j + 2
          CASE ELSE
            j = amax
        END SELECT
      LOOP
    LOOP WHILE iflag
  NEXT k
  'Matrix "Krzen"
  Kuerzen = -1
END FUNCTION

SUB matrix (Text AS STRING)
  CLS
  MatrixKopf
  MatrixKorpus
  PRINT Text
  PRINT "Anzahl Gleichungen:"; anzZeilen
  DO
  LOOP UNTIL LEN(INKEY$)
END SUB

SUB Matrixc (s AS INTEGER, z AS INTEGER)
  DIM i AS INTEGER
  DIM j AS INTEGER
  DIM Zeile AS INTEGER
  CLS
  MatrixKopf
  FOR Zeile = 0 TO anzZeilen - 1
    FOR i = 0 TO nint - 1
      FOR j = 0 TO nExp
        IF s = idx(j, i) AND Zeile = z THEN
          COLOR 12
        END IF
        PRINT USING "####"; a(idx(j, i), Zeile);
        COLOR 7
      NEXT j
    NEXT i
    PRINT USING "| ####"; b(Zeile)
  NEXT Zeile
  PRINT "Anzahl Gleichungen:"; anzZeilen
  DO
  LOOP UNTIL LEN(INKEY$)
END SUB

SUB MatrixKopf
  DIM i AS INTEGER
  DIM j AS INTEGER
  FOR i = 0 TO nint - 1
    FOR j = 0 TO nExp
      PRINT USING "  \\"; txtExp(idx(j, i));
    NEXT j
  NEXT i
  PRINT "| X   "
END SUB

SUB MatrixKorpus
  DIM Zeile AS INTEGER
  
  FOR Zeile = 0 TO 17'anzZeilen - 1
    MatrixZeile (Zeile)
  NEXT Zeile
END SUB

SUB MatrixZeile (Zeile AS INTEGER)
  DIM i AS INTEGER
  DIM j AS INTEGER
  
  FOR i = 0 TO nint - 1
    FOR j = 0 TO nExp
      PRINT USING "####"; a(idx(j, i), Zeile);
      '      SELECT CASE a(idx(j, i), Zeile)
      '        CASE IS < -.0000000001#
      '          PRINT " -";
      '        CASE IS > .0000000001#
      '          PRINT " +";
      '        CASE ELSE
      '          PRINT " 0";
      '      END SELECT
    NEXT j
  NEXT i
  PRINT USING "| ####"; b(Zeile);
  '  SELECT CASE b(Zeile)
  '        CASE IS < -.0000000001#
  '          PRINT "| m ";
  '        CASE IS > .0000000001#
  '          PRINT "| + ";
  '        CASE ELSE
  '          PRINT "| 0 ";
  '      END SELECT
  PRINT ":"; Zeile
END SUB

SUB SpeicherLoesung
  DIM i AS INTEGER
  DIM iflag AS INTEGER
  DIM j AS INTEGER
  DIM jflag AS INTEGER
  DIM k AS INTEGER
  PRINT CHR$(12)
  IF nc > 0 THEN
    PRINT "'"; USING "##"; nc;
    PRINT "-times continuously differentiable"
  ELSE
    PRINT "' Continous"
  END IF
  PRINT "' Interpolates exactly up to x ^"; nGrd
  PRINT "' Needs"; nint; " intervals"
  PRINT "' Uses polynoms up to order"; nExp
  PRINT "' "
  PRINT "FUNCTION Spline"; CHR$(48 + nc); CHR$(48 + nGrd); CHR$(48 + nint); CHR$(65 + nExp)
  PRINT #1, "'"
  IF nc > 0 THEN
    PRINT #1, "'"; USING "##"; nc;
    PRINT #1, "-times continuously differentiable"
  ELSE
    PRINT #1, "' Continous"
  END IF
  PRINT #1, "' Interpolates exactly up to x ^"; nGrd
  PRINT #1, "' Needs"; nint; " intervals"
  PRINT #1, "' Uses polynoms up to order"; nExp
  PRINT #1, "' "
  PRINT #1, "FUNCTION Spline"; CHR$(48 + nc); CHR$(48 + nGrd); CHR$(48 + nint);
  PRINT #1, CHR$(65 + nExp); "(t AS DOUBLE) AS DOUBLE"
  PRINT #1, "  DIM x AS DOUBLE"
  PRINT #1, "  x = abs(t)"
  jflag = 0
  FOR j = 0 TO nint - 1
    IF jflag = 0 THEN
      PRINT #1, "  IF x <";
      jflag = 1
    ELSE
      PRINT #1, "  ELSEIF x <";
    ENDIF
    PRINT #1, USING "##"; j + 1;
    PRINT #1, "# THEN"
    PRINT #1, "    x = ";
    PRINT #1, USING "##"; j + 1;
    PRINT #1, "# - x"
    PRINT #1, "    Spline"; CHR$(48 + nc); CHR$(48 + nGrd); CHR$(48 + nint); CHR$(65 + nExp); " =";
    iflag = 0
    FOR k = nExp TO 0 STEP -1
      i = idx(k, j)
      PRINT #1, " ";
      IF zaehler(i) THEN
        IF zaehler(i) < 0 THEN
          PRINT #1, "-";
        ELSE
          IF iflag THEN
            PRINT #1, "+";
          END IF
        END IF
        iflag = -1
        IF Nenner(i) > 1 THEN
          PRINT #1, ABS(zaehler(i));
          PRINT #1, "# /";
          PRINT #1, Nenner(i);
          IF k >= 1 THEN
            PRINT #1, "# * ";
          ELSE
            PRINT #1, "#";
          END IF
        ELSEIF ABS(zaehler(i)) > 1 THEN
          PRINT #1, ABS(zaehler(i));
          IF k >= 1 THEN
            PRINT #1, "# * ";
          ELSE
            PRINT #1, "#";
          END IF
        ELSE
          PRINT #1, " ";
        END IF
        SELECT CASE k
          CASE 0
          CASE 1
            PRINT #1, "x";
          CASE ELSE
            PRINT #1, "x ^"; k;
        END SELECT
      END IF
    NEXT k
    PRINT #1,
  NEXT j
  PRINT #1, "  ELSE"
  PRINT #1, "    Spline"; CHR$(48 + nc); CHR$(48 + nGrd); CHR$(48 + nint); CHR$(65 + nExp); " = 0#"
  PRINT #1, "  END IF"
  PRINT #1, "END FUNCTION"
END SUB

SUB SpiegelPunkte
  DIM dx AS DOUBLE
  DIM i AS INTEGER
  DIM iPas(15) AS INTEGER
  DIM j AS INTEGER
  DIM sum AS DOUBLE

  CLS
  iPas(0) = -1
  FOR i = 1 TO 11
    FOR j = 15 TO 1 STEP -1
      iPas(j) = iPas(j) - iPas(j - 1)
      PRINT USING "#####"; iPas(j);
    NEXT j
    PRINT USING "#####"; iPas(j)
  NEXT i
  dx = 0
  FOR i = 0 TO 10
    sum = 0
    FOR j = 1 TO 15
      sum = sum + iPas(j) * (j + dx) ^ i
    NEXT j
    PRINT USING "##"; i;
    PRINT ":"; sum - dx ^ i
  NEXT i

END SUB

SUB ZeigLoesung
  DIM i AS INTEGER
  DIM j AS INTEGER
  DIM k AS INTEGER
  DIM iflag AS INTEGER
  FOR j = 0 TO nint - 1
    PRINT CHR$(65 + j); "= ";
    iflag = 0
    FOR k = nExp TO 0 STEP -1
      i = idx(k, j)

      IF zaehler(i) THEN
        IF zaehler(i) < 0 THEN
          PRINT TAB(4); "-";
        ELSE
          IF iflag THEN
            PRINT TAB(4); "+";
          END IF
        END IF
        iflag = -1
        IF Nenner(i) > 1 THEN
          PRINT ABS(zaehler(i)); "/"; Nenner(i); "* ";
        ELSE
          IF ABS(zaehler(i) > 1) THEN
            PRINT ABS(zaehler(i)); "* ";
          END IF
        END IF
        SELECT CASE k
          CASE 0
          CASE 1
            PRINT "x";
          CASE ELSE
            PRINT "x ^"; k;
        END SELECT
      END IF
    NEXT k
    PRINT
  NEXT j
END SUB

SUB Help
  PRINT "USAGE: splinefind filename.bas"
END SUB