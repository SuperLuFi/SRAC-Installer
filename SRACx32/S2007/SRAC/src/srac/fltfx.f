C             FLTFX               LEVEL=1        DATE=80.09.29
      SUBROUTINE FLTFX(E1,NE1,N1)
C   *** FLTFX CONVERTS FLOATING NUMBER TO INTEGER
      N=-4
      E=ABS(E1)
      IF(E.NE.0.0)GO TO 1
      N=0
      NE=0
      GO TO 6
    3 E=E*10.0
      N=N-1
    1 IF(E.GE.1.0)GO TO 4
      GO TO 3
    5 E=E/10.0
      N=N+1
    4 IF(E.GE.10.0)GO TO 5
      E=E*10000.0
    2 NE=E
      E=E-FLOAT(NE)
      IF(E.LT.0.5)GO TO 6
      IF(NE.NE.99999)GO TO 7
      N=N+1
      NE=10000
      GO TO 6
    7 NE=NE+1
    6 NE1=NE
      N1=N
      RETURN
      END
