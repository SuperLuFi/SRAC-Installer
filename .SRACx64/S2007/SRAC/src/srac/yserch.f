      SUBROUTINE    YSERCH(     IL, IU,SMAX,SMIN, Y1  )
C
      DIMENSION  Y1(1)
      SMAX= 0.
      SMIN= 0.
      IF( IU*IL.EQ.0)  RETURN
      SMIN  = 1.0 E+30
      SMAX=-1.0E+30
C
      DO  100  I=IL,IU
      YY  = Y1(I)
      IF((YY.NE.0.). AND. (YY.GT.SMAX) )  SMAX= YY
      IF((YY.NE.0.). AND. (YY.LT.SMIN) )  SMIN= YY
  100 CONTINUE
      IF(SMIN.EQ.1.0E+30)  SMIN= 0.
      IF(SMAX.EQ.-1.0E+30)  SMAX=0.
      RETURN
      END
