      SUBROUTINE LSCALE(IMAX3,IMAX,X,WITH,XWITH,LOGNX,ISTEP,IXMIN,AX1,
     *AX2,MSCALE,RATIO)
      REAL  X(IMAX3)
C     MAX,MIN,DEF
      IF(RATIO.LE.0.)   RATIO=5.0
      IF(MSCALE.EQ.1.AND.LOGNX.EQ.1)  GO TO  300
      IF(MSCALE.EQ.1.AND.LOGNX.EQ.0)  GO TO  260
      XMAX=-1.E-30
      XMIN=1.E+30
      DO 10 I=1,IMAX
      IF(X(I).GT.XMAX.AND.X(I).NE.0.)  XMAX=X(I)
      IF(X(I).LT.XMIN.AND.X(I).NE.0.)  XMIN=X(I)
   10 CONTINUE
      RATIOX=XMAX/XMIN
      IF(RATIOX.LE.RATIO.OR.XMAX.LE.0.)  GO TO  200
      XMAX=ALOG10(XMAX)
      XMIN=ALOG10(XMIN)
      IF(XMAX.GE.0.)  IXMAX=XMAX+0.995
      IF(XMAX.LT.0.)  IXMAX=XMAX
      IF(XMIN.GE.0.)  IXMIN=XMIN
      IF(XMIN.LT.0.)  IXMIN=XMIN-0.995
      ISTEP=IXMAX-IXMIN
  300 CONTINUE
      IF(MSCALE.EQ.1)  ISTEP=WITH/XWITH +0.3
      XWITH= WITH/FLOAT(ISTEP)
      DO 20 I=1,IMAX
      IF(X(I).LE.0.)   X(I)=0.
      IF(X(I).EQ.0.) GO TO 20
      X(I)=(ALOG10(X(I))-FLOAT(IXMIN))*XWITH
   20 CONTINUE
      LOGNX=1
      RETURN
  200 CONTINUE
      CALL  SCALE(X,WITH,IMAX,1,10.)
      AX1=X(IMAX+1)
      AX2=X(IMAX+2)
  260 CONTINUE
      DO  700 I=1,IMAX
      X(I)=(X(I)-AX1)/AX2
  700 CONTINUE
      LOGNX=0
      RETURN
      END