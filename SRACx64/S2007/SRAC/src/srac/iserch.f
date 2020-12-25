      SUBROUTINE   ISERCH(IJ,EL,EU,IL,IU,EMAX,EMIN,     X1, ICUMN,
     1 IMAX1   )
      DIMENSION    X1(1),        ICUMN(1), IMAX1(1)
C
      IL= 0
      IU= 0
      IK= IJ
      IMI= ICUMN(IK)
      INI= IMAX1(IK)
      IZI= IMI + INI - 1
      EMAX = 0.
      EMIN = 0.
      IF(EU.LT.X1(IMI) ) RETURN
      IF(EL.GT.X1(IZI) ) RETURN
      IMAXM1= INI-1
      IF(IMAXM1.EQ.0)  GO TO  201
      DO   100  I=1,IMAXM1
      I1=IMI+I
C      I1=IMI+I-1
      I0  = I1 -1
      A1  = X1(I0)
      A2  = X1(I1)
      IF( EL.GE.A1.AND.EL.LT.A2.AND.A2.LE.EU)  IL=I0
      IF( EU.GE.A1.AND.EU.LT.A2.AND.A1.GE.EL)  IU=I0
C     IF( EL.GE.A1.AND.EL.LT.A2)   IL=I0
C     IF( EU.GE.A1.AND.EU.LT.A2)   IU=I0
  100 CONTINUE
      GO TO  200
  201 CONTINUE
      A1=X1(IMI)
      IF(.NOT.(A1.GE.EL.AND.A1.LE.EU) )RETURN
      IL=IMI
      IU=IMI
       EMAX=A1
      EMIN=A1
      RETURN
  200 CONTINUE
      IF( (IL.NE.0).AND.(X1(IL).NE.EL) )  IL=IL+1
      IF(X1(IMI).GE.EL.AND.X1(IMI).LE.EU)  IL=IMI
      IF(X1(IZI).GE.EL.AND.X1(IZI).LE.EU)  IU=IZI
C     IF(  EL.LE.X1(IMI) )    IL=IMI
C     IF(  EU.GE.X1(IZI) )    IU=IZI
      IF(  IL*IU.EQ.0    )    RETURN
      EMAX = X1(IU)
      EMIN = X1(IL)
      RETURN
      END
