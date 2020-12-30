C***********************************************************************
C  SUBROUTINE BESLI0 : BESSEL(I0) FUNCTION IN DOUBLE PRECISION(EPS<1E-6)
C  CALL BESLI0(X,Y,ICON)
C  X     : INPUT VALUE
C  Y     : OUTPUT VALUE
C  ICON  : CONDITION CODE
C          =0 : NORMAL END
C          =1 : X>+80.0 OR X<-80 THEN SET Y=2.47518E33 TO AVOID OVERFLOW
C***********************************************************************
C
      SUBROUTINE BESLI0(X,Y,ICON)
C
      REAL    AX
      REAL*8  P1,P2,P3,P4,P5,P6,P7,Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,XX
      DATA    P1,P2,P3,P4,P5,P6,P7/
     &        1.0D0,        3.5156229D0, 3.0899424D0, 1.2067492D0,
     &        0.2659732D0,  0.360768D-1, 0.45813D-2  /
      DATA    Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9/
     &        0.39894228D0, 0.1328592D-1,0.225319D-2, -0.157565D-2,
     &        0.916281D-2 ,-0.2057706D-1,0.2635537D-1,-0.1647633D-1,
     &        0.392377D-2 /
C
C***********************************************************************
C
      XXMAX = 80.0
      AX = ABS(X)
      ICON = 0
C
C+++ ABS(X) > XXMAX
C
      IF(AX.GT.XXMAX) THEN
        ICON = 1
        Y = 2.47518E33
        GOTO 9000
      ENDIF
C
C+++ ABS(X) < XXMAX
C
      AX = ABS(X)
      IF (AX.LT.3.75) THEN
         XX= (X/3.75)**2
         Y = P1+XX*(P2+XX*(P3+XX*(P4+XX*(P5+XX*(P6+XX*P7)))))
      ELSE
         XX= 3.75/AX
         Y = (EXP(AX)/SQRT(AX))*(Q1+XX*(Q2+XX*(Q3+XX*(Q4+
     >       XX*(Q5+XX*(Q6+XX*(Q7+XX*(Q8+XX*Q9))))))))
      ENDIF
 9000 CONTINUE
      RETURN
      END
