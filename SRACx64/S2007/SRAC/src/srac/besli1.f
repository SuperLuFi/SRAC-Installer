C***********************************************************************
C  SUBROUTINE BESLI1 : BESSEL(I1) FUNCTION IN DOUBLE PRECISION
C  CALL BESLI1(X,Y,ICON)
C  X     : INPUT VALUE
C  Y     : OUTPUT VALUE
C  ICON  : CONDITION CODE
C          =0 : NORMAL END
C          =1 : X>+80.0 THEN SET Y=+2.45966E+33 TO AVOID OVERFLOW
C               X<-80.0 THEN SET Y=-2.45966E+33 TO AVOID OVERFLOW
C***********************************************************************
C
      SUBROUTINE BESLI1(X,Y,ICON)
C
      REAL    AX
      REAL*8  P1,P2,P3,P4,P5,P6,P7,Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,XX
      DATA    P1,P2,P3,P4,P5,P6,P7/
     &        0.5D0,        0.87890594D0,0.51498869D0,0.15084934D0,
     &        0.2658733D-1, 0.301532D-2, 0.32411D-3  /
      DATA    Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9/
     &        0.39894228D0,-0.3988024D-1,-0.362018D-2, 0.163801D-2,
     &       -0.1031555D-1, 0.2282967D-1,-0.2895312D-1,0.1787654D-1,
     &       -0.420059D-2 /
C
C***********************************************************************
C
C
      XXMAX = 80.0
      AX = ABS(X)
      ICON = 0
C
C+++ ABS(X) > XXMAX
C
      IF(AX.GT.XXMAX) THEN
        ICON = 1
        Y = 2.45966E+33
        IF (X.LT.0.) Y = -2.45966E+33
        GOTO 9000
      ENDIF
C
C+++ ABS(X) < XXMAX
C
      IF (AX.LT.3.75) THEN
         XX= (X/3.75)**2
         Y = X*(P1+XX*(P2+XX*(P3+XX*(P4+XX*(P5+XX*(P6+XX*P7))))))
      ELSE
         XX= 3.75/AX
         Y = (EXP(AX)/SQRT(AX))*(Q1+XX*(Q2+XX*(Q3+XX*(Q4+
     >       XX*(Q5+XX*(Q6+XX*(Q7+XX*(Q8+XX*Q9))))))))
         IF (X.LT.0.) Y = -Y
      ENDIF
 9000 CONTINUE
      RETURN
      END
