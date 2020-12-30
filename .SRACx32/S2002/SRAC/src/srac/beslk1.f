C***********************************************************************
C  SUBROUTINE BESLK1 : BESSEL(K1) FUNCTION IN DOUBLE PRECISION
C  CALL BESLK1(X,Y,ICON)
C  X     : INPUT VALUE
C  Y     : OUTPUT VALUE
C  ICON  : CONDITION CODE
C          =0 : NORMAL END
C          =1 : X>73.0       THEN SET Y=0.0 TO AVOID OVERFLOW
C          =2 : 0.0<=X<5E-19 THEN SET Y=1.0E30 INSTEAD OF INFINITY
C          =3 : X<0.0        THEN SET Y=0.0 (ERROR) 
C***********************************************************************
C
      SUBROUTINE BESLK1(X,Y,ICON)
C
      REAL*8  P1,P2,P3,P4,P5,P6,P7,Q1,Q2,Q3,Q4,Q5,Q6,Q7,XX
      REAL*8  R1,R2,R3,R4,R5,R6,R7
      DATA    P1,P2,P3,P4,P5,P6,P7/
     &        1.0D0,        0.15443144D0,-0.67278579D0,-0.18156897D0,
     &        -0.1919402D-1,-0.110404D-2,-0.4686D-4  /
      DATA    Q1,Q2,Q3,Q4,Q5,Q6,Q7/
     &        1.25331414D0, 0.23498619D0,-0.3655620D-1,0.1504268D-1,
     &        -0.780353D-2,0.325614D-2,-0.68245D-3   /
      DATA    R1,R2,R3,R4,R5,R6,R7/
     &        0.5D0,        0.87890594D0,0.51498869D0,0.15084934D0,
     &        0.2658733D-1, 0.301532D-2, 0.32411D-3  /
C
C***********************************************************************
C
      XXMAX = 73.0   
      XXMIN = 5.0E-19
      ICON = 0
C
C+++  X  > XXMAX
C
      IF(X.GT.XXMAX) THEN
        ICON = 1
        Y = 0.0   
        GOTO 9000
      ENDIF
C+++  0.0 <= X < XXMIN
C
      IF(X.GE.0.0 .AND. X.LT.XXMIN) THEN 
        ICON = 2
        Y = 1.0E30
        GOTO 9000
      ENDIF
C
C+++  X  < 0.0
C
      IF(X.LT.0.0) THEN
        ICON = 3
        Y = 0.0
        GOTO 9000
      ENDIF
C+++ XXMIN <= X <= XXMAX
C
      IF (X.LE.2.0) THEN
         XI= (X/3.75)**2
         YI= X*(R1+XI*(R2+XI*(R3+XI*(R4+XI*(R5+XI*(R6+XI*R7))))))
         XX= X*X/4.0
         Y =(LOG(X/2.0)*YI)+(1.0/X)*(P1+XX*(P2+XX*(P3+XX*(P4+
     >       XX*(P5+XX*(P6+XX*P7))))))
      ELSE
         XX= (2.0/X)
         Y = (EXP(-X)/SQRT(X))*(Q1+XX*(Q2+XX*(Q3+XX*(Q4+
     >       XX*(Q5+XX*(Q6+XX*Q7))))))
      ENDIF
 9000 CONTINUE
      RETURN
      END
