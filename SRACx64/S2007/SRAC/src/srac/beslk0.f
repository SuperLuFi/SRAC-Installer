C***********************************************************************
C  SUBROUTINE BESLK0 : BESSEL(K0) FUNCTION IN DOUBLE PRECISION
C  CALL BESLK0(X,Y,ICON)
C  X     : INPUT VALUE
C  Y     : OUTPUT VALUE
C  ICON  : CONDITION CODE
C          =0 : NORMAL END
C          =1 : X>73.0       THEN SET Y=0.0 TO AVOID OVERFLOW
C          =2 : 0.0<=X<5E-19 THEN SET Y=1.0E30 INSTEAD OF INFINITY
C          =3 : X<0.0        THEN SET Y=0.0 (ERROR)
C***********************************************************************
C
      SUBROUTINE BESLK0(X,Y,ICON)
C
      REAL*8  P1,P2,P3,P4,P5,P6,P7,Q1,Q2,Q3,Q4,Q5,Q6,Q7,XX
      REAL*8  R1,R2,R3,R4,R5,R6,R7
      DATA    P1,P2,P3,P4,P5,P6,P7/
     &        -0.57721566D0,0.42278420D0,0.23069756D0,0.3488590D-1,
     &        0.262698D-2,  0.10750D-3,  0.74D-5     /
      DATA    Q1,Q2,Q3,Q4,Q5,Q6,Q7/
     &        1.25331414D0,-0.7832358D-1,0.2189568D-1,-0.1062446D-1,
     &        0.587872D-2, -0.251540D-2, 0.53208D-3  /
      DATA    R1,R2,R3,R4,R5,R6,R7/
     &        1.0D0,        3.5156229D0, 3.0899424D0, 1.2067492D0,
     &        0.2659732D0,  0.360768D-1, 0.45813D-2  /
C
C***********************************************************************
C
      XXMAX = 73.0 
      XXMIN = 5.0E-19
      ICON = 0
C
C+++   X  > XXMAX
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
C
C+++ XXMIN <=  X  <= XXMAX
C
      IF (X.LE.2.0) THEN
         XI= (X/3.75)**2
         YI= R1+XI*(R2+XI*(R3+XI*(R4+XI*(R5+XI*(R6+XI*R7)))))
         XX= X*X/4.0
         Y = (-LOG(X/2.0)*YI)+(P1+XX*(P2+XX*(P3+XX*(P4+
     >       XX*(P5+XX*(P6+XX*P7))))))
      ELSE
         XX= (2.0/X)
         Y = (EXP(-X)/SQRT(X))*(Q1+XX*(Q2+XX*(Q3+XX*(Q4+
     >       XX*(Q5+XX*(Q6+XX*Q7))))))
      ENDIF
 9000 CONTINUE
      RETURN
      END
