C
C     IC=0  B = SUM A
C     IC=1  D = SUM(A*C) /B  B : GIVEN
C     IC=2  D = SUM(A*C*E)/SUM(A*C)
C
C     ARRAY A,C,E FOR FINE GROUPS ARRAY B,D FOR COARSE GROUPS
C     ARRAY NEGF CONTAINS THE COARSE GROUP NO. OF THE FINE GROUP
C     NEF  NUMBER OF FINE   GROUPS
C
      SUBROUTINE COLLAP(NEF,NRGF,IC,A,B,C,D,E)
C
      DIMENSION NRGF(NEF),A(1),B(1),C(1),D(1),E(1)
C
C     NMAX=0
C     DO 5 J=1,NEF
C     N=NRGF(J)
C     IF(N.EQ.0) GO TO 5
C     NMAX=MAX0(NMAX,N)
C     IF (IC.NE.0) D(N)=0.
C     IF (IC.NE.1) B(N)=0.
C   5 CONTINUE
C     DO 10 J=1,NEF
C     N=NRGF(J)
C     IF(N.EQ.0) GO TO 10
C     IF (IC.EQ.0) B(N)=B(N)+A(J)
C     IF (IC.EQ.1) D(N)=D(N)+A(J)*C(J)
C     IF (IC.EQ.2) D(N)=D(N)+A(J)*C(J)*E(J)
C     IF (IC.EQ.2) B(N)=B(N)+A(J)*C(J)
C  10 CONTINUE
C     IF (IC.EQ.0) RETURN
C     DO 15 N=1,NMAX
C     IF (B(N).NE.0.)
C    *D(N)=D(N)/B(N)
C  15 CONTINUE
C     RETURN
C     END
C
C
C-----NEW VERSION
C
      NMAX   = 0
      DO 5 J = 1 ,NEF
      N      = NRGF(J)
      IF(N.LE.0) GO TO 5
      NMAX   = MAX0(NMAX,N)
    5 CONTINUE
C
      IF(IC.EQ.0) THEN
                  DO 10 J = 1 , NMAX
                  B(J)    = 0.0
   10             CONTINUE
                  DO 15 J = 1 , NEF
                  N       = NRGF(J)
                  IF(N.LE.0) GO TO 15
                  B(N)    =  B(N) + A(J)
   15             CONTINUE
                  RETURN
                  ENDIF
C
      IF(IC.EQ.1) THEN
                  DO 20 J = 1 , NMAX
                  D(J)    = 0.0
   20             CONTINUE
                  DO 25 J = 1 , NEF
                  N       = NRGF(J)
                  IF(N.LE.0) GO TO 25
                  D(N)    =  D(N) + A(J)*C(J)
   25             CONTINUE
                  DO 26 N = 1 , NMAX
                  IF(B(N).NE.0.0)   D(N) = D(N)/B(N)
   26             CONTINUE
                  RETURN
                  ENDIF
C
      IF(IC.EQ.2) THEN
                  DO 30 J = 1 , NMAX
                  B(J)    = 0.0
                  D(J)    = 0.0
   30             CONTINUE
                  DO 35 J = 1 , NEF
                  N       = NRGF(J)
                  IF(N.LE.0) GO TO 35
                  B(N)    = B(N) + A(J)*C(J)
                  D(N)    = D(N) + A(J)*C(J)*E(J)
   35             CONTINUE
                  DO 40 N = 1 , NMAX
                  IF(B(N).NE.0.0)   D(N) = D(N)/B(N)
   40             CONTINUE
                  ENDIF
C
      RETURN
      END
