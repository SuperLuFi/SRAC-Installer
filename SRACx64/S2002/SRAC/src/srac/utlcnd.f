      SUBROUTINE UTLCND(IST,IMAX,LAPSE,NRGF,IC,A,B,C,D)
C
C     IC=0  B = SUM A
C     IC=1  D = SUM(A*C) /B  B : GIVEN
C     IC=2  D = SUM(A*B*C)/SUM(B*C)
C
      DIMENSION NRGF(1),A(1),B(1),C(1),D(1)
      DIMENSION WORK(200),WORK2(200),DWORK(200)
C
      IF(IC.EQ.1) GO TO 11
      IF(IC.GE.2) GO TO 31
C
      DO 5 I = 1,LAPSE
      B(I)   = 0.0
    5 CONTINUE
      DO 10 J=IST,IMAX
      N      = NRGF(J)
      B(N)   = B(N) + A(J)
   10 CONTINUE
      RETURN
C
   11 CONTINUE
      DO 15 I = 1,LAPSE
      D(I)    = 0.0
   15 CONTINUE
      DO 20 J =IST,IMAX
      WORK(J) = A(J)*C(J)
   20 CONTINUE
      DO 25 J =IST,IMAX
      N       = NRGF(J)
      D(N)    = D(N) + WORK(J)
   25 CONTINUE
      DO 30 N =1,LAPSE
      D(N)    = D(N)/B(N)
   30 CONTINUE
      RETURN
C
   31 CONTINUE
      DO 40 I = 1,LAPSE
      D(I)    = 0.0
      DWORK(I)= 0.0
   40 CONTINUE
      DO 45 J =IST,IMAX
      WORK(J) = A(J)*B(J)*C(J)
      WORK2(J)=      B(J)*C(J)
   45 CONTINUE
      DO 50 J =IST,IMAX
      N       = NRGF(J)
      D(N)    = D(N)     + WORK(J)
      DWORK(N)= DWORK(N) + WORK2(J)
   50 CONTINUE
C
      DO 60 N =  1,LAPSE
      SAVE    = DWORK(N)
      IF(SAVE.NE.0)  D(N)  = D(N) / SAVE
   60 CONTINUE
C
      RETURN
      END
