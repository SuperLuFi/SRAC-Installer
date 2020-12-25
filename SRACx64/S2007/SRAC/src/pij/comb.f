C  ***********  MODULE  TRKCMB  ***************************************
      SUBROUTINE COMB(LL1,LL2,D1,D2,IM1,IM2,IP1,IP2,IER)
C  ********************************************************************
C     COMBINE TWO SETS OF D,IM,IP ARRAYS INTO ONE SET#1
C  ********************************************************************
      DIMENSION D1(*),D2(*),IM1(*),IM2(*),IP1(*),IP2(*)
C
      X=D2(1)
      Y=D2(LL2)
C     WRITE(6,*)'X=',X,'Y=',Y
    2 L=0
   30 L=L+1
C     WRITE(6,*)'D1(L)=',D1(L)
      IF(ABS(D1(L)-X).LT. 1.0E-3) GO TO 40
      IF(D1(L).LT.X) GO TO 30
      N=1
      GO TO 50
   40 N=0
   45 LP=L
C  50 WRITE(6,*)'D1(L)=',D1(L),L
   50 IF(ABS(D1(L)-Y).LT. 1.0E-3) GO TO 70
C     IF(ABS(D1(L)-Y).LT. 1.0E-3) GO TO 70
      IF(D1(L) .GT. Y ) GO TO 60
      L=L+1
      GO TO 50
   60 ND=1
      IMM=IP1(L)
      GO TO 75
   70 ND=0
      IMM=IM1(L)
   75 LM=L
C  80 WRITE(6,*)'LM=',LM,'ANS=',LP+1-N
   80 IF(LM.GT.LP+1-N) GO TO 999
C     IF(LM.GT.LP+1-N) GO TO 999
      L1=LL1+1
      L2=LL2+1
      L3=LL1+LL2+N+ND-1
   90 L1=L1-1
      L3=L3-1
      D1(L3)=D1(L1)
      IP1(L3)=IP1(L1)
      IM1(L3)=IM1(L1)
      IF(L1.GT.LM) GO TO 90
      L3=L3-ND+1
  100 L2=L2-1
      L3=L3-1
      D1(L3)=D2(L2)
      IM1(L3)=IM2(L2)
      IF(L2.EQ.1) GO TO 110
      IP1(L3)=IP2(L2)
      GO TO 100
  110 IF(ND.EQ.0) IM1(LP+LL2-1)=IMM
      IF(ND.EQ.1) IM1(LP+LL2-2)=IMM
      IF(N .EQ.1) IP1(LP      )=IM1(LP-1)
      LL1=LL1+LL2+N+ND-2
      RETURN
  999 WRITE(6,*) ' *** OVERRIDE OCCURS IN D1 & D2 ARRAYS COMB STOP'
      IER = 1
      RETURN  
      END
