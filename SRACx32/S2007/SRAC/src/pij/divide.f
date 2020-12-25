C             DIVIDE              LEVEL=2        DATE=82.03.12
      SUBROUTINE DIVIDE(DP,DM,M,D,IM,IP)
      COMMON / PIJ1C / DUM(22),LL,DUM1(13)
      DIMENSION D(1),IM(1),IP(1)
      X=DP
      Y=DM
      IF(Y.GE.D(1)) RETURN
      IF(X.LE.D(LL))RETURN
      IF(X.GE.D(1)) X=D(1)
      IF(Y.LE.D(LL))Y=D(LL)
    2 L=0
    3 L=L+1
      IF(ABS(D(L)-X).LT.1.E-5) GO TO 4
      IF(D(L).GT.X) GO TO 3
      LP=L
      N=1
      JTEMP=IP(LP)
      KTEMP=JTEMP
      GO TO 5
    4 N=0
      LP=L
      JTEMP=IM(LP)
      KTEMP=IP(LP)
    5 IF(ABS(D(L)-Y).LT.1.E-5) GO TO 7
      IF(D(L) .LT. Y ) GO TO 6
      L=L+1
      GO TO 5
    6 ND=1
      GO TO 8
    7 ND=0
    8 LM=L
      ITEMP=IP(LM)
      L=LL+1
    9 L=L-1
      K=L+N+ND
      D(K)=D(L)
      IP(K)=IP(L)
      IM(K)=IM(L)
      IF(L.GT.LM) GO TO 9
   10 IF(L.EQ.LP) GO TO 11
      L=L-1
      K=L+N
      D(K)=D(L)
      IP(K)=IP(L)+M
      IM(K)=IM(L)+M
      GO TO 10
   11 D(LP)=X
      IM(LP)=JTEMP+M
      IP(LP)=KTEMP
      K=LM+N
      D(K)=Y
      IP(K)=ITEMP+M
      IM(K)=IP(K+1)
      LL=LL+N+ND
      RETURN
      END
