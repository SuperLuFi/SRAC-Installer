      SUBROUTINE INTRP(DP,IA,IB,D,IM,IP)
      COMMON / PIJ1C / DUM(22),LL,DUM2(13)
      DIMENSION D(1),IM(1),IP(1)
      X=DP
      IF(LL.EQ.0)RETURN
       IF(X.GT.D(1)) RETURN
       IF(X.LT.D(LL)) RETURN
      L=1
    3 L=L+1
      IF(ABS(D(L)-X).LT. 1.E-5) GOTO 4
      IF(D(L).GT.X) GOTO 3
      LP=L
      N=1
      GOTO 5
    4 N=0
      LP=L
      GOTO 11
C
    5 L=LL+1
    9 L=L-1
      K=L+N
      D(K)=D(L)
      IP(K)=IP(L)
      IM(K)=IM(L)
      IF(L.GT.LP) GOTO 9
   11 D(LP)=X
      IP(LP)=IA
      IM(LP-1)=IA
      IM(LP)=IB
      IP(LP+1)=IB
      LL=LL+N
      RETURN
      END
