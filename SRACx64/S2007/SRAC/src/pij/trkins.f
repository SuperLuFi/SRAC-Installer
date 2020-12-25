C  ********************************************************************
C                  MODULE TRKINS
      SUBROUTINE TRKINS(DP,IA,IB,DM,D,IM,IP,LL)
C  ********************************************************************
C     ARRANGE THE ARRAY IN ASCENDIND ORDER OF D
C  ********************************************************************
      DIMENSION D(*),IM(*),IP(*)
C     WRITE(6,*) ' INSERT IM=',IA,' IP=',IB,' TT(IM)=',DP,' TT(IP)=',DM
      X=DP
      Y=DM
      IF(X.LE.Y) GO TO 1
      X=DM
      Y=DP
    1 IF(LL.NE.0) GO TO 2
      LL=2
      D(1)=X
      D(2)=Y
      IM(1)=IA
      IP(2)=IB
      RETURN
C
    2 L=0
      IF(Y.LE.D(1)) RETURN
      IF(X.GE.D(LL))RETURN
      IF(X.LT.D(1)) X=D(1)
      IF(Y.GT.D(LL))Y=D(LL)
    3 L=L+1
      IF(ABS(D(L)-X).LT.1.E-3) GO TO 4
      IF(D(L).LT.X) GO TO 3
      LP=L
      N=1
      GO TO 5
    4 N=0
      LP=L
    5 IF(ABS(D(L)-Y).LT.1.E-3) GO TO 7
      IF(D(L) .GT. Y ) GO TO 6
      L=L+1
      GO TO 5
    6 ND=1
      LM=L
      GO TO 8
    7 ND=0
      LM=L
    8 IF(N+ND.EQ.0) GO TO 11
      L=LL+1
    9 L=L-1
      K=L+N+ND
      D(K)=D(L)
      IP(K)=IP(L)
      IM(K)=IM(L)
      IF(L.GT.LM) GO TO 9
      IF(N.EQ.0)  GO TO 11
   10 IF(L.EQ.LP) GO TO 11
      L=L-1
      D(L+1)=D(L)
      IP(L+1)=IP(L)
      IM(L+1)=IM(L)
      GO TO 10
   11 D(LP)=X
      IP(LP)=IM(LP-1)
      IM(LP)=IA
C     IF(LP+1.LT.LM)
      IF(IA.EQ.IB)   IP(LP+1)=IA
      K=LM+N
      D(K)=Y
      IP(K)=IB
      IM(K)=IP(K+1)
C     IF(K-1.GT.LP)
      IF(IA.EQ.IB)   IM(K-1)=IB
      LL=LL+N+ND
      RETURN
      END
