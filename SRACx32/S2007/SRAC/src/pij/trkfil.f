C  ********************************************************************
C                  MODULE TRKFIL
      SUBROUTINE TRKFIL(DP,IA,DM,D,IM,IP,LL)
C  ********************************************************************
C     ARRANGE ARRAY IN ASCENDING ORDER OF D (SAME AS INSERT) AND
C     FILL IM,IP BETWEEN NEW POINTS BY IA
C  ********************************************************************
C      COMMON /ITRACE/ DUM(30),LL
      DIMENSION D(*),IM(*),IP(*)
C     WRITE(6,*) ' INSERT IM=',IA,' TT(IM)=',DP,' TT(IP)=',DM
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
      IP(2)=IA
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
      GO TO 8
    7 ND=0
    8 LM=L
      IMM=IM(LM)
      IF(N+ND.EQ.0) GO TO 11
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
      IP(LP+1)=IA
      K=LM+N
      D(K)=Y
      IP(K)=IA
      IM(K-1)=IA
                IF(LM+N.GT.LP) THEN
                  DO 20 L=LP+1,LM+N
                  IP(L)=IA
                  IM(L)=IA
   20 CONTINUE
                    ENDIF
      IM(K)=IP(K+1)
      LL=LL+N+ND
      RETURN
      END
