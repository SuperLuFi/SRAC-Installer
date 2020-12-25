C             INSERT                           UPDATE=94.06.24
      SUBROUTINE INSERT(DP,IA,IB,DM,D,IM,IP)
      COMMON / PIJ1C / DUM(22),LL,DUM2(13)
      DIMENSION D(1),IM(1),IP(1)
      X=DP
      Y=DM
      IF(X.GE.Y) GO TO 1
      X=DM
      Y=DP
    1 IF(LL.NE.0) GO TO 2
C          HEAD AND TAIL    AS INITIAL CONDITION
      LL=2
      D(1)=X
      D(2)=Y
      IM(1)=IA
      IP(2)=IB
      RETURN
C
    2 L=0
      IF(Y.GE.D(1)) RETURN
      IF(X.LE.D(LL))RETURN
      IF(X.GT.D(1)) X=D(1)
      IF(Y.LT.D(LL))Y=D(LL)
    3 L=L+1
C         FIND LP
      IF(ABS(D(L)-X).LT.1.E-5) GO TO 4
      IF(D(L).GT.X) GO TO 3
      LP=L
      N=1
      GO TO 5
    4 N=0
      LP=L
C         FIND LM
    5 IF(ABS(D(L)-Y).LT.1.E-5) GO TO 7
      IF(D(L) .LT. Y ) GO TO 6
      L=L+1
      GO TO 5
    6 ND=1
      LM=L
      GO TO 8
    7 ND=0
      LM=L
    8 IF(N+ND.EQ.0) GO TO 11
      L=LL+1
C        PART 1  HEAD TO LP D(LP)=<X
C        PART 2  LP   TO LM  D(LM)=<Y
C        PART 3  LM   TO TAIL
C      MOVE PART 3 (N+ND) ADDRESS BACKWARD
    9 L=L-1
      K=L+N+ND
      D(K)=D(L)
      IP(K)=IP(L)
      IM(K)=IM(L)
      IF(L.GT.LM) GO TO 9
C        MOVE PART 2 (N) ADDRESS BACKWARD
      IF(N.EQ.0)  GO TO 11
   10 IF(L.EQ.LP) GO TO 11
      L=L-1
      D(L+1)=D(L)
      IP(L+1)=IP(L)
      IM(L+1)=IM(L)
      GO TO 10
C        NEW POINT LP
   11 D(LP)=X
      IP(LP)=IM(LP-1)
      IM(LP)=IA
C     CONDITIONAL 1994.06.26
      IF(LP+1.LT.LM+N) IP(LP+1)=IA
C        NEW POINT LM+N
      K=LM+N
      D(K)=Y
      IP(K)=IB
      IM(K)=IP(K+1)
C     CONDITIONAL 1994.06.26
      IF(K.GT.LP+1) IM(K-1)=IB
      LL=LL+N+ND
      RETURN
      END
