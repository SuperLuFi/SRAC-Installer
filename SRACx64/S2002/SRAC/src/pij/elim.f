C***********************************************************************
      SUBROUTINE ELIM(X,IM,II,IREG,L0,LLL )
C***********************************************************************
      DIMENSION IM(*),X(*),II(*) ,IREG(*)
      DO 2 L=1,LLL
      NZONE = IM(L)
      II(L) = IREG(NZONE)
    2 CONTINUE
      N=0
      LL=L0
      IF(LL .EQ. 1) GO TO 7
      DO 4 L=1,L0-1
      K=L-N
      IF(II(K) .EQ. II(L+1)) GO TO 3
      X(K+1)=X(L+1)
      II(K+1)=II(L+1)
      GO TO 4
    3 N=N+1
      X(K)=X(K)+X(L+1)
    4 CONTINUE
      L0=L0-N
    7 IF(LLL.EQ.LL) THEN
      LLL=L0
      RETURN
                    ENDIF
      II(L0+1)=II(LL+1)
      X(L0+1)=X(LL+1)
      IF(LLL.EQ.LL+1) RETURN
      DO 6 L=LL+1,LLL-1
      K=L-N
      IF(II(K) .EQ. II(L+1)) GO TO 5
      X(K+1)=X(L+1)
      II(K+1)=II(L+1)
      GO TO 6
    5 N=N+1
      X(K)=X(K)+X(L+1)
    6 CONTINUE
      LLL=LLL-N
      RETURN
      END
