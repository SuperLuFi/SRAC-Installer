C             ADJNT               LEVEL=1        DATE=80.09.29
      SUBROUTINE ADJNT(X,N,IGM)
      DIMENSION X(N,1)
      L=1
      K=IGM
    1 IF(K.LE.L)GO TO 2
      DO 3 I=1,N
      E1=X(I,L)
      X(I,L)=X(I,K)
    3 X(I,K)=E1
      L=L + 1
      K=K - 1
      GO TO 1
    2 RETURN
      END
