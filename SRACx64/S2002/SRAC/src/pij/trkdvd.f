C ******************** MODULE TRKDVD  *******************************
             SUBROUTINE TRKDVD(IPP,IMM,X,D,IM,IP,LL)
C            INSERT ONE POINT INTO D,IM,IP ARRAYS
C            CALLED BY GEOMETRY ROUTINES
C     IPP    REGION NUMBER OF ENTRY SIDE
C     IMM    REGION NUMBER OF EXIT  SIDE
C     X      DISTANCE FROM H TO BE INSERTED IN D(*) ARRAY
C *********************************************************************
C     COMMON / ITRACE/ DUM(30),LL
      DIMENSION D(*),IM(*),IP(*)
      IF(LL.LT.2)   RETURN
      IF(X+1.E-3.LT.D(1)) RETURN
      IF(X.GT.D(LL)+1.E-3)RETURN
      L=0
    3 L=L+1
      IF(ABS(D(L)-X).LT.1.E-3) GO TO 4
      IF(D(L).LT.X) GO TO 3
      N=1
      LP=L
      GO TO 6
    4 N=0
      LP=L
      GO TO 11
C     SHIFT THE DATA AFTER LP IN THE ARRAYS D,IM,IP
    6 L=LL+1
    9 L=L-1
      K=L+N
      D(K)=D(L)
      IP(K)=IP(L)
      IM(K)=IM(L)
      IF(L.GT.LP) GO TO 9
C     L=LP   THEN
   11 D(LP)=X
      IM(LP)=IMM
      IF(LP.LT.LL+N) IP(LP+1)=IMM
      IP(LP)=IPP
      IF(LP.GT.1)    IM(LP-1)=IPP
      LL=LL+N
      RETURN
      END
