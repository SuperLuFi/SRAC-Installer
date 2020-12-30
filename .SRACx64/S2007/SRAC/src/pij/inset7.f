C      MODULE INSERT7
C *** INSET7 HAS THE SIMILAR FUNCTION AS INSERT
C *** IT DIFFERS TO PUT TAG IA IB ONE INNER POINTS
      SUBROUTINE INSET7(DP,IA,IB,DM,D,IM,IP)
      COMMON / PIJ1C / DUM(22),LL
      COMMON / PIJ2C / DUM2(10),NTTAB
      COMMON /MAINC /DUMMY1(63),NOUT1,NOUT2,DUMMY(435)
      DIMENSION D(1),IM(1),IP(1)
C
C *** ALWAYS X > Y
      X=DP
      Y=DM
      IF(X.GE.Y) GO TO 1
      X=DM
      Y=DP
C *** INITIALIZE
    1 IF(LL.EQ.0) THEN
      LL=2
      D(1)=X
      D(2)=Y
      IM(1)=IA
      IP(2)=IB
      RETURN
                  ENDIF
C
      L=0
C **  OUT OF RANGE
      IF(Y.GE.D(1)) RETURN
      IF(X.LE.D(LL)) RETURN
C **  NO EXTENTION ALLOWED
      IF(X.GT.D(1))  X=D(1)
      IF(Y.LT.D(LL)) Y=D(LL)
    3 L=L+1
C  *** FIND LP   AS D(LP-1)>X>=D(LP)
      IF(ABS(D(L)-X).LT.1.E-5) GO TO 4
      IF(D(L).GT.X) GO TO 3
C **  CASE X>D(LP)
      LP=L
      N=1
      GO TO 5
C **  CASE X=D(LP)
    4 N=0
      LP=L
C  *** FIND LM  AS D(LM-1)>Y>=D(LM)
    5 IF(ABS(D(L)-Y).LT.1.E-5) GO TO 7
      IF(D(L) .LT. Y ) GO TO 6
      L=L+1
      GO TO 5
C **  CASE Y>D(LM)
    6 ND=1
      LM=L
      GO TO 8
C **  CASE Y=D(LM)
    7 ND=0
      LM=L
    8 IF(N+ND.EQ.0) GO TO 11
      L=LL+1
C        CLASSIFY ARRAY INTO 3 PARTS
C        PART 1  HEAD TO LP-1    X>=D(LP)
C        PART 2  LP+1 TO LM-1    Y>= D(LM)
C        PART 3  LM   TO TAIL
C      MOVE PART 3    N+ND BACKWARD
    9 L=L-1
      K=L+N+ND
      D(K)=D(L)
      IP(K)=IP(L)
      IM(K)=IM(L)
      IF(L.GT.LM) GO TO 9
      IF(N.EQ.0)  GO TO 11
C        MOVE PART 2 N BACKWARD
   10 IF(L.EQ.LP) GO TO 11
      L=L-1
      D(L+1)=D(L)
CCCCC PROPER TO INSET7
      IP(L+1)=IA
CCCCC PROPER TO INSET7
      IM(L+1)=IB
      GO TO 10
C        NEW POINT LP
   11 D(LP)=X
      IP(LP)=IM(LP-1)
      IM(LP)=IA
C     UNCONDITIONALLY
      IP(LP+1)=IA
CCCCC PROPER TO INSET7
      IF(LP+2.LT.LM+N) IM(LP+1)=IA
C        NEW POINT LM+N
      K=LM+N
      D(K)=Y
      IP(K)=IB
      IM(K)=IP(K+1)
C     CONDITIONALLY
      IF(K-1.GT.LP) IM(K-1)=IB
CCCCC PROPER TO INSET7
      IF(K-2.GT.LP) IP(K-1)=IB
      LL=LL+N+ND
      IF(LL.GT.NTTAB) GO TO 14
      RETURN
   14 WRITE(NOUT1,15) LL,NTTAB
      STOP
   15 FORMAT('0 **** OVERFLOW OF TABLE LENGTH    ',2I4)
      END
