C             PUNSH               LEVEL=1        DATE=80.09.29
      SUBROUTINE PUNSH(X,N)
C   *** CLEVER SET OF SUBROUTINES WHICH PUNCH CARDS IN ANISN FORMAT
C   *** PUNSH SETS UP EACH CARD PUNCHED BY DTFPUN UTILIZING REPEATS
      DIMENSION ETR(6),NN(6),R(6),X(1)
CADD SASAQ
      CHARACTER*4 BB,RR,R
      DATA BB,RR/'    ','R   '/
      NCARD=1
      I6=0
      K=0
      ITRIP=0
      II=N+1
      DO 1 I=2,II
      IF(ITRIP.LE.0)GO TO 2
      IF(I.EQ.II.AND.I6.EQ.0)GO TO 1
      ITRIP=0
      GO TO 3
    2 IF(I.GE.II)GO TO 4
      IF(X(I).NE.X(I-1))GO TO 4
      K=K+1
      IF(K.LT.98)GO TO 1
      ITRIP=1
    4 I6=I6+1
      NN(I6)=0
      R(I6)=BB
      IF(K.LE.0)GO TO 5
      NN(I6)=K+1
      R(I6)=RR
      K=0
    5 ETR(I6)=X(I-1)
      IF(I6.GE.6)GO TO 6
    3 IF(I.LT.II)GO TO 1
    6 CALL DTFPUN(ETR,NN,R,I6,NCARD)
      NCARD=NCARD +1
      I6=0
    1 CONTINUE
      RETURN
      END
