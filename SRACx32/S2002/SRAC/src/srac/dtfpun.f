C             DTFPUN              LEVEL=1        DATE=80.09.29
      SUBROUTINE DTFPUN(E,NN,R,ISUM,NCARD)
C   *** DTFPUN USES VARIABE FORMAT TO PUNCH AND NUMBER CARDS
      DIMENSION E(6),NN(6),R(6),IE(6),IEXP(6),S1(6),S2(6)
      DIMENSION FMT(26),FBT(4),FET(2)
CADD SASAQ
      CHARACTER*4  POS,XNEG,FMT,FBT,FET,S1,S2
      DATA POS,XNEG/'+   ','-  '/
      DATA FMT(1),FMT(26),FET(1),FET(2)/'(   ','I8) ','T73,','I8) '/
      DATA FBT(1),FBT(2) ,FBT(3),FBT(4)/'I2,2','A1,I','5,A1',',I2,'/
      DO 2 J=1,6
      K=(J-1)*4 + 1
      DO 3 I=1,4
      L=K+I
    3 FMT(L)=FBT(I)
    2 CONTINUE
      IF(ISUM.EQ.6)GO TO 4
      K=ISUM*4 + 2
      FMT(K)=FET(1)
      FMT(K+1)=FET(2)
    4 DO 1 I=1,ISUM
      S1(I)=POS
      IF(E(I).LT.0.0)S1(I)=XNEG
      CALL FLTFX(E(I),IE(I),IEXP(I))
      S2(I)=XNEG
      IF(IEXP(I).GE.0)S2(I)=POS
      IEXP(I)=IABS(IEXP(I))
    1 CONTINUE
      WRITE (7,FMT) (NN(I),R(I),S1(I),IE(I),S2(I),IEXP(I),I=1,ISUM)
     &,NCARD
      RETURN
      END
