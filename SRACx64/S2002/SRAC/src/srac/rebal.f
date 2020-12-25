C             REBAL               LEVEL=1        DATE=81.11.14
      SUBROUTINE REBAL (F,FU,FD,FL,FR,AB,QQ,H,G,IM,JM,IP,JP,K)
C
      COMMON /TW1C/ DD(1),LIM1,IA(210)
      COMMON /WORK/AAA(1),LIM2,AA(130)
      DIMENSION D(212),A(132)
      EQUIVALENCE (D(1),DD(1)),(AAA(1),A(1))
C
      DIMENSION F(IM,JM),FU(IM,JP),FD(IM,JP),FL(IP,JM),FR(IP,JM),
     &AB(IM,JM),QQ(IM,JM),H(1),G(1)
C
      EQUIVALENCE (IA(28),IRBM),(IA(45),EPSR),( A( 21),TS),
     &( A(49),IFLAG)
C
      IF (IFLAG.GT.0) GO TO 220
      IREBAL=0
  100 ERR=0.
      DO 210 J=1,JM
      IF (IM.EQ.1) T=F(1,J)
      DO 180 I=1,IM
      SNUM=QQ(I,J)
      DEN=AB(I,J)+FR(I+1,J)+FL(I,J)+FD(I,J)+FU(I,J+1)
      IF (J.GT.1) GO TO 120
      IF (K.NE.3) DEN=DEN-FU(I,J)
      IF (K.EQ.3) SNUM=SNUM+FU(I,JP)*F(I,JM)
      IF (J.EQ.JM) GO TO 130
  110 SNUM=SNUM+F(I,J+1)*FD(I,J+1)
      GO TO 140
  120 SNUM=SNUM+F(I,J-1)*FU(I,J)
      IF (J.NE.JM) GO TO 110
  130 IF (K.NE.3) DEN=DEN-FD(I,J+1)
      IF (K.EQ.3) SNUM=SNUM+FD(I,1)*F(I,1)
  140 IF (I.NE.1) GO TO 160
      DEN=DEN-FR(I,J)
      IF (I.NE.IM) GO TO 150
      TT=DEN-FL(I+1,J)
      IF(TT.LE.0.0)GO TO 220
      F(I,J)=SNUM/TT
      GO TO 200
  150 IF (DEN.EQ.0.0) GO TO 220
      H(I)=FL(I+1,J)/DEN
      G(I)=SNUM/DEN
      GO TO 180
  160 IF (I.EQ.IM) GO TO 170
      T=DEN-H(I-1)*FR(I,J)
      IF (T.EQ.0.0) GO TO 220
      DENA=1.0/T
      H(I)=FL(I+1,J)*DENA
      G(I)=(SNUM+G(I-1)*FR(I,J))*DENA
      GO TO 180
  170 H(I)=0.0
      T=DEN-FR(I,J)*H(I-1)-FL(I+1,J)
      IF (T.EQ.0.0) GO TO 220
      G(I)=(SNUM+G(I-1)*FR(I,J))/T
  180 CONTINUE
      DO 190 IB=1,IM
      I=IP-IB
      T=G(I)+H(I)*F(I+1,J)
      IF (T.LE.0.0) GO TO 220
      ERR=AMAX1(ERR,ABS(1.-T/F(I,J)))
  190 F(I,J)=T
  200 IF (IM.EQ.1) ERR=AMAX1(ERR,ABS(1.0-T/F(1,J)))
  210 CONTINUE
      IREBAL=IREBAL+1
      IF (IREBAL.GT.IRBM) GO TO 220
      IF (ERR.GT.EPSR) GO TO 100
      RETURN
C
C     WHOLE SYSTEM REBALANCE
C
  220 T=0.0
      DO 230 J=1,JM
      T=T+FR(IP,J)-FL(IP,J)-FR(1,J)+FL(1,J)
      DO 230 I=1,IM
  230 T=T+AB(I,J)
      DO 240 I=1,IM
  240 T=T+FU(I,JP)-FD(I,JP)-FU(I,1)+FD(I,1)
      IF (T.LE.0.0) GO TO 250
      T=TS/T
      GO TO 260
  250 T=1.0
  260 DO 270 J=1,JM
      DO 270 I=1,IM
  270 F(I,J)=T
      RETURN
      END
