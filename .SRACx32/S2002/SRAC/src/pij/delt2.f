C     MEMBER DELTSC2 IDRECT=2 SCALAR WITH TAYLOR EXPANSION
      SUBROUTINE DELT2(NR,III,S,U,X,P,PR,MAT,SIG,LENG)
C       CALLED AT EVERY ENERGY GROUP FOR CYLINDRICAL COORDINATE
C     L0: NUMBER OF CUTS IN THE SOURCE CELL ON A LINE
C     LLL: TOTAL NUMBER OF CUTS ON A LINE
C     WEIGHT: WEIGHT OF A LINE
C     III(L): REGION IDENTIFICATION NUMBER OF CUT L
C     MAT   : MATERIAL NUMBER
C     SIG :   MACRO CROSS SECTION OF MATERIAL M
C     S(L):   MACRO CROSS SECTION OF CUT L
C     X(L):   LENGTH OF CUT L
C     U(L):   OPTICAL LENGTH OF CUT L
C     N :    CURRENT ENERGY GROUP NUMBER
C     NG:    MAXIMUM NUMBER OF ENERGY GROUPS
C     NR   : TOTAL NUMBER OF REGION
C     IL   : FILE ALLOCATION NUMBER BETWEEN 1,2,3
C     LENG : MAX LLL  USED IN VP VERSION
C
      COMMON / PIJ2C / IDUM(5), IBOUND,IDRECT,LCOUNT,
     1         IDUM2(25),IL
      COMMON /ABC11/ A(5500),B(5500),C(5500)
      REAL *8  A,B,C ,T1(2),T2(2),T3(2),T4(2),DY,Y,U0
      DIMENSION III(*),S(*),U(*),X(*),P(NR,*),PR(NR,*)
      DIMENSION SIG(*),MAT(*),DUMMY(3)
      EQUIVALENCE(WEIGHT,DUMMY(1))
C     INTEGRATION OF KI FUNCTION
C
C     INTEGRATION
      DO 100 I=1,NR
*     VOL(I)=0.
      DO 100 J=1,NR+1
      P(I,J)=0.
  100 CONTINUE
      CALL OPNBUF(IL)
      DO 1000 LINE=1,LCOUNT
      CALL RDBUF(LLL,L0,WEIGHT,X(1),III(1),IL)
      DO 200 L=1,LLL
      I       =III(L)
      M=MAT(I)
      S(L) = SIG(M)
      U(L) = S(L)*X(L)
  200 CONTINUE
*     IF(LINE.EQ.10 .AND. N.EQ.1)
*    &WRITE(6,'(A/(8(I4,E12.5)))')' S ALONG LINE',(III(L),S(L),L=1,LLL)
      DO 399 L=1,L0
      I=III(L)
      UI=U(L)
      IF(UI.LT. 0.003) GO TO 350
C
C     SIG(L) .NE. 0  SIG(LD).NE.0
C
      WI=WEIGHT/S(L)
      U0=-UI
      T3(1)=UI
      T4(1)=0.
      T3(2)=0.6666666666*UI
      T4(2)=0.
C
      DO 340 LD=L,LLL
      J=III(LD)
      UJ=U(LD)
      IF(UJ.LE.0.003) GO TO 325
C     SIG(L).NE.0  SIG(LD).NE.0
      U0=U0+UJ
      Y=U0
      T1(1)=T3(1)
      T1(2)=T3(2)
C     T3(1)=FKIN(3,Y)
      JJ=INT(100.*Y)+1
      IF(Y.GE.11.0) JJ=1100
      NN=JJ+2200
      DY=Y-0.01D0*DFLOAT(JJ-1)
      T3(1)= (B(NN)*DY+A(NN))*DY+C(NN)
C     T3(2)=FKIN(5,Y)
      NN=JJ+4400
      T3(2)= (B(NN)*DY+A(NN))*DY+C(NN)
      Y=U0+UI
      T2(1)=T4(1)
      T2(2)=T4(2)
C     T4(1)=FKIN(3,Y)
      JJ=INT(100.D0*Y)+1
      IF(Y.GE.11.0) JJ=1100
      NN=JJ+2200
      DY=Y-0.01D0*DFLOAT(JJ-1)
      T4(1)= (B(NN)*DY+A(NN))*DY+C(NN)
C     T4(2)=FKIN(5,Y)
      NN=JJ+4400
      T4(2)= (B(NN)*DY+A(NN))*DY+C(NN)
      P (I,J)=P (I,J)+WI*(T1(1)-T2(1)-T3(1)+T4(1))/S(LD)
      PR(I,J)=PR(I,J)+WI*(T1(2)-T2(2)-T3(2)+T4(2))/S(LD)
      IF(U0.GT.6.) GO TO 399
      GO TO 340
C     SIG(L).NE.0 SIG(LD).EQ.0
  325 U0=U0+UJ
      Y=U0
C     T1(1)=FKIN(2,Y)
      JJ=INT(100.D0*Y)+1
      IF(Y.GE.11.0) JJ=1100
      NN=JJ+1100
      DY=Y-0.01D0*DFLOAT(JJ-1)
      T1(1)= (B(NN)*DY+A(NN))*DY+C(NN)
C     T1(2)=FKIN(4,Y)
      NN=JJ+3300
      T1(2)= (B(NN)*DY+A(NN))*DY+C(NN)
C     T3(1)=FKIN(3,Y)
      NN=JJ+2200
      T3(1)= (B(NN)*DY+A(NN))*DY+C(NN)
C     T3(2)=FKIN(5,Y)
      NN=JJ+4400
      T3(2)= (B(NN)*DY+A(NN))*DY+C(NN)
      Y=U0+UI
C     T2(1)=FKIN(2,Y)
      JJ=INT(100.D0*Y)+1
      IF(Y.GE.11.0) JJ=1100
      NN=JJ+1100
      DY=Y-0.01D0*DFLOAT(JJ-1)
      T2(1)= (B(NN)*DY+A(NN))*DY+C(NN)
C     T2(2)=FKIN(4,Y)
      NN=JJ+3300
      T2(2)= (B(NN)*DY+A(NN))*DY+C(NN)
C     T4(1)=FKIN(3,Y)
      NN=JJ+2200
      T4(1)= (B(NN)*DY+A(NN))*DY+C(NN)
C     T4(2)=FKIN(5,Y)
      NN=JJ+4400
      T4(2)= (B(NN)*DY+A(NN))*DY+C(NN)
      P (I,J)=P (I,J)+WI*(T1(1)-T2(1))*X(LD)
      PR(I,J)=PR(I,J)+WI*(T1(2)-T2(2))*X(LD)
  340 CONTINUE
C     IF(IBOUND.EQ.1) GO TO 399
      P (I,NR+1)=P (I,NR+1)+WI*(T3(1)-T4(1))
      PR(I,NR+1)=PR(I,NR+1)+WI*(T3(2)-T4(2))
      GO TO 399
C     SIG(L).EQ.0
  350 FACT=0.5
      U0=-UI/2.
      WI=WEIGHT*X(L)
      DO 390 LD=L,LLL
      J=III(LD)
      UJ=U(LD)
      IF(UJ.LE.0.003) GO TO 375
C     SIG(L).EQ.0  SIG(LD).NE.0
      U0=U0+UJ
      Y=U0
      T1(1)=T2(1)
      T1(2)=T2(2)
C     T2(1)=FKIN(2,Y)
      JJ=INT(100.D0*Y)+1
      IF(Y.GE.11.0) JJ=1100
      NN=JJ+1100
      DY=Y-0.01D0*DFLOAT(JJ-1)
      T2(1)= (B(NN)*DY+A(NN))*DY+C(NN)
C     T2(2)=FKIN(4,Y)
      NN=JJ+3300
      T2(2)= (B(NN)*DY+A(NN))*DY+C(NN)
      P (I,J)=P (I,J)+WI*(T1(1)-T2(1))/S(LD)
      PR(I,J)=PR(I,J)+WI*(T1(2)-T2(2))/S(LD)
      IF(U0.GT.6.0) GO TO 399
      GO TO 390
C
C     SIG(L).EQ.0  SIG(LD).EQ.0
C
  375 U0=U0+UJ
      Y=U0
C     T1(1)=FKIN(1,Y)
      JJ=INT(100.D0*Y)+1
      IF(Y.GE.11.0) JJ=1100
      NN=JJ
      DY=Y-0.01D0*DFLOAT(JJ-1)
      T1(1)= (B(NN)*DY+A(NN))*DY+C(NN)
C     T1(2)=FKIN(3,Y)
      NN=JJ + 2200
      T1(2)= (B(NN)*DY+A(NN))*DY+C(NN)
C     T2(1)=FKIN(2,Y)
      NN=JJ+1100
      T2(1)= (B(NN)*DY+A(NN))*DY+C(NN)
C     T2(2)=FKIN(4,Y)
      NN=JJ+3300
      T2(2)= (B(NN)*DY+A(NN))*DY+C(NN)
      P (I,J)=P (I,J)+WI*T1(1)*X(LD)*FACT
      PR(I,J)=PR(I,J)+WI*T1(2)*X(LD)*FACT
      FACT=1.0
  390 CONTINUE
C     IF(IBOUND.EQ.1) GO TO 399
      P (I,NR+1)=P (I,NR+1)+WI*T2(1)
      PR(I,NR+1)=PR(I,NR+1)+WI*T2(2)
  399 CONTINUE
 1000 CONTINUE
*     IF(N.EQ.1)
*    &WRITE(6,'(8(I3,E12.5))') ' VOL NUM=',(I,VOL(I),I=1,NR)
      RETURN
      END
