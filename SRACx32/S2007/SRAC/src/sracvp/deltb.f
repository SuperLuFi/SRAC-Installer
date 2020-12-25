C     MEMBER DELTVP  IDRECT=1  WITHOUT TAYLOE EXP.
C     VOCTORIZE BY LINE BAND
C
CVP
C     SUBROUTINE DELT (NR,III,S,U,X,P,MAT,SIG,LENG)
      SUBROUTINE DELTB(NR,III,S,U,X,P,MAT,SIG,LENG,PP,IBANK)
C
C       CALLED BY ENERGY GROUP FOR CYLINDRICAL COORDINATE
C     VP VERSION OF DELT BY LINE PARALLEL
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
C     NR1  : NR+1
C     IL   : FILE ALLOCATION NUMBER BETWEEN 1,2,3
C
      COMMON / PIJ2C / IDUM(5), IBOUND,IDRECT,LCOUNT,
     1         IDUM2(25),IL
      COMMON /BANK/ MAXL0(4),MAXLL(4),LBANK
      COMMON /ABC11/ A(5500),B(5500),C(5500)
      REAL *8  A,B,C ,T1,T2,T3,T4,U01,DY
      DIMENSION III(LENG,*),U(LENG,*),X(LENG,*),S(LENG,*)
      DIMENSION P(NR,NR+1),MAT(*),SIG(*)
      DIMENSION LLL(512),L0(512),WEIGHT(514),WI1(512)
     &         ,U01(512),T1(512),T2(512),T3(512),T4(512)
     &         ,II(512),JJ(512),UI(512)
     &         ,IEND(512),JEND(512),ELM1(512)
CVP
      DIMENSION PP(NR,NR+1,IBANK)
CVP
C     INTEGRATION
      DO 100 J=1,NR+1
      DO 100 I=1,NR
      P(I,J)=0.
  100 CONTINUE
CVP
      DO 101 K=1,IBANK
      DO 101 J=1,NR+1
      DO 101 I=1,NR
      PP(I,J,K)=0.
  101 CONTINUE
      CALL OPNBUF(IL)
      LTIME=LCOUNT/LBANK+1
      LEFT=LCOUNT
      DO 1000 LBAND=1,LTIME
      LAST=MIN(LBANK,LEFT)
      DO 150 LINE=1,LAST
      CALL RDBUF(LLL(LINE),L0(LINE),WEIGHT(LINE),X(1,LINE)
     &           ,III(1,LINE),IL)
      IEND(LINE)=0
  150 CONTINUE
      DO 200 L=1,MAXLL(IL)
      DO 180 LINE=1,LAST
      IF(L.LE.LLL(LINE))THEN
      I       =III(L,LINE)
      S(L,LINE) = SIG(MAT(I       ))
      U(L,LINE) = S(L,LINE)*X(L,LINE)
                        ENDIF
  180 CONTINUE
  200 CONTINUE
C
      DO 400 L=1,MAXL0(IL)
      DO 310 LINE=1,LAST
      IF(L.LE.L0(LINE))                                   THEN
      II(LINE)=III(L,LINE)
      UI(LINE)=U(L,LINE)
C***  IF(UI(LINE).GT. 0.003)                    THEN
C
C     SIG(L) .NE. 0
C
      WI1(LINE)=WEIGHT(LINE)/S(L,LINE)
      U01(LINE)=-UI(LINE)
      T3(LINE)=UI(LINE)
      T4(LINE)=0.
      JEND(LINE)=0
C***  SIG(L).EQ.0
*     U02(LINE)= UI(LINE)/2.
*     T6(LINE)=1.0
*     WI2(LINE)=WEIGHT(LINE)*X(L,LINE)
*     Y      =U02(LINE)
C     FKIN(1,Y)
*     J=INT(100.*Y)+1
*     IF(Y.GT.11.0) J=1100
*     NN=J
*     DY=Y-0.01D0*DFLOAT(J-1)
*     T5(LINE)= (B(NN)*DY+A(NN))*DY+C(NN)
C     FKIN(2,Y)
*     NN=J+1100
*     T6(LINE)= (B(NN)*DY+A(NN))*DY+C(NN)
*     ELM2(LINE) = WI2(LINE)*T5(LINE)*X(L,LINE)/2.
C                                               ELSE
C                    IEND(LINE)=1
C                                               ENDIF
                                                          ELSE
                     IEND(LINE)=1
                                                          ENDIF
  310 CONTINUE
*     DO 305 LINE=1,LAST
*     IF(IEND(LINE).EQ.0 .AND. UI(LINE).LT.0.003) THEN
*     P (II(LINE),II(LINE))=P (II(LINE),II(LINE))+ ELM2(LINE)
*                                                 ENDIF
* 305 CONTINUE
      DO 340 LD=L,MAXLL(IL)
      DO 320 LINE=1,LAST
      IF(IEND(LINE).EQ.0 .AND. JEND(LINE).EQ.0) THEN
CI    ELM1(LINE)=0.
*     ELM2(LINE)=0.
      JJ(LINE)=III(LD,LINE)
      UJ      =U(LD,LINE)
      T1(LINE)=T3(LINE)
      T2(LINE)=T4(LINE)
      U01(LINE)=U01(LINE)+UJ
      Y      =U01(LINE)
C     FKIN(3,Y)
      J=INT(100.*Y)+1
      IF(Y.GT.11.0) J=1100
      NN=J+2200
      DY=Y-0.01D0*DFLOAT(J-1)
      T3(LINE)= (B(NN)*DY+A(NN))*DY+C(NN)
C     FKIN(3,Y)
      Y      =U01(LINE)+UI(LINE)
      J=INT(100.*Y)+1
      IF(Y.GT.11.0) J=1100
      NN=J+2200
      DY=Y-0.01D0*DFLOAT(J-1)
      T4(LINE)= (B(NN)*DY+A(NN))*DY+C(NN)
      ELM1(LINE)=
     &   WI1(LINE)*(T1(LINE)-T2(LINE)-T3(LINE)+T4(LINE))/S(LD,LINE)
C     UI<0.003
*     T5(LINE)=T6(LINE)
*     U02(LINE)=U02(LINE)+UJ
*     Y      =U02(LINE)
C     FKIN(2,Y)
*     J=INT(100.*Y)+1
*     IF(Y.GT.11.0) J=1100
*     NN=J+1100
*     DY=Y-0.01D0*DFLOAT(J-1)
*     T6(LINE)= (B(NN)*DY+A(NN))*DY+C(NN)
*     ELM2(LINE)= WI2(LINE)*(T5(LINE)-T6(LINE))/S(LD,LINE)
                                                ENDIF
  320 CONTINUE
      DO 330 LINE=1,LAST,IBANK
      LE = MIN(LAST,LINE+IBANK-1)
      L2 = 0
*VOCL LOOP,NOVREC
      DO 330 L1 = LINE,LE
      L2 = L2 + 1
      IF(IEND(L1).EQ.0 .AND. JEND(L1).EQ.0)    THEN
*     IF(UI(LINE).GT.0.003) THEN
      PP(II(L1),JJ(L1),L2)=PP(II(L1),JJ(L1),L2) + ELM1(L1)
*                           ELSE
*     P (II(LINE),JJ(LINE))=P (II(LINE),JJ(LINE)) + ELM2(LINE)
*                           ENDIF
      IF(U01(L1).GT.6.0) JEND(L1)=1
      IF(LD.EQ.LLL(L1)) JEND(L1)=1
                                                   ENDIF
  330 CONTINUE
  340 CONTINUE
C
      IJEND = 0
      DO 345 LINE=1,LAST
       IF (IEND(LINE).EQ.0 .AND. JEND(LINE).EQ.1) IJEND=1
  345 CONTINUE
      IF (IJEND .EQ. 0) GO TO 400
      DO 350 LINE=1,LAST,IBANK
      LE = MIN(LAST,LINE+IBANK-1)
      L2 = 0
*VOCL LOOP,NOVREC
      DO 350 L1 = LINE,LE
      L2 = L2 + 1
      IF(IEND(L1).EQ.0 .AND. JEND(L1).EQ.1) THEN
*     IF(UI(LINE).GT.0.003) THEN
      PP(II(L1),NR+1,L2)=PP(II(L1),NR+1,L2)+WI1(L1)*(T3(L1)-T4(L1))
*                           ELSE
*     P (II(LINE),NR+1)=P (II(LINE),NR+1)+WI2(LINE)*T6(LINE)
*                           ENDIF
      JEND(L1)=2
                                                ENDIF
  350 CONTINUE
  400 CONTINUE
      LEFT=LEFT-LBANK
 1000 CONTINUE
      DO 500 L1 = 1,IBANK
      DO 500 I = 1,NR+1
      DO 500 J = 1,NR
      P(J,I) = P(J,I) + PP(J,I,L1)
  500 CONTINUE
*     DO 1100 J=1,10
*     WRITE(6,1111) (P(I,J),I=1,10)
*1100 CONTINUE
*1111 FORMAT('  DELTCHK ',10E12.5)
      RETURN
      END
