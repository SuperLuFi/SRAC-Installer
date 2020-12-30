      SUBROUTINE UMCCRS(XIN,YIN,QVAL,MEMBER,X,XX,YY,Y,EMESHF,CROSS,LOC ,
     +                  JPLOT )
C
      REAL*8          SUM,DELOW,DEHI,DELUU,SIGX,SUMX
      REAL*8          E1,E2,CRS1,CRS2,A,B,DSAVE
      REAL*8          EBOUND,UIGP,UFGP,UBGP
C
      CHARACTER*8     MEMBER
      CHARACTER*4     IDTMP
C
      COMMON /UMCPIN/ ITAPE,MATNO,NPE,NPF,NPC,IZA,ISWUN,AM,ELOW,EHI,
     +                RFTEMP,QVALE,QVALF,QVALC,EULOW,EUHIGH
      COMMON /UMCINT/ NPMAX,IFISS,IENDUN,IDPOS,ISWTMP,MF,MT,NP,IMAX,
     +                NSYSI,NSYSO,RQTEMP,NPRT,IPRINT
C
      COMMON /UMC001/ LIBTYP,NEF,ISTART,NG,NOMESH,KFGP,NGMAX,MAXINT,NSET
      COMMON /UMC002/ ENERGY(75),EE(47),NI(46),INTNO(46),
     +                NXG(10),NFI(10),NOIG(10),MST(46),MEND(46)
      COMMON /UMC003/ EBOUND(11),UIGP(10),UFGP(10),UBGP(46)
      COMMON /UMC004/ INTBL(2000),ENGD(2000)
      COMMON /UMC005/ SIG1D(3,46)
      COMMON /UMCTMP/ NTEMP,TMPSET(40),IDTMP(40)
      COMMON /PCOWK3/ WORK(1000),IWORK(46)
C
      REAL*4          EMESHF(IMAX),CROSS(IMAX)
      REAL*4          XIN(NPMAX),YIN(NPMAX),XX(NPMAX),YY(NPMAX)
      REAL*8          X(NPMAX),Y(NPMAX)
      INTEGER*4       LOC(IMAX)
C
C     START
C
      IF(IPRINT.GT.1) WRITE(NPRT,101) MATNO,MF,MT,NP,RQTEMP,QVAL
C
      IF(NP.GT.NPMAX) THEN
                      WRITE(NSYSO,110) NPMAX,NP
                      WRITE(NPRT ,110) NPMAX,NP
                      STOP 998
                      ENDIF
C
      EMAX  =  1.1 * EBOUND(1)
CDEL  IF(EMAX.GT.XIN(NP))  EMAX = XIN(NP)
CADD  BY JAIS K.KANKEO  1/16/1990
      DO 710 I = 1 , NP
      ISET     = I
      IF(XIN(I).GT.EMAX) GO TO 711
  710 CONTINUE
  711 IF(IPRINT.GT.1)  THEN
      WRITE(NSYSO,112) ISET,EMAX,XIN(ISET),XIN(1),XIN(NP)
                       ENDIF
      EMAX     = XIN(ISET)
CEND
      CALL UMCFIL(XIN,YIN,X,XX,YY,Y,EMAX)
C
      CALL DCLEA( X , NPMAX , 0.00D+0 )
      CALL DCLEA( Y , NPMAX , 0.00D+0 )
      NP1      = NP + 1
C
      DO 720 I = 1  , NP
      X(I)     = XIN(NP1-I)
      Y(I)     = YIN(NP1-I)
  720 CONTINUE
C
      IF(IPRINT.GT.1) THEN
                      WRITE(NPRT,102) X(1),Y(1)
                      WRITE(NPRT,104) X(NP),Y(NP)
                      ENDIF
C
      IF(MT.EQ.  2) THEN
                    DO 10 I = 1 , NP
                    IF(Y(I).LT.0.001) Y(I) = 0.001
   10               CONTINUE
                    ENDIF
C
      IF(MT.EQ. 18) THEN
                    DO 15 I = 1 , NP
                    IF(Y(I).LT.1.0E-8) Y(I) = 0.0
   15               CONTINUE
                    ENDIF
C
      IF(MT.EQ.102) THEN
                    DO 20 I = 1 , NP
                    IF(Y(I).LT.0.0)   Y(I) = 0.0
   20               CONTINUE
                    ENDIF
C
      CALL CLEA( EMESHF , IMAX  , 0.0 )
      CALL CLEA( LOC    , IMAX  , 0.0 )
C
      IPOS   = 1
      LOP1   = 1
C
      DO  100 N  = 1 , NOMESH
      DEHI   = EBOUND(N)
      LOP2   = NXG(N)*KFGP
      DELUU  = UFGP(N)
      DO   50 J  = LOP1 , LOP2
      DO   30 K  = IPOS,NP-1
      KK     = K
      IF(DEHI.LE.X(K).AND.DEHI.GT.X(K+1)) GO TO 40
   30 CONTINUE
      STOP  905
   40 CONTINUE
      LOC(J) = KK
      DEHI   = DEHI*EXP(-DELUU)
      IPOS   = KK - 1
   50 CONTINUE
      LOP1   = LOP2 + 1
  100 CONTINUE
C
      DEHI     = EBOUND(NOMESH+1)
      DO  130 K  = IPOS,NP-1
      KK     = K
      IF(DEHI.LE.X(K).AND.DEHI.GT.X(K+1)) GO TO 140
  130 CONTINUE
      STOP  905
  140 CONTINUE
      LOC(LOP1)= KK
C
      ISW    = 0
      LOP1   = 1
      DO  1000 N = 1 , NOMESH
      DELUU  = UFGP(N)
      DEHI   = EBOUND(N)
      LOP2   = NXG(N)*KFGP
      DO   800 J  = LOP1, LOP2
      DELOW    = DEHI*EXP(-DELUU)
      IST      = LOC(J)
      IEND     = LOC(J+1)
      EMESHF(J)= DEHI
C
               SUM        = 0.0
               DO 600 LOP = IST,IEND
               E1     = X(LOP)
               E2     = X(LOP+1)
               IF(E1.EQ.E2) GO TO  600
               CRS1   = Y(LOP)
               CRS2   = Y(LOP+1)
               A      = (CRS1-CRS2)/(E1-E2)
               B      = (CRS2*E1-CRS1*E2)/(E1-E2)
               IF(E1.GT.DEHI ) E1 = DEHI
               IF(E2.LT.DELOW) E2 = DELOW
               SUM    = SUM + A*(E1-E2) + B*DLOG(E1/E2)
  600          CONTINUE
      CROSS(J) = SUM / DELUU
      DEHI     = DELOW
  800 CONTINUE
      LOP1     = LOP2 + 1
 1000 CONTINUE
      NPNEW    = LOP1
      EMESHF(LOP1) = EBOUND(NOMESH+1)
      CROSS (LOP1) = CROSS (LOP1  -1)
C
      IF(IPRINT.GT.1) THEN
                      WRITE(NPRT,105)
                      WRITE(NPRT,106)
                      ENDIF
C
      IPASS  = 0
      JSW    = 1
      LOP1   = 1
      DSAVE  = 0.000D+0
      SUMX   = 0.000D+0
      IF(MT.EQ.  2)  IPOS = 1
      IF(MT.EQ. 18)  IPOS = 2
      IF(MT.EQ.102)  IPOS = 3
C
      DO  2000 N = 1 , NG
      SUM    = 0.0
      LOP2   = NI(N)*KFGP
      IF(NI(N).GT.NXG(JSW)) JSW = JSW + 1
      DELUU  = UFGP(JSW)
      DO  1500 J  = LOP1,LOP2
      SUM    = SUM + DELUU*CROSS(J)
 1500 CONTINUE
      DELUU  = UBGP(N) - DSAVE
      SIGX   = SUM /DELUU
      SUMX   = SUMX + SIGX
      DSAVE  = UBGP(N)
      SIG1D(IPOS,N) = SIGX
      IF(IPRINT.GT.1)
     +WRITE(NPRT,107) N,EE(N),EE(N+1),DELUU,SIGX,LOP1,LOP2
      LOP1   = LOP2 + 1
 2000 CONTINUE
      IF(SUMX.NE.0.0)  IPASS     = 1
C
      IF(IPRINT.GT.1) WRITE(NPRT,106)
      IF(IPASS.EQ.0.AND.IFISS.EQ.0.AND.MT.EQ.18) RETURN
C     OUTPUT TO PDS-FILE
      MEMBER (5:7) = '000'
      MEMBER (8:8) = IDTMP(IDPOS) (1:1)
      IF(MT.EQ.  2)  MEMBER (1:1) = 'E'
      IF(MT.EQ. 18)  MEMBER (1:1) = 'F'
      IF(MT.EQ.102)  MEMBER (1:1) = 'C'
      IF(MT.EQ. 18)  IFISS        =  1
C
      DO 4000 LOP = 1 , NSET
      LENG = (MEND(LOP)-MST(LOP)+1)*KFGP
      IST  = (MST(LOP)-1)*KFGP + 1
      MEMBER(5:5) = IDTMP(LOP) (1:1)
      IF(IPRINT.GT.1)
     @ WRITE(NSYSO,111) LOP,MST(LOP),MEND(LOP),IST,LENG,MEMBER
C
      IRC  = 0
      CALL SEARCH( MEMBER , LEN  , IRC )
      IF(IRC.EQ.0)  CALL DELETE ( MEMBER )
      CALL WRITE ( MEMBER ,CROSS(IST),LENG )
 4000 CONTINUE
C
      NP  =  MEND(NSET)*KFGP
      IF(MT.EQ.102)  CALL UMCONT(EMESHF,MEMBER)
C
      IF(JPLOT.EQ.1)
     +CALL UMCPLT(MEMBER,EMESHF,CROSS,NP,IMAX,X,Y,NPMAX,RQTEMP,MT)
C
  101 FORMAT(1H1,20X,'****************************************',
     *      /1H ,20X,'*      FINE CROSS SECTION TABLE        *',
     *      /1H ,20X,'****************************************',
     *      /1H ,20X,'*      MATERIAL NUMBER : ',I6,'        *',
     *      /1H ,20X,'*      FILE     NUMBER : ',I6,'        *',
     *      /1H ,20X,'*      REACTION NUMBER : ',I6,'        *',
     *      /1H ,20X,'*      NUMBER OF DATA  : ',I6,'        *',
     *      /1H ,20X,'*      TEMPERATURE     : ',F12.5,'  *',
     *      /1H ,20X,'*      Q-VALUE         : ',E12.5,'  *',
     *      /1H ,20X,'****************************************'/)
  104 FORMAT(1H ,10X,' E-LOW  & X-SECTION : ',1P2E15.8)
  102 FORMAT(1H ,10X,' E-HIGH & X-SECTION : ',1P2E15.8)
  105 FORMAT(
     *///1H ,5X,'GROUP  ','UPPER ENG.  ','LOWER ENG.  ',' LETHARGY   ',
     *     ' AVERAGED   ','LOP1   LOP2 ',
     *  /1H ,5X,'NUMBER ','  (EV)      ','  (EV)      ',' WIDTH      ',
     *     ' X-SECTION  ','            ')
  107 FORMAT(1H ,5X,I3,3X,1P4E12.5,2I6)
  106 FORMAT(1H ,3X,11(7H-------))
  109 FORMAT(1H ,' WARNING ---- MEMBER(',A8,') ALREADY EXISTS IN ',
     *       A8,'-FILE. SO OUTPUT IS SKIPPED ]] '/)
  110 FORMAT(1H ,' ERROR STOP AT SUBROUTINE(MCRFIL) BECAUSE DIMENION SIZ
     +E IS OVERFLOWED ]]]'/1H ,' RESERVED  SIZE IS ',I5,' WORDS .'
     +                    /1H ,' REQUESTED SIZE IS ',I5,' WORDS .')
  111 FORMAT(1H ,' ## LOP MST MEND IST LENG MEMBER ## ',5I6,2X,A8)
  112 FORMAT(1H ,' ## ISET EMAX XIN(ISET) ELOW EHIGH ## ',I6,1P4E12.5)
C
C     END OF PROCESS
C
      RETURN
      END
