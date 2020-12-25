      SUBROUTINE CONDEM(NODE,IC,NER,NREG,WT,WX,WT1,WX1)
C
C     SUBPROGRAM TO COLLAPSE A SET OF MACRO-X-SECTIONS
C     FOLLOWED BY CONCATENATION
C
C     IC=1  INPUT SET       FAST THERMAL GIVEN
C     IC=2  INPUT SET       ALL          GIVEN
C
      CHARACTER *4    FILENM
C
      COMMON /MAINC/ IOPT(38),ISCT,I40,ICTOT,ICOND,IP1C,IDUM(11)
     *               ,NEF,NET
     *               ,NERF,NERT,NMAT,DUM2(4),NOUT1,NOUT2
      COMMON /PDSPDS/ BUF(540),IFLSW,FILENM(3),ECODE,TEMPRY
C
      COMMON /WORK  / AA(8500),AAN(8500),AA1(8500)
     1               ,SIG(107,12),SIGN2T(107),XI(107) ,SIGT1(107)
     2               ,SIGS(107,107),SIGN2N(107,107) ,SIGS1(107,107)
     3               ,DUMY(770),BSIGF(15,107),XD(15,107),DD(8500)
     4               ,BLSIGF(15,107),EE(8500),AA2(8500)
C
      DIMENSION       NREG(*),WT(*),WX(*),II(1),IIN(1),WT1(*),WX1(*)
      CHARACTER*4     NODE(2),NODEX(2)
      DIMENSION       II1(1)
C
      EQUIVALENCE    (AA(1),II(1)),(AAN(1),IIN(1)),(AA1(1),II1(1))
C
C === INITAL SET
C
      FILENM(1)    = 'MACR'
      FILENM(2)    = 'OWRK'
      NODE(2)(1:1) = 'A'
      NODE(2)(4:4) = '4'
C
      CALL CLEA(  AA  , 8500 , 0.0 )
      CALL CLEA(  AAN , 8500 , 0.0 )
      CALL CLEA(  AA1 , 8500 , 0.0 )
      CALL CLEA(  DD  , 8500 , 0.0 )
      CALL CLEA(  EE  , 8500 , 0.0 )
      CALL CLEA(  AA2 , 8500 , 0.0 )
C === CONCATENATION =====
      IP1    = 0
      LTH0F  = 0
      LTH1F  = 0
      LTH0T  = 0
      LTH1T  = 0
      LTHN   = 0
      LTHD   = 0
      IF(IC.EQ.2) GO TO 100
C
C === READ SCATTERING MATRIX IN FAST RANGE PO & P1   ( IC=1 CASE )
C
   10 NODE(2) (1:1) = 'F'
      NODE(2) (4:4) = '3'
      ISW    = 0
      JSW    = 0
      CALL SEARCH(NODE,LTH1F,ISW)
      IF(ISW.EQ.0.AND.IP1C.EQ.1) THEN
                                 IP1 = 1
                                 CALL READ(NODE,AA1,LTH1F)
                                 NODE(2) (4:4) = '4'
                                 ELSE
                                 IP1 = 0
                                 NODE(2) (4:4) = '2'
                                 ENDIF
C
      CALL SEARCH(NODE,LTH0F,ISW)
      IF(ISW.EQ.1)   THEN
                     WRITE(NOUT1,40) NODE
                     RETURN
                     ENDIF
      CALL READ(NODE,AA,LTH0F)
CTFREE
      IF(IP1.EQ.1) THEN
                   NODE(2) (4:4) = '2'
                   CALL READ(NODE,AA2,LTH0F)
                   ENDIF
C === READ N2N DATA IF ANY
      NODE(2) (4:4) = 'M'
      CALL SEARCH(NODE,LTHN,ISW)
      IFN2N         = 0
      IF(ISW.EQ.0) THEN
                   IFN2N=1
                   CALL READ(NODE,AAN,LTHN)
                   ENDIF
C === READ  DELAYED NEUTRON DATA : WRITE ONLY FOR FISSIONABLE MATERIAL
      NODEX(1)       = NODE(1)
      NODEX(2)       = NODE(2)
      NODEX(2) (4:4) = 'Y'
      IDELAY         = 0
      CALL SEARCH(NODEX,LTHD,ISW)
      IF (ISW.EQ.0) THEN
                    IDELAY         = 1
                    NFAMLY         = 6
CM                  IF(LTHD.EQ.NEF*30) NFAMLY=15
                    IF(LTHD.EQ.NEF*45) NFAMLY=15
                    CALL READ(NODEX,DD,LTHD)
                    ENDIF
      IF(IOPT(4).EQ.0) GO TO 200
C
   40 FORMAT(' *** WARNING BY MISSING MEMBER ',2A4,' IN MACROWRK'
     * ,' FILE : ENCOUNTERED IN CONDENSE STEP')
C
C === READ SCATTERING MATRIX IN THERMAL RANGE PO & P1  ( IC=1 CASE )
C
      NODE (2) (1:1) = 'T'
      NODE (2) (4:4) = '2'
      IF(IP1.EQ.1) NODE (2) (4:4) = '4'
      CALL SEARCH(NODE,LTH0T,JSW)
      IF(JSW.EQ.1)   THEN
                     WRITE(NOUT1,40) NODE
                     RETURN
                     ENDIF
C
      CALL READ(NODE,AA(LTH0F+1),LTH0T)
      IF(IP1.EQ.1) THEN
                   NODE (2) (4:4) = '3'
                   CALL SEARCH(NODE,LTH1T,ISW)
                   IF(ISW.EQ.0) THEN
                                CALL READ(NODE,AA1(LTH1F+1),LTH1T)
                                ENDIF
                   NODE(2) (4:4) = '2'
                   CALL READ(NODE,AA2(LTH0F+1),LTH0T)
                   ENDIF
C === DELAYED NEUTRON DATA IN THERMAL RANGE
      IF (IDELAY.EQ.0) GO TO 200
      DO 70 I = NEF*NFAMLY+1,LTHD
      EE(I)   = DD(I)
   70 CONTINUE
C     86/01/21 THERMAL NON-FISSION AND FAST FISSIONABLE
      NODEX(2) (1:1) = 'T'
      CALL SEARCH(NODEX,LTHD1,ISWD)
      IF(ISWD.EQ.0) CALL READ(NODEX,EE(LTHD+1),LTHD1)
      LOC            = (NEF+NET)*NFAMLY
      LOC2           = LOC + LOC
      DO 80  I       = 1,NEF*NFAMLY
      DD(LOC+I)      = EE(NEF*NFAMLY+I)
      DD(LOC2+I)     = EE(2*NEF*NFAMLY+I)
   80 CONTINUE
      LOC            = LOC+NEF*NFAMLY
      LOC1           = LTHD+NET*NFAMLY
      LOCB           = LOC2 + NEF*NFAMLY
      LOC2           = LOC1 + NET*NFAMLY
      DO 90  I       = 1 , NET*NFAMLY
      IF (ISWD.EQ.0) THEN
                     DD(NEF*NFAMLY+I) = EE(LTHD+I)
                     DD(LOC+I)        = EE(LOC1+I)
                     DD(LOCB+I)       = EE(LOC2+I)
                     ELSE
                     DD(NEF*NFAMLY+I) = 0.0
                     DD(LOC+I)        = 0.0
                     DD(LOCB+I)       = 0.0
                     ENDIF
   90 CONTINUE
      GO TO 200
C
C === READ SCATTERING MATRIX IN ALL RANGE PO & P1  ( IC=2 CASE )
C
  100 CONTINUE
      IF(IOPT(4).EQ.0) NODE(2) (1:1) = 'F'
      NODE(2) (4:4) = '3'
      CALL SEARCH(NODE,LTH1F,ISW)
      IF(ISW.EQ.0.AND.IP1C.EQ.1) THEN
                                 IP1 = 1
                                 CALL READ(NODE,AA1,LTH1F)
                                 NODE(2) (4:4) = '4'
                                 ELSE
                                 IP1 = 0
                                 NODE(2) (4:4) = '2'
                                 ENDIF
C
      CALL SEARCH(NODE,LTH0F,ISW)
      IF(ISW.EQ.1)  THEN
                    WRITE(NOUT1,40) NODE
                    GO TO 10
                    ENDIF
C
      CALL READ(NODE,AA,LTH0F)
CTFREE
      IF(IP1.EQ.1) THEN
                   NODE(2) (4:4) = '2'
                   CALL READ(NODE,AA2,LTH0F)
                   ENDIF
C === N2N DATA
      NODE(2) (4:4) = 'M'
      CALL SEARCH(NODE,LTHN,ISW)
      IFN2N         = 0
      IF(ISW.EQ.0) THEN
                   IFN2N = 1
                   CALL READ(NODE,AAN,LTHN)
                   ENDIF
C === DELAYED NEUTRON DATA
      NODEX(1)      = NODE(1)
      NODEX(2)      = NODE(2)
      NODEX(2)(4:4) = 'Y'
      CALL SEARCH(NODEX,LTHD,ISW)
      IDELAY = 0
      IF (ISW.EQ.0) THEN
                    NFAMLY = 6
CM                  IF(LTHD.EQ.(NEF+NET)*30) NFAMLY=15
                    IF(LTHD.EQ.(NEF+NET)*45) NFAMLY=15
                    IDELAY = 1
                    CALL READ(NODEX,DD,LTHD)
                    ENDIF
C
C === CLEAR ARRAYS
C
  200 CONTINUE
      CALL CLEA ( WX     ,   NER , 0.0 )
      CALL CLEA ( WX1    ,   NER , 0.0 )
      CALL CLEA ( SIG    ,  1498 , 0.0 )
      CALL CLEA ( SIGT1  ,   107 , 0.0 )
      CALL CLEA ( SIGS   , 11449 , 0.0 )
      CALL CLEA ( SIGS1  , 11449 , 0.0 )
      CALL CLEA ( SIGN2N , 11449 , 0.0 )
      CALL CLEA ( SIGN2T ,   107 , 0.0 )
      IF (IDELAY.NE.0) THEN
                       CALL CLEA(BSIGF ,15*107,0.0)
                       CALL CLEA(XD    ,15*107,0.0)
                       CALL CLEA(BLSIGF,15*107,0.0)
                       ENDIF
CTFREE
      ISWTR = 0
      IF(IP1.EQ.1) THEN
                   ISWTR = 2
                   IF(IOPT(16).EQ.3.AND.IOPT(17).EQ.1) ISWTR = 1
                   ENDIF
C
C === PREPARE TRANSPORT CROSS SECTIONS OF FINE GROUP STRUCTURE ===
C
      IF(ISWTR.EQ.1) THEN
                   K1       = 0
                   DO 350 N = 1,NEF+NET
                   NJ       = N-II1(K1+1)
                   LEGT     = II1(K1+2)
                   K1       = K1 + 10
                   IF(LEGT.LE.0) GO TO 350
                   DO 340 J = 1,LEGT
                   K1       = K1+1
                   NJ       = NJ+1
                   SIGT1(NJ)=SIGT1(NJ)+AA1(K1)*WT1(N)
  340              CONTINUE
  350              CONTINUE
C ***************  SIGT1(N)  SIG P1 TOTAL OF FINE GROUP FOR THE MOMENT
                   DO 360 N = 1,NEF+NET
                   SIGT1(N)=0.33333333*SIGT1(N)/WT1(N)
  360              CONTINUE
                   ENDIF
C
C === ACCUMURATE REACTION RATE IN FEW GROUP STRUCTURE===
C
      K        =  0
      K1       =  0
      LOCB     =  0
      KN       =  0
      LOCX     = (NEF+NET)*NFAMLY
      LOCBL    = LOCX + LOCX
C
      DO 500 N = 1,NEF+NET
      ND       = NREG(N)
      WX (ND)  = WX (ND) + WT (N)
      WX1(ND)  = WX1(ND) + WT1(N)
      XI (ND)  = XI (ND) + AA(K+7)
CTFREE
      IF(ISWTR.EQ.1) THEN
                     SAVETR = AA(K+6) - SIGT1(N)
                     AA(K+8)= 0.333333333/SAVETR
                     AA(K+9)= 0.333333333/SAVETR
                     ENDIF
      IF(ISWTR.EQ.2) SIGT1(N) = AA(K+6) - AA2(K+6)
      IF(ICTOT.NE.0) THEN
                     SAVE1  = AA(K+8)
                     SAVE2  = AA(K+9)
                     IF(SAVE1.NE.0.0)  AA(K+8) = 0.333333333/SAVE1
                     IF(SAVE2.NE.0.0)  AA(K+9) = 0.333333333/SAVE2
                     ENDIF
CEND
C     REACTION RATE
      DO 430 J  = 1,8
      SIG(ND,J) = SIG(ND,J) + AA(K+2+J)*WT(N)
  430 CONTINUE
CMOD  IF(IFN2N.NE.0) SIGN2T(ND) = SIGN2T(ND) + WT(N)*AAN(KN+6)
      IF(IFN2N.NE.0.AND.KN.LT.LTHN) THEN
         SIGN2T(ND) = SIGN2T(ND) + WT(N)*AAN(KN+6)
         ENDIF
C
C     ACT FISS NFIS TOT KI D1 D2 ABSP 1/SIGTR SIGTJ SIGT1J SIGT1F
C **  SIG(N, 9) HARMONIC AVERAGE OF SIGTR BY F
C **  SIG(N,10) ARITHMTC AVERAGE OF SIGT0 BY J
C **  SIG(N,11) ARITHMTC AVERAGE OF SIGT1 BY J
C
      SIG(ND, 9)= SIG(ND, 9) + WT(N)/(AA(K+6)-SIGT1(N))
      IF(IP1.GT.0) THEN
                   SIG(ND,10) = SIG(ND,10) + AA (K +6)*WT1(N)
                   SIG(ND,11) = SIG(ND,11) + AA1(K1+6)*WT1(N)
                   ENDIF
C     DELAYED NEUTRON DATA
      IF (IDELAY.EQ.0) GO TO 450
      DO 440    IG = 1,NFAMLY
      LOCB         = LOCB + 1
      LOCX         = LOCX + 1
      BSIGF(IG,ND) = BSIGF(IG,ND) + DD(LOCB)*WT(N)
      XD   (IG,ND) = XD   (IG,ND) + DD(LOCX)
CADD
      LOCBL        = LOCBL + 1
      BLSIGF(IG,ND)= BLSIGF(IG,ND) + DD(LOCBL)*WT(N)
  440 CONTINUE
C
CT441 FORMAT(1H ,' # NREG # ',20I6)
C ==  P0 SCATTERING
  450 CONTINUE
      NJ           = N - II(K+1)
      LEGT         =     II(K+2)
      K            = K + 10
CT    IF(N.EQ.NEF+1) WRITE(6,*) ' ** N NJ LEGT K ** ',N,NJ,LEGT,K
C
      DO 460    J  = 1,LEGT
      K            = K + 1
      NJ           = NJ+ 1
      NJD          = NREG(NJ)
      SIGS(ND,NJD) = SIGS(ND,NJD) + AA(K)*WT(N)
  460 CONTINUE
C === P1 SCATTERING
      IF(IP1.GT.0) THEN
                   NJ       = N  - II1(K1+1)
                   LEGT     =      II1(K1+2)
                   K1       = K1 + 10
                   DO 470 J = 1,LEGT
                   K1       = K1 +  1
                   NJ       = NJ +  1
                   NJD      = NREG(NJ)
                   SIGS1(ND,NJD) = SIGS1(ND,NJD) + AA1(K1)*WT1(N)
  470              CONTINUE
                   ENDIF
C ==  N2N DATA
      IF(IFN2N.NE.0 .AND. KN.LT.LTHN) THEN
      NJ            = N  - IIN(KN+1)
      LEGN          =      IIN(KN+2)
      KN            = KN + 10
      DO 480     J  = 1,LEGN
      KN            = KN +  1
      NJ            = NJ +  1
      NJD           = NREG(NJ)
      SIGN2N(ND,NJD)= SIGN2N(ND,NJD) + AAN(KN)*WT(N)
  480 CONTINUE
                                      ENDIF
  500 CONTINUE
C
C === NORMALIZATION & PACK INTO ONE-DIMENSIONAL ARRAY
C
      K        = 0
      K1       = 0
      LOCB     = 0
      KN       = 0
      LOCX     = NER*NFAMLY
      LOCBL    = LOCX + LOCX
C
      CALL CLEA(  AA  , 8500 , 0.0 )
      CALL CLEA(  AAN , 8500 , 0.0 )
      CALL CLEA(  AA1 , 8500 , 0.0 )
      CALL CLEA(  DD  , 8500 , 0.0 )
C
      DO 800 N = 1,NER
      DO 550 J = 1,8
      AA(K+2+J)= SIG(N,J)/WX(N)
  550 CONTINUE
      AA(K+7)  = XI(N)
C     N2N TOTAL
      IF(IFN2N.NE.0) AAN(KN+6) = SIGN2T(N)/WX(N)
C === DELAYED NEUTRON DATA
      IF (IDELAY.EQ.0) GO TO 570
      DO 560 IG = 1,NFAMLY
      LOCB      = LOCB + 1
      LOCX      = LOCX + 1
      LOCBL     = LOCBL+ 1
      DD(LOCB)  = BSIGF(IG,N)/WX(N)
      DD(LOCX)  = XD   (IG,N)
      DD(LOCBL) = BLSIGF(IG,N)/WX(N)
  560 CONTINUE
C
  570 CONTINUE
      KSAVE     = K
      TTSAVE    = AA(K+6)
      SIG(N,9)  = WX(N)/SIG(N,9)
      IF(IP1.GT.0) THEN
                   SIG(N,10)=SIG(N,10)/WX1(N)
                   SIG(N,11)=SIG(N,11)*0.33333333/WX1(N)
                   ENDIF
C
C **  SIG(N, 9) HARMONIC AVERAGE OF SIGTR BY F
C **  SIG(N,10) ARITHMTC AVERAGE OF SIGT0 BY J
C **  SIG(N,11) ARITHMTC AVERAGE OF SIGT1 BY J
C
C === HARMONIC AVERAGE OF TRANSPORT CROSS SECTION
      IF(ICTOT.EQ.0) THEN
                     AA(K+6) = SIG(N,9)
                     IF(IP1.GT.0)  THEN
                                   AA(K+6)   = AA(K+6) + SIG(N,11)
                                   AA1(K1+6) = 3.0*SIG(N,11)
                                   ENDIF
                     ENDIF
C ================= ARITHMTC AVERAGE OF TRANSPORT CROSS SECTION
      IF(ICTOT.NE.0) THEN
                     AA(K+6)   = TTSAVE
                     SAVED1    = AA(K+8)
                     SAVED2    = AA(K+9)
                     IF(SAVED1.NE.0.0) AA(K+8) = 0.3333333333/SAVED1
                     IF(SAVED2.NE.0.0) AA(K+9) = 0.3333333333/SAVED2
C
                     IF(IP1.GT.0)  THEN
                                   AA1(K1+6) = 3.0*SIG(N,11)
                                   ENDIF
                     ENDIF
C
      DIFP0    = TTSAVE - AA(K+6)
C
C === FIND VECTOR LENGTH FOR P0
C
      DO 600  L = 1,N
      LS        = L
      IF(SIGS(N,L).NE.0) GO TO 610
  600 CONTINUE
  610 II(K+1)   = N   - LS + 1
      DO 620 LD = N,NER
      LA        = NER - LD + N
      IF(SIGS(N,LA).NE.0.) GO TO 630
  620 CONTINUE
  630 II(K+2)   = LA  - LS + 1
C === SCATTERING VECTOR
      K         = K + 10
      DO 650 LD = LS,LA
      K         = K +  1
      AA(K)     = SIGS(N,LD)/WX(N)
  650 CONTINUE
C === MODIFY SELF-SCATTERING CROSS SECTION DUE TO HARMONIC AVRAGE SIGT
      LS        = KSAVE + 10 +II(KSAVE+1)
      AA(LS)    = AA(LS) - DIFP0
C === FIND VECTOR LENGTH FOR P1
      IF(IP1.GT.0) THEN
                   K1SAVE    = K1
                   DO 670  L = 1,N
                   LS        = L
                   IF(SIGS1(N,L).NE.0) GO TO 680
  670              CONTINUE
  680              II1(K1+1) = N   - LS + 1
                   DO 690 LD = N,NER
                   LA        = NER - LD + N
                   IF(SIGS1(N,LA).NE.0.) GO TO 700
  690              CONTINUE
  700              II1(K1+2) = LA  - LS + 1
C ================ SCATTERING VECTOR
                   K1        = K1  + 10
                   SUM       = 0.0
                   DO 710 LD = LS,LA
                   K1        = K1  +  1
                   AA1(K1)   = SIGS1(N,LD)/WX1(N)
                   SUM       = SUM +  AA1(K1)
  710              CONTINUE
C === MODIFY SELF-SCATTERING CROSS SECTION TO MEET P1 TOTAL BALANCE
                   LS        = K1SAVE  +  10 + II1(K1SAVE+1)
                   AA1(LS)   = AA1(LS) - SUM + AA1(K1SAVE+6)
C === MODIFY SELF-SCATTERING CROSS SECTION TO CANCEL THE ANGUALAR
C     DEPENDENCY OF TOTAL X-SECTION
                   SIGP1     = AA1(K1SAVE+6)
*     WRITE(6,*) ' *** NODE  NG  ICTOT *******',NODE,N,ICTOT
*     WRITE(6,*) ' **AA1(LS) DIFP0 SUM SIGP1**',AA1(LS),DIFP0,SUM,SIGP1
                   AA1(LS)   = AA1(LS) + (TTSAVE - SIG(N,10))*3.00000
                   AA1(K1SAVE+6)=SIGP1 + (TTSAVE - SIG(N,10))*3.00000
*     WRITE(6,*) ' **AA1(LS) TTSAVE SIG(N,10)*',AA1(LS),TTSAVE,SIG(N,10)
                   ENDIF
C
C === FIND N2N VECTOR LENGTH
C
      IF(IFN2N.EQ.0) GO TO 800
      IIN(KN+1) = 1
      LS        = N
      DO 730 LD = N,NER
      LA        = NER - LD + N
      IF(SIGN2N(N,LA).NE.0.) GO TO 740
  730 CONTINUE
CDEL  GO TO 800
      LA          = N
      SIGN2N(N,N) = 0.0
      AAN(KN+6)   = 0.0
CEND
  740 IIN(KN+2) = LA  - LS + 1
C === SCATTERING VECTOR
      KN        = KN  + 10
      DO 750 LD = LS,LA
      KN        = KN  +  1
      AAN(KN)   = SIGN2N(N,LD)/WX(N)
  750 CONTINUE
C
  800 CONTINUE
C
C ==  OUTPUT COLLAPESED EFFECITVE MACROSCOPIC X-SECTION INTO MACRO FILE
C
      LTH0          = K
      LTH1          = K1
      LTHN          = KN
      FILENM(1)     = 'MACR'
      FILENM(2)     = 'O   '
C ==  WRITE P0 MATRIX
      NODE(2) (1:1) = 'A'
      NODE(2) (4:4) = '0'
      IF(IOPT(4).EQ.0) NODE(2) (1:1) = 'F'
CM    CALL OVRWRT(NODE,AA,LTH)
      CALL SEARCH(NODE,LEN,ISW)
      IF (ISW.EQ.1) THEN
                    CALL WRITE(NODE,AA,LTH0)
                    ELSE
                    WRITE(NOUT1,*) ' WARNING **** ',NODE,' ALREADY',
     @                             ' EXISTS IN MACRO FILE ]] '
                    WRITE(NOUT2,*) ' WARNING **** ',NODE,' ALREADY',
     @                             ' EXISTS IN MACRO FILE ]] '
                    ENDIF
C ================= WRITE P1 MATRIX
      IF(IP1.GT.0)  THEN
                    NODE(2) (4:4) = '1'
                    CALL SEARCH(NODE,LEN,ISW)
                    IF(ISW.EQ.1) THEN
                                 CALL WRITE(NODE,AA1,LTH1)
                                 ELSE
                    WRITE(NOUT1,*) ' WARNING **** ',NODE,' ALREADY',
     @                             ' EXISTS IN MACRO FILE ]] '
                    WRITE(NOUT2,*) ' WARNING **** ',NODE,' ALREADY',
     @                             ' EXISTS IN MACRO FILE ]] '
                                 ENDIF
                    ENDIF
C ================== N2N DATA
      IF(IFN2N.NE.0) THEN
                     NODE(2) (4:4) = 'N'
                     CALL SEARCH(NODE,LEN,ISW)
                     IF (ISW.EQ.1) THEN
                                   CALL WRITE(NODE,AAN,LTHN)
                                   ELSE
                     WRITE(NOUT1,*) ' WARNING **** ',NODE,' ALREADY',
     @                              ' EXISTS IN MACRO FILE ]] '
                     WRITE(NOUT2,*) ' WARNING **** ',NODE,' ALREADY',
     @                             ' EXISTS IN MACRO FILE ]] '
                                   ENDIF
                     ENDIF
C ==================== DELAYED NEUTRON DATA WRITE
      IF (IDELAY.NE.0) THEN
                       NODEX(1)       = NODE(1)
                       NODEX(2)       = NODE(2)
                       NODEX(2) (4:4) = 'Z'
CM                     CALL OVRWRT(NODEX,DD,NER*NFAMLY*2)
                       CALL SEARCH(NODEX,LEN,ISW)
                       IF (ISW.EQ.1) THEN
CM                                   CALL WRITE(NODEX,DD,NER*NFAMLY*2)
                                     CALL WRITE(NODEX,DD,NER*NFAMLY*3)
                                 ELSE
                       WRITE(NOUT1,*) ' WARNING **** ',NODEX,' ALREADY',
     @                                ' EXISTS IN MACRO FILE ]] '
                       WRITE(NOUT2,*) ' WARNING **** ',NODEX,' ALREADY',
     @                                ' EXISTS IN MACRO FILE ]] '
                                 ENDIF
                       ENDIF
C === WRITE FLUX
      FILENM(1)='FLUX'
      FILENM(2)='    '
      NODE(2)(4:4) = 1H0
C     CALL OVRWRT(NODE,WX,NER)
      CALL SEARCH(NODE,LEN,ISW)
                       IF (ISW.EQ.1) THEN
                                     CALL WRITE(NODE,WX,NER)
                                 ELSE
                       WRITE(NOUT1,*) ' WARNING **** ',NODE,' ALREADY',
     @                                ' EXISTS IN FLUX FILE ]] '
                       WRITE(NOUT2,*) ' WARNING **** ',NODE,' ALREADY',
     @                                ' EXISTS IN FLUX FILE ]] '
                                 ENDIF
C
C     END OF PROCESS
C
      RETURN
      END
