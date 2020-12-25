C***********************************************************************
C                          MAFCAL
C***********************************************************************
      SUBROUTINE MAFCAL(MTNAME,NISO  ,TEMP  ,XL    ,DC    ,ISW   ,IDENT,
     1                  IRES  ,DN    ,LXMICR,DANCOF,ISWF  ,MATD  ,VOLM ,
     2                  NAMRES,IND   ,MCODE ,DENRES,KCODE ,IDENTH,DENHM,
     3                  SIG0HM,SSTHM ,SIGTHM,NCODE ,ENBND ,LSS   ,LGV  ,
     4                  SIGC  ,SIGF  ,SIGFNU,SIGT  ,CHI   ,D1    ,D2   ,
     5                  SIGA  ,SIGN2T,SIGS  ,SIGN2N,SIG0  ,SIGWF ,SIGWM,
     6                  SIGWT ,FMTX  ,TSIG  ,SIGTIJ,GAMMA ,PIJ   ,DENWRK
     7                 ,SSTWRK,SFCTR ,X1    ,X2    ,Y1    ,Y2    ,WK1  ,
     8                  WK2   ,WK3   ,LTH   ,LA    ,LD    ,FTEMP ,FSIG0,
     9                  SSC   ,SSF   ,SSNU  ,SCHI  ,SSTR  ,SSE   ,SST  ,
     A                  SSIN  ,SS2N  ,UM    ,SMTX  ,STR   ,FTAB  ,CHIMTX
     B                 ,WTFLUX,SSFNU ,SFACT )
C
C     MAFCAL ----- CALCULATE EFFECTIVE CROSS SECTION
C
      DOUBLE PRECISION  JNEFST,FNEFST,JNMACR,FNMACR
C
      COMMON  /PDSPDS/ BUFFER(540),IFLSW,NFILE(3),ECODE,ITEMP
CMOD  COMMON  /$D2O  / ID2O
      COMMON /D2OCM / ID2O
C
      COMMON  /MAFCNL/ IOPT(20),JNEFST,FNEFST,JNMACR,FNMACR,NEF,IGT,
     +                 NMAT,KNMAX,MXMTX,MXTEMP,MXSIG0,LNMAX,IDS,NEF1,
     +                 MXREAC,NOUT1,NOUT2,NORES,NMP,NISOHM,ISNCAL,IPLMAX
     +                ,IGMAX,NET,NET5
      COMMON  /MAFCNT/ AMASS,SIGP,ICAPT,IFISS,IIRES,LTOT,IFS,IFTR,IFC,
     1                 IFF,IFE,IFER,NGMIN,NGMAX,NSIG,NTEMP,SIGC0
      COMMON  /MAFDBL/ MTREPL,MICFL,MICMOD,IPATH,SXLL,SBELL,SVF,SVM,
     1                 SDAN  ,SGAMMA,IGEOMS
      COMMON  /MAFWRK/ A(17000),NAMEP(2),LOCAM(11),LOCAF(6)
C
CMOD  PARAMETER   ( MXNISO = 110 , MAXNG = 107  , MAXMT3 = 6 )
      INCLUDE  'BMICRINC'
      COMMON   /MICCOM/ EFFMIC(MAXNG,MAXMT3,MXNISO),LENEFF
C
      CHARACTER *4     NFILE,MTNAME,IDENT,NAMEP
      CHARACTER *4     NAMEXX,NAMEYY,ICHRA8
      CHARACTER *8     IDMT
      CHARACTER *1     IDIPL(5)
      CHARACTER *4     IDMPL(5)
C
      DIMENSION   MTNAME(2,NMAT),NISO(NMAT),TEMP(NMAT),XL(NMAT),DC(NMAT)
      DIMENSION   ISW(NMAT),IDENT(2,KNMAX,NMAT),IRES(KNMAX,NMAT)
      DIMENSION   DN(KNMAX,NMAT),LXMICR(KNMAX,NMAT)
      DIMENSION   DANCOF(KNMAX,NMAT),ISWF(NMAT),MATD(NMP),VOLM(NMAT)
C
      DIMENSION   NAMRES(NORES),IND(NORES,NMAT),MCODE(NORES,NMAT)
      DIMENSION   DENRES(NORES,NMAT),KCODE(NORES,NMAT)
C
      CHARACTER*8  IDENTH(NISOHM)
      DIMENSION   DENHM(NISOHM),SIG0HM(NEF,NISOHM)
      DIMENSION   SSTHM(NEF,2,NISOHM),SIGTHM(NEF,NMAT)
      DIMENSION   NCODE(NISOHM,NMAT),DENWRK(NISOHM),SSTWRK(NISOHM)
C
      DIMENSION   ENBND(NEF1),LSS(NEF),LGV(NEF)
      DIMENSION   SIGC(NEF),SIGF(NEF),SIGFNU(NEF),SIGT(NEF),CHI(NEF)
      DIMENSION   D1(NEF),D2(NEF),SIGA(NEF),SIGN2T(NEF)
      DIMENSION   SIGS(IDS,NEF),SIGN2N(IDS,NEF)
      DIMENSION   SIGWF(NEF),SIGWM(NEF),SIGWT(NEF,1)
      DIMENSION   SIG0(NEF,KNMAX),FMTX(3,NEF,KNMAX)
      DIMENSION   TSIG(NMAT),SIGTIJ(NEF,NMP),GAMMA(NMP,NMP)
      DIMENSION   PIJ(NMP,NMP,NEF)
C
      DIMENSION  SFCTR(NEF,MXREAC),X1(MXTEMP),X2(MXSIG0),Y1(MXTEMP),
     1           Y2(MXSIG0),WK1(LNMAX),WK2(LNMAX),WK3(LNMAX)
C
      DIMENSION  LTH(MXMTX),LD(MXMTX),LA(MXMTX),
     1           FTEMP(MXTEMP),FSIG0(MXSIG0)
C
      DIMENSION  SSC(NEF),SSF(NEF),SSNU(NEF),SCHI(NEF),SSTR(NEF),
     1           SSE(NEF),SST(NEF),SSIN(NEF),SS2N(NEF),UM(NEF),
     2           SMTX(IDS,NEF),STR(IDS,NEF),FTAB(MXSIG0,MXTEMP,NEF)
      DIMENSION  CHIMTX(NEF,IGMAX),WTFLUX(IGMAX,NMAT)
      DIMENSION  SSFNU(IGMAX,KNMAX,NMAT),SFACT(KNMAX,NMAT)
C
C-----LOCAL ARRAY FOR DELAYED NEUTRON DATA
C
      DIMENSION   IA(17000),ZERO(74)
      DIMENSION   DD(3330),DD1(761)
CMOD  DIMENSION   BVSIGF(15,74),XD(15,74),XDBAR(15,74)
      DIMENSION   BVSIGF(15,74),XD(15,74),XDBAR(15),BLSIGF(15,74)
      EQUIVALENCE (A(1),IA(1)),(DD(534),DD1(1))
C
      DATA  IDIPL  /' ','Q','S','T','U'/
      DATA  IDMPL  /'   3','   5','   6','   7','   8'/
      DATA  ZERO   /74*0.0/
C
C     INITIAL SET
C
      ISTONE   = 0
      IFLSW    = 1
      IOPT19   = IOPT(19)
      NFILE(1) = 'MACR'
      NFILE(2) = 'OWRK'
      BELL     = 1.200
      IF(IGT.EQ.1)  BELL = 1.40
C
C     SIGMA-0 AND DANCOFF CORRECTION FACTOR CALCULATION
C
      CALL  MAFSIG(MTNAME,NISO  ,XL    ,DC    ,IDENT ,IRES  ,DN    ,
     1             LXMICR,SIG0  ,TSIG  ,PIJ   ,ENBND ,ISW   ,SIGWF ,
     2             SIGWM ,SIGWT ,DANCOF,NAMRES,IND   ,MCODE ,DENRES,
     3             SIGTIJ,GAMMA ,MATD  ,VOLM  ,SIG0HM,SSTHM ,SIGTHM,
     4             NCODE ,KCODE ,ISWF  ,SIGT  ,ISTONE   )
C
      IF(IOPT(3).EQ.2)       THEN
             CALL  MAFTON(   NISO  ,IRES  ,DN    ,SIG0  ,ISW   ,VOLM  ,
     1                       IDENTH,DENHM ,SIG0HM,SSTHM ,SIGTHM,NCODE ,
     2                       DENWRK,SSTWRK,SIGTIJ,PIJ   ,MATD  ,ISTONE,
     3                       MTNAME,IDENT ,DANCOF,XL    ,BELL  )
                             ENDIF
C
C     CROSS SECTION CALCULATION START
C
      IFLSW    = 1
      NFILE(1) = 'FAST'
      NFILE(2) = 'U   '
      LENG1    = 2*NEF
      LENG2    = NEF*(9+2*IDS)
      LENG3    = NEF*(10+2*IDS)
      LENG4    = NEF*KNMAX
      LENG5    = MXSIG0*MXTEMP*NEF
      LENG6    = NEF*MXREAC
      LENG7    = NEF*IDS
      LENG8    = NEF*KNMAX*3
      LENG9    = NEF*IGMAX
C
      REWIND 3
C    **********************************************
C    *          LOOP OF MATERIAL                  *
C    **********************************************
      DO 2000 NN=1,NMAT
      CALL CLEA(SIG0,LENG4,0.0)
      READ(3) II,SIG0
      IF(ISW(NN).NE.3)  GO TO 2000
      MMK=NISO(NN)
      IF(MMK.LE.0)      GO TO 2000
C-----INITIAL SET
      CALL  CLEA(SIGC,LENG2,0.0)
      CALL ICLEA(LSS ,LENG1,0  )
      CALL  CLEA(FMTX,LENG8,1.0)
CADD
      CALL  CLEA(EFFMIC,LENEFF,0.0)
C---- N2N     NEUTRON DATA
      IFN2N     = 0
C---- DELAYED NEUTRON DATA
      NDELAY    = 0
C---- DEUTERIUM (GAMMA,N) DELAYED NEUTRON
      NFAMLY    = 6
      CALL  CLEA ( DD , 3330 , 0.0 )
      IF(ID2O.EQ.1) THEN
                    NFILE(1)='FAST'
                    NFILE(2)='U   '
                    NFAMLY  = 15
                    LENGD   = 21+10*NEF
                    CALL READ('YD020000',DD1,LENGD)
                    ENDIF
C
      CALL  CLEA(BVSIGF,15*NEF,0.0)
      CALL  CLEA(BLSIGF,15*NEF,0.0)
      CALL  CLEA(XD    ,15*NEF,0.0)
CMOD  CALL  CLEA(XDBAR ,15*NEF,0.0)
      CALL  CLEA(XDBAR ,15    ,0.0)
C
      RTEMP     = TEMP(NN)
      XLL       = XL(NN)
      IF(XLL.LE.0.0)  XLL=1.0E+20
C---------------------CHECK TEMPERATURE  JAN 24/85 TSUCHI
      IF(RTEMP.LE.0.) THEN
                      WRITE(NOUT1,1200)  NN
                      ENDIF
C    **********************************************
C    *          LOOP OF NUCLIDE                   *
C    **********************************************
      DO 600 MM = 1,MMK
      DNTMP     = DN(MM,NN)
C     WEIGHT FOR FISSION SPECTRUM WHTCHI
      WHTCHI    = SFACT(MM,NN)*DNTMP
CDEL  IF(DNTMP.LE.0.0)      GO TO 600
      IF(IRES(MM,NN).EQ.-1) GO TO 600
C
      NAMEP(1)=IDENT(1,MM,NN)
      NAMEP(2)=IDENT(2,MM,NN)
      IF(IRES(MM,NN).NE.1) THEN
                           NFILE(1)='FAST'
                           NFILE(2)='U   '
                           NAMEP(2)(4:4) = '0'
                              ELSE
                              NFILE(1)='MICR'
                              NFILE(2)='EF  '
                              NAMEP(1)(1:1) = 'C'
                              NAMEP(2)(1:1) = 'F'
                              KANS     = 0
                              CALL  SEARCH(NAMEP(1),LENG,KANS)
                              IF(KANS.EQ.1)  THEN
                                             NAMEP(2)(4:4) = '0'
                                             JANS = 0
           WRITE(NOUT1,*) ' *** MEMBER ',IDENT(1,MM,NN),IDENT(2,MM,NN),
     +                                 ' NOT FOUND IN FILE DD=MICREF'
           WRITE(NOUT2,*) ' *** MEMBER ',IDENT(1,MM,NN),IDENT(2,MM,NN),
     +                                 ' NOT FOUND IN FILE DD=MICREF'
                                   CALL  SEARCH(NAMEP(1),LENG,JANS)
                                   IF(JANS.EQ.1) THEN
                                                 NFILE(1) = 'FAST'
                                                 NFILE(2) = 'U   '
                                                 NAMEP(2) = '0000'
           WRITE(NOUT1,*) ' *** MEMBER ',IDENT(1,MM,NN),IDENT(2,MM,NN),
     +                                 ' IS RESET ',NAMEP(1)(2:4),'0000'
           WRITE(NOUT2,*) ' *** MEMBER ',IDENT(1,MM,NN),IDENT(2,MM,NN),
     +                                 ' IS RESET ',NAMEP(1)(2:4),'0000'
                                                 ENDIF
                                             ENDIF
                           ENDIF
C
      CALL  MAFCON(LTH   ,LA    ,LD    ,FTEMP ,FSIG0)
C
C     HETEROGENEITY CORRECTION
C
      DANCF  = DANCOF(MM,NN)
      SS     = ( BELL / (1.0+(BELL-1.0)*DANCF) ) * ( 1.0-DANCF )/XLL
CMOD  S      = SS   / DNTMP
      IF(DNTMP.GE.1.0000E-15)  THEN
                               S = SS / DNTMP
                               ELSE
                               S = SS * 1.000E+15
                               ENDIF
C-----IF TONE'S METHOD IS ADOPPTED , THEN SET S ZERO.
      IF(IOPT(3).EQ.2.AND.VOLM(NN).GT.0.0)  S = 0.0
C
      DO 150 I=1,NEF
      SIG0(I,MM)=SIG0(I,MM)+S
  150 CONTINUE
C
      IF(IPATH.NE.1)      GO TO 200
      IF(NN.NE.MTREPL)    GO TO 200
      IF(IIRES.NE.1)      GO TO 200
      IF(DNTMP.LT.1.0E-6) GO TO 200
C
C   ********************************************
C   * DOUBLE HETRO. TREATMENT 1984/4/4         *
C   ********************************************
C
      JPOS    = 0
      JJK     = NISO(MICFL)
      NAMEXX  = IDENT(1,MM,NN)
      NAMEXX (1:1) = '0'
      DO  160 I = 1 ,JJK
      NAMEYY    = IDENT(1,I,MICFL)
      NAMEYY (1:1) = '0'
      IF(NAMEXX.EQ.NAMEYY) JPOS = I
  160 CONTINUE
      IF(JPOS.EQ.0)  GO TO 200
      DNTMP1    =  DN(JPOS,MICFL)
      IF(DNTMP1.LE.1.00E-15)  DNTMP1 = 1.000E-15
C
CM    WRITE(6,171) MTNAME(1,NN),MTNAME(2,NN),IDENT(1,MM,NN),
CM   *             IDENT(2,MM,NN),NN,MM,DNTMP,DNTMP1,MTREPL,JPOS,SS,S
C
      DO 170 I = 1 , NEF
      SIGTTT= SIGWT(I,MM)
      SIG01 = SIGWF(I)/DNTMP1 - SIGTTT
      IF(JJK.EQ.1) SIG01 = 0.0
      BETA1 = SIGWM(I)*SVM
      BETA  = BETA1 / ( BETA1 + SS )
      SIG02 = ( (1-SDAN)/SXLL )*( SBELL/( 1.0+SDAN*( SBELL*BETA-1.0 ) ))
      SIG03 = SIG02/DNTMP1
      SIG04 = SIG01 + SIG03
CM    WRITE(6,172)I,SIGTTT,BETA1,BETA,SIG01,SIG02,SIG03,SIG04,SIG0(I,MM)
      SIG0(I,MM) = SIG04
  170 CONTINUE
C
CM171 FORMAT(/1H ,10X,'  ## CHECK WRITE AT MACROF FOR W-HETERO ## ',
CM   *       /1H , 5X,' MTNAME ===> ',2A4,
CM   *       /1H , 5X,' NUCLIDE===> ',2A4,
CM   *       /1H , 5X,' NN MM  ===> ',2I8,
CM   *       /1H , 5X,' DNTMP  ===> ',E12.5,
CM   *       /1H , 5X,' DNTMP1 ===> ',E12.5,
CM   *       /1H , 5X,' MTREPL ===> ',I8,
CM   *       /1H , 5X,' JPOS   ===> ',I8,
CM   *       /1H , 5X,' SS     ===> ',E12.5,
CM   *       /1H , 5X,' S      ===> ',E12.5,//1H ,
CM   *' ##GRP SIGTT       BETA1       BETA        SIG01       ',
CM   *'SIG02       SIG03       SIG0-NEW    SIG0-OLD ##')
CM172 FORMAT(1H ,I5,1P9E12.5)
C
C     ***************************************
C     * CALCULATES SELF-SHIELDING FACTORS   *
C     ***************************************
  200 CONTINUE
      CALL  CLEA(SFCTR,LENG6,1.0)
      IF(DNTMP.LE.1.0E-15) GO TO 301
      IF(IFS.EQ.0)       GO TO 301
      IF(NGMAX.LE.0)     GO TO 301
      IF(NGMIN.GT.NGMAX) GO TO 301
      IF(NGMIN.LE.0)  NGMIN=1
C
      CALL  CLEA(A,17000,1.0)
      NAMEP(1) (1:1) = 'F'
      CALL  SEARCH(NAMEP(1),LENG,IANS)
      IF(IANS.EQ.1)  GO TO 301
      CALL  READ  (NAMEP(1),A,LENG)
C
      DO 300 MT=1,MXREAC
      CALL  CLEA(FTAB,LENG5,1.0)
      GO TO (210,220,230,240,250,300,300) ,MT
  210 CONTINUE
      IF(IFTR.EQ.0) GO TO 300
CKSK  IDMT=8HTTR
      IDMT='TTR     '
      GO TO 280
  220 CONTINUE
      IF(ICAPT.EQ.0) GO TO 300
      IF(IFC.EQ.0)   GO TO 300
CKSK  IDMT=8HCAPT
      IDMT='CAPT    '
      GO TO 280
  230 CONTINUE
      IF(IFISS.EQ.0) GO TO 300
      IF(IFF.EQ.0)   GO TO 300
CKSK  IDMT = 8HFISS
      IDMT = 'FISS    '
      GO TO 280
  240 CONTINUE
      IF(IFE.EQ.0)   GO TO 300
CKSK  IDMT = 8HELAS
      IDMT = 'ELAS    '
      GO TO 280
  250 CONTINUE
      IF(IFER.EQ.0)  GO TO 300
CKSK  IDMT = 8HELAR
      IDMT = 'ELAR    '
  280 CONTINUE
      IST  = LOCAF(MT)
      IF(IST.LE.0)  GO TO 300
      DO 290    I = NGMIN,NGMAX
      DO 290    K = 1,NTEMP
      DO 290    J = 1,NSIG
      FTAB(J,K,I) = A(IST)
      IST         = IST+1
  290 CONTINUE
C
      CALL SPLINE(FTAB  ,SFCTR(1,MT)  ,SIG0(1,MM)   ,RTEMP ,FSIG0 ,
     +            FTEMP ,X1    ,X2    ,Y1    ,Y2    ,WK1   ,WK2   ,
     +            WK3   ,MXSIG0,MXTEMP,LNMAX ,NEF   ,NTEMP ,NSIG  ,
     +            NGMIN ,NGMAX )
  300 CONTINUE
C     ***************************************
C     * READ INFINTE MICROSCOPIC X-SECTION  *
C     ***************************************
  301 CONTINUE
      IF(LTOT.LE.0)  GO TO 600
      CALL CLEA(A,17000,0.0)
      NAMEP(1) (1:1) = 'M'
      CALL READ(NAMEP(1),A,LTOT)
      CALL  CLEA(SSC,LENG3,0.0)
C ----CAPTURE
      IF(ICAPT.EQ.0)  GO TO 311
      IST       = LOCAM(1)
      DO 1311 I = 1,NEF
      SSC(I)    = A(IST)
      IST       = IST+1
 1311 CONTINUE
C ----FISSION
  311 CONTINUE
      IF(IFISS.EQ.0)  GO TO 313
      IST       = LOCAM(2)
      IF(IST.LE.0)  GO TO 312
      DO 1301 I = 1,NEF
      SSF(I)    = A(IST)
      IST       = IST+1
 1301 CONTINUE
      IST       = LOCAM(4)
      DO 1302 I = 1,NEF
      SCHI(I)   = A(IST)
      IST       = IST+1
 1302 CONTINUE
      IST       = LOCAM(3)
      DO 1303 I = 1,NEF
      SSNU(I)   = A(IST)
      IST       = IST+1
 1303 CONTINUE
      GO TO 313
C
  312 IFISS=0
C
C-----TOTAL
C
  313 CONTINUE
      IST       = LOCAM(5)
      DO 1314 I = 1,NEF
      SSTR(I)   = A(IST)
      IST       = IST+1
 1314 CONTINUE
C-----ELASTIC SCATTERING
  314 CONTINUE
      IST       = LOCAM(7)
      DO 1315 I = 1,NEF
      SSE(I)    = A(IST)
      IST       = IST+1
 1315 CONTINUE
C
C     MATRIX DATA READ
C
  315 CONTINUE
      DO 400 MT = 1,MXMTX
      IF(MT.EQ.3)       GO TO 400
      IF(LTH(MT).LE.0)  GO TO 400
      IDWN      = LD(MT)+1
      IEXT      = LA(MT)
      IF(IDWN.LE.0.OR.IEXT.LE.0)  GO TO 400
      IST       = LOCAM(7+MT)
      IF(IST.LE.0)  GO TO 400
      CALL  CLEA(SMTX,LENG7,0.0)
      GO TO (320,330,400,350),MT
      GO TO 400
C-----N-N (INELASTIC)
  320 CONTINUE
CKSK  IDMT=8HN-N
      IDMT='N-N     '
      GO TO 360
C-----N2N
  330 CONTINUE
CKSK  IDMT=8HN2N
      IDMT='N2N     '
      GO TO 360
C-----ELASTIC P1
  350 CONTINUE
CKSK  IDMT=8HELP1
      IDMT='ELP1    '
C
  360 CONTINUE
      DO 1360 I = 1,IEXT
      DO 1360 J = 1,IDWN
      SMTX(J,I) = A(IST)
      IST       = IST+1
 1360 CONTINUE
C
      DO 390 I = 1,IEXT
      SUM      = 0.0
      DO 370 J = 1,IDWN
      SAVE     = SMTX(J,I)
      SUM      = SUM + SAVE
      IF(MT.EQ.4) GO TO 370
      SIGS(J,I)   = SIGS(J,I)   + DNTMP*SAVE
C     N2N MATRIX  : TSUCHI DEC 24/85
      IF(MT.EQ.2) THEN
      SIGN2N(J,I) = SIGN2N(J,I) + DNTMP*SAVE*0.5
                  ENDIF
      STR(J,I)    = STR(J,I)    + SAVE
  370 CONTINUE
      GO TO (381,382,400,384),MT
C-----INELASTIC
  381 CONTINUE
      SSIN(I)  = SUM
      GO TO 390
C-----N2N
  382 CONTINUE
      IFN2N    = MAX0(IFN2N,I)
      SS2N(I)  = SUM*0.5000000
      EFFMIC(I,3,MM) = SUM*0.50000000
C     N2N TOTAL   : TSUCHI DEC 24/85
      SIGN2T(I)= SIGN2T(I) + DNTMP*SS2N(I)
      GO TO 390
C-----UM (AVERAGE COSINE OF THE SCATTERING ANGLE)
  384 CONTINUE
      SAVE  = SSE(I)
      UM(I)  = 0.0
      IF(SAVE.NE.0.0) UM(I) = 0.333333*SUM/SAVE
  390 CONTINUE
  400 CONTINUE
C
      CALL  CLEA(SMTX,LENG7,0.0)
      IDWN   = 0
      IEXT   = 0
      IF(LTH(3).LE.0)            GO TO 410
      IDWN   = LD(3) + 1
      IEXT   = LA(3)
      IF(IDWN.LE.0.OR.IEXT.LE.0) GO TO 410
CKSK  IDMT   = 8HELP0
      IDMT   = 'ELP0    '
      IST    = LOCAM(10)
      IF(IST.LE.0)               GO TO 410
C
      DO 1410 I = 1,IEXT
      DO 1410 J = 1,IDWN
      SMTX(J,I) = A(IST)
      IST       = IST+1
 1410 CONTINUE
C
C     DELAYEDE NEUTRON DATA READ FROM FASTU
C
  410 CONTINUE
      IDELAY = 0
      IF (IFISS.GT.0) THEN
                      CALL CLEA( CHIMTX , LENG9 , 0.0 )
                      NFILE(1) = 'FAST'
                      NFILE(2) = 'U   '
                      NAMEP(1) (1:1) = 'Y'
                      NAMEP(2) = '0000'
                      CALL SEARCH(NAMEP(1),LENGY,ISW1)
                      IF (ISW1.EQ.0) THEN
                                     CALL READ(NAMEP(1),DD,LENGY)
                                     IDELAY   = 1
                                     NDELAY   = 1
                                     ENDIF
                      ISWMTX         = 0
                      IF (IRES(MM,NN).NE.1) THEN
                      NAMEP(1) (1:1) = 'X'
                      CALL SEARCH(NAMEP(1),LENGY,ISW1)
                      IF (ISW1.EQ.0) THEN
                                     CALL READ(NAMEP(1),CHIMTX,LENGY)
                                     ISWMTX   = 1
                                     ENDIF
                                            ENDIF
                      ENDIF
C     ***************************************
C     * CALCULATE EFFECTIVE X-SECTION       *
C     ***************************************
C-----CAPTURE
      IF(ICAPT.EQ.1) THEN
      DO  420 I = 1 , NEF
      SAVE      = SSC(I)*SFCTR(I,2)
      SSC(I)    = SAVE
      SIGC(I)   = SIGC(I) + SAVE*DNTMP
      EFFMIC(I,1,MM) = SAVE
  420 CONTINUE
                     ENDIF
C-----FISSION
      IF(IFISS.EQ.1) THEN
      DO  430 I = 1 , NEF
      SAVE      = SSF(I)*SFCTR(I,3)
      SSF(I)    = SAVE
      EFFMIC(I,2,MM) = SAVE
      EFFMIC(I,6,MM) = SAVE*SSNU(I)
      SAVE      = SAVE*DNTMP
      SIGF(I)   = SIGF(I)   + SAVE
      SIGFNU(I) = SIGFNU(I) + SAVE*SSNU(I)
CDEL  CHI(I)    = CHI(I)    + SCHI(I)*WHTCHI
  430 CONTINUE
                     JST  = 1
                     IF(ISWMTX.EQ.1) JST = NEF + 1
                     DO 1415   I = JST , IGMAX
                     DO 1415   J =   1 , NEF
                     CHIMTX(J,I) = SCHI(J)
 1415                CONTINUE
                     DO 1420   I = 1 , NEF
                     SSFNU(I,MM,NN) = SSF(I)*SSNU(I)
 1420                CONTINUE
                     CALL CLEA ( SCHI , NEF , 0.0 )
                     SUMCHI      = 0.0
                     DO 1430   I = 1 , NEF
                     DO 1430   J = 1 , IGMAX
                     SAVE     = SSFNU(J,MM,NN)*WTFLUX(J,NN)*CHIMTX(I,J)
                     SCHI(I)     = SCHI(I) + SAVE
                     SUMCHI      = SUMCHI  + SAVE
                     CHI (I)     = CHI (I) + SAVE*DNTMP
 1430                CONTINUE
                     WHTCHI      = 0.0
                     DO 1440   J = 1 , IGMAX
                     SAVE        = SSFNU(J,MM,NN)*WTFLUX(J,NN)
                     WHTCHI      = WHTCHI + SAVE*DNTMP
 1440                CONTINUE
                     FACTXI      = 0.0
                     IF(SUMCHI.GT.0.0) FACTXI = 1.0 / SUMCHI
                     DO 1450   I = 1, NEF
                     SCHI(I)     = SCHI(I)*FACTXI
 1450                CONTINUE
C--------------------CHECK WRITE
*                    WRITE(6,1451)
*    *               MTNAME(1,NN),MTNAME(2,NN),IDENT(1,MM,NN),
*    *               IDENT(2,MM,NN),NN,MM,ISWMTX,NEF,IGMAX,
*    *               DNTMP,WHTCHI,FACTXI
*                    WRITE(6,1452) (CHIMTX(I,1)    ,I=1,NEF)
*                    WRITE(6,1453) (CHIMTX(I,NEF)  ,I=1,NEF)
*                    WRITE(6,1454) (CHIMTX(I,IGMAX),I=1,NEF)
*                    WRITE(6,1455)  SCHI
*                    WRITE(6,1456) (WTFLUX(I,NN),I=1,IGMAX)
*                    WRITE(6,1457) (SSFNU(I,MM,NN),I=1,IGMAX)
                     ENDIF
C
 1451 FORMAT(1H ,' ## CHECK WRITE AT SUBR(MAFCAL) FOR CHI-VECTOR ## ',
     *      /1H ,'  > MTNAME IDENT NN MM      : ',2A4,2X,2A4,2X,2I6,
     *      /1H ,'  > ISWMTX NEF   IGMAX      : ',3I6,
     *      /1H ,'  > DENSITY WHTCHI FACTXI   : ',1P3E12.5)
 1452 FORMAT(1H ,' ## CHI(1G) ## ',1P10E11.4)
 1453 FORMAT(1H ,' ## CHI(NEF)## ',1P10E11.4)
 1454 FORMAT(1H ,' ## CHI(TH) ## ',1P10E11.4)
 1455 FORMAT(1H ,' ## SCHI    ## ',1P10E11.4)
 1456 FORMAT(1H ,' ## FLUX    ## ',1P10E11.4)
 1457 FORMAT(1H ,' ## SSFNU   ## ',1P10E11.4)
C
C     DELAYED NEUTRON DATA
C
      IF (IDELAY.EQ.1.AND.IFISS.EQ.1) THEN
C  MOD  2/8/93 WHTCHI KEEP BIRTH RATE OF DELAYED NEUTRON
      WHTCHI   = 0.0
      DO 422 I = 1,NEF
      IF(SSNU(I).GT.0.0) THEN
                         WHTCHI = WHTCHI +
     @          DD(14+I)*SSFNU(I,MM,NN)*DNTMP*WTFLUX(I,NN) / SSNU(I)
                         ENDIF
  422 CONTINUE
      IF(DD(2).GT.0.0)   THEN
      DO 423 I = NEF+1,IGMAX
                         WHTCHI = WHTCHI +
     @          DD(NEF+15)*SSFNU(I,MM,NN)*DNTMP*WTFLUX(I,NN) / DD(2)
  423 CONTINUE
                         ENDIF
      DO 424 J=1,6
C MOD XDBAR(J,I) = XDBAR(J,I) +  DD(8+J)*DD(14+I)*WHTCHI
      XDBAR(J)   = XDBAR(J  ) +  DD(8+J)*WHTCHI
  424 CONTINUE
      DO 425   I = 1,NEF
      SAVE       = SSF(I)*DNTMP
      DO 425   J = 1,6
      BVSIGF(J,I)=BVSIGF(J,I)+DD(8+J)*DD(14+I)*SAVE
CITJ MODIFY START 93/12/20
      BLSIGF(J,I)=BLSIGF(J,I)+DD(8+J)*DD(14+I)*SAVE/DD(2+J)
CITJ MODIFY END
CMOD  XD(J,I)    =XD(J,I)    +DD(NEF*J+I+15)*DD(8+J)*DD(14+I)*WHTCHI
      XD(J,I)    =XD(J,I)    +DD(NEF*J+I+15)*DD(8+J)*WHTCHI
  425 CONTINUE
C --- DEUTERIUM GAMMA,N DELAYED NEUTRON
      IF(NFAMLY.EQ.15) THEN
                       DO 426   J=7,15
CMOD                   XDBAR(J,I)  = XDBAR(J,I)+DD1(5+J)*DD(14+I)*WHTCHI
                       XDBAR(J  )  = XDBAR(J  )+DD1(5+J)*WHTCHI
  426                  CONTINUE
                       DO 427 I = 1,NEF
                       SAVE     = SSF(I)*DNTMP
                       DO 427 J = 7,15
                       BVSIGF(J,I) = BVSIGF(J,I)+DD1(5+J)*DD(14+I)*SAVE
CITJ MODIFY START 93/12/20
                       BLSIGF(J,I) = BLSIGF(J,I)+DD1(5+J)*DD(14+I)*SAVE
     >                               / DD1(J-4)
CITJ MODIFY END
CMOD                   XD(J,I)= XD(J,I)+SCHI(I)*DD1(5+J)*DD(14+I)*WHTCHI
                       XD(J,I)= XD(J,I)+SCHI(I)*DD1(5+J)*WHTCHI
  427                  CONTINUE
                      ENDIF
                                      ENDIF
C-----ELASTIC SCATTERING
      DO 500     I = 1 , NEF
      FACT         = SFCTR(I,4)
      FMTX(3,I,MM) = FACT
      SAVE         = FACT*SSE(I)
      SSE(I)       = SAVE
      EFFMIC(I,4,MM) = SAVE
      SAVEGG       = 0.0
      IF(I.GT.IEXT)  GO TO 440
      IF(IDWN.LE.0)  GO TO 440
      IF(IFER.EQ.1)  FACT=SFCTR(I,5)
      SUM          = 0.0
      FMTX(2,I,MM) = FACT
C
      DO 435    J = 1,IDWN
      IF(J.EQ.1) GO TO 435
      SAVE        = SMTX(J,I)*FACT
      SUM         = SUM       + SAVE
      SMTX(J,I)   = SAVE
      STR(J,I)    = STR(J,I)  + SAVE
      SIGS(J,I)   = SIGS(J,I) + SAVE*DNTMP
  435 CONTINUE
      SAVEGG      = SMTX(1,I)
      SMTX(1,I)   = SSE(I)-SUM
      EFFMIC(I,5,MM) = SUM
  440 CONTINUE
C-----DIFFUSION COEFFICIENT
      SST(I)      = SSC(I) + SSF(I) + SSE(I) + SSIN(I) + SS2N(I)
      IF(IOPT(15).EQ.1) THEN
                        SAVE        = SSTR(I)*SFCTR(I,1)
                        DEL         = SAVE-SST(I)
                        SST(I)      = SAVE
                        SMTX(1,I)   = SMTX(1,I) + DEL
CADD 8/30/1990 BY JAIS K.KANEKO
                        SSE(I)      = SSE(I)    + DEL
                        ENDIF
C
      DEL         = UM(I)*SSE(I)
      SSTR(I)     = SST(I)-DEL
      D1(I)       = D1(I)     + DNTMP*SSTR(I)
      STR(1,I)    = STR(1,I)  + SMTX(1,I)
      SIGS(1,I)   = SIGS(1,I) + DNTMP*SMTX(1,I)
      SIGT(I)     = SIGT(I)   + SST(I)*DNTMP
      SIGA(I)     = SIGA(I)   + (SSC(I)+SSF(I))*DNTMP
      IF(SAVEGG.GT.0.0)  FMTX(1,I,MM) = SMTX(1,I) / SAVEGG
  500 CONTINUE
C
C  >> PRINT-OUT EFFECTIVE MICOROSCOPIC CROSS SECTION
C
C@MOD IF(IFS.EQ.0)    GO TO 600
      IMASK  = 0
      IF(IFS.GT.0.AND.DNTMP.GT.0.0) IMASK = 1
      RRTEMP = RTEMP
      IF(IMASK.EQ.0)   RRTEMP = 300.00
C
      IF(LXMICR(MM,NN).EQ.1.OR.LXMICR(MM,NN).EQ.3)
     +CALL MAFPRT(MTNAME,LTH   ,LA    ,LD    ,SSC   ,SSF   ,SSNU  ,
     +            SCHI  ,SSTR  ,SSE   ,SST   ,SSIN  ,SS2N  ,UM    ,
     +            SMTX  ,STR   ,SIG0  ,SFCTR ,IDENT(1,MM,NN)      ,
     +            NN    ,MM    ,IOPT19,IMASK ,FMTX(1,1,MM) ,RRTEMP )
  600 CONTINUE
C
      IFLSW    = 1
      NFILE(1) = 'MICR'
      NFILE(2) = 'EF  '
      NAMEP(1) = MTNAME(1,NN)
      NAMEP(2) = 'BMIC'
      LENG     = MAXNG*MAXMT3*MMK
      CALL OVRWRT( NAMEP , EFFMIC , LENG )
C
      DO 650 I = 1,NEF
      SAVE     = D1(I)
      D1(I)    = 0.0
      IF(SAVE.NE.0.0) D1(I) = 0.33333333/SAVE
      D2(I)    = D1(I)
  650 CONTINUE
C
C  >> PRINT-OUT AND DISK-OUT EFFECTIVE MACROSCOPIC CROSS SECTION
C
      ICHRA8='   4'
      CALL  MAFOUT(MTNAME(1,NN) ,
     +             LSS   ,LGV   ,SIGC  ,SIGF  ,SIGFNU,SIGT  ,
     +             CHI   ,D1    ,D2    ,SIGA  ,SIGS  ,
     +             ENBND ,0     ,ICHRA8,IOPT19)
C
C
C  >> PRINT-OUT AND DISK-OUT N2N MACROSCOPIC CROSS SECTION
C
      IF(IFN2N.NE.0) THEN
                     ICHRA8 = '   M'
               CALL  MAFOUT(MTNAME(1,NN) ,
     +                      LSS   ,LGV   ,ZERO  ,ZERO  ,ZERO  ,SIGN2T,
     +                      ZERO  ,ZERO  ,ZERO  ,ZERO  ,SIGN2N,
     +                      ENBND ,0     ,ICHRA8,IOPT19)
                     ENDIF
CADD  10/14/1988
CM    IF(NN.EQ.1) THEN
CM                DO 655 II= 1 , 3
CM                IF(II.EQ.1) WRITE(6,*) '1  **** FMTX(1,I,MM) ****'
CM                IF(II.EQ.2) WRITE(6,*) '1  **** FMTX(2,I,MM) ****'
CM                IF(II.EQ.3) WRITE(6,*) '1  **** FMTX(3,I,MM) ****'
CM                DO 655 I = 1 , NEF
CM                WRITE(6,656) I,(FMTX(II,I,MM),MM=1,MMK)
CM655             CONTINUE
CM                ENDIF
CM656 FORMAT(1H ,' GROUP = ',I2,3X,1P10E11.4,(/1H ,14X,1P10E11.4))
CM
C === DELAYED NEUTRON DATA DISK-OUT
C
      IF (NDELAY.EQ.0) GO TO 680
      NAMEP(1)=MTNAME(1,NN)
      NAMEP(2)=MTNAME(2,NN)
      NAMEP(2) (1:1) = 'F'
      NAMEP(2) (4:4) = 'Y'
      LOC      = 0
C
      CALL CLEA ( DD  , NFAMLY*NEF*3 , 0.0 )
      DO 670 I = 1,NEF
      DO 670 J = 1,NFAMLY
      LOC      = LOC +1
      DD(LOC)  = BVSIGF(J,I)
      BAR      =  1.0
CMOD  IF (XDBAR(J,I).NE.0.0) BAR = 1.0/XDBAR(J,I)
      IF (XDBAR(J)  .NE.0.0) BAR = 1.0/XDBAR(J)
      DD(LOC+NFAMLY*NEF)   = XD(J,I) * BAR
      DD(LOC+NFAMLY*NEF*2) = BLSIGF(J,I)
  670 CONTINUE
CMOD  CALL WRITE(NAMEP(1),DD,NFAMLY*NEF*2)
      CALL WRITE(NAMEP(1),DD,NFAMLY*NEF*3)
  680 CONTINUE
C     ****************************************
C     * P1 MATRIX DATA PROCESS START         *
C     ****************************************
  700 CONTINUE
C---- INITIAL SET
      CALL  CLEA(SIGC,LENG2,0.0)
      CALL ICLEA(LSS ,LENG1,0  )
C---- NUCLIDE LOOP
      DO 900 MM = 1,MMK
      DNTMP     = DN(MM,NN)
      IF(DNTMP.LE.0.0)      GO TO 900
      IF(IRES(MM,NN).EQ.-1) GO TO 900
C
      NAMEP(1)  = IDENT(1,MM,NN)
      NAMEP(2)  = IDENT(2,MM,NN)
      IF(IRES(MM,NN).NE.1) THEN
                           NFILE(1)='FAST'
                           NFILE(2)='U   '
                           NAMEP(2)(4:4) = '0'
                              ELSE
                              NAMEP(1)(1:1) = 'C'
                              NAMEP(2)(1:1) = 'F'
                              NFILE(1)='MICR'
                              NFILE(2)='EF  '
                               KANS     = 0
                               CALL  SEARCH(NAMEP(1),LENG,KANS)
                               IF(KANS.EQ.1)  THEN
                                              NAMEP(2)(4:4) = '0'
                                              JANS = 0
                                    CALL  SEARCH(NAMEP(1),LENG,JANS)
                                    IF(JANS.EQ.1) THEN
                                                  NFILE(1) = 'FAST'
                                                  NFILE(2) = 'U   '
                                                  NAMEP(2) = '0000'
                                                  ENDIF
                                              ENDIF
                           ENDIF
C
      CALL  MAFCON(LTH   ,LA    ,LD    ,FTEMP ,FSIG0)
C
      IF(LTOT.LE.0)     GO TO 900
      IST    = LOCAM(11)
      IF(IST.LE.0)      GO TO 900
      IF(LTH(4).LE.0)   GO TO 900
      IDWN   = LD(4)+1
      IEXT   = LA(4)
      IF(IDWN.LE.0.OR.IEXT.LE.0)  GO TO 900
      NAMEP(1)(1:1) = 'M'
      CALL  CLEA(A,17000,0.0)
      CALL  READ(NAMEP(1),A,LTOT)
C
                DO 850 I = 1,IEXT
                FACT     = FMTX(1,I,MM)
                SIGS(1,I)= SIGS(1,I) + DNTMP*A(IST)*FACT
                SUM      = DNTMP*A(IST)*FACT
                IST      = IST + 1
                FACT     = FMTX(2,I,MM)
C
                DO 800 J = 2,IDWN
                SIGS(J,I)= SIGS(J,I)+DNTMP*A(IST)*FACT
                SUM      = SUM + DNTMP*A(IST)*FACT
                IST      = IST + 1
  800           CONTINUE
                SIGT(I)  = SIGT(I)+SUM
  850           CONTINUE
  900 CONTINUE
C---- PRINT-OUT AND DISK-OUT EFFECTIVE MACROSCOPIC CROSS SECTION
      ICHRA8='   3'
      CALL  MAFOUT(MTNAME(1,NN) ,
     +             LSS   ,LGV   ,SIGC  ,SIGF  ,SIGFNU,SIGT  ,
     +             CHI   ,D1    ,D2    ,SIGA  ,SIGS  ,
     +             ENBND ,1     ,ICHRA8,IOPT19)
C    *******************************************
C    *   P2 TO P5  MATRIX DATA PROCESS START   *
C    *                   -- ADDED 8/1/1983 --- *
C    *******************************************
      IF(ISNCAL.EQ.0)  GO TO 2000
      IF(IPLMAX.LE.1)  GO TO 2000
      MAXIPL = IPLMAX
      DO 1990 IPL = 2 , MAXIPL
C-----INITIAL SET
      CALL  CLEA(SIGC,LENG2,0.0)
      CALL ICLEA(LSS ,LENG1,0  )
      IPPASS = 0
C-----NUCLIDE LOOP
      DO 1900 MM = 1,MMK
      DNTMP      = DN(MM,NN)
      IF(DNTMP.LE.0.0)      GO TO 1900
      IF(IRES(MM,NN).EQ.-1) GO TO 1900
C
      NAMEP(1)   = IDENT(1,MM,NN)
      NAMEP(2)   = IDENT(2,MM,NN)
C
      IF(IRES(MM,NN).NE.1) THEN
                           NFILE(1)='FAST'
                           NFILE(2)='U   '
                           NAMEP(2)(4:4) = '0'
                              ELSE
                              NAMEP(1)(1:1) = 'C'
                              NAMEP(2)(1:1) = 'F'
                              NFILE(1)='MICR'
                              NFILE(2)='EF  '
                               KANS     = 0
                               CALL  SEARCH(NAMEP(1),LENG,KANS)
                               IF(KANS.EQ.1)  THEN
                                              NAMEP(2)(4:4) = '0'
                                              JANS = 0
                                    CALL  SEARCH(NAMEP(1),LENG,JANS)
                                    IF(JANS.EQ.1) THEN
                                                  NFILE(1) = 'FAST'
                                                  NFILE(2) = 'U   '
                                                  NAMEP(2) = '0000'
                                                  ENDIF
                                              ENDIF
                           ENDIF
C
      CALL  MAFCON(LTH   ,LA    ,LD    ,FTEMP ,FSIG0)
C
      IF(LTH(3).LE.0)             GO TO 1900
      IDWN   = LD(3)+1
      IEXT   = LA(3)
      IF(IDWN.LE.0.OR.IEXT.LE.0)  GO TO 1900
      NAMEP(1)(1:1) = IDIPL(IPL)
      CALL  SEARCH(NAMEP(1),LENG,ISWDAT)
      IF(ISWDAT.EQ.1)             GO TO 1900
      IPPASS = 1
      CALL  CLEA(A,17000,0.0)
      CALL  READ(NAMEP(1),A,LENG)
C
                IST      = 1
                DO 1850 I= 1,IEXT
                FACT     = FMTX(1,I,MM)
                SIGS(1,I)= SIGS(1,I) + DNTMP*A(IST)*FACT
                SUM      = DNTMP*A(IST)*FACT
                IST      = IST + 1
                FACT     = FMTX(2,I,MM)
C
                DO 1800 J= 2,IDWN
                SIGS(J,I)= SIGS(J,I)+DNTMP*A(IST)*FACT
                SUM      = SUM + DNTMP*A(IST)*FACT
                IST      = IST + 1
 1800           CONTINUE
                SIGT(I)  = SIGT(I)+SUM
 1850           CONTINUE
 1900 CONTINUE
C
      IF(IPPASS.EQ.0)  WRITE(NOUT1,2102) IPL,MTNAME(1,NN),MTNAME(2,NN)
C-----PRINT-OUT AND DISK-OUT P2 TO P5  MACROSCOPIC CROSS SECTION
      ICHRA8 = IDMPL(IPL)
      CALL  MAFOUT(MTNAME(1,NN) ,
     +             LSS   ,LGV   ,SIGC   ,SIGF  ,SIGFNU,SIGT  ,
     +             CHI   ,D1    ,D2     ,SIGA  ,SIGS  ,
     +             ENBND ,IPL   ,ICHRA8,IOPT19)
 1990 CONTINUE
 2000 CONTINUE
C
C     MFACAL END
C
      REWIND 3
      RETURN
C
 1200 FORMAT(' *** NON-POSITIVE TEMPERATURE ENCOUNTERED *** FOR ',
     * I2,'-TH MIXTURE ***'
     * ,' IT WILL CAUSE ALOG ERROR IN THE F-TAB INTERPOLATION STEP ')
C
 2001 FORMAT(1H ,' ##SSC ## ',1P10E11.4)
 2002 FORMAT(1H ,' ##FISS## ',1P10E11.4)
 2003 FORMAT(1H ,' ##KAI ## ',1P10E11.4)
 2004 FORMAT(1H ,' ##NU  ## ',1P10E11.4)
 2005 FORMAT(1H ,' ##TOTL## ',1P10E11.4)
 2006 FORMAT(1H ,' ##ELS ## ',1P10E11.4)
 2012 FORMAT(1H ,' ##SSIN## ',1P10E11.4)
 2013 FORMAT(1H ,' ##SS2N## ',1P10E11.4)
 2014 FORMAT(1H ,' ##UM  ## ',1P10E11.4)
C
 2102 FORMAT(1H ,'     *** CAUTION *****  P',I1,' COMPONENT OF ',2A4,
     *' IS ALL ZERO.(MACROF)')
C
C
      END
