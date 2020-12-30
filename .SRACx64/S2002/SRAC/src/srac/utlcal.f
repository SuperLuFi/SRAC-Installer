C
C**** SUBPROGRAM TO COMPOSE USER'S THERMAL LIBRARY ****************
C
      SUBROUTINE UTLCAL(NEGT,NGT,NGT1,NGT4,NET1,NET5,NTMAX,MXSIG0,
     1                  WF     ,WR     ,FLUX1  ,FLUX2  ,WFNUC  ,
     2                  WRNUC  ,FDATA  ,BDATA  ,FTABF  ,FTABB  ,
     3                  SIGINF ,SIGINR ,XNU    ,XNUB   ,AR     ,
     4                  AF     ,NREG   ,WORK   ,FDATA0 ,BDATA0 ,
     5                  WR1    ,WR2    ,LENWRK )
C
      INCLUDE  'MATDTINC'
C
      INTEGER         FILESW
      CHARACTER *4    FILENM,CASENM,NUCLID,IDTEMP
      CHARACTER *8    MNNAME,NTNAME,IDENT,MEMBER
C
      COMMON /MAINC/  IOPT(36),JNMACR(2),FNMACR(2),DUM(8),JNFLUX(2),
     *                FNFLUX(2),NEFL,NETL,NEF,NET,NERF,NERT,NMAT,NETL1,
     *                BSQ ,NIN1,NIN2,NOUT1,NOUT2,IT0,
     *                DUM3(6),DUM4(28),CASENM(2),DUM5(898)
C
      COMMON /PDSPDS/ BUFFER(540),FILESW,FILENM(3),ECODE,TEMP
C
      COMMON /USERIX/ NUCLID(2,MXLISO),NUCLNO
C
      COMMON /TMPSET/ STND(35),IDTEMP(61),NTDUMY
C
      DIMENSION       NEGT(NET)
      DIMENSION       II(50)
      DIMENSION       AI(50)
      DIMENSION       ICONT(200)
      DIMENSION       AA(200)
C
      DIMENSION       WF    (NGT,NTMAX) , WR    (NET,NTMAX)
      DIMENSION       WR1   (NET,NTMAX) , WR2   (NET,NTMAX)
      DIMENSION       FLUX1 (NGT,NTMAX) , FLUX2 (NGT,NTMAX)
      DIMENSION       WFNUC (NGT,NTMAX) , WRNUC (NET,NTMAX)
      DIMENSION       XNU   (NGT,NTMAX) , XNUB  (NET,NTMAX)
      DIMENSION       FDATA (NGT,NGT4,NTMAX)
      DIMENSION       BDATA (NET,NET5,NTMAX)
      DIMENSION       FDATA0(NGT,   4,NTMAX)
      DIMENSION       BDATA0(NET,   5,NTMAX)
      DIMENSION       FTABF (NGT,MXSIG0,NTMAX)
      DIMENSION       FTABB (NET,MXSIG0,NTMAX)
      DIMENSION       SIGINF(NGT,NTMAX ,    2)
      DIMENSION       SIGINR(NET,     2)
      DIMENSION       AR    (NET1)      , AF(NGT1)
      DIMENSION       NREG  (NGT1)      , WORK(LENWRK)
C
CKK9  INTEGER*2       NP1(7)
      INTEGER*4       NP1(7)
      CHARACTER*1     LPN(7)
C
      EQUIVALENCE    (II(1),IFFS  ),(II(2),IPN  )
      EQUIVALENCE    (II(3),IFT   ),(II(4),NGMIN)
      EQUIVALENCE    (II(5),NGMAX ),(II(6),NSIG ),(II(7),NTEMP)
      EQUIVALENCE    (II(8),IWTYPE),(II(30),LENMEM)
C
      EQUIVALENCE    (II(1),AI(1))
      EQUIVALENCE    (AA(1),ICONT(1))
C
CKK9  DATA LPN          /1HM,1HK,1HP,1HQ,1HS,1HT,1HU/
      DATA LPN          /'M','K','P','Q','S','T','U'/
C
C-----INITIAL SET
C
C
      FILESW = 1
      J      = 0
      CALL   ICLEA ( NREG , NGT1 , 0 )
C
      IF(NETL1.GT.1) THEN
                     DO 10 K = 1,NETL1-1
                     NREG(K) = 0
                     J       = K
   10                CONTINUE
                     ENDIF
C
      DO 30 N= 1,NET
         DO 20 I= 1,NEGT(N)
         J      = J+1
   20    NREG(J)= N
   30 CONTINUE
C
CWRITE
*     WRITE(6,7022) NETL,NETL1,NET
*     WRITE(6,7023) (NEGT(N),N=1,NET)
*     WRITE(6,7024) (NREG(N),N=1,NETL)
*7022 FORMAT(1H ,' ## NETL NETL1 NET ## ',3I6)
*7023 FORMAT(1H ,' ## NEGT(N) ## ',10I6)
*7024 FORMAT(1H ,' ## NREG(N) ## ',10I6)
C
C --- COLLAPSE STARTS
C
C     FIND 'THERMALX'
C
      NTMAX0    = 0
      CALL CLEA( WF     , NGT*NTMAX , 0.0 )
      CALL CLEA( WR     , NET*NTMAX , 0.0 )
      CALL CLEA( FLUX1  , NGT*NTMAX , 0.0 )
      CALL CLEA( FLUX2  , NGT*NTMAX , 0.0 )
      CALL CLEA( WR1    , NET*NTMAX , 0.0 )
      CALL CLEA( WR2    , NET*NTMAX , 0.0 )
C
      FILENM(1) =  'THER'
      FILENM(2) =  'MALP'
CDEL  FILENM(3) =  '    '
C *** READ 'WTFLUX01' MEMBER
      MEMBER    = 'WTFLUX01'
      ISW       = 0
      NTEMP1    = 0
      CALL SEARCH(MEMBER,LGT,ISW)
      IF(ISW.EQ.0) THEN
                   CALL READ( MEMBER , FLUX1 , LGT )
                   NTEMP1  = LGT/NGT
                   DO 40 NT = 1 , NTEMP1
       CALL UTLCND(NETL1,NETL,NET,NREG,0,FLUX1(1,NT),WR1(1,NT),AF,AF)
   40              CONTINUE
                   ENDIF
C *** READ 'WTFLUX02' MEMBER
      MEMBER    = 'WTFLUX02'
      ISW       = 0
      NTEMP2    = 0
      CALL SEARCH(MEMBER,LGT,ISW)
      IF(ISW.EQ.0) THEN
                   CALL READ( MEMBER , FLUX2 , LGT )
                   NTEMP2  = LGT/NGT
                   DO 50 NT = 1 , NTEMP2
       CALL UTLCND(NETL1,NETL,NET,NREG,0,FLUX2(1,NT),WR2(1,NT),AF,AF)
   50              CONTINUE
                   ENDIF
C
*     WRITE(6,*) ' ***  NGT NETL NETL1 NTEMP1 NTEMP2 *** '
*     WRITE(6,*)        NGT,NETL,NETL1,NTEMP1,NTEMP2
C
      NTNAME    = 'THERMALX'
      DO 100 NT = 1 , NTMAX
      NTNAME(8:8) = IDTEMP (NT) (4:4)
C
      FILENM(1) =  'THER'
      FILENM(2) =  'MALP'
CDEL  FILENM(3) =  '    '
C
      CALL SEARCH(NTNAME,LGT,ISW)
      IF(ISW.EQ.1.AND.NT.EQ.1) THEN
                               WRITE(6,6010) NTNAME
                               STOP  999
                               ENDIF
C
      IF(ISW.EQ.1.AND.NT.GT.1) GO TO  101
C-----READ WEIGHT AND ENERGY BOUNDARIES
      NTMAX0   = NT
      CALL READ(NTNAME,AA,LGT)
C
      DO 60 I  = 1,NETL
      WF(I,NT) = AA(I+1)
   60 CONTINUE
C
      JJ       = 0
      DO 70 I  = NETL+1 , LGT-1
      JJ       = JJ+1
      AF(JJ)   = AA(I+1)
   70 CONTINUE
      AR(1)    = AF(NETL1)
      NJ       = 0
      DO 80 N  = 1,NET
      NJ       = NJ + NEGT(N)
      AR(N+1)  = AF(NJ+NETL1)
   80 CONTINUE
C     WEIGHT
      CALL UTLCND(NETL1,NETL,NET,NREG,0,WF(1,NT),WR(1,NT),AF,AF)
C ---'THERMAL' SEARCH IN USER-THERMAL-LIB
      FILENM(1) =  'THER'
      FILENM(2) =  'MALU'
CDEL  FILENM(3) =  '    '
      CALL SEARCH(NTNAME,LENG,ISW)
      IF(ISW.EQ.0)  GO TO 100
      IF(NT.EQ.1) CALL ENTAPR ('FINE    ','THERMAL ',NET,AR)
C
      NETA    = NET*2 + 2
      JJ      = 0
      DO 95 I = 1,NETA-1
      IF(I.GT.NET) GO TO 90
      AA(I+1) = WR(I,NT)
      GO TO 95
   90 CONTINUE
      JJ      = JJ + 1
      AA(I+1) = AR(JJ)
   95 CONTINUE
      ICONT(1)= NET
      CALL WRITE(NTNAME,AA,NETA)
  100 CONTINUE
C
C-----SET COUNTER OF NUCLIDE ZERO
C
  101 ICNT     = 0
      LENGF    = NGT*NGT4*NTMAX
      LENGB    = NET*NET5*NTMAX
CWRITE
*     WRITE(6,7021) NTMAX0
C
*     CALL UTLWOT(WF,NTMAX0,NGT ,1,'TEMP','GRP.','    ',
*    +            ' FINE WEIGHTING    '                    )
*     CALL UTLWOT(WR,NTMAX0,NET ,1,'TEMP','GRP.','    ',
*    +            ' BROAD WEIGHTING   '                    )
C
*7021 FORMAT(1H ,' ## NTMAX0 ## ',I6)
C
C-----LOOP OF NUCLIDE
C
  120 ICNT=ICNT+1
      IF(ICNT.GT.NUCLNO)  GO TO 1001
C
C     IDENT  (8H) CURRENT NUCLIDE NAME
C     MTNAME (8H) CURRENT NUCLIDE NAME WITH '0' AT THE LAST CHAR
C     NTNAME (8H) CURRENT MEMBER NAME 'THERMALX'
C
      MNNAME  =NUCLID(1,ICNT) // NUCLID(2,ICNT)
      MNNAME(1:1) = 'C'
      MNNAME(6:8) = '000'
C ---- CHECK C-MEMBER IN THERMALU FILE
      FILENM(1)= 'THER'
      FILENM(2)= 'MALU'
CDEL  FILENM(3)= '    '
C
      LGTCON = 0
      ISW    = 0
      CALL SEARCH(MNNAME,LGTCON,ISW)
      IF(ISW.EQ.0) GO TO 120
C ---- CHECK C-MEMBER IN THERMALP FILE
      FILENM(1)= 'THER'
      FILENM(2)= 'MALP'
CDEL  FILENM(3)= '    '
C
      LGTCON = 0
      CALL SEARCH(MNNAME,LGTCON,ISW)
C
      IF(ISW.EQ.1) THEN
                   WRITE(NOUT1,6050) MNNAME
                   GO TO 120
                   ENDIF
C-----READ CONT MEMBER FORM THERMALP
      CALL READ(MNNAME,II,LGTCON)
C-----CHECK LIBRARY TYPE
      IF(LENMEM.LE.0) THEN
        WRITE(NOUT1,*) ' ** ERROR STOP AT SUBR(UTLCAL) ]]            '
        WRITE(NOUT2,*) ' ** ERROR STOP AT SUBR(UTLCAL) ]]            '
        WRITE(NOUT1,*) ' ** THE TYPE OF PUBLIC THERMAL LIBRARY IS OLD ]'
        WRITE(NOUT2,*) ' ** THE TYPE OF PUBLIC THERMAL LIBRARY IS OLD ]'
        STOP 999
        ENDIF
C
      MEMBER  = MNNAME
      MEMBER(1:1) = 'W'
C-----CHECK NSIG VALUE
      IF(NSIG.GT.MXSIG0) THEN
          WRITE(NOUT1,*) ' ** FATAL PROGRAMMING ERROR AT SUB(UTLCAL) ]]'
          WRITE(NOUT2,*) ' ** FATAL PROGRAMMING ERROR AT SUB(UTLCAL) ]]'
          WRITE(NOUT1,*) ' ** NSIG IS ',NSIG,' BUT RESERVED IS ',MXSIG0
          WRITE(NOUT2,*) ' ** NSIG IS ',NSIG,' BUT RESERVED IS ',MXSIG0
          STOP 999
          ENDIF
CWRITE
*     WRITE(6,7001) MNNAME
*     WRITE(6,7002) (II(I),I=1,LGTCON)
*     WRITE(6,7003) (AI(I),I=1,LGTCON)
*7001 FORMAT(1H ,' ## NUCLID OF THERMALP ## ',A8)
*7002 FORMAT(1H ,' ## II (CONT) ## ',10I11)
*7003 FORMAT(1H ,' ## AI (CONT) ## ',1P10E11.4)
C-----SET NGMINR,NGMAXR
      NGMINR = NREG(NGMIN)
      NGMAXR = NREG(NGMAX)
      IF(NGMINR.LE.0)    NGMINR = 1
      IF(NGMAX.LT.NETL1) THEN
                         NGMINR=0
                         NGMAXR=0
                         IFT   =0
                         NSIG  =0
                         ENDIF
C-----SET NP1
      IF(LENMEM.LT.NGT*NGT)  IPN = 0
      IKK      = IPN
      DO 125 JJ= 7 , 2 , -1
      NDIV     = 2**(JJ-1)
      NP1(JJ)  = IKK/NDIV
      IF(NP1(JJ).EQ.1)  IKK = IKK - NDIV
  125 CONTINUE
      NP1(1)   = IKK
C
      IF(IPN.EQ.0) THEN
                   NP1(2) = 1
                   ENDIF
C
      CALL CLEA( WFNUC  ,   NGT*NTMAX    , 0.0   )
      CALL CLEA( WRNUC  ,   NET*NTMAX    , 0.0   )
      CALL CLEA( SIGINF ,   NGT*NTMAX*2  , 0.0   )
      CALL CLEA( SIGINR ,   NET*2        , 0.0   )
C
C-----SET WFNUC & WRNUC
C
      IF(IWTYPE.EQ.0) THEN
                      DO 720 NT = 1 , NTEMP
                      DO 710 I  = 1 , NETL
                      WFNUC(I,NT) = WF(I,NT)
  710                 CONTINUE
                      DO 715 I  = 1 , NET
                      WRNUC(I,NT) = WR(I,NT)
  715                 CONTINUE
  720                 CONTINUE
                      ENDIF
C
      IF(IWTYPE.EQ.1) THEN
                      DO 740 NT = 1 , NTEMP
                      DO 730 I  = 1 , NETL
                      WFNUC(I,NT) = FLUX1(I,NT)
  730                 CONTINUE
                      DO 735 I  = 1 , NET
                      WRNUC(I,NT) = WR1(I,NT)
  735                 CONTINUE
  740                 CONTINUE
                      ENDIF
C
      IF(IWTYPE.EQ.2) THEN
                      DO 760 NT = 1 , NTEMP
                      DO 750 I  = 1 , NETL
                      WFNUC(I,NT) = FLUX2(I,NT)
  750                 CONTINUE
                      DO 755 I  = 1 , NET
                      WRNUC(I,NT) = WR2(I,NT)
  755                 CONTINUE
  760                 CONTINUE
                      ENDIF
C
      IF(IWTYPE.EQ.3) THEN
                LENGW        = NTEMP * NGT
                CALL  READ(MEMBER,WFNUC,LENGW)
C
*               CALL  UTLWOT(WFNUC,NTEMP,NETL,1,'TEMP','GRP.','    ',
*    +                       ' WEIGHTING FLUX  ) '    )
C
                   DO 770 NT = 1 , NTEMP
      CALL UTLCND(NETL1,NETL,NET,NREG,0,WFNUC(1,NT),WRNUC(1,NT),AF,AF)
  770              CONTINUE
                   ENDIF
C
*     CALL UTLWOT(WFNUC,NTEMP ,NGT ,1,'TEMP','GRP.','    ',
*    +            ' FINE WEIGHTING    '                    )
*     CALL UTLWOT(WRNUC,NTEMP ,NET ,1,'TEMP','GRP.','    ',
*    +            ' BROAD WEIGHTING   '                    )
C
C-----LOOP OF MATRIX TYPE
C
      DO 320 LL=1,7
      IF(NP1(LL).EQ.0) GO TO 320
      MNNAME (1:1) = LPN(LL)
      MNNAME (6:8) = '000'
C
      CALL  CLEA(FDATA, LENGF  , 0.0  )
      CALL  CLEA(BDATA, LENGB  , 0.0  )
C
      FILENM(1)= 'THER'
      FILENM(2)= 'MALP'
CDEL  FILENM(3)= '    '
C
      CALL SEARCH(MNNAME,LTH,ISW)
      IF(ISW.NE.0)  THEN
                    WRITE(6,6010) MNNAME
                    NP1(LL) = 0
                    GO TO 320
                    ENDIF
C
C
C
      CALL  READ( MNNAME , FDATA , LTH )
C
CDEL  IF(LL.EQ.2.AND.LENMEM.EQ.NGT*4) IPN = 0
C
      IF(LL.LE.2) THEN
            IF(IPN.GT.0) THEN
                  DO 130 NT = 1 , NTEMP
                  DO 130  I = 1 , NETL
                  SIGINF(I,NT,1) = FDATA(I,NGT+2,NT)
                  SIGINF(I,NT,2) = FDATA(I,NGT4 ,NT)
  130             CONTINUE
C
                  ELSE
                  DO 135 NT = 1 , NTEMP
                  DO 135  I = 1 , NETL
                  SIGINF(I,NT,1) = FDATA0(I,2,NT)
                  SIGINF(I,NT,2) = FDATA0(I,4,NT)
  135             CONTINUE
                  ENDIF
            ENDIF
C
C     IR=1 UPSCAT IR=2 CAPT IR=3 TOT IR=4 FISS
C
      IF(LENMEM.LT.NGT*NGT) THEN
      DO 140 NT=1,NTEMP
      DO 140 IR=1,4
      CALL UTLCND(NETL1,NETL,NET,NREG,1,WFNUC(1,NT),WRNUC(1,NT),
     +            FDATA0(1,IR,NT),BDATA0(1,IR,NT))
  140 CONTINUE
C
                  ELSE
      DO 150 NT=1,NTEMP
      DO 150 IR=1,4
      CALL UTLCND(NETL1,NETL,NET,NREG,1,WFNUC(1,NT),WRNUC(1,NT),
     +            FDATA(1,NGT+IR,NT),BDATA(1,NET+IR,NT))
  150 CONTINUE
                 ENDIF
C
C------COLLAPSE NU-VALUE USING SIGF*PHI WEIGHTING
C
      IF(LL.LE.2.AND.IFFS.EQ.1) THEN
                  IDENT = MNNAME
                  IDENT (1:1) = 'Z'
                  CALL SEARCH(IDENT,LNU,ISW)
                  IF(ISW.EQ.0) THEN
                               CALL READ(IDENT,XNU,LNU)
C
                               ELSE
                               DO 152 N = 1 , NTEMP
                               DO 152 I = 1 , NETL
                               XNU(I,N) = AI(29)
  152                          CONTINUE
                               ENDIF
C
                  DO  156  NT = 1 , NTEMP
                  CALL UTLCND(NETL1,NETL,NET,NREG,2,XNU(1,NT),
     +                        SIGINF(1,NT,2),WFNUC(1,NT),XNUB(1,NT))
C
                      IF(IPN.EQ.0) THEN
                                   DO  154  I = 1 , NET
                                   BDATA0(I,5   ,NT)  = XNUB(I,NT)
  154                              CONTINUE
                                   ELSE
                                   DO  155  I = 1 , NET
                                   BDATA (I,NET5,NT)  = XNUB(I,NT)
  155                              CONTINUE
                                   ENDIF
C
CM                WRITE(6,157)  IDENT,ISW,AI(29)
CM                WRITE(6,158) (XNU (I,NT),I=1,NETL)
CM                WRITE(6,159) (XNUB(I,NT),I=1,NET)
C
  156             CONTINUE
                  ENDIF
C
* 157 FORMAT(1H ,' ## IDENT ISW NU(2200M) ## ',A8,I6,F10.5)
* 158 FORMAT(1H ,' ## NU (FINE) ## ',10F10.5)
* 159 FORMAT(1H ,' ## NU (BROAD)## ',10F10.5)
C
      IF(LL.LE.2) THEN
               IF(IPN.GT.0) THEN
                  DO   160  I = 1 , NET
                  SIGINR(I,1) = BDATA(I,NET+2,1)
                  SIGINR(I,2) = BDATA(I,NET+4,1)
  160             CONTINUE
                            ELSE
                  DO   162  I = 1 , NET
                  SIGINR(I,1) = BDATA0(I,2,1)
                  SIGINR(I,2) = BDATA0(I,4,1)
  162             CONTINUE
                            ENDIF
                  ENDIF
C
      IF(IPN.EQ.0) GO TO 301
C
C
      IUP       = NET+1
      DO 300 NT = 1,NTEMP
      DO 290 N  = 1,NETL
      NJ        = NREG(N)
      IF(NJ.GT.0.AND.NJ.LE.NET) THEN
                  DO 250 ND = 1,NETL
                  NI        = NREG(ND)
      IF(NI.LE.0) BDATA(NJ,IUP,NT)  =  BDATA(NJ,IUP,NT)
     +            +    FDATA(ND,N,NT)*WFNUC(N,NT)/WRNUC(NJ,NT)
      IF(NI.GT.0) BDATA(NI,NJ ,NT)  =  BDATA(NI,NJ ,NT)
     +            +    FDATA(ND,N,NT)*WFNUC(N,NT)/WRNUC(NJ,NT)
  250             CONTINUE
                  ENDIF
  290 CONTINUE
  300 CONTINUE
C
*      WRITE(6,*) ' *** IDENT : ',MNNAME
*      CALL UTLWOT( BDATA , NET5 , NET , 1 , 'GRP.' , 'TRAN' , '    ',
*    +              '  THERMALU DATA     '      )
C
  301 CONTINUE
CM    IF(LL.LE.2.AND.NT.GT.1.AND.IFT.NE.0) THEN
      IF(LL.LE.2.AND.IFT.NE.0) THEN
             IF(IPN.EQ.0) THEN
                  DO 310 NT= 2 , NTEMP
                  DO 310 I = 1 , NET
                  SAVEC       = BDATA0(I,2,NT)
                  SAVET       = BDATA0(I,3,NT)
                  SAVEF       = BDATA0(I,4,NT)
                  SAVERC      = SIGINR(I,1)
                  SAVERF      = SIGINR(I,2)
               BDATA0(I,2,NT) = SAVERC
               BDATA0(I,4,NT) = SAVERF
               BDATA0(I,3,NT) = SAVET - SAVEC - SAVEF + SAVERC + SAVERF
  310             CONTINUE
C
                  ELSE
                  DO 315 NT= 2 , NTEMP
                  DO 315 I = 1 , NET
                  SAVEC       = BDATA (I,NET+2,NT)
                  SAVET       = BDATA (I,NET+3,NT)
                  SAVEF       = BDATA (I,NET+4,NT)
                  SAVERC      = SIGINR(I,1)
                  SAVERF      = SIGINR(I,2)
               BDATA (I,NET+2,NT) = SAVERC
               BDATA (I,NET+4,NT) = SAVERF
           BDATA (I,NET+3,NT) = SAVET - SAVEC - SAVEF + SAVERC + SAVERF
  315             CONTINUE
                  ENDIF
                  ENDIF
C
      FILENM(1)= 'THER'
      FILENM(2)= 'MALU'
CDEL  FILENM(3)= '    '
C
      IF(LL.EQ.1)   MNNAME (1:1) = LPN(2)
      IST  = 1
      LTH  = NET*(NET+5)*NTEMP
      LTH0 = NET*5*NTEMP
      IF(LL.LE.2.AND.IPN.EQ.0) LTH = LTH0
      CALL SEARCH(MNNAME,LENG,ISW)
C
      IF(ISW.EQ.1) THEN
                   IF(LTH.GT.LTH0) THEN
                                   CALL WRITE(MNNAME,BDATA ,LTH )
                                   ELSE
                                   CALL WRITE(MNNAME,BDATA0,LTH0)
                                   ENDIF
                   ENDIF
C
  320 CONTINUE
C
C-----COLLAPSE F-TABLE
C
      NTEMPF    = 0
      IFC       = 0
      IFF       = 0
      IF(IFT.EQ.0) GO TO 1000
C
      FILENM(1) =  'THER'
      FILENM(2) =  'MALP'
CDEL  FILENM(3) =  '    '
C
      MNNAME (1:1) = 'F'
      MNNAME (8:8) = 'C'
C
      CALL SEARCH(MNNAME,LTH,ISW)
C
      IF(ISW.NE.0) THEN
                   WRITE(6,6010) MNNAME
                   GO TO  560
                   ENDIF
C
C *** GET CAPTURE F-TABLE
C
      CALL CLEA( FTABF , MXSIG0*NGT*NTMAX , 1.0 )
      CALL CLEA( FTABB , MXSIG0*NET*NTMAX , 1.0 )
      CALL READ  (MNNAME,WORK,LTH)
      IFC      = 1
      NTEMPF   = NTEMP
      IPOS     = 0
C
      DO 390 NT=     1,NTEMP
      DO 390 N = NGMIN,NGMAX
      DO 390 J = 1,NSIG
      IPOS     = IPOS+1
      FTABF(N,J,NT)=WORK(IPOS)
  390 CONTINUE
  400 CONTINUE
C-----COLLAPSE CAPTURE F-TABLE
  401 CONTINUE
      DO 420 N =  1 , NETL
      DO 420 NT=  1 , NTEMPF
      DO 420 J =  1 , NSIG
      FTABF(N,J,NT)=FTABF(N,J,NT)*SIGINF(N,NT,1)
  420 CONTINUE
      DO 430 NT=  1 , NTEMPF
      DO 430 J =  1 , NSIG
      CALL UTLCND(NETL1,NETL,NET,NREG,1,WFNUC(1,NT),WRNUC(1,NT),
     +            FTABF(1,J,NT),FTABB(1,J,NT))
  430 CONTINUE
      DO 440 I =  1 , NET
      SAVE     = SIGINR(I,1)
      IF(SAVE.NE.0.0) THEN
      DO 433 NT=  1 , NTEMPF
      DO 433 J =  1 , NSIG
  433 FTABB(I,J,NT) = FTABB(I,J,NT)/SAVE
                      ELSE
      DO 436 NT=  1 , NTEMPF
      DO 436 J =  1 , NSIG
  436 FTABB(I,J,NT) = 1.0
                      ENDIF
  440 CONTINUE
C
      FILENM(1)= 'THER'
      FILENM(2)= 'MALU'
CDEL  FILENM(3)= '    '
      LENGF    = NSIG*NTEMPF*NET
C
      CALL SEARCH(MNNAME,LENG,ISW)
      IF(ISW.EQ.0) GO TO 560
      KK           = 0
      DO 445  I    = 1 , NET
      DO 445  J    = 1 , NTEMPF
      DO 445  K    = 1 , NSIG
      KK           = KK + 1
      WORK(KK)    = FTABB(I,K,J)
  445 CONTINUE
      CALL WRITE(MNNAME,WORK,LENGF)
CWRITE
*     CALL UTLWOT(WORK,NTEMPF,NSIG,NET,'SIG0','TEMP','GRP.',
*    +            ' F-TABLE (CAPTURE) '                    )
C
C-----COLLAPSE FISSION F-TABLE
C
  560 CONTINUE
*     WRITE(6,*) ' **  IFT IFFS IFC ** ',IFT,IFFS,IFC
C
      IF(IFFS.EQ.0) GO TO 600
      FILENM(1)= 'THER'
      FILENM(2)= 'MALP'
C
      MNNAME (8:8) = 'F'
      CALL SEARCH(MNNAME,LTH,ISW)
      IF(ISW.NE.0) THEN
                   WRITE(6,6010) MNNAME
                   GO TO  600
                   ENDIF
C
C *** GET FISSION F-TABLE
C
      CALL READ(MNNAME,WORK,LTH)
      CALL CLEA( FTABF , MXSIG0*NGT*NTMAX , 1.0 )
      CALL CLEA( FTABB , MXSIG0*NET*NTMAX , 1.0 )
C
      IPOS     = 0
      IFF      = 1
      NTEMPF   = NTEMP
C
      DO 450 NT=     1,NTEMP
      DO 450 N = NGMIN,NGMAX
      DO 450 J = 1,NSIG
      IPOS     = IPOS+1
      FTABF(N,J,NT)=WORK(IPOS)
  450 CONTINUE
C
C-----COLLAPSE FISSION F-TABLE
C
      DO 455 N =  1 , NETL
      DO 455 NT=  1 , NTEMPF
      DO 455 J =  1 , NSIG
      FTABF(N,J,NT)=FTABF(N,J,NT)*SIGINF(N,NT,2)
  455 CONTINUE
      DO 460 NT=  1 , NTEMPF
      DO 460 J =  1 , NSIG
      CALL UTLCND(NETL1,NETL,NET,NREG,1,WFNUC(1,NT),WRNUC(1,NT),
     +            FTABF(1,J,NT),FTABB(1,J,NT))
  460 CONTINUE
C
      DO 470 I =  1 , NET
      SAVE     = SIGINR(I,2)
      IF(SAVE.NE.0.0) THEN
      DO 463 NT=  1 , NTEMPF
      DO 463 J =  1 , NSIG
  463 FTABB(I,J,NT) = FTABB(I,J,NT)/SAVE
                      ELSE
      DO 466 NT=  1 , NTEMPF
      DO 466 J =  1 , NSIG
  466 FTABB(I,J,NT) = 1.0
                      ENDIF
  470 CONTINUE
C
  480 FILENM(1)= 'THER'
      FILENM(2)= 'MALU'
CDEL  FILENM(3)= '    '
      LENGF    = NSIG*NTEMPF*NET
C
      CALL SEARCH(MNNAME,LENG,ISW)
      IF(ISW.EQ.0) GO TO 600
      KK           = 0
      DO 550  I    = 1 , NET
      DO 550  J    = 1 , NTEMPF
      DO 550  K    = 1 , NSIG
      KK           = KK + 1
      WORK(KK)     = FTABB(I,K,J)
  550 CONTINUE
      CALL WRITE(MNNAME,WORK,LENGF)
CWRITE
*     CALL UTLWOT(WORK,NTEMPF,NSIG,NET,'SIG0','TEMP','GRP.',
*    +            ' F-TABLE (FISSION) '                    )
C
  600 CONTINUE
C-----OUTPUT CONT MEMBER TO THERMALU LIBRARY
 1000 CONTINUE
C@ADD
      IF(IPN.GT.0) THEN
      IPN       = NP1(1) + 2*NP1(2) + 4*NP1(3) + 8*NP1(4) + 16*NP1(5)
     +                   +32*NP1(6) +64*NP1(7)
                   ENDIF
C
      NGMIN     = 1
      NGMAX     = NET
C
      FILENM(1) =  'THER'
      FILENM(2) =  'MALU'
CDEL  FILENM(3) =  '    '
C
      II(30)    = NET*(5+NET)
      IF(IPN.EQ.0) II(30) = 5*NET
      MNNAME (1:1) = 'C'
      MNNAME (8:8) = '0'
CKK9
      IF(NTEMPF.EQ.0)  THEN
                       IFT   = 0
                       NGMIN = 0
                       NGMAX = 0
                       ENDIF
C
*     WRITE(6,*) ' ** MNNAME IFT NTEMPF IFC IFF : ',MNNAME,IFT,NTEMPF,
*    +                                              IFC,IFF
CEND
      CALL SEARCH(MNNAME,LENG,ISW)
      IF(ISW.EQ.1) CALL WRITE(MNNAME,II,LGTCON)
CWRITE
*     WRITE(6,7004) MNNAME
*     WRITE(6,7002) (II(I),I=1,LGTCON)
*     WRITE(6,7003) (AI(I),I=1,LGTCON)
*7004 FORMAT(1H ,' ## NUCLID OF THERMALU ## ',A8)
C
      GO TO 120
C
 1001 CONTINUE
      RETURN
C
 6010 FORMAT(1H0,9X,'MEMBER NAME ',A8,' NOT FOUND IN PUBLIC LIB')
 6050 FORMAT(1H ,' CAUTION -- NO DATA FOR ',A8,' IN PUBLIC-LIB.'
     +         /'          -- COPY PROCESS SKIPPED FOR THIS NUCLIDE.')
 6051 FORMAT(1H ,' CAUTION -- NO DATA FOR ',A8,' IN PUBLIC-LIB.')
      END
