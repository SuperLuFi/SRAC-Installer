      SUBROUTINE IRACAL(MTNAME,NISO  ,TEMP  ,XL    ,DC    ,TOTSIG,IDENT
     &                 ,IRES  ,DN    ,LXMICR,VOL   ,SFCTR ,X1    ,X2
     &                 ,Y1    ,Y2    ,WK1   ,WK2   ,WK3   ,ENBND ,SIG0
     &                 ,FTAB  ,SIGF  ,SIGFNU,SIGC  ,SSF   ,SSNU  ,SSC
     &                 ,XSECFC,LTH   ,LA    ,LD    ,FTEMP ,FSIG0 ,PIJ
     &                 ,ISWF  ,SIGT  ,POT   ,AMF   ,ISWM  ,AMM   ,DNM
     &                 ,TRR   ,SSTF  ,SSTM  ,SSSF  ,SSSM  ,SGMAF ,SGMAM
     &                 ,DELAMF,DELAMM,PARAF1,PARAM1,PARAF2,PARAM2,XF
     &                 ,XM,YF ,YM    ,WDATA ,PDATA ,FTABNR,SIG0NR
     &                 ,PSEOUT )
C
C     IRACAL ----- IR METHOD STRAT
C
      DOUBLE PRECISION  JNEFST,FNEFST,JNMACR,FNMACR,ID,IDMT
C
      COMMON  /IRACNL/ IOPT(20),JNEFST,FNEFST,JNMACR,FNMACR,NEF,IGT,
     &                 NMAT,KNMAX,MXMTX,MXTEMP,MXSIG0,LNMAX,IDS,NEF1,
     &                 MXREAC,NOUT1,NOUT2
C
      COMMON  /IRACNT/ AMASS,SIGP,ICAPT,IFISS,IIRES,LTOT,IFS,IFTR,IFC,
     &                 IFF,IFE,IFER,NGMIN,NGMAX,NSIG,NTEMP,SIGC0
C
      COMMON  /IRADAT/  ISTART,NEFR,NEFB,NOMTF,NISOM,TCOR,S,NRES,
     &                  NPSE,VOLF,VOLM,KKNMAX,NMP,MATD(50)
      COMMON  /IRAPRM/ ENGAM(300),GT(300),GN(300),GG(300),GF(300),
     &                 PEAK(300),SIGOP(300),BETINF(300),ZETINF(300),
     &                 RINT(300)
      COMMON  /IRAPSE/ TAKA,SQGUZ,C1B,ZETT,DELTA1
      COMMON  /IRAWRK/ A(17000),NAMEP(2),LOCAM(11),LOCAF(6)
      COMMON  /PDSPDS/ BUFFER(540),IFLSW,NFILE(3),ECODE,ITEMP
C
CMOD  PARAMETER   ( MXNISO = 110 , MAXNG = 107  , MAXMT3 = 6 )
      INCLUDE  'BMICRINC'
      COMMON   /MICCOM/ EFFMIC(MAXNG,MAXMT3,MXNISO),LENEFF
C
C     DIMENSION  MTNAME(2,NMAT),NISO(NMAT),TEMP(NMAT),XL(NMAT),DC(NMAT)
      DIMENSION  MTNAME(2,NMAT),NISO(NMAT),TEMP(NMAT),XL(NMAT)
     &          ,IDENT(2,KNMAX,NMAT),IRES(KNMAX,NMAT),
     &           DN(KNMAX,NMAT),LXMICR(KNMAX,NMAT),VOL(NMAT)
CJAIS MODIFIED FOR NUCLIDE-WISE DANCOFF FACTOR ***4/8/1985***
      DIMENSION  DC(KNMAX,NMAT)
C
      DIMENSION TOTSIG(NMAT),SFCTR(NEF,2,2),X1(MXTEMP),X2(MXSIG0),
     &          Y1(MXTEMP),Y2(MXSIG0),WK1(LNMAX),WK2(LNMAX),WK3(LNMAX),
     &          ENBND(NEF1),SIG0(NEF,2),FTAB(MXSIG0,MXTEMP,NEF),
     &          SIGF(NEF),SIGFNU(NEF),SIGC(NEF),SSF(NEF),SSNU(NEF),
     &          SSC(NEF),LTH(MXMTX),LA(MXMTX),LD(MXMTX),FTEMP(MXTEMP),
     &          FSIG0(MXSIG0)
C
      DIMENSION PIJ(NMAT,NMAT),ISWF(KNMAX),ISWM(NISOM),SIGT(NEF),
     &          SSTF(KNMAX,NEFR),SSSF(KNMAX,NEFR),AMF(KNMAX),POT(KNMAX),
     &          SSTM(NISOM,NEFR),SSSM(NISOM,NEFR),AMM(NISOM),
     &          XSECFC(NEF,2,2),DNM(NISOM),
     &          SGMAF(KNMAX),DELAMF(KNMAX),PARAF1(KNMAX),PARAF2(KNMAX),
     &          SGMAM(NISOM),DELAMM(NISOM),PARAM1(NISOM),PARAM2(NISOM),
     &          XF(KNMAX),YF(KNMAX),XM(NISOM),YM(NISOM),TRR(NISOM)
C=======================================================================
      DIMENSION  WDATA(12,NEF),PDATA(4,300),PSEOUT(2,300)
      DIMENSION  FTABNR(MXSIG0,NEF),SIG0NR(NEF,2)
C=======================================================================
      DIMENSION   IA(17000)
      EQUIVALENCE (A(1),IA(1))
C     CHARACTER *4 IDPQ,NAMEP
      DIMENSION   IDPQ(6)
CADD SASAQ
      CHARACTER*4 IDPQ,NFILE,NAMEP,IDENT,MTNAME
      DATA   IDPQ/'   M','   F','   0','   2','   P','   C'/
C
C     *************************************************
C     *      PROBLEM CHECK  AND INITIAL SET           *
C     *************************************************
      DO 10 I=1,NMAT
      CALL PACK(MTNAME(2,I),1,IDPQ(2))
      CALL PACK(MTNAME(2,I),4,IDPQ(4))
   10 CONTINUE
C
      DO 12 I=1,NMAT
      MMK = NISO(I)
      IF(MMK.LE.0)  GO TO 12
      DO 11 M=1,MMK
      CALL PACK(IDENT(1,M,I),1,IDPQ(3))
      IF(IRES(M,I).NE.1) THEN
      CALL PACK(IDENT(2,M,I),1,IDPQ(3))
      CALL PACK(IDENT(2,M,I),4,IDPQ(3))
                         ELSE
      CALL PACK(IDENT(2,M,I),1,IDPQ(2))
                         ENDIF
   11 CONTINUE
   12 CONTINUE
C
      IF(NMP.LE.1)  THEN
                    WRITE(NOUT1,14)
                    RETURN
                    ENDIF
C
   14 FORMAT(1H ,' WARNING --- ONE MATERIAL PROBLEM. THEN IRA ROUTINE ',
     & 'IS SKIPPED ]]')
C
      IF(NOMTF.EQ.0)  THEN
                      WRITE(NOUT1,15)
                      RETURN
                      ENDIF
   15 FORMAT(1H ,' WARNING ------NO FUEL MATERIAL ]] THEN IRA ROUTINE',
     &' IS SKIPPED ]]')
C
      IF(VOLF.LE.0.0.OR.VOLM.LE.0.0) THEN
                      WRITE(NOUT1,16)
                      RETURN
                      ENDIF
C
   16 FORMAT(1H ,' WARNING ------VOLUME UNDEFINED ]] THEN IRA ROUTINE',
     &' IS SKIPPED ]]')
C
      IPASS    = 0
      IFLSW    = 1
      NFILE(1) = 'MACR'
      NFILE(2) = 'OWRK'
      NFILE(3) = '    '
      NAMEP(1) = 'CONT'
      NAMEP(2) = 'F002'
      LENG     = NEF*2 + 2
      CALL  READ(NAMEP(1),A,LENG)
      NEFMCR   = IA(1)
      IF(NEFMCR.EQ.NEF)  GO TO 30
      WRITE(NOUT1,22) NEF,NEFMCR
   22 FORMAT(///1H ,10X,'ERROR STOP ----- UNMATCH GROUP NO.(AT SUBR.',
     &'IRACAL)',    /1H ,10X,'GROUP NO. OF INPUT(INPUT1):',I3,
     &         /1H ,10X,'GROUP NO. OF MACRO-LIB    :',I3)
      STOP
C
   30 CONTINUE
      NAMEP(1) = MTNAME(1,NOMTF)
      NAMEP(2) = MTNAME(2,NOMTF)
      CALL SEARCH(NAMEP(1),LENG,ISW)
      IF(ISW.EQ.0)  GO TO 33
      WRITE(NOUT1,31) NAMEP(1),NAMEP(2)
      RETURN
   31 FORMAT(//1H ,10X,'MEMBER(',2A4,') DOES NOT EXIST IN MACRO-LIB',
     &        /1H ,10X,'IR METHOD IS SKIPPED')
C
   33 CONTINUE
      IST = NEF+1
      DO 34 I=1,NEF1
      IST = IST+1
      ENBND(I)=A(IST)
   34 CONTINUE
C
C     IFLSW    = 1
C     NFILE(1) = 4HFAST
C     NFILE(2) = 4HU
C     NFILE(3) = 4H
      CALL  CLEA(SIGF  ,NEF,0.0)
      CALL  CLEA(SIGFNU,NEF,0.0)
      CALL  CLEA(SIGC  ,NEF,0.0)
C     *************************************************
C     *  PARAMETER SETTING  ( SIGT,SSTM,NISOM ....)   *
C     *************************************************
      CALL  IRASIG(NISO  ,XL    ,DC    ,TOTSIG,IDENT ,IRES  ,DN    ,
     &             LTH   ,LA    ,LD    ,FTEMP ,FSIG0 ,PIJ   ,SSTF  ,
     &             SSTM  ,SSSF  ,SSSM  ,AMF   ,AMM   ,SIGT  ,POT   ,
     &             ISWM  ,MTNAME,ENBND ,DNM   ,VOL)
C
      NISOF = NISO(NOMTF)
      IFLSW = 1
      RTEMP = TEMP(NOMTF)
CJAIS DELETED FOR NUCLIDE-WISE DANCOFF FACTOR ***4/8/1985***
C     DANCF = DC(NOMTF)
      XLL   = XL(NOMTF)
      IF(XLL.EQ.0.0)  XLL = 1.0E+20
      TCOR  = (800.+0.45*RTEMP)/(800.+RTEMP)
      BELL  = 1.200
C=======================================================================
C     IF(IGT.EQ.1)  BELL=1.69
      IF(IGT.EQ.1)  BELL=1.40
C=======================================================================
      NAMEP(1) = MTNAME(1,NOMTF)
CMOD  NAMEP(2) = MTNAME(2,NOMTF)(2:2) // 3HEFF
CKSK  NAMEP(2) = 4HBMIC
      NAMEP(2) = 'BMIC'
      NFILE(1) = 'MICR'
      NFILE(2) = 'EF  '
      CALL  CLEA(  EFFMIC , LENEFF , 0.0 )
      LENG     =  MAXNG*MAXMT3*NISOF
      CALL  READ( NAMEP , EFFMIC ,  LENG )
C    *********************************************************
C    * LOOP OF NUCLIDE ( FUEL MATERIAL )                     *
C    *********************************************************
      DO 600  M=1,NISOF
      IF(IRES(M,NOMTF).NE.2)  GO TO 600
      NFILE(1) = 'FAST'
      NFILE(2) = 'U   '
      NFILE(3) = '    '
      MASS     = AMF(M)+0.2
C    *********************************************************
C    * FERTILE NUCLIDE CHECK                                 *
C    *********************************************************
      IF((MASS-232)*(MASS-238)*(MASS-240).NE.0)  GO TO 600
C    *********************************************************
C    * X-SECTION READING FROM FASTU-LIBRARY                  *
C    *********************************************************
C     DATA   IDPQ/'   M','   F','   0','   2','   P','   C'/
      NAMEP(1) = IDENT(1,M,NOMTF)
      NAMEP(2) = IDENT(2,M,NOMTF)
      CALL PACK(NAMEP(1),1,IDPQ(6))
      CALL PACK(NAMEP(2),1,IDPQ(3))
      CALL PACK(NAMEP(2),4,IDPQ(3))
CJAIS ADDED FOR NUCLIDE-WISE DANCOFF FACTOR ***4/8/1985***
      DANCF    = DC(M,NOMTF)
      DNTMP    = DN(M,NOMTF)
      IF(DNTMP.LE.0.0001) GO TO 600
      DNTMP    = 1.0/DNTMP
      CALL IRACON(LTH,LA,LD,FTEMP,FSIG0)
      IF(IIRES.NE.1)   GO TO 600
      IF(IFS.EQ.0)     GO TO 600
      IF(NGMAX.LT.ISTART)    GO TO 600
      IF(NGMIN.LE.0)         GO TO 600
      IF(NGMIN.GT.NGMAX)     GO TO 600
      NRES=M
C --- MATRIX DATA READ
      CALL  PACK(NAMEP(1),1,IDPQ(1))
      IF(LTOT.LE.0)  GO TO 600
      CALL  READ(NAMEP(1),A,LTOT)
C --- TOTAL AND SIG0(NR METHOD)
      IST   = LOCAM(5)-1
      DO 110 I=1,NEF
      SAVE  = A(IST+I)
      SIG0(I,1)=SIGT(I)*DNTMP-SAVE
  110 CONTINUE
C --- CAPTURE
      CALL  CLEA(SSC,NEF,0.0)
      CALL  CLEA(SSF,NEF,0.0)
      CALL  CLEA(SSNU,NEF,0.0)
C
      IF(ICAPT.EQ.0)  GO TO 121
      IST=LOCAM(1)-1
      DO 120 I=1,NEF
      SSC(I)=A(IST+I)
  120 CONTINUE
C --- FISSION,NU
  121 IF(IFISS.EQ.0) GO TO 131
      IST1=LOCAM(2)-1
      IST2=LOCAM(3)-1
      DO 130 I=1,NEF
      SSF(I)=A(IST1+I)
  130 SSNU(I)=A(IST2+I)
C    *********************************************************
C    * RESONANCE PARAMEER READ                               *
C    *********************************************************
  131 CONTINUE
      CALL IRARSP(IDENT(1,M,NOMTF),NRS,EHIGH,ELOW)
C
      IF(NRS.LE.0)  GO TO 600
C=======================================================================
      CALL CLEA( WDATA , NEF*12,0.0 )
      CALL CLEA( PDATA , 1200  ,0.0 )
      CALL CLEA( PSEOUT,  600  ,0.0 )
C=======================================================================
C    *********************************************************
C    * HETEROGENEITY CORRECTION                              *
C    *********************************************************
      S=(BELL/(1.0+(BELL-1.0)*DANCF))*(1.0-DANCF)*DNTMP/XLL
      DO 140 I=1,NEF
      SIG0(I,1)=SIG0(I,1)+S
      SIG0(I,2)=SIG0(I,1)
  140 CONTINUE
C    *********************************************************
C    *   ISW,NPSE PARAMETER SETTING                          *
C    *********************************************************
      CALL ICLEA(ISWF,KNMAX,1)
      NPSE=0
C     DO 160 I = 1,NISOF
C     IDUM     = IRES(I,NOMTF)
C     IF(IDUM.EQ.-1)                GO TO 151
C     IF(IDUM.NE. 1)  ISWF(I) = 1
C     GO TO 160
C
C 151          CONTINUE
C              NAMEP(1) = IDENT(1,I,NOMTF)
C              NAMEP(2) = IDENT(1,NRES,NOMTF)
C              CALL PACKX(NAMEP(2),3,IDENT(1,NRES,NOMTF),4)
C              CALL PACK(NAMEP(2),4,IDPQ(5))
C=======================================================================
C              IF(NAMEP(1).NE.NAMEP(2))   GO TO 160
C                       NPSE     = I
C                       ISWF(I)  = 1
C                       NAMEP(1) = IDENT(1,I,NOMTF)
C                       NAMEP(2) = IDENT(2,I,NOMTF)
C                       CALL PACK(NAMEP(1),1,IDPQ(6))
C                       CALL READ(NAMEP(1),PSEOUT,600)
C=======================================================================
C     IF(NAMEP(1).EQ.NAMEP(2))  NPSE   = I
C     IF(NAMEP(1).EQ.NAMEP(2))  ISWF(I)= 1
  160 CONTINUE
C    *********************************************************
C    *   IR PARAMETER SEARCH                                 *
C    *********************************************************
      IF(IOPT(19).GT.0)
     &WRITE(NOUT2,271) IDENT(1,M,NOMTF),IDENT(2,M,NOMTF),DANCF,S,
     &                 NRES,NPSE
C
      DO 170 I=1,NEF
      ITOP = I
      IF(EHIGH.GT.ENBND(I+1)) GO TO 171
  170 CONTINUE
  171 CONTINUE
      DO 180 I=1,NEF
      IEND = I
      IF(ELOW.GE.ENBND(I+1).OR.ENGAM(NRS).GE.ENBND(I+1)) GO TO 181
  180 CONTINUE
  181 CONTINUE
      IF(IEND.LT.ISTART)  GO TO 600
      IF(ITOP.LT.ISTART)  ITOP=ISTART
      ISET = 1
      NONG=ITOP
C    *********************************************************
C    *  RESONANCE PARAMETER LOOP                             *
C    *********************************************************
  300 IF(NONG.GT.IEND)  GO TO 301
      EONE = ENBND(NONG)
      ETWO = ENBND(NONG+1)
      SUM1 = 0.0
      SUM2 = 0.0
C=======================================================================
      WSUM1= 0.0
      WSUM2= 0.0
      WSUM3= 0.0
      II   = NONG-NEFB
C=======================================================================
      IF(ISET.GT.NRS) GO TO 301
      EEEE=ENGAM(ISET)
  250 IF(EEEE.GE.EONE)  GO TO 270
      IF(EEEE.LT.ETWO)  GO TO 299
C
C     CALL  IRASET(DN(1,NOMTF) ,DNM   ,SSTF  ,SSTM  ,SSSF  ,SSSM  ,
      CALL  IRASET(DN(1,NOMTF) ,DNM          ,SSTF(1,II)   ,
     &             SSTM(1,II)  ,SSSF(1,II)   ,SSSM(1,II)   ,
     &             POT         ,AMF   ,AMM   ,SGMAF ,SGMAM ,DELAMF,
     &             DELAMM      ,PARAF1,PARAM1,PARAF2,PARAM2,XF    ,
     &             XM          ,YF    ,YM    ,TRR   ,ISWF  ,ISWM  ,
     &             NISOF       ,ENGAM(ISET)  ,GT(ISET)     ,
     &             GN(ISET)    ,GF(ISET)     ,GG(ISET)     ,
     &             PEAK(ISET)  ,SIGOP(ISET)  ,ZETINF(ISET) ,
     &             BETINF(ISET),ANS          ,ITR          ,
     &             SIGMA       ,THT          ,ANSA         ,
     &             ANSP        ,PSEOUT(1,ISET) )
C
      IF(LXMICR(M,NOMTF).LE.1) GO TO 260
C    *********************************************************
C    *  PSUEDO PARAMETER SETTING  FOR DOUBLE HETEROGENEITY   *
C    *********************************************************
C=======================================================================
                  CALL IRAMIX(AMIX ,AMF ,S  ,KNMAX,NISOM,RTAKA,SSSSSS)
                  PDATA(1,ISET) = RTAKA
                  PDATA(2,ISET) = AMIX
                  PDATA(4,ISET) = NONG  + 0.1
  260 WDATA(5,NONG) = 1.0
      WDATA(4,NONG) = SIGMA
C=======================================================================
      SUM1          = SUM1  + RINT(ISET)*ANS
      SUM2          = SUM2  + RINT(ISET)
C=======================================================================
      WSUM1         = WSUM1 + RINT(ISET)*ANSP
      WSUM2         = WSUM2 + RINT(ISET)*THT
      WSUM3         = WSUM3 + RINT(ISET)*ANSA
C=======================================================================
      EFFINT        = RINT(ISET)/SQGUZ
      IF(IOPT(19).EQ.0) GO TO 270
      STAKA         = S*TAKA
      WRITE(NOUT2,272) NONG,ENGAM(ISET),PEAK(ISET),RINT(ISET),
     &                 SQGUZ,EFFINT,ANS,ITR,ANSA,TAKA,STAKA
      WRITE(NOUT2,273) (PARAF1(JJ),JJ=1,NISOF)
      WRITE(NOUT2,274) (PARAM1(JJ),JJ=1,NISOM)
C=======================================================================
C     WRITE(NOUT2,276) (SSTF(JJ,II),JJ=1,NISOF)
C     WRITE(NOUT2,277) (SSSF(JJ,II),JJ=1,NISOF)
C     WRITE(NOUT2,278) (SSTM(JJ,II),JJ=1,NISOM)
C     WRITE(NOUT2,279) (SSSM(JJ,II),JJ=1,NISOM)
C=======================================================================
  270 CONTINUE
      ISET = ISET + 1
      IF(ISET.GT.NRS)  GO TO 299
      EEEE = ENGAM(ISET)
      IF(EEEE.GE.ETWO)  GO TO 250
  299 IF(SUM2.EQ.0.0)   GO TO 298
      SUMDIV        = 1.0 / SUM2
      SIG0(NONG,2)  = SUM1*SUMDIV
C=======================================================================
      WDATA(10,NONG)= SIG0(NONG,2)
      WDATA(1,NONG) = WSUM1*SUMDIV
      WDATA(2,NONG) = WSUM2*SUMDIV
      WDATA(3,NONG) = WSUM3*SUMDIV
C=======================================================================
  298 NONG=NONG+1
      IF(NONG.LE.IEND) GO TO 300
  301 CONTINUE
      IF(IOPT(19).GT.0) WRITE(NOUT2,275)
C    *********************************************************
C    *   IR PARAMETER SEARCH ENDED                           *
C    *********************************************************
  271 FORMAT(////1H ,20X,'IR-PARAMETER TABLE',5X,'ABSORBER = ',2A4,
     &//1H ,15X,'DANCOFF FACTOR = ',1PE12.5,
     &'   HETEROGENEOUS EFFECT  S = ',1PE12.5,'  NRES = ',I3,
     &'  NPSE = ',I3,//1H ,10X,'GRP. ENERGY      PEAK        INFINITE',
     &'    SQGUZ       INFI./SQGUZ   SIG0           ITR ',
     &' ANSA       MU*         S X MU*    ',
     &/1H ,5X,25('-----'))
  272 FORMAT(1H ,6X,I6,2X,1P6E12.5,3X,I5,1P3E12.5)
  273 FORMAT(1H ,6X,'(PRAM-F)',
     &  2X, F8.4,4X, F8.4,4X, F8.4,4X, F8.4,4X,
     &      F8.4,4X, F8.4,4X, F8.4,4X, F8.4,4X:)
  274 FORMAT(1H ,6X,'(PRAM-M)',
     &  2X, F8.4,4X, F8.4,4X, F8.4,4X, F8.4,4X,
     &      F8.4,4X, F8.4,4X, F8.4,4X, F8.4,4X:)
  275 FORMAT(1H ,5X,25('-----'))
  276 FORMAT(1H ,6X,'(TOT -F)',1P8E12.5)
  277 FORMAT(1H ,6X,'(SDWN-F)',1P8E12.5)
  278 FORMAT(1H ,6X,'(TOT -M)',1P8E12.5)
  279 FORMAT(1H ,6X,'(SDWN-M)',1P8E12.5)
C    *********************************************************
C    *  SHIELDING FACTOR CALCULATION                         *
C    *********************************************************
      CALL  CLEA(SFCTR,NEF*4,1.0)
C
      NAMEP(1)=IDENT(1,M,NOMTF)
      NAMEP(2)=IDENT(2,M,NOMTF)
      CALL  PACK(NAMEP(1),1,IDPQ(2))
      CALL  SEARCH(NAMEP(1),LENG,ISW)
      IF(ISW.EQ.1)  GO TO 600
      CALL  CLEA(A,17000,0.0)
      CALL  READ(NAMEP(1),A,LENG)
C
      IF(IFC.EQ.0)  GO TO 320
C    *********************************************************
C    *  SHIELDING FACTOR CALCULATION FOR CAPTUR REACTION     *
C    *********************************************************
      IST      = LOCAF(2)
      CALL CLEA(FTAB,NEF*MXSIG0*MXTEMP,1.0)
      DO 310 I = NGMIN,NGMAX
      DO 310 K = 1,NTEMP
      DO 310 J = 1,NSIG
      FTAB(J,K,I) = A(IST)
      IST=IST+1
  310 CONTINUE
C
      CALL SPLINE(FTAB  ,SFCTR(1,1,1)  ,SIG0(1,1)   ,RTEMP ,FSIG0 ,
     &            FTEMP ,X1    ,X2    ,Y1    ,Y2    ,WK1   ,WK2   ,
     &            WK3   ,MXSIG0,MXTEMP,LNMAX ,NEF   ,NTEMP ,NSIG  ,
     &            NGMIN ,NGMAX )
C
      CALL SPLINE(FTAB  ,SFCTR(1,2,1)  ,SIG0(1,2)   ,RTEMP ,FSIG0 ,
     &            FTEMP ,X1    ,X2    ,Y1    ,Y2    ,WK1   ,WK2   ,
     &            WK3   ,MXSIG0,MXTEMP,LNMAX ,NEF   ,NTEMP ,NSIG  ,
     &            NGMIN ,NGMAX )
C
  320 CONTINUE
      IF(IFF.EQ.0)  GO TO 350
C    *********************************************************
C    *  SHIELDING FACTOR CALCULATION FOR FISSION REACTION    *
C    *********************************************************
      IST      = LOCAF(3)
      CALL CLEA(FTAB,NEF*MXSIG0*MXTEMP,1.0)
      DO 330 I = NGMIN,NGMAX
      DO 330 K = 1,NTEMP
      DO 330 J = 1,NSIG
      FTAB(J,K,I) = A(IST)
      IST=IST+1
  330 CONTINUE
C
      CALL SPLINE(FTAB  ,SFCTR(1,1,2)  ,SIG0(1,1)   ,RTEMP ,FSIG0 ,
     &            FTEMP ,X1    ,X2    ,Y1    ,Y2    ,WK1   ,WK2   ,
     &            WK3   ,MXSIG0,MXTEMP,LNMAX ,NEF   ,NTEMP ,NSIG  ,
     &            NGMIN ,NGMAX )
C
      CALL SPLINE(FTAB  ,SFCTR(1,2,2)  ,SIG0(1,2)   ,RTEMP ,FSIG0 ,
     &            FTEMP ,X1    ,X2    ,Y1    ,Y2    ,WK1   ,WK2   ,
     &            WK3   ,MXSIG0,MXTEMP,LNMAX ,NEF   ,NTEMP ,NSIG  ,
     &            NGMIN ,NGMAX )
C
  350 CONTINUE
      IPASS=1
      DNTMP=DN(M,NOMTF)
      DO 360 I=1,NEF
      XSECFC(I,1,1) = SSF(I)*SFCTR(I,1,2)
      XSECFC(I,2,1) = SSF(I)*SFCTR(I,2,2)
      XSECFC(I,1,2) = SSC(I)*SFCTR(I,1,1)
      XSECFC(I,2,2) = SSC(I)*SFCTR(I,2,1)
C=======================================================================
      WDATA(12,I)   = SFCTR (I,2,1)
      WDATA( 7,I)   = XSECFC(I,2,1) + XSECFC(I,2,2)
C=======================================================================
      IF(I.LT.ISTART)  GO TO 355
      SAVE          = DNTMP*SSF(I)*(SFCTR(I,2,2)-SFCTR(I,1,2))
      SIGF(I)       = SIGF(I)  +SAVE
      SIGFNU(I)     = SIGFNU(I)+SAVE*SSNU(I)
      SIGC(I)       = SIGC(I)  +SSC(I)*DNTMP*(SFCTR(I,2,1)-SFCTR(I,1,1))
C=======================================================================
CDEL  EFFMIC(I,1,M) = XSECFC(I,2,1)
CDEL  EFFMIC(I,2,M) = XSECFC(I,2,2)
CDEL  EFFMIC(I,6,M) = XSECFC(I,2,2)*SSNU(I)
      EFFMIC(I,1,M) = XSECFC(I,2,2)
      EFFMIC(I,2,M) = XSECFC(I,2,1)
      EFFMIC(I,6,M) = XSECFC(I,2,1)*SSNU(I)
  355 CONTINUE
  360 CONTINUE
C
C     IF(LXMICR(M,NOMTF).LE.1) GO TO 400
                               GO TO 400
C    *********************************************************
C    *  PSUEDO NUCLIDE CALCULATION FOR DOUBLE HETEROGENEITY  *
C    *********************************************************
C=======================================================================
C           WRITE(6,383)
C           DO 370 I = ITOP , IEND
C           IF(WDATA(5,I).EQ.0.0)  GO TO 370
C           TTTT = WDATA(1,I)
C           HHHH =  0.0
C           IF(TTTT.NE.0.0)  HHHH = WDATA(7,I)/TTTT
C           TTTT =  WDATA(2,I)*( WDATA(4,I)/S - 1.0 )
C           TTTT =  1.0 / ( TTTT + 1.0 )
C           DISADV =  ( 1.0 - TTTT*HHHH ) / ( 1.0 - HHHH)
C           WDATA(11,I)= DISADV
C           DISADV = ( VOLM + VOLF ) / ( VOLF + DISADV*VOLM )
C           WDATA(6,I) = DISADV
C           WDATA(8,I) = HHHH
C           WDATA(9,I) = TTTT
C           WRITE(6,381) I,(WDATA(J,I),J=1,11)
C 370       CONTINUE
C           IF(IFC.EQ.0)  GO TO 400
C           IST     =LOCAF(2)
C           CALL CLEA(FTAB,NEF*MXSIG0*MXTEMP,1.0)
C           DO 710 I=NGMIN,NGMAX
C           DO 710 K=1,NTEMP
C           DO 710 J=1,NSIG
C           FTAB(J,K,I)=A(IST)
C           IST=IST+1
C 710       CONTINUE
C
C           CALL CLEA(FTABNR,NEF*MXSIG0,1.0)
C
C                 DO  740  J = 1 , NSIG
C                 DO  720  I = 1 , NEF
C                 SIG0NR(I,1) = FSIG0(J)
C 720             SIG0NR(I,2) = 1.0
C
C     CALL SPLINE(FTAB  ,SIG0NR(1,2)  ,SIG0NR(1,1)  ,RTEMP ,FSIG0 ,
C    +            FTEMP ,X1    ,X2    ,Y1    ,Y2    ,WK1   ,WK2   ,
C    +            WK3   ,MXSIG0,MXTEMP,LNMAX ,NEF   ,NTEMP ,NSIG  ,
C    +            NGMIN ,NGMAX )
C
C                 DO 730  I = 1 , NEF
C                 FTABNR(J,I) = SIG0NR(I,2)
C 730             CONTINUE
C 740             CONTINUE
C
C      WRITE(6,741)
C      DO 750 I = ITOP,IEND
C      WRITE(6,742) I , WDATA(10,I) , WDATA(12,I) , WDATA(6,I)
C      WRITE(6,743) (FTABNR(J,I),J=1,NSIG)
C 750  CONTINUE
C
C 381 FORMAT(1H ,' ##WDATA## ',I2,1P11E11.4)
C 382 FORMAT(1H ,' ## I PDATA ## ',I6,6X,1P5E12.5)
C 383 FORMAT(1H1//1H ,20X,' ## CHECK LIST (SUB.IRACAL) ## '//)
C 741 FORMAT(1H1//1H ,10X,' ### FTABNR LIST ## '//)
C 742 FORMAT(1H ,' ## I SIG0 F-C DISA  ## ',I12,1P3E12.5)
C 743 FORMAT(1H ,' ## FTABNR ## ',1P10E12.5)
C
C             DO 760 I = 1 , NSIG
C             X2(I) = ALOG(FSIG0(I))
C 760         CONTINUE
C
C             DO 800 I = 1 , NRS
C             IF(PDATA(4,I).LE.0.0)  GO TO 800
C             NG = PDATA(4,I)
C             IF(NG        .LE.  0)  GO TO 800
C
C             DO 770 J = 1 , NSIG
C             Y2(J)      = ALOG(FTABNR(J,NG))
C 770         CONTINUE
C
C             FSET       = WDATA(12,NG)*WDATA(6,NG)
C             CALL IRASRC
C    *           (FSET  ,ANS   ,FSIG0 ,X2    ,Y2    ,FTABNR(1,NG),
C    *            WK1   ,WK2   ,WK3   ,MXSIG0,LNMAX ,NSIG  )
C             PDATA(3,I) = ( ANS - WDATA(3,NG) ) / PDATA(1,I)
C 800         CONTINUE
C
C             CALL  CLEA ( PSEOUT , 600 , 0.0 )
C             SUMMAS = 0.0
C             SUMMUS = 0.0
C             SUMSUM = 0.0
C             WRITE(6,383)
C             DO  380 I =  1 , NRS
C             PSEOUT(1,I) = PDATA(2,I)
C             PSEOUT(1,I) = PDATA(1,I)
C             PSEOUT(2,I) = PDATA(3,I)
C
C             IF(PDATA(1,I).EQ.0.0)  GO TO 380
C             SUMSUM = SUMSUM + 1.0
C             SUMMAS = SUMMAS + PDATA(2,I)
C             ANS    = PDATA(1,I)*PDATA(3,I)
C             SUMMUS = SUMMUS + ANS
C             WRITE(6,382) I,(PDATA(J,I),J=1,4),ANS
C 380         CONTINUE
C
C             AVRMAS = 0.0
C             AVRMUS = 0.0
C             IF(SUMSUM.NE.0.0)  AVRMAS = SUMMAS / SUMSUM
C             IF(SUMSUM.NE.0.0)  AVRMUS = SUMMUS / SUMSUM
C
C             WRITE(NOUT2,399) AVRMAS,AVRMUS
C             NAMEP(1)=IDENT(1,M,NOMTF)
C             NAMEP(2)=4H0000
C             CALL PACKX(NAMEP(1),3,IDENT(1,M,NOMTF),4)
C             CALL PACK (NAMEP(1),4,IDPQ(5))
C             CALL PACKX(NAMEP(2),2,MTNAME(2,NOMTF),2)
C             CALL PACKX(NAMEP(2),3,MTNAME(2,NOMTF),3)
C
C             CALL PACK (NAMEP(1),1,IDPQ(6))
C             NFILE(1)=4HFAST
C             NFILE(2)=4HU
C             CALL OVRWRT(NAMEP(1),PSEOUT,600)
C
C 399 FORMAT(//1H ,10X,'PSEUDO MASS CALCULATION RESULTS'
C    +       //1H ,20X,' AVERAGED MASS : ',E12.5,
C    +        /1H ,20X,' AVERAGED MU*S : ',E12.5)
C=======================================================================
C 398 CALL IRAMIX(AMIX  ,AMF   ,S     ,KNMAX ,NISOM,RTAKA ,SSSS  )
C     WRITE(NOUT2,399) AMIX,SSSS
C 399 FORMAT(//1H ,10X,'PSEUDO MASS CALCULATION RESULTS'
C    +       //1H ,20X,'MASS : ',E12.5,
C    +        /1H ,20X,'  S  : ',E12.5)
C     CALL  ICLEA(IA,41,0)
C     A(27)=AMIX
C     A(28)=SSSS
C     CALL OVRWRT(NAMEP(1),IA,41)
C
  400 CONTINUE
C    *********************************************************
C    *  EFFECTIVE MICROSCOPIC CROSS SECTION EDITTING         *
C    *********************************************************
      CALL  IRAPRT(MTNAME(1,NOMTF),IDENT(1,M,NOMTF),SIG0,
     &             XSECFC,ENBND,SFCTR,LXMICR(M,NOMTF),RTEMP)
C
  600 CONTINUE
C    *********************************************************
C    *  MACRO-LIB MODIFICATION                               *
C    *********************************************************
      IFLSW    = 1
      NFILE(1) = 'MACR'
      NFILE(2) = 'OWRK'
      NFILE(3) = '    '
C
      IF(IPASS.EQ.1)
     &CALL IRAOUT(MTNAME(1,NOMTF),SIGF,SIGFNU,SIGC,ENBND,
     &            XSECFC(1,1,1),XSECFC(1,1,2),SFCTR(1,1,1),SFCTR(1,1,2),
     &            XSECFC(1,2,1),XSECFC(1,2,2),SFCTR(1,2,1),SFCTR(1,2,2))
C=======================================================================
      NAMEP(1) = MTNAME(1,NOMTF)
CKSK  NAMEP(2) = 4HBMIC
      NAMEP(2) = 'BMIC'
      NFILE(1) = 'MICR'
      NFILE(2) = 'EF  '
      LENG     =  MAXNG*MAXMT3*NISOF
      CALL  OVRWRT( NAMEP , EFFMIC ,  LENG )
C    *********************************************************
C    *  IRA CALCULATION ENDED                                *
C    *********************************************************
C
      RETURN
      END
