C **********************************************************************
C                         MICREF
C **********************************************************************
      SUBROUTINE MICREF(ICOLP,MTNM,NECF,NISO,TEMP,IXMC,LOCISO,IDNT,IRES,
     *                  DN,MAR,IXR,NXR,NRR,NETOT,NRTOT,IDENT,NREG,VOLR,
CKSK *                  FLUX,WMF,WMR,WXR,VOLM,VOLX,POWTOT,MATXRG,CC)
     *                  FLUX,WMF,WMR,WXR,VOLM,VOLX,POWTOT,MATXRG,CC,
     *                  LOC11  )
C
C         PRODUCE CONDENSED EFFECTIVE MICROSCOPIC X-SECTIONS AND
C                   STORE   INTO  DDN=FT52F001
C
C     INFORMATION GIVEN
C       ICOLP  I    INDICATOR FOR COLLAPSE IF =1
C       NEF    I    NUMBER OF USER'S FINE FAST GROUPS
C       NET    I    NUMBER OF USER'S FINE THERMAL GROUPS
C       NERF   I    NUMBER OF USER'S COARSE FAST GROUPS
C       NERT   I    NUMBER OF USER'S COARSE THERMAL GROUPS
C       NECF   IA   NUMBER OF FINE FAST GROUPS IN A COARSF GROUP
C       NECT   IA   NUMBER OF FINE THERMAL GROUPS IN A COARSF GROUP
C       NMAT   I    NUMBER OF MIXTURE
C       MTNM   AA   MIXTURE NAME(A8)S
C       NISO   IA   NUMBER OF NUCLEI IN A MIXTURE
C       TEMP   AA   TEMPERATURE OF MIXTURE
C       IDNT   IA   NUCLEIDE IDENTIFICATION(A8)S
C       IRES   IA   INDEX OF RESONANCE TREATMENT
C       DN     FA   ATOMIC NUMBER DENSITY
C       IXMC   IA   INDEX TO GENERATE EFFECTIVE MICRO-X-SECTION
C       NRR    I    NUMBER OF R-REGIONS READ FROM FLUX FILE
C       IXR    NRR  X-REGION NUMBER BY R-REGION
C       NXR         MAX NUMBER OF X-REGION ; EDIT ONLY FOR NXR=1
C       MICROSCOPIC FAST X-SECTIONS ARE IN DDN(FASTU) WITH MAT.ID
C                IN 6,7 TH CHARACTERS OF NUCLEIDE ID (A8)
C       MICROSCOPIC THERMAL X-SECTIONS ARE IN DDN(THERMALU) WITH MAT.ID
C                   IN 6,7,TH CHARACTERS OF NUCLEIDE ID(AS)
C       WEIGHT FLUX  FOR MAT IN MATRIX CASEA002 OR (CASEF002+CASET002)
C       PROGRAM EXECUTE BY  M-REGION :INDEPENDENT OF X-REGION
C
      CHARACTER*4    MTNM,IDNT
      CHARACTER*4    CASEID,FILENM,NUMB,IDENT(2)
C
      COMMON /MAINC/ IOPT(52)
     1   ,NEFL     ,NETL     ,NEF      ,NET      ,NERF     ,NERT
     2   ,NMAT     ,NETL1    ,BSQ      ,NIN1     ,NIN2     ,NOUT1
     3   ,NOUT2,IT0,NEFL1    ,NEFL2    ,NEFL3    ,NEF1     ,NEF2
     4   ,NEF3     ,ISTEP    ,NSOUC    ,NFIN     ,NFOUT    ,DUMMY1(4)
     5   ,LCNEGF   ,LCNEGT   ,LCNECF   ,LCNECT   ,LCMTNM   ,LCNISO
     6   ,LCTEMP   ,LCXL     ,LCXCDC   ,LCLISO   ,LCIDNT   ,LCDN
     7   ,LCIRES   ,LCIXMC   ,NTOT  ,MAXWOK  ,IPLOT,IRANG,ICF,INITL
     8   ,CASEID(2)
C
      COMMON /PDSPDS/ BUF(540),IFLSW,FILENM(3),ECODE,TEMPRY
      COMMON /TMPSET/ STND(35),NUMB(61),NTDUMY
C
CMOD  PARAMETER   ( MXNISO = 110 , MAXNG = 107  , MAXMT3 = 6 )
      INCLUDE  'BMICRINC'
      COMMON   /MICCOM/ EFFMIC(MAXNG,MAXMT3,MXNISO),LENEFF
C
      DIMENSION       MTNM(2,1),NECF(1),NISO(1)
      DIMENSION       TEMP(1),IRES(1),DN(1),LOCISO(1)
      DIMENSION       IXMC(1),IDNT(2,1),MAR(1),IXR(1)
C
      INTEGER*4       NREG(NETOT),MATXRG(NMAT)
      REAL*4          VOLR(NRR),FLUX(NRR,NETOT)
      REAL*4          WMF(NETOT,NMAT),WMR(NRTOT,NMAT)
      REAL*4          WXR(NRTOT,NXR),VOLX(NXR),VOLM(NMAT)
      REAL*4          CC (NRTOT,MAXMT3,MXNISO,NMAT)
      REAL*4          POWTOT(NMAT,4)
C
      REAL*4          FLCELF(MAXNG)
      REAL*4          FLCELR(MAXNG)
C
C
C     START OF PROCESS
C
      IICOLP  = ICOLP
C
      IF(IOPT(19).GT.0) WRITE(NOUT2,2500)
C
      CALL   CLEA( POWTOT ,4*NMAT , 0.0 )
      CALL  ICLEA( MATXRG ,  NMAT , 0   )
C
      CALL   CLEA( FLCELF , MAXNG , 0.0 )
      CALL   CLEA( FLCELR , MAXNG , 0.0 )
C *** NREG ARRAY TO ASSOCIATE FINE GROUP TO COARSE GROUP
      IF(ICOLP.EQ.0) THEN
C ****************** NO CONDENSED GROUP STRUCTURE
                     DO 10 NG = 1,NETOT
                     NREG(NG) = NG
   10                CONTINUE
C
                     ELSE
                     K         = 0
                     DO 20 NG  = 1,NRTOT
                     DO 20 NOG = 1,NECF(NG)
                     K         = K+1
                     NREG(K)   = NG
   20                CONTINUE
                     ENDIF
C
C *** READ VOLUMES ASSOCIATED WITH R-REGION FROM FLUX FILE
C
      FILENM(1)= 'FLUX'
      FILENM(2)= '    '
C
      CALL READ(IDENT,VOLR,NRR)
C
C *** READ FLUX(R-REGION,E-GROUP)
C
      IDENT(2) = 'A002'
      IF(IOPT(79).GT.0)   CALL PACK(IDENT(2),2,NUMB(IOPT(79)))
      CALL SEARCH(IDENT,LTH,ISW)
C
      IF(ISW.EQ.1) THEN
                   IDENT(2)(1:1) = 'F'
                   CALL SEARCH(IDENT,LTH,ISW1)
                   IF(ISW1.EQ.1) GO TO 1300
                   ENDIF
C
      CALL READ(IDENT,FLUX,LTH)
      IF(ISW.EQ.0)     GO TO 60
      IF(IOPT(4).EQ.0) GO TO 60
C
      IDENT(2)(1:1) = 'T'
      CALL SEARCH(IDENT,LTH1,ISW1)
      IF(ISW1.EQ.1) GO TO 1300
      CALL READ(IDENT,FLUX(1,NEF+1),LTH1)
C
   60 CONTINUE
CDEL  IF(NXR.NE.1) GO TO 66
      CALL CLEA(WXR,NRTOT*NXR ,0.0)
      CALL CLEA(WMF,NETOT*NMAT,0.0)
      CALL CLEA(WMR,NRTOT*NMAT,0.0)
      CALL CLEA(VOLM,NMAT,0.0)
      CALL CLEA(VOLX,NXR ,0.0)
C
      TOTVOL   = 0.0
      DO 69 NR = 1,NRR
      IPOS     = IXR(NR)
      MPOS     = MAR(NR)
      IF(IPOS.GT.0) THEN
                    VOLX(IPOS) = VOLX(IPOS) + VOLR(NR)
                    TOTVOL     = TOTVOL     + VOLR(NR)
                    DO 65 NG = 1,NETOT
                    NGR      = NREG(NG)
                    WXR(NGR,IPOS) = WXR(NGR,IPOS) + FLUX(NR,NG)
CADD
                    FLCELF(NG)    = FLCELF(NG)    + FLUX(NR,NG)
                    FLCELR(NGR)   = FLCELR(NGR)   + FLUX(NR,NG)
   65               CONTINUE
                    ENDIF
      IF(MPOS.GT.0) THEN
                    MATXRG(MPOS) = IPOS
                    VOLM(MPOS) = VOLM(MPOS) + VOLR(NR)
                    DO 66 NG = 1,NETOT
                    NGR      = NREG(NG)
                    WMR(NGR,MPOS) = WMR(NGR,MPOS) + FLUX(NR,NG)
                    WMF(NG ,MPOS) = WMF(NG ,MPOS) + FLUX(NR,NG)
   66               CONTINUE
                    ENDIF
   69 CONTINUE
C
C === BEGIN LOOP OF MATERIAL
C
      LENG       = NRTOT*MAXMT3*MXNISO*NMAT
CKSK
      LTMP = LOC11 + LENG
      IF (LTMP.GT.MAXWOK) then
        WRITE(NOUT1,*) ' MEMORY SIZE IS LACKING IN MICREF ROUTINE'
        WRITE(NOUT1,*) ' REQUIRED MEMORY SIZE: ', LTMP
        STOP
      ENDIF
CKSK
      CALL  CLEA(  CC  , LENG , 0.0 )
C
      IFLSW     = 1
      FILENM(1) = 'MICR'
      FILENM(2) = 'EF  '
C
      DO 1000 NM = 1,NMAT
      IF(NISO(NM).EQ.0)     GO TO 1000
      POWTOT(NM,1) = VOLM(NM)
      IF(VOLM(NM).EQ.0)     GO TO 1000
C
C *** READ EFFECITVE 1-D CROSS SECTION FROM MICREF-FILE
C
      IDENT(1) = MTNM(1,NM)
      IDENT(2) = 'BMIC'
CDEL  IF(IOPT(79).GT.0)   CALL PACK(IDENT(2),1,NUMB(IOPT(79)))
      MMK      = NISO(NM)
      LENG     = MAXNG*MAXMT3*MMK
      CALL   READ( IDENT , EFFMIC , LENG )
C
C     BEGIN LOOP OF NUCLIDE
C
CDEL  IXMM       = 0
      LISO       = LOCISO(NM) - 1
C     IXMM: EDIT INDICATOR OF THE MIXTURE
      DO 900  NUC= 1,MMK
      LISO       = LISO+1
CDEL  IXM        = IXMC(LISO)-2*(IXMC(LISO)/2)
CDEL  TEST IF THE EFFECTIVE MICR IS REQUIRED
CDEL  IF(IXM.EQ.0)          GO TO 900
CDEL  IXMM       = IXMM + 1
C     TEST IF PSEUDO NUCLIDE
CDEL  IF(IRES(LISO)+1.EQ.0) GO TO 900
C
      POWER   =  0.0
      ABSORP  =  0.0
      CAPTUR  =  0.0
      IDENT(1) = IDNT(1,LISO)
      IDENT(2) = IDNT(2,LISO)
C
      DO 600  MT = 1 , MAXMT3
      DO 600  NG = 1 , NETOT
      N          = NREG(NG)
      CC(N,MT,NUC,NM) = CC(N,MT,NUC,NM) + EFFMIC(NG,MT,NUC)*WMF(NG,NM)
  600 CONTINUE
C
      DO 610  N  = 1 , NRTOT
      CC(N,4,NUC,NM)  = CC(N,1,NUC,NM) + CC(N,2,NUC,NM)
  610 CONTINUE
C
      DO 700  N  = 1 , NRTOT
      FSAVE      = WMR(N,NM)
C
      IF(FSAVE.NE.0.0) THEN
                       FSAVE = 1.00000/FSAVE
                       ELSE
                       WRITE(6,*) ' WARNING ZERO FLUX WAS FOUND AT ',
     @                 N,'-TH COARSE GROUP AT MICREF ROUTINE ]] '
                       FSAVE = 1.000
                       ENDIF
C
      DO 650  MT = 1 , MAXMT3
      IF(MT.EQ.1) THEN
                  CAPTUR= CAPTUR+ CC(N,MT,NUC,NM)
                  ABSORP= ABSORP+ CC(N,MT,NUC,NM)
                  ENDIF
      IF(MT.EQ.2) THEN
                  ABSORP= ABSORP+ CC(N,MT,NUC,NM)
                  POWER = POWER + CC(N,MT,NUC,NM)
                  ENDIF
      CC(N,MT,NUC,NM) = CC(N,MT,NUC,NM) * FSAVE
  650 CONTINUE
  700 CONTINUE
C
      POWER = POWER *DN(LISO)
      ABSORP= ABSORP*DN(LISO)
      CAPTUR= CAPTUR*DN(LISO)
C
      POWTOT(NM,2) = POWTOT(NM,2) + POWER
C
      IF(IDENT(1)(2:4).EQ.'U03'.OR.IDENT(1)(2:4).EQ.'U05') THEN
                     POWTOT(NM,3) = POWTOT(NM,3) + ABSORP
                     ENDIF
      IF(IDENT(1)(2:4).EQ.'PU9'.OR.IDENT(1)(2:4).EQ.'PU1') THEN
                     POWTOT(NM,3) = POWTOT(NM,3) + ABSORP
                     ENDIF
C
      IF(IDENT(1)(2:4).EQ.'TH2'.OR.IDENT(1)(2:4).EQ.'U04') THEN
                     POWTOT(NM,4) = POWTOT(NM,4) + CAPTUR
                     ENDIF
      IF(IDENT(1)(2:4).EQ.'U08'.OR.IDENT(1)(2:4).EQ.'PU0') THEN
                     POWTOT(NM,4) = POWTOT(NM,4) + CAPTUR
                     ENDIF
C
      IF(IDENT(1)(2:4).EQ.'PA3') THEN
                     POWTOT(NM,4) = POWTOT(NM,4) - ABSORP
                     ENDIF
C
      IF(IOPT(19).GT.0) THEN
                        WRITE(NOUT2,2400) IDENT,MTNM(1,NM),MTNM(2,NM),
     @                  VOLM(NM),DN(LISO),POWER,ABSORP,CAPTUR
                        ENDIF
  900 CONTINUE
C
C **  WRITE SPECTRUM OF MIXTURE IN CONDENSED GROUP STRUCTURE INTO FLUX F
 1000 CONTINUE
C
C     SET OF ISOLATED MATERIAL X-SECTION
C
      DO 2000 NM = 1,NMAT
      IF(NISO(NM).EQ.0)     GO TO 2000
      IF(VOLM(NM).GT.0.0)   GO TO 2000
C
C *** READ EFFECITVE 1-D CROSS SECTION FROM MICREF-FILE
C
      IDENT(1) = MTNM(1,NM)
      IDENT(2) = 'BMIC'
CDEL  IF(IOPT(79).GT.0)   CALL PACK(IDENT(2),1,NUMB(IOPT(79)))
      MMK      = NISO(NM)
      LENG     = MAXNG*MAXMT3*MMK
      CALL   READ( IDENT , EFFMIC , LENG )
C
C     BEGIN LOOP OF NUCLIDE
C
      DO 1800 NUC= 1,MMK
      DO 1600 MT = 1 , MAXMT3
      DO 1600 NG = 1 , NETOT
      N          = NREG(NG)
      CC(N,MT,NUC,NM)=CC(N,MT,NUC,NM) + EFFMIC(NG,MT,NUC)*FLCELF(NG)
 1600 CONTINUE
C
      DO 1650 N  = 1 , NRTOT
      CC(N,4,NUC,NM)  = CC(N,1,NUC,NM) + CC(N,2,NUC,NM)
 1650 CONTINUE
C
      DO 1700 N  = 1 , NRTOT
      FSAVE      = FLCELR(N)
      IF(FSAVE.NE.0.0) THEN
                       FSAVE = 1.00000/FSAVE
                       ENDIF
      DO 1700 MT = 1 , MAXMT3
      CC(N,MT,NUC,NM) = CC(N,MT,NUC,NM) * FSAVE
 1700 CONTINUE
 1800 CONTINUE
      VSAVE      = TOTVOL
      IF(VSAVE.GT.0.0) THEN
                       VSAVE = 1.00000/VSAVE
                       ELSE
                       VSAVE = 1.00000
                       ENDIF
      DO 1850 N  = 1 , NETOT
      WMF(N,NM)  = FLCELF(N)*VSAVE
 1850 CONTINUE
      DO 1900 N  = 1 , NRTOT
      WMR(N,NM)  = FLCELR(N)*VSAVE
 1900 CONTINUE
      VOLM(NM)   = 1.000
 2000 CONTINUE
C
C
C     END OF MATERIAL LOOP
C
      WRITE(NOUT2,3500)
      SUMPOW = 0.0
      SUMVOL = 0.0
      SUMABS = 0.0
      SUMCAP = 0.0
C
      DO 3000 NM = 1 , NMAT
      IF(POWTOT(NM,2).LE.0.0)  GO TO 3000
      CRATIO     = 0.0
      IF(POWTOT(NM,3).GT.0.0) CRATIO = POWTOT(NM,4)/POWTOT(NM,3)
      WRITE(NOUT2,3400) MTNM(1,NM),MTNM(2,NM),
     *                  POWTOT(NM,1),POWTOT(NM,2),
     *                  POWTOT(NM,3),POWTOT(NM,4),CRATIO
      SUMPOW = SUMPOW + POWTOT(NM,2)
      SUMVOL = SUMVOL + POWTOT(NM,1)
      SUMABS = SUMABS + POWTOT(NM,3)
      SUMCAP = SUMCAP + POWTOT(NM,4)
 3000 CONTINUE
      CRATIO     = 0.0
      IF(SUMABS.GT.0.0) CRATIO = SUMCAP/SUMABS
      WRITE(NOUT2,3400) ' TOT','AL  ',SUMVOL,SUMPOW,SUMABS,SUMCAP,CRATIO
      WRITE(NOUT2,*)
C
C *** OUTPUT X-SECTION TO FT52F001
C
      REWIND 52
      WRITE(52)  NMAT,NXR,NRTOT
      WRITE(52) (NISO(I),I=1,NMAT),
     @          ((WMR(J,I),J=1,NRTOT),I=1,NMAT),
     @          (MATXRG(I),I=1,NMAT),
     @          (VOLM(I),I=1,NMAT),(VOLX(I),I=1,NXR),
     @          ((WXR(J,I),J=1,NRTOT),I=1,NXR)
      WRITE(52)  CC
C
      IF(IOPT(18).GT.0) THEN
                        DO 4000 M= 1 , NMAT
                        MMK      = NISO(M)
                        IF(NISO(M).GT.0) THEN
                              WRITE(52)  (((CC(I,J,K,M),I=1,NRTOT),
     @                                     J=1,MAXMT3),K=1,MMK)
                                         ENDIF
 4000                   CONTINUE
                        ENDIF
      REWIND 52
C
C     END OF PROCESS
C
      RETURN
C
C *** ERROR MESSAGE
C
C
 1300 WRITE(NOUT1,2300) IDENT
      RETURN
C
 2300 FORMAT(' *** FINE SPECTRUM ',2A4,' IN FLUX FILE NOT FOUND')
 2400 FORMAT(/10X, 2A4,2X,   2A4,3X,1P5E12.5)
 2500 FORMAT(/10X,'MEMBER  IN MIXTURE    OF VOLUME   BY  DENSITY',
     *'  REL POWER  ABSORPTION  CAPTURE ')
C
C3400 FORMAT(/10X, 2A4,2X,1P5E12.5)
 3400 FORMAT( 10X, 2A4,2X,1P5E12.5)
 3500 FORMAT(/15X,' MATERIAL-WISE POWER DISTRIBUTION ',
     *      //10X,'MTNAME',4X,' VOLUME      RELATIVE    ',
     *             'FISSILE     FERTILE     CONVERSION'
     *       /10X,'NAME  ',4X,'  (CC)       POWER       ',
     *             'ABSORPTION  CAPTURE     RATIO',/)
C
      END
