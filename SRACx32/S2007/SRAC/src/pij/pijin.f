      SUBROUTINE  PIJIN
C
CDEL  PARAMETER    ( MEMPIJ = 10000 )
CDEL  PARAMETER    ( MXMESH =   200 )
CDEL  PARAMETER    ( MXNTAB =  2000 )
CDEL  PARAMETER    ( MXDBHT =   450 )
      INCLUDE  'PIJPMINC'
      INCLUDE  'DBHETINC'
C
      COMMON / PIJ1C / NX,NY,NTPIN,NAPIN,NCELL,NM,NGR,NGA,NDPIN,IDIVP,
     1                 BETM,NX1,NY1,LOCAL(3),IXP,IYP,IZP,NDPIN1,
     2                 NDR,NDA,LL,L0,RO1,DRO,FVOL,LOCAL1,LOCAL2,RR,
     3                 ANG,LOCAL3,LOCAL4,SINB,COSB,LOCAL5
      COMMON / PIJ2C /
     1         IGT,NZ,NR,NRR,NXR,IBOUND,IDRECT,LCOUNT,IEDPIJ,IFORM,
     2         NTTAB,NUTAB,SZ,ITYPE,NGLAST,LOCAL6(3),
     3         IEDFLX,ITMINN,ITMOUT,ITBG,LCMX,ITDM,IPT,
     4         EPSI,EPSO,EPSG,RELC,OVERX,FACTOR,ICOOD,MM,NO1,
     4         IVP,NO2(4),
     5         LCMMR,LCNREG,LCIRR,LCIXR,LCMAR,LCMAT,LCVOL,
     6         LCVOLR,LCVOLX,LCVOLM,LCMATD,AA(MEMPIJ)
CM   6         LCVOLR,LCVOLX,LCVOLM,LCMATD,AA(1200)
      COMMON /DOUBLE/
     1  IDB   ,IGEOM ,METHOD,NTRD  ,NRRD  ,NMD   ,MDNO  ,DRF   ,DRM   ,
     2  DXL   ,DVF   ,DVM   ,DVCELL,RHO   ,GAMMA ,IDB016(5),
     3  LDIA  ,LDIB  ,LDMMRD,LDIRRD,LDIXRD,LDMARD,LDMTTD,LDMTDD,LDVLTD,
     4  LDVLRD,LDVLMD,LDIC  ,LDLAST,IDB034(17)   ,BB(MXDBHT)
CKSK 4  LDVLRD,LDVLMD,LDIC  ,LDLAST,IDB034(17)   ,BB(450)
      COMMON / PIJC / L00,L01,L02,L03,L04,L05,L06,L07,L08,L09,
     1                L10,L11,L12,L13,L14,L15,L16,L17,L18,L19,L20,L21,
     2                L22,L23,L24,L25,L26,L27,L28,L29,L30,L31,L32,L33,
     3                L34,L35,DD(65)
      COMMON  /MAINC/IDUM0(63),NOUT1,NOUT2,IT0,IDUM67(29),MXDIM,
     1   IOPEN,IDUM97(3),IDCASE(2),TITLE(18),DUM2(880)
C    1   IDUM97(4),IDCASE(2),TITLE(18),DUM2(880)
      COMMON /WORK/A(60000)
C-----ADDED BY JAIS K.KANEKO  6/14/84 ----------------------------------
CKSK  COMMON /PLTCNT/ JJPLT
      COMMON /IGEOM/
     * NTPINZ, NGRZM ,NGRZP,
     * NGRM  , NGRP  , PITCH,
     * XLENG , NPINRT, NPINZ
C
C    *******************************************************************
C    *******************************************************************
C-----END END END END END END END --------------------------------------
      DIMENSION IA(18),IIAA(950),IPL(3)
CKO *** ADD IGT=15,16 ****
      CHARACTER*4   TITL12(18,16),TITLE1(18,7),TITLE2(18,9)
C
      EQUIVALENCE(TITL12(1,1),TITLE1(1,1)),(TITL12(1,8),TITLE2(1,1))
      EQUIVALENCE(AA(1),IIAA(1))
      COMMON /ABC11/AK(5500),BK(5500),CK(5500)
      REAL*8 AK,BK,CK
C
      DATA  TITLE1/
     1'    ','( SP','HERE',' )  ','    ','    ','    ','    ','    ',
     2'    ','    ','    ','    ','    ','    ','    ','    ','    ',
     3'    ','( IN','FINI','TE S','LAB ',')   ','    ','    ','    ',
     4'    ','    ','    ','    ','    ','    ','    ','    ','    ',
     5'    ','( CI','RCUL','AR C','YLIN','DER ',')   ','    ','    ',
     6'    ','    ','    ','    ','    ','    ','    ','    ','    ',
     7'    ','( SQ','UARE',' CYL','INDE','R ) ','    ','    ','    ',
     8'    ','    ','    ','    ','    ','    ','    ','    ','    ',
     9'    ','( TW','O DI','MENS','IONA','L SQ','UARE',' CYL','INDE',
     A'R ) ','    ','    ','    ','    ','    ','    ','    ','    ',
     B'    ','( HE','XAGO','NAL ','CYLI','NDER',' )  ','    ','    ',
     C'    ','    ','    ','    ','    ','    ','    ','    ','    ',
     D'    ','( TW','O DI','MENS','IONA','L HE','XAGO','NAL ','CYLI',
     E'NDER',' )  ','    ','    ','    ','    ','    ','    ','    '/
      DATA  TITLE2/
     1'    ','( OC','TANT',' SYM','METR','IC P','ILLA','R ) ','    ',
     2'    ','    ','    ','    ','    ','    ','    ','    ','    ',
     3'    ','( OC','TANT',' SYM','METR','IC P','ILLA','R WI','TH R',
     4'EGUL','AR A','RRAY',' OF ','PIN ','RODS',' )  ','    ','    ',
     5'    ','( AN','NULA','R AS','SEMB','LY W','ITH ','REGU','LAR ',
     6'ARRA','YS O','F PI','N RO','DS )','    ','    ','    ','    ',
     7'    ','( AN','NULA','R AS','SEMB','LY W','ITH ','ASSY','MMET',
     8'RIC ','PIN ','RODS',' )  ','    ','    ','    ','    ','    ',
     9'    ','( HE','XAGO','NAL ','ASSE','MBLY',' WIT','H AS','YMME',
     A'TRIC',' PIN',' ROD','S ) ','    ','    ','    ','    ','    ',
     B'    ','( X ','Y TW','O DI','MENS','IONA','L CE','LL W','ITH ',
     C'OR W','ITHO','OT P','IN R','ODS ','ON G','RID ',')   ','    ',
     D'    ','( MU','LTI-','LAYE','R OF',' HEX','AGON',' WIT','H HE',
     E'XAGO','NAL ','ARRA','Y OF',' PIN',' ROD','S ) ','    ','    ',
     F'    ','( HE','XAGO','NAL ','ASSE','MBLY',' WIT','H TR','IANG',
     G'ULAR',' PIN',' ROD',' ARR','ANGE','MENT',' )  ','    ','    ',
     H'    ','( X ','Y TW','O DI','MENS','IONA','L CE','LL W','ITH ',
     I'OR W','ITHO','OT P','IN R','ODS ','ON G','RID ',')   ','    '/
C
C     ICOOD : COORDINATE SYSTEM INDICATOR USED IN DIRECTIONAL PROB.
      ICOOD=1
C     CONTROL INDEX FOR DOUBLY HETEROGENEOUS TREATMENT : IDB
      IDB=0
C      FVOL : FACTOR MULTIPLIED TO THE VOLUMES NUMERICALLY INTEGRATED
C      FOR COMPARISON WITH ANALYTIC ONES
      REWIND 85
      READ(85)AK
      READ(85)BK
      READ(85)CK
CM    DO 10 I=1,1200
CM    AA(I)=0.
CM 10 CONTINUE
C
      CALL CLEA(  AA , MEMPIJ , 0.0 )
      NGLAST = 0
      CALL REAM (IA,IA,IA,0,18,0)
C
      IGT    = IA(1)
      NZ     = IA(2)
      NR     = IA(3)
C     NVR    = IA(3)
      NRR    = IA(4)
      NXR    = IA(5)
      IBOUND = IA(6)
      IDRECT = 1
      IF (IDUM0(17).EQ.3) IDRECT=2
CADDED
      NTTAB = MXNTAB
C      TITLE WRITE
      WRITE(NOUT2,6100) IDCASE,(TITLE(I),I=1,18)
CKO   WRITE(NOUT2,6110)IGT,(TITL12(I,IGT),I=1,16),NZ,NR,NRR,NXR,
      WRITE(NOUT2,6110)IGT,(TITL12(I,IGT),I=1,17),NZ,NR,NRR,NXR,
     1               IBOUND,IDRECT
C
C
      NX      = IA(7)
C     NRXZ    = IA(7)
      NY      = IA(8)
C     NTYZ    = IA(8)
      NTPIN   = IA(9)
      NAPIN   = IA(10)
CM    JRPH    = IA(10)
      NCELL   = IA(11)
      IEDPIJ  = IA(12)
      NGR     = IA(13)
      NDA     = IA(14)
      NDPIN   = IA(15)
      IDIVP   = IA(16)
      IBETM   = IA(17)
      IPLOPT  = IA(18)
C
      WRITE ( NOUT2,6115) NX , NY  , NTPIN, NAPIN,NCELL   , IEDPIJ
     1              , NGR, NDA , NDPIN, IDIVP    , IBETM
     2              ,IPLOPT
C
CJAIS MODIFIED FOR UNRESOLVED IOPT VARIABLE  ***4/10/1985***
CDEL  IF(NRR.GT.20.AND. IABS(IDUM0(5)).EQ.2 ) GO TO 6020
C
CMOD  IF(NRR.GT.100)   GO TO 6025
      IF(NR.GT.MXMESH) GO TO 6025
CEND
CJAIS END
      IF(NCELL.LT.0) THEN
                 IVP=1
                      ELSE
                 IVP=0
                      ENDIF
      NCELL   = IABS(NCELL)
      IF(IABS(IBOUND).EQ.1 ) THEN
      IF(NCELL .GE.5) WRITE(NOUT1,6135) NCELL
      IF(NCELL .GT.1) GO TO 20
      IF(NCELL .LE.1) WRITE(NOUT1,6140) NCELL
      STOP
                             ELSE
                     NCELL=1
                             ENDIF
   20 CALL REAM(TITLE,IEDFLX,EPSI,0,7,6)
      PI=3.141593
      BETM=FLOAT(IBETM)*PI/180.
      NX1 = NX + 1
      NDPIN1 = NDPIN + 1
      NY1=NY+1
CDEL  NTTAB = 300
CDEL  NTTAB = 500
C
      NUTAB  = NTTAB * NCELL
      LCNREG = 1
      LCIRR  = LCNREG+ NZ
      LCIXR  = LCIRR + NR
      LCMAR  = LCIXR + NRR
      LCMAT  = LCMAR + NRR
      LCVOL  = LCMAT + NR
      LCVOLR = LCVOL + NR
      IF(NR.EQ.NRR) LCVOLR=LCVOL
      LCVOLX = LCVOLR+ NRR
      IF(NRR.EQ.NXR) LCVOLX=LCVOLR
      LCMMR  = LCVOLX+ NXR
      LCMATD = LCMMR + NRR
CM    IF(LCMATD.GT.1200) THEN
      IF(LCMATD.GT.MEMPIJ) THEN
                           WRITE(6,*) ' (PIJIN) LCMATD=',LCMATD
                           STOP
                           ENDIF
C
          L01=1
C 1   RX  BY L01
          L02 = L01 + NX1+NAPIN
      IF(IGT.EQ.11.OR.IGT.EQ.12) L02 = L01 + NX1 + NTPIN
C 2   RPP BY L02
          L03 = L02 + MAX0(NTPIN,NAPIN)
C 3   RDP BY L03
          L04 = L03 + NDPIN1
CKO   IF(IGT.GE.11 .AND. IGT.LE.13 .OR. IGT.EQ.15)
      IF(IGT.GE. 9)
CMOD *    L04 = L03 + NDPIN1*MAX0(NTPIN,NAPIN)
     *    L04 = L03 + NDPIN1*MAX0(NTPIN,NAPIN**2)
C 4   NPIN  BY L04
          L05 = L04 + 2*MAX0(NTPIN,NAPIN)
C 5   THETA BY L05
          L06 = L05 + MAX0(NTPIN,NAPIN)+1
C         TY(0) MAY BE USED
C 6   TY    BY L06
          L07 = L06
          L08 = L07 + NY1
C 8   D     BY L08
          L09 = L08 + NTTAB
C 9   IM    BY L09
          L10 = L09 + NTTAB
C 10  IP    BY L10
          L11 = L10 + NTTAB
C 11  II    BY L11
          L12 = L11 + NUTAB
C 12  XX    BY L12
          L14 = L12 + NUTAB
C 14  S     BY L14
          L25 = L14 + NR
C 25  XX    BY L25
          L26 = L25 + NUTAB
C 26  III   BY L26
          L27 = L26 + NUTAB
C 27  L27 SHALL BE IN USED IN THE LOCAL ROUTINES
      CALL PIJ1(A(L01),A(L02),A(L03),A(L04),A(L05),A(L06),
     1         AA(LCNREG),AA(LCIRR),AA(LCIXR)
     2         ,AA(LCMAT),AA(LCMAR),AA(LCMATD),AA(LCMMR),BB(1))
      IF(IGT.NE.15) THEN
               CALL CHECK(A(L01),A(L06),A(L03),A(L02),A(L04),A(L05),ISW)
                    ENDIF
      LCVOLM=LCMATD+NM
      IF(NM.EQ.NRR) LCVOLM=LCVOLR
CM    IF (LCVOLM.GT.1200) THEN
      IF (LCVOLM.GT.MEMPIJ) THEN
                            WRITE(6,*) ' LCVOLM+??=',LCVOLM
                            STOP
                            ENDIF
C
      IF(ISW.NE.0) STOP
C
C ** CALL PARTICULAR GEOMETRY ROUTINES
   40 IF (IGT.LE.0)  GO TO 6030
      IF (IGT.GE.8)  GO TO 50
C     PATH   FOR IGT=1,2,3,4,5,6,7
      CALL PATH
      GO TO 100
   50 CONTINUE
      IF(IGT.GE.10) GO TO 60
C     CLUP77 FOR IGT=8,9
      CALL CLUP77
      GO TO 100
   60 CONTINUE
      IF(IGT.GE.11) GO TO 70
C     CLUP   FOR IGT=10
      CALL CLUP
      GO TO 100
   70 CONTINUE
      IF(IGT.GE.13) GO TO 80
C     CLUPH  FOR IGT=11,12
      CALL CLUPH
      GO TO 100
   80 CONTINUE
      IF(IGT.GE.14)  GO TO 90
C     PATHXY FOR IGT=13
      CALL PATHXY
      GO TO 100
   90 IF(IGT.GT.14) GO TO 95
C     PATHHH FOR IGT=14
      CALL PATHHH
      GO TO 100
   95 CONTINUE
CKO   IF(IGT.GT.15) GO TO 6030
      IF(IGT.GT.15) GO TO 97
C     PATHHX FOR IGT=15
      CALL PATHHX(A(L01),A(L06),A(L04),A(L04+NTPIN),A(L02),A(L05))
CKO*******ADD IGT=16
      GO TO 100
   97 CONTINUE
      IF(IGT.GT.16) GO TO 6030
C     PATHXY FOR IGT=16
      CALL PATHXY
CKO*******
  100 CONTINUE
C  **** CALL PLOTER ROUTINE ****
      IF ( IPLOPT .EQ. 0 )  GO TO 55
C  **** ADDED BY K.KANEKO 6/14/84 *****
CK       IF(IOPEN.GT.0)   THEN
CK                        CALL PLOT (0.0,0.0,999)
CK                        ENDIF
CK       CALL PLOTS (DUMMY,DUMMY)
C  **** END ************* 6/14/84 *****
CKSK FOR UNIX VERSION(PIFLIB) 2/22/96 BY K.OKUMURA
         IF(IOPEN.EQ.0) THEN
           CALL PLOTS(0,0)
           IOPEN = 1
         ELSE
           CALL PLOT(0.0,0.0,666)
         ENDIF
CKSK **********************************
         NFIG = 0
   30       CALL REAM(IA,IPL,IA,0,3,0)
CKO****
CKSK        IF ( IGT.EQ.15 )  THEN
CKSK          WRITE (NOUT1,*) ' PLOTER OPTION WAS SKIPPED, BECAUSE',
CKSK &        ' PLOTER ROUTINE FOR IGT=15 IS NOT INSTALLED YET'
CKSK          GO TO 32
CKSK        ENDIF
CKO****
            CALL GEOMTY (TITLE,NFIG,IPL(1),IPL(2),IERR,
     1               A(L01),A(L02),A(L03),A(L04),A(L05),A(L06),
     2               NX,NY,NTPIN,NAPIN,NDPIN,IDIVP,IGT,NZ,
     3               AA(LCNREG),AA(LCIRR),AA(LCIXR),AA(LCMAR))
            NFIG = 10
            WRITE(NOUT1,6145) IERR
   32       IF( IPL(3) .EQ. 0 ) GO TO 30
C  **** ADDED BY K.KANEKO 6/14/84 *****
CKSK     CALL PLOT (0.0,0.0,999)
CKSK     IOPEN = 0
CKSK     JJPLT = 0
C  **** END ************* 6/14/84 *****
   55 IF(NMD.EQ.0)  RETURN
C
C     ARRAY ALLOCATION FOR DOUBLE HETEROGENEITY
C
      LDIA  =        1
      LDIB  =LDIA   + NRR
      LDMMRD=LDIB   + NRRD
      LDIRRD=LDMMRD + NRRD
      LDIXRD=LDIRRD + NTRD
      LDMARD=LDIXRD + NRRD
      LDMTTD=LDMARD + NRRD
      LDMTDD=LDMTTD + NTRD
      LDVLTD=LDMTDD + NMD
      LDVLRD=LDVLTD + NTRD
      LDVLMD=LDVLRD + NRRD
      LDIC  =LDVLMD + NMD
      LDLAST=LDIC   + NR
CKSK  IF(LDLAST.LT.450) GO TO 110
CKSK  WRITE(NOUT1,6150) LDLAST
      IF(LDLAST.LT.MXDBHT) GO TO 110
      WRITE(NOUT1,6150) LDLAST, MXDBHT
      STOP
C
 110  CALL PIJ1D(
     1          BB(LDIA),BB(LDIB),BB(LDIC)
     2         ,AA(LCIRR),AA(LCIXR)
     3         ,AA(LCMAT),AA(LCMAR),AA(LCMATD),AA(LCMMR)
     4         ,AA(LCVOL),AA(LCVOLR),AA(LCVOLM)
     5         ,BB(LDIRRD),BB(LDIXRD)
     6         ,BB(LDMTTD),BB(LDMARD),BB(LDMTDD),BB(LDMMRD)
     7         ,BB(LDVLTD),BB(LDVLRD),BB(LDVLMD)  )
C
      RETURN
CDEL
C6020 WRITE(NOUT1,6120) NRR
CDEL  STOP
 6025 WRITE(NOUT1,6125) NRR,MXMESH
      STOP
 6030 WRITE(NOUT1,6130) IGT
      STOP
 6100 FORMAT (1H1,2A4,18A4,'*** STEP *** INPUT FOR PIJ *** ')
CKO***** 16A4=> 17A4 ***
 6110 FORMAT(1H ,20X, 40HGEOMETRY TYPE                           ,I3
     *       ,1X,17A4
     1      /1H ,20X, 40HNUMBER OF SUB - REGIONS                 ,I3
     2      /1H ,20X, 40HNUMBER OF  T  - REGIONS                 ,I3
     3      /1H ,20X, 40HNUMBER OF  R  - REGIONS                 ,I3
     4      /1H ,20X, 40HNUMBER OF  X  - REGIONS                 ,I3
     6      /1H ,20X, 40HOUTER BOUNDARY CONDITION (-1,0,1,2)     ,I3
     7      /1H ,20X, 40HDIRECTIONAL PIJ (1,2)                   ,I3
     8          ,'  INDICATED BY IC17')
 6115 FORMAT(1H ,20X, 40HNUMBER OF R OR X MESH                   ,I3
     1      /1H ,20X, 40HNUMBER OF THETA OR Y MESH               ,I3
     2      /1H ,20X, 40HTOTAL NUMBER OF PIN RODS                ,I3
     3      /1H ,20X, 40HNUMBER OF RINGS OF PIN ROD ARRAY        ,I3
     4      /1H ,20X, 40HNUMBER OF LATTICE CELLS TRACED          ,I3
     5      /1H ,20X, 40HPRINT CONTROL OF PIJ (0,1)              ,I3
     6      /1H ,20X, 40HORDER OF GAUSS RADIAL INTEGRATION       ,I3
     8      /1H ,20X, 40HNO. OF DIVISION FOR ANGULAR INTEGRATION ,I3
     9      /1H ,20X, 40HNUMBER OF ANNULAR DIVISION IN A PIN ROD ,I3
     A      /1H ,20X, 40HDIVISION BY RPP  (0,1,2)                ,I3
     C      /1H ,20X, 40HANGLE RANGE BY DEGREE                   ,I3
     E      /1H ,20X, 40HPLOTER OPTION  (0,1)                    ,I3 )
 6120 FORMAT(' *** NUMBER OF R-REGIONS ',I6,' EXCEEDS THE LIMIT OF 20'
     *      ,' WHEN PEACO ROUTINE IS USED')
 6130 FORMAT(' *** ILLEGAL GEOMETRY SPECIFIED : IGT=',I3,
     * ' ---> ERROR STOP ')
C6125 FORMAT(' *** NUMBER OF R-REGIONS ',I6,' EXCEEDS THE LIMIT OF 100'
CM   *      ,' EVEN IF PEACO ROUTINE IS NOT USED')
 6125 FORMAT(' *** NUMBER OF T-REGIONS ',I6,' EXCEEDS THE LIMIT OF ',I6
     *      ,' EVEN IF PEACO ROUTINE IS NOT USED')
 6135 FORMAT(5X,'*** TIME CONSUMING DUE TO TOO MANY NBR OF CELLS TRACED'
     *         ,' PER NEUTRON ',I2)
 6140 FORMAT(5X,'*** NBR OF CELLS TRACED PER NEUTRON IS TOO SMALL ',I2)
 6145 FORMAT(5X,' ** PLOTER ROUTINE ERRER CODE= ',I4,' **')
C6150 FORMAT('0 **** DIMENSION OVER FOR COMMON DOUBLE ',I7)
 6150 FORMAT(//1H ,'<<<  ERROR STOP (PIJIN )  >>>',/,1X,
     & ' REQUIRED WORK AREA FOR DOUBLE HETEROGENEOUS OPTION GREATER ',/,
     & ' THAN DIMENSION SIZE : ',I7,' > ',I7,/,
     & ' CHANGE THE PARAMETER VALUE IN INCLUDE STATEMENT')
      END
