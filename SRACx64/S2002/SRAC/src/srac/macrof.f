C***********************************************************************
C   MACROF  : CONTROL ROUTINE FOR FAST MACRO. X-SECTION CALCULATION
C***********************************************************************
      SUBROUTINE  MACROF
C
      DOUBLE PRECISION  JFASTL,FFASTL,JMACRO,FMACRO
C
      COMMON /MAINC/  IOPT(20),JNFSTL(2),FNFSTL(2),JNTHEL(2),FNTHEL(2)
     1    ,JNEFST(2),FNEFST(2),JNETHE(2),FNETHE(2),JNMACR(2),FNMACR(2)
     2    ,JNMCRS(2),FNMCRS(2),JNEMIC(2),FNEMIC(2),JNFLUX(2),FNFLUX(2)
     3    ,NEFL     ,NETL     ,NEF      ,NETX     ,NERF     ,NERT
     4    ,NMAT     ,NETL1    ,BSQ,NIN1 ,NIN2     ,NOUT1    ,NOUT2
     5    ,IT0      ,NBFL(3)  ,NBF(3)   ,DUMMY1(8)
     6    ,LCNEGF   ,LCNEGT   ,LCNECF   ,LCNECT   ,LCMTNM   ,LCNISO
     7    ,LCTEMP   ,LCXL     ,LCXCDC   ,LCLISO   ,LCIDNT   ,LCDN
     8    ,LCIRES   ,LCIXMC   ,IFTOT    ,MEMOT    ,IOPEN    ,IRANGE
     9    ,ICF      ,IDUMY0
     A    ,CASEID(2),TITLE(18)
     B    ,AA(380)
C
      COMMONM /WORK  / A(1)
      COMMON  /PIJ2C / IPAA(1000)
      COMMON  /DOUBLE/ LDOUBL(50)
C
      COMMON  /MAFCNL/ KOPT(20),JFASTL,FFASTL,JMACRO,FMACRO,IMAX,IGT,
     1                 KNMAT,KNMAX,MXMTX,MXTEMP,MXSIG0,LNMAX,IDS,IMAX1,
     2                 MXREAC,LOUT1,LOUT2,NORES,NMP,NISOHM,ISNCAL,IPLMAX
     3                ,IGMAX,NET,NET5
      COMMON  /MAFDBL/ MTREPL,MICFL,MICMOD,IPATH,SXLL,SBELL,SVF,SVM,
     1                 SDAN,SGAMMA,IGEOMS
      COMMON  /MAFSX1/  J1, J2, J3, J4, J5, J6, J7, J8, J9,J10,
     1                 J11,J12,J13,J14,J15,J16,J17,J18,J19,J20,
     2                 J21,J22,J23,J24,J25,J26,J27,J28,J29,J30,
     3                 J31,J32,J33,J34,J35,J36,J37,J38,J39,J40,
     4                 J41,J42,J43,J44,J45,J46,J47,J48,J49,J50,
     5                 J51,J52,J53,J54,J55,J56,J57,J58,J59,J60,
     6                 J61,J62,J63,J64,J65,J66,J67,J68,J69,J70,
     6                 J71,J72,J73,J74,J75,J76,J77,J78,J79,J80,
     7                 J81,J82,MAXX
C
      DIMENSION        NN(120),IA(380),LOCM(82)
C
      EQUIVALENCE     (IOPT(1),NN(1)),(AA(1),IA(1))
      EQUIVALENCE     (LOCM(1),  J1 )
C
C     INITIAL SET
C
      MEMORY = IOPT(96)
C
      MXMTX  = 4
      MXTEMP = 4
      MXSIG0 = 8
      LNMAX  = MAX0(MXTEMP,MXSIG0)
      IGT    = IPAA(1)
      IMAX   = NEF
      NET    = NETX
      KNMAT  = NMAT
      IDS    = IMAX + NET
      NET5   = NET  + 5
      IGMAX  = NEF  + NET
      IF(IDS.GT.107)  IDS=107
      IMAX1  = IMAX+1
      MXREAC = 5
      LOUT1  = NOUT1
      LOUT2  = NOUT2
      IPATH  = 0
      NMP    = 0
      LOCMAX = 83
C
      IF(IOPT(1).NE.0.AND.(LDOUBL(1).EQ.1.OR.LDOUBL(1).EQ.3)) IPATH=1
      IF(IOPT(1).NE.0) NMP = IPAA(33)
C
      CALL  CLEA( A    , MEMORY , 0.0 )
      CALL ICLEA( LOCM , LOCMAX ,   0 )
C
      KNMAX   = 0
      DO 10 K = 1,KNMAT
      ISW     = IA(LCNISO+K-1)
      IF(ISW.GT.KNMAX)  KNMAX = ISW
   10 CONTINUE
C
      CALL ENCOD1(JFASTL, NN(29),NN(30))
      CALL ENCOD1(FFASTL, NN(31),NN(32))
      CALL ENCOD1(JMACRO, NN(37),NN(38))
      CALL ENCOD1(FMACRO, NN(39),NN(40))
C
      DO 20 I = 1,20
   20 KOPT(I) = NN(I)
C
      ISNCAL  = 0
      IF(IOPT( 2).EQ.2.OR.IOPT( 2).EQ.3)  ISNCAL = 1
      IF(IOPT(12).EQ.2.OR.IOPT(12).EQ.3)  ISNCAL = 1
      IPLMAX  = 1
      IF(ISNCAL.EQ.1) IPLMAX = NN(39)
C
C     MEMORY LOCATION FOR VARIABLE DIMENSION FOR FIRST STEP
C
      J1  =  1
      J2  = J1 + KNMAT*2
      J3  = J2 + KNMAT
      J4  = J3 + KNMAT
      J5  = J4 + KNMAT
      J6  = J5 + KNMAT
      J7  = J6 + KNMAT
      J8  = J7 + KNMAT*KNMAX*2
      J9  = J8 + KNMAT*KNMAX
      J10 = J9 + KNMAT*KNMAX
      J11 =J10 + KNMAT*KNMAX
      J12 =J11 + KNMAT*KNMAX
      J13 =J12 + KNMAT
      J14 =J13 + NMP
      J15 =J14 + KNMAT
      J16 =J15 + KNMAT*2
      J17 =J16 + KNMAT*IGMAX
      J18 =J17 + KNMAT*IGMAX*KNMAX
      J19 =J18 + NET*NET5
      J20 =J19 + KNMAT*KNMAX
C
      CALL MAFDAT(A( J1),A( J2),A( J3),A( J4),A( J5),A( J6),A(J7),
     1            A( J8),A( J9),A(J10),A(J11),A(J13),A(J15),A(J20))
C
      IF ( KNMAX .EQ. 0 )  RETURN
C
      ISKIP  = 0
*     WRITE(6,*) ' **  NOW  MAFFLX **** '
C
      CALL  MAFFLX(A( J1),A( J2),A( J3),A( J6),A( J7),
     1             A( J8),A( J9),A(J15),A(J16),A(J17),
     2             A(J18),A(J19),ISKIP)
C
      IF(ISKIP.EQ.1) GO TO 999
C
      J21 = J20 + NORES
      J22 = J21 + NORES*KNMAT
      J23 = J22 + NORES*KNMAT
      J24 = J23 + NORES*KNMAT
      J25 = J24 + NORES*KNMAT
C
      CALL MAFPRE(A(J2),A(J3),A(J7),A(J8),A(J12),A(J13),A(J14),A(J25))
C
C     CALCULATE MEMORY LOCATION FOR FINAL STEP
C
      CALL MAFSFX(LOCM,LOCMAX)
C
      IOPT(80) = MAXX
      MAXY     = MEMORY - MAXX
      IF(MAXY.LT.0) THEN
                    WRITE(NOUT2,1001)  MEMORY,MAXX,MAXY
                    MAXY = MAXX - MEMORY
                    WRITE(NOUT1,1002) MAXY
                    STOP
                    ENDIF
C
C     CALL  MAFSST --- SET TOTAL X-SECTION FOR HOMO. CELL
C
*     WRITE(6,*) ' **  NOW  MAFSST **** '
      CALL MAFSST( A( J1),A( J2),A( J6),A( J7),A( J8),A( J9),
     1             A(J14),A(J16),A(J17),A(J19),A(J25),A(J26),
     2             A(J27),A(J28),A(J29),A(J30),A(J34),A(J35),
     3             A(J36),A(J37),A(J41),
     4             A(J56),A(J57),A(J58),A(J59),A(J60),A(J61),
     5             A(J62),A(J53),A(J64),A(J65),A(J66),A(J67),
     6             A(J68),A(J69),A(J70),A(J71),A(J72),A(J73),
     7             A(J74),A(J75),A(J76),A(J77),A(J78),A(J79),
     8             A(J80),A(J81))
C
C     CALL  MAFCAL --- EFFECTIVE MACRO X-SECTION CAL ROUTINE
C
*     WRITE(6,*) ' ** NOW  MAFCAL  **** '
      CALL MAFCAL( A( J1),A( J2),A( J3),A( J4),A( J5),
     1             A( J6),A( J7),A( J8),A( J9),A(J10),
     2             A(J11),A(J12),A(J13),A(J14),A(J20),
     3             A(J21),A(J22),A(J23),A(J24),A(J25),
     4             A(J26),A(J27),A(J28),A(J29),A(J30),
     5             A(J31),A(J32),A(J33),A(J34),A(J35),
     6             A(J36),A(J37),A(J38),A(J39),A(J40),
     7             A(J41),A(J42),A(J43),A(J44),A(J45),
     8             A(J46),A(J47),A(J48),A(J49),A(J50),
     9             A(J51),A(J52),A(J53),A(J54),A(J55),
     A             A(J56),A(J57),A(J58),A(J59),A(J60),
     B             A(J61),A(J62),A(J63),A(J64),A(J65),
     C             A(J66),A(J67),A(J68),A(J69),A(J70),
     D             A(J71),A(J72),A(J73),A(J74),A(J75),
     E             A(J76),A(J77),A(J78),A(J79),A(J80),
     F             A(J81),A(J82),A(J16),A(J17),A(J19) )
C
C     FAST-MACRO END
C
  999 IOPT(80)=0
      RETURN
C
 1001 FORMAT(////,5X,'* DYNAMIC AREA INFORMATION. (MACROF) *'
     1     ///1H ,10X,'MEMORY LOCATIONS RESERVED FOR DATA STORAGE -- ',
     2        I6,' WARDS.',
     3       /1H ,10X,'MEMORY LOCATIONS USED FOR THIS PROBLEM ------ ',
     4        I6,' WARDS.',
     5       /1H ,10X,'MEMORY LOCATIONS NOT USED ------------------- ',
     6        I6,' WARDS.')
 1002 FORMAT(//1H ,10X,' MEMORY SIZE IS ',I6,' WARDS OVER IN MACROF ]]')
C
      END
