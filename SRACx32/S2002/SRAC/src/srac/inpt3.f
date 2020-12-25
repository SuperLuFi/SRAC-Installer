C***********************************************************************
C                           INPT3
C                       READ X-SECTIONS
C***********************************************************************
      SUBROUTINE INPT3(NGMAX,MTM,LOC,LSS,LGV,XEC,MATN,III,LGXEC)
C
      CHARACTER *4 RANGE,IDENT,FILENM,MATN,ICF,CASEID
C
      DIMENSION MTM(NMP) ,LOC(NGMAX,NMP),LSS(NGMAX,NMP),LGV(NGMAX,NMP),
     &          XEC(5000),MATN(2,NMP)   ,III(5000),IDENT(2),RANGE(3)
C
      COMMON /TUD1C/ II1,NMP,II3,NGUP,NGKMAX,II6,IG,IBOUND,
     &               IGUESS,ID,ITMAX,ITMOUT,ITBG,LCMX,ITDM,IPT,
     &               EPS,EPS0,EPSG,RELCA,OVERX,FACTOR,XLAMD,BSQ1,
     &               IPTXEC,ITFLUX,IPTS,IDOPT,NXR,LCIK,LCNK,LCXR,
     &               LCRK,LCNN1,LCVOLR,LCMTM,LCMTR,DUMT(13),II(500)
C
      COMMON /MAINC/ IOPT(20),JNFSTL(2),FNFSTL(2),JNTHEL(2),FNTHEL(2)
     &    ,JNEFST(2),FNEFST(2),JNETHE(2),FNETHE(2),JNMACR(2),FNMACR(2)
     &    ,JNMCRS(2),FNMCRS(2),JNEMIC(2),FNEMIC(2),JNFLUX(2),FNFLUX(2)
     &   ,NEFL     ,NETL     ,NEF      ,NET      ,NERF     ,NERT
     &   ,NMAT     ,NETL1    ,BSQB     ,NIN1     ,NIN2     ,NOUT1
     &   ,NOUT2,IT0,NEFL1    ,NEFL2    ,NEFL3    ,NEF1     ,NEF2
     &   ,NEF3     ,ISTEP    ,DUMMY1(7)
     &   ,LCNEGF   ,LCNEGT   ,LCNECF   ,LCNECT   ,LCMTNM   ,LCNISO
     &   ,LCTEMP   ,LCXL     ,LCXCDC   ,LCLISO   ,LCIDNT   ,LCDN
     &   ,LCIRES   ,LCIXMC   ,DUMMY2(3),IRANG,ICF,DUM100
     &   ,CASEID(2),TITLE(18)
     &   ,IIA(880)
      COMMON /PDSPDS /BUF(540),IFLSW,FILENM(3),ECODE,TEMPRY
C
      DATA RANGE/'FAST','THER','ALL '/
C
C *** START OF PROCESS
C
      IFLSW     = 1
      FILENM(1) = 'MACR'
      FILENM(2) = 'O   '
      FILENM(3) = '    '
      IF(ICF.EQ.'0002') FILENM(2)='OWRK'
C *** CROSS-SECTION ***
      K2        = 0
      NGUP      = NGMAX
      NGKMAX    = 0
      DO 130 NM = 1,NMP
      MTNO      = MTM(NM)
      IDENT(1)  = MATN(1,MTNO)
      IDENT(2)  = MATN(2,MTNO)
      CALL PACKX(IDENT(2),1,RANGE(IRANG+1),1)
      CALL PACK (IDENT(2),4,ICF)
      WRITE(NOUT2,6050) NM,MTNO,IDENT
      CALL SEARCH(IDENT,LENGTH,ISW)
      IF(ISW.EQ.1) GO TO  901
      CALL READ(IDENT,XEC(K2+1),LENGTH)
C --- N2N DATA
      IF(ICF.EQ.'0000') CALL PACK(IDENT(2),4,'   N')
      IF(ICF.EQ.'0002') CALL PACK(IDENT(2),4,'   M')
      CALL SEARCH(IDENT,LTHN,ISW)
      N2N     = 0
      IF(ISW.EQ.0) THEN
                   N2N   = 1
                   LOCN  = K2 + LENGTH
                   CALL READ(IDENT,XEC(LOCN+1),LTHN)
                   ENDIF
C --- ENERGY LOOP START
      DO 130  NG = 1,NGMAX
       K1        = K2+1
      LOC(NG,NM) = K1
      LSS(NG,NM) = III(K1)
      LGV(NG,NM) = III(K1+1)
      XEC(K1+1)  = XEC(K1+9)
      IF(XEC(K1+6).NE.0.)  NGKMAX = MAX0(NGKMAX,NG)
C --- N2N DATA
      IF(N2N.NE.0 .AND. LOCN.LT.K2+LENGTH+LTHN) THEN
                     XEC(K+1) = XEC(K+1) - XEC(LOCN+11)
                     XEC(K+5) = XEC(K+5) - XEC(LOCN+11)
                     LOCN     = LOCN + 10 + III(LOCN+2)
                                                ENDIF
C
      K1         = K1 + 10
      K2         = K1 + LGV(NG,NM) - 1
      J          = LGV(NG,NM) + NG - LSS(NG,NM) - NGMAX
      XEC(K1-10) = 0.0
      IF(J .LE. 0) GO TO 115
      J1         = K2 - J + 1
      T          = 0.0
      DO 114   K = J1 , K2
      T          = T + XEC(K)
  114 CONTINUE
      XEC(K1-10) = T
      LGV(NG,NM) = LGV(NG,NM)-J
  115 CONTINUE
      IF(LSS(NG,NM).LE.NG) GO TO 116
      XEC(K1-10) = XEC(K1-10) + XEC(K1)
  116 CONTINUE
      XEC(K1-1)  = XEC(K1- 1) + XEC(K1-10)
      IF(LSS(NG,NM).NE.1) NGUP = MIN0(NGUP,NG-LSS(NG,NM)+1)
      K3         = K1 + LSS(NG,NM) - 1
      XEC(K1-5)  = XEC(K1-5) - XEC(K3)
      XEC(K3)    = 0.0
  130 CONTINUE
C
      LGXEC      = K2
      IF(IPTXEC.LE.0)  THEN
                       RETURN
                       ENDIF
C
      WRITE(NOUT2,15) TITLE
      DO 150  NM = 1,NMP
      MTNO       = MTM(NM)
      WRITE(NOUT2,20) NM ,MTNO
      DO 140  NG = 1,NGMAX
      K          = LOC(NG,NM)
      WRITE(NOUT2,17) NG,LSS(NG,NM),LGV(NG,NM),XEC(K+2),XEC(K+3),
     &     XEC(K+4),XEC(K+5),XEC(K+6),XEC(K+7),XEC(K+8),XEC(K+1),XEC(K)
      K1         = K + 10
      K2         = K1+ LGV(NG,NM) - 1
      WRITE(NOUT2,18) (XEC(K),K=K1,K2)
  140 CONTINUE
  150 CONTINUE
C
      RETURN
C
  901 WRITE(NOUT1,911) IDENT(1),IDENT(2),RANGE(IRANG+1)
      STOP
C
   15 FORMAT(1H1,18A4,20X,'*** CROSS-SECTION ***'//15X,'LSS   LGV',3X,
     &       'SIG.ACT',5X,'SIG.F',4X,'NU*SIG.F',7X,'SIG.RM',6X,' X   ',
     &       7X,' D1   ',6X,' D2   ',6X,'SIG.AB',6X,'SIG OUT '//)
   17 FORMAT('   GROUP ',I3,3X,2I5,1P9E12.5)
   18 FORMAT(25X,1P8E12.5)
   20 FORMAT(2X,'** MATERIAL REGION',I3,' ** MATERIAL NUMBER ',I3)
C
  911 FORMAT(1H0,'*** MATERIAL *',2A4,'* NOT FOUND IN ',A4,
     &        ' RANGE OF MACRO X-SECTION FILE *** TUD2 STEP ***')
C
 6050 FORMAT(10X,'** MATERIAL REGION',I3,' ** MATERIAL NUMBER',I3,
     &       ' ** MATERIAL NAME ',2A4)
C     DEBUG SUBCHK
      END
