      SUBROUTINE PIJ3(NGMAX,NRMAX,MTNAM,MATR,VOL)
C **********************************************************************
C                            PIJ3
C **********************************************************************
      COMMON / PIJ2C / IGT,NZ,NR,NRR,NXR,IBOUND,IDRECT,LCOUNT,IEDPIJ,
     &                 IFORM,NTTAB,NUTAB,SZ,II14,
     &                 NGLAST,NGUP,ITAPE,NGKMAX,
     &                 IEDFLX,ITMINN,ITMOUT,ITBG,LCMX,ITDM,IPT,
     &                 EPSI,EPSO,EPSG,RELCA,OVERX,FACTOR,ICOOD,NMP,
     &          NO1(6),LCMMR,LCNREG,LCIRR,LCIXR,LCMAR,LCMAT,LCVOL,
     &                 LCVOLR,LCVOLX,LCVOLM,LCMATD,AA(950)
C
      COMMON / MAINC / DUM(63),NOUT1,NOUT2,DUM66(11),
     &                 ITYPE,I78(18),MXDIM,
     &                 I97,IRANG,I99(2),IDCASE(2),TITLE(18),AAA(380)
C
      COMMON /WORK/    A(30000)
C
      DIMENSION        MTNAM(1),MATR(1),VOL(1)
      DIMENSION        I(2000)
      CHARACTER*4      RANGE(2,3)
C
      EQUIVALENCE     (A(1),I(1)),(DUM(40),IFIXS)
C
      DATA RANGE/'FAST','    ','THER','MAL ','ALL ','    '/
C
C *** START OF PROCESS
C
      ITAPE = 1
      IFLAG = 0
      IF(ITMINN.LE.0) THEN
                      EPSI  = 1.000E-4
                      EPSO  = 1.000E-5
                      RELCA = 1.200
                      EPSG  = 0.001
                      OVERX = 100.0
CM                    FACTOR= 0.700
                      FACTOR= 0.500
                      ITBG  =  5
                      LCMX  =  5
                      ITDM  =  5
                      IPT   = -1
                      ITMOUT= 60
                      ITMINN= 20
                      ENDIF
C
CM    IF(IRANG.EQ.1) ITMOUT=1
CM    IF(IRANG.EQ.1) ITMINN=200
C
      IF(IRANG.EQ.1) THEN
                     IT1SAV=ITMOUT
                     IT2SAV=ITMINN
                     ITMOUT=     1
                     ITMINN=   200
                     ENDIF
C
      WRITE(NOUT2,11) IDCASE,TITLE,RANGE(1,IRANG+1),RANGE(2,IRANG+1)
      WRITE(NOUT2,13) IEDFLX,ITMINN,ITMOUT,ITBG,LCMX,ITDM,IPT
      WRITE(NOUT2,14) EPSI,EPSO,EPSG,RELCA,OVERX,FACTOR
      WRITE(NOUT2,15) ITYPE,IRANG,IFIXS
C
   11 FORMAT(1H1//10X,'===PIJ3 STEP==='/
     & 10X,'***',2A4,'***',18A4,'*** IN ',2A4,' RANGE ***')
   13 FORMAT( /10X,'ITERATION PARAMETERS '
     &       /10X,'PRINT (1+2+4+8)(FLUX,XEC,PIJ,S)          ',I5
     &       /10X,'MAX OF INNER ITERATIONS PER OUTER        ',I5
     &       /10X,'MAX OF OUTER ITERATIONS                  ',I5
     &       /10X,'EARLIST EXTRAPOLATION                    ',I5
     &       /10X,'NUMBER OF ITERATIONS TESTED              ',I5
     &       /10X,'MINIMUM DELAY                            ',I5
     &       /10X,'MONITOR PRINT(0,1)(SKIP,PRINT)           ',I5
     &       )
   14 FORMAT( 10X,'CONVERGENCE CRITERION OF INNER ',E15.5
     &       /10X,'CONVERGENCE CRITERION OF OUTER ',E15.5
     &       /10X,'EXTRAPOLATION CRITERION        ',E15.5
     &       /10X,'OVER-RELAXATION (INITIAL)      ',E15.5
     &       /10X,'MAX EXTRAPOLATION              ',E15.5
     &       /10X,'BASE FACTOR OF OVER-RELAXATION ',E15.5 )
   15 FORMAT( /10X,'PROBLEM TYPE  '
     &       /10X,'ITYPE (0/1  =EIGENVALUE/FIXED S)         ',I5
     &       /10X,'IRANG (0/1/2=FAST/THERMAL/ALL)           ',I5
     &       /10X,'IFIXS (0/1/2)                            ',I5/)
C
C *** SET MEMORY LOCATION FOR VAIABLE DIMENSION
C
      NRP1 = NGMAX
C === LOCATION OF GROUP VECTOR
      INLOC = 1
C === LOCATION OF SELF-SCATTER IN SCATTERING VECTOR
      INLSS = INLOC  + NMP*NGMAX
C === LENGTH OF SCATTERING VECTOR
      INLGV = INLSS  + NMP*NGMAX
C === REACTION RATE DISTRIBUTION BY REGION
      INRR  = INLGV  + NMP*NGMAX
C === REACTION RATE ENERGY DISTRIBUTION
      INRE  = INRR   + NRMAX
C === WHOLE PACKAGE OF MACROSCOPIC CROSS SECTIONS
      INXEC = INRE   + NGMAX
C
      CALL INP2F(NGMAX,I(INLOC),I(INLSS),I(INLGV),A(INXEC),INP,A(INXEC),
     &                    MTNAM,AA(LCMATD))
C === COLLISION PROBABILITIES P
      INP  = INXEC + INP
C === FLUX  FLUX
   16 INF  =INP    + NRMAX*NRMAX*NRP1
C === DISTRIBUTED SOURCE  Q
      INQ  =INF    + NGMAX*NRMAX
C === BIRTH RATE   H
      INH  =INQ    + NRMAX*NGMAX
C === DISTRIBUTED SOURCE INTO THERMAL RANGE S
      INS  =INH    + NRMAX*NGMAX
C === FISSION DISTRIBUTION            FR
      INFR =INS    + NRMAX*NGMAX
C === FLUX BY MATERIAL REGION         FM
      INFM =INFR   + NRMAX
C === REACTION BY MATERIAL REGION     RM
      INRM =INFM   + NMP
C === LEAKAGE PROBABILITIES           DR
      INDR =INRM   + NMP
C === FIRST FLEIGHT COMPONENT OF FLUX FFF
      INFFF=INDR +NRMAX*NGMAX
C === AREA FOR MATRIX INVERSION       GFX
      INGFX=INFFF
      IF(IFIXS.EQ.2) INGFX=INFFF+NRMAX*NGMAX
C === MAXIMUM DATA REQUIREMENT        K
      K    = INGFX
      IF(IRANG.NE.1) K=INGFX+NRMAX*NRMAX
C === WORK DIMENSION OF NEW-FLUX      FLXNEW
      INFLXN = K
      INREBQ = INFLXN + NRMAX*NGMAX
      INREBA = INREBQ + NGMAX
      INREBF = INREBA + NGMAX
      INREBR = INREBF + NGMAX
      INREBS = INREBR + NGMAX
      INREBW = INREBA + NGMAX*NGMAX
      K      = INREBW + NGMAX*NRMAX
C
      IF(K.LE.MXDIM) GO TO 120
      IF(IFLAG.EQ.0) THEN
                     ITAPE  = 2
                     NRP1   = 1
                     IFLAG  = 1
                     WRITE(NOUT1,115) K
                     IF(IFORM.NE.1)  THEN
                                     WRITE(NOUT1,125)
                                     STOP
                                     ENDIF
                     GO TO 16
                     ENDIF
C
      K      = K-MXDIM
      WRITE(NOUT1,19)   K,MXDIM
      STOP
C
   19 FORMAT('0//DIMENSION OVER.',I10,' FORM',I10)
   20 FORMAT(10X,'STRAGE USED ',I10,' WITHIN',I10,' IN PIJ3 STEP'/)
  115 FORMAT(' **** INSUFFICIENT MEMORY FOR STORING ALL PIJ **', I7,
     & ' REQUIRED *** TRY BY CHANGING SCHEME TO KEEP ONE-GROUP PIJ ')
  125 FORMAT(' **** IFORM : FORM OF PIJ MUST BE 1 DUE TO CORE SIZE ',
     &  ' LIMIT')
C
C     END OF MEMORY LOCATION
C
  120 WRITE(NOUT1,20) K,MXDIM
      LENG = K - INP + 1
      CALL   CLEA( A(INP) , LENG , 0.0 )
C
      CALL INP3F(NRMAX,NGMAX,A(INRR),A(INRE),A(INP),
     &        A(INF),A(INQ),A(INLOC),I(INLSS),I(INLGV),A(INXEC),A(INDR)
     &       ,VOL,A(INFR),MATR,A(INFFF))
C
      CALL ITER(NRMAX,NGMAX,A(INRR),A(INRE),A(INP),
     & A(INF),A(INQ),A(INH),A(INS),I(INLOC),I(INLSS),I(INLGV),A(INXEC),
     &       A(INDR),A(INFR),MATR,A(INGFX),A(INFFF),VOL,
     @  A(INFLXN),A(INREBQ),A(INREBA),A(INREBF),A(INREBR),A(INREBS),
     @  A(INREBW) )
C
      CALL TEDIT (NRMAX,NGMAX,A(INRR),A(INRE),
     & A(INF),A(INQ),A(INH),A(INS),I(INLOC),I(INLSS),I(INLGV),A(INXEC),
     &     A(INFM),A(INRM),A(INDR),VOL,MATR)
C
C *** END OF PROCESS
C
      IF(IRANG.EQ.1) THEN
                     ITMOUT=IT1SAV
                     ITMINN=IT2SAV
                     ENDIF
      RETURN
      END
