      SUBROUTINE REACT(IMICR,ISTEP)
C
C     CALCULATE RATE OF DETECTOR REACTION & INTEGRATED PARAMETER
C                                          OF FISSILE NUCLIDE
C IMICR = 1 MICRO          = 0 MACRO
C      ISTEP = 1 IOPT(2)        = 2 IOPT(12)
C
      COMMON /REACC/ IOPT(4),MREC,ICODE,IXMAX,IYMAX,IZMAX,IDIM,
     1               IGMAX,NGF,NGT,IRANG,IMESHT,IMESH1,IMESH2,IMESH3,
     2               IDUM(3),LMTNM1,LIREC1,LNMSH1,LMTNM2,LIREC2,LNMSH2,
     3               LFG,LNREC,LMPOI,LLU235,LLU238,LFGS,LMESH1,LMESH2,
     4               LMESH3,LNMSH3,LAST,IRALIM,LR1,LR2,LR3,LVOL,LFINAM,
     5               DUM(6),A(1960)
      COMMON /MAINC/ IOPTS(52)
     3    ,NEFL     ,NETL     ,NEF      ,NET      ,NERF    ,NERT
     4    ,NMAT     ,NETL1    ,BSQ      ,NIN1     ,NIN2    ,NOUT1
     5    ,NOUT2    ,IT0      ,NEFL1    ,NEFL2    ,NEFL3   ,NEF1
     6    ,NEF2     ,NEF3     ,ISTP     ,NSOUC    ,NFIN    ,NFOUT
     7    ,ITYPE    ,IMCEF    ,IBNSTP   ,MEMFST
     8    ,LCNEGF   ,LCNEGT   ,LCNECF   ,LCNECT   ,LCMTNM   ,LCNISO
     9    ,LCTEMP   ,LCXL     ,LCXCDC   ,LCLISO   ,LCIDNT   ,LCDN
     A   ,LCIRES   ,LCIXMC   ,NFTOT    ,MEMORY   ,IOPEN    ,IRANGS
     B   ,ICFS     ,INITL
     C   ,CASEID(2),TITLE(18) ,S(880)
      COMMON /WORK/  B(6000)
      COMMON /TUD1C/ NRT,NMPT,NGTT,NGST,NGKT,NNT,DUMT(22),NXRT,LCIK,
     1               LCNK,LCXR,LCRK,LNNNR,LCVOLT,LCMTM,LCMTR,LCVLMT,
     2               DUMTT(12),IIT(500)
C     COMMON /CIT1C/ NMC,NXRC,IDC,IRN,LCNMC,LCNXRC,LCMACC,LCVOLC,
C    1               ICMAX,ICT(3000)
      COMMON /PIJ2C/ PIJ(50),PAA(950)
      COMMON /SN1C/ ISA(1000)
      COMMON /TW1C/ TWTRN(4000)
      EQUIVALENCE (NRR,PIJ(4)),(IZM,ISA(36)),(IMJM,TWTRN(58))
      CHARACTER *4 ICFS,ICF,CASEID
C
C
C
      IRANG  = 2
      IF (IOPTS(4).EQ.0) IRANG = 0
C
CM    WRITE(6,*) ' ** REACT STEP IS NOW ENTERED ** '
CM    WRITE(6,*) ' ** IMICR ISTEP IRANG = ',IMICR,ISTEP,IRANG
CM    WRITE(6,*) ' ** IOPT(1) TO IOPT(4)= ',IOPT
C
      IF (ISTEP.EQ.1) THEN
         ICF = '0002'
         ELSE
         IF (IOPTS(10).EQ.0) THEN
            ICF = '0002'
            ELSE
              ICF = '0000'
         ENDIF
      ENDIF
C
C      IMIX : M-REGION AND X-REGION ADJUST INDICATOR
C            = 0 ADJUST , = 1 NON-ADJUST
C
       IF (IOPTS(2).EQ.0 .OR. IOPTS(12).EQ.0) THEN
          IMIX = 1
          ELSE
            IF (IOPTS(2).NE.IOPTS(12)) THEN
               IMIX = 0
               ELSE
                 IMIX = 1
            ENDIF
        ENDIF
C
CI    IF(IMICR.EQ.1 .AND. ICFS.EQ. '0000' ) THEN
CI*** UNMATCH OF ENERGY GROUP STRUCTURE
CI    WRITE(NOUT1,6100)
CI            STOP
CI                                          ENDIF
      IGMAXC = NGF + NGT
      NGSTRT = 1
      IEND = 1
      LFLUX = 1
      IFMESH = IMESHT
      LMES13 = LMESH1
      IF (IMICR .EQ. 0) IFMESH = IMESH1 + IMESH2
      IF (IMICR .EQ. 1) THEN
                        IFMESH = IMESH3
                        LMES13 = LMESH3
                        END IF
C
      ISIZE  = LFLUX  + IGMAXC*IFMESH - 1
C
      IF (ICODE.EQ.5) THEN
                      REWIND 92
                      READ (92) (DUMMY,I=1,70),LMAX,MMAX
                      REWIND 92
                      LNRGN = ISIZE + 1
                      LPVOL = LNRGN + IXMAX*IYMAX*IZMAX
                      ISIZE = LPVOL + LMAX - 1
                      ENDIF
C
      LSIGMA = ISIZE + 1
      ISIZE  = LSIGMA + 2*IGMAXC - 1
      LSIGMB = ISIZE + 1
      IF (IOPT(3).EQ.0) GO TO 100
      LPXVX  = LSIGMA + 4*IGMAXC
      LPMVM  = LPXVX  + IGMAXC
      ISIZE  = LPMVM  + IGMAXC - 1
  100 CONTINUE
      LBUFF  = MEMORY - LSIGMB + 1
C    FLUX READ BUFFER
      LFLUXB = LSIGMA
      GO TO (101,102,103,104,105) ,ICODE
  101 CONTINUE
      IBUFF = IXMAX*IGMAXC
      GO TO 106
  102 CONTINUE
      IBUFF = IXMAX*IGMAXC
      GO TO 106
  103 CONTINUE
      IBUFF = IXMAX*IYMAX
      GO TO 106
  104 CONTINUE
      IBUFF = (IXMAX+1)*IGMAXC
      LVOLB = LFLUXB + IBUFF
      ISIZE = LVOLB + IXMAX - 1
      GO TO 106
  105 CONTINUE
      IBUFF = 2*IXMAX*IYMAX
      IF(IDIM.EQ.3) IBUFF = IXMAX*IYMAX*IZMAX
  106 CONTINUE
      ISIZE  = MAX0(ISIZE,LFLUX+IBUFF-1)
      IF (IMICR.NE.1 .OR. IOPT(3).EQ.0) GO TO 107
      LVOLB  = LFLUXB + IBUFF
      ISIZE  = MAX0(ISIZE,LVOLB+IBUFF-1)
  107 CONTINUE
      IF (ISIZE.LE.MEMORY) GO TO 110
      WRITE (NOUT1,6000) MEMORY,ISIZE
      STOP
  110 CONTINUE
C-----  FLUX READ
CM    WRITE(6,*) ' ** IFMESH = ',IFMESH,' & IMESH3 = ',IMESH3
      IF (IFMESH.EQ.0) GO TO 140
      IF(IOPTS(18).LT.0) THEN
                         WRITE(7,10) CASEID
                         WRITE(7,15) IGMAXC
                         ENDIF
C
   10 FORMAT(2A4)
   15 FORMAT(6X,4I6)
C
CM    WRITE(6,*) ' ** FLUXRD STEP IS NOW ENTERED ** '
C
      CALL FLUXRD(B(LFLUX) ,B(LFLUXB),A(LMES13),IFMESH    ,IGMAXC    ,
     1            IXMAX    ,IYMAX    ,IZMAX    ,IBUFF     ,ICODE     ,
     2            IDIM     ,A(LVOL)  ,ISTEP    ,IRANG     ,B(LVOLB)  ,
     3            ICF      ,LNRGN    ,LPVOL    ,LMAX                  )
C
      IF (IMICR.NE.0) GO TO 130
      IF(IOPT(1).LE.0) GO TO 120
C
C     REACTION RATE OF NON FILTED DETECTOR
C
      CALL REACT1(A(LMTNM1),A(LNMSH1),A(LIREC1),A(LMESH1),A(LFG)   ,
     1            B(LSIGMA),B(LFLUX) ,B(LSIGMB),B(LSIGMB),A(LR1)   ,
     2            IOPT(1)  ,IMESHT   ,IGMAXC   ,LBUFF    ,0        ,
     3            NOUT1    ,NOUT2    ,MEMORY   ,LSIGMB   ,IEND     ,
     4            NGSTRT   ,IRANG    ,ICF      ,IGMAX    ,NGF      ,
     5            NGT                                               )
  120 CONTINUE
      IF(IOPT(2).LE.0) GO TO 140
C
C     REACTION RATE FILTED DETECTOR
C
      LFLUX2 = LFLUX + IMESH1*IGMAXC
      CALL REACT1(A(LMTNM2),A(LNMSH2),A(LIREC2),A(LMESH2),A(LFG)   ,
     1            B(LSIGMA),B(LFLUX2),B(LSIGMB),B(LSIGMB),A(LR2)   ,
     2            IOPT(2)  ,IMESHT   ,IGMAXC   ,LBUFF    ,1        ,
     3            NOUT1    ,NOUT2    ,MEMORY   ,LSIGMB   ,IEND     ,
     4            NGSTRT   ,IRANG    ,ICF      ,IGMAX    ,NGF      ,
     5            NGT                                               )
      IF(IOPT(18).LT.0) THEN
      WRITE(7,20) (A(I),I=LFG,LFG+IGMAXC-1)
   20 FORMAT(6E12.5)
                        END IF
      GO TO 140
C
  130 CONTINUE
CM    WRITE(6,*) ' ** IOPT(3) = ',IOPT(3),' ** '
CM    WRITE(6,*) ' ** NMAT    = ',NMAT
C
      IF(IOPT(3).LE.0) GO TO 140
C
C     INTEGRATIONAL PARAMETER
C
      IMES12 = 0
      LFLUX3 = LFLUX + IMES12*IGMAXC
      LVOL3  = LVOL  +  IMES12
C
CM    WRITE(6,*) ' ** REACT3 STEP IS NOW ENTERED ** '
C
      CALL REACT3(A(LNREC) ,A(LMPOI) ,A(LLU235),A(LLU238),A(LFGS)  ,
     1            A(LNMSH3),A(LMESH3),A(LR3)   ,B(LSIGMA),B(LPMVM) ,
     2            B(LPXVX) ,B(LFLUX3),S(LCMTNM),S(LCNISO),S(LCIDNT),
     3            S(LCIXMC),S(LCDN)  ,NMAT     ,1        ,
     4            MREC     ,IOPT(3)  ,IGMAX    ,IGMAXC   ,NOUT2    ,
     5            IEND     ,NGSTRT   ,A(LVOL3) ,IMIX     ,IOPTS(2)  )
  140 CONTINUE
C
C     CONVERSION RATIO CALCULATION
C
C
CM    WRITE(6,*) ' ** IOPT(4) = ',IOPT(4),' ** IMICR = ',IMICR
C
      IF (IOPT(4).EQ.0 .OR. IMICR.EQ.1) RETURN
C
      LFINA = 1
      LNCOMP = LFINA  + 2*MMAX
      LSIGMA = LNCOMP + MMAX
      LP1E   = LSIGMA + IGMAXC*5*MMAX
      LNRGN  = LP1E   + IXMAX*IYMAX*IZMAX
      LPVOL  = LNRGN  + IXMAX*IYMAX*IZMAX
      LCAP   = LPVOL  + LMAX
      LABS   = LCAP   + MMAX*5
      NCOMP  = LABS   + MMAX*5
      LREAC1 = NCOMP  + LMAX
      LREAC2 = LREAC1 + IOPT(4)
      LSIGMB = LREAC2 + IOPT(4)
      LBUFF  = MEMORY - LSIGMB + 1
      IF (LSIGMB.GT.MEMORY) THEN
         WRITE(NOUT1,6000) MEMORY,LSIGMB
         STOP
      ENDIF
C
      CALL CNVCAL(A(LFINAM),B(LFINA ),B(LNCOMP),B(LSIGMA),B(LP1E  ),
     1            B(LNRGN ),B(LPVOL ),B(LSIGMB),B(LSIGMB),B(LCAP  ),
     2            B(LABS  ),B(NCOMP) ,B(LREAC1),B(LREAC2),
     3            IOPT(4)  ,IXMAX    ,IYMAX    ,IZMAX    ,LMAX     ,
     4            MMAX     ,IGMAXC   ,LBUFF    ,ICF      ,IRANG    ,
     5            LSIGMB   ,NOUT1    ,NOUT2    ,  9      ,IDIM     ,
     6            MEMORY                                            )
      RETURN
 6000 FORMAT(1H0,10X,'*REACTION DATA STEP* (REACT)'/11X,
     1               '/REACC/ LIMIT EXCEED'/11X,
     2               'ALLOCATED',I6,' WORDS,BUT NEEDS',I6,' WORDS')
 6100 FORMAT(' ***** INTEGRAL PARAMETER CALCULATION BY EFFECTIVE MICRO
     * CALLED UNDER FEW-GROUP STATUS. SET ZERO IC10 OR IC13')
      END
