      SUBROUTINE PIJ2(NG,IOPTM)
C *********************************************************************
C                           PIJ2
C *********************************************************************
      COMMON / PIJ2C / IGT,NZ,NR,NRR,NXR,IBOUND,IDRECT,LCOUNT,IEDPIJ,
     1                 IFORM,NTTAB,NUTAB,SZ,ITYPE
     2                ,NGLAST,IDIM,IDIMD,ICOOD1,IDUM2(13),ICOOD,NM,IL,
     3 IVP,IOPT,DUM3(3),LCMMR,LCNREG,LCIRR,LCIXR,LCMAR,LCMAT,LCVOL,
     4                 LCVOLR,LCVOLX,LCVOLM,LCMATD,AA(950)
      COMMON /MAINC/ DDM(63),NOUT1,NOUT2,IT0,DDM67(13),MEMFST
     * ,DDM81(15),MXDIM,DUM97(4),
     1                 CASENM(2),TITLE(18),DDM2(880)
C     COMMON /PDSPDS/  BUFFER(540),IFLSW,NFILE(3),ECODE,ITMP
      COMMON / WORK /  A(62000)
      COMMON /BANK/ MAXL0(4),MAXLL(4),LBANK
CDEL  DATA/LBANK/500/
      DIMENSION        IA(20)
C
      EQUIVALENCE (A(1),IA(1)),(DDM(40),IFIXS)
C
C *** START OF PROCESS
C
      REWIND 21
      REWIND 22
C
CITJ MODIFY VECTOR REGISTER LENGTH START
C     LBANK  = 500
      LBANK  = 512
      IF (IVP .NE. 0) LBANK = 1
CITJ MODIFY VECTOR REGISTER LENGTH END
      IF(ICOOD.NE.1) LBANK=1
      IOPT   = IOPTM
      ISDRCT = IDRECT
CDELETED
C     IF(IOPT.EQ.4) THEN
C                   IDRECT  = 1
C                   IDUM3(1)= 3
C                   IOPT    = 2
C                   ENDIF
CEND
      IF (IOPT.EQ.1) THEN
                     NN    = NR
                     LCVT  = LCVOL
                     LCMTT = LCMAT
                     GO TO 30
                     ENDIF
C
      IF( IOPT.EQ.2) THEN
                     NN    = NRR
                     LCVT  = LCVOLR
                     LCMTT = LCMMR
                     GO TO 30
                     ENDIF
C
   20 IF(IOPT.EQ.3)  THEN
                     IDRECT = 1
                     NN     = NM
                     LCVT   = LCVOLM
CM                   LCMTT  = 1
CKSK DANCOFF FOR LARGE NUMBER OF MATERIALS (>20)
CKSK                 DO 25       I = 1,20
                     DO 25       I = 1,NM
   25                IA(I+MEMFST)  = I
                     GO TO 30
                     ENDIF
C
C     CALLED BY PCOPIJ OR PCOQIJ
C
      IF( IOPT.EQ.4) THEN
                     IOPT   = 2
                     IDRECT = 1
                     NN     = NRR
                     LCVT   = LCVOLR
                     LCMTT  = LCMMR
                     GO TO 30
                     ENDIF
C
      WRITE(6,*)' *** ILLEGAL IOPT ENCOUNTERED IN PIJ2 --> STOP ',IOPT
      STOP
C
C *** SET ADDRESSING ****
C
   30 CONTINUE
      IL=IOPT
      IF(NR.EQ.NRR.AND.IOPT.EQ.2) IL=1
      IF(NRR.EQ.NM.AND.IOPT.EQ.3) IL=2
      IF(NR.EQ.NRR.AND.NRR.EQ.NM) IL=1
CDEL  NNTTAB = MAXL0(IL)*LBANK
      NNUTAB = MAXLL(IL)*LBANK
      NN1    = NN     +  1
CITJ MODIFY 96/02/13 START
CKSK  L01    = MEMFST + 21
C     L01    = MEMFST + NM + 1
C L01  SIG
C     L02=L01+NM*NG
COMMENT L02  P
C     L03=L02+NN*NN1
COMMENT L03  PR
C     L04=L03+NN*NN1*(IDRECT-1)
COMMENT L04  G
C     L05=L04+NN1
COMMENT L05  U
C     L06=L05+NNUTAB
COMMENT L06  S
C     L07=L06+NNUTAB
COMMENT L07  XX
C     L08=L07+NNUTAB
COMMENT L08  III
CVP ICOOD=1 2-D PIJ
C     LAST=L08+NNUTAB
CKSK DANCOFF FOR LARGE NUMBER OF MATERIALS (>20)
CKSK  LSIG   = MEMFST + 21
      LSIG   = MEMFST + NM + 1
      LP     = LSIG+NM*NG
      LPR    = LP  +NN*NN1
      LG     = LPR+NN*NN1*(IDRECT-1)
      LU     = LG+NN1
      LS     = LU+NNUTAB
      LXX    = LS +NNUTAB
      LIII   = LXX+NNUTAB
      LSIGN  = LIII+NNUTAB
      LAST   = LSIGN+NM
CVP ICOOD=1 2-D PIJ ONLY
      IF (ICOOD .EQ. 1) THEN
       IF (IVP .NE. 0) THEN
        LU00 = LAST
        IF (MOD(LU00,2).EQ.0) LU00 = LU00+1
        LU0I = LU00 + (MAXLL(IL)+1)*2
        LT1  = LU0I +  MAXLL(IL)   *2
        LT2  = LT1  + 2*MAXLL(IL)*IDRECT
        LSUM = LT2  + 2*MAXLL(IL)*IDRECT
        LAST = LSUM + 2*(MAXLL(IL)+1)*IDRECT
       END IF
CWORK AREA REST CHECK
       LREST = MXDIM-LAST-1
       NEED = NN*NN1*IDRECT
       IBANK = LREST/NEED
       IF (IBANK .LT. 32) THEN
        IBANK = 0
        ELSE IF (IBANK .GT. 512) THEN
        IBANK = 512
       END IF
CITJ.MOD
       WRITE(NOUT1,*) ' IBANK=',IBANK,' LBANK=',LBANK
CITJ.MOD
       LPP=LAST
       LPPR = LPP  + NN*NN1*IBANK
       LAST = LPPR + NN*NN1*IBANK*(IDRECT-1)
      END IF
C === IFORM =0 ORDINARY PROB. =1 MODIFIED
C === ITERATIVE PROCESS IN PIJ3 PREFERES THE MODIFIED
C === CASE INCLUDING AIR GAP NEEDS IFORM=0
      IFORM=0
      IF(IOPT.EQ.1) IFORM=1
      IF(LAST.GT.MXDIM) THEN
                        WRITE(NOUT1,9000) LAST,MXDIM
                        WRITE(NOUT2,9000) LAST,MXDIM
                        STOP
                        ENDIF
C
      WRITE(NOUT1,9010) LAST,MXDIM
      WRITE(NOUT2,9010) LAST,MXDIM
C
      CALL SIGRD(A(LSIG),NM,NG)
C
      IF(IOPT.EQ.3)      THEN
C     CALL PAINT(A(L01),A(L02),A(L03),A(L04),A(L07),A(L08),A(L05),A(L06)
C    *      ,NN,NG,IA(MEMFST+1),AA(LCVT),A(LPP),A(LPPR),A(LSIGIV),IBANK)
      CALL PAINT(A(LSIG),A(LP),A(LPR),A(LG),A(LXX),A(LIII),A(LU),A(LS)
     *      ,NN,NG,IA(MEMFST+1),AA(LCVT),A(LPP),A(LPPR),A(LSIGN),IBANK,
     *       A(LU00),A(LU0I),A(LT1),A(LT2),A(LSUM),MAXLL(IL))
                         ELSE
C     CALL PAINT(A(L01),A(L02),A(L03),A(L04),A(L07),A(L08),A(L05),A(L06)
C    *      ,NN,NG,AA(LCMTT)   ,AA(LCVT),A(LPP),A(LPPR),A(LSIGIV),IBANK)
      CALL PAINT(A(LSIG),A(LP),A(LPR),A(LG),A(LXX),A(LIII),A(LU),A(LS)
     *      ,NN,NG,AA(LCMTT)   ,AA(LCVT),A(LPP),A(LPPR),A(LSIGN),IBANK,
     *       A(LU00),A(LU0I),A(LT1),A(LT2),A(LSUM),MAXLL(IL))
                         ENDIF
      IDRECT=ISDRCT
C
 9000 FORMAT(1H0,9X,'DIMENSION OVER ',I7,' FROM ',I7,' IN PIJ2-STEP' )
 9010 FORMAT(10X,'STRAGE USED ',I7,' WITHIN ',I7,' IN PIJ2-STEP')
C
      RETURN
      END
