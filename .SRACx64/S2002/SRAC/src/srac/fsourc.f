      SUBROUTINE FSOURC(NR,NG,NMP,MATD,MATNM,MMR,IA)
C
C     SUBPROGRAM TO PRODUCE DISTRIBUTED SOURCE BY FISSION TO THE
C     TRANSPORT CODE  INTO  F32
C
CMOD  PARAMETER  ( MAXMAT =  30)
CMOD  PARAMETER  ( MXMESH = 200)
C
      INCLUDE  'MATDTINC'
      INCLUDE  'PIJPMINC'
C
      COMMON /MAINC/ IOPT(54),NEF,NET,NERF,NERT
     *              ,DUM2(5),NOUT1,NOUT2,DUM66(8)
     *              ,NSOUC,NFIN,NFOUT,ITYPE
C
      COMMON /PDSPDS/BUF(540),IFLSW,FILENM(3),ECODE,TEMPRY
      COMMON /WORK / XI(74,MAXMAT),AAA(500),PD(MXMESH),
     +               S(MXMESH,74),AA(20000)
C
      DIMENSION      MMR(1),IA(1),MATD(1)
      DIMENSION      II(1)
      DIMENSION      WG(74),WGT(48)
C
      EQUIVALENCE   (AA(1),II(1))
      EQUIVALENCE   (WG(1),AAA(4)),(WGT(1),AAA(79))
C
      CHARACTER *4   NODE(2),MATNM(2,*),FILENM,LETF,LETT,NUMB2
C
CKSK  DATA LETF/4H   F/,LETT/4H   T/,NUMB2/4H   2/
      DATA LETF/'   F'/,LETT/'   T'/,NUMB2/'   2'/
C
C === NR   NO OF REGIONS (MESH)
C === NMAT NO OF MATERIALS
C === MATNM NODE NAME (A8) OF MATERIAL
C === MMR M-REGION BY R-REGION
C === MATD  MATERIAL NO. BY M-REGION
C === IA   (ZONE NO BY MESH )  = IRR R-REGION BY T-REGION
C === NG   NO OF ENERGY GROUP
C === ASSUMPTION ON FLUX.... SPATIALLY FLAT,WEIGHTING SPECTRUM
C         WG,WGT CONTINEOUSLY CONNECTED
C     SUM= SUM OF NU*FISSION*FLUX IN FAST RANGE
C     SUMT=SUM OF NU*FISSION*FLUX IN THERMAL RANGE
C
C
      CALL CLEA( PD ,  MXMESH    , 1.0 )
      CALL CLEA( XI ,  74*MAXMAT , 0.0 )
      REWIND NSOUC
C
      IFLSW     = 1
CKSK  FILENM(1) = 4HMACR
      FILENM(1) = 'MACR'
CKSK  FILENM(2) = 4HOWRK
      FILENM(2) = 'OWRK'
C     FILENM(3) = 4H
CKSK  NODE(1)   = 4HCONT
      NODE(1)   = 'CONT'
CKSK  NODE(2)   = 4HF002
      NODE(2)   = 'F002'
C === TEST NR,NMAT
      IF(NR .GT.MXMESH) GO TO 600
      IF(NMP.GT.MAXMAT) GO TO 610
CJ    READ(JNMACR.FNMACR.'FAST') NGR,(WG(N),N=1,NGR)
      CALL READ(NODE,NGR   ,1    )
      CALL READ(NODE,AAA(3),NGR+1)
      IF (IOPT(4).NE.0) THEN
CJ    READ(JNMACR.FNMACR.'THERMAL') NGR,(WGT(N),N=1,NGR)
                        CALL PACK(NODE(2),      1,LETT )
                        CALL READ(NODE   ,    NGR,1    )
                        CALL READ(NODE   ,AAA(78),NGR+1)
                        ENDIF
C     STORE FISSION SPECTRUM TO XI BY MATERIAL
      POWER     = 0.0
      DO 100 NM = 1,NMP
      MATNO     = IABS(MATD(NM))
      NODE(1)   = MATNM(1,MATNO)
      NODE(2)   = MATNM(2,MATNO)
C === P0 TERM ONLY PROCESSED
CJ    READ(JNMACR.FNMACR.NODE.'FAST'.#0)    LTH,(AA(I),I=1,LTH)
      CALL PACK(NODE(2),1, LETF)
      CALL PACK(NODE(2),4,NUMB2)
      CALL SEARCH(NODE,LTH,ISW)
      IF(ISW.EQ.1) THEN
                   NODE(2)(1:1)='A'
                   CALL SEARCH(NODE,LTH,ISW)
                   IF(ISW.EQ.1) THEN
                                WRITE(NOUT1,35) NODE(1),NODE(1)
                                STOP
                                ENDIF
                   ENDIF
C
      CALL READ(NODE,AA,LTH)
      LOC     = 0
   40 SUM     = 0.0
      DO 50 N = 1,NG
CM    IF(N.EQ.1 .AND. AA(LOC+5).EQ.0.0) GO TO 60
      IF(N.EQ.1 .AND. AA(LOC+7).EQ.0.0) GO TO 60
      XI(N,NM)= AA(LOC+7)
   45 SUM     = SUM + AA(LOC+5)*WG(N)
      LOC     = LOC + 10 + II(LOC+2)
   50 CONTINUE
   60 SUMT    = 0.0
C
      IF(IOPT(4).EQ.0) GO TO 90
CJ    READ(JNMACR.FNMACR.NODE.'THERMAL'.#0) LTH,(AA(I),I=1,LTH )
      IFLAG    = 0
      CALL PACK  (NODE(2),1   ,LETT)
      CALL SEARCH(NODE   ,LTH ,ISW )
      IF(ISW.EQ.1) THEN
                   NODE(2)(1:1)='A'
                   CALL SEARCH(NODE,LTH,ISW)
                   IF(ISW.EQ.1) THEN
                                WRITE(NOUT1,65) NODE(1),NODE(1)
                                STOP
                                ENDIF
                   IFLAG = 1
                   ENDIF
C
      IF(IFLAG.EQ.0) THEN
                     CALL READ(NODE,AA,LTH)
                     LOC  = 0
                     ENDIF
C
C     READ MACRO NU*FISS IN THE THERMAL RANGE
C
      DO 80 N   = 1,NGR
      SUMT      = SUMT + AA(LOC+5)*WGT(N)
      LOC       = LOC  + 10 + II(LOC+2)
   80 CONTINUE
CTFREE
CM 90 IF(SUM.EQ.0.) GO TO 100
CM    DO 95  N = 1,NG
CM    XI(N,NM) = XI(N,NM)*(SUM+SUMT)
CM 95 CONTINUE
CM    POWER    = POWER + SUM + SUMT
CEND
   90 CONTINUE
      DO 95  N = 1,NG
      XI(N,NM) = XI(N,NM)*SUMT
   95 CONTINUE
      POWER    = POWER + SUMT
  100 CONTINUE
C
C*** READ IN FISSION SPECTRUM FOR NON-FISSILE CASE
C
      IF(POWER.EQ.0.0) THEN
CKSK                   FILENM(1) = 4HFAST
                       FILENM(1) = 'FAST'
CKSK                   FILENM(2) = 4HU
                       FILENM(2) = 'U   '
CKSK                   NODE(1)   = 4HFISS
                       NODE(1)   = 'FISS'
CKSK                   NODE(2)   = 4HYILD
                       NODE(2)   = 'YILD'
                       CALL READ(NODE,XI,NG)
                       DO 200 N  = 1,NG
                       DO 200 I  = 1,NR
                       S(I,N)    = XI(N,1)
  200                  CONTINUE
                       GO TO 550
                       ENDIF
C
C*** READ IN FISSION SPECTRUM FOR NON-FISSILE CASE
C
CK300 IF(IOPT(40).EQ.1) CALL REAG(PD,NR,4HPOWE,4HR DS)
  300 IF(IOPT(40).EQ.1) CALL REAG(PD,NR,'POWE','R DS')
      DO 500 N = 1,NG
      DO 400 I = 1,NR
      J        = IA(I)
      M        = MMR(J)
      DO 310 K = 1,NMP
      IF (IABS(MATD(K)).EQ.IABS(M)) GO TO 320
  310 CONTINUE
      WRITE(99,1000) M,N
      STOP
  320 M        = K
      S(I,N)   = XI(N,M)*PD(I)
  400 CONTINUE
  500 CONTINUE
C
  550 WRITE(NSOUC) ((S(I,N),I=1,NR),N=1,NG)
C
CM    WRITE(99,605) ((S(I,N),I=1,NR),N=1,NG)
CM    WRITE(99,605) ((S(I,N),I=1,NR),N=1,NG)
C
      IF(ITYPE.NE.0) THEN
                     DO 620 I = 1,NR
                     DO 620 N = 1,NG
                     S(I,N)   = WG(N)
  620                CONTINUE
                     WRITE(NFIN) ((S(I,N),I=1,NR),N=1,NG)
                     ENDIF
C
      RETURN
C
C ***  ERROR CASE FOR SHORTAGE OF ARRAY DIMENSION SIZE
C
  600 WRITE(NOUT1,601) NR
      WRITE(NOUT2,601) NR
      STOP
C
  610 WRITE(NOUT1,611) NMP
      WRITE(NOUT2,611) NMP
      STOP
C
   35 FORMAT(' *** MEMBER ',A4,'FXX2 NOR ',A4,'AXX2 NOT FOUND ON',
     *       ' MACROWRK FILE IN FSOURCE STEP  ---> ERROR STOP')
   65 FORMAT(' *** MEMBER ',A4,'TXX2 NOR ',A4,'AXX2 NOT FOUND ON',
     *       ' MACROWRK FILE IN FSOURCE STEP  ---> ERROR STOP')
C
  605 FORMAT(1H0,10X,'FSOURC E.G',/(15X,1P,6E14.5))
  601 FORMAT(' *** NUMBER OF SPATIAL MESH EXCEEDS THE LIMIT IN FSOURC '
     * ,I6)
  611 FORMAT(' *** NUMBER OF MATERIALS EXCEEDS THE LIMIT IN FSOURC '
     * ,I6)
C
C1000 FORMAT('0***ERROR***(FSOURC) MATERIAL BY R-REGION ('I6') INTERVAL'
C    *       ' (',I6,')'  )
 1000 FORMAT('0***ERROR***(FSOURC) MATERIAL BY R-REGION (',I6,
     *      ') INTERVAL',' (',I6,')'  ) 
C
      END
