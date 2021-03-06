C             DUMPER              LEVEL=1        DATE=81.11.14
      SUBROUTINE DUMPER (NOTDP,IOP)
C
C     WRITES RESTART DUMP.  ALL RECORDS CONTAIN A COUNT
C     IOP IS THE CURRENT NUMBER OF GROUPS ON THE ANGULAR FLUX FILE
C
      COMMON /TW1C/ DD(1),LIM1,IA(210)
      COMMON /WORK/AAA(1),LIM2,AA(130)
      DIMENSION D(212),A(132)
      EQUIVALENCE (D(1),DD(1)),(AAA(1),A(1))
CMSASA FOR PC BUT NOT USED ON VPP (K.OKUMURA)
C     EQUIVALENCE (D(105),LAST1),(A(52),LAST2),(D(106),LASTEC),
C    &(D(155),LENCLR),(A(61),LC),(D(113),LTFX),(D(132),LTHAF),
C    &(D(133),LTVAF),(D(158),NAFLUX),(D(159),NDUMP1),(D(160),NDUMP2)
C
CKSK  EQUIVALENCE (DD(105),LAST1),(AAA(52),LAST2),(DD(106),LASTEC),
CKSK &(DD(155),LENCLR),(AAA(61),LC),(DD(113),LTFX),(DD(132),LTHAF),
CKSK &(DD(133),LTVAF),(DD(158),NAFLUX),(DD(159),NDUMP1),(DD(160),NDUMP2)
      EQUIVALENCE (IA(103),LAST1),(AA(50),LAST2),(IA(104),LASTEC),
     &(IA(153),LENCLR),(AA(59),LC),(IA(111),LTFX),(IA(130),LTHAF),
     &(IA(131),LTVAF),(IA(156),NAFLUX),(IA(157),NDUMP1),(IA(158),NDUMP2)
CKSK
C
      DIMENSION IPARM(5)
C
      EQUIVALENCE (IA(4),IGM),(IA(58),NM),(IA(65),JT),(IA(66),ITJT),
     &(IA(78),JTP)
      EQUIVALENCE (IPARM(2),NORDM), (IPARM(3),NORDAF)
C
C
C     PRELIMINARY OPERATIONS
C
      IPARM(1)=IGM
      IPARM(4)=ITJT
      IPARM(5)=NM
      NORDAF=0
      IF (IOP.NE.0) NORDAF=IOP*2*(JTP+JT)
      ITEMP=LASTEC-1-(IGM*LTFX)
      NORDM=ITEMP/LENCLR
      ITEMR=ITEMP-NORDM*LENCLR
      IF (ITEMR.NE.0) NORDM=NORDM+1
C
C     SAVE CONSTANTS AND COMMONS
C
      CALL REED (NOTDP,0,0.0,0,4)
      CALL RITE (NOTDP,IPARM,0.0,5,6)
CKSK  CALL RITE (NOTDP,0,D(1),204  ,3)
      CALL RITE (NOTDP,0,DD(1),204  ,3)
CKSK  CALL RITE (NOTDP,0,A(1),130  ,3)
      CALL RITE (NOTDP,0,AAA(1),130  ,3)
CKSK  CALL RITE (NOTDP,0,D(1),LAST1,3)
      CALL RITE (NOTDP,0,DD(1),LAST1,3)
CKSK  CALL RITE (NOTDP,0,A(1),LAST2,3)
      CALL RITE (NOTDP,0,AAA(1),LAST2,3)
C
C     SAVE FLUX BY GROUP FROM LCM
C
      IECSR=1
      DO 100 IG=1,IGM
CKSK  CALL REED (0,IECSR,A(LC),LTFX,1)
CKSK  CALL RITE (NOTDP,0,A(LC),LTFX,3)
      CALL REED (0,IECSR,AAA(LC),LTFX,1)
      CALL RITE (NOTDP,0,AAA(LC),LTFX,3)
      IECSR=IECSR+LTFX
  100 CONTINUE
C
C     SAVE REMAINDER OF LCM IN BLOCKS OF SIZE EQUAL TO LAST
C
      DO 110 IDX=1,NORDM
      ILEN=LENCLR
      IF ((IDX.EQ.NORDM).AND.(ITEMR.NE.0)) ILEN=ITEMR
CKSK  CALL REED (0,IECSR,A(LC),ILEN,1)
CKSK  CALL RITE (NOTDP,0,A(LC),ILEN,3)
      CALL REED (0,IECSR,AAA(LC),ILEN,1)
      CALL RITE (NOTDP,0,AAA(LC),ILEN,3)
      IECSR=IECSR+ILEN
  110 CONTINUE
      IF (NORDAF.EQ.0) GO TO 150
C
C     SAVE ANGULAR FLUX FROM NAFLUX
C
      CALL REED (NAFLUX,0,0.0,0,4)
      DO 140 IDX=1,IOP
      IDZ=2*JTP
      DO 120 IDY=1,IDZ
CKSK  CALL REED (NAFLUX,0,A(LC),LTVAF,2)
CKSK  CALL RITE (NOTDP,0,A(LC),LTVAF,3)
      CALL REED (NAFLUX,0,AAA(LC),LTVAF,2)
      CALL RITE (NOTDP,0,AAA(LC),LTVAF,3)
  120 CONTINUE
      IDZ=2*JT
      DO 130 IDY=1,IDZ
CKSK  CALL REED (NAFLUX,0,A(LC),LTHAF,2)
CKSK  CALL RITE (NOTDP,0,A(LC),LTHAF,3)
      CALL REED (NAFLUX,0,AAA(LC),LTHAF,2)
      CALL RITE (NOTDP,0,AAA(LC),LTHAF,3)
  130 CONTINUE
  140 CONTINUE
C
C     RESTORE CORE
C
  150 CONTINUE
      CALL RITE (NOTDP,0,0.0,0,4)
      CALL REED (NOTDP,0,0.0,4,7)
CKSK  CALL REED (NOTDP,0,A,ITEMP,3)
      CALL REED (NOTDP,0,AAA,ITEMP,3)
      CALL REED (NOTDP,0,0.0,0,4)
C
C     EXCHANGE  UNITS
C
      ITEMP=NDUMP1
      NDUMP1=NDUMP2
      NDUMP2=ITEMP
      RETURN
      END
