C     MERGE ARRAY RPP INTO RX
C              & NPTX(I) POSITION OF I-TH PIN IN RX ARRAY
C     CALLED BY CLUP77 , PATHXY , PATHHH
      SUBROUTINE MERGRX(RX,NPTX,RPP,TEMP)
      DIMENSION RX(*),NPTX(*),TEMP(*),RPP(*)
      COMMON / PIJ1C / NX,NY,NTPIN,NAPIN,NCELL,NM,NGR,NGD,NDPIN,
     1                 IDIVP,BETM,NX1,NY1,DUM(6),NDPIN1,
     2                 NDR,NDA,LL,L0,RO1,DRO,FVOL,IDUM28(8),NMESH
      COMMON / PIJ2C / IGT,NZ,NR,NRR,NXR,IBOUND,IDRECT,ICOUNT,IEDPIJ,
     1                 IFORM,NTTAB,NUTAB,SZ
      COMMON /MAINC/   DUMMY1(63),NOUT1,NOUT2
C
C *** MERGE RPP INTO RX
C
            NX1=NX+1
            IP=1
            IX=1
            JJ=1
      DO 10 I=1,NX1+NAPIN
      IF(RX(IX).LT.RPP(IP) .OR. IP.GT.NAPIN) THEN
            TEMP(JJ)=RX(IX)
            JJ=JJ+1
            IX=IX+1
         IF(IX.GT.NX1) GO TO 15
                                             ELSE
         IF(IDIVP.GT.0)              THEN
            NPTX(IP)=JJ
           IF(RX(IX).GT.RPP(IP))THEN
            TEMP(JJ)=RPP(IP)
            JJ=JJ+1
                                ENDIF
                                     ELSE
           IF(RX(IX).GT.RPP(IP))THEN
            NPTX(IP)=JJ-1
                                ELSE
            NPTX(IP)=JJ
                                ENDIF
                                     ENDIF
            IP=IP+1
                                             ENDIF
   10       CONTINUE
   15       NX1=JJ-1
            NX=NX1-1
         DO 20 J=1,NX1
         RX(J)=TEMP(J)
   20    CONTINUE
      WRITE(NOUT2,30)    'RX -NEW-DIVISION'
      WRITE(NOUT2,40) (RX(I),I=1,NX1)
      WRITE(NOUT2,30)    'PIN POSITION    '
      WRITE(NOUT2,50) (NPTX(I),I=1,NAPIN)
   30 FORMAT(1H0,9X,'===',A,'===')
   40 FORMAT(10X,10E12.5)
   50 FORMAT(10X,10I12)
      END
