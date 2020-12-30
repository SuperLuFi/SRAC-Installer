      SUBROUTINE  GEOMTY  (TITLE,NFIG,IGOM,IFTYPE,IERR,
     @                     RX,RPP,RDP,NPIN,THETA,TY,
     @                     NX,NY,NTPIN,NAPIN,NDPIN,IDIVP,
     @                     IGT,NZ,NREG,IRR,IXR,MAR)
C
C***********************************************************************
C                                                                      *
C       GEOMTY    : DRAWNING GEOMTRY                                   *
C                                                                      *
C                                                                      *
C       MODULE NO.  :                                                  *
C       MODULE TYPE : SUBROUTINE                                       *
C       CODER       : HATA KENICHROU                                   *
C       DATE        : 1982.01.21                                       *
C                                                                      *
C***********************************************************************
C                                                                      *
C    -- CALLED BY --                                                   *
C                                                                      *
C        PIJIN                                                         *
C                                                                      *
C    -- CALLS --                                                       *
C                                                                      *
C        ORGSET,GEOM01,GEOM02,       GEOM04,GEOM05,GEOM06,             *
C        GEOM07,GEOM08,GEOM09,GEOM10,GEOM11,GEOM12,GEOM13,GEOM14       *
C                                                                      *
C......................................................................*
C                                                                      *
C    -- INPUT --                                                       *
C                                                                      *
C       (ARGUMENT)                                                     *
C                                                                      *
C   TITLE   :  DATA'S TITLE                                            *
C   IGOM    : CONTROL VALUE FOR THE REGION TO BE DRAWN IN THIS ROUTINE *
C                 1>IGOM>31  OR -1>IGOM>-31                            *
C             1=SUB,2=T ,4=R , 16=X-REGION.   8=MATERIAL NO.           *
C              (IT MAY BE COMBINDED AS IGOM=31 (1+2+4+8+16). THUS,31   *
C               INDICATE TO BE DRAWN ALL FIVE REGIONS)                 *
C             IGOM<0 .... NO DRAWING REGION NUMBER IN EACH FIGURE.     *
C                                                                      *
C   IFTYPE  : CONTROL VARIABLE FOR THE LOCATION OF DRAWING             *
C                       FIGURES AND THE NUMBER OF FRAME. 1,2 OR 3.     *
C             =1  ONE FIGURE PER ONE FRAME, SIZE=20 (CM).              *
C             =2  TWO FIGURES PER ONE FRAME, SIZE=15 (CM).             *
C             =3  FROM THREE TO FIVE FIGURES PER ONE FRAM, SIZE=10(CM) *
C    RX      :                                                         *
C    RPP     :                                                         *
C    RDP     :                                                         *
C    NPIN    :                                                         *
C    THETA   :                                                         *
C    TY      :                                                         *
C    NX      :                                                         *
C    NY      :                                                         *
C    NTPIN   :                                                         *
C    NAPIN   :                                                         *
C    NDPIN   :                                                         *
C    IDIVP   :                                                         *
C    IGT     :                                                         *
C    NZ      :                                                         *
C    NREG    :                                                         *
C    IRR     :                                                         *
C    IXR     :                                                         *
C    MAR     :                                                         *
C                                                                      *
C    -- OUTPUT --                                                      *
C                                                                      *
C       (ARGUMENT)                                                     *
C                 :                                                    *
C   IERR    : RETURN CODE. IF ERROR IS DETECTED, THIS VALUE IS MEANING *
C             SOME ERR. IERR=0,IF NORMAL END.                          *
C                                                                      *
C                                                                      *
C    -- LOCAL VARIABLES --                                             *
C                                                                      *
C   NFIG    :  NUMBER OF FIGURES ON ONE PAGE                           *
C                                                                      *
C***********************************************************************
C
      DIMENSION  KDRAWR(5),KDRAWF(5),NUM(1000,5)
      DIMENSION  RX(50),RPP(50),RDP(50),NPIN(50),THETA(50),TY(50)
      DIMENSION  NREG(1000),IRR(1000),IXR(1000),MAR(1000)
C
C
      IERR = 0
      IF (IFTYPE.LE.0 .OR. IFTYPE.GT.4)  IERR = 30
      IF (IGOM.LT.-31 .OR. IGOM.GT.31)   IERR = 40
      IF ( IERR .NE. 0 )  RETURN
C
C     DRAWING FORMAT.
C
      INIT  = 0
CKSK  CALL  NEWPEN ( 16-IFTYPE )
      KDRAWN = 1
      IF (IGOM.LT.0)   KDRAWN = 0
      IIGOM = IABS(IGOM)
CMSASA  ... NON-STANDARD INTRINSIC IAND
C           ( THAT MAY BE "AND" IN SOME SYSTEM)
C     DO  10  I = 1,5
C  10    KDRAWR(I) = IAND(IABS(IGOM),2**(I-1))
C
      DO 10 I=1,5
         KDRAWR(I) = MOD(ABS(IGOM),2**I)/2**(I-1)
   10 CONTINUE
C
C
C     *** REGION-NO. SETTING  &  DECISION OF DRAWNING FIGURE
C
      DO  100  I = 1,5
  100    KDRAWF(I) = 0
C     * SUB-REGION *
      IF ( NZ .NE. 1 )  KDRAWF(1) = 1
      DO  110  I = 1,NZ
  110    NUM(I,1) = I
C     * T-REGION *
      DO  120  I = 1,NZ
         IF ( NREG(I) .NE. 1 )  KDRAWF(2) = 1
  120    NUM(I,2) = NREG(I)
C     * R-REGION *
      DO  130  I = 1,NZ
         IF ( IRR(NUM(I,2)) .NE. 1 )  KDRAWF(3) = 1
  130    NUM(I,3) = IRR(NUM(I,2))
C     * MATERIAL NO. *
      DO  140  I = 1,NZ
         IF ( MAR(NUM(I,3)) .NE. 1 )  KDRAWF(4) = 1
  140    NUM(I,4) = MAR(NUM(I,3))
C     * X-REGION *
      DO  150  I = 1,NZ
         IF ( IXR(NUM(I,3)) .NE. 1 )  KDRAWF(5) = 1
  150    NUM(I,5) = IXR(NUM(I,3))
C
C********   DRAWING LOOP  *********
C
      DO  8000  I = 1,5
      IF (KDRAWR(I) .EQ. 0 )  GO TO 8000
C
C
      CALL ORGSET (IGT,I,IFTYPE,NFIG,TITLE)
CKSK  CALL NEWPEN ( 16-IFTYPE )
      CALL NEWPEN (1)          
CKO   GO TO (1001,1002,1001,1004,1005,1006,1007,1008,1009,1010,
CKO  @                            1011,1011,1013,1014),IGT
      GO TO (1001,1002,1001,1004,1005,1006,1007,1008,1009,1010,
     @       1011,1012,1013,1014,1015,1013),IGT
          IERR=1
          GO TO 8000
C
 1001 CALL  GEOM01 (IFTYPE,INIT,KDRAWF(I),KDRAWN,NUM(1,I), NX,RX)
         GO TO 7000
 1002 CALL  GEOM02 (IFTYPE,INIT,KDRAWF(I),KDRAWN,NUM(1,I), NX,RX)
         GO TO 7000
 1004 CALL  GEOM04 (IFTYPE,INIT,KDRAWF(I),KDRAWN,NUM(1,I), NX,RX)
         GO TO 7000
 1005 CALL  GEOM05 (IFTYPE,INIT,KDRAWF(I),KDRAWN,NUM(1,I), NX,NZ,RX)
         GO TO 7000
 1006 CALL  GEOM06 (IFTYPE,INIT,KDRAWF(I),KDRAWN,NUM(1,I), NX,RX)
         GO TO 7000
 1007 CALL  GEOM07 (IFTYPE,INIT,KDRAWF(I),KDRAWN,NUM(1,I), NX,NZ,RX)
         GO TO 7000
 1008 CALL  GEOM08 (IFTYPE,INIT,KDRAWF(I),KDRAWN,NUM(1,I), NX,RX)
         GO TO 7000
 1009 CALL  GEOM09 (IFTYPE,INIT,KDRAWF(I),KDRAWN,NUM(1,I),
     @                          NX,NAPIN,NDPIN,IDIVP, RX,RPP,RDP)
         GO TO 7000
 1010 CALL  GEOM10 (IFTYPE,INIT,KDRAWF(I),KDRAWN,NUM(1,I),
     @                     NX,NAPIN,NDPIN,IDIVP, RX,RPP,RDP,NPIN,THETA)
         GO TO 7000
 1011 CALL  GEOM11 (IFTYPE,INIT,KDRAWF(I),KDRAWN,NUM(1,I),
     @          NX,NY,NTPIN,NDPIN+1,IDIVP, RX,TY,RPP,RDP,NPIN,THETA)
         GO TO 7000
 1012 CALL  GEOM12 (IFTYPE,INIT,KDRAWF(I),KDRAWN,NUM(1,I),
     @          NX,NY,NTPIN,NDPIN+1,IDIVP, RX,TY,RPP,RDP,NPIN,THETA)
         GO TO 7000
 1013 CALL  GEOM13 (IFTYPE,INIT,KDRAWF(I),KDRAWN,NUM(1,I), NX,NY,NTPIN,
     @  NDPIN,NPIN,NPIN(1+NTPIN),IDIVP,RX,TY,RDP)
         GO TO 7000
C
 1014 CALL  GEOM14 (IFTYPE,INIT,KDRAWF(I),KDRAWN,NUM(1,I),
     @       NX,NAPIN,NDPIN,NTPIN,IDIVP, RX,RPP,RDP,NPIN,THETA)
         GO TO 7000
C
C----- MODIFYED FROM 2001/03/29 -----
 1015 CALL  GEOM15 (IFTYPE,INIT,KDRAWF(I),KDRAWN,NUM(1,I),NX,NY,
     @       NAPIN,NDPIN,NTPIN,RX,TY,NPIN,NPIN(1+NTPIN),RDP)
C------------------------------------
         GO TO 7000
C
 7000 INIT = 1
 8000 CONTINUE
C
C
C
C
      CALL  PLOT (0.0,0.0,777)
      RETURN
      END
