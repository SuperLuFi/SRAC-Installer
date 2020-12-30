      SUBROUTINE  GEOM09 (IFTYPE,INIT,KDRAWF,KDRAWN,NUM,
     @                    NX    ,NAPIN,NDPIN,IDIVP,  RX,RPP,RDP)
C
C***********************************************************************
C                                                                      *
C       GEOM09    : OCTANT SYMMETRIC PILLAR                            *
C                             WITH  REGULAR ARRAYS OF PIN RODS         *
C                                                                      *
C       MODULE NO.  :                                                  *
C       MODULE TYPE : SUBROUTINE                                       *
C       CODER       : HATA KENICHROU                                   *
C       DATE        : 1982.02.05                                       *
C                                                                      *
C***********************************************************************
C                                                                      *
C    -- CALLED BY --                                                   *
C                                                                      *
C         GEOMTY                                                       *
C                                                                      *
C    -- CALLS --                                                       *
C                                                                      *
C        CNTPLY,CIRCLH                                                 *
C                                                                      *
C......................................................................*
C                                                                      *
C    -- INPUT --                                                       *
C                                                                      *
C       (ARGUMENT)                                                     *
C          IFTYPE :  NUMBER OF FIGUARING TYPE                          *
C          INIT   :  FIRST CALL OR NOT                                 *
C          KDRAWF :  DRAWNING   FLAG                                   *
C          KDRAWN :  NUMBERING  FLAG                                   *
C          NUM    :  TABLE OF NUMBERS                                  *
C          NX     :  NUMBER OF RADIUS MESH                             *
C          NAPIN  :  NUMBER OF ARRAYS WITH PIN RODS                    *
C          NDPIN  :  NUMBER OF RADIUS FOR PIN ROD DIVIDING             *
C          IDIVP  :  DIVIDE FLAG                                       *
C          RX     :  TABLE OF RADIUS MESH                              *
C          RPP    :  TABLE OF RADIUS OF REGULAR ARRAYS OF PIN RODS     *
C          RDP    :  TABLE OF PIN DIVIDE RADIUS MESH                   *
C                                                                      *
C    -- LOCAL VARIABLES --                                             *
C                                                                      *
C         CC      :  TABLE OF PILLAR POSITION                          *
C         CYLINP  :  TABLE OF RADIUS OF ANNULAR PIN ROD                *
C         PINPOS  :  CENTER POSITION OF PIN RODS                       *
C         JNTPOS  :  JOINT TABLE OF CENTER POSITION OF PIN RODS        *
C                      AND TABLE OF PILLAR POSITION                    *
C         MAXNUM  :  DIMENSION OF JNTPOS                               *
C                                                                      *
C***********************************************************************
C
      PARAMETER ( NCIRCL =  50 )
      PARAMETER ( MAXPIN = 500 )
C
CM    REAL       JNTPOS(100)
      REAL       JNTPOS(MAXPIN)
C
C     DIMENSION  NUM(300),RX(50),RPP(20),RDP(NDPIN+1,100)
      DIMENSION  NUM(1000),RX(50),RPP(20),RDP(NDPIN+1,100)
CMOD  DIMENSION  CC(50),CYLINP(10,100),PINPOS(20),RPIN(100)
      DIMENSION  CC(MAXPIN),CYLINP(NCIRCL,MAXPIN)
      DIMENSION  PINPOS(NCIRCL),RPIN(MAXPIN)
C
      DATA  ZMAX  / 100. /,    EPS / 0.1 /,   PAI / 3.14159265 /
C
C
C
C
      NOUT1 = 6
      IF ( INIT.EQ.1 )  GO TO 100
C
C        *****  SCALING INPUT DATA FOR DRAWING FIGURE  *****
C
         NTPIN = (NAPIN*(NAPIN+1))/2
         RMAX  = RX(NX+1)
C
         IF(NX   .GE.MAXPIN) THEN
         WRITE(NOUT1,*) ' ** FATAL PROGRAMMING ERROR AT SUB(GEOM09) !!'
         WRITE(NOUT1,*) ' ** DIMENSION SIZE OF CC ARRAY IS OVER !!'
         WRITE(NOUT1,*) ' ** REQUESTED IS ',NX+1,
     +                  ' BUT RESERVED IS ',MAXPIN,' !! '             
         WRITE(NOUT1,*) ' ** PLEASE MODIFY SUB(GEOM09) !! '
         STOP
         ENDIF
C
         IF(NTPIN.GE.MAXPIN) THEN
         WRITE(NOUT1,*) ' ** FATAL PROGRAMMING ERROR AT SUB(GEOM09) !!'
         WRITE(NOUT1,*) ' ** DIMENSION SIZE OF JNTPOS ARRAY IS OVER !!'
         WRITE(NOUT1,*) ' ** REQUESTED IS ',NTPIN+1,
     +                  ' BUT RESERVED IS ',MAXPIN,' !! '             
         WRITE(NOUT1,*) ' ** PLEASE MODIFY SUB(GEOM09) !! '
         STOP
         ENDIF
C
      IF(NAPIN.GT.NCIRCL) THEN
         WRITE(NOUT1,*) ' ** FATAL PROGRAMMING ERROR AT SUB(GEOM09) !!'
         WRITE(NOUT1,*) ' ** DIMENSION SIZE OF PINPOS ARRAY IS OVER !!'
         WRITE(NOUT1,*) ' ** REQUESTED IS ',NAPIN,
     +                  ' BUT RESERVED IS ',NCIRCL,' !! '             
         WRITE(NOUT1,*) ' ** PLEASE MODIFY SUB(GEOM09) !! '
         STOP
         ENDIF
C
      IF(NDPIN.GE.NCIRCL) THEN
         WRITE(NOUT1,*) ' ** FATAL PROGRAMMING ERROR AT SUB(GEOM09) !!'
         WRITE(NOUT1,*) ' ** DIMENSION SIZE OF CYLINP ARRAY IS OVER !!'
         WRITE(NOUT1,*) ' ** REQUESTED IS ',NDPIN+1,
     +                  ' BUT RESERVED IS ',NCIRCL,' !! '             
         WRITE(NOUT1,*) ' ** PLEASE MODIFY SUB(GEOM09) !! '
         STOP
         ENDIF
C
C  ***   EXCLUDE RPP FROM RX  IF IDIVP=0
         NPC    = 1
         KOVER  = 0
C
         DO  10  I = 1,NX+1
            J=I-KOVER
C
            IF(IDIVP.EQ.0  ) GO TO 5
            IF(NPC.GT.NAPIN) GO TO 5
C
            IF(RPP(NPC).EQ.RX(I)) THEN
                        NPC=NPC+1
            IF(I.NE.1 .AND. I.NE.NX+1) THEN
                                       KOVER=KOVER+1
                                       GO TO 10
                                       ENDIF
                        ENDIF
    5    CONTINUE
         CC(J) = RX(I)/RMAX*ZMAX*2
   10    CONTINUE
C
         NXX=NX-KOVER
         DO  20  I = 1,NAPIN
            PINPOS(I) = RPP(I)/RMAX*ZMAX*2
   20    CONTINUE
C
         DO  30  J = 1,NTPIN
         DO  30  I = 1,NDPIN+1
            CYLINP(I,J) = RDP(I,J)/RMAX*ZMAX*2
   30    CONTINUE
         CC(NXX+1) = ZMAX*2
         RPIN(J) = CYLINP(NDPIN+1,J)
C
C        *****  MAKING POSITION TABLE FOR NUMBERING  *****
C
            MAXNUM = NXX
         DO  60  I = 1,NXX+1
            JNTPOS(I) = CC(I)
   60    CONTINUE
C
         IF ( IDIVP .EQ. 0 )  GO TO 100
            MAXNUM = NXX+NAPIN
            DO  70  I = 1,NAPIN
               JNTPOS(I+NXX+1) = PINPOS(I)
   70    CONTINUE
C
C
            IF ( PINPOS(1) .GT. EPS )  GO TO 72
               JNTPOS(1) = JNTPOS(MAXNUM+1)
               MAXNUM = MAXNUM-1
C
   72       CONTINUE
               DO  90  I = 1,MAXNUM+1
               IMIN = I
               RMIN = JNTPOS(I)
C
               DO  80  J = I,MAXNUM+1
                  IF ( RMIN .LT. JNTPOS(J) )  GO TO 80
                     IMIN = J
                     RMIN = JNTPOS(J)
   80             CONTINUE
C
               JNTPOS(IMIN) = JNTPOS(I)
               JNTPOS(I)    = RMIN
   90 CONTINUE
C
C
C     *****  DRAWNING OUTER LINES  *****
C
  100 CONTINUE
      ALEN = ZMAX*2.06
      CALL  PLOT  (ALEN,0.,3)
      CALL  PLOT  (0.,0.,2)
      CALL  PLOT  (0.,ALEN,2)
C
      ALEN = ZMAX*2.00
      CALL  PLOT  (0.,ALEN,3)
      CALL  PLOT  (ALEN,ALEN,2)
      CALL  PLOT  (ALEN,0.,2)
C
C
C
      IF ( KDRAWF .EQ. 0 )  RETURN
C        *****************************
C        *****  DRAWNING PILLAR  *****
C        *****************************
         DO  110  I = 2,NXX
            CALL  PLOT (CC(I),0.,3)
            CALL  PLOT (CC(I),ALEN,2)
            CALL  PLOT (0.,CC(I),3)
  110       CALL  PLOT (ALEN,CC(I),2)
C        *******************************
C        *****  DRAWNING PIN RODS  *****
C        *******************************
         DO  120  I = 1,NAPIN
         DO  120  J = 1,NAPIN
            IJ=LOCF(I,J,NAPIN,NTPIN)
         DO  120  K = 2,NDPIN+1
            X = PINPOS(I)
            Y = PINPOS(J)
            IF ( ABS(X) .GE. EPS )  GO TO 111
               THS = -PAI/2
               THE =  PAI/2
               IF ( ABS(Y) .LT. EPS)   THS = 0
               GO TO 120
  111       IF ( ABS(Y) .GE. EPS )  GO TO 112
               THS =   0
               THE = PAI
               GO TO 120
  112       CONTINUE
               THS =   0
               THE = PAI*2
  120       CALL  CIRCLG (X,Y,CYLINP(K,IJ),THS,THE)
         IF ( IDIVP .EQ. 0 )  GO TO 200
C           ***********************************
C           *****  DRAWNING DIVIDE LINES  *****
C           ***********************************
            IF ( PINPOS(1) .GE. EPS )  GO TO 122
               IS = 2
               SSPOS = RPIN(1)
               GO TO 125
  122       CONTINUE
               IS = 1
               SSPOS = 0.0
  125       DO  140  I = IS,NAPIN
               SPOS = SSPOS
               DO  130  J = IS,NAPIN
               K=LOCF(I,J,NAPIN,NTPIN)
                  EPOS = PINPOS(J)-RPIN(K)
                  CALL  PLOT  (SPOS,PINPOS(I),3)
                  CALL  PLOT  (EPOS,PINPOS(I),2)
                  CALL  PLOT  (PINPOS(I),SPOS,3)
                  CALL  PLOT  (PINPOS(I),EPOS,2)
                  SPOS = EPOS+2*RPIN(K)
  130             CONTINUE
               EPOS = ZMAX*2
               CALL  PLOT  (SPOS,PINPOS(I),3)
               CALL  PLOT  (EPOS,PINPOS(I),2)
               CALL  PLOT  (PINPOS(I),SPOS,3)
  140          CALL  PLOT  (PINPOS(I),EPOS,2)
C
C
C
  200    IF ( KDRAWN .EQ. 0 )  RETURN
C           *****  NUMBERING  *****
            L = 0
            DO  250  I = 1,MAXNUM
            DO  250  J = I,MAXNUM
               X1 = JNTPOS( J )
               X2 = JNTPOS(J+1)
               Y1 = JNTPOS( I )
               Y2 = JNTPOS(I+1)
               X = (X1+X2)/2
               Y = (Y1+Y2)/2
               DO  210  KX = 1,NAPIN
               DO  210  KY = 1,NAPIN
               K=LOCF(KX,KY,NAPIN,NTPIN)
                  PX = PINPOS(KX)
                  PY = PINPOS(KY)
                  IF ( PX.LE.X1-EPS  .OR.  PX.GE.X2+EPS  .OR.
     @                 PY.LE.Y1-EPS  .OR.  PY.GE.Y2+EPS )  GO TO 210
                     IF ( PX.LE.X1+EPS  .OR.  PX.GE.X2-EPS  .OR.
     @                    PY.LE.Y1+EPS  .OR.  PY.GE.Y2-EPS )  GO TO 202
                        XC = X2
                        YC = Y2
                        GO TO 205
  202                CONTINUE
                        XC = X2
                        IF ( ABS(PX-X1) .LE. EPS )  XC = X2
                        IF ( ABS(PX-X2) .LE. EPS )  XC = X1
                        IF ( ABS(PY-Y1) .LE. EPS )  YC = Y2
                        IF ( ABS(PY-Y2) .LE. EPS )  YC = Y1
  205                R  = SQRT( (XC-PX)**2+(YC-PY)**2 )
                     XT = PX+RPIN(K)*(XC-PX)/R
                     YT = PY+RPIN(K)*(YC-PY)/R
                     X  = (XT+XC)/2
                     Y  = (YT+YC)/2
                     GO TO 240
  210             CONTINUE
  240          L = L+1
               CALL INUM (X,Y,IFTYPE,6-IFTYPE,NUM(L))
  250          CONTINUE
            DO  260  I = 1,NAPIN
            DO  260  J = I,NAPIN
               K=LOCF(I,J,NAPIN,NTPIN)
               L = L+1
               X = PINPOS(J)
               Y = PINPOS(I)
               CALL  INUM (X,Y,IFTYPE,6-IFTYPE,NUM(L))
               L = L+NDPIN-1
               Y = Y + (CYLINP(NDPIN,K)+CYLINP(NDPIN+1,K))/2
  260          CALL  INUM (X,Y,IFTYPE,6-IFTYPE,NUM(L))
C
            RETURN
      END
