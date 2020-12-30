C             ORGSET
C
C***********************************************************************
C                                                                      *
C       ORGSET    : ORIGIN SETTING                                     *
C                                                                      *
C                                                                      *
C       MODULE NO.  :                                                  *
C       MODULE TYPE : SUBROUTINE                                       *
C       DATE        : 1994.05.11                                       *
C                                                                      *
C***********************************************************************
C                                                                      *
C    -- CALLED BY --                                                   *
C                                                                      *
C         GEOMTY                                                       *
C                                                                      *
C    -- CALLS --                                                       *
C                                                                      *
C         GTITLE                                                       *
C                                                                      *
C......................................................................*
C                                                                      *
C    -- INPUT --                                                       *
C                                                                      *
C       (ARGUMENT)                                                     *
C                 :                                                    *
C                                                                      *
C     SET AND RESET GEOMETRY ORIGIN FOR EACH FIGURE.                   *
C                                                                      *
C      IGT .... GEOMETRY TYPE NUMBER. ( 1 .. 16)                       *
C      IFTYPE.. DRAWING TYPE NUMBER. (1,2,3 ... 1,2 AND 5 FIG./FRAME)  *
C                                                                      *
C         IGT = 2,8,9,13,16.. BOTTOM OF THE LEFT CORNER.               *
C         IGT = 1,3,4,5,6,7,10,11,12,14..... CENTER OF THE FIGURE.     *
C                                                                      *
C      NFIG.... NUMBER OF FIGURE                                       *
C            RESET 1  BY PAGE ADBANCE                                  *
C            IF NFIG = 0   FIRST  ENTRY                                *
C                                                                      *
C                                                                      *
C                                                                      *
C    -- IN/OUT --                                                      *
C                                                                      *
C       (ARGUMENT)                                                     *
C          NFIG   :                                                    *
C                                                                      *
C    -- LOCAL VARIABLES --                                             *
C                                                                      *
C          XORG,YORG  :  POSITION OF ORGIN  (REAL SIZE)                *
C                                                                      *
C                                                                      *
C***********************************************************************
C
C
C
C
C
C
      SUBROUTINE  ORGSET (IGT,KFIG,IFTYPE,NFIG,TITLE)
      DIMENSION  ORGX(5),ORGY(5),SCALE(3),TITLE(18)
      DATA ORGX /  27.5,  142.0,  257.5,   27.5,  142.0 /
      DATA ORGY / 172.2,  172.2,  172.2,   57.3,   57.3 /
      DATA SCALE / 1.0, 0.75, 0.5  /
C
C     ****   PAGING CHECK.
C
      IF ( NFIG .NE. 10 )  GO TO 11
         CALL  PLOT   ( 0.0,  0.0,  666 )
         CALL  FACTOR ( 1.0 )
         NFIG = 0
   11 IF ( NFIG .NE. 0 )  GO TO 12
         CALL  GTITLE (0,0,TITLE)
         XORG = 0
         YORG = 0
   12 NFIG = NFIG+1
C
C     ****   RESET ORIGIN.
C
      CALL  FACTOR ( 1.0 )
      CALL  PLOT  (-XORG,-YORG,-3)
C
C
C *****   SELECTING  ORIGIN  BY NUMBER OF FIGURE
C
      IF (IFTYPE .NE. 1 )  GO TO 21
C        ***   ONE FIGURE    IN ONE PAGE  ***
         XORG = 142
         YORG = 115
   21 IF (IFTYPE .NE. 2 )  GO TO 22
C        ***   TWO FIGURES   IN ONE PAGE  ***
         XORG = NFIG*172-116
         YORG = 115
   22 IF (IFTYPE .NE. 3 )  GO TO 30
C        ***   FIVE FIGURES  IN ONE PAGE  ***
         XORG = ORGX(NFIG)
         YORG = ORGY(NFIG)
C
C  *****   SETING ORIGIN  *****
C
   30 CALL  PLOT   (XORG,YORG,-3)
      CALL  FACTOR (SCALE(IFTYPE))
      CALL  GTITLE  (KFIG,IFTYPE,TITLE)
      IF (IGT.NE.2 .AND. IGT.NE.8 .AND.
     @    IGT.NE.9 .AND. IGT.NE.13 .AND. IGT.NE.16)  GO TO 40
C        ***   RESETING  ORIGIN  FROM CENTER  TO CORNER   ***
         XORG = XORG-100*SCALE(IFTYPE)
         YORG = YORG-100*SCALE(IFTYPE)
         CALL  PLOT   ( -100.0,  -100.0,  -3)
C
   40 IF ( NFIG.EQ.1  .AND.  IFTYPE.EQ.1   .OR.
     @     NFIG.EQ.2  .AND.  IFTYPE.EQ.2         )  NFIG = 10
C
C
      RETURN
      END
