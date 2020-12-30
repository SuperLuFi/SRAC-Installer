      SUBROUTINE  SRTCYL ( NAPIN,NTPIN,NDPIN,CYLINP,RRPIN,THETA,
     @                     CYLINA,NPIN,NPINT)
C
C***********************************************************************
C                                                                      *
C       SRTCYL    : SORT CYLINDER(WITH PIN RODS) DATA                  *
C                                                                      *
C                                                                      *
C       MODULE NO.  :                                                  *
C       MODULE TYPE : SUBROUTINE                                       *
C       DATE        : 1984.03.18                                       *
C                                                                      *
C***********************************************************************
C                                                                      *
C    -- CALLED BY --                                                   *
C                                                                      *
C        GEOM11                                                        *
C                                                                      *
C                                                                      *
C......................................................................*
C                                                                      *
C    -- INPUT --                                                       *
C                                                                      *
C       (ARGUMENT)                                                     *
C          NTPIN  :  NUMBER OF TOTAL PIN RODS                          *
C          NDPIN  :  NUMBER OF RADIUS FOR PIN ROD DIVIDING             *
C                                                                      *
C    -- OUTPUT --                                                      *
C                                                                      *
C       (ARGUMENT)                                                     *
C          NAPIN  :  NUMBER OF ARRAYS OF PIN RODS                      *
C          CYLINA :  TABLE OF RADIUS OF ARRAYS OF PIN RODS             *
C          NPIN   :  NUMBER OF PIN RODS ON CYLINDER                    *
C          NPINT  :  TOTOAL NUMBER OF PIN RODS                         *
C                                                                      *
C    -- IN/OUT --                                                      *
C                                                                      *
C       (ARGUMENT)                                                     *
C          RRPIN  :  TABLE OF RADIUS OF POSITION OF PIN RODS           *
C          CYLINP :  TABLE OF PIN DIVIDE RADIUS MESH                   *
C          THETA  :  TABLE OF ANGLE OF POSITION VECTOR OF PIN RODS     *
C                                                                      *
C***********************************************************************
C
      DIMENSION  CYLINA(1),RRPIN(1),THETA(1)
C     DIMENSION  NPIN(50),CYLINP(10,50),NPINT(50)
      DIMENSION  NPIN(50),CYLINP(50,500),NPINT(50)
C
      DATA  EPS / 0.1 /
C
C
C
C
C
C
C        DO  40  I = 1,NTPIN
C           RMIN = RRPIN(I)
C           SMIN = THETA(I)
C           IMIN = I
C           IF ( I .GT. NTPIN-1 )   GO TO 32
C           DO  30  J = I+1,NTPIN
C              IF ( RRPIN(J) .GE. RMIN-EPS  .OR.
C    @              THETA(J) .GE. SMIN )      GO TO 30
C                 RMIN = RRPIN(J)
C                 SMIN = THETA(J)
C                 IMIN = J
C  30          CONTINUE
C  32       RRPIN(IMIN) = RRPIN(I)
C           RRPIN(I)    = RMIN
C           THETA(IMIN) = THETA(I)
C           THETA(I)    = SMIN
C           DO  35  K = 1,NDPIN
C              DUMMY          = CYLINP(K,IMIN)
C              CYLINP(K,IMIN) = CYLINP(K,I)
C  35          CYLINP(K,I)    = DUMMY
C  40       CONTINUE
C
      NAPIN = 1
      CYLINA(NAPIN) = RRPIN(1)
      NPIN(NAPIN)   = 0
      NPINT(NAPIN)  = 0
C
      DO  50  I = 1,NTPIN
         IF ( ABS(CYLINA(NAPIN)-RRPIN(I)) .GE. EPS )  GO TO 45
            NPIN(NAPIN) = NPIN(NAPIN)+1
            GO TO 50
   45    CONTINUE
            NAPIN = NAPIN+1
            CYLINA(NAPIN) = RRPIN(I)
            NPIN(NAPIN)   = 1
            NPINT(NAPIN)  = NPINT(NAPIN-1)+NPIN(NAPIN-1)
   50    CONTINUE
C
      RETURN
      END
