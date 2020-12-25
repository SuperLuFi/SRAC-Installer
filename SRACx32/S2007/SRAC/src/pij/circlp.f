C             CIRCLP                             DATE=94.06.16
C
C***********************************************************************
C                                                                      *
C       CIRCLP    : DRAWNING CIRCL WITH PIN RODS                       *
C                                                                      *
C       MODULE NO.  :                                                  *
C       MODULE TYPE : SUBROUTINE                                       *
C                                                                      *
C***********************************************************************
C                                                                      *
C    -- CALLED BY --                                                   *
C                                                                      *
C        GEOM10,GEOM12                                                 *
C                                                                      *
C    -- CALLS --                                                       *
C                                                                      *
C        CIRCLG                                                        *
C                                                                      *
C......................................................................*
C                                                                      *
C    -- INPUT --                                                       *
C                                                                      *
C       (ARGUMENT)                                                     *
C          INST   :  FIGURAING METHOD                                  *
C                       = 0   NO OPERATION                             *
C                       = 1   DRAWNING CIRCLE WITH JUNPING PIN RODS    *
C                       = 2   DRAWNING PERFECT CIRCLE                  *
C          RR     :  RADIUS OF ANNULAR CYLINDER WITH PIN RODS          *
C          NPIN   :  NUMBER OF PIN RODS ON CYLINDER                    *
C          RPIN   :  RADIUS OF PIN ROD                                 *
C          THETA  :  ANGLE OF PIN POSITION                             *
C          NY     :  NUMBER OF DIVIDE LINES                            *
C          TY     :  ANGLE OF DIVIDE LINES                             *
C                                                                      *
C                                                                      *
C***********************************************************************
C
      SUBROUTINE  CIRCLP (INST,RR,NPIN,RPIN, THETA,NY,TY)
      DIMENSION  THETA(10),TY(10)
      DATA  PAI / 3.14159265 /,   EPS / 0.1 /
C
C
C
C
C
      IF ( INST .EQ. 1 )  GO TO 100
      IF ( INST .EQ. 2 )  GO TO 200
      RETURN
C
C
C
C
C
C
  100    CONTINUE
C           *****  DRAWNING CIRCLE WITH SKIPPING ALL PIN RODS  *****
            X  = RR - RPIN**2/(2*RR)
            SS = ACOS(X/RR)
            DO  140  J = 1,NPIN
               IF ( J .NE. NPIN )  GO TO 120
                  THS = THETA(NPIN)+SS
                  THE = THETA(1)-SS+2*PAI
                  GO TO 140
  120          CONTINUE
                  THS = THETA(J)+SS
                  THE = THETA(J+1)-SS
  140       CALL  CIRCLG (0.,0., RR,THS,THE)
            RETURN
  200 CONTINUE
         CALL CIRCLG (0.,0.,  RR,0.,2*PAI)
         RETURN
      END
