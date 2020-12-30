C             HEXAGP              LEVEL=6        DATE=84.03.08
C
C***********************************************************************
C                                                                      *
C       HEXAGP    : DRAWNING HEXAGON CHOPPED BY PIN RODS*
C                                                                      *
C                                                                      *
C       MODULE NO.  :                                                  *
C       MODULE TYPE : SUBROUTINE                                       *
C       DATE        : 1984.03.05                                       *
C                                                                      *
C***********************************************************************
C                                                                      *
C    -- CALLED BY --                                                   *
C                                                                      *
C        GEOM14                                                        *
C                                                                      *
C    -- CALLS --                                                       *
C                                                                      *
C        CIRCLG,HEXP                                                   *
C                                                                      *
C......................................................................*
C                                                                      *
C    -- INPUT --                                                       *
C                                                                      *
C       (ARGUMENT)                                                     *
C          INST   :  FIGURING PATTERN                                  *
C                       = 0   NO OPERATION                             *
C                       = 1   DRAW  HEXAGO CHOPPED BY PIN ROD
C                       = 2   DRAW  FULL HEXAGON
C          RR     :  HALF OF SIDE-TO-SIDE SPACING
C          NPIN   :  NUMBER OF PIN RODS ON HEXAGON                     *
C          RPIN   :  RADIUS OF PIN ROD                                 *
C          RPA    :  RADIAL POSITION OF PIN RODS                       *
C          THETA  :  ANGULAR POSITION OF PIN RODS                      *
C          NY     :  NUMBER OF DIVIDE LINES                            *
C          TY     :  ANGLE OF DIVIDE LINES                             *
C                                                                      *
C                                                                      *
C***********************************************************************
C
      SUBROUTINE  HEXAGP (INST,RR,NPIN,RPIN,RPA,THETA)
      DIMENSION  THETA(200),RPA(200)
C
C
C
C
      IF(RR.EQ.0) RETURN
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
C           *****  DRAW HEXAGON CHOPPED BY PIN RODS  *****
            RATIO= RPIN/RR*0.8660254*FLOAT(NPIN/6)
            CALL NEWPEN(1)
            X2=RPA(NPIN)*COS(THETA(NPIN))
            Y2=RPA(NPIN)*SIN(THETA(NPIN))
            DO  140  J = 1,NPIN
               X1=X2
               Y1=Y2
               X2=RPA(J)*COS(THETA(J))
               Y2=RPA(J)*SIN(THETA(J))
C
               X3=X1+(X2-X1)*RATIO
               Y3=Y1+(Y2-Y1)*RATIO
               X4=X2-(X2-X1)*RATIO
               Y4=Y2-(Y2-Y1)*RATIO
               CALL PLOT(X3,Y3,3)
               CALL PLOT(X4,Y4,2)
  140       CONTINUE
            RETURN
C   ****  DRAW FULL HEXAGON ****
  200    CALL HEXP   (0.,0.,  RR,1       )
         RETURN
      END
