      SUBROUTINE  NUMCYL (II,CYLINJ,K1,K2,CYLINA,NPIN,NPINT,THETA,
     @                    RP1,RP2 ,NY,TY, IFTYPE,ISIZE,NUM,LL)
C
C***********************************************************************
C                                                                      *
C       NUMCYL    : NUMBERNING  ANNULAR CYLINDER                       *
C                                                                      *
C                                                                      *
C       MODULE NO.  :                                                  *
C       MODULE TYPE : SUBROUTINE                                       *
C       DATE        : 1994.06.16                                       *
C                                                                      *
C***********************************************************************
C                                                                      *
C    -- CALLED BY --                                                   *
C                                                                      *
C        GEOM12                                                        *
C                                                                      *
C    -- CALLS --                                                       *
C                                                                      *
C        INUM,CHKPIN                                                   *
C                                                                      *
C......................................................................*
C                                                                      *
C    -- INPUT --                                                       *
C                                                                      *
C       (ARGUMENT)                                                     *
C          II     :  INDICATER FOR OUTER CIRCLE OF ANNULAR             *
C          CYLINJ :  TABLE OF RADIUS FOR ANNULAR                       *
C          K1     :  INDICATER FOR INNER CIRCLE WITH PIN RODS          *
C          K2     :  INDICATER FOR OUTER CIRCLE WITH PIN RODS          *
C          CYLINA :  RADIUS'S TABLE OF CIRCLE WITH PIN RODS            *
C          NPIN   :  NUMBER OF PIN RODS ON SAME CIRCLE                 *
C          NPINT  :  TABLE OF NUMBERS                                  *
C          THETA  :  TABLE OF ANGLE OF PIN RODS' POSITION              *
C          RP1    :  RADIUS OF PIN RODS ON INNER CIRCLE                *
C          RP2    :  RADIUS OF PIN RODS ON OUTER CIRCLE                *
C          NY     :  NUMBER OF DIVIDING LINE IF =1 MODIFIED TO 0
C          TY     :  TABLE OF END ANGLE OF DIVIDED REGION              *
C          IFTYPE :  FIGURE   TYPE  =1,2,3                             *
C          ISIZE  :  SIZE OF NUMBER                                    *
C          NUM    :  TABLE OF NUMBER                                   *
C          LL     :  POINTER OF NUMBER                                 *
C                                                                      *
C    -- LOCAL VALUE --                                                 *
C                                                                      *
C          R1     :  RADIUS OF OUTER CIRCLE                            *
C          R2     :  RADIUS OF INNER CIRCLE                            *
C          SETA1  :  TABLE OF PIN RODS' ANGLE  IN LIMIT ANGLE          *
C          SETA2  :  TABLE OF PIN RODS' ANGLE  IN LIMIT ANGLE          *
C          N1     :  NUMBER PIN RODS IN LIMIT ANGLE                    *
C          N2     :  NUMBER PIN RODS IN LIMIT ANGLE                    *
C                                                                      *
C***********************************************************************
C
C     DIMENSION  TY(30),THETA(50),NPIN(50),NPINT(50),NUM(300)
      DIMENSION  TY(30),THETA(50),NPIN(50),NPINT(50),NUM(1000)
      DIMENSION  CYLINJ(100),CYLINA(50),SETA1(40),SETA2(40)
C
      DATA   PAI / 3.14159265 /
C  
C
C
      CALL CLEA(SETA1,40,0.)
      CALL CLEA(SETA2,40,0.)
C
      L      =  LL
      INDEXY =  1
      ITY0   =  0
      THS    = TY(NY)-2.*PAI
      IF(TY(1).EQ.0.)   THEN
      ITY0   =  1
      THS    = TY(1)
                        ENDIF
C
   50 CONTINUE
C
      IF ( NY .LE. 1 )  THEN
         THS = 0
         THE = PAI/2
                        ELSE
         THE = TY(INDEXY+ITY0)
                IF(INDEXY.EQ.NY) THEN
                IF(ITY0.EQ.0) THEN
                THE=TY(NY)
                              ELSE
                THE=2.*PAI
                              ENDIF
                                 ENDIF
                         ENDIF
         R1 = CYLINJ(II-1)
         R2 = CYLINJ( II )
         IF ( II .EQ. 2  .AND.  NY .LE. 1 )  R2 = 0
         R = (R1+R2)/2
         X = R*COS((THS+THE)/2)
         Y = R*SIN((THS+THE)/2)
C
      CALL  INUM (X,Y,IFTYPE,ISIZE,NUM(L))
      INDEXY = INDEXY+1
      THS    = THE
      L      = L+1
      IF ( INDEXY .LE. NY )   GO TO 50
C
      RETURN
      END
