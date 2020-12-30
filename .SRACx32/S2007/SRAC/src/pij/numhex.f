      SUBROUTINE  NUMHEX (II,CYLINJ,K1,K2,CYLINA,NPIN,NPINT,THETA,
     @                    RP1,RP2 ,NY,TY, IFTYPE,ISIZE,NUM,LL)
C
C***********************************************************************
C                                                                      *
C       NUMHEX    : NUMBERNING  HEXAGON
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
C          NY     :  NUMBER OF DIVIDE LINE                             *
C          TY     :  TABLE OF END ANGLE OF DIVIDE REGION               *
C          IFTYPE :  FIGURING TYPE  =1,2,3                             *
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
      DATA   PAI / 3.14159265 /,   EPS / 1.0 /
C
C
C
      L = LL
      INDEXY = 1
C
    1 IF ( NY .NE. 0 )  GO TO 50
         THS = 0
         THE = PAI/2
         GO TO 100
C
   50 CONTINUE
         IF ( INDEXY .EQ. 1 )  THS = TY(NY)-2*PAI
         IF ( INDEXY .NE. 1 )  THS = TY(INDEXY-1)
         THE = TY(INDEXY)
C
  100 N1 = 0
      N2 = 0
      IF ( K1 .NE. 0 )  CALL CHKPIN (NPIN(K1),THETA(1+NPINT(K1)),
     @                                             THS,THE,N1,SETA1(2))
      IF ( K2 .NE. 0 )  CALL CHKPIN (NPIN(K2),THETA(1+NPINT(K2)),
     @                                             THS,THE,N2,SETA2(2))
      INST = 0
C
      IF ( N1 .NE. 0 )  INST = INST+1
      IF ( N2 .NE. 0 )  INST = INST+2
      IF ( N1+N2 .EQ. 1 )  INST = 9
      IF ( INST .NE. 9 )  GO TO 200
C
         IF ( N1 .EQ. 1 )  SS = SETA1(2)
         IF ( N2 .EQ. 1 )  SS = SETA2(2)
         IF ( ABS(THS-SS) .GT. ABS(THE-SS) )  THE = SS
         IF ( ABS(THS-SS) .LE. ABS(THE-SS) )  THS = SS
         SETA1(2) = THS
         SETA2(2) = THS
         SETA1(3) = THE
         SETA2(3) = THE
         INST = N1+N2*2
C
  200 IF ( INST .NE. 3  )  GO TO 500
         SETA1(1)    = THS
         SETA1(N1+2) = THE
         THS = SETA1(2)
         THE = SETA1(3)
         SS=0.
               IF( R2.NE.0.)
     @   SS = 0.4*(THE-THS)*R1/R2
         CALL CHKPIN (NPIN(K2),THETA(1+NPINT(K2)),
     @              (THS+THE)/2-SS,(THS+THE)/2+SS,N2,SETA2)
         IF ( N2 .EQ. 0 )  GO TO 450
         IF ( N2 .NE. 1 )   GO TO  390
            SSC = 30/(CYLINJ(II-1)+CYLINJ(II))
            IF ( (SETA2(1)-THS) .LE. SSC )  GO TO 310
               THE = SETA2(1)
               GO TO 450
C
  310       IF ( (THE-SETA2(1)) .LE. SSC )  GO TO 390
               THS = SETA2(1)
               GO TO 450
C
  390       SMAX = 0
            DO  400  I =1,N1+1
               TH1 = SETA1(I)
               TH2 = SETA1(I+1)
               IF ( TH2-TH1 .LE. SMAX )  GO TO 400
               SS=0.
               IF( R2.NE.0.)
     @            SS  = 0.4*(TH2-TH1)*R1/R2
                  CALL CHKPIN (NPIN(K2),THETA(1+NPINT(K2)),
     @                  (TH1+TH2)/2-SS,(TH1+TH2)/2+SS,N2,SETA2)
                  IF ( N2 .NE. 0 )  GO TO 400
                     THS = TH1
                     THE = TH2
                     SMAX = TH2-TH1
  400          CONTINUE
C
  450    INST = 0
C
  500 IF ( INST .NE. 0 )  GO TO 600
         R1 = CYLINJ(II-1)
         R2 = CYLINJ( II )
         IF ( II .EQ. 2  .AND.  NY .EQ. 0 )  R2 = 0
         R = (R1+R2)/2
         X = R*COS((THS+THE)/2)
         Y = R*SIN((THS+THE)/2)
C
  600    IF ( INST .NE. 1 )  GO TO 700
            R1 = CYLINA(K1)
            R2 = CYLINJ(II)
            SS = (SETA1(3)+SETA1(2))/2
            S  = (SETA1(3)-SETA1(2))/2
            R0 = 0.5*(R1**2+R2**2-2*R1*R2*COS(S)-RP1**2)
     @                                          /(RP1+R2-R1*COS(S))
            RS = AMIN1(R0,(R2-R1)/2)
            R  = R2-RS
            X = R*COS(SS)
            Y = R*SIN(SS)
C
  700 IF ( INST .NE. 2  )  GO TO 1000
         R1 = CYLINJ(II-1)
         R2 = CYLINA(K2)
         SS = (SETA2(3)+SETA2(2))/2
         S  = (SETA2(3)-SETA2(2))/2
         R0 = 0.5*(R1**2+R2**2-2*R1*R2*COS(S)-RP2**2)
     @                                         /(RP2-R1+R2*COS(S))
         RS = AMIN1(ABS(R0),(R2-R1)/2)
         R  = R1+RS
         X = R*COS(SS)
         Y = R*SIN(SS)
C
 1000 CALL  INUM (X,Y,IFTYPE,ISIZE,NUM(L))
      INDEXY = INDEXY+1
      L = L+1
      IF ( INDEXY .LE. NY )   GO TO 1
C
      RETURN
      END
