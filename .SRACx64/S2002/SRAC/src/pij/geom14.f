      SUBROUTINE  GEOM14 (IFTYPE,INIT,KDRAWF,KDRAWN,NUM,
     @           NX,NAPIN,NDPIN,NTPIN,IDIVP, RX,RPP,RDP,NPIN,THETA)
C
C***********************************************************************
C                                                                      *
C       GEOM14    : HEXAGONAL CELL WITH HEXAGONAL ARRAY OF PIN RODS    *
C                                                                      *
C                                                                      *
C       MODULE NO.  :                                                  *
C       MODULE TYPE : SUBROUTINE                                       *
C       DATE        : 1994.05.22                                       *
C                                                                      *
C***********************************************************************
C                                                                      *
C    -- CALLED BY --                                                   *
C                                                                      *
C        GEOMTY                                                        *
C                                                                      *
C    -- CALLS --                                                       *
C                                                                      *
C        CIRCLG,NUMPIN,NUMCYL,HEXAGP                                   *
C......................................................................*
C                                                                      *
C    -- INPUT --                                                       *
C                                                                      *
C       (ARGUMENT)                                                     *
C          IFTYPE :  NUMBER OF FIGUARING TYPE                          *
C          INIT   :  FIRST CALL OR NOT                                 *
C          KDRAWN :  NUMBERING  FLAG                                   *
C          KDRAWF :  DRAWNING   FLAG                                   *
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
C         CYLINA  :  TABLE OF RADIUS OF ANNULAR WITH PIN RODS (GRAPHIC)*
C         CYLINP  :  TABLE OF PIN DIVIDE RADIUS MESH  (GRAPHIC         *
C         CYLINJ  :  TABLE OF RADII OF HEXAGONS (GRAPHIC)              *
C         NPINT   :  TABLE OF POINTER FOR PIN DATA                     *
C                                                                      *
C***********************************************************************
C
C     DIMENSION  NUM(300),RX(20),RPP(3000),RDP(7),NPIN(10),THETA(300)
C     DIMENSION  CYLINP(6),CYLINA(10)
      DIMENSION  NUM(1000),RX(20),RPP(3000),RDP(7),NPIN(10),THETA(300)
      DIMENSION  CYLINP(50),CYLINA(10)
      DIMENSION  NPINT(10),CYLINJ(30)
      DIMENSION RPA(300),AFR(7),ALR(7),AFC(6),ALC(6)
C
      DATA RMAX  /  95. /,    PI / 3.14159265 /,   EPS / 1.0 /
      DATA ALR/0.0001,0.3334,0.6667,1.0001,1.3334,1.6667,2.0001/
      DATA AFR/-0.0001,0.3333,0.6666,0.9999,1.3333,1.6666,1.9999/
      DATA AFC/ 0.6667,1.0000,1.3333,1.6667,0.0000,0.3333/
      DATA ALC/ 1.6667,2.0000,0.3333,0.6667,1.0000,1.3333/
C
C
C
      NOUT1 = 6
      IF ( INIT .EQ. 1 )  GO TO 100
C
C  ** TEST TOTAL NUMBER OF PIN RODS
C
         DO  10  I = 1,NX+1
            CYLINJ(I) = RX(I)/RX(NX+1)*RMAX
   10    CONTINUE
C
      IF(NAPIN.EQ.0) GO TO 100
      NT=0
C
      DO 25 I=1,NAPIN
      NT=NT+NPIN(I)
   25 CONTINUE
            NPINT(1) = 0
C
            DO  130  I = 2,NAPIN
  130       NPINT(I) = NPINT(I-1)+NPIN(I-1)
C
      IF(NT.NE.NTPIN) THEN
                      WRITE(NOUT1,3013) 'NPIN TOTAL',NT,NTPIN
                      STOP
                      ENDIF
C
 3013 FORMAT(10X,'** UNMATCH IN ',A,2I5 )
C
         DO  20  I = 1,NAPIN
   20       CYLINA(I) = RPP(NPINT(I)+1)/RX(NX+1)*RMAX
C
         DO  30  I = 2,NDPIN+1
   30       CYLINP(I-1) = RDP(I)/RX(NX+1)*RMAX
C
         RPIN = CYLINP(NDPIN)
C
         DO  40  I = 1,NTPIN
   40       RPA   (I) = RPP(I)/RX(NX+1)*RMAX
C
C
C ***    FIGURE OUTER BOUNDARY
  100 CALL  HEXP   (0.,0., CYLINJ(NX+1),3)
C
      IF ( KDRAWF .EQ. 0 )  RETURN
C
C
C
         DO  110  I = 2,NX
C  ***   FIGURE INNER HEXAGON BY RX
  110       CALL  HEXP   (0.,0., CYLINJ(I),2)
      IF (NTPIN.EQ.0) GO TO 345
C
         DO  120  I = 1,NTPIN
               AF=0.
               AL=2.*PI
               R=RPA(I)
C
         IF(R.GE.CYLINJ(NX+1)*.9999)     THEN
             DO 11 J=1,6
         IF(THETA(I).GT.AFR(J)*PI .AND. THETA(I).LT.AFR(J+1)*PI)
     &         AF=AFC(J)*PI
C
         IF(THETA(I).GT.ALR(J)*PI .AND. THETA(I).LT.ALR(J+1)*PI)
     &         AL=ALC(J)*PI
   11         CONTINUE
                                          ENDIF
C
         IF(AF.GT.AL) AF=AF-2.*PI
               X  = R*COS(THETA(I))
               Y  = R*SIN(THETA(I))
               DO  120  K = 1,NDPIN
C  ***   FIGURE  PIN ROD
               CALL CIRCLG (X,Y, CYLINP(K),AF,AL)
  120    CONTINUE
C
C   ****  FIGURE HEXAGON BY RPP
C           DO  340  I = 1,NAPIN
C              CALL HEXAGP (IDIVP,CYLINA(I),NPIN(I),CYLINP(NDPIN),
C    @                      RPA(1+NPINT(I)),THETA(1+NPINT(I)))
C 340          CONTINUE
C
C
C
C
C
  345    IF ( KDRAWN .EQ. 0 )  RETURN
C           *****  NUMBERING PIN RODS  *****
            IF ( NDPIN .EQ. 1 )  ISIZE = 6-IFTYPE
            IF ( NDPIN .NE. 1 )  ISIZE = 5-IFTYPE
            IF ( IDIVP .NE. 2 )  INST = 1
            IF ( IDIVP .EQ. 2 )  INST = 2
            L = 1
C
            DO  320  I = 1,NAPIN
                  CALL  NUMPIN (INST,RPA(1+NPINT(I)),THETA(1+NPINT(I)),
     @                             CYLINP,NDPIN,L,NUM,IFTYPE,ISIZE)
C
                  IF ( INST .EQ. 2 .AND. NPIN(I).GT.1)  THEN
                                                        L = L+2*NDPIN
                                                        ELSE
                                                        L = L+NDPIN
                                                        END IF
  320          CONTINUE
C
C
C           *****  NUMBERING HEXAGONAL CYLINDER  *****
C     WRITE(NOUT1,*) ' CRX=',(CYLINJ(I),I=1,NX)
C     WRITE(NOUT1,*) ' CRP=',(CYLINJ(I),I=1,NAPIN)
C     WRITE(NOUT1,*) ' REG=',(NUM(I),I=1,L)
C
            DO  500  I = 2,NX+1
               K0 = 0
               K1 = 0
               K2 = 0
C
               DO  460  J = 1,NAPIN
                  IF ( ABS(CYLINA(J)-CYLINJ(I-1)) .LT. EPS )  K1 = J
                  IF ( ABS(CYLINA(J)-CYLINJ( I )) .LT. EPS )  K2 = J
                  IF ( CYLINA(J) .GT. CYLINJ(I-1)+EPS .AND.
     @                 CYLINA(J) .LT. CYLINJ( I )-EPS      )  K0 = J
  460             CONTINUE
               IF ( K0 .NE. 0 )  K1 = K0
C
            IF(I.EQ.2 .AND.  CYLINA(1).EQ.0.) THEN
               R= (RPIN+CYLINJ(2))/2.
               SS=PI/6.
               X=R*COS(SS)
               Y=R*SIN(SS)
             CALL INUM(X,Y,IFTYPE,ISIZE,NUM(L))
               GO TO 500
                                              END IF
C
               CALL  NUMHEX(I,CYLINJ,K1,K2,CYLINA,NPIN,NPINT,
     @                      THETA,RPIN,RPIN, 0,0.0, IFTYPE,ISIZE,NUM,L)
  500          L = L+1
C
            RETURN
      END
