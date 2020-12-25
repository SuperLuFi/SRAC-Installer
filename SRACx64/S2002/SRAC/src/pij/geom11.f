      SUBROUTINE  GEOM11 (IFTYPE,INIT,KDRAWF,KDRAWN,NUM, NX,NY0,
     @                   NTPIN,NDPIN1,IDIVP,  RX,TY,RPP,RDP,NPIN,THETA)
C
C***********************************************************************
C                                                                      *
C       GEOM11    : ANNULAR ASSEMBLY WITH ASSYMMETRIC PIN RODS         *
C                                                                      *
C                                                                      *
C       MODULE NO.  :                                                  *
C       MODULE TYPE : SUBROUTINE                                       *
C       CODER       : HATA KENICHROU                                   *
C       DATE        : 1982.02.04                                       *
C                                                                      *
C***********************************************************************
C                                                                      *
C    -- CALLED BY --                                                   *
C                                                                      *
C        GEOMTY                                                        *
C                                                                      *
C    -- CALLS --                                                       *
C                                                                      *
C        SRTCYL  :  SORTING  CYLINDER (WITH PIN RODS) DATA             *
C        CIRCLP  :  DRAWNING CYLINDER WITH PIN RODS                    *
C        NUMPIN  :  NUMBERING PIN RODS                                 *
C                                                                      *
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
C          NTPIN  :  NUMBER OF TOTAL PIN RODS                          *
C          NDPIN  :  NUMBER OF RADIUS FOR PIN ROD DIVIDING             *
C          IDIVP  :  DIVIDE FLAG                                       *
C          RX     :  TABLE OF RADIUS MESH                              *
C          RPP    :  TABLE OF RADIUS OF REGULAR ARRAYS OF PIN RODS     *
C          RDP    :  TABLE OF PIN DIVIDE RADIUS MESH                   *
C                                                                      *
C                                                                      *
C    -- LOCAL VARIABLES --                                             *
C                                                                      *
C         CYLINR  :  TABLE OF RADIUS MESH            (GRAPHIC)         *
C         CYLINA  :  RADIUS OF CIRCLE WITH PIN RODS  (GRAPHIC)         *
C         CYLINP  :  TABLE OF PIN DIVIDE RADIUS MESH (GRAPHIC)         *
C         RRPIN   :  RADIUS OF PIN POSITION'S VECTOR                   *
C         CYLINJ  :  TABLE OF RADIUS MESH (JOINTED CYLINA & CYLINR)    *
C         ALEN    :  SIDE LENGTH                                       *
C         NPIN    :  NUMBER OF PIN RODS THAT IS ON ANNULAR CIRCLE      *
C         NPINT   :  INTERMEDEATE TOTAL NUMBER OF PIN RODS             *
C                                                                      *
C***********************************************************************
C
      DIMENSION  RX(50),TY(50),RPP(50),THETA(50),RDP(NDPIN1,50)
C     DIMENSION  CYLINP(10,50),RRPIN(50),NPIN(50),NPINT(50)
C     DIMENSION  CYLINR(50),NUM(300),NFLAG(50),CYLINA(50),CYLINJ(100)
      DIMENSION  CYLINP(50,500),RRPIN(500),NPIN(50),NPINT(50)
      DIMENSION  CYLINR(50),NUM(1000),NFLAG(50),CYLINA(50),CYLINJ(100)
C
      DATA  PAI / 3.14159265 /,  EPS  / 1.0 /,    EPSR   / 0.05 /
C
C
      NY=NY0
      IF(NY0.LE.1) NY=0
      NDPIN = NDPIN1-1
      IF ( INIT .EQ. 1 )  GO TO 100
      RMAX = 100
      DO  10  I = 1,NX+1
        CYLINR(I) = RX(I)/RX(NX+1)*RMAX
   10 CONTINUE
      DO  20  J = 1,NTPIN
      DO  20  I = 2,NDPIN+1
        CYLINP(I-1,J) = RDP(I,J)/RX(NX+1)*RMAX
   20 CONTINUE
      DO  30  I = 1,NTPIN
        RRPIN(I) = RPP(I)/RX(NX+1)*RMAX
   30 CONTINUE
C
  100 ALEN = 200
      CALL  CIRCLG (0.,0., CYLINR(NX+1),0.,2*PAI)
      IF ( KDRAWF .EQ. 0 )  RETURN
      IF  ( NY .EQ. 0 )  GO TO 125
      DO  120  I = 1,NY
        RR = RMAX
        X = RR*COS(TY(I))
        Y = RR*SIN(TY(I))
        CALL  PLOT (0., 0., 3 )
        CALL  PLOT (X,Y,2)
  120 CONTINUE
  125 DO  130  I = 1,NTPIN
        X = RRPIN(I)*COS(THETA(I))
        Y = RRPIN(I)*SIN(THETA(I))
        DO  130  J = 1,NDPIN
               CALL  CIRCLG (X,Y, CYLINP(J,I), 0.0,2*PAI)
  130 CONTINUE
C
      IF(INIT.EQ.0)
     @   CALL SRTCYL (NAPIN,NTPIN,NDPIN,CYLINP,RRPIN,THETA,
     @                                           CYLINA,NPIN,NPINT)
         IF  (  NX .LT. 2 )  GO TO 299
            DO  220  J = 2,NX
               DO  210  I = 1,NAPIN
                  IF ( ABS(CYLINA(I)-CYLINR(J)) .LT. EPS )  GO TO 220
  210             CONTINUE
               CALL  CIRCLH (0.,0., CYLINR(J),ALEN)
  220          CONTINUE
C
C
C
C
  299    DO  300  I = 1,NAPIN
C----- MODIFYED FROM 2001/03/28 -----
C           DO  260  J = 2,NX
C              IF ( ABS(CYLINA(I)-CYLINR(J)) .GE. EPS )  GO TO 260
C                 NFLAG(I) = 2
C                 GO TO 280
C 260          CONTINUE
C           NFLAG(I) = IDIVP
C 280       IF ( NFLAG(I).EQ.2  .AND.  NY.NE.0 )  NFLAG(I) = 3
            DO  260  J = 2,NX
               IF ( ABS(CYLINA(I)-CYLINR(J)) .GE. EPS )  GO TO 260
                  NFLAG(I) = IDIVP
                  GO TO 280
  260          CONTINUE
               NFLAG(I) = IDIVP
  280       CONTINUE
C------------------------------------
            CALL CIRCLP (NFLAG(I),CYLINA(I),NPIN(I),
     @               CYLINP(NDPIN,1+NPINT(I)),THETA(1+NPINT(I)),NY,TY)
  300       CONTINUE
C
C
         IF ( KDRAWN .EQ. 0 )  RETURN
C           *****  NUMBERING PIN RODS  *****
            IF ( NDPIN .EQ. 1 )  ISIZE = 5-IFTYPE
            IF ( NDPIN .NE. 1 )  ISIZE = 4-IFTYPE
            L = 1
            DO  400  I = 1,NAPIN
            DO  400  J = 1,NPIN(I)
               IF ( NFLAG(I).EQ.0 .OR. NFLAG(I).EQ.1 )   INST = 1
               IF ( NFLAG(I).EQ.2 .OR. NFLAG(I).EQ.3 )   INST = 2
               IF ( NY .EQ. 0 )  GO TO 370
C----- MODIFYED FROM 2001/03/28 -----
C                 DO  310  K = 1,NY
C                    IF ( ABS(TY(K)-THETA(J+NPINT(I))) .GE. EPSR )
C    @                                                       GO TO 310
C                       INST = 3
C                       GO TO 370
C 310                CONTINUE
C------------------------------------
  370          CALL  NUMPIN (INST,CYLINA(I),THETA(J+NPINT(I)),
     @               CYLINP(1,J+NPINT(I)),NDPIN,L,NUM,IFTYPE,ISIZE)
               IF ( INST .EQ. 1 )  L = L+NDPIN
               IF ( INST .NE. 1 )  L = L+2*NDPIN
  400          CONTINUE
C
C           *****  NUMBERING ANNULAR CYLINDER *****
C           IF ( NY .EQ. 0 )  GO TO 422
C              DO  420  I = 1,NY
C                 IF ( TY(I) .LE.-EPSR )   TY(I) = TY(I)+2*PAI
C                 SMIN = TY(I)
C                 IMIN = I
C                 DO  410  J = I,NY
C                    IF ( TY(J) .GE. SMIN )  GO TO 410
C                       SMIN = TY(J)
C                       IMIN = J
C 410                CONTINUE
C                 TY(IMIN) = TY(I)
C 420             TY( I  ) = SMIN
C
C
C
C
C
  422       NJ = NX+1
            DO  430  I = 1,NX+1
  430          CYLINJ(I) = CYLINR(I)
            IF ( IDIVP .EQ. 0 )  GO TO 500
               DO  450  I = 1,NAPIN
                  DO  440  J = 1,NX
                     IF ( ABS(CYLINA(I)-CYLINR(J)) .LT. EPS )  GO TO 450
  440                CONTINUE
                        NJ = NJ+1
                        CYLINJ(NJ) = CYLINA(I)
  450             CONTINUE
               DO  470  I = 1,NJ
                  RMIN = CYLINJ(I)
                  IMIN = I
                  DO  460  J = I,NJ
                     IF ( CYLINJ(J) .GE. RMIN )   GO TO 460
                        RMIN = CYLINJ(J)
                        IMIN = J
  460                CONTINUE
                  CYLINJ(IMIN) = CYLINJ(I)
  470             CYLINJ( I )  = RMIN
C
  500       INCR = NY
            IF ( INCR .EQ. 0 )  INCR = 1
            DO  600  I = 2,NJ
               K0 = 0
               K1 = 0
               K2 = 0
               DO  550  J = 1,NAPIN
                  IF ( ABS(CYLINA(J)-CYLINJ(I-1)) .LT. EPS )  K1 = J
                  IF ( ABS(CYLINA(J)-CYLINJ( I )) .LT. EPS )  K2 = J
                  IF ( CYLINA(J) .GT. CYLINJ(I-1)+EPS .AND.
     @                 CYLINA(J) .LT. CYLINJ( I )-EPS      )  K0 = J
  550             CONTINUE
               IF ( K0 .NE. 0 )  K1 = K0
               CALL  NUMCYL(I,CYLINJ, K1,K2,CYLINA, NPIN,NPINT,THETA,
     @                             CYLINP(NDPIN,K1), CYLINP(NDPIN,K2),
     @                                      NY,TY,  IFTYPE,ISIZE,NUM,L)
  600          L = L+INCR
C
      RETURN
      END
