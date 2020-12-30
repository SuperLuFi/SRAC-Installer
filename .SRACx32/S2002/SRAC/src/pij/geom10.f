      SUBROUTINE  GEOM10 (IFTYPE,INIT,KDRAWF,KDRAWN,NUM,
     @                    NX,NAPIN,NDPIN,IDIVP, RX,RPP,RDP,NPIN,THETA)
C
C***********************************************************************
C                                                                      *
C       GEOM10    : ANNULAR ASSEMBLY WITH REGULAR ARRAY OF PIN RODS    *
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
C        CIRCLG,NUMPIN,NUMCYL                                          *
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
C         CYLINR  :  TABLE OF RADIUS MESH FOR (GRAPHIC)                *
C         CYLINA  :  TABLE OF RADIUS OF ANNULAR WITH PIN RODS (GRAPHIC)*
C         CYLINP  :  TABLE OF PIN DIVIDE RADIUS MESH  (GRAPHIC         *
C         CYLINJ  :  JOINT & SORTING TABLE  ( CYLINA & CYLINR )        *
C         NJ      :  DIMENSION OF CYLINJ ( = NX+1+NA-? )               *
C         NPINT   :  TABLE OF POINTER FOR PIN DATA                     *
C                                                                      *
C***********************************************************************
C
C     DIMENSION  NUM(300),RX(50),RPP(50),RDP(50),NPIN(10),THETA(10)
      DIMENSION  NUM(1000),RX(50),RPP(50),RDP(50),NPIN(10),THETA(10)
      DIMENSION  CYLINR(50),CYLINP(50),CYLINA(50)
      DIMENSION  NPINT(50),CYLINJ(50)
C
      DATA  RMAX  / 100. /,    PAI / 3.14159265 /,   EPS / 1.0 /
C
C
C
C
      IF ( INIT .EQ. 1 )  GO TO 100
         DO  10  I = 1,NX+1
   10       CYLINR(I) = RX(I)/RX(NX+1)*RMAX
         DO  20  I = 1,NAPIN
   20       CYLINA(I) = RPP(I)/RX(NX+1)*RMAX
         DO  30  I = 2,NDPIN+1
   30       CYLINP(I-1) = RDP(I)/RX(NX+1)*RMAX
         RPIN = CYLINP(NDPIN)
C
C
  100 CALL  CIRCLG (0.,0., CYLINR(NX+1),0.,2*PAI)
C
      IF ( KDRAWF .EQ. 0 )  RETURN
C----- MODIFYED FROM 2001/03/29 -----
C        DO  110  I = 2,NX
C              J1 = J1+1
C 110       CALL  CIRCLG (0.,0., CYLINR(I),0.,2*PAI)
         DO  110  I = 2,NX
               J1 = J1+1
           DO 105 II = 1,NAPIN
             IF ( ABS(CYLINR(I)-CYLINA(II)) .LE. EPS ) GOTO 110
  105      CONTINUE
            CALL  CIRCLG (0.,0., CYLINR(I),0.,2*PAI)
  110    CONTINUE
C------------------------------------
         L = 0
         DO  120  I = 1,NAPIN
            R = CYLINA(I)
            DO  120  J = 1,NPIN(I)
               L  = L+1
               X  = R*COS(THETA(L))
               Y  = R*SIN(THETA(L))
               DO  120  K = 1,NDPIN
  120             CALL CIRCLG (X,Y, CYLINP(K),0.,2*PAI)
C
            NPINT(1) = 0
            DO  130  I = 2,NAPIN
  130          NPINT(I) = NPINT(I-1)+NPIN(I-1)
            DO  140  I = 1,NAPIN
               CALL CIRCLP (IDIVP,CYLINA(I),NPIN(I),CYLINP(NDPIN),
     @                                           THETA(1+NPINT(I)),0,0)
  140          CONTINUE
C
C
C
C
C
         IF ( KDRAWN .EQ. 0 )  RETURN
C           *****  NUMBERING PIN RODS  *****
            IF ( NDPIN .EQ. 1 )  ISIZE = 5-IFTYPE
            IF ( NDPIN .NE. 1 )  ISIZE = 4-IFTYPE
            IF ( IDIVP .NE. 2 )  INST = 1
            IF ( IDIVP .EQ. 2 )  INST = 2
            L = 1
            DO  320  I = 1,NAPIN
               DO  310  J = 1,NPIN(I)
                  CALL  NUMPIN (INST,CYLINA(I),THETA(J+NPINT(I)),
     @                             CYLINP,NDPIN,L,NUM,IFTYPE,ISIZE)
  310             CONTINUE
                  IF ( INST .EQ. 1 )  L = L+NDPIN
                  IF ( INST .NE. 1 )  L = L+2*NDPIN
  320          CONTINUE
C
C
C           *****  NUMBERING ANNULAR CYLINDER  *****
            NJ = NX+1
            DO  410  I = 1,NX+1
  410          CYLINJ(I) = CYLINR(I)
            IF ( IDIVP .EQ. 0 )  GO TO 455
C----- MODIFYED FROM 2001/03/28 -----
C              DO  430  I = 1,NAPIN
C                 NJ = NJ+1
C 430             CYLINJ(NJ) = CYLINA(I)
C------------------------------------
               DO  450  I = 1,NJ
                  RMIN = CYLINJ(I)
                  IMIN = I
                  DO  440  J = I,NJ
                     IF ( CYLINJ(J) .GE. RMIN )   GO TO 440
                        RMIN = CYLINJ(J)
                        IMIN = J
  440                CONTINUE
                  CYLINJ(IMIN) = CYLINJ(I)
  450             CYLINJ( I )  = RMIN
C
  455       DO  500  I = 2,NJ
               K0 = 0
               K1 = 0
               K2 = 0
               DO  460  J = 1,NAPIN
                  IF ( ABS(CYLINA(J)-CYLINJ(I-1)) .LT. EPS )  K1 = J
                  IF ( ABS(CYLINA(J)-CYLINJ( I )) .LT. EPS )  K2 = J
                  IF ( CYLINA(J) .GT. CYLINJ(I-1)+EPS .AND.
     @                 CYLINA(J) .LT. CYLINJ( I )-EPS      )  K0 = J
  460             CONTINUE
               IF ( K0 .NE. 0 )  K1 = K0
               CALL  NUMCYL(I,CYLINJ,K1,K2,CYLINA,NPIN,NPINT,
     @                      THETA,RPIN,RPIN, 0,0.0, IFTYPE,ISIZE,NUM,L)
  500          L = L+1
C
            RETURN
      END
