      SUBROUTINE  GEOM15 (IFTYPE,INIT,KDRAWF,KDRAWN,NUM,
     @           NX,NY,NAPIN,NDPIN,NTPIN,RX,TY,NPX,NPY,RDP)
C
C***********************************************************************
C                                                                      *
C       GEOM15    : HEXAGONAL CELL WITH HEXAGONAL ARRAY OF PIN RODS    *
C                                                                      *
C                                                                      *
C       MODULE NO.  :                                                  *
C       MODULE TYPE : SUBROUTINE                                       *
C       DATE        : 2001.03.                                         *
C                                                                      *
C***********************************************************************
C                                                                      *
C    -- CALLED BY --                                                   *
C                                                                      *
C        GEOMTY                                                        *
C                                                                      *
C    -- CALLS --                                                       *
C                                                                      *
C        CIRCLG,NEWPEN,PLOT  ,CIRCLG,INUM                              *
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
C          NY     :  NUMBER OF OUTTER REGION MESH                      *
C          NAPIN  :  NUMBER OF ARRAYS WITH PIN RODS                    *
C          NDPIN  :  NUMBER OF RADIUS FOR PIN ROD DIVIDING             *
C          RX     :  TABLE OF RADIUS MESH                              *
C          TY     :  TABLE OF OUTTER REGION MESH                       *
C          NPX    :  X POSITION OF PIN ROD IN RX (IXP)                 *
C          NPY    :  Y POSITION OF PIN ROD IN RX (IYP)                 *
C          RDP    :  TABLE OF PIN DIVIDE RADIUS MESH                   *
C                                                                      *
C    -- LOCAL VARIABLES --                                             *
C                                                                      *
C         CYLINP  :  TABLE OF PIN DIVIDE RADIUS MESH  (GRAPHIC)        *
C         CYLINJ  :  TABLE OF RADII OF HEXAGONS (GRAPHIC)              *
C                                                                      *
C***********************************************************************
C
C
      DIMENSION  NUM(1000),RX(50),TY(50),NPX(NTPIN),NPY(NTPIN),
     @           RDP(NDPIN+1,NTPIN)
      DIMENSION  CYLINP(50,500)
      DIMENSION  CYLINJ(50)
      DIMENSION  NPINNO(500)
C
      DATA RMAX  / 180. /,    PI / 3.14159265 /,   EPS / 1.0 /
C
CKSK  X0 = RMAX*(-0.25)
      X0 = RMAX*(-0.50)
      Y0 = RMAX*(-0.25)
C
      DO  100  I = 1,NX+1
        CYLINJ(I) = RX(I)/RX(NX+1)*RMAX
  100 CONTINUE
C
      IF(NTPIN.EQ.0) GOTO 170
C
      NPNTTR = (NAPIN+1)*NAPIN/2.0+1
      NPINTR = 0
C
      DO  110  I = 1,NPNTTR
        IF ( NPX(I).EQ.0 .AND. NPY(I).EQ.0 ) THEN
          NPINNO(1) = I
          NPINTR = NPINTR + 1
          DO  120  J = 2,NDPIN+1
            CYLINP(J-1,I) = RDP(J,I)/RX(NX+1)*RMAX
  120     CONTINUE
          GOTO 130
        ENDIF
  110 CONTINUE
      NPINNO(1) = 0
  130 IFLAG=1
C
      IF(NAPIN.EQ.0) GOTO 170
C
      DO  140  IY = 0,NAPIN-1
        DO  150  IX = IY+1,NAPIN
            IFLAG = IFLAG + 1
          DO  160  I = 1,NPNTTR
            IF ( NPX(I).EQ.IX .AND. NPY(I).EQ.IY ) THEN
              NPINNO(IFLAG) = I
              NPINTR = NPINTR + 1
              DO  165  J = 2,NDPIN+1
                CYLINP(J-1,I) = RDP(J,I)/RX(NX+1)*RMAX
  165         CONTINUE
              GOTO 150
            ENDIF
  160     CONTINUE
          NPINNO(IFLAG) = 0
  150   CONTINUE
  140 CONTINUE
C
      IF ( KDRAWF .EQ. 0 )  RETURN
C
C ***    FIGURE OUTER BOUNDARY
C
  170 CALL NEWPEN(1)
      NX1 = NX + 1
      DO 180 I=2,NX1
        X1=CYLINJ(I)+X0
        X2=CYLINJ(I)*0.5+X0
        Y1=Y0
        Y2=CYLINJ(I)*SQRT(3.0)/2.0+Y0
        CALL PLOT(X1,Y1,3)
        CALL PLOT(X2,Y2,2)
  180 CONTINUE
C
      CALL PLOT(X0,Y0,2)
      CALL PLOT(X1,Y1,2)
C
      IF (NAPIN .LE. 1 ) GOTO 200
      RP05=CYLINJ(NAPIN+1)/2.0
      DO 190 I=2,NAPIN
        X1=CYLINJ(I)*0.5
        X2=X1+(RP05-X1)*2.0
        X3=CYLINJ(NAPIN+2-I)
        Y1=CYLINJ(I)*SQRT(3.0)/2.0
        CALL PLOT (X0+X1,Y0+Y1,3)
        CALL PLOT (X0+X2,Y0+Y1,2)
        CALL PLOT (X0+X3,Y0,2)
  190 CONTINUE
C
  200 IF ( NY .GE. 2 ) THEN
        DO 210 I = 1,NY-1
          X1 = CYLINJ(NX+1)-0.5*TY(I+1)*RMAX
          Y1 = SQRT(3.0)*0.5*TY(I+1)*RMAX
          X2 = CYLINJ(NAPIN+1)*(1.0-0.5*TY(I+1))
          Y2 = SQRT(3.0)*0.5*CYLINJ(NAPIN+1)*TY(I+1)
          CALL  PLOT (X0+X1,Y0+Y1,3)
          CALL  PLOT (X0+X2,Y0+Y2,2)
  210   CONTINUE
      ENDIF
C
C  ***   FIGURE  PIN ROD
      DO  220  I = 1,NPNTTR
        IF ( NPINNO(I).NE.0) THEN
          IN=NPINNO(I)
          X=CYLINJ(NPX(IN)+1)-0.5*CYLINJ(NPY(IN)+1)
          Y=SQRT(3.0)/2.0*CYLINJ(NPY(IN)+1)
          DO  230  J = 1,NDPIN
            CALL CIRCLG(X0+X,Y0+Y,CYLINP(J,IN),0.0,2*PI)
  230     CONTINUE
          IF( NPY(IN).EQ.0 ) THEN
          X=CYLINJ(NPX(IN)+1)-0.5*CYLINJ(NPX(IN)+1)
          Y=SQRT(3.0)/2.0*CYLINJ(NPX(IN)+1)
          DO  240  J = 1,NDPIN
            CALL CIRCLG(X0+X,Y0+Y,CYLINP(J,IN),0.0,2*PI)
  240     CONTINUE
          ENDIF
        ENDIF
  220 CONTINUE
C
      IF ( KDRAWN .EQ. 0 )  RETURN
C
C     *****  NUMBERING HEXAGONAL CYLINDER  *****
      IF ( NDPIN .EQ. 1 )  ISIZE = 6-IFTYPE
      IF ( NDPIN .NE. 1 )  ISIZE = 5-IFTYPE
      L  =  0
      DO  250  IX = 1,NAPIN
        X = (CYLINJ(IX)+CYLINJ(IX+1))/2.0
        Y = 0.0
        IXP = IX
        IYP = 0
        DO  250  I = 1,IX
          IYP = IYP + 1
          X = X
          Y = (CYLINJ(IYP)+(CYLINJ(IYP+1)-CYLINJ(IYP))/3.0)*SQRT(3.0)/2.0
          L = L + 1
          CALL INUM(X0+X,Y0+Y,IFTYPE,ISIZE,NUM(L))
          IF ( I .EQ. IX ) GOTO 250
          IXP = IXP - 1
          X = X - (CYLINJ(IYP+1)-CYLINJ(IYP))/2.0
          Y = Y + SQRT(3.0)/2.0*(CYLINJ(IYP+1)-CYLINJ(IYP))/3.0
          L = L + 1
          CALL INUM(X0+X,Y0+Y,IFTYPE,ISIZE,NUM(L))
  250 CONTINUE
C
      IF ( NAPIN .EQ. NX ) GOTO 280 
      ICOUNT = 3
      DO  260  I = NAPIN+1,NX
        DO 270  IY = 1,NY 
          YP = (2-MOD(ICOUNT,3)*0.5)*TY(IY) 
     @       + (1+MOD(ICOUNT,3)*0.5)*TY(IY+1)
          Y = SQRT(3.0)/2.0*CYLINJ(I+1)*YP/3.0
          X = CYLINJ(I+1)-Y/SQRT(3.0)
          L = L + 1
          CALL INUM(X0+X,Y0+Y,IFTYPE,ISIZE,NUM(L))
  270   CONTINUE
        ICOUNT = ICOUNT + 1
  260 CONTINUE
C
  280 IF ( NTPIN .EQ. 0 ) GOTO 300
C
C     *****  NUMBERING PIN RODS  *****
      DO  290  I = 1,NPINTR
        X=CYLINJ(NPX(I)+1)-0.5*CYLINJ(NPY(I)+1)
        Y=SQRT(3.0)/2.0*CYLINJ(NPY(I)+1)
        L = L + 1
        CALL INUM(X0+X,Y0+Y,IFTYPE,ISIZE,NUM(L))
        IF ( NDPIN.GE.2) THEN
          XC = X + CYLINP(NDPIN,I)
          YC = Y 
          L=L+NDPIN-1
          CALL INUM(X0+XC,Y0+YC,IFTYPE,ISIZE,NUM(L))
        ENDIF
  290 CONTINUE
C
  300 RETURN
C
      END
