      SUBROUTINE  GEOM13 (IFTYPE,INIT,KDRAWF,KDRAWN,NUM, NX,NY,NTPIN,
     @                    NDPIN,NPX,NPY,IDIVP,RX,TY,RDP)
C
C***********************************************************************
C                                                                      *
C       GEOM13    : X Y TWO DIMENSIONAL INFINITE PILLAR                *
C                   INCLUDING PIN RODS ON GROD POINTS                  *
C                                                                      *
C       MODULE NO.  :                                                  *
C       MODULE TYPE : SUBROUTINE                                       *
C       DATE        : 1984.03.07                                       *
C                                                                      *
C***********************************************************************
C                                                                      *
C    -- CALLED BY --                                                   *
C                                                                      *
C        GEOMTY                                                        *
C                                                                      *
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
C          NX     :  NUMBER OF X MESH                                  *
C          NY     :  NUMBER OF Y MESH                                  *
C          NTPIN  :  NUMBER OF PIN RODS                                *
C          NDPIN  :  NUMBER OF PIN ROD DIVISION                        *
C          IDIVP  :  MODERATOR REGION DIVISION CONTROL                *
C          RX     :  TABLE OF X MESH                                   *
C          TY     :  TABLE OF Y MESH                                   *
C          RDP    :  RADII OF PIN ROD                                  *
C                                                                      *
C                                                                      *
C    -- LOCAL VARIABLES --                                             *
C                                                                      *
C         CX      :  TABLE OF X PILLAR POSITION                        *
C         CY      :  TABLE OF Y PILLAR POSITION                        *
C         CRDP    :  RELATIVE RDP
C         ALEN    :  SIDE LENGTH                                       *
C                                                                      *
C***********************************************************************
C
      COMMON / PIJ2C / IGT
C
      DIMENSION  RX(50),TY(50),NPX(NTPIN),NPY(NTPIN),RDP(NDPIN+1,NTPIN)
      DIMENSION  NUM(1000),CX(50),CY(50),CRDP(50,500)
C
      DATA  ALEN  / 200. / , PI/ 3.141592/
C
C
C
      IF ( INIT .EQ. 1 )  GO TO 30
         IF ( RX(NX+1) .LT. TY(NY+1) )  GO TO 5
C
        IF(IGT.EQ.13) THEN
            VMAX = 2.*RX(NX+1)
            DX = ALEN/2.
            DY = (VMAX-   TY(NY+1))/VMAX*ALEN
                      ELSE
            VMAX =    RX(NX+1)
            DX = 0
            DY = (VMAX-   TY(NY+1))/VMAX*ALEN/2.
                      ENDIF
            GO TO 7
C
    5    CONTINUE
        IF(IGT.EQ.13) THEN
            VMAX = 2.*TY(NY+1)
            DX = (VMAX-   RX(NX+1))/VMAX*ALEN
            DY =                      ALEN/2.
                      ELSE
            VMAX =    TY(NY+1)
            DX = (VMAX-   RX(NX+1))/VMAX*ALEN/2.
            DY = 0
                      ENDIF
C
    7    DO  10  J = 1,NX+1
   10       CX(J) = RX(J)/VMAX*ALEN+DX
C
         DO  20  J = 1,NY+1
   20       CY(J) = TY(J)/VMAX*ALEN+DY
C
      IF(NTPIN.EQ.0) GO TO 30
         DO  25  J=1,NTPIN
         DO  25  I=1,NDPIN+1
         CRDP(I,J)=RDP(I,J)/VMAX*ALEN
   25 CONTINUE
C
C
   30 CALL NEWPEN(3)
      CALL PLOT (CX(   1), CY(   1), 3)
      CALL PLOT (CX(   1), CY(NY+1), 2)
      CALL PLOT (CX(NX+1), CY(NY+1), 2)
      CALL PLOT (CX(NX+1), CY(   1), 2)
      CALL PLOT (CX(   1), CY(   1), 2)
      CALL NEWPEN(2)
C
C *** PIN ROD
C
      IF(NTPIN.EQ.0) GO TO 36
      DO 35 N=1,NTPIN
      XX=CX(NPX(N))
      YY=CY(NPY(N))
      AF=0.
      AL=2.*PI
C     IF(NPY(N).EQ.1    .AND. NPX(N).LT. NX+1)    AF=0.
      IF(NPX(N).EQ.NX+1 .AND. NPY(N).LT. NY+1)    AF=0.5*PI
      IF(NPY(N).EQ.NY+1 .AND. NPX(N).GT. 1)       AF=    PI
      IF(NPX(N).EQ.1    .AND. NPY(N).GT. 1)       AF=1.5*PI
      IF(NPX(N).EQ.1    .AND. NPY(N).LT. NY+1)    AL=0.5*PI
      IF(NPY(N).EQ.1    .AND. NPX(N).GT. 1)       AL=    PI
      IF(NPX(N).EQ.NX+1 .AND. NPY(N).GT. 1)       AL=1.5*PI
C     IF(NPY(N).EQ.NY+1 .AND. NPX(N).LT. NX+1)    AL=2.0*PI
      IF(AF.GT.AL) AF=AF-2.0*PI
C
      DO 34 J=1,NDPIN
      CALL CIRCLG(XX,YY,CRDP(J+1,N),AF,AL)
   34 CONTINUE
   35 CONTINUE
C
   36 IF ( KDRAWF .EQ. 0 )  RETURN
      CALL NEWPEN(1)
C
         DO  40  J = 2,NX
      CALL PLOT(CX(J),CY(1),3)
      IF(IDIVP.GT.0 .OR. NTPIN.EQ.0)  CALL PLOT(CX(J),CY(NY+1),2)
      IF(IDIVP.EQ.0 .AND. NTPIN.GT.0) CALL DASHP(CX(J),CY(NY+1),3.)
   40    CONTINUE
C
         DO  50  J = 2,NY
      CALL PLOT(CX(1),CY(J),3)
      IF(IDIVP.GT.0 .OR. NTPIN.EQ.0)  CALL PLOT(CX(NX+1),CY(J),2)
      IF(IDIVP.EQ.0 .AND. NTPIN.GT.0) CALL DASHP(CX(NX+1),CY(J),3.)
   50    CONTINUE
C
         IF ( KDRAWN .EQ. 0 )  RETURN
            L = 1
            DO  70  J = 1,NY
            DO  70  I = 1,NX
               X = (CX(I)+CX(I+1))/2
               Y = (CY(J)+CY(J+1))/2
               CALL  INUM (X,Y,IFTYPE,6-IFTYPE,NUM(L))
         IF(  NTPIN.EQ.0 .OR. IDIVP.NE.0)  L=L+1
   70       CONTINUE
C
      IF(NTPIN.EQ.0) RETURN
C
      L=1
      IF(IDIVP.NE.0) L=NX*NY
      DO 110 N=1,NTPIN
      L=L+1
      XX=CX(NPX(N))
      YY=CY(NPY(N))
      CALL INUM(XX,YY,IFTYPE,6-IFTYPE,NUM(L))
      L=L+NDPIN-1
      IF(NPY(N).LT.NY+1)   THEN
      YY=YY+(CRDP(NDPIN,N)+CRDP(NDPIN+1,N))/2.
                           ELSE
      YY=YY-(CRDP(NDPIN,N)+CRDP(NDPIN+1,N))/2.
                           END IF
      CALL INUM(XX,YY,IFTYPE,6-IFTYPE,NUM(L))
  110 CONTINUE
C
            RETURN
      END
