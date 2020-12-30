      SUBROUTINE  GEOM16(IFTYPE,INIT,KDRAWF,KDRAWN,NUM, NX,NY,NTPIN,
     @                   NDPIN,NPX,NPY,RX,TY,RDP)
C***********************************************************************
C                                                                      *
C       IGT=13    : X Y TWO DIMENSIONAL INFINITE PILLAR  PERIODIC      *
C       IGT=16    : X Y TWO DIMENSIONAL INFINITE PILLAR  MIRROR        *
C                   PIN RODS ON GRID POINTS                  *
C       EXCLUSIVE  FOR  IDIVP=2
C
C       DATE        : 1994.04.19                                       *
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
C          IDIVP  :  FUEL      REGION DIVISION CONTROL                *
C          RX     :  TABLE OF X MESH                                   *
C          TY     :  TABLE OF Y MESH                                   *
C          RDP    :  RADII OF PIN ROD                                  *
C          NPX    :  X POSITION OF PIN ROD                             *
C          NPY    :  Y POSITION OF PIN ROD                             *
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
C     DIMENSION  NUM(300),CX(50),CY(50),CRDP(5,200)
      DIMENSION  NUM(1000),CX(50),CY(50),CRDP(50,500)
C
      DATA  ALEN  / 200. / , PI/ 3.141592/
C
C
C
      IF ( INIT .EQ. 1 )  GO TO 30
C
         IF ( RX(NX+1) .LT. TY(NY+1) )  GO TO 5
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
C     IF(NTPIN.EQ.0) GO TO 30
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
C     IF(NTPIN.EQ.0) GO TO 36
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
      DO 34 J=1,NDPIN
      CALL CIRCLG(XX,YY,CRDP(J+1,N),AF,AL)
   34 CONTINUE
   35 CONTINUE
C
   36 IF ( KDRAWF .EQ. 0 )  RETURN
      CALL NEWPEN(1)
         DO  40  J = 2,NX
      CALL PLOT(CX(J),CY(1),3)
C     IF(IDIVP.GT.0 .OR. NTPIN.EQ.0)  CALL PLOT(CX(J),CY(NY+1),2)
                                      CALL PLOT(CX(J),CY(NY+1),2)
C     IF(IDIVP.EQ.0 .AND. NTPIN.GT.0) CALL DASHP(CX(J),CY(NY+1),3.)
   40    CONTINUE
         DO  50  J = 2,NY
      CALL PLOT(CX(1),CY(J),3)
C     IF(IDIVP.GT.0 .OR. NTPIN.EQ.0)  CALL PLOT(CX(NX+1),CY(J),2)
                                      CALL PLOT(CX(NX+1),CY(J),2)
C     IF(IDIVP.EQ.0 .AND. NTPIN.GT.0) CALL DASHP(CX(NX+1),CY(J),3.)
   50    CONTINUE
         IF ( KDRAWN .EQ. 0 )  RETURN
            L = 0
C
            DO  70  J = 1,NY
            DO  70  I = 1,NX
             L=L+1
               X = (3.*CX(I)+CX(I+1))/4.
               Y = (CY(J)+3.*CY(J+1))/4.
               CALL  INUM (X,Y,IFTYPE,6-IFTYPE,NUM(L))
   70       CONTINUE
      L=NX*NY+1
C
C
C
      DO 110 N=1,NTPIN
      LA=0
C     NUMBER FOR INNERMOST RADIUS
C     LEFT LOWER
      IF(NPX(N).GT.1 .AND. NPY(N).GT.1) THEN
      YY=CY(NPY(N))-(CRDP(1,N)+CRDP(2,N))*0.35
      XX=CX(NPX(N))-(CRDP(1,N)+CRDP(2,N))*0.35
      CALL INUM(XX,YY,IFTYPE,6-IFTYPE,NUM(L))
      L=L+1
      LA=LA+NDPIN-1
                                        ENDIF
C     RIGHT LOWER
      IF(NPX(N).LT.NX+1 .AND. NPY(N).GT.1) THEN
      YY=CY(NPY(N))-(CRDP(1,N)+CRDP(2,N))*0.35
      XX=CX(NPX(N))+(CRDP(1,N)+CRDP(2,N))*0.35
      CALL INUM(XX,YY,IFTYPE,6-IFTYPE,NUM(L))
      L=L+1
      LA=LA+NDPIN-1
                                        ENDIF
C     LEFT UPPER
      IF(NPX(N).GT.1 .AND. NPY(N).LT.NY+1) THEN
      YY=CY(NPY(N))+(CRDP(1,N)+CRDP(2,N))*0.35
      XX=CX(NPX(N))-(CRDP(1,N)+CRDP(2,N))*0.35
      CALL INUM(XX,YY,IFTYPE,6-IFTYPE,NUM(L))
      L=L+1
      LA=LA+NDPIN-1
                                        ENDIF
C     RIGHT UPPER
      IF(NPX(N).LT.NX+1 .AND. NPY(N).LT.NY+1) THEN
      YY=CY(NPY(N))+(CRDP(1,N)+CRDP(2,N))*0.35
      XX=CX(NPX(N))+(CRDP(1,N)+CRDP(2,N))*0.35
      CALL INUM(XX,YY,IFTYPE,6-IFTYPE,NUM(L))
      L=L+1
      LA=LA+NDPIN-1
                                        ENDIF
      L=L+LA
  110 CONTINUE
C
C
C
      RETURN
      END
