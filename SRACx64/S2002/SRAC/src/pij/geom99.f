      SUBROUTINE  GEOM99(IFTYPE,INIT,KDRAWF,KDRAWN,NUM, NX,NTPIN,
     @                   NDPIN,NAPIN,NPTX,RX,RDP)
C
C***********************************************************************
C                                                                      *
C       IGT=09    : X X TWO DIMENSIONAL INFINITE PILLAR  MIRROR
C       EXCLUSIVE  FOR  IDIVP=2
C
C       DATE        : 1994.05.10                                       *
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
C          NPTX   :  X POSITION OF PIN ROD                             *
C          NPTY   :  Y POSITION OF PIN ROD                             *
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
      DIMENSION  RX(50),NPTX(NAPIN),RDP(NDPIN+1,NTPIN)
C     DIMENSION  NUM(300),CX(50),CY(50),CRDP(5,200)
      DIMENSION  NUM(1000),CX(50),CY(50),CRDP(50,500)
C
      DATA  ALEN  / 200. / , PI/ 3.141592/
C
C
C
      NAPIN2=(NAPIN*(NAPIN+1))/2
      IF ( INIT .EQ. 1 )  GO TO 30
            VMAX = RX(NX+1)
            DX = 0
            DY = 0
         DO  10  J = 1,NX+1
            CX(J) = RX(J)/VMAX*ALEN+DX
   10       CY(J) = CX(J)
C     IF(NTPIN.EQ.0) GO TO 30
         DO  25  J=1,NTPIN
         DO  25  I=1,NDPIN+1
         CRDP(I,J)=RDP(I,J)/VMAX*ALEN
   25 CONTINUE
C
C
   30 CALL NEWPEN(3)
      CALL PLOT (CX(   1), CY(   1), 3)
      CALL PLOT (CX(   1), CY(NX+1), 2)
      CALL PLOT (CX(NX+1), CY(NX+1), 2)
      CALL PLOT (CX(NX+1), CY(   1), 2)
      CALL PLOT (CX(   1), CY(   1), 2)
      CALL NEWPEN(2)
C
C *** PIN ROD
C
C     IF(NTPIN.EQ.0) GO TO 36
C     DO 35 N=1,NTPIN
C     N=0
      DO 35 NPY=1,NAPIN
      DO 35 NPX=1,NAPIN
      N=LOCF(NPX,NPY,NAPIN,NAPIN2)
      XX=CX(NPTX(NPX))
      YY=CY(NPTX(NPY))
      AF=0.
      AL=2.*PI
C     IF(NPTY(N).EQ.1    .AND. NPTX(N).LT. NX+1)  AF=0.
      IF(NPTX(NPX).EQ.NX+1 .AND. NPTX(NPY).LT. NX+1) AF=0.5*PI
      IF(NPTX(NPY).EQ.NX+1 .AND. NPTX(NPX).GT. 1)   AF=    PI
      IF(NPTX(NPX).EQ.1  .AND. NPTX(NPY).GT. 1)     AF=1.5*PI
      IF(NPTX(NPX).EQ.1  .AND. NPTX(NPY).LT. NX+1)  AL=0.5*PI
      IF(NPTX(NPY).EQ.1  .AND. NPTX(NPX).GT. 1)     AL=    PI
      IF(NPTX(NPX).EQ.NX+1 .AND. NPTX(NPY).GT. 1)   AL=1.5*PI
C     IF(NPTY(N).EQ.NY+1 .AND. NPTX(N).LT. NX+1)   AL=2.0*PI
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
C     IF(IDIVP.GT.0 .OR. NTPIN.EQ.0)  CALL PLOT(CX(J),CY(NX+1),2)
                                      CALL PLOT(CX(J),CY(NX+1),2)
C     IF(IDIVP.EQ.0 .AND. NTPIN.GT.0) CALL DASHP(CX(J),CY(NY+1),3.)
   40    CONTINUE
         DO  50  J = 2,NX
      CALL PLOT(CX(1),CY(J),3)
C     IF(IDIVP.GT.0 .OR. NTPIN.EQ.0)  CALL PLOT(CX(NX+1),CY(J),2)
                                      CALL PLOT(CX(NX+1),CY(J),2)
C     IF(IDIVP.EQ.0 .AND. NTPIN.GT.0) CALL DASHP(CX(NX+1),CY(J),3.)
   50    CONTINUE
         IF ( KDRAWN .EQ. 0 )  RETURN
            L = 0
            DO  70  J = 1,NX
            DO  70  I = J,NX
             L=L+1
               X = (CX(I)+CX(I+1))/2.
               Y = (CY(J)+CY(J+1))/2.
               CALL  INUM (X,Y,IFTYPE,6-IFTYPE,NUM(L))
   70       CONTINUE
      L=(NX*(NX+1))/2+1
C
C     DO 110 N=1,NTPIN
C
      DO 110 NPY=1,NAPIN
      DO 110 NPX=NPY,NAPIN
      LA=0
C     NUMBER FOR INNERMOST RADIUS
C     LEFT LOWER
      IF(NPTX(NPX).GT.1 .AND. NPTX(NPY).GT.1) THEN
      YY=CY(NPTX(NPY))-(CRDP(1,N)+CRDP(2,N))*0.35
      XX=CX(NPTX(NPX))-(CRDP(1,N)+CRDP(2,N))*0.35
      CALL INUM(XX,YY,IFTYPE,6-IFTYPE,NUM(L))
      L=L+1
      LA=LA+NDPIN-1
                                        ENDIF
C     RIGHT LOWER
      IF(NPTX(NPX).LT.NX+1 .AND. NPTX(NPY).GT.1) THEN
      YY=CY(NPTX(NPY))-(CRDP(1,N)+CRDP(2,N))*0.35
      XX=CX(NPTX(NPX))+(CRDP(1,N)+CRDP(2,N))*0.35
      CALL INUM(XX,YY,IFTYPE,6-IFTYPE,NUM(L))
      L=L+1
      LA=LA+NDPIN-1
                                        ENDIF
C     LEFT UPPER
      IF(NPX.NE.NPY)                               THEN
      IF(NPTX(NPX).GT.1 .AND. NPTX(NPY).LT.NX+1) THEN
      YY=CY(NPTX(NPY))+(CRDP(1,N)+CRDP(2,N))*0.35
      XX=CX(NPTX(NPX))-(CRDP(1,N)+CRDP(2,N))*0.35
      CALL INUM(XX,YY,IFTYPE,6-IFTYPE,NUM(L))
      L=L+1
      LA=LA+NDPIN-1
                                        ENDIF
                                                   ENDIF
C     RIGHT UPPER
      IF(NPTX(NPX).LT.NX+1 .AND. NPTX(NPY).LT.NX+1) THEN
      YY=CY(NPTX(NPY))+(CRDP(1,N)+CRDP(2,N))*0.35
      XX=CX(NPTX(NPX))+(CRDP(1,N)+CRDP(2,N))*0.35
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
