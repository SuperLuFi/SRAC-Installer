      SUBROUTINE  GEOM08 (IFTYPE,INIT,KDRAWF,KDRAWN,NUM, NX,RX)
C
C***********************************************************************
C                                                                      *
C       GEOM08    : OCTANT CYMMETRIC PILLAR                            *
C                                                                      *
C                                                                      *
C       MODULE NO.  :                                                  *
C       MODULE TYPE : SUBROUTINE                                       *
C       CODER       : HATA KENICHROU                                   *
C       DATE        : 1982.01.23                                       *
C                                                                      *
C***********************************************************************
C                                                                      *
C    -- CALLED BY --                                                   *
C                                                                      *
C         GEOMTY                                                       *
C                                                                      *
C......................................................................*
C                                                                      *
C    -- INPUT --                                                       *
C                                                                      *
C       (ARGUMENT)                                                     *
C          IFTYPE :  NUMBER OF FIGUARING TYPE                          *
C          INIT   :  FIRST CALL OR NOT                                 *
C          KDRAWF :  DRAWNING   FLAG                                   *
C          KDRAWN :  NUMBERING  FLAG                                   *
C          NUM    :  TABLE OF NUMBERS                                  *
C          NX     :  NUMBER OF RADIUS MESH                             *
C          RX     :  TABLE OF RADIUS MESH                              *
C                                                                      *
C                                                                      *
C                                                                      *
C    -- LOCAL VARIABLES --                                             *
C                                                                      *
C         CC      :  TABLE OF PILLAR POSITION                          *
C                                                                      *
C***********************************************************************
C
C     DIMENSION NUM(300),RX(50),CC(50)
      DIMENSION NUM(1000),RX(50),CC(50)
C
      DATA  ZMAX  / 100. /
C
      IF (INIT.EQ.1)  GO TO 15
         DO  10  I = 1,NX+1
   10       CC(I) = RX(I)/RX(NX+1)*ZMAX*2
C
   15 ALEN = ZMAX*2.06
      CALL  PLOT  (ALEN,0.,3)
      CALL  PLOT  (0.,0.,2)
      CALL  PLOT  (0.,ALEN,2)
      ALEN = ZMAX*2.00
      CALL  PLOT  (0.,ALEN,3)
      CALL  PLOT  (ALEN,ALEN,2)
      CALL  PLOT  (ALEN,0.,2)
C
C
C
C
C
      IF ( KDRAWF .EQ. 0 )  RETURN
         DO  20  I = 2,NX
            CALL  PLOT (CC(I),0.,3)
            CALL  PLOT (CC(I),ALEN,2)
            CALL  PLOT (0.,CC(I),3)
   20       CALL  PLOT (ALEN,CC(I),2)
         IF ( KDRAWN .EQ. 0 )  RETURN
C           *****  NUMBERING   ******
            L = 0
            DO  30  I = 1,NX
               Y1 = CC( I )
               Y2 = CC(I+1)
               DO  30  J = I,NX
                  L = L+1
                  X1 = CC( J )
                  X2 = CC(J+1)
                  CALL INUM ((X2+X1)/2,(Y2+Y1)/2,
     @                                 IFTYPE,6-IFTYPE,NUM(L))
   30             CONTINUE
      RETURN
      END
