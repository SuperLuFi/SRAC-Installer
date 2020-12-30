      SUBROUTINE GEOM02 (IFTYPE,INIT,KDRAWF,KDRAWN,NUM, NX,RX)
C
C***********************************************************************
C                                                                      *
C       GEOM02    :   INFINITE SLAB                                    *
C                                                                      *
C                                                                      *
C       MODULE NO.  :                                                  *
C       MODULE TYPE : SUBROUTINE                                       *
C       CODER       : HATA KENICHROU                                   *
C       DATE        : 1982.01.21                                       *
C                                                                      *
C***********************************************************************
C                                                                      *
C    -- CALLED BY --                                                   *
C                                                                      *
C      GEOMTY                                                          *
C                                                                      *
C    -- CALLS --                                                       *
C                                                                      *
C      INUM                                                            *
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
C          NX     :  NUMBER OF X MESH                                  *
C          RX     :  TABLE OF X MESH                                   *
C                                                                      *
C    -- LOCAL VARIABLES --                                             *
C                                                                      *
C          XD     :   GRAPHIC X_POSITION  OF SLAB                      *
C                                                                      *
C                                                                      *
C***********************************************************************
C
C     DIMENSION  XD(100),RX(50),NUM(300)
      DIMENSION  XD(100),RX(50),NUM(1000)
C
C
      IF (INIT.EQ.1)  GO TO 15
         DO  10  I = 1,NX+1
   10       XD(I) = RX(I)/RX(NX+1)*200
C
C
   15 CALL  PLOT (XD(1),       0.,  3)
      CALL  PLOT (XD(1),     180.,  2)
      CALL  PLOT (XD(NX+1),   0.0,  3)
      CALL  PLOT (XD(NX+1), 180.0,  2)
C
C
C
C
      IF ( KDRAWF .EQ. 0 )  RETURN
         DO  20  I = 2,NX
            CALL  PLOT (XD(I),   0.0, 3)
            CALL  PLOT (XD(I), 180.0, 2)
   20       CONTINUE
         IF ( KDRAWN .EQ. 0 )  RETURN
C           *****  NUMBERING  ********
            DO  30  I = 1,NX
               X =  (XD(I)+XD(I+1))/2
               CALL  INUM (X,90.0,IFTYPE,6-IFTYPE,NUM(I))
   30          CONTINUE
C
      RETURN
      END
