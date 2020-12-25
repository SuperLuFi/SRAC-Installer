      SUBROUTINE GEOM01(IFTYPE,INIT,KDRAWF,KDRAWN,NUM, NX,RX)
C
C***********************************************************************
C                                                                      *
C       GEOM01    :   SPHERE  OR  CIRCULAR CYLINDER                    *
C                                                                      *
C                                                                      *
C       MODULE NO.  :                                                  *
C       MODULE TYPE : SUBROUTINE                                       *
C       CODER       : HATA KENICHROU                                   *
C       DATE        : 1981.01.23                                       *
C                                                                      *
C***********************************************************************
C                                                                      *
C    -- CALLED BY --                                                   *
C                                                                      *
C         GEOMTY                                                       *
C                                                                      *
C    -- CALLS --                                                       *
C                                                                      *
C         CIRCLG  :  DRAWNING  CIRCLE                                  *
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
C    -- LOCAL VARIABLES --                                             *
C                                                                      *
C         CYLINR  :  TABLE OF GRAPHIC RADIUS MESH                      *
C         XYD     :  NUMBERING POSITION                                *
C                                                                      *
C***********************************************************************
C
C
C     DIMENSION  NUM(300),RX(50),CYLINR(200)
      DIMENSION  NUM(1000),RX(50),CYLINR(200)
C
      DATA    PAI  /  3.14159265  /
C
C
C
C*********  CALCULATION OF GRAPHIC MESH  ****************
C
      IF ( INIT .EQ. 1 )  GO TO 15
         DO  10  I = 2,NX+1
   10       CYLINR(I-1) = RX(I)*100/RX(NX+1)
C
C
C
C
C************  DRAWNING BLOCK  ************************
C
   15 CALL CIRCLG (0.,0., CYLINR(NX), 0.0, 2*PAI )
C
      IF ( KDRAWF .EQ. 0 )  RETURN
         DO   20   I = 1,NX
   20       CALL CIRCLG (0.,0., CYLINR(I), 0.0, 2*PAI)
         IF (  KDRAWN .EQ. 0 ) RETURN
C           *****  NUMBERING  *******
            CALL  INUM (0.0, 0.0, IFTYPE,6-IFTYPE,NUM(1))
            DO   30   I = 1,NX-1
               XYD = (CYLINR(I)+CYLINR(I+1))/2/SQRT(2.)
               CALL  INUM (XYD,XYD,IFTYPE,6-IFTYPE,NUM(I+1))
   30          CONTINUE
C
      RETURN
      END
