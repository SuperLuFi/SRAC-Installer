      SUBROUTINE GEOM06 (IFTYPE,INIT,KDRAWF,KDRAWN,NUM, NX,RX)
C
C***********************************************************************
C                                                                      *
C       GEOM06    :  HEXAGONAK CYLINDER                                *
C                                                                      *
C                                                                      *
C       MODULE NO.  :                                                  *
C       MODULE TYPE : SUBROUTINE                                       *
C       DATE        : 1994.07.19                                       *
C                                                                      *
C***********************************************************************
C                                                                      *
C    -- CALLED BY --                                                   *
C                                                                      *
C        GEOMTY                                                        *
C                                                                      *
C    -- CALLS --                                                       *
C                                                                      *
C        CIRCLH     :  DRAWNING CIRCL IN LIMIT HEXAGON                 *
C        CNTPLY     :  DRAWNING POLYGON                                *
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
C         CYLINR  :  TABLE OF GRAPHIC RADIUS MESH                      *
C         ALEN    :   SIDE LENGTH  OF  HEXAGON                         *
C                                                                      *
C                                                                      *
C***********************************************************************
C
C     DIMENSION  CYLINR(200),NUM(300),RX(50)
      DIMENSION  CYLINR(200),NUM(1000),RX(50)
C
      DATA  ALEN  / 100.0 /
      DATA  PAI / 3.14159265 /
C
C
      IF (INIT.EQ.1)  GO TO 15
C        RMAX = ALEN*SQRT(3/4.)
C        DO 10 J = 1,NX+1
C  10       CYLINR(J) = RX(J)/RX(NX+1)*RMAX
         DO 10 J = 1,NX+1
   10       CYLINR(J) = RX(J)/RX(NX+1)*ALEN
C
C
C
   15 CALL CNTPLY (ALEN,6)
C
      IF ( KDRAWF .EQ. 0 )  RETURN
         DO  20  I = 2,NX
   20       CALL  CIRCLH (0.,0., CYLINR(I),ALEN)
         IF (KDRAWN .EQ. 0 ) RETURN
C           *****  NUMBERING  *****
            DO  30  I = 1,NX
               IF ( I .NE. 1 )  GO TO 25
                  CALL  INUM ( 0.0, 0.0, IFTYPE,6-IFTYPE,NUM(I))
                  GO TO 30
   25          CONTINUE
                  RLONG = CYLINR(I+1)
                  IF ( I .EQ. NX )   RLONG=ALEN
                  R = (CYLINR(I)+RLONG)/2
                  X = R*COS(PAI/3)
                  Y = R*SIN(PAI/3)
                  CALL  INUM (X,Y,IFTYPE,6-IFTYPE,NUM(I))
   30          CONTINUE
C
      RETURN
      END
