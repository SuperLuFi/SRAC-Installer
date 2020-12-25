      SUBROUTINE GEOM04 (IFTYPE,INIT,KDRAWF,KDRAWN,NUM, NX,RX)
C
C***********************************************************************
C                                                                      *
C       GEOM04    :  SQUARE CYLINDER                                   *
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
C    -- CALLS --                                                       *
C                                                                      *
C         CNTPLY  :  DRQWNING POLYGON                                  *
C         CIRCLS  :  DRQWNING CIRCLE IN LIMIT SQUARE                   *
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
C         ALEN    :  HALF SIDE LENGTH  OF  SQUARE                      *
C                                                                      *
C                                                                      *
C***********************************************************************
C
C     DIMENSION  CYLINR(200),NUM(300),RX(50)
      DIMENSION  CYLINR(200),NUM(1000),RX(50)
      DATA  ALEN  / 100.0 /,      PAI  / 3.14159265 /
C
C
      IF (INIT.EQ.1)  GO TO 15
           RMAX=RX(NX+1)/SQRT(2.)
         DO  10  J = 1,NX
   10       CYLINR(J) = RX(J)/RMAX*ALEN
         CYLINR(NX+1) = ALEN*SQRT(2.)
C
C
   15 CALL CNTPLY (ALEN*2,4)
C
C
C
      IF ( KDRAWF .EQ. 0 )  RETURN
         DO  110  I = 2,NX
  110       CALL  CIRCLS (0.,0., CYLINR(I),ALEN)
         IF ( KDRAWN .EQ. 0 )  RETURN
C           *****  NUMBERING  *****
            DO  200  I = 1,NX
               IF ( I .NE. 1 )  GO TO 190
                  CALL  INUM ( 0.0, 0.0, IFTYPE,6-IFTYPE,NUM(I))
                  GO TO 200
  190          CONTINUE
                  R = (CYLINR(I)+CYLINR(I+1))/2
                  X = R*COS(PAI/4)
                  Y = R*SIN(PAI/4)
                  CALL  INUM (X,Y,IFTYPE,6-IFTYPE,NUM(I))
  200          CONTINUE
C
      RETURN
      END
