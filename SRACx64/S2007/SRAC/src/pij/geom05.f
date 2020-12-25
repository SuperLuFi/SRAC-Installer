      SUBROUTINE GEOM05 (IFTYPE,INIT,KDRAWF,KDRAWN,NUM, NX,NZ,RX)
C
C***********************************************************************
C                                                                      *
C       GEOM05    :  TWO DIMENSIONAL SQUARE CYLINDER                   *
C                                                                      *
C                                                                      *
C       MODULE NO.  :                                                  *
C       MODULE TYPE : SUBROUTINE                                       *
C       UPDATE      : 1994.07.19                                       *
C                                                                      *
C***********************************************************************
C                                                                      *
C    -- CALLED BY --                                                   *
C                                                                      *
C        GEOMTY                                                        *
C                                                                      *
C    -- CALLS --                                                       *
C                                                                      *
C        CIRCLS   :  DRAWNING CIRCL IN LIMIT SQRARE                    *
C        CNTPLY   :  DRAWNING POLYGON                                  *
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
C         ALEN    :  HALF SIDE LENGTH  OF  SQUARE                      *
C         XPOS    :  X POSITION OF DIVIDE LINE                         *
C         YPOS    :  Y POSITION OF DIVIDE LINE                         *
C                                                                      *
C***********************************************************************
C
C     DIMENSION  CYLINR(200),NUM(300),RX(50),XPOS(2),YPOS(2)
      DIMENSION  CYLINR(200),NUM(1000),RX(50),XPOS(2),YPOS(2)
C
      DATA  ALEN  / 100.0 /,    PAI  / 3.14159265 /
      DATA  XPOS,YPOS  /  100., 41.4,    41.4, 100.  /
C
      IF (INIT.EQ.1)  GO TO 15
           RMAX=RX(NX+1)/SQRT(2.)
         DO  10  J = 1,NX+1
   10       CYLINR(J) = RX(J)/RMAX*ALEN
C
C
C
   15 CALL CNTPLY (ALEN*2,4)
C
      IF ( KDRAWF .EQ. 0 )  RETURN
         DO  30  I = 1,2
            X = XPOS(I)
            Y = YPOS(I)
            CALL  PLOT (X,Y,3)
            CALL  PLOT (-X,-Y,2)
            X = -X
            CALL  PLOT (X,Y,3)
   30       CALL  PLOT (-X,-Y,2)
         DO  40  I = 2,NX
   40       CALL  CIRCLS (0.,0., CYLINR(I),ALEN)
         IF ( KDRAWN .EQ. 0 )  RETURN
C           *****  NUMBERING  *****
            DO  60  J = 1,NZ
               I = (J+1)/2
               RSHORT = CYLINR(I)
               RLONG  = CYLINR(I+1)
               IF ( I .EQ.  1 )   RSHORT = CYLINR(2)*0.4
               IF ( J .EQ. NZ )   RLONG  = ALEN*SQRT(2.)
               IF ( MOD(J,2) .NE. 1 )  GO TO 55
                  IF ( RLONG.GT.ALEN )   RLONG  = ALEN
                  IF (RSHORT.GE.RLONG)   GO TO 60
                     R = (RSHORT+RLONG)/2
                     X = 0.0
                     Y =  R
                     CALL INUM (X,Y,IFTYPE,6-IFTYPE,NUM(J))
                     GO TO 60
   55          CONTINUE
                  R = (RSHORT+RLONG)/2
                  X = R*COS(PAI/4)
                  Y = R*SIN(PAI/4)
                  CALL  INUM (X,Y,IFTYPE,6-IFTYPE,NUM(J))
   60          CONTINUE
C
      RETURN
      END
