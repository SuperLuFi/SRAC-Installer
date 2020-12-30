      SUBROUTINE GEOM07 (IFTYPE,INIT,KDRAWF,KDRAWN,NUM, NX,NZ,RX)
C
C***********************************************************************
C                                                                      *
C       GEOM07    :  TWO DIMENSIONAL   HEXAGONAL CYLINDER              *
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
C        CIRCLH   :  DRAWNING CIRCL IN LIMIT HEXGON                    *
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
C          NZ     :  NUMBER OF SUB REGIONS                             *
C          RX     :  TABLE OF RADIUS MESH                              *
C                                                                      *
C    -- LOCAL VARIABLES --                                             *
C                                                                      *
C         CYLINR  :  TABLE OF GRAPHIC RADIUS MESH                      *
C         ALEN    :  SIDE LENGTH  OF  HEXAGON                          *
C         XPOS    :  X_POSITIONS  OF  DIVIDE LINE                      *
C         YPOS    :  Y_POSITIONS  OF  DIVIDE LINE                      *
C                                                                      *
C***********************************************************************
C
C     DIMENSION  CYLINR(200),NUM(300),RX(50),XPOS(3),YPOS(3)
      DIMENSION  CYLINR(200),NUM(1000),RX(50),XPOS(3),YPOS(3)
C
      DATA  XPOS,YPOS  /  23.2, 63.4, 86.6,   86.6, 63.4, 23.2  /
      DATA  ALEN  / 100.0 /,  PAI / 3.14159265 /
C
C
      IF (INIT.EQ.1)  GO TO 15
C        RMAX = ALEN*SQRT(3/4.)
         DO  10  J = 1,NX+1
C  10       CYLINR(J) = RX(J)/RX(NX+1)*RMAX
   10       CYLINR(J) = RX(J)/RX(NX+1)*ALEN
C
C
C
C
   15 CALL CNTPLY (ALEN,6)
C
      IF ( KDRAWF .EQ. 0 )  RETURN
         DO  30  I = 1,3
            X = XPOS(I)
            Y = YPOS(I)
            CALL  PLOT (X,Y,3)
            CALL  PLOT (-X,-Y,2)
            X = -X
            CALL  PLOT (X,Y,3)
   30       CALL  PLOT (-X,-Y,2)
         DO  40  I = 2,NX
   40       CALL  CIRCLH (0.,0., CYLINR(I),ALEN)
         IF ( KDRAWN .EQ. 0 )  RETURN
C           *****  NUMBERING  *****
            DO  50  J = 1,NZ
               I = (J+1)/2
               RSHORT = CYLINR(I)
               RLONG  = CYLINR(I+1)
               IF ( I .EQ.  1 )   RSHORT = CYLINR(2)*0.4
               IF ( J .EQ. NZ )   RLONG  = ALEN
               IF ( MOD(J,2) .EQ. 0 )  GO TO 45
                  IF ( RLONG.GT.ALEN )   RLONG  = ALEN
                  R = (RSHORT+RLONG)/2
                  X = 0.0
                  Y =  R
                  IF (RSHORT.LT.RLONG)
     @               CALL INUM (X,Y,IFTYPE,6-IFTYPE,NUM(J))
                  GO TO 50
   45          CONTINUE
                  R = (RSHORT+RLONG)/2
                  X = R*COS(PAI/3)
                  Y = R*SIN(PAI/3)
                  CALL  INUM (X,Y,IFTYPE,6-IFTYPE,NUM(J))
   50          CONTINUE
C
      RETURN
      END
