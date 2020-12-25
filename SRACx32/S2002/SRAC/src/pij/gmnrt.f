C **********************************************************************
                        SUBROUTINE GMNRT
C **********************************************************************
C                       CALLED BY GMNAME & GMNHX0
C         FIND MODERATOR REGION NUMBERS OF R-THETA GEOMETRY
C                       AROUND A PIN ROD
C **********************************************************************
C
C                DIRECTION                  G-REGION MAP
C
C         ****************************  ****************************
C          *                        *    *       6         5       *
C            *       2   1        *        *                     *
C              *                *            *     4     3     *
C                *   3   4    *                *             *
C                  *        *                     *  2  1  *
C                    *    *                         *    *
C                      **                             **
C
C **********************************************************************
     *             (NRX,NTY,IX,IY,LOC,FCT)
                  DIMENSION    LOC(6),FCT(6)
                        LOC(1)=IX*NTY+IY
                        LOC(2)=LOC(1)+1
                        LOC(3)=LOC(2)-NTY
                        LOC(4)=LOC(3)-1
                      IF(IX.EQ.0 )  THEN
                        LOC(1)=0
                        LOC(3)=0
                        LOC(4)=0
                                    ENDIF
                      IF(IX.EQ.NRX) THEN
                        LOC(1)=0
                        LOC(2)=0
                                    ENDIF
                      IF(IY.EQ.0 )  THEN
                        LOC(1)=0
                        LOC(4)=0
                         IF(IX.LT.NRX.AND.IX.GT.0) THEN
                        LOC(1)=(IX+1)*NTY
                        FCT(1)=FCT(2)
                                                   ENDIF
                         IF(IX.GT.0)   THEN
                        LOC(4)= IX   *NTY
                        FCT(4)=FCT(3)
                                        ENDIF
                                    ENDIF
                      IF(IY.EQ.NTY) THEN
                        LOC(1)=0
                        LOC(2)=0
                        LOC(3)=0
                        LOC(4)=0
                                    ENDIF
                          DO 10 I=1,4
                        IF(LOC(I).EQ.0)
     *                     FCT(I)=0.
   10                      CONTINUE
                            RETURN
                             END
