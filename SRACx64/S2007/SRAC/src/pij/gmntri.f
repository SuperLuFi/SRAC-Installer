C **********************************************************************
                      SUBROUTINE GMNTRI
C **********************************************************************
C                  CALLED BY GRVOL,GMNHEX
C   FIND MODERATOR REGION NUMBERS OF TRI-ANGULAR MESH AROUND A PIN ROD
C **********************************************************************
     *              (NRX,IX,IY,LOC,FCT)
C                                                 *
C                                G-REGION MAP    *16*
C                           *                   *  15 *
C     DIRECTION           *   *                *  9  14*
C                       *   2   *             *    8   13 *
C                     *   3   1   *          *  4    7   12*
C                   *     4   6     *       *    3    6    11 *
C                 *         5         *    * 1     2    5    10*
C               ************************* ***********************
C
                DIMENSION    LOC(*),FCT(*)
                        LOC(6)=IX**2+2*IY
                        LOC(1)=LOC(6)+1
                        LOC(2)=LOC(1)+1
                        LOC(5)=(IX-1)**2+2*IY-1
                        LOC(4)=LOC(5)+1
                        LOC(3)=LOC(4)+1
                    IF(IX.EQ.IY  )THEN
                        LOC(2)=0
                        LOC(3)=0
                        LOC(4)=0
                                  ENDIF
                    IF(IY.EQ.0  ) THEN
                        LOC(4)=0
                        LOC(5)=0
                        LOC(6)=0
                                  ENDIF
                    IF(IX.EQ.NRX) THEN
                        LOC(2)=0
                        LOC(1)=0
                        LOC(6)=0
                                  ENDIF
                         DO 10 I=1,6
                       IF(LOC(I).EQ.0)
     *                      THEN
                          FCT(I)=0.
                            ELSE
                       FCT(I)=0.16666666
                            ENDIF
   10                      CONTINUE
                            RETURN
                             END
