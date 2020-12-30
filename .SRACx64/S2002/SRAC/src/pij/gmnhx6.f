C **********************************************************************
                      SUBROUTINE GMNHX6
C **********************************************************************
C                      CALLED BY GMNAME
C         FIND MODERATOR REGION NUMBERS AROUND A PIN ROD
C                   IN A HEXANT OF A HEXAGONAL CELL
C **********************************************************************
C                                                 *
C                                                *  *
C                                               *     *
C                                              *       *
C                             G-REGION MAP    *16*  18   *
C                        *                   *  15 *       *
C  DIRECTION           *   *                *  9  14*    *   *
C                    *   2   *             *    8   13 *      *
C                  *   3   1   *          *  4    7   12*   17 *
C                *     4   6     *       *    3    6    11 *     *
C              *         5         *    * 1     2    5    10*     *
C            ************************* *****************************
C
C **********************************************************************
     *       (NRX,JRPH,NTY,TY,IX,IY,LOC,FCT)
       DIMENSION    LOC(6),FCT(6),TY(NTY+1)
C***********************************************************************
            IF(IX.EQ.0 .AND. IY.EQ.0) THEN
                    LOC(1)=1
                    FCT(1)=0.16666666
                    DO 10 I=2,6
                    LOC(I)=0
                    FCT(I)=0.
  10                CONTINUE
                    GO TO 200
                                    ENDIF
       CALL GMNHX0(NRX,JRPH,NTY,TY,IX,IY,LOC,FCT)
            IF(IY.EQ.IX   .AND. IX.LE.JRPH)  GO TO 250
            IF(IY.EQ.NTY .AND. IX.GT.JRPH)   GO TO 250
  200 CONTINUE
*     WRITE(6,*) ' GMNHX6 NRX=',NRX,' JRPH=',JRPH,' IX=',IX,' IY=',IY,
*    *            ' NTY=',NTY,' TY=',TY
*     WRITE(6,*) '  LOC=',LOC
*     WRITE(6,*) '  FACT=',FCT
                            RETURN
  250                DO 260 I=1,6
                     LOC(I)=0
                     FCT(I)=0.
  260                CONTINUE
                     GO TO 200
                            END
