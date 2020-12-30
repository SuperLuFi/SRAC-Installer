C **********************************************************************
                      SUBROUTINE GMNHX0
C **********************************************************************
C                      CALLED BY GMNHX6 AND GMNHX7
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
          DIMENSION    LOC(6),FCT(6),TY(NTY+1),LL(6),FF(6)
C                  ** KEEP FCT(1),FCT(2) **
                         F11=FCT(1)
                         F22=FCT(2)
C                  IN THE PERIPHERIC PART
                      IF(IX.GT.JRPH)
     *                     THEN
             CALL GMNRT(NRX-JRPH,NTY,IX-JRPH,IY,LOC,FCT)
C            CALL GSADD(4,LOC,FCT,JRPH**2)
             CALL GSHIFT(4,LOC,FCT,JRPH**2,200)
                         GO TO 200
                            ELSE
             CALL GMNTRI(JRPH,IX,IY,LOC,FCT)
                         IF(IY.EQ.0) THEN
             CALL GMNTRI(JRPH,IX,IX,LL ,FF )
             CALL GTRNS (1,LOC(4),FCT(4),LL(5),FF(5))
             CALL GTRNS (1,LOC(5),FCT(5),LL(6),FF(6))
             CALL GTRNS (1,LOC(6),FCT(6),LL(1),FF(1))
                                     ENDIF
                       IF(NRX.EQ.JRPH) GO TO 200
                       IF(IX .LT.JRPH) GO TO 200
                        LOC(6)=0
C   ** NOTE. FOLLOWING STATEMENTS FOR THE CASE IX=JRPH<NRX **
C       IF IX=JRPH THEN FIND REGION NUMBER IN PERIPHERIC
                        DO 100 I=1,NTY+1
         IF(ABS(FLOAT(IY)/FLOAT(JRPH)-TY(I)).LT.1.E-4) THEN
                        LOC(2)=JRPH**2+I
                        LOC(1)=LOC(2)-1
                        FCT(1)=F11
                        FCT(2)=F22
               IF(I.EQ.1)  THEN
                        LOC(1)=JRPH**2+NTY
                        FCT(1)=0.3333333
                        FCT(2)=0.3333333
                        LOC(5)=0
                           ENDIF
               IF(I.EQ.NTY+1)   THEN
                        LOC(1)=0
                        LOC(2)=0
                        FCT(1)=0.
                        FCT(2)=0.
                        LOC(5)=0
                           ENDIF
                           GO TO 200
                                                         ENDIF
         IF(I.EQ.NTY+1) GO TO 100
         IF(FLOAT(IY)/FLOAT(JRPH).LT.TY(I+1)-1.E-4) GO TO 150
  100                     CONTINUE
                           I=NTY
  150                   LOC(1)=JRPH**2+I
                        FCT(1)=0.5
                        LOC(2)=0
                        FCT(2)=0.
                            ENDIF
  200 CONTINUE
*     WRITE(6,*) ' GMNHX0 NRX=',NRX,' JRPH=',JRPH,' IX=',IX,' IY=',IY,
*    *            ' NTY=',NTY,' TY=',TY
*     WRITE(6,*) '  LOC=',LOC
*     WRITE(6,*) '  FACT=',FCT
                            RETURN
                            END
