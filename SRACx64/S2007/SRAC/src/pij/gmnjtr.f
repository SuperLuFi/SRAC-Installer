C **********************************************************************
                      SUBROUTINE GMNJTR
C **********************************************************************
C                  CALLED BY GMNJNT
C   FIND MODERATOR REGION NUMBERS OF TRI-ANGULAR MESH AROUND A PIN ROD
C **********************************************************************
     *         (NRX,IX,IY,RPIN,PITCH,LOC,FCT)
C **********************************************************************
C                   DIRECTION                   G-REGION MAP
C                   *  12  10  *                   *  25  *
C                 * 13   11   09 *                *   24   *
C                *  14   02   08  *              * 16   23  *
C              *  15   03  01   07  *           *  15   22   *
C              *  16   04  06   24  *          * 09   14  21  *
C                *   17  05   23  *           *  08   13  20   *
C                  * 18  20   22 *           * 04   0   12  19  *
C                    * 19  21*              *  03  06   11  18   *
C                                          * 01  02   05   10  17 *
C
C  ****************************************************************
           DIMENSION    LOC(24),FCT(24)
           DATA CRT3,PI/1.7320508,3.1415926/
C
                        LOC(24)=(IX+1)**2+2*IY
                        LOC( 7)=LOC(24)+1
                        LOC( 8)=LOC( 7)+1
                        LOC( 9)=LOC( 8)+1
                        LOC(10)=LOC( 9)+1
                        LOC(22)=IX**2+2*IY-2
                        LOC(23)=LOC(22)+1
                        LOC(6)=LOC(23)+1
                        LOC(1)=LOC( 6)+1
                        LOC(2)=LOC( 1)+1
                        LOC(11)=LOC( 2)+1
                        LOC(12)=LOC(11)+1
                        LOC(21)=(IX-1)**2+2*IY-3
                        LOC(20)=LOC(21)+1
                        LOC( 5)=LOC(20)+1
                        LOC( 4)=LOC( 5)+1
                        LOC( 3)=LOC( 4)+1
                        LOC(14)=LOC( 3)+1
                        LOC(13)=LOC(14)+1
                        LOC(19)=(IX-2)**2+2*IY-3
                        LOC(18)=LOC(19)+1
                        LOC(17)=LOC(18)+1
                        LOC(16)=LOC(17)+1
                        LOC(15)=LOC(16)+1
C
           IF(RPIN.GT.PITCH) GO TO 100
C ********  ROOT(0.75)*PITCH < RPIN < PITCH *******************
           ARG=CRT3*PITCH/2.
           TH=ACOS(ARG/RPIN)
           F1=ARG*RPIN*SIN(TH)+(PI/6.-TH)*RPIN**2
           F3=RPIN*(TH*RPIN-ARG*SIN(TH))
           F2=0.
                   GO TO 110
  100      IF(RPIN.GT.CRT3*PITCH) GO TO 200
C ********  PITCH < RPIN < ROOT(3)*PITCH *******************
           TH=ASIN((3.*PITCH-SQRT(4.*RPIN**2-3.*PITCH**2))/4./RPIN)
           TH2=PI/6.-TH
           F1=CRT3*PITCH**2/4.
           F2=RPIN*(RPIN*TH2-PITCH*SIN(TH2))/2.
           F3=RPIN*(TH*RPIN+PITCH*SIN(TH2))-F1
  110            CALL CLEA(FCT(1),6,F1)
                         DO 120 J=1,6
                       FCT(3*J+4)=F2
                       FCT(3*J+5)=F3
                       FCT(3*J+6)=F2
  120                      CONTINUE
                    IF(IX.EQ.IY) THEN
                  CALL ICLEA(LOC(2),3,0)
                  CALL ICLEA(LOC(10),9,0)
                  CALL  CLEA(FCT(2),3,0.)
                  CALL  CLEA(FCT(10),9,0.)
                                 ENDIF
                    IF(IY.EQ.0 ) THEN
                  CALL ICLEA(LOC(4),3,0)
                  CALL ICLEA(LOC(16),9,0)
                  CALL  CLEA(FCT(4),3,0.)
                  CALL  CLEA(FCT(16),9,0.)
                                 ENDIF
                    IF(IX.EQ.NRX) THEN
                 CALL ICLEA(LOC(1),2,0)
                 CALL ICLEA(LOC(6),7,0)
                 CALL ICLEA(LOC(22),3,0)
                 CALL  CLEA(FCT(1),2,0.)
                 CALL  CLEA(FCT(6),7,0.)
                 CALL  CLEA(FCT(22),3,0.)
                                 ENDIF
*     WRITE(6,198) ' GMNTRJ LOC=',LOC
*     WRITE(6,199) ' GMNTRJ FAC=',FCT
* 198 FORMAT(A,10I12/(12X,10I12))
* 199 FORMAT(A,10F12.5/(12X,10F12.5))
                            RETURN
C ********  ROOT(3)*PITCH < RPIN ** ERROR STOP *****************
  200 WRITE(6,*) '  *** TOO BIG PIN RADIUS ',RPIN,' COMPARED WITH '
     * ,'TRIANGULAR PITCH ',PITCH,' ** ERROR STOP AT GMNJNT **'
                             STOP
                             END
