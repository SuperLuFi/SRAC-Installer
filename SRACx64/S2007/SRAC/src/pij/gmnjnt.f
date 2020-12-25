C **********************************************************************
                      SUBROUTINE GMNJNT
C **********************************************************************
C                  CALLED BY GRVOL AS THE REPLACEMENT OF GMNAME
C   FIND MODERATOR REGION NUMBERS OF TRI-ANGULAR MESH AROUND A PIN ROD
C **********************************************************************
     *      (JRPH,IX,IY,RPIN,PITCH,LOC,FCT)
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
      DATA CRT3/1.7320508/
      IF(RPIN.GT.CRT3*PITCH) GO TO 200
      IF(IX.EQ.IY+1)  GO TO 210
      IF(IX.EQ.IY-1)  GO TO 210
      IF(IY.EQ.1)  GO TO 210
      IF(IX.EQ.1)  GO TO 210
      IF(IX.EQ.JRPH-1)  GO TO 210
      IF(IY.EQ.JRPH-1)  GO TO 210
C
      CALL GMNJH6(JRPH,IX,IY,RPIN,PITCH,LOC,FCT)
C
C ********  ROOT(3)*PITCH < RPIN ** ERROR STOP *****************
  200 WRITE(6,*) '  *** TOO BIG PIN RADIUS ',RPIN,' COMPARED WITH '
     * ,'TRIANGULAR PITCH ',PITCH,' ** ERROR STOP AT GMNJNT **'
                             STOP
  210 WRITE(6,*) '  *** IMPROPER POSITION OF A BIG PIN ROD '
     * ,' IX=',IX,' IY=',IY,' WHILE MAXIMUM NRX=',JRPH
                             STOP
                             END
