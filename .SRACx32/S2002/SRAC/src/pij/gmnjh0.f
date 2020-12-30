C **********************************************************************
                      SUBROUTINE GMNJH0
C **********************************************************************
C                      CALLED BY GMNJH6 AND GMNJH7
C         FIND MODERATOR REGION NUMBERS AROUND A PIN ROD
C                   IN A HEXANT OF A HEXAGONAL CELL
C **********************************************************************
     *       (NRX,IX,IY,RPIN,PITCH,LOC,FCT)
          DIMENSION    LOC(24),FCT(24),LL(24),FF(24)
             CALL GMNJTR(NRX,IX,IY,RPIN,PITCH,LOC,FCT)
                 IF(IX.GT.0 .AND. IY.EQ.0) THEN
             CALL GMNJTR(NRX,IX,IX,RPIN,PITCH,LL,FF)
             CALL GTRNS (2,LOC(4),FCT(4),LL(5),FF(5))
             CALL GTRNS (1,LOC(6),FCT(6),LL(1),FF(1))
             CALL GTRNS (6,LOC(16),FCT(16),LL(19),FF(19))
             CALL GTRNS (3,LOC(22),FCT(22),LL( 7),FF( 7))
                                           ENDIF
*     WRITE(6,*) ' GMNJH0 NRX=',NRX,' IX=',IX,' IY=',IY,
*    *   'PIN RAD=',RPIN,' PITCH=',PITCH
*     WRITE(6,'(A,10I12/(7X,10I12))') '  LOC=',LOC
*     WRITE(6,'(A,10F12.5/(7X,10F12.5))') '  FACT=',FCT
                            RETURN
                            END
