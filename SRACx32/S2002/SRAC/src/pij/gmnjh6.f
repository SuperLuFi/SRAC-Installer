C **********************************************************************
                      SUBROUTINE GMNJH6
C **********************************************************************
C                      CALLED BY GMNJNT
C         FIND MODERATOR REGION NUMBERS AROUND A PIN ROD
C                   IN A HEXANT OF A HEXAGONAL CELL
C **********************************************************************
     *       (NRX,IX,IY,RPIN,PITCH,LOC,FCT)
       DIMENSION    LOC(24),FCT(24)
C***********************************************************************
       CALL GMNJH0(NRX,IX,IY,RPIN,PITCH,LOC,FCT)
            IF(IY.EQ.IX .AND. IX.GT.0)  GO TO 250
*     WRITE(6,*) ' GMNJH6 NRX=',NRX,' IX=',IX,' IY=',IY,
*    *   'PIN RAD=',RPIN,' PITCH=',PITCH
*     WRITE(6,*) '  LOC=',LOC
*     WRITE(6,*) '  FACT=',FCT
  200                       RETURN
  250                DO 260 I=1,24
                     LOC(I)=0
                     FCT(I)=0.
  260                CONTINUE
                     GO TO 200
                            END
