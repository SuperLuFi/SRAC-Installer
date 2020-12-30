C **********************************************************************
                      SUBROUTINE GMNAME
C **********************************************************************
C         FIND MODERATOR REGION NUMBERS AROUND A PIN ROD
C                      CALLED BY PREHX
C      SUBROUTINES GMNTRI,GMNHEX,GMNREC,GMNHX2,GMNRT ARE CALLED
C **********************************************************************
     *           (NRX,NTY,IX,IY,LOC,FCT,JRPH,TY)
                   DIMENSION    LOC(*),FCT(*)
C     COMMON /IGEOM/ IDUMY1(2),JRPH ,IDUMY2(1),IDUMY3(13),LTY
************************************************************************
C                        HEXAGONAL 60
        CALL GMNHX6(NRX,JRPH,NTY,TY,IX,IY,LOC,FCT)
*       WRITE(6,*) '     MODERATOR G-REGIONS AROUND THE PIN ROD ',
*    * 'LOCATED ON IX=',IX,' IY=',IY
*       WRITE(6,'(A,6I12)')  ' LOC=',(LOC(I),I=1,6)
*       WRITE(6,'(A,6F12.5)')' FCT=',(FCT(I),I=1,6)
                            RETURN
                             END
