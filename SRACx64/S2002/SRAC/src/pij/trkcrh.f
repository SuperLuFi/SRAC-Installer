C    *******************************************************************
                  SUBROUTINE TRKCRH
     *     (P,HI,HO,ISHRI,ISHRO,ANG,IER)
C                 CALLED BY  TRKHX6
C TWO CROSSING POINTS HI AND HO WITH SIGHNED DISTANCE BETWEEN H(FOOT
C OF O ON THE LINE RHO ANG) AND ANY OF SIX HEXAGONAL SIDES OF P
C LOCATED AT THE CENTER
C    *******************************************************************
C                 ARGUMENTS OF THE ROUTINE
C    P    SIDE LENGTH OF THE HEXAGON
C    HI,HO   DISTANCES BETWEEN THE ENTRY AND EXIT AND H
C    *******************************************************************
      COMMON / PIJ1C / NX,NY,NTPIN,NAPIN,NCELL,NM,NGR,NGA,NDPIN,IDIVP,
     1                 BETM,NX1,NY1,LOCAL(3),IXP,IYP,IZP,NDPIN1,
     2                 NDR,NDA,LL,L0,RO1,DRO,FVOL,LOCAL1,LOCAL2,RR,
     3                 DUM0,LOCAL3,LOCAL4,SINB,COSB,LOCAL5
C    *******************************************************************
C     DIMENSION TT(1),IM(1),IP(1)
C    *******************************************************************
      DIMENSION HH(6),AA(6),BB(6),CC(6)
      DIMENSION JJ(6,12,2)
      DATA AA/1.7320508,0.,-1.7320508,1.7320508,0.,-1.7320508/
      DATA BB/1.,1.,1.,1.,1.,1./
      DATA CC/-1.7320508,-0.8660254,-1.7320508,1.7320508,0.8660254
     *                  , 1.7320508/
      DATA PI/3.1415926/
C     DIMENSION JJ(6,12,2)
      DATA JJ/
     * 3,5,4,6,1,2,    3,5,4,1,2,6,   3,4,5,6,1,2,   3,4,5,1,2,6,
     * 4,6,5,1,2,3,    4,6,5,2,1,3,   4,5,6,1,2,3,   4,5,6,2,1,3,
     * 1,5,6,2,3,4,    1,5,6,3,2,4,   5,6,1,2,3,4,   5,6,1,3,2,4,
     * 5,4,3,1,2,6,    3,5,4,1,2,6,   5,4,3,2,1,6,   3,5,4,2,1,6,
     * 6,5,4,2,1,3,    4,6,5,2,1,3,   5,6,4,3,2,1,   4,6,5,3,2,1,
     * 1,6,5,3,2,4,    1,5,6,3,2,4,   1,6,5,4,3,2,   1,5,6,4,3,2/
C***********************************************************************
       RHO2=RR
C************************************
                   DO 10 I=1,6
      CALL TRKCRS(AA(I),BB(I),CC(I)*P,HH(I),ANG)
   10              CONTINUE
                   IF(RHO2.GT.0.) THEN
                   IFLAG=2
                                 ELSE
                   IFLAG=1
                                 ENDIF
                   DO 20  I=1,12
                   ISHRI=JJ(3,I,IFLAG)
                   ISHRO=JJ(4,I,IFLAG)
      IF(MAX(HH(JJ(1,I,IFLAG)),HH(JJ(2,I,IFLAG))).LE.HH(JJ(3,I,IFLAG)))
     *              THEN
      IF(HH(JJ(3,I,IFLAG)) .LT. HH(JJ(4,I,IFLAG))) THEN
      IF(MIN(HH(JJ(5,I,IFLAG)),HH(JJ(6,I,IFLAG))).GE.HH(JJ(4,I,IFLAG)))
     *              THEN
                   GO TO 30
                    ENDIF
                                                    ENDIF
                    ENDIF
   20 CONTINUE
      N=2
C     WRITE(6,*) ' CRH - NEITHER ENTRY NOR EXIT FOUND RHO=',RHO,' ANG='
C    * ,ANG*180/PI,' DEG ON P=',P
C     WRITE(6,*) '    HH(6)=',HH
      IER =1
      RETURN  
   30 CONTINUE
      N=1
      HI=HH(ISHRI)
      HO=HH(ISHRO)
C     WRITE(6,*)'HI=',HI,'HO=',HO
                    RETURN
                    END
