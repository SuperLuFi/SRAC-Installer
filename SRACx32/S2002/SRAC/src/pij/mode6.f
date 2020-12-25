C    *************  MODULE TRKMD6  *************************************
           SUBROUTINE MODE6(NZ,NZZ,IX,IY,IZ,IR)
C             CALLED BY TRKHX6
C          DEFINE REGION NUMBER IN TRIANGULAR MESH IN A HEXAGOAN
C    *******************************************************************
C                       *************       IN EACH     *
C                     *   *   2    *  *     PERIOD    *   *
C                    *  3   *    *  1  *            *   4   *
C      PERIOD       *********************         * * * * * * *
C                     * 4   *    *  6  *        *   *   3   *   *
C                       *  *  5   *  *        *   1   *   *   2   *
C                        ************       * * * * * * * * * * * * *
C
      IF(IX.GT.NZ .AND. IY.GT.NZ .AND. IZ.GT.NZ) GO TO 10
      IF(IX.GT.NZ .AND. IY.LE.NZ .AND. IZ.GT.NZ) GO TO 20
      IF(IX.LE.NZ .AND. IY.LE.NZ .AND. IZ.GT.NZ) GO TO 30
      IF(IX.LE.NZ .AND. IY.LE.NZ .AND. IZ.LE.NZ) GO TO 40
      IF(IX.LE.NZ .AND. IY.GT.NZ .AND. IZ.LE.NZ) GO TO 50
      IF(IX.GT.NZ .AND. IY.GT.NZ .AND. IZ.LE.NZ) GO TO 60
C     WRITE(6,*) ' *** ERROR FOUND IN MODE6 IX=',IX,' IY=',IY,' IZ=',IZ
C    *   ,' *** ERROR STOP ***'
C              STOP
      IR=0
      RETURN
C     MESH INTERVAL NUMBER AS IN THE TRIANGULAR MESH
   10 IPR=1
      IXX=IX-NZ
      IYY=IY-NZ
      IZZ=IZ-NZ
      GO TO 70
   20 IPR=2
      IXX=IZ-NZ
      IYY=IX-NZ
      IZZ=NZ+1-IY
      GO TO 70
   30 IPR=3
      IXX=NZ+1-IY
      IYY=IZ-NZ
      IZZ=NZ+1-IX
      GO TO 70
   40 IPR=4
      IXX=NZ+1-IX
      IYY=NZ+1-IY
      IZZ=NZ+1-IZ
      GO TO 70
   50 IPR=5
      IXX=NZ+1-IZ
      IYY=NZ+1-IX
      IZZ=IY-NZ
      GO TO 70
   60 IPR=6
      IXX=IY-NZ
      IYY=NZ+1-IZ
      IZZ=IX-NZ
C     GO TO 70
   70 IR=IXX**2-2*(IYY-1)-MOD(IXX+IYY+IZZ+1,2)
      IR=NZZ*(IPR-1)+IR
      RETURN
      END
