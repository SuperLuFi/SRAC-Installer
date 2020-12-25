C******** FUNCTION INVERT REGION NUMBER OWING TO OCTANT SYMMETRY
      FUNCTION IOCTS(IJ,IZ,JZ,NX,NY)
      IF(IZ.NE.1 .AND. IZ.NE.2) GO TO 100
      IF(JZ.NE.1 .AND. JZ.NE.2) GO TO 100
      IF(NY.GT.NX)              GO TO 100
           IF(IJ.EQ.1)          GO TO 100
      IF(NY.EQ.NX)   THEN
           IF(IJ.EQ.2)          GO TO 100
           IF(IJ.EQ.3) IOCTS=2
           IF(IJ.EQ.4) IOCTS=3
                     ELSE
           IF(IJ.EQ.4)          GO TO 100
           IF(IJ.EQ.2) IOCTS=3
           IF(IJ.EQ.3) IOCTS=2
                     ENDIF
      RETURN
  100 IOCTS=IJ
  200 RETURN
      END
