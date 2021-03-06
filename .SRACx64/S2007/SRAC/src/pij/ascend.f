C***********************************************************************
      SUBROUTINE ASCEND(ARRAY,LENGTH,IZERO,NAME,I2,IERR)
C     TEST WHETHER IF THE DATA ARE ARRANGED IN ASCENDING ORDER
C     THE ARRAY IS LABELLED BY 'NAME'.
C     I2 IS THE SECONDARY SUBSCRIPT,IF ANY NON-ZERO VALUE SENT.
C     IF IZERO IS 1, THE FIRST DATA MUST BE ZERO
C     IERR: ERROR CODE, IF NOT ZERO , THE EXECUTION SHALL BE STOPPED
C
      DIMENSION ARRAY(LENGTH)
      IERR=0
      IF(IZERO.NE.0) GO TO 10
      IF(ARRAY(1).EQ.0) GO TO 10
      IF(I2.NE.0) WRITE(6,6000) NAME ,I2,ARRAY(1)
      IF(I2.EQ.0) WRITE(6,6001) NAME    ,ARRAY(1)
      IERR=1
      RETURN
C
   10 IF(LENGTH.EQ.1) RETURN
      DO 20 I=2,LENGTH
      I1=I-1
      IF(ARRAY(I1).LE.ARRAY(I)) GO TO 20
      IF(I2.NE.0) WRITE(6,6010) NAME,I1,I2,I,I2,ARRAY(I1),ARRAY(I)
      IF(I2.EQ.0) WRITE(6,6011) NAME   ,I1,I,ARRAY(I1),ARRAY(I)
   20 CONTINUE
      RETURN
C
 6000 FORMAT(' *** THE FIRST DATA OF ARRAY MUST BE ZERO; ',A4,
     *  '(1,',I3,')=',E12.5)
 6001 FORMAT(' *** THE FIRST DATA OF ARRAY MUST BE ZERO; ',A4,
     *  '(1)=',E12.5)
 6010 FORMAT(' *** THE DATA OF ARRAY MUST BE IN ASCENDING ORDER; ',A4,
     * '(',I3,',',I3,') AND (',I3,',',I3,')=',2E12.5)
 6011 FORMAT(' *** THE DATA OF ARRAY MUST BE IN ASCENDING ORDER; ',A4,
     * '(',I3,') AND (',I3,')=',2E12.5)
      END
