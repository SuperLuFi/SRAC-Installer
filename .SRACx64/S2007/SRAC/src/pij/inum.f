C             INUM                LEVEL=1        DATE=94.06.29
C
C***********************************************************************
C                                                                      *
C       INUM      : DRAWING INTEGER NUMBER                             *
C                                                                      *
C                                                                      *
C       MODULE NO.  :                                                  *
C       MODULE TYPE : SUBROUTINE                                       *
C                                                                      *
C***********************************************************************
C                                                                      *
C    -- CALLED BY --                                                   *
C                                                                      *
C       GEOM01...GEOM13                                                *
C       GEOM09...GEOM12                                                *
C                                                                      *
C......................................................................*
C                                                                      *
C    -- INPUT --                                                       *
C                                                                      *
C       (ARGUMENT)                                                     *
C         XO      :  X POSITION OF STRING'S CENTER                     *
C         YO      :  Y POSITION OF STRING'S CENTER                     *
C         IFTYPE  :  FIGUARE TYPE     =1,2,3                           *
C         ISIZE   :  CHARACTER SIZE                                    *
C         NDUM    :  INTEGER TO BE WRITTEN                             *
C                                                                      *
C                                                                      *
C    -- LOCAL VARIABLES --                                             *
C                                                                      *
C        SCALE    :  SCALE FACTOR'S TABLE  (IT'S INDICATED BY IFTYPE)  *
C        CLENGS   :  LENGTH OF CHARACTER (MM)                          *
C        VCLEN    :  VERTUAL LENGTH OF STRINGS                         *
C        CN       :  TABLE OF DECIMAL CHARACTERS                       *
C        CHAR     :  DECIMAL STRINGS  (CONVERTED FROM NDUM)            *
C                                                                      *
C***********************************************************************
C
C
      SUBROUTINE  INUM (XO,YO,IFTYPE,ISIZE,NDUM)
C
      DIMENSION  SCALE(3),CLENGS(6)
      CHARACTER*1    CN(10),CHAR(20)
      DATA  CN / '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' /
      DATA  SCALE  /  1.0, 0.75, 0.5  /
      DATA  CLENGS  /  1.69,  2.54,  3.17,  4.23,  6.77,  13.54  /
C
C**********  DATA  CHECK  **********************************************
C
      IF ( NDUM .LE. 0 )
     @  WRITE(6,6000) XO,YO,IFTYPE,ISIZE,NDUM
 6000   FORMAT(' ** CAUTION IN INUM ** NON-POSITIVE NUMBER IS',
     @  ' DETECTED ** (X,Y=', 2F7.3,
     @      ' )  IFTYPE =',I2,' ISIZE =',I2,' N =',I10)
C
C
C
C**********  CONVERTING  NDUM(INTEGER)  TO  CHAR(CHARACTER)  ***********
C
   10 NN = NDUM
      DO  20  I = 1,20
         IF ( NN .LT. 10**I )  GO TO 21
   20    CONTINUE
   21 NCHAR = I
      DO  30  I = 1,NCHAR
         L = 10**(NCHAR-I)
         N = NN/L
         NN = MOD(NN,L)
   30    CHAR(I) = CN(N+1)
C     ENCODE (20,100,STRING) (CHAR(I),I=1,NCHAR)
C 100 FORMAT (20A1)
C
C**********  WRITING CONVERTED CHARACTER  ******************************
C
      VCLEN = CLENGS(ISIZE)/SCALE(IFTYPE)
      XL = VCLEN*NCHAR*0.8
      YL = VCLEN
C     CALL  GSCHAR (XO-XL/2,YO-YL/2,ISIZE,CHAR,111,NCHAR)
      CALL  UGSCHA (XO-XL/2,YO-YL/2,ISIZE,CHAR,111,NCHAR)
C
      RETURN
      END
