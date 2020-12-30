C
      SUBROUTINE    NUMBER ( X , Y , HH , FLT , ANGLE , N )
C
C/******************************************************************
C *                                                                *
C *    PIFLIB.H : HEADER FILE FOR PLOTTER ROUTINE ON WS            *
C *                                                                *
C ******************************************************************/
C
C    FLOAT XORG, YORG           /* X, Y ORIGIN ON X-WINDOW           */
C    FLOAT PSXORG, PSYORG       /* X, Y ORIGIN ON POST SCRIPT        */
C    FLOAT XPT0, YPT0           /* TERMINATE POINTS OF BEFORE STROKE */
C    FLOAT PSSTRX, PSSTRY       /* TERMINATE POINTS OF PS STROKE     */
C    FLOAT XPIXMM, YPIXMM       /* VALUE A PIXEL PER MM              */
C    FLOAT FVAL                 /* FACTOR VALUE                      */
C    FLOAT XSTR, YSTR           /* CURRENT POINT OF WHERE ROUTINE    */
C    
C    CHAR IFLAG                 /* CONDITION FLAG FOR GDFILE         */
C    CHAR IFPLT3                /* IF CALL PLOT(,,(+OR-)3)           */ 
C    CHAR PSPLOT                /* IF DRAWING BEFORE IN POST SCRIPT  */
C    CHAR IFDRAW                /* IF DRAWING BEFORE IN X-WINDOW     */
C    LONG LTYPE                 /* CURRENT LINE TYPE                 */
C    FLOAT PSCALE               /* SCALING FACTOR OF XPIF            */
C    CHAR IFXPIF                /* IF CALLED FROM  XPIF              */
C    LONG IPUNIT                /* PS FILE LOGICAL UNIT NUMBER       */
C    UNSIGNED LONG COLOR[8]     /* COLOR VALUE                       */
C    
      COMMON / BIFLIB /
     1    XORG    , YORG    , PSXORG  , PSYORG  , XPT0    , YPT0    ,
     2    PSSTRX  , PSSTRY  , XPIXMM  , YPIXMM  , FVAL    ,
     3    XSTR    , YSTR    , IFLAG   , IFPLT3  , PSPLOT  ,
     4    IFDRAW  , LTYPE   , PSCALE  , IFXPIF  , IPUNIT  , COLOR(8)
C	
C/******************************************************************
C *                                                                *
C *    GGSX.H : HEADER FILE FOR GGS ROUTINES ON EWS                *
C *                                                                *
C ******************************************************************/
C
      COMMON /GCOMXY/ JFILM, KPEN, JOPT, KERR
      COMMON /GCOMXY/ FCT, HFCT, OLX, OLY, XRG, YRG, FF
C
      COMMON /LTPCOM/ LTP001, LTP002
C
C
      CHARACTER*1  NUMB(80), TEMP(80)
CMSASA NAME 'CHAR' SHOULD BE USED AS INTRINSIC FUNCTION.
C     CHARACTER*80 CHAR
      CHARACTER*80 CHARS
C
      EQUIVALENCE (TEMP(1), CHARS)
      REAL*8  FFLT , DFLT
      DATA    MAXS  /  80  /
C
      IF ( N .GT. 9 )        N      =  9
C	
      DFLT    =  FLT
      FFLT    =  DFLT + 0.0000000005
      IF ( ABS(FFLT) .GE. 1.0E+9 )    THEN
         FFLT      =  1.0E+10
      END IF
C
      WRITE(CHARS,'(F19.9)')   FFLT
      J       =  0
      DO  100  I  =  1  ,  MAXS
         IF ( TEMP(I) .NE. ' ' )    THEN
             J          =  J + 1
             NUMB(J)  =  TEMP(I)
         END IF
  100 CONTINUE
      DO  200  I  =  1  ,  MAXS
         IF ( NUMB(I) .EQ. '.' )    THEN
            NCHAR   =  I
            GO TO  210
         END IF
  200 CONTINUE
  210 CONTINUE
C
CMSASA ... NO NEED TO ADD '\0' FOR FORTRAN STRING HERE ...
C      ... ADDING CHAR(0) MAY BE BETTER IF SYMBOL IS CODED WITH C
      IF ( N .EQ. 0 )    THEN
C        NUMB(NCHAR+1)  =  '\0'
         NUMB(NCHAR+1)  =  CHAR(0)
      ELSE IF ( N .LT. 0 )    THEN
C        NUMB(NCHAR)  =  '\0'
         NUMB(NCHAR)  =  CHAR(0)
         NCHAR   =  NCHAR - 1
      ELSE
         NCHAR   =  NCHAR + N
C        NUMB(NCHAR+1)  =  '\0'
         NUMB(NCHAR+1)  =  CHAR(0)
      END IF
C
      CALL  SYMBOL ( X , Y , HH , NUMB , ANGLE , NCHAR )
C
      RETURN
      END
