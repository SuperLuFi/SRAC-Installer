      SUBROUTINE    FACTOR ( FVALUE )
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
      FVAL    =  FVALUE * PSCALE
C
      RETURN
      END

