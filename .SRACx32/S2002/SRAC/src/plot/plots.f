C
      SUBROUTINE    PLOTS( OPTION, IDUMMY )
c
c/******************************************************************
c *                                                                *
c *    piflib.h : Header File for Plotter Routine on WS            *
c *                                                                *
c ******************************************************************/
c
c    float xorg, yorg           /* x, y origin on X-Window           */
c    float psxorg, psyorg       /* x, y origin on Post Script        */
c    float xpt0, ypt0           /* Terminate Points of before stroke */
c    float psstrx, psstry       /* Terminate Points of PS stroke     */
c    float xpixmm, ypixmm       /* Value a pixel per mm              */
c    float fval                 /* factor value                      */
c    float xstr, ystr           /* Current Point of where routine    */
c
c    char iflag                 /* Condition flag for GDFile         */
c    char ifplt3                /* if call plot(,,(+or-)3)           */
c    char psplot                /* if drawing before in Post Script  */
c    char ifdraw                /* if drawing before in X-Window     */
c    long ltype                 /* Current Line Type                 */
c    float pscale               /* scaling factor of xpif            */
c    char ifxpif                /* if called from  xpif              */
c    long ipunit                /* ps file logical unit number       */
c    unsigned long color[8]     /* Color value                       */
c
      common / biflib /
     1    xorg    , yorg    , psxorg  , psyorg  , xpt0    , ypt0    ,
     2    psstrx  , psstry  , xpixmm  , ypixmm  , fval    ,
     3    xstr    , ystr    , iflag   , ifplt3  , psplot  ,
     4    ifdraw  , ltype   , pscale  , ifxpif  , ipunit  , color(8)
c
c/******************************************************************
c *                                                                *
c *    ggsx.h : Header File for GGS Routines on EWS                *
c *                                                                *
c ******************************************************************/
c
      common /gcomxy/ jfilm, kpen, jopt, kerr
      common /gcomxy/ fct, hfct, olx, oly, xrg, yrg, ff
c
      common /ltpcom/ ltp001, ltp002
c
c
      character     option(1)
c
      character*80  pscom(30)
c
CKSK  data  XORIG, YORIG / 20.0 , 17.0 /
      data  XORIG, YORIG / 50.0 , 17.0 /
c
      pscom( 1) =  '%!PS-Adobe-2.0'
      pscom( 2) =  '%%Creator: SRAC95'
      pscom( 3) =  '%%EndComments'
      pscom( 4) =  '/pifdict 30 dict def'
      pscom( 5) =  'pifdict begin'
      pscom( 6) =  '/Color true def'
      pscom( 7) =
     *  '/DC {Color {setrgbcolor} {pop pop pop 0 setgray} ifelse} def'
      pscom( 8) =  '/LC0 { stroke 0 0 0 DC } def'
      pscom( 9) =  '/LC1 { stroke 1 1 1 DC } def'
      pscom(10) =  '/LC2 { stroke 1 0.1 0.1 DC } def'
      pscom(11) =  '/LC3 { stroke 0.4 1 0.1 DC } def'
      pscom(12) =  '/LC4 { stroke 0.1 0.2 1 DC } def'
      pscom(13) =  '/LC5 { stroke 0.3 1 1 DC } def'
      pscom(14) =  '/LC6 { stroke 1 0.2 1 DC } def'
      pscom(15) =  '/LC7 { stroke 1 1 0.1 DC } def'
      pscom(16) =  '/LT1 { stroke [5 5] 0 setdash } def'
      pscom(17) =  '/LT2 { stroke [10 5] 0 setdash } def'
      pscom(18) =  '/LT3 { stroke [10 5 5 5] 0 setdash } def'
      pscom(19) =  '/LT4 { stroke [10 5 5 5 5 5] 0 setdash } def'
      pscom(20) =  '/LT5 { stroke [10 5 5 5 5 5 5 5] 0 setdash } def'
      pscom(21) =  '/LT6 { stroke [10 5 2 5] 0 setdash } def'
      pscom(22) =  '/LT7 { stroke [10 5 2 2 2 5] 0 setdash } def'
      pscom(23) =  '/LT8 { stroke [10 5 2 2 2 2 2 5] 0 setdash } def'
      pscom(24) =  '/LT9 { stroke [10 5 8 5 2 5 8 5] 0 setdash } def'
      pscom(25) =  '/LT10 { stroke [2 2] 0 setdash } def'
      pscom(26) =  '/LT11 { stroke [2 2 2 2 2 10] 0 setdash } def'
      pscom(27) =  '/M {moveto} def'
      pscom(28) =  '/L {lineto} def'
      pscom(29) =  'end'
      pscom(30) =  '%%EndInitialize'
c
      ipunit    =  89
c
      xpixmm  =  11.3385827
      ypixmm  =  11.3385827
c
      psxorg  =  XORIG
CITJ+97/07/08
C     psyorg  =  XORIG
      psyorg  =  YORIG
CITJ-97/07/08
c
      ltype   =  0
      psplot  =  0
      pscale  =  1.0
      fval    =  1.0
c
      ifilm   =  0
      kpen    =  1
      jopt    =  0
      kerr    =  0
      fct     =  1.0
      hfct    =  0.10583353
      olx     =  0.0
      oly     =  0.0
      xrg     =  psxorg
      yrg     =  psyorg
      ff      =  0.0
c
      do  100  i  =  1  ,  30
         write(ipunit,'(a)')  pscom(i)
  100 continue
c
C   intatr();
c
      return
      END
