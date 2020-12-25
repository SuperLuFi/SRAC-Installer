#ifndef NO_CPUTIME_C

#include <stdio.h>
#include <unistd.h>

#ifdef SUN_C

#include <sys/types.h>
#include <sys/times.h>

#elif POSIX_C 

#include <sys/times.h>

#elif ANSI_C 

#include <time.h>

#endif


/******************************************************************
 *  Fortran called C subroutine :
 *
 *   UNIX venders have their own convention or rule for external
 *  names of FORTRAN coded subprograms or commons.
 *
 *   We prepare equivalent sources for C coded routines
 * 
 *     1. Attach underscore to FORTRAN name. ( SUN & Other BSD UNIX)
 *     2. Same as FORTRAN name (lowercase).   (HP-UX,IBM-AIX etc.)
 *     3. Uppercase string of FORTRAN name.   ( Ncube etc. )
 *     4. "_" + uppercase string of FORTRAN name.   ( HI/OSF )
 *
 *******************************************************************/
/********************************************************************
 *    1st version:  15 Oct 1992
 *    2nd version:  7 Mar 1996
 *             For non POSIX but ANSI compilers, enable monitoring
 *             using "clock" function ( it may return bogus value
 *             if a calling interval is more than ~36 minuites
 *             when unit of the time tick value is microsecond.
 *******************************************************************/

#ifdef POSIX_C

/*******************************************************************
 *
 *    Elapsed CPU time monitor on OS's having POSIX library routine
 *    "times".
 *    ( the function "times" is defined in POSIX but not in ANSI-C)
 *   
 *
 *    Programmed by M.Sasaki (the Japan Research Institute Co. Ltd.)
 *    
 *
 ******************************************************************/


static int init =0 ;

void cputime_( ttt )
   float * ttt ;
{
   static struct tms  tt ;
   static double resolution ;

   if( init == 0 )
   {
       resolution = 1.0/(double)sysconf( _SC_CLK_TCK ) ;
       times( &tt ) ;
       init = 1 ;
   }
   times( &tt ) ;
   /**return (tt.tms_utime + tt.tms_stime)*resolution **/;
   *ttt =  (float)((tt.tms_utime + tt.tms_stime)*resolution) ;
}

void cputime ( ttt )
   float * ttt ;
{
   static struct tms  tt ;
   static double resolution ;

   if( init == 0 )
   {
       resolution = 1.0/(double)sysconf( _SC_CLK_TCK ) ;
       times( &tt ) ;
       init = 1 ;
   }
   times( &tt ) ;
   *ttt =  (float)((tt.tms_utime + tt.tms_stime)*resolution) ;
}
void CPUTIME( ttt )
   float * ttt ;
{
   static struct tms  tt ;
   static double resolution ;

   if( init == 0 )
   {
       resolution = 1.0/(double)sysconf( _SC_CLK_TCK ) ;
       times( &tt ) ;
       init = 1 ;
   }
   times( &tt ) ;
   *ttt =  (float)((tt.tms_utime + tt.tms_stime)*resolution) ;
}
void _CPUTIME( ttt )
   float * ttt ;
{
   static struct tms  tt ;
   static double resolution ;

   if( init == 0 )
   {
       resolution = 1.0/(double)sysconf( _SC_CLK_TCK ) ;
       times( &tt ) ;
       init = 1 ;
   }
   times( &tt ) ;
   *ttt =  (float)((tt.tms_utime + tt.tms_stime)*resolution) ;
}

#endif /** end of POSIX_C version **/

#ifdef SUN_C

/*******************************************************************
 *
 *    Elapsed CPU time monitor on SUN
 *
 *    Programmed by M.Sasaki (the Japan Research Institute Co. Ltd.)
 *    
 *    1st version:  20 Jan 1993
 *
 ******************************************************************/


static int init =0 ;

void cputime_( ttt )
   float * ttt ;
{
   static struct tms  tt ;
   static double resolution ;

   if( init == 0 )
   {
#ifdef _SC_CLK_TCK
       resolution = 1.0/(double)sysconf( _SC_CLK_TCK ) ;
#else
       resolution = 1.0/(double)60.0 ;
#endif
       times( &tt ) ;
       init = 1 ;
   }

   times( &tt ) ;
   *ttt =  (float)((tt.tms_utime + tt.tms_stime)*resolution) ;
}

#endif /** end of SUN_C version **/

#ifdef ANSI_C

static int init =0 ;
void MY_CPUTIME() ;

void cputime( ttt )
   float * ttt ;
{
   MY_CPUTIME(ttt) ;
}
void cputime_( ttt )
   float * ttt ;
{
   MY_CPUTIME(ttt) ;
}
void CPUTIME( ttt )
   float * ttt ;
{
   MY_CPUTIME(ttt) ;
}
void _CPUTIME( ttt )
   float * ttt ;
{
   MY_CPUTIME(ttt) ;
}

void MY_CPUTIME( ttt )
   float * ttt ;
{
   static clock_t tt, t1;
   static double resolution, tttd ;
   static double accum_time = 0.0 ;

#define MIN_36 35.791394133333   /*** 2^31 / 1000,000 / 60 ***/

   if( init == 0 )
   {
       tt   = clock() ;
       accum_time = (double)tt / CLOCKS_PER_SEC ;

       if ( CLOCKS_PER_SEC == 1000000 )
       {
          init = 1 ;  /** clock tic is micro second  **/
       }
       else
          init = 2 ;

       resolution = 1.0/(double)CLOCKS_PER_SEC ;
   }

   t1 = clock() ;
   accum_time += (double)(t1 - tt)*resolution ;
   if( init == 1 && t1 < tt ) accum_time += MIN_36 ;
   tt   = t1 ;
   *ttt = (float)accum_time ;
}
#endif /** end of ANSI_C version **/

#endif /** end of ifndef **/
