/*
 *    **************************************************
 *    *                                                *
 *    *                   BIGGVAR.H                    *
 *    *                                                *
 *    *          Extended Precision Calculator         *
 *    *                                                *
 *    *           External Global Variables            *
 *    *                                                *
 *    *              Version 4.3 02-04-89              *
 *    *                                                *
 *    *              Judson D. McClendon               *
 *    *              329 37th Court N.E.               *
 *    *              Birmingham, AL 35215              *
 *    *                 205-853-8440                   *
 *    *            Compuserve [74415,1003]             *
 *    *                                                *
 *    **************************************************
 */



/*
 *    **************************************************
 *    *                                                *
 *    *           External Global Variables            *
 *    *                                                *
 *    **************************************************
 */

extern NORMTYPE
   stack[4],               /* Stack registers  */
   reg[10],                /* Memory registers */
   lastx;                  /* Last X register  */

extern WORKTYPE
   work[3];                /* Work registers   */

extern int
   normprec,               /* Normal precision  */
   compprec,               /* Compute precision */
   workprec,               /* Work precision    */
   groupsize,              /* Digit group size  */
   entrysignrow,           /* Row for X entry   */
   menunbr,                /* Menu number       */
   chr;                    /* Input Character   */

extern long
   minfloatprn,            /* Min exp for float */
   maxfloatprn;            /* Max exp for float */

extern FILE
   *printfile;             /* Print file handle */

extern char
   stackname[4],           /* Stack register names */
   printid[15];            /* Print file name      */

extern BOOLEAN
   stacklift,              /* Lift stack for new X if TRUE */
   scinotation,            /* Force sci notation if TRUE   */
   charpresent,            /* Character present if TRUE    */
   menucleared,            /* Screen menu cleared if TRUE  */
   printtoscreen;          /* Print to screen if TRUE      */
