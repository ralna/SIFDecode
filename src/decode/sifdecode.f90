! THIS VERSION: SIFDECODE 1.2 - 24/10/2016 AT 13:30 GMT.

!-*-*-*-*-*-*-*-*-*-*-*- S I F D E C O D E   M O D U l E -*-*-*-*-*-*-*-*-*-*-

!  Copyright reserved, Bongartz/Conn/Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released as part of CUTE, December 1990
!   Became separate subroutines in SifDec, April 2004
!   Updated fortran 2003 version packaged and released December 2012

!  For full documentation, see
!   http://galahad.rl.ac.uk/galahad-www/specs.html

    MODULE SIFDECODE

      IMPLICIT NONE

      PRIVATE
      PUBLIC :: SIFDECODE_decode

!---------------
!  V e r s i o n
!---------------

      CHARACTER ( LEN = 6 ) :: version = '1.0   '

!--------------------
!   P r e c i s i o n
!--------------------

      INTEGER, PARAMETER :: sp = KIND( 1.0E+0 )
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

      INTEGER, PARAMETER :: nbytes = 8

!----------------------
!   P a r a m e t e r s
!----------------------

!  -------------------------------------------------------------------

!  default array lengths

!  the following parameters define the default sizes of problem
!  dependent arrays. The actual sizes used will be determined
!  automatically, and the arrays adjusted as the SIF file is
!  decoded, but good initial values may speed things up

!  [In SifDec an error messages was issued if any of these sizes
!   was too small, telling the user which parameter to increase.
!   This was, as our users never tired of telling us, frankly tedious]

!  -------------------------------------------------------------------

!  maximum number of variables

      INTEGER, PARAMETER :: n_guess_debug = 1
      INTEGER, PARAMETER :: n_guess_small = 200
      INTEGER, PARAMETER :: n_guess_medium = 20000
      INTEGER, PARAMETER :: n_guess_large = 2000000

!  maximum number of groups

      INTEGER, PARAMETER :: ng_guess_debug = 1
      INTEGER, PARAMETER :: ng_guess_small = 100
      INTEGER, PARAMETER :: ng_guess_medium = 10000
      INTEGER, PARAMETER :: ng_guess_large = 1000000

!  maximum number of different group types

      INTEGER, PARAMETER :: ngtype_guess_debug = 1
      INTEGER, PARAMETER :: ngtype_guess_small = 10
      INTEGER, PARAMETER :: ngtype_guess_medium = 10
      INTEGER, PARAMETER :: ngtype_guess_large = 10

!  maximum total number of real parameters associated with groups

      INTEGER, PARAMETER :: ngp_guess_debug = 1
      INTEGER, PARAMETER :: ngp_guess_small = 200
      INTEGER, PARAMETER :: ngp_guess_medium = 20000
      INTEGER, PARAMETER :: ngp_guess_large = 2000000

!  maximum number of nonlinear elements

      INTEGER, PARAMETER :: nel_guess_debug = 1
      INTEGER, PARAMETER :: nel_guess_small = 1000
      INTEGER, PARAMETER :: nel_guess_medium = 100000
      INTEGER, PARAMETER :: nel_guess_large = 1000000

!  maximum number of different nonlinear element types

      INTEGER, PARAMETER :: netype_guess_debug = 1
      INTEGER, PARAMETER :: netype_guess_small = 20
      INTEGER, PARAMETER :: netype_guess_medium = 20
      INTEGER, PARAMETER :: netype_guess_large = 20

!  maximum total number of elemental variables

      INTEGER, PARAMETER :: nelvar_guess_debug = 1
      INTEGER, PARAMETER :: nelvar_guess_small = 2000
      INTEGER, PARAMETER :: nelvar_guess_medium = 200000
      INTEGER, PARAMETER :: nelvar_guess_large = 2000000

!  maximum total number of auxliliary parameters used when defining function
!  and derivative values

      INTEGER, PARAMETER :: nauxpar_guess_debug = 1
      INTEGER, PARAMETER :: nauxpar_guess_small = 100
      INTEGER, PARAMETER :: nauxpar_guess_medium = 100
      INTEGER, PARAMETER :: nauxpar_guess_large = 100

!  maximum number of real parameters associated with nonlinear elements

      INTEGER, PARAMETER :: nelp_guess_debug = 1
      INTEGER, PARAMETER :: nelp_guess_small = 1000
      INTEGER, PARAMETER :: nelp_guess_medium = 100000
      INTEGER, PARAMETER :: nelp_guess_large = 1000000

!  maximum number of nonzeros in linear elements

      INTEGER, PARAMETER :: nnza_guess_debug = 1
      INTEGER, PARAMETER :: nnza_guess_small = 12000
      INTEGER, PARAMETER :: nnza_guess_medium = 1200000
      INTEGER, PARAMETER :: nnza_guess_large = 12000000

!  maximum number of integer parameters

      INTEGER, PARAMETER :: niindex_guess_debug = 1
      INTEGER, PARAMETER :: niindex_guess_small = 100
      INTEGER, PARAMETER :: niindex_guess_medium = 100
      INTEGER, PARAMETER :: niindex_guess_large = 100

!  maximum number of real parameters

      INTEGER, PARAMETER :: nrindex_guess_debug = 1
      INTEGER, PARAMETER :: nrindex_guess_small = 20000
      INTEGER, PARAMETER :: nrindex_guess_medium = 200000
      INTEGER, PARAMETER :: nrindex_guess_large = 2000000

!  maximum number of statements in any level of a do-loop

      INTEGER, PARAMETER :: maxins_guess_debug = 1
      INTEGER, PARAMETER :: maxins_guess_small = 200
      INTEGER, PARAMETER :: maxins_guess_medium = 200
      INTEGER, PARAMETER :: maxins_guess_large = 200

!  maximum number of array instructions

      INTEGER, PARAMETER :: maxarray_guess_debug = 1
      INTEGER, PARAMETER :: maxarray_guess_small = 150
      INTEGER, PARAMETER :: maxarray_guess_medium = 150
      INTEGER, PARAMETER :: maxarray_guess_large = 150

!  maximum number of vectors of bounds

      INTEGER, PARAMETER :: nbnd_guess_debug = 1
      INTEGER, PARAMETER :: nbnd_guess_small = 2
      INTEGER, PARAMETER :: nbnd_guess_medium = 2
      INTEGER, PARAMETER :: nbnd_guess_large = 2

!  maximum number of vectors of solutions

      INTEGER, PARAMETER :: nstart_guess_debug = 1
      INTEGER, PARAMETER :: nstart_guess_small = 3
      INTEGER, PARAMETER :: nstart_guess_medium = 3
      INTEGER, PARAMETER :: nstart_guess_large = 3

!  maximum number of vectors of bounds on the objective function

      INTEGER, PARAMETER :: nobjbound_guess_debug = 1
      INTEGER, PARAMETER :: nobjbound_guess_small = 2
      INTEGER, PARAMETER :: nobjbound_guess_medium = 2
      INTEGER, PARAMETER :: nobjbound_guess_large = 2

!  increase factor when enlarging arrays = increase_n / increase_d > 1

      INTEGER, PARAMETER :: increase_n = 3
      INTEGER, PARAMETER :: increase_d = 2

!  other parameters used by the decoder

      INTEGER, PARAMETER :: buffer = 75
      INTEGER, PARAMETER :: max_record_length = 160
      INTEGER, PARAMETER :: nfunct = 14
      INTEGER, PARAMETER :: nbytes_by_2 = nbytes / 2
      REAL ( KIND = wp ), PARAMETER :: zero = 0.0_wp
      REAL ( KIND = wp ), PARAMETER :: one = 1.0_wp
      REAL ( KIND = wp ), PARAMETER :: ten = 10.0_wp
      REAL ( KIND = wp ), PARAMETER :: biginf = ten ** 20
      REAL ( KIND = wp ), PARAMETER :: epsmch = EPSILON( one )
      LOGICAL, PARAMETER :: oneobj = .FALSE.
      LOGICAL, PARAMETER :: debug_extend = .FALSE.
      CHARACTER ( LEN = 10 ), PARAMETER :: cqsqr = '123456789S'
      CHARACTER ( LEN = 10 ), PARAMETER :: cqprod = '123456789P'
      CHARACTER ( LEN = 10 ), PARAMETER :: cqgrou = '123456789G'
      CHARACTER ( LEN = 1 ), DIMENSION( 10 ), PARAMETER :: CHARS =             &
                 (/ '0', '1', '2', '3', '4', '5','6', '7', '8', '9'/)
      CHARACTER ( LEN = 1 ), DIMENSION( 26 ), PARAMETER :: LCHARS =            &
                 (/ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',          &
                    'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',          &
                    'u', 'v', 'w', 'x', 'y', 'z' /)
      CHARACTER ( LEN = 1 ), DIMENSION( 26 ), PARAMETER :: UCHARS =            &
                 (/ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',          &
                    'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',          &
                    'U', 'V', 'W', 'X', 'Y', 'Z' /)
      CHARACTER ( LEN = 6 ), DIMENSION( nfunct ), PARAMETER :: FUNCTN =        &
         (/ 'ABS   ', 'SQRT  ', 'EXP   ', 'LOG   ', 'LOG10 ',                  &
            'SIN   ', 'COS   ', 'TAN   ', 'ARCSIN', 'ARCCOS',                  &
            'ARCTAN', 'HYPSIN', 'HYPCOS', 'HYPTAN' /)

!--------------------------------
!  G l o b a l  v a r i a b l e s
!--------------------------------

!  the number of unfilled entries in the current hash table

      INTEGER :: hash_empty

!  the largest prime that is no larger than the size of current hash table

      REAL ( KIND = wp ) :: hash_prime

!-------------------------------------
!   G e n e r i c  i n t e r f a c e s
!-------------------------------------

!  define generic interfaces to routines for extending allocatable arrays

      INTERFACE ALLOCATE_array
        MODULE PROCEDURE ALLOCATE_array_integer,                               &
                         ALLOCATE_array_real,                                  &
                         ALLOCATE_array_logical,                               &
                         ALLOCATE_array_character,                             &
                         ALLOCATE_array2_integer,                              &
                         ALLOCATE_array2_real,                                 &
                         ALLOCATE_array2_character,                            &
                         ALLOCATE_array3_integer
      END INTERFACE

      INTERFACE EXTEND_array
        MODULE PROCEDURE EXTEND_array_integer,                                 &
                         EXTEND_array_real,                                    &
                         EXTEND_array_character,                               &
                         EXTEND_array2_integer,                                &
                         EXTEND_array2_real,                                   &
                         EXTEND_array2_character,                              &
                         EXTEND_array3_integer
      END INTERFACE

    CONTAINS

!-*-*-*-*-*- S I F D E C O D E   D E C O D E    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE SIFDECODE_decode( iingps, outda, iinfn, outfn, outff, outfd,  &
                                   outra, iingr, outgr, outgf, outgd, iinex,   &
                                   outex, outem, outea, print_level, out,      &
                                   noname, ialgor, iauto, iad0, single, size,  &
                                   start, status )
      INTEGER :: iingps, iinfn, iingr, outea, outex, print_level, out
      INTEGER :: outda, outra, outfn, outgr, outff, outfd, outgf, outgd, outem
      INTEGER :: ialgor, iad0, iauto, size, start, status
      LOGICAL :: noname, single

!  ------------------------------------------------------------------------
!  decode a SIF file and convert the data into a form suitable for input to
!  GALAHAD or other external packages using CUTEst

!  formerly sdlanc in SiFDec
!  ------------------------------------------------------------------------

!  local variables

      INTEGER :: i, ig, isg, iinex, nevnames, nivnames, nepnames, ngpnames
      INTEGER :: nlinob, nnlnob, nlineq, nnlneq, nlinin, nnlnin, alloc_status
      INTEGER :: nfree, nfixed, nlower, nupper, nboth, nslack, nreal, narray
      INTEGER :: n, ng, nbnd, neltype, nlvars, nobj, nrange, nconst, nobjgr
      INTEGER :: nnza, ngtype, nstart, nlisgp, nlisep, nnlvrs, nobbnd, nrival
      INTEGER :: nelvar, nelnum, neling, len1_vstart, len1_cstart, len_defined
      INTEGER :: len1_blu, len_rinames, len_iinames, length, len_exnames
      INTEGER :: len_renames, len_innames, len_lonames, len_minames
      REAL ( KIND = wp ) :: blo, bup
      LOGICAL :: debug, gotlin
      CHARACTER ( LEN = 10 ) :: pname
      CHARACTER ( LEN = 10 ) :: nameof, namerh, namera, namebn, namest, nameob
      CHARACTER ( LEN = 24 ) :: bad_alloc
      CHARACTER ( LEN = 72 ) :: lineex
      CHARACTER ( LEN = max_record_length ) :: nuline

!  array definitions

      CHARACTER ( LEN = 1 ), DIMENSION( 2 ), PARAMETER :: S = (/ ' ', 's' /)
      CHARACTER ( LEN = 3 ), DIMENSION( 2 ), PARAMETER :: ARE                  &
        = (/ 'is ', 'are' /)

!  allocatable array definitions

      INTEGER, ALLOCATABLE, DIMENSION( : ) :: GTYPESP_ptr, ELVAR, IIVAL, IWK
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: GSTATE, GTYPE, GP_ptr
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TYPEV, TYPEE, EP_ptr, EV_ptr
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: ELV, INV, ELP
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: ELING_el, ELING_g, ELING_ptr
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: A_row, A_col
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: ABYROW_col, ABYROW_ptr
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TABLE, INLIST
      INTEGER, ALLOCATABLE, DIMENSION( :, : ) :: IDROWS
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: B, BL, BU, X, VSCALE
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: A_val, ABYROW_val
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: EP_val, DEFAULT, RIVAL
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: GP_val, GP_val_orig
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: RSCALE, GSCALE
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: FBOUND_l, FBOUND_u
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: WEIGHT, ESCALE, CSCALE
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: B_l_default
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: B_u_default
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( :, : ) :: B_l, B_u
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( :, : ) :: VSTART, CSTART
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( :, : ) :: RDROWS
      LOGICAL, ALLOCATABLE, DIMENSION( : ) :: DEFINED
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: VNAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: GANAMES, GNAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: ETYPES, GTYPES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: ONAMES, SNAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: BNAMES, LNAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: EVNAMES, IVNAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: EPNAMES, GPNAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: RENAMES, INNAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: LONAMES, OBBNAME
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: MINAMES, EXNAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: IINAMES, RINAMES
      CHARACTER ( LEN = 12 ), ALLOCATABLE, DIMENSION( : ) :: KEY

!  set starting array sizes

!  debug problems

      IF ( size == 0 ) THEN
        len_renames = nauxpar_guess_debug
        len_innames = nauxpar_guess_debug
        len_lonames = nauxpar_guess_debug
        len_minames = nauxpar_guess_debug
        len_exnames = nauxpar_guess_debug

!  small problems

      ELSE IF ( size == 1 ) THEN
        len_renames = nauxpar_guess_small
        len_innames = nauxpar_guess_small
        len_lonames = nauxpar_guess_small
        len_minames = nauxpar_guess_small
        len_exnames = nauxpar_guess_small

!  large problems

      ELSE IF ( size == 3 ) THEN
        len_renames = nauxpar_guess_large
        len_innames = nauxpar_guess_large
        len_lonames = nauxpar_guess_large
        len_minames = nauxpar_guess_large
        len_exnames = nauxpar_guess_large

!  medium problems (default)

      ELSE ! IF ( size == 2 ) THEN
        len_renames = nauxpar_guess_medium
        len_innames = nauxpar_guess_medium
        len_lonames = nauxpar_guess_medium
        len_minames = nauxpar_guess_medium
        len_exnames = nauxpar_guess_medium
      END IF

      debug = print_level < 0
      IF ( single ) THEN
        WRITE( out, 2050 )
      ELSE
        WRITE( out, 2060 )
      END IF
      IF ( print_level /= 0 ) print_level = 9

!  read the GPS MPS data

      CALL INTERPRET_gpsmps(                                                   &
                   n, ng, nnza, nobj, nconst, nrange, nbnd, nstart, neltype,   &
                   ngtype, nelvar, nlvars, nnlvrs, nlisgp, nlisep, nelnum,     &
                   neling, narray, nrival, nobbnd, nevnames, nivnames,         &
                   nepnames, ngpnames, pname,                                  &
                   ELING_el, ELING_g, length, TABLE, KEY, INLIST,              &
                   GSTATE, ELV, INV, TYPEE, IDROWS, ELVAR,                     &
                   ELING_ptr, GTYPE, ELP, GTYPESP_ptr, IWK, EP_ptr, EV_ptr,    &
                   GP_ptr, IIVAL, TYPEV,                                       &
                   A_row, A_col, A_val, len1_blu, B_l, B_u,                    &
                   len1_vstart, VSTART, len1_cstart, CSTART,                   &
                   RSCALE, CSCALE, RDROWS, RIVAL, DEFAULT,                     &
                   EP_val, B_l_default, B_u_default,                           &
                   GP_val, GP_val_orig, FBOUND_l, FBOUND_u, WEIGHT,            &
                   len_iinames, IINAMES, len_rinames, RINAMES,                 &
                   GNAMES, VNAMES, BNAMES, ETYPES, IVNAMES,                    &
                   LNAMES, ONAMES, EVNAMES, SNAMES, GANAMES, GTYPES, EPNAMES,  &
                   GPNAMES, OBBNAME, single, size, iingps, out, status, debug )
      IF ( status /= 0 ) THEN
        WRITE( out, 2010 ) status
        RETURN
      END IF

!  deallocate workspace

      DEALLOCATE( RIVAL, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'RIVAL' ; GO TO 990 ; END IF

      DEALLOCATE( IIVAL, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'IIVAL' ; GO TO 990 ; END IF

      DEALLOCATE( ELING_g, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'ELING_g' ; GO TO 990 ; END IF

      DEALLOCATE( GP_val_orig, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'GP_val_orig' ; GO TO 990 ; END IF

!  assign the groups to constraint types and objectives

      nlinob = 0 ; nnlnob = 0 ; nlineq = 0
      nnlneq = 0 ; nlinin = 0 ; nnlnin = 0
      DO ig = 1, ng
        isg = GSTATE( ig )
        IF ( isg > 0 ) THEN
          isg = MOD( isg - 1, 4 )
          IF ( isg == 0 ) nlinob = nlinob + 1
          IF ( isg == 1 ) nlineq = nlineq + 1
          IF ( isg >= 2 ) nlinin = nlinin + 1
        ELSE
          isg = MOD( isg + 1, 4 )
          IF ( isg == 0 ) nnlnob = nnlnob + 1
          IF ( isg == - 1 ) nnlneq = nnlneq + 1
          IF ( isg <= - 2 ) nnlnin = nnlnin + 1
        END IF
      END DO

!  select rhs, ranges and bounds

      IF ( nconst > 0 ) namerh = VNAMES( nlvars + 1 )
      IF ( nrange > 0 ) namera = VNAMES( nlvars + nconst + 1 )
      IF ( nbnd > 0 ) namebn = BNAMES( 1 )
      IF ( nstart > 0 ) THEN
        IF ( start > 0 .AND. start <= nstart ) THEN
          namest = SNAMES( start )
        ELSE
          namest = SNAMES( 1 )
        END IF
      END IF
      IF ( nobj > 0 .AND. oneobj ) nameof = ONAMES( 1 )
      IF ( nobbnd > 0 ) nameob = OBBNAME( 1 )
      IF ( print_level /= 0 ) WRITE( out, 2070 )                               &
                                nconst, nrange, nbnd, nstart, nobj, nobbnd

!  convert to input for GALAHAD or other external packages

      CALL MAKE_outsdif( n, nlvars, ng, nelnum, neling, nobj, nelvar, nlisgp,  &
                         nlisep, nbnd, nnza, nconst, nstart, nrange, nobjgr,   &
                         nobbnd, neltype, ngtype, pname, nameob, namerh,       &
                         namera, namebn, namest, nameof,                       &
                         ELING_ptr, ELVAR, EV_ptr, ABYROW_col, ABYROW_ptr,     &
                         A_row, A_col, length, TABLE, KEY, INLIST,             &
                         GSTATE, IDROWS, ELV, INV, GTYPESP_ptr, ELING_el,      &
                         EP_ptr, GP_ptr, TYPEE, GTYPE, TYPEV, IWK, A_val,      &
                         len1_blu, B_l, B_u, len1_vstart, VSTART,              &
                         len1_cstart, CSTART, RSCALE, CSCALE, RDROWS,          &
                         DEFAULT, WEIGHT, B_l_default, B_u_default,            &
                         GP_val, EP_val, FBOUND_l, FBOUND_u, ABYROW_val,       &
                         B, BL, BU, X, ESCALE, GSCALE, VSCALE,                 &
                         GNAMES, VNAMES, BNAMES, SNAMES, ONAMES, ETYPES,       &
                         GTYPES, OBBNAME, ialgor, iauto, out, outda,           &
                         single, status, debug )
      IF ( status /= 0 ) THEN
         WRITE( out, 2020 ) status
         RETURN
      END IF

!  deallocate workspace

      DEALLOCATE( A_row, A_col, A_val, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'A' ; GO TO 990 ; END IF

      DEALLOCATE( B_l, B_u, B_l_default, B_u_default, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'B_l/u' ; GO TO 990 ; END IF

      DEALLOCATE( IDROWS, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'IDROWS' ; GO TO 990 ; END IF

      DEALLOCATE( RDROWS, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'RDROWS' ; GO TO 990 ; END IF

      DEALLOCATE( FBOUND_l, FBOUND_u, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'FBOUND' ; GO TO 990 ; END IF

      DEALLOCATE( DEFAULT, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'DEFAULT' ; GO TO 990 ; END IF

      DEALLOCATE( RSCALE, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'RSCALE' ; GO TO 990 ; END IF

      DEALLOCATE( CSCALE, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'CSCALE' ; GO TO 990 ; END IF

      DEALLOCATE( WEIGHT, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'WEIGHT' ; GO TO 990 ; END IF

      DEALLOCATE( VSTART, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'VSTART' ; GO TO 990 ; END IF

      DEALLOCATE( CSTART, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'CSTART' ; GO TO 990 ; END IF

      DEALLOCATE( BNAMES, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'BNAMES' ; GO TO 990 ; END IF

      DEALLOCATE( ONAMES, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'ONAMES' ; GO TO 990 ; END IF

      DEALLOCATE( SNAMES, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'SNAMES' ; GO TO 990 ; END IF

      DEALLOCATE( OBBNAME, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'OBBNAME' ; GO TO 990 ; END IF

!  assign the variables to bound types

      nfree = 0 ; nfixed = 0 ; nlower = 0 ; nupper = 0 ; nboth = 0
      IF ( ialgor <= 2 ) THEN
        nslack = nlinin + nnlnin
      ELSE
        nslack = 0
      END IF
      nreal = n - nslack
      DO i = 1, nreal
        blo = BL( i ) ; bup = BU( i )
        IF ( blo <= - biginf .AND. bup >= biginf ) nfree = nfree  + 1
        IF ( blo <= - biginf .AND. bup < biginf ) nupper = nupper + 1
        IF ( blo > - biginf .AND. bup >= biginf ) nlower = nlower + 1
        IF ( blo > - biginf .AND. bup < biginf ) THEN
          IF ( blo == bup ) THEN
            nfixed = nfixed + 1
          ELSE
            nboth = nboth  + 1
          END IF
        END IF
      END DO

!  print problem summary

      IF ( nlinob > 0 ) WRITE( out, 2100 ) nlinob, TRIM( S( ONLY1( nlinob ) ) )
      IF ( nnlnob > 0 ) WRITE( out, 2110 ) nnlnob, TRIM( S( ONLY1( nnlnob ) ) )
      IF ( nlineq + nlinin + nnlneq + nnlnin > 0 ) WRITE( out, 2000)
      IF ( nlineq > 0 ) WRITE( out, 2120 ) TRIM( ARE( ONLY1( nlineq ) )) ,     &
                nlineq, TRIM( S( ONLY1( nlineq ) ) )
      IF ( nlinin > 0 ) WRITE( out, 2130 ) TRIM( ARE( ONLY1( nlinin ) ) ),     &
                nlinin, TRIM( S( ONLY1( nlinin ) ) )
      IF ( nnlneq > 0 ) WRITE( out, 2140 ) TRIM( ARE( ONLY1( nnlneq ) ) ),     &
                nnlneq, TRIM( S( ONLY1( nnlneq ) ) )
      IF ( nnlnin > 0 ) WRITE( out, 2150 ) TRIM( ARE( ONLY1( nnlnin ) ) ),     &
                nnlnin, TRIM( S( ONLY1( nnlnin ) ) )
      WRITE( out, 2000 )
      IF ( nfree  > 0 ) WRITE( out, 2200 ) TRIM( ARE( ONLY1( nfree  ) ) ),     &
                nfree, TRIM( S( ONLY1( nfree  ) ) )
      IF ( nupper > 0 ) WRITE( out, 2210 ) TRIM( ARE( ONLY1( nupper ) ) ),     &
                nupper, TRIM( S( ONLY1( nupper ) ) )
      IF ( nlower > 0 ) WRITE( out, 2220 ) TRIM( ARE( ONLY1( nlower ) ) ),     &
                nlower, TRIM( S( ONLY1( nlower ) ) )
      IF ( nboth  > 0 ) WRITE( out, 2230 ) TRIM( ARE( ONLY1( nboth  ) ) ),     &
                nboth,  TRIM( S( ONLY1( nboth  ) ) )
      IF ( nfixed > 0 ) WRITE( out, 2240 ) TRIM( ARE( ONLY1( nfixed ) ) ),     &
                nfixed, TRIM( S( ONLY1( nfixed ) ) )
      IF ( nslack > 0 ) WRITE( out, 2250 ) TRIM( ARE( ONLY1( nslack ) ) ),     &
                nslack, TRIM( S( ONLY1( nslack ) ) )
      WRITE( outda, 2080 ) pname, nfree, nfixed, nlower, nupper, nboth,        &
                nslack, nlinob, nnlnob, nlineq, nnlneq, nlinin, nnlnin

!  print details of the problem

      CALL PRINT_details( n, ng, nelvar, neltype, nevnames, nepnames,          &
                          ngpnames, ngtype, nlvars, nelnum, neling, nlisgp,    &
                          nlisep, nnza, GSTATE, ELING_ptr,                     &
                          ELVAR, GTYPE, TYPEE, ELV, INV, ELP, GTYPESP_ptr,     &
                          GP_ptr, EP_ptr, EV_ptr, ELING_el, TYPEV, IWK,        &
                          ABYROW_ptr, ABYROW_col, ABYROW_val, B,               &
                          BL, BU, X, EP_val, GP_val, GSCALE, ESCALE,           &
                          VSCALE, pname, VNAMES, GNAMES, LNAMES, ETYPES,       &
                          EVNAMES, GANAMES, EPNAMES, GPNAMES, GTYPES,          &
                          out, print_level )
      IF ( noname ) pname = '          '

!  deallocate workspace

      DEALLOCATE( ABYROW_col, ABYROW_val, ABYROW_ptr, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'ABYROW' ; GO TO 990 ; END IF

      DEALLOCATE( BL, BU, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'BL/BU' ; GO TO 990 ; END IF

      DEALLOCATE( B, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'B' ; GO TO 990 ; END IF

      DEALLOCATE( X, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'X' ; GO TO 990 ; END IF

      DEALLOCATE( VSCALE, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'VSCALE' ; GO TO 990 ; END IF

      DEALLOCATE( ELVAR, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'ELVAR' ; GO TO 990 ; END IF

      DEALLOCATE( IWK, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'IWK' ; GO TO 990 ; END IF

      DEALLOCATE( GSTATE, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'GSTATE' ; GO TO 990 ; END IF

      DEALLOCATE( GTYPE, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'GTYPE' ; GO TO 990 ; END IF

      DEALLOCATE( GP_ptr, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'GP_ptr' ; GO TO 990 ; END IF

      DEALLOCATE( TYPEV, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'TYPEV' ; GO TO 990 ; END IF

      DEALLOCATE( TYPEE, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'TYPEE' ; GO TO 990 ; END IF

      DEALLOCATE( EP_ptr, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'EP_ptr' ; GO TO 990 ; END IF

      DEALLOCATE( EV_ptr, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'EV_ptr' ; GO TO 990 ; END IF

      DEALLOCATE( ELING_el, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'ELING_el' ; GO TO 990 ; END IF

      DEALLOCATE( ELING_ptr, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'ELING_ptr' ; GO TO 990 ; END IF

      DEALLOCATE( EP_val, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'EP_val' ; GO TO 990 ; END IF

      DEALLOCATE( GP_val, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'GP_val' ; GO TO 990 ; END IF

      DEALLOCATE( GSCALE, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'GSCALE' ; GO TO 990 ; END IF

      DEALLOCATE( ESCALE, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'ESCALE' ; GO TO 990 ; END IF

      DEALLOCATE( VNAMES, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'VNAMES' ; GO TO 990 ; END IF

      DEALLOCATE( GNAMES, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'GNAMES' ; GO TO 990 ; END IF

      DEALLOCATE( LNAMES, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'LNAMES' ; GO TO 990 ; END IF

!  allocate workspace

      len_defined = MAX( neltype, ngtype )
      CALL ALLOCATE_array( DEFINED, len_defined, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'DEFINED' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( RENAMES, len_renames, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'RENAMES' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( INNAMES, len_innames, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'INNAMES' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( LONAMES, len_lonames, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'LONAMES' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( MINAMES, len_minames, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'MINAMES' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( EXNAMES, len_exnames, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'EXNAMES' ; GO TO 980 ; END IF

!  make subroutines elfun and range

      IF ( iauto == 0 ) THEN
        CALL MAKE_elfun( iinfn, out, outfn, outra, status,                     &
                         nevnames, nivnames, nepnames, neltype,                &
                         pname, EVNAMES, IVNAMES,                              &
                         len_renames, RENAMES, len_innames, INNAMES,           &
                         len_lonames, LONAMES, len_minames, MINAMES,           &
                         len_exnames, EXNAMES,                                 &
                         DEFINED, length, TABLE, KEY, INLIST,                  &
                         ETYPES, ELV, INV, EPNAMES, ELP, debug,                &
                         single, nuline, gotlin, print_level )
        IF ( status /= 0 ) THEN
          WRITE( out, 2030 ) status
          RETURN
        END IF

!  make subroutines elfunf, elfund and range

      ELSE
        CALL MAKE_elfun_ad( iinfn, out, outff, outfd, outra, outem, status,    &
                            nevnames, nivnames, nepnames, neltype,             &
                            pname, EVNAMES, IVNAMES,                           &
                            len_renames, RENAMES, len_innames, INNAMES,        &
                            len_lonames, LONAMES, len_minames, MINAMES,        &
                            len_exnames, EXNAMES,                              &
                            DEFINED, length, TABLE, KEY, INLIST,               &
                            ETYPES, ELV, INV, EPNAMES, ELP, debug,             &
                            single, nuline, gotlin, iauto,                     &
                            iad0, print_level )
        IF ( status /= 0 ) THEN
          WRITE( out, 2090 ) status
          RETURN
        END IF
      END IF

!  deallocate workspace

      DEALLOCATE( ELV, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'ELV' ; GO TO 990 ; END IF

      DEALLOCATE( INV, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'ELV' ; GO TO 990 ; END IF

      DEALLOCATE( ELP, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'ELP' ; GO TO 990 ; END IF

      DEALLOCATE( ETYPES, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'ETYPES' ; GO TO 990 ; END IF

      DEALLOCATE( EPNAMES, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'EPNAMES' ; GO TO 990 ; END IF

      DEALLOCATE( EVNAMES, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'EVNAMES' ; GO TO 990 ; END IF

      DEALLOCATE( IVNAMES, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'IVNAMES' ; GO TO 990 ; END IF

!  make subroutine group and obtain group information

      IF ( iauto == 0 ) THEN
        CALL MAKE_group( iingr, out, outgr, status, ngtype, ngpnames,          &
                         pname, GANAMES,                                       &
                         len_renames, RENAMES, len_innames, INNAMES,           &
                         len_lonames, LONAMES, len_minames, MINAMES,           &
                         len_exnames, EXNAMES,                                 &
                         GPNAMES, DEFINED, GTYPES, GTYPESP_ptr,                &
                         debug, length, TABLE, KEY, INLIST,                    &
                         single, nuline, gotlin, print_level )
        IF ( status /= 0 ) THEN
          WRITE( out, 2040 ) status
          RETURN
        END IF

!  make subroutines groupf and groupd

      ELSE
        CALL MAKE_group_ad( iingr, out, outgf, outgd, outem, status,           &
                            ngtype, ngpnames, pname, GANAMES,                  &
                            len_renames, RENAMES, len_innames, INNAMES,        &
                            len_lonames, LONAMES, len_minames, MINAMES,        &
                            len_exnames, EXNAMES,                              &
                            GPNAMES, DEFINED, GTYPES, GTYPESP_ptr,             &
                            debug, length, TABLE, KEY, INLIST,                 &
                            single, nuline, gotlin, iauto, iad0, print_level )
        IF ( status /= 0 ) THEN
           WRITE( out, 2160 ) status
           RETURN
        END IF
      END IF

!  deallocate workspace

      DEALLOCATE( GTYPESP_ptr, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'GTYPESP_ptr' ; GO TO 990 ; END IF

      DEALLOCATE( TABLE, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'TABLE' ; GO TO 990 ; END IF

      DEALLOCATE( INLIST, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'INLIST' ; GO TO 990 ; END IF

      DEALLOCATE( KEY, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'KEY' ; GO TO 990 ; END IF

      DEALLOCATE( DEFINED, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'DEFINED' ; GO TO 990 ; END IF

      DEALLOCATE( GTYPES, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'GTYPES' ; GO TO 990 ; END IF

      DEALLOCATE( GANAMES, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'GANAMES' ; GO TO 990 ; END IF

      DEALLOCATE( GPNAMES, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'GPNAMES' ; GO TO 990 ; END IF

      DEALLOCATE( RENAMES, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'RENAMES' ; GO TO 990 ; END IF

      DEALLOCATE( INNAMES, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'INNAMES' ; GO TO 990 ; END IF

      DEALLOCATE( LONAMES, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'LONAMES' ; GO TO 990 ; END IF

      DEALLOCATE( MINAMES, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'MINAMES' ; GO TO 990 ; END IF

      DEALLOCATE( EXNAMES, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'EXNAMES' ; GO TO 990 ; END IF

!  finally, read any additional programs

      DO
        IF ( gotlin ) THEN
          lineex( 1 : 72 ) = nuline( 1 : 72 )
          gotlin = .FALSE.
        ELSE
          READ( UNIT = iinex, FMT = 1000, END = 600, ERR = 600 ) lineex
        END IF

!  skip blank lines

        DO i = 1, 72
          IF ( lineex( i : i ) /= ' ' ) THEN
            WRITE( outex, 1000 ) lineex
            EXIT
          END IF
        END DO
      END DO

!  if required, translate any external file to accept automatic differentiation
!   constructs

  600 CONTINUE
      IF ( iauto == 1 .OR. iauto == 2 ) THEN
        CALL TRANSLATE_for_ad( out, status, outex, outea, outem, single,       &
                               iauto, iad0, len_rinames, RINAMES,              &
                               len_iinames, IINAMES )
        IF ( status /= 0 ) WRITE( out, 2170 ) status
      END IF

!  deallocate workspace

      DEALLOCATE( RINAMES, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'RINAMES' ; GO TO 990 ; END IF

      DEALLOCATE( IINAMES, STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'IINAMES' ; GO TO 990 ; END IF

      RETURN

!  allocation errors

  980 CONTINUE
      WRITE( out, "( ' ** Message from SIFDECODE_decode-',                     &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  deallocation errors

  990 CONTINUE
      WRITE( out, "( ' ** Message from SIFDECODE_decode-',                     &
     &    /, ' Deallocation error (status = ', I0, ') for ', A )" )            &
        alloc_status, bad_alloc
      RETURN

!  non-executable statements

 1000 FORMAT( A72 )
 2000 FORMAT( ' ' )
 2010 FORMAT( /, ' Return from INTERPRET_gpsmps, status = ', I0 )
 2020 FORMAT( /, ' Return from MAKE_outsdif, status = ', I0 )
 2030 FORMAT( /, ' Return from MAKE_elfun, status = ', I0 )
 2040 FORMAT( /, ' Return from MAKE_group, status = ', I0 )
 2050 FORMAT( /, ' Single precision version will be formed', / )
 2060 FORMAT( /, ' Double precision version will be formed', / )
 2070 FORMAT( /, '  nconst  nrange    nbnd  nstart    nobj  nobbnd ', /, 6I8, /)
 2080 FORMAT( A10, 12I8 )
 2090 FORMAT( /, ' Return from MAKE_elfun_ad, status = ', I0 )
 2100 FORMAT( ' The objective function uses ', I0, ' linear group', A )
 2110 FORMAT( ' The objective function uses ', I0, ' nonlinear group', A )
 2120 FORMAT( ' There ', A, 1X, I0, ' linear equality constraint', A )
 2130 FORMAT( ' There ', A, 1X, I0, ' linear inequality constraint', A )
 2140 FORMAT( ' There ', A, 1X, I0, ' nonlinear equality constraint', A )
 2150 FORMAT( ' There ', A, 1X, I0, ' nonlinear inequality constraint', A )
 2160 FORMAT( /, ' Return from MAKE_group_ad, status = ', I0 )
 2170 FORMAT( /, ' Return from TRANSLATE_for_ad, status = ', I0 )
 2200 FORMAT( ' There ', A, 1X, I0, ' free variable', A1 )
 2210 FORMAT( ' There ', A, 1X, I0, ' variable', A,                            &
                ' bounded only from above ' )
 2220 FORMAT( ' There ', A, 1X, I0, ' variable', A,                            &
                ' bounded only from below ' )
 2230 FORMAT( ' There ', A, 1X, I0,                                            &
              ' variable', A, ' bounded from below and above ' )
 2240 FORMAT( ' There ', A, 1X, I0, ' fixed variable', A )
 2250 FORMAT( ' There ', A, 1X, I0, ' slack variable', A )

!  end of subroutine SIFDECODE_decode

      END SUBROUTINE SIFDECODE_decode

!-  S I F D E C O D E   I N T E R P R E T _ G P S M P S   S U B R O U T I N E  -

      SUBROUTINE INTERPRET_gpsmps(                                             &
                         n, ng, nnza, nobj, nconst, nrange, nbnd, nstart,      &
                         neltype, ngtype, nelvar, nlvars, nnlvrs, nlisgp,      &
                         nlisep, nelnum, neling, narray, nrival, nobbnd,       &
                         nevnames, nivnames, nepnames, ngpnames, pname,        &
                         ELING_el, ELING_g, length, TABLE, KEY, INLIST,        &
                         GSTATE, ELV, INV, TYPEE, IDROWS, ELVAR, ELING_ptr,    &
                         GTYPE, ELP, GTYPESP_ptr, IWK, EP_ptr, EV_ptr,         &
                         GP_ptr, IIVAL, TYPEV, A_row, A_col, A_val,            &
                         len1_blu, B_l, B_u, len1_vstart, VSTART,              &
                         len1_cstart, CSTART, RSCALE, CSCALE, RDROWS,          &
                         RIVAL, DEFAULT, EP_val, B_l_default, B_u_default,     &
                         GP_val, GP_val_orig, FBOUND_l, FBOUND_u, WEIGHT,      &
                         len_iinames, IINAMES, len_rinames, RINAMES,           &
                         GNAMES, VNAMES, BNAMES, ETYPES, IVNAMES, LNAMES,      &
                         ONAMES, EVNAMES, SNAMES, GANAMES, GTYPES, EPNAMES,    &
                         GPNAMES, OBBNAME, single, size, input, out,           &
                         status, debug )
      INTEGER :: nobbnd, nrival, nelvar, nnza, length, n, ng
      INTEGER :: nconst, nrange, nbnd, nstart, neltype, ngtype
      INTEGER :: nlvars, nnlvrs, nelnum, neling, narray, nobj
      INTEGER :: nlisgp, nlisep, size, input, out, status
      INTEGER :: len1_blu, len1_vstart, len1_cstart, len_iinames, len_rinames
      INTEGER :: nevnames, nivnames, nepnames, ngpnames
      LOGICAL :: single, debug
      CHARACTER ( LEN = 10 ) :: pname
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: ELV, INV, ELP
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: EV_ptr, TYPEE, EP_ptr, TYPEV
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: GTYPESP_ptr, ELVAR, IIVAL, IWK
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: ELING_ptr, GSTATE, GTYPE, GP_ptr
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: ELING_el, ELING_g
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: A_row, A_col
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: INLIST, TABLE
      INTEGER, ALLOCATABLE, DIMENSION( :, : ) :: IDROWS
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: A_val, WEIGHT
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: CSCALE, RSCALE
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: EP_val, DEFAULT, RIVAL
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: GP_val_orig, GP_val
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: B_l_default
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: B_u_default
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: FBOUND_l, FBOUND_u
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( :, : ) :: B_l, B_u
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( :, : ) :: VSTART, CSTART
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( :, : ) :: RDROWS
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: OBBNAME, VNAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: GANAMES, GNAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: GTYPES, ETYPES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: ONAMES, SNAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: BNAMES, LNAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: EVNAMES, IVNAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: EPNAMES, GPNAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: IINAMES, RINAMES
      CHARACTER ( LEN = 12 ), ALLOCATABLE, DIMENSION( : ) :: KEY

!  ----------------------------------------------------------------------------
!  read a GPS MPS data file

!  formerly GPSMPS in SiFDec

!  MPS indicator cards
!  --------------------

!  definition   purpose
!  ----------   --------
!  NAME         problem name
!  ROWS         names of rows (alias group names)
!  COLUMNS      names of columns (alias variable names)
!  RHS          right-hand-sides (alias constant terms in groups)
!  RHS'         alias for rhs
!  RANGES       additional bounds on rows
!  BOUNDS       bounds on columns
!  ENDATA       end of input data

!  additional indicator cards
!  ---------------------------

!  definition   purpose
!  ----------   --------
!  GROUPS       alias for rows
!  CONSTRAINTS  alias for rows
!  VARIABLES    alias for columns
!  CONSTANTS    alias for rhs
!  START_POINT  estimate of minimizer
!  HESSIAN      quadratic terms
!  QUADRATIC    alias for Hessian
!  QUADS        alias for Hessian
!  QUADOBJ      alias for Hessian
!  QSECTION     alias for Hessian
!  QMATRIX      alias for Hessian
!  ELEMENT_TYPE types of nonlinear elements
!  ELEMENT_USES definitions of nonlinear elements
!  GROUP_TYPE   types of nontrivial groups
!  GROUP_USES   definitions of groups

!  data card description
!  ----------------------

!  see 'The SIF reference report', Chapter 7 in
!       A. R. Conn, N. I. M. Gould and Ph. L. Toint,
!       LANCELOT A Fortran Package for Large-Scale Nonlinear Optimization
!       (RElease A), Springer Series in Computational Mathematics 17,
!       Springer Verlag 1992

!  see also http://www.cuter.rl.ac.uk/sifdec/Doc/sif.pdf
!  and      http://www.numerical.rl.ac.uk/lancelot/sif/sifhtml.html

!  -----------------------------------------------------------------------------
!  returns with negative values of status indicate that insufficient
!  array space has been allowed, as follows:

!    status = - 1  when length not large enough
!    status = - 2  when A_row, A_col or A_val cannot be extended further
!    status = - 3  when ELV, INV, ELP or ETYPES cannot be extended further
!    status = - 4  when GTYPESP_ptr, GANAMES or GTYPES cannot be
!                  extended further
!    status = - 5  when ONAMES cannot be extended further
!    status = - 6  when GSTATE, GTYPE, GNAMES, RSCALE, IDROWS or RDROWS
!                  cannot be extended further
!    status = - 7  when TYPEV, CSCALE, DEFAULT or VNAMES
!                  cannot be extended further
!    status = - 8  when VSTART, CSTART or SNAMES cannot be extended further
!    status = - 9  when ELING_el, ELING_p, WEIGHT, EV_ptr, EP_ptr, TYPEE, or
!                  LNAMES cannot be extended further
!    status = - 10 no longer used
!    status = - 11 when INSTR1, INSTR2, INSTR3, RVALUE1, RVALUE2 or RVALUE3
!                  cannot be extended further
!    status = - 12 when ARRAY, CARRAY, FARRAY IARRAY or VARRAY cannot be
!                  extended further
!    status = - 13 when B_l, B_u, B_l_default, B_u_default or BNAMES cannot
!                  be extended further
!    status = - 14 when EVNAMES cannot be extended further
!    status = - 15 when ELVAR cannot be extended further
!    status = - 16 when IVNAMES cannot be extended further
!    status = - 17 when EP_val cannot be extended further
!    status = - 18 when GP_val or GP_val_orig cannot be extended further
!    status = - 19 when EPNAMES cannot be extended further
!    status = - 20 when GPNAMES cannot be extended further
!    status = - 21 when IIVAL or IINAMES cannot be extended further
!    status = - 22 when RIVAL or RINAMES cannot be extended further
!    status = - 23 when FBOUND_l, FBOUND_u or OBBNAME cannot be extended further
!  -----------------------------------------------------------------------------

!  parameter definitions

      INTEGER, PARAMETER :: mblank = 1, mfixed = 2, mfree = 3, mname = 4
      INTEGER, PARAMETER :: mrows = 5, mgroup = 6, mcnstr = 7, mcols = 8
      INTEGER, PARAMETER :: mvars = 9, mconst = 10, mrhs = 11, mrhsp = 12
      INTEGER, PARAMETER :: mrange = 13, mbound = 14, mstart = 15, mqhess = 16
      INTEGER, PARAMETER :: mquadr = 17, mquads = 18, mquado = 19, mqsect = 20
      INTEGER, PARAMETER :: mqmatr = 21, metype = 22, meuses = 23, mgtype = 24
      INTEGER, PARAMETER :: mguses = 25, mobbnd = 26, mendat = 27
      INTEGER, PARAMETER :: maxnul = 20
      INTEGER, PARAMETER :: maxlev = 3
      INTEGER, DIMENSION( mendat ), PARAMETER :: LENIND                        &
        = (/ 0, 12, 11, 4, 4, 6, 11, 7, 9, 9, 3, 4, 6, 6, 11, 7, 9, 5, 7, 8,   &
             7, 12, 12, 10, 10, 12, 6  /)
      CHARACTER ( LEN = 12 ), DIMENSION( mendat ), PARAMETER :: INDIC8         &
        = (/ '            ', 'FIXED FORMAT', 'FREE FORMAT ', 'NAME        ',   &
             'ROWS        ', 'GROUPS      ', 'CONSTRAINTS ', 'COLUMNS     ',   &
             'VARIABLES   ', 'CONSTANTS   ', 'RHS         ', 'RHS''        ',  &
             'RANGES      ', 'BOUNDS      ', 'START POINT ', 'HESSIAN     ',   &
             'QUADRATIC   ', 'QUADS       ', 'QUADOBJ     ', 'QSECTION    ',   &
             'QMATRIX     ', 'ELEMENT TYPE', 'ELEMENT USES', 'GROUP TYPE  ',   &
             'GROUP USES  ', 'OBJECT BOUND', 'ENDATA      ' /)

!  local variables

      INTEGER :: i, ip, is, intype, intypo, j, k, k1, k2, l, l1, l2, l3, level2
      INTEGER :: ifree, ifield, novals, nvar, ncol
      INTEGER :: nlines, ilines, ngrupe, nelmnt, niival
      INTEGER :: iptype, istype, ndtype, level, ijump, lineno
      INTEGER :: level3, lev1, lev2, lev3, lev1s, lev2s, lev3s
      INTEGER :: lev1e, lev2e, lev3e, lev1i, lev2i, lev3i, levl3a
      INTEGER :: ninstr1, ninstr2, ninstr3
      INTEGER :: used_length, new_length, min_length, alloc_status
      INTEGER :: used_length2, new_length2, min_length2
      INTEGER :: used_length3, new_length3, min_length3
      INTEGER :: len_elvar, len_gtypesp_ptr, len_ganames, len_gtypes, len_gstate
      INTEGER :: len_gtype, len_gnames, len_rscale, len2_idrows, len2_rdrows
      INTEGER :: len2_cstart, len2_vstart, len_snames, len_onames
      INTEGER :: len_eling_el, len_eling_g, len_weight, len_typee, len_ev_ptr
      INTEGER :: len_ep_ptr, len_lnames, len_elv, len_inv, len_elp, len_etypes
      INTEGER :: len_evnames, len_ivnames, len_epnames, len_gpnames, len_ep_val
      INTEGER :: len_a, len2_blu, len_blu_default, len_bnames, len_obbname
      INTEGER :: len_cscale, len_typev, len_default, len_vnames, len_iival
      INTEGER :: len2_instr1, len2_instr2, len2_instr3, len_rvalue1, len_rvalue2
      INTEGER :: len_rvalue3, len_farray, len2_array, len2_carray, len2_varray
      INTEGER :: len_gp_val_orig, len_fbound, len_rival, len3_iarray
      REAL ( KIND = wp ) :: value4, value6
      LOGICAL :: defnam, inrep, defaut, doloop
      LOGICAL :: end_bound_section, end_start_section, end_quadratic_section
      LOGICAL :: end_element_type_section, end_element_uses_section
      LOGICAL :: end_group_type_section, start_group_uses_section
      LOGICAL :: dgrset, adddoloop, qgroup, qsqr, qprod
      LOGICAL :: setana, delset, grp1st, grpyet, varyet, fixed
      CHARACTER ( LEN = 2 ) :: field1, colfie
      CHARACTER ( LEN = 10 ) :: field2, field3, field5, grupe, elmnt
      CHARACTER ( LEN = 10 ) :: detype, dgtype
      CHARACTER ( LEN = 12 ) :: field, header
      CHARACTER ( LEN = 24 ) :: bad_alloc
      CHARACTER ( LEN = max_record_length ) :: nuline, blnkln
      INTEGER, DIMENSION( 4 ) :: LOOP
      CHARACTER ( LEN = 65 ), DIMENSION( maxnul ) :: NULINA
      INTEGER, ALLOCATABLE, DIMENSION( :, : ) :: INSTR1, INSTR2, INSTR3
      INTEGER, ALLOCATABLE, DIMENSION( :, :, : ) :: IARRAY
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: RVALUE1, RVALUE2
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: RVALUE3
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( :, : ) :: VARRAY
      CHARACTER ( LEN = 2 ), ALLOCATABLE, DIMENSION( : )  :: FARRAY
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( :, : )  :: CARRAY, ARRAY

!  initial allocatble array dimensions

      INTEGER :: len_gp_ptr, len_eling_ptr
      INTEGER :: len1_idrows = 2
      INTEGER :: len1_rdrows = 2
      INTEGER :: len1_instr1 = 5
      INTEGER :: len1_instr2 = 5
      INTEGER :: len1_instr3 = 5
      INTEGER :: len1_array = 3
      INTEGER :: len1_carray = 2
      INTEGER :: len1_varray = 2
      INTEGER :: len1_iarray = 5
      INTEGER :: len2_iarray = 3

!  set starting array sizes

!  debug problems

      IF ( size == 0 ) THEN
        len_elvar = nelvar_guess_debug
        len_gtypesp_ptr = MAX( ngtype_guess_debug + 1, 2 )
        len_ganames = MAX( ngtype_guess_debug, 1 )
        len_gtypes = MAX( ngtype_guess_debug, 1 )
        len_gstate = ng_guess_debug
        len_gtype = ng_guess_debug
        len_gnames = ng_guess_debug
        len_rscale = ng_guess_debug
        len2_idrows = ng_guess_debug
        len2_rdrows = ng_guess_debug
        len2_cstart = MAX( nstart_guess_debug, 1 )
        len2_vstart = MAX( nstart_guess_debug, 1 )
        len_snames = nstart_guess_debug
        len_onames = ng_guess_debug
        len_gp_val_orig = ngp_guess_debug
        len_eling_el = nel_guess_debug
        len_eling_g = nel_guess_debug
        len_weight = nel_guess_debug
        len_typee = nel_guess_debug
        len_ev_ptr = nel_guess_debug + 1
        len_ep_ptr = nel_guess_debug + 1
        len_lnames = nel_guess_debug
        len_elv = netype_guess_debug + 1
        len_inv = netype_guess_debug + 1
        len_elp = netype_guess_debug + 1
        len_etypes = MAX( netype_guess_debug, 1 )
        len_evnames = 5 * netype_guess_debug
        len_ivnames = 5 * netype_guess_debug
        len_epnames = 3 * netype_guess_debug
        len_gpnames = 2 * ngtype_guess_debug
        len_ep_val = nelp_guess_debug
        len_a = nnza_guess_debug
        len2_blu = MAX( nbnd_guess_debug, 1 )
        len_blu_default = MAX( nbnd_guess_debug, 1 )
        len_bnames = MAX( nbnd_guess_debug, 1 )
        len_obbname = nobjbound_guess_debug
        len_fbound = nobjbound_guess_debug
        len_cscale = n_guess_debug
        len_typev = n_guess_debug
        len_default = n_guess_debug
        len_vnames = n_guess_debug
        len_iival = niindex_guess_debug
        len_rival = nrindex_guess_debug
        len2_instr1 = MAX( maxins_guess_debug, 1 )
        len2_instr2 = maxins_guess_debug
        len2_instr3 = maxins_guess_debug
        len_rvalue1 = MAX( maxarray_guess_debug, 1 )
        len_rvalue2 = maxarray_guess_debug
        len_rvalue3 = maxarray_guess_debug
        len_farray = MAX( maxarray_guess_debug, 1 )
        len2_array = maxarray_guess_debug
        len2_carray = maxarray_guess_debug
        len2_varray = maxarray_guess_debug
        len3_iarray = maxarray_guess_debug
        len_iinames = niindex_guess_debug
        len_rinames = nrindex_guess_debug
        length = n_guess_debug + ng_guess_debug + nel_guess_debug              &
                   + nauxpar_guess_debug + 1000

!  small problems

      ELSE IF ( size == 1 ) THEN
        len_elvar = nelvar_guess_small
        len_gtypesp_ptr = MAX( ngtype_guess_small + 1, 2 )
        len_ganames = MAX( ngtype_guess_small, 1 )
        len_gtypes = MAX( ngtype_guess_small, 1 )
        len_gstate = ng_guess_small
        len_gtype = ng_guess_small
        len_gnames = ng_guess_small
        len_rscale = ng_guess_small
        len2_idrows = ng_guess_small
        len2_rdrows = ng_guess_small
        len2_cstart = MAX( nstart_guess_small, 1 )
        len2_vstart = MAX( nstart_guess_small, 1 )
        len_snames = nstart_guess_small
        len_onames = ng_guess_small
        len_gp_val_orig = ngp_guess_small
        len_eling_el = nel_guess_small
        len_eling_g = nel_guess_small
        len_weight = nel_guess_small
        len_typee = nel_guess_small
        len_ev_ptr = nel_guess_small + 1
        len_ep_ptr = nel_guess_small + 1
        len_lnames = nel_guess_small
        len_elv = netype_guess_small + 1
        len_inv = netype_guess_small + 1
        len_elp = netype_guess_small + 1
        len_etypes = MAX( netype_guess_small, 1 )
        len_evnames = 5 * netype_guess_small
        len_ivnames = 5 * netype_guess_small
        len_epnames = 3 * netype_guess_small
        len_gpnames = 2 * ngtype_guess_small
        len_ep_val = nelp_guess_small
        len_a = nnza_guess_small
        len2_blu = MAX( nbnd_guess_small, 1 )
        len_blu_default = MAX( nbnd_guess_small, 1 )
        len_bnames = MAX( nbnd_guess_small, 1 )
        len_obbname = nobjbound_guess_small
        len_fbound = nobjbound_guess_small
        len_cscale = n_guess_small
        len_typev = n_guess_small
        len_default = n_guess_small
        len_vnames = n_guess_small
        len_iival = niindex_guess_small
        len_rival = nrindex_guess_small
        len2_instr1 = MAX( maxins_guess_small, 1 )
        len2_instr2 = maxins_guess_small
        len2_instr3 = maxins_guess_small
        len_rvalue1 = MAX( maxarray_guess_small, 1 )
        len_rvalue2 = maxarray_guess_small
        len_rvalue3 = maxarray_guess_small
        len_farray = MAX( maxarray_guess_small, 1 )
        len2_array = maxarray_guess_small
        len2_carray = maxarray_guess_small
        len2_varray = maxarray_guess_small
        len3_iarray = maxarray_guess_small
        len_iinames = niindex_guess_small
        len_rinames = nrindex_guess_small
        length = n_guess_small + ng_guess_small + nel_guess_small              &
                   + nauxpar_guess_small + 1000

!  large  problems

      ELSE IF ( size == 3 ) THEN
        len_elvar = nelvar_guess_large
        len_gtypesp_ptr = MAX( ngtype_guess_large + 1, 2 )
        len_ganames = MAX( ngtype_guess_large, 1 )
        len_gtypes = MAX( ngtype_guess_large, 1 )
        len_gstate = ng_guess_large
        len_gtype = ng_guess_large
        len_gnames = ng_guess_large
        len_rscale = ng_guess_large
        len2_idrows = ng_guess_large
        len2_rdrows = ng_guess_large
        len2_cstart = MAX( nstart_guess_large, 1 )
        len2_vstart = MAX( nstart_guess_large, 1 )
        len_snames = nstart_guess_large
        len_onames = ng_guess_large
        len_gp_val_orig = ngp_guess_large
        len_eling_el = nel_guess_large
        len_eling_g = nel_guess_large
        len_weight = nel_guess_large
        len_typee = nel_guess_large
        len_ev_ptr = nel_guess_large + 1
        len_ep_ptr = nel_guess_large + 1
        len_lnames = nel_guess_large
        len_elv = netype_guess_large + 1
        len_inv = netype_guess_large + 1
        len_elp = netype_guess_large + 1
        len_etypes = MAX( netype_guess_large, 1 )
        len_evnames = 5 * netype_guess_large
        len_ivnames = 5 * netype_guess_large
        len_epnames = 3 * netype_guess_large
        len_gpnames = 2 * ngtype_guess_large
        len_ep_val = nelp_guess_large
        len_a = nnza_guess_large
        len2_blu = MAX( nbnd_guess_large, 1 )
        len_blu_default = MAX( nbnd_guess_large, 1 )
        len_bnames = MAX( nbnd_guess_large, 1 )
        len_obbname = nobjbound_guess_large
        len_fbound = nobjbound_guess_large
        len_cscale = n_guess_large
        len_typev = n_guess_large
        len_default = n_guess_large
        len_vnames = n_guess_large
        len_iival = niindex_guess_large
        len_rival = nrindex_guess_large
        len2_instr1 = MAX( maxins_guess_large, 1 )
        len2_instr2 = maxins_guess_large
        len2_instr3 = maxins_guess_large
        len_rvalue1 = MAX( maxarray_guess_large, 1 )
        len_rvalue2 = maxarray_guess_large
        len_rvalue3 = maxarray_guess_large
        len_farray = MAX( maxarray_guess_large, 1 )
        len2_array = maxarray_guess_large
        len2_carray = maxarray_guess_large
        len2_varray = maxarray_guess_large
        len3_iarray = maxarray_guess_large
        len_iinames = niindex_guess_large
        len_rinames = nrindex_guess_large
        length = n_guess_large + ng_guess_large + nel_guess_large              &
                   + nauxpar_guess_large + 1000

!  medium problems (default)

      ELSE ! IF ( size == 2 ) THEN
        len_elvar = nelvar_guess_medium
        len_gtypesp_ptr = MAX( ngtype_guess_medium + 1, 2 )
        len_ganames = MAX( ngtype_guess_medium, 1 )
        len_gtypes = MAX( ngtype_guess_medium, 1 )
        len_gstate = ng_guess_medium
        len_gtype = ng_guess_medium
        len_gnames = ng_guess_medium
        len_rscale = ng_guess_medium
        len2_idrows = ng_guess_medium
        len2_rdrows = ng_guess_medium
        len2_cstart = MAX( nstart_guess_medium, 1 )
        len2_vstart = MAX( nstart_guess_medium, 1 )
        len_snames = nstart_guess_medium
        len_onames = ng_guess_medium
        len_gp_val_orig = ngp_guess_medium
        len_eling_el = nel_guess_medium
        len_eling_g = nel_guess_medium
        len_weight = nel_guess_medium
        len_typee = nel_guess_medium
        len_ev_ptr = nel_guess_medium + 1
        len_ep_ptr = nel_guess_medium + 1
        len_lnames = nel_guess_medium
        len_elv = netype_guess_medium + 1
        len_inv = netype_guess_medium + 1
        len_elp = netype_guess_medium + 1
        len_etypes = MAX( netype_guess_medium, 1 )
        len_evnames = 5 * netype_guess_medium
        len_ivnames = 5 * netype_guess_medium
        len_epnames = 3 * netype_guess_medium
        len_gpnames = 2 * ngtype_guess_medium
        len_ep_val = nelp_guess_medium
        len_a = nnza_guess_medium
        len2_blu = MAX( nbnd_guess_medium, 1 )
        len_blu_default = MAX( nbnd_guess_medium, 1 )
        len_bnames = MAX( nbnd_guess_medium, 1 )
        len_obbname = nobjbound_guess_medium
        len_fbound = nobjbound_guess_medium
        len_cscale = n_guess_medium
        len_typev = n_guess_medium
        len_default = n_guess_medium
        len_vnames = n_guess_medium
        len_iival = niindex_guess_medium
        len_rival = nrindex_guess_medium
        len2_instr1 = MAX( maxins_guess_medium, 1 )
        len2_instr2 = maxins_guess_medium
        len2_instr3 = maxins_guess_medium
        len_rvalue1 = MAX( maxarray_guess_medium, 1 )
        len_rvalue2 = maxarray_guess_medium
        len_rvalue3 = maxarray_guess_medium
        len_farray = MAX( maxarray_guess_medium, 1 )
        len2_array = maxarray_guess_medium
        len2_carray = maxarray_guess_medium
        len2_varray = maxarray_guess_medium
        len3_iarray = maxarray_guess_medium
        len_iinames = niindex_guess_medium
        len_rinames = nrindex_guess_medium
        length = n_guess_medium + ng_guess_medium + nel_guess_medium           &
                   + nauxpar_guess_medium + 1000
      END IF

!  set initial values for integer variables

      intype = 1 ; intypo = 1 ; lineno = 0 ; nvar = 0 ; nnza = 0
      ng = 0 ; nbnd = 0 ; nstart = 0 ; nobj = 0 ; neltype = 0 ; ngtype = 0
      nelnum = 0 ; ngrupe = 0 ; nelvar = 0 ; nlisep = 0 ; nlisgp = 0
      neling = 0 ; niival = 0 ; nrival = 0 ; nobbnd = 0 ; ndtype = 0
      nevnames = 0 ; nivnames = 0 ; nepnames = 0 ; nlvars = - 1 ; nnlvrs = - 1
      nconst = - 1 ; nrange = - 1 ; level = 0 ; ilines = 0 ; nlines = 0
      iptype = 0 ; istype = 0 ; status = 0

!  set initial values for logical variables

      defnam = .FALSE. ; doloop = .FALSE. ;
      end_bound_section = .FALSE. ; end_start_section = .FALSE.
      end_quadratic_section = .FALSE.
      end_element_type_section = .FALSE. ; end_element_uses_section = .FALSE.
      end_group_type_section = .FALSE. ; start_group_uses_section = .FALSE.
      grpyet = .FALSE. ; varyet = .FALSE. ; delset = .FALSE. ; dgrset = .FALSE.
      grp1st = .TRUE. ; fixed = .TRUE. ; qgroup = .FALSE. ; qsqr = .FALSE.
      qprod = .FALSE.
      elmnt = "          "

!  set initial values for real variables

      value4 = 0.0D+0 ; value6 = 0.0D+0

!  allocate arrays

      CALL ALLOCATE_array( TYPEV, len_typev, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'TYPEV' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( ELV, len_elv, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'ELV' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( INV, len_inv, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'INV' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( ELP, len_elp, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'ELP' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( EV_ptr, len_ev_ptr, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'EV_ptr' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( TYPEE, len_typee, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'TYPEE' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( EP_ptr, len_ep_ptr, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'EP_ptr' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( ELING_el, len_eling_el, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'ELING_el' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( ELING_g, len_eling_el, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'ELING_g' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( ELVAR, len_elvar, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'ELVAR' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( ETYPES, len_etypes, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'ETYPES' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( IIVAL, len_iival, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'IIVAL' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( RIVAL, len_rival, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'RIVAL' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( GTYPESP_ptr, len_gtypesp_ptr, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'GTYPESP_ptr' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( GANAMES, len_ganames, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'GANAMES' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( GTYPES, len_gtypes, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'GTYPES' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( GSTATE, len_gstate, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'GSTATE' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( GTYPE, len_gtype, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'GTYPE' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( GP_val_orig, len_gp_val_orig, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'GP_val_orig' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( EP_val, len_ep_val, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'EP_val' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( A_row, len_a, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'A_row' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( A_col, len_a, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'A_col' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( A_val, len_a, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'A_val' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( B_l_default, len_blu_default, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'B_l_default' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( B_u_default, len_blu_default, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'B_u_default' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( CSCALE, len_cscale, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'CSCALE' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( DEFAULT, len_default, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'DEFAULT' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( FBOUND_l, len_fbound, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'FBOUND_l' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( FBOUND_u, len_fbound, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'FBOUND_u' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( RSCALE, len_rscale, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'RSCALE' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( WEIGHT, len_weight, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'WEIGHT' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( IDROWS, len1_idrows, len2_idrows, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'IDROWS' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( RDROWS, len1_rdrows, len2_rdrows, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'RDROWS' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( BNAMES, len_bnames, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'BNAMES' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( VNAMES, len_vnames, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'VNAMES' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( GNAMES, len_gnames, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'GNAMES' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( LNAMES, len_lnames, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'LNAMES' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( ONAMES, len_onames, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'GNAMES' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( OBBNAME, len_obbname, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'OBBNAME' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( SNAMES, len_snames, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'SNAMES' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( EVNAMES, len_evnames, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'EVNAMES' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( IVNAMES, len_ivnames, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'IVNAMES' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( EPNAMES, len_epnames, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'EPNAMES' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( GPNAMES, len_gpnames, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'GPNAMES' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( IINAMES, len_iinames, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'IINAMES' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( RINAMES, len_rinames, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'RINAMES' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( INSTR1, len1_instr1, len2_instr1, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'INSTR1' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( INSTR2, len1_instr2, len2_instr2, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'INSTR2' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( INSTR3, len1_instr3, len2_instr3, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'INSTR3' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( RVALUE1, len_rvalue1, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'RVALUE1' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( RVALUE2, len_rvalue2, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'RVALUE1' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( RVALUE3, len_rvalue3, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'RVALUE1' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( FARRAY, len_farray, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'FARRAY' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( VARRAY, len1_varray, len2_varray, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'VARRAY' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( CARRAY, len1_carray, len2_carray, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'CARRAY' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( ARRAY, len1_array, len2_array, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'ARRAY' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( IARRAY, len1_iarray, len2_iarray, len3_iarray,      &
                           alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'IARRAY' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( INLIST, length, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'INLIST' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( TABLE, length, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'TABLE' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( KEY, length, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'KEY' ; GO TO 980 ; END IF

!  set up TABLE data

      CALL HASH_initialize( length, TABLE )

!  initialize row data

      RSCALE( :  len_rscale ) = one
      IDROWS( : len1_idrows, : len2_idrows ) = 0
      RDROWS( : len1_idrows, : len2_idrows ) = zero
      DEFAULT( : len_default ) = zero

!  initialize dictionary data

      INLIST( : length ) = 0

!  set a blank line

      DO i = 1, max_record_length
        BLNKLN( i : i ) = ' '
      END DO

!  start of main loop

  100 CONTINUE

!  read next line from the input file

      IF ( ilines + 1 > nlines ) THEN
        lineno = lineno + 1
        nuline = blnkln
        IF ( fixed ) THEN
          READ ( input, 1000, END = 810, ERR = 810 ) nuline
          IF ( out > 0 .AND. debug ) WRITE( out, 2990 ) lineno, nuline
        ELSE
          READ ( input, 1010, END = 810, ERR = 810 ) nuline
          IF ( out > 0 .AND. debug ) WRITE( out, 2970 ) lineno, nuline

!  if the card is in free format, translate it into fixed format

          CALL FREE_format( nuline, max_record_length, mendat, INDIC8,         &
                            LENIND, NULINA, maxnul, nlines, .TRUE.,            &
                            status, out )
          IF ( status > 0 ) GO TO 800

!  if there are non-blank lines on the free format card, read the first

          IF ( nlines > 0 ) THEN
            ilines = 1
            nuline = blnkln
            nuline = NULINA( ilines )
            IF ( out > 0 .AND. debug ) WRITE( out, 2980 ) lineno, ilines, nuline

!  there are only blank lines on the free format card

          ELSE
            GO TO 100
          END IF
        END IF

!  read next line from the last encountered free format card

      ELSE
        ilines = ilines + 1
        nuline = blnkln
        nuline = NULINA( ilines )
        IF ( out > 0 .AND. debug ) WRITE( out, 2980 ) lineno, ilines, nuline
      END IF

!  consider the header part of the card

      header = NULINE( 1 : 12 )

!  ignore blank lines

      IF ( header == INDIC8( mblank ) ) THEN
        IF (  NULINE( 13 : 14 ) == '  ' .AND. NULINE( 15 : 24 ) == '          '&
              .AND. NULINE( 40 : 49 ) == '          ' ) GO TO 100
      END IF
      IF ( NULINE( 1 : 1 ) /= ' ' ) THEN

!  ignore comment cards

        IF ( NULINE( 1 : 1 ) == '*' ) GO TO 100

!  check if we have entered fixed-format input

        IF ( header == INDIC8( mfixed ) ) THEN
          fixed = .TRUE.
          GO TO 100
        END IF

!  check if we have entered free-format input

        IF ( header == INDIC8( mfree ) ) THEN
          fixed = .FALSE.
          GO TO 100
        END IF

!  check that the first encountered indicator card is the name card

        IF ( .NOT. defnam  ) THEN
          IF ( header /= INDIC8( mname ) ) THEN
            status = 1
            IF ( out > 0 ) WRITE( out, 2010 )
            GO TO 800

!  indicator card is name
!  -----------------------

          ELSE
            defnam = .TRUE.
            pname = NULINE( 15: 24 )
            GO TO 100
          END IF
        END IF

!  an indicator card has been found

        IF ( .NOT. grp1st ) intype = mrows
        DO i = intype, mendat
          IF ( header == INDIC8( i ) ) THEN
            intype = i
            GO TO 120
          END IF
        END DO

!  the indicator card is not recognised

        status = 2
        IF ( out > 0 ) WRITE( out, 2020 )
        GO TO 800

  120   CONTINUE
        IF ( intype == mgroup .OR. intype == mcnstr ) intype = mrows
        IF ( intype == mrhs .OR. intype == mrhsp ) intype = mconst
        IF ( intype == mvars ) intype = mcols
        IF ( intype == mquadr .OR. intype == mquads .OR.                       &
             intype == mquado .OR. intype == mqsect .OR.                       &
             intype == mqmatr ) intype = mqhess

!  ensure that the groups and variables sections do not get mixed up

        IF ( .NOT. grp1st .AND. varyet .AND. intype == mcols ) THEN
          status = 21
          IF ( out > 0 ) WRITE( out, 2210 )
          GO TO 800
        END IF
        IF ( intype == mrows ) grpyet = .TRUE.
        IF ( intype == mcols ) varyet = .TRUE.
        IF ( varyet .AND. .NOT. grpyet ) grp1st = .FALSE.

!  ensure that previously started do-loops have been finished

        IF ( intype /= intypo .AND. doloop ) THEN
           status = 38
           IF ( out > 0 ) WRITE( out, 2380 )
           GO TO 800
        END IF
        intypo = intype

!  all of the linear variables have been specified

        IF ( intype >= mconst ) THEN
          IF ( nlvars < 0 ) THEN
            nlvars = nvar
            n = nlvars
            len1_vstart = nlvars
            CALL ALLOCATE_array( VSTART, len1_vstart, len2_vstart,             &
                                 alloc_status )
            IF ( alloc_status /= 0 ) THEN
              bad_alloc = 'VSTART' ; GO TO 980 ; END IF

            len1_blu = nlvars
            CALL ALLOCATE_array( B_l, len1_blu, len2_blu,                      &
                                 alloc_status )
            IF ( alloc_status /= 0 ) THEN
              bad_alloc = 'B_l' ; GO TO 980 ; END IF

            len1_blu = nlvars
            CALL ALLOCATE_array( B_u, len1_blu, len2_blu,                      &
                                 alloc_status )
            IF ( alloc_status /= 0 ) THEN
              bad_alloc = 'B_u' ; GO TO 980 ; END IF
          END IF
        END IF

!  the right-hand-side vectors have been completed

        IF ( intype >= mrange ) THEN
          IF ( nconst < 0 ) nconst = nvar - nlvars
          IF ( nconst == 0 ) THEN
            nconst = 1
            nvar = nvar + 1
            IF ( nvar > len_default ) THEN
              used_length = nvar - 1 ; min_length = nvar
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( DEFAULT, len_default, used_length, new_length,&
                                 min_length, buffer, status, alloc_status,     &
                                 'DEFAULT' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'DEFAULT' ; status = - 7 ; GO TO 980 ; END IF
              len_default = new_length
            END IF

            IF ( nvar > len_vnames ) THEN
              used_length = nvar - 1 ; min_length = nvar
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( VNAMES, len_vnames, used_length, new_length,  &
                                 min_length, buffer, status, alloc_status,     &
                                 'VNAMES' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'VNAMES' ; status = - 7 ; GO TO 980 ; END IF
              len_vnames = new_length
            END IF

            DEFAULT( nvar ) = zero
            VNAMES( nvar ) = 'no rhs    '
          END IF
        END IF

!  the range vectors have been completed

        IF ( intype >= mbound ) THEN
          IF ( nrange < 0 ) nrange = nvar - nconst - nlvars
        END IF

!  the bound vectors have been completed

        IF ( intype >= mstart ) THEN
          IF ( .NOT. end_bound_section ) THEN
            end_bound_section = .TRUE.
            IF ( nbnd == 0 ) THEN
              nbnd = 1
              defaut = .TRUE.
              B_l_default( nbnd ) = zero
              B_u_default( nbnd ) = biginf
              DO i = 1, nlvars
                B_l( i, nbnd ) = zero
                B_u( i, nbnd ) = biginf
              END DO
            END IF
            len1_cstart = ng + 1
            CALL ALLOCATE_array( CSTART, len1_cstart, len2_cstart,             &
                                 alloc_status )
            IF ( alloc_status /= 0 ) THEN
              bad_alloc = 'CSTART' ; GO TO 980 ; END IF
          END IF
        END IF

!  the starting vectors have been completed

        IF ( intype >= mqhess ) THEN
          IF ( .NOT. end_start_section ) THEN
            end_start_section = .TRUE.
            IF ( nstart == 0 ) THEN
              nstart = 1
              DO i = 1, nlvars
                VSTART( i, nstart ) = zero
              END DO
              DO i = 1, ng
                CSTART( i, nstart ) = zero
              END DO
            END IF
          END IF
        END IF

!  the quadratic Hessian has been completed

        IF ( intype >= metype ) THEN
          IF ( .NOT. end_quadratic_section ) THEN
            end_quadratic_section = .TRUE.
            len_gp_ptr = ng + 1
            CALL ALLOCATE_array( GP_ptr, len_gp_ptr, alloc_status )
            IF ( alloc_status /= 0 ) THEN
              bad_alloc = 'GP_ptr' ; GO TO 980 ; END IF

            len_eling_ptr = ng + 1
            CALL ALLOCATE_array( ELING_ptr, len_eling_ptr, alloc_status )
            IF ( alloc_status /= 0 ) THEN
              bad_alloc = 'ELING_ptr' ; GO TO 980 ; END IF
          END IF
        END IF

!  the element types have all been specified

        IF ( intype >= meuses ) THEN
          IF ( .NOT. end_element_type_section ) THEN
            end_element_type_section = .TRUE.

!  if the last element has no explicit internal representation,
!  use its elemental representation

            IF ( neltype > 0 ) THEN
              IF ( ETYPES( neltype ) /= cqsqr .AND.                            &
                   ETYPES( neltype ) /= cqprod ) THEN
                IF ( .NOT. inrep ) THEN
                  i = nevnames - ELV( neltype ) + 1
                  IF ( nivnames + i > len_ivnames ) THEN
                    used_length = nivnames ; min_length = nivnames + i
                    new_length = increase_n * min_length / increase_d + 1
                    CALL EXTEND_array( IVNAMES, len_ivnames, used_length,      &
                                       new_length, min_length, buffer,         &
                                       status, alloc_status, 'IVNAMES' )
                    IF ( status /= 0 ) THEN
                      bad_alloc = 'IVNAMES' ; status = - 16 ; GO TO 980
                    END IF
                    len_ivnames = new_length
                  END IF
                  DO k = ELV( neltype ), nevnames
                    nivnames = nivnames + 1
                    IVNAMES( nivnames ) = EVNAMES( k )
                  END DO
                ELSE
                  IF ( nivnames - INV( neltype ) >=                            &
                       nevnames - ELV( neltype ) ) THEN
                    status = 76
                    IF ( out > 0 ) WRITE( out, 2760 )
                    GO TO 800
                  END IF
                END IF
              END IF

              IF ( neltype >= len_elv ) THEN
                used_length = neltype ; min_length = neltype + 1
                new_length = min_length + 1
                CALL EXTEND_array( ELV, len_elv, used_length, new_length,      &
                                   min_length, buffer, status, alloc_status,   &
                                   'ELV' )
                IF ( status /= 0 ) THEN
                  bad_alloc = 'ELV' ; status = - 3 ; GO TO 980 ; END IF
                len_elv = new_length
              END IF

              IF ( neltype >= len_inv ) THEN
                used_length = neltype ; min_length = neltype + 1
                new_length = min_length + 1
                CALL EXTEND_array( INV, len_inv, used_length, new_length,      &
                                   min_length, buffer, status, alloc_status,   &
                                   'INV' )
                IF ( status /= 0 ) THEN
                  bad_alloc = 'INV' ; status = - 3 ; GO TO 980 ; END IF
                len_inv = new_length
              END IF

              IF ( neltype >= len_elp ) THEN
                used_length = neltype ; min_length = neltype + 1
                new_length = min_length + 1
                CALL EXTEND_array( ELP, len_elp, used_length, new_length,      &
                                   min_length, buffer, status, alloc_status,   &
                                   'ELP' )
                IF ( status /= 0 ) THEN
                  bad_alloc = 'ELP' ; status = - 3 ; GO TO 980 ; END IF
                len_elp = new_length
              END IF
            END IF
            ELV( neltype + 1 ) = nevnames + 1
            INV( neltype + 1 ) = nivnames + 1
            ELP( neltype + 1 ) = nepnames + 1
          END IF
        END IF

!  the nonlinear elements have all been specified

        IF ( intype >= mgtype ) THEN

!  check if there are any nonlinear variables

          IF ( nnlvrs < 0 ) nnlvrs = n - nlvars
          IF ( .NOT. end_element_uses_section ) THEN
            end_element_uses_section = .TRUE.

!  set column data for nonlinear variables

            IF ( n > len_cscale ) THEN
              used_length = nlvars ; min_length = n
              new_length = n
              CALL EXTEND_array( CSCALE, len_cscale, used_length, new_length,  &
                                 min_length, buffer, status, alloc_status,     &
                                 'CSCALE' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'CSCALE' ; status = - 7 ; GO TO 980 ; END IF
              len_cscale = new_length
            END IF

            IF ( n > len_typev ) THEN
              used_length = nlvars ; min_length = n
              new_length = n
              CALL EXTEND_array( TYPEV, len_typev, used_length, new_length,    &
                                 min_length, buffer, status, alloc_status,     &
                                 'TYPEV' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'TYPEV' ; status = - 7 ; GO TO 980 ; END IF
              len_typev = new_length
            END IF
            TYPEV( nlvars + 1 : n ) = 0
            CSCALE( nlvars + 1 : n ) = one

!  check that the nonlinear elements have been completely specified
!  first check the parameter values have been set

            IF ( nelnum > 0 ) THEN
              DO j = 1, nelnum
                k = TYPEE( j )
                ip = ELP( k ) - 1
                k1 = ELP( k + 1 ) - ELP( k )
                k2 = EP_ptr( j ) - 1
                DO i = 1, k1
                  IF ( EP_val( k2 + i ) == biginf ) THEN
                    status = 28
                    IF ( out > 0 ) WRITE( out, 2280 )                          &
                       LNAMES( j ), EPNAMES( ip + i )
                  END IF
                END DO

!  now check the elemental variables have been set

                is = ELV( k ) - 1
                k1 = ELV( k + 1 ) - ELV( k )
                k2 = EV_ptr( j ) - 1
                DO i = 1, k1
                  IF ( ELVAR( k2 + i ) == 0 ) THEN
                    status = 16
                    IF ( out > 0 ) WRITE( out, 2160 )                          &
                       LNAMES( j ), EVNAMES( is + i )
                  END IF
                END DO
              END DO
            END IF
            IF ( status /= 0 ) RETURN
          END IF
          EP_ptr( nelnum + 1 ) = nlisep + 1
          EV_ptr( nelnum + 1 ) = nelvar + 1
        END IF

!  the group types have all been specified

        IF ( intype >= mguses ) THEN

!  check if this is the first group type

          IF ( ngtype == 0 ) THEN
            ngpnames = 0

!  check that the argument for the last group-type has been set

          ELSE
            IF ( .NOT. end_group_type_section ) THEN
              end_group_type_section = .TRUE.
              IF ( .NOT. setana ) THEN
                status = 25
                IF ( out > 0 ) WRITE( out, 2250 )
                RETURN
              END IF
              IF ( ngtype >= len_gtypesp_ptr ) THEN
                used_length = ngtype ; min_length = ngtype + 1
                new_length = increase_n * min_length / increase_d + 1
                CALL EXTEND_array( GTYPESP_ptr, len_gtypesp_ptr,               &
                                   used_length, new_length, min_length,        &
                                   buffer, status, alloc_status,               &
                                   'GTYPESP_ptr' )
                IF ( status /= 0 ) THEN
                  bad_alloc = 'GTYPESP_ptr' ; status = - 4 ; GO TO 980 ; END IF
                len_gtypesp_ptr = new_length
              END IF
              GTYPESP_ptr( ngtype + 1 ) = ngpnames + 1
            END IF
          END IF
        END IF

        IF ( intype == mendat ) THEN
          k = MAX( ng + 1, nelnum )
          CALL ALLOCATE_array( IWK, k, alloc_status )
          IF ( alloc_status /= 0 ) THEN
            bad_alloc = 'IWK' ; GO TO 980 ; END IF

          CALL ALLOCATE_array( GP_val, nlisgp, alloc_status )
          IF ( alloc_status /= 0 ) THEN
            bad_alloc = 'GP_val' ; GO TO 980 ; END IF

!  check that the groups have been completely specified by checking that the
!  parameter values have been set

          nlisgp = 0
          DO j = 1, ng
            k = GTYPE( j )
            IF ( k < 0 ) THEN
              k = - k - 1
              GTYPE( j ) = k
              GP_ptr( j ) = nlisgp + 1
              IF ( k /= 0 ) THEN
                k1 = GTYPESP_ptr( k + 1 ) - GTYPESP_ptr( k )
                IF ( k1 > 0 ) THEN
                  ip = GTYPESP_ptr( k ) - 1
                  k2 = GP_ptr( j ) - 1
                  status = 34
                  DO i = 1, k1
                    nlisgp = nlisgp + 1
                    GP_val( nlisgp ) = biginf
                    IF ( out > 0 )                                             &
                      WRITE( out, 2340 ) GNAMES( j ), GPNAMES( ip + i )
                  END DO
                END IF
              END IF
            ELSE IF ( k == 0 ) THEN
              GP_ptr( j ) = nlisgp + 1
            ELSE
              GP_ptr( j ) = nlisgp + 1
              ip = GTYPESP_ptr( k ) - 1
              k1 = GTYPESP_ptr( k + 1 ) - GTYPESP_ptr( k )
              k2 = GP_ptr( j ) - 1
              DO i = 1, k1
                nlisgp = nlisgp + 1
                GP_val( nlisgp ) = GP_val_orig( k2 + i )
                IF ( GP_val( nlisgp ) == biginf ) THEN
                  status = 34
                  IF ( out > 0 ) WRITE( out, 2340 )                            &
                    GNAMES( j ), GPNAMES( ip + i )
                END IF
              END DO
            END IF
          END DO
          GP_ptr( ng + 1 ) = nlisgp + 1
          IF ( status /= 0 ) RETURN

!  sort the list of elements for each group, so that the elements for group i
!  precede those for group i + 1, i = 1, ng - 1

          IF ( neling > 0 ) THEN
            CALL REORDER( ng, neling, ELING_el, ELING_g,                       &
                          WEIGHT, ELING_ptr, IWK )
          ELSE
            ELING_ptr( : ng + 1 ) = 1
          END IF

!  ensure there is sufficient room for the names of slack variables

          IF ( nvar + ng > len_vnames ) THEN
            used_length = nvar ; min_length = nvar + ng
            new_length = nvar + ng
            CALL EXTEND_array( VNAMES, len_vnames, used_length, new_length,    &
                               min_length, buffer, status, alloc_status,       &
                               'VNAMES' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'VNAMES' ; status = - 7 ; GO TO 980 ; END IF
            len_vnames = new_length
          END IF

          IF ( nvar + ng > len_typev ) THEN
            used_length = nvar ; min_length = nvar + ng
            new_length = nvar + ng
            CALL EXTEND_array( TYPEV, len_typev, used_length, new_length,      &
                               min_length, buffer, status, alloc_status,       &
                               'TYPEV' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'TYPEV' ; status = - 7 ; GO TO 980 ; END IF
            len_typev = new_length
          END IF
        END IF

!  indicator card is endata
!  -------------------------

        IF ( intype == mendat ) GO TO 900
        GO TO 100
      END IF

!  a data card has been found. Rread the character fields 1, 2, 3 and 5
!  from the new data line

      field1 = NULINE(  2 :  3 )
      field2 = NULINE(  5 : 14 )
      field3 = NULINE( 15 : 24 )
      field5 = NULINE( 40 : 49 )

!  start of a do-loop
! ===================

      IF ( field1 == 'DO' ) THEN
        IF ( level >= 3 ) THEN
          status = 13
          IF ( out > 0 ) WRITE( out, 2130 )
          GO TO 800
        END IF

!  this is the first level of the loop

        IF ( level == 0 ) THEN
          doloop = .TRUE.
          narray = 0
          ninstr1 = 0 ; ninstr2 = 0 ; ninstr3 = 0

!  this is the second or third level of the loop

        ELSE IF ( level == 1 ) THEN
          ninstr1 = ninstr1 + 1
          IF ( ninstr1 > len2_instr1 ) THEN
            used_length = 5 ; new_length = 5 ; min_length = 5
            used_length2 = ninstr1 - 1 ; min_length2 = ninstr1
            new_length2 = increase_n * min_length2 / increase_d + 1
            CALL EXTEND_array( INSTR1, 5, len2_instr1, used_length,            &
                               used_length2, new_length, new_length2,          &
                               min_length, min_length2, buffer,                &
                               status, alloc_status, 'INSTR1' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'INSTR1' ; status = - 11 ; GO TO 980 ; END IF
            len2_instr1 = new_length2
          END IF
          IF ( debug .AND. out > 0 ) WRITE( out, 4010 ) 1, ninstr1
          INSTR1( 1, ninstr1 ) = 1
        ELSE IF ( level == 2 ) THEN
          ninstr2 = ninstr2 + 1
          IF ( ninstr2 > len2_instr2 ) THEN
            used_length = 5 ; new_length = 5 ; min_length = 5
            used_length2 = ninstr2 - 1 ; min_length2 = ninstr2
            new_length2 = increase_n * min_length2 / increase_d + 1
            CALL EXTEND_array( INSTR2, 5, len2_instr2, used_length,            &
                               used_length2, new_length, new_length2,          &
                               min_length, min_length2, buffer,                &
                               status, alloc_status, 'INSTR2' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'INSTR2' ; status = - 11 ; GO TO 980 ; END IF
            len2_instr2 = new_length2
          END IF
          IF ( debug .AND. out > 0 ) WRITE( out, 4010 ) 2, ninstr2
          INSTR2( 1, ninstr2 ) = 1
        ELSE
          ninstr3 = ninstr3 + 1
          IF ( ninstr3 > len2_instr3 ) THEN
            used_length = 5 ; new_length = 5 ; min_length = 5
            used_length2 = ninstr3 - 1 ; min_length2 = ninstr3
            new_length2 = increase_n * min_length2 / increase_d + 1
            CALL EXTEND_array( INSTR3, 5, len2_instr3, used_length,            &
                               used_length2, new_length, new_length2,          &
                               min_length, min_length2, buffer,                &
                               status, alloc_status, 'INSTR3' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'INSTR3' ; status = - 11 ; GO TO 980 ; END IF
            len2_instr3 = new_length2
          END IF
          IF ( debug .AND. out > 0 ) WRITE( out, 4010 ) 3, ninstr3
          INSTR3( 1, ninstr3 ) = 1
        END IF

!  record the location of the do-loop variable in the array inlist

        field = FIELD2( 1 : 10 ) // 'II'
        CALL HASH_enlarge_and_insert( length, 12, field,                       &
                                      TABLE, KEY, INLIST, ifree )
        IF ( ifree <= 0 ) THEN
          IF ( ifree == 0 ) GO TO 700
          ifree = - ifree
        ELSE
          niival = niival + 1
          IF ( niival > len_iival ) THEN
            used_length = niival - 1 ; min_length = niival
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( IIVAL, len_iival, used_length, new_length,      &
                               min_length, buffer, status, alloc_status,       &
                               'IIVAL' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'IIVAL' ; status = - 21 ; GO TO 980 ; END IF
            len_iival = new_length
          END IF

          IF ( niival > len_iinames ) THEN
            used_length = niival - 1 ; min_length = niival
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( IINAMES, len_iinames, used_length, new_length,  &
                               min_length, buffer, status, alloc_status,     &
                               'IINAMES' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'IINAMES' ; status = - 21 ; GO TO 980 ; END IF
            len_iinames = new_length
          END IF
          INLIST( ifree ) = niival
          IINAMES( niival ) = FIELD( 1 : 7 )
        END IF
        IF ( level == 0 ) THEN
          LOOP( 1 ) = INLIST( ifree )
        ELSE IF ( level == 1 ) THEN
          INSTR1( 2, ninstr1 ) = INLIST( ifree )
        ELSE IF ( level == 2 ) THEN
          INSTR2( 2, ninstr2 ) = INLIST( ifree )
        ELSE
          INSTR3( 2, ninstr3 ) = INLIST( ifree )
        END IF

!  record the starting value of the do-loop variable

        field = FIELD3( 1 : 10 ) // 'II'
        CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
        IF ( ifield <= 0 ) THEN
          status = 3
          IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
          GO TO 800
        END IF
        IF ( level == 0 ) THEN
          LOOP( 2 ) = INLIST( ifield )
        ELSE IF ( level == 1 ) THEN
          INSTR1( 3, ninstr1 ) = INLIST( ifield )
        ELSE IF ( level == 2 ) THEN
          INSTR2( 3, ninstr2 ) = INLIST( ifield )
        ELSE
          INSTR3( 3, ninstr3 ) = INLIST( ifield )
        END IF

!  record the finishing value of the do-loop variable and
!  set the incremental value of the do-loop variable to 1

        field = FIELD5( 1 : 10 ) // 'II'
        CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
        IF ( ifield <= 0 ) THEN
          status = 3
          IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
          GO TO 800
        END IF
        IF ( level == 0 ) THEN
          LOOP( 3 ) = INLIST( ifield )
          LOOP( 4 ) = - 1
        ELSE IF ( level == 1 ) THEN
          INSTR1( 4, ninstr1 ) = INLIST( ifield )
          INSTR1( 5, ninstr1 ) = - 1
        ELSE IF ( level == 2 ) THEN
          INSTR2( 4, ninstr2 ) = INLIST( ifield )
          INSTR2( 5, ninstr2 ) = - 1
        ELSE
          INSTR3( 4, ninstr3 ) = INLIST( ifield )
          INSTR3( 5, ninstr3 ) = - 1
        END IF
        level = level + 1
        GO TO 100
      END IF

!  a do-loop variable is to have a non-trivial increment

      IF ( field1 == 'DI' ) THEN

!  record the location of the do-loop variable in the array inlist

        adddoloop = .FALSE.
        IF ( level == 1 ) THEN
          adddoloop = ( FIELD2( 1 : 10 ) == IINAMES( LOOP( 1 ) ) )
        ELSE IF( level == 2 ) THEN
          adddoloop = ( FIELD2( 1 : 10 ) ==                                    &
            IINAMES( INSTR1( 2, ninstr1 ) ) )
        ELSE IF( level == 3 ) THEN
          adddoloop = ( FIELD2( 1 : 10 ) ==                                    &
            IINAMES( INSTR2( 2, ninstr2 ) ) )
        ENDIF
        IF ( adddoloop ) THEN
          IF ( debug .AND. out > 0 .AND. level == 2 )                          &
            WRITE( out, 4030 ) 1, ninstr1
          IF ( debug .AND. out > 0 .AND. level == 3 )                          &
            WRITE( out, 4030 ) 2, ninstr2
          field = FIELD3( 1 : 10 ) // 'II'
          CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
          IF ( ifield <= 0 ) THEN
            status = 3
            IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
            GO TO 800
          END IF
          IF ( level == 1 ) THEN
            LOOP( 4 ) = INLIST( ifield )
          ELSE IF ( level == 2 ) THEN
            INSTR1( 5, ninstr1 ) = INLIST( ifield )
          ELSE IF ( level == 3 ) THEN
            INSTR2( 5, ninstr2 ) = INLIST( ifield )
          END IF
        END IF
        GO TO 100
      END IF

!  end of one or more do-loops
! ============================

      IF ( field1 /= 'OD' .AND. field1 /= 'ND' ) GO TO 341

!  terminate the current level of loop

      IF ( field1 == 'OD' ) THEN
        IF ( level == 1 ) THEN
          ninstr1 = ninstr1 + 1
          IF ( ninstr1 > len2_instr1 ) THEN
            used_length = 5 ; new_length = 5 ; min_length = 5
            used_length2 = ninstr1 - 1 ; min_length2 = ninstr1
            new_length2 = increase_n * min_length2 / increase_d + 1
            CALL EXTEND_array( INSTR1, 5, len2_instr1, used_length,            &
                               used_length2, new_length, new_length2,          &
                               min_length, min_length2, buffer,                &
                               status, alloc_status, 'INSTR1' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'INSTR1' ; status = - 11 ; GO TO 980 ; END IF
            len2_instr1 = new_length2
          END IF
          INSTR1( 1, ninstr1 ) = 2
          IF ( debug .AND. out > 0 ) WRITE( out, 4020 ) 1, ninstr1
        ELSE IF ( level == 2 ) THEN
          ninstr2 = ninstr2 + 1
          IF ( ninstr2 > len2_instr2 ) THEN
            used_length = 5 ; new_length = 5 ; min_length = 5
            used_length2 = ninstr2 - 1 ; min_length2 = ninstr2
            new_length2 = increase_n * min_length2 / increase_d + 1
            CALL EXTEND_array( INSTR2, 5, len2_instr2, used_length,            &
                               used_length2, new_length, new_length2,          &
                               min_length, min_length2, buffer,                &
                               status, alloc_status, 'INSTR2' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'INSTR2' ; status = - 11 ; GO TO 980 ; END IF
            len2_instr2 = new_length2
          END IF
          INSTR2( 1, ninstr2 ) = 2
          IF ( debug .AND. out > 0 ) WRITE( out, 4020 ) 2, ninstr2
        ELSE IF ( level == 3 ) THEN
          ninstr3 = ninstr3 + 1
          IF ( ninstr3 > len2_instr3 ) THEN
            used_length = 5 ; new_length = 5 ; min_length = 5
            used_length2 = ninstr3 - 1 ; min_length2 = ninstr3
            new_length2 = increase_n * min_length2 / increase_d + 1
            CALL EXTEND_array( INSTR3, 5, len2_instr3, used_length,            &
                               used_length2, new_length, new_length2,          &
                               min_length, min_length2, buffer,                &
                               status, alloc_status, 'INSTR3' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'INSTR3' ; status = - 11 ; GO TO 980 ; END IF
            len2_instr3 = new_length2
          END IF
          INSTR3( 1, ninstr3 ) = 2
          IF ( debug .AND. out > 0 ) WRITE( out, 4020 ) 3, ninstr3
        END IF
        level = level - 1
      ELSE
        IF ( level == 3 ) THEN
          ninstr3 = ninstr3 + 1
          IF ( ninstr3 > len2_instr3 ) THEN
            used_length = 5 ; new_length = 5 ; min_length = 5
            used_length2 = ninstr3 - 1 ; min_length2 = ninstr3
            new_length2 = increase_n * min_length2 / increase_d + 1
            CALL EXTEND_array( INSTR3, 5, len2_instr3, used_length,            &
                               used_length2, new_length, new_length2,          &
                               min_length, min_length2, buffer,                &
                               status, alloc_status, 'INSTR3' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'INSTR3' ; status = - 11 ; GO TO 980 ; END IF
            len2_instr3 = new_length2
          END IF
          INSTR3( 1, ninstr3 ) = 2
          IF ( debug .AND. out > 0 ) WRITE( out, 4020 ) 3, ninstr3
          level = 2
        END IF
        IF ( level == 2 ) THEN
          ninstr2 = ninstr2 + 1
          IF ( ninstr2 > len2_instr2 ) THEN
            used_length = 5 ; new_length = 5 ; min_length = 5
            used_length2 = ninstr2 - 1 ; min_length2 = ninstr2
            new_length2 = increase_n * min_length2 / increase_d + 1
            CALL EXTEND_array( INSTR2, 5, len2_instr2, used_length,            &
                               used_length2, new_length, new_length2,          &
                               min_length, min_length2, buffer,                &
                               status, alloc_status, 'INSTR2' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'INSTR2' ; status = - 11 ; GO TO 980 ; END IF
            len2_instr2 = new_length2
          END IF
          INSTR2( 1, ninstr2 ) = 2
          IF ( debug .AND. out > 0 ) WRITE( out, 4020 ) 2, ninstr2
          level = 1
        END IF
        IF ( level == 1 ) THEN
          ninstr1 = ninstr1 + 1
          IF ( ninstr1 > len2_instr1 ) THEN
            used_length = 5 ; new_length = 5 ; min_length = 5
            used_length2 = ninstr1 - 1 ; min_length2 = ninstr1
            new_length2 = increase_n * min_length2 / increase_d + 1
            CALL EXTEND_array( INSTR1, 5, len2_instr1, used_length,            &
                               used_length2, new_length, new_length2,          &
                               min_length, min_length2, buffer,                &
                               status, alloc_status, 'INSTR1' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'INSTR1' ; status = - 11 ; GO TO 980 ; END IF
            len2_instr1 = new_length2
          END IF
          INSTR1( 1, ninstr1 ) = 2
          IF ( debug .AND. out > 0 ) WRITE( out, 4020 ) 1, ninstr1
          level = 0
        END IF
      END IF

!  execute do-loop instructions
! =============================

      IF ( level /= 0 ) GO TO 339

!  execute level-1 do-loop instructions

        lev1s = IIVAL( LOOP( 2 ) )
        lev1e = IIVAL( LOOP( 3 ) )
        IF ( LOOP( 4 ) <= 0 ) THEN
          lev1i = 1
        ELSE
          lev1i = IIVAL( LOOP( 4 ) )
        END IF

!  mock do-loop

          lev1 = lev1s
  220     CONTINUE
          IF ( .NOT. ( ( lev1i > 0 .AND. lev1 <= lev1e ) .OR.                  &
               ( lev1i < 0 .AND. lev1 >= lev1e ) ) ) GO TO 337
          l1 = 0 ; l2 = 0 ; l3 = 0

!  set the loop value

          IIVAL( LOOP( 1 ) ) = lev1
          IF ( debug .AND. out > 0 ) WRITE( out, 5000 )                        &
            1, IINAMES( LOOP( 1 ) ), lev1

!  execute the remaining list of level-1 instructions

  230     CONTINUE
          l1 = l1 + 1

!  see if the level-1 loop is to be terminated

          IF ( INSTR1( 1, l1 ) == 2 ) GO TO 300

!  see if a level-2 loop is to be started

          IF ( INSTR1( 1, l1 ) /= 1 ) GO TO 295

!  execute level-2 do-loop instructions

          lev2s = IIVAL( INSTR1( 3, l1 ) )
          lev2e = IIVAL( INSTR1( 4, l1 ) )
          IF ( INSTR1( 5, l1 ) <= 0 ) THEN
            lev2i = 1
          ELSE
            lev2i = IIVAL( INSTR1( 5, l1 ) )
          END IF
          level2 = l2
          levl3a = l3

!  mock do-loop

          lev2 = lev2s
  240     CONTINUE
          l2 = level2
          IF ( .NOT. ( lev2i > 0 .AND. lev2 <= lev2e ) .OR.                    &
             ( lev2i < 0 .AND. lev2 >= lev2e ) ) GO TO 292
          l3 = levl3a

!  set the loop value

          IIVAL( INSTR1( 2, l1 ) ) = lev2
          IF ( debug .AND. out > 0 ) WRITE( out, 5000 )                        &
                  2, IINAMES( INSTR1( 2, l1 ) ), lev2

!  execute the remaining list of level-2 instructions

  250     CONTINUE
          l2 = l2 + 1

!  see if the level-2 loop is to be terminated

          IF ( INSTR2( 1, l2 ) == 2 ) GO TO 290

!  see if a level-3 loop is to be started

          IF ( INSTR2( 1, l2 ) /= 1 ) GO TO 283

!  execute level-3 do-loop instructions

          lev3s = IIVAL( INSTR2( 3, l2 ) )
          lev3e = IIVAL( INSTR2( 4, l2 ) )
          IF ( INSTR2( 5, l2 ) <= 0 ) THEN
            lev3i = 1
          ELSE
            lev3i = IIVAL( INSTR2( 5, l2 ) )
          END IF
          level3 = l3

!  mock do-loop

          lev3 = lev3s
  260     CONTINUE
          l3 = level3
          IF ( .NOT.  ( lev3i > 0 .AND. lev3 <= lev3e ) .OR.                   &
                      ( lev3i < 0 .AND. lev3 >= lev3e ) ) GO TO 281

!  set the loop value

          IIVAL( INSTR2( 2, l2 ) ) = lev3
          IF ( debug .AND. out > 0 )                                           &
             WRITE( out, 5000 ) 3, IINAMES( INSTR2( 2, l2 ) ), lev3

!  execute the remaining list of level-3 instructions

  270     CONTINUE
          l3 = l3 + 1

!  see if the level-3 loop is to be terminated

          IF ( INSTR3( 1, l3 ) == 2 ) GO TO 280

!  execute level-3 index instructions

          IF ( INSTR3( 1, l3 ) >= 21 .AND. INSTR3( 1, l3 ) <= 50 ) THEN
            CALL EVALUATE_integer( niival, nrival, IIVAL, RIVAL,               &
                                   INSTR3( 1, l3 ) )
            IF ( debug .AND. out > 0 ) WRITE( out, 5010 ) 3, l3,               &
              IINAMES( INSTR3( 2, l3 ) ), IIVAL( INSTR3( 2, l3 ) )
          END IF
          IF ( INSTR3( 1, l3 ) >= 51 .AND. INSTR3( 1, l3 ) <= 99 ) THEN
            CALL EVALUATE_real( niival, nrival, IIVAL, RIVAL, RVALUE3( l3 ),   &
                                INSTR3( 1, l3 ), status )
            IF ( status > 0 ) GO TO 800
            IF ( debug .AND. out > 0 ) WRITE( out, 5020 ) 3, l3,               &
                RINAMES( INSTR3( 2, l3 ) ), RIVAL( INSTR3( 2, l3 ) )
          END IF
          IF ( INSTR3( 1, l3 ) >= 100 ) THEN
            narray = INSTR3( 2, l3 )
            CALL GET_line( niival, nrival, IIVAL, IARRAY( 1, 1, narray ),      &
                           VARRAY( 1, narray ), ARRAY( 1, narray ),            &
                           CARRAY( 1, narray ), FARRAY( narray ), RIVAL,       &
                           IINAMES, novals, INSTR3( 1, l3 ), field1,           &
                           field2, field3, value4, field5, value6,             &
                           length, TABLE, KEY, INLIST, out, status )
            IF ( status > 0 ) GO TO 800
            IF ( status < 0 ) GO TO 700
            IF ( debug .AND. out > 0 ) WRITE( out, 5060 )                      &
                   3, l3, field1, field2, field3, value4, field5, value6
            ijump = 3
            GO TO 400
          END IF
          GO TO 270
  280     CONTINUE
          lev3 = lev3 + lev3i
          GO TO 260

!  the do-loop is not executed. find the next relevant instruction

  281     CONTINUE
          l3 = l3 + 1
          IF ( INSTR3( 1, l3 ) /= 2 ) GO TO 281

!  end of level-3 do-loop

  283     CONTINUE

!  execute level-2 index instructions

          IF ( INSTR2( 1, l2 ) >= 21 .AND. INSTR2( 1, l2 ) <= 50 ) THEN
            CALL EVALUATE_integer( niival, nrival, IIVAL, RIVAL,               &
                                   INSTR2( 1, l2 ) )
            IF ( debug .AND. out > 0 ) WRITE( out, 5010 ) 2, l2,               &
                  IINAMES( INSTR2( 2, l2 ) ), IIVAL( INSTR2( 2, l2 ) )
          END IF
          IF ( INSTR2( 1, l2 ) >= 51 .AND. INSTR2( 1, l2 ) <= 99 ) THEN
            CALL EVALUATE_real( niival, nrival, IIVAL, RIVAL, RVALUE2( l2 ),   &
                                INSTR2( 1, l2 ), status )
            IF ( status > 0 ) GO TO 800
            IF ( debug .AND. out > 0 ) WRITE( out, 5020 ) 2, l2,               &
               RINAMES( INSTR2( 2, l2 ) ), RIVAL( INSTR2( 2, l2 ) )
          END IF
          IF ( INSTR2( 1, l2 ) >= 100 ) THEN
            narray = INSTR2( 2, l2 )
            CALL GET_line( niival, nrival, IIVAL, IARRAY( 1, 1, narray ),      &
                           VARRAY( 1, narray ), ARRAY( 1, narray ),            &
                           CARRAY( 1, narray ), FARRAY( narray ), RIVAL,       &
                           IINAMES, novals, INSTR2( 1, l2 ), field1,           &
                           field2, field3, value4, field5, value6,             &
                           length, TABLE, KEY, INLIST, out, status )
            IF ( status > 0 ) GO TO 800
            IF ( status < 0 ) GO TO 700
            IF ( debug .AND. out > 0 ) WRITE( out, 5060 )                      &
                  2, l2, field1, field2, field3, value4, field5, value6
            ijump = 2
            GO TO 400
          END IF
          GO TO 250
  290     CONTINUE
          lev2 = lev2 + lev2i
          GO TO 240

!  the do-loop is not executed. find the next relevant instruction

  292     CONTINUE
          l2 = l2 + 1
          IF ( INSTR2( 1, l2 ) /= 2 ) GO TO 292
          level2 = l2

!  end of level-2 do-loop

  295     CONTINUE

!  execute level-1 index instructions

          IF ( INSTR1( 1, l1 ) >= 21 .AND. INSTR1( 1, l1 ) <= 50 ) THEN
            CALL EVALUATE_integer( niival, nrival, IIVAL, RIVAL,               &
                                   INSTR1( 1, l1 ) )
            IF ( debug .AND. out > 0 ) WRITE( out, 5010 ) 1, l1,               &
              IINAMES( INSTR1( 2, l1 ) ), IIVAL( INSTR1( 2, l1 ) )
          END IF
          IF ( INSTR1( 1, l1 ) >= 51 .AND. INSTR1( 1, l1 ) <= 99 ) THEN
            CALL EVALUATE_real( niival, nrival, IIVAL, RIVAL, RVALUE1( l1 ),   &
                                INSTR1( 1, l1 ), status )
            IF ( status > 0 ) GO TO 800
            IF ( debug .AND. out > 0 ) WRITE( out, 5020 ) 1, l1,               &
                RINAMES( INSTR1( 2, l1 ) ), RIVAL( INSTR1( 2, l1 ) )
          END IF
          IF ( INSTR1( 1, l1 ) >= 100 ) THEN
            narray = INSTR1( 2, l1 )
            CALL GET_line( niival, nrival, IIVAL, IARRAY( 1, 1, narray ),      &
                           VARRAY( 1, narray ), ARRAY( 1, narray ),            &
                           CARRAY( 1, narray ), FARRAY( narray ), RIVAL,       &
                           IINAMES, novals, INSTR1( 1, l1 ), field1,           &
                           field2, field3, value4, field5, value6,             &
                           length, TABLE, KEY, INLIST, out, status )
             IF ( status > 0 ) GO TO 800
             IF ( status < 0 ) GO TO 700
             IF ( debug .AND. out > 0 ) WRITE( out, 5060 )                     &
               1, l1, field1, field2, field3, value4, field5, value6
             ijump = 1
             GO TO 400
          END IF
          GO TO 230
  300     CONTINUE
          lev1 = lev1 + lev1i
          GO TO 220
  337     CONTINUE

!  end of level-1 do-loop

          doloop = .FALSE.
  339   CONTINUE
        GO TO 100
  341 CONTINUE

!  construct a list of do-loop instructions: 1) arithmetic instructions
! =====================================================================

      IF ( doloop ) THEN
        IF ( level == 1 ) THEN
          ninstr1 = ninstr1 + 1
          IF ( ninstr1 > len2_instr1 ) THEN
            used_length = 5 ; new_length = 5 ; min_length = 5
            used_length2 = ninstr1 - 1 ; min_length2 = ninstr1
            new_length2 = increase_n * min_length2 / increase_d + 1
            CALL EXTEND_array( INSTR1, 5, len2_instr1, used_length,            &
                               used_length2, new_length, new_length2,          &
                               min_length, min_length2, buffer,                &
                               status, alloc_status, 'INSTR1' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'INSTR1' ; status = - 11 ; GO TO 980 ; END IF
            len2_instr1 = new_length2
          END IF

          IF ( ninstr1 > len_rvalue1 ) THEN
            used_length = ninstr1 - 1 ; min_length = ninstr1
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( RVALUE1, len_rvalue1, used_length, new_length,  &
                               min_length, buffer, status, alloc_status,       &
                               'RVALUE1' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'RVALUE1' ; status = - 11 ; GO TO 980 ; END IF
            len_rvalue1 = new_length
          END IF
        ELSE IF ( level == 2 ) THEN
          ninstr2 = ninstr2 + 1
          IF ( ninstr2 > len2_instr2 ) THEN
            used_length = 5 ; new_length = 5 ; min_length = 5
            used_length2 = ninstr2 - 1 ; min_length2 = ninstr2
            new_length2 = increase_n * min_length2 / increase_d + 1
            CALL EXTEND_array( INSTR2, 5, len2_instr2, used_length,            &
                               used_length2, new_length, new_length2,          &
                               min_length, min_length2, buffer,                &
                               status, alloc_status, 'INSTR2' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'INSTR2' ; status = - 11 ; GO TO 980 ; END IF
            len2_instr2 = new_length2
          END IF

          IF ( ninstr2 > len_rvalue2 ) THEN
            used_length = ninstr2 - 1 ; min_length = ninstr2
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( RVALUE2, len_rvalue2, used_length, new_length,  &
                               min_length, buffer, status, alloc_status,       &
                               'RVALUE2' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'RVALUE2' ; status = - 11 ; GO TO 980 ; END IF
            len_rvalue2 = new_length
          END IF
        ELSE
          ninstr3 = ninstr3 + 1
          IF ( ninstr3 > len2_instr3 ) THEN
            used_length = 5 ; new_length = 5 ; min_length = 5
            used_length2 = ninstr3 - 1 ; min_length2 = ninstr3
            new_length2 = increase_n * min_length2 / increase_d + 1
            CALL EXTEND_array( INSTR3, 5, len2_instr3, used_length,            &
                               used_length2, new_length, new_length2,          &
                               min_length, min_length2, buffer,                &
                               status, alloc_status, 'INSTR3' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'INSTR3' ; status = - 11 ; GO TO 980 ; END IF
            len2_instr3 = new_length2
          END IF

          IF ( ninstr3 > len_rvalue3 ) THEN
            used_length = ninstr3 - 1 ; min_length = ninstr3
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( RVALUE3, len_rvalue3, used_length, new_length,  &
                               min_length, buffer, status, alloc_status,       &
                               'RVALUE3' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'RVALUE3' ; status = - 11 ; GO TO 980 ; END IF
            len_rvalue3 = new_length
          END IF
        END IF

!  an arithmetic instruction is to be processed

        IF ( field1 == 'IE' .OR. field1 == 'IA' .OR.                           &
             field1 == 'IS' .OR. field1 == 'IM' .OR.                           &
             field1 == 'ID' .OR. field1 == 'IR' .OR.                           &
             field1 == 'I=' .OR. field1 == 'I+' .OR.                           &
             field1 == 'I-' .OR. field1 == 'I*' .OR.                           &
             field1 == 'I/' .OR. field1 == 'RE' .OR.                           &
             field1 == 'RA' .OR. field1 == 'RS' .OR.                           &
             field1 == 'RM' .OR. field1 == 'RD' .OR.                           &
             field1 == 'RI' .OR. field1 == 'RF' .OR.                           &
             field1 == 'R=' .OR. field1 == 'R+' .OR.                           &
             field1 == 'R-' .OR. field1 == 'R*' .OR.                           &
             field1 == 'R/' .OR. field1 == 'R(' ) THEN
          IF ( level == 1 ) THEN
            CALL DECODE_scalar_instruction( niival, nrival, 1, ninstr1,        &
                         debug, RVALUE1( ninstr1 ),                            &
                         len_iival, IIVAL, len_rival, RIVAL,                   &
                         len_iinames, IINAMES, len_rinames, RINAMES,           &
                         INSTR1( 1, ninstr1 ),                                 &
                         field1, field2, field3, field5, NULINE( 25 : 36 ),    &
                         length, TABLE, KEY, INLIST, out, status )
          ELSE IF ( level == 2 ) THEN
            CALL DECODE_scalar_instruction( niival, nrival, 2, ninstr2,        &
                         debug, RVALUE2( ninstr2 ),                            &
                         len_iival, IIVAL, len_rival, RIVAL,                   &
                         len_iinames, IINAMES, len_rinames, RINAMES,           &
                         INSTR2( 1, ninstr2 ),                                 &
                         field1, field2, field3, field5, NULINE( 25 : 36 ),    &
                         length, TABLE, KEY, INLIST, out, status )
          ELSE
            CALL DECODE_scalar_instruction( niival, nrival, 3, ninstr3,        &
                         debug, RVALUE3( ninstr3 ),                            &
                         len_iival, IIVAL, len_rival, RIVAL,                   &
                         len_iinames, IINAMES, len_rinames, RINAMES,           &
                         INSTR3( 1, ninstr3 ),                                 &
                         field1, field2, field3, field5, NULINE( 25 : 36 ),    &
                         length, TABLE, KEY, INLIST, out, status )
          END IF
          IF ( status > 0 ) GO TO 800
          IF ( status < 0 ) GO TO 700

!  construct a list of do-loop instructions: 2) array definitions
! ===============================================================

        ELSE
          IF ( field1( 1 : 1 ) /= 'X' .AND. field1( 1 : 1 ) /= 'Z'             &
               .AND. field1( 1 : 1 ) /= 'A' ) THEN
             status = 6
             IF ( out > 0 ) WRITE( out, 2060 )
             GO TO 800
          END IF
          narray = narray + 1
          IF ( narray > len_farray ) THEN
            used_length = narray - 1 ; min_length = narray
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( FARRAY, len_farray, used_length, new_length,    &
                               min_length, buffer, status, alloc_status,       &
                               'FARRAY' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'FARRAY' ; status = - 12 ; GO TO 980 ; END IF
            len_farray = new_length
          END IF

          IF ( narray > len2_varray ) THEN
            used_length = 2 ; new_length = 2 ; min_length = 2
            used_length2 = narray - 1 ; min_length2 = narray
            new_length2 = increase_n * min_length2 / increase_d + 1
            CALL EXTEND_array( VARRAY, 2, len2_varray, used_length,            &
                               used_length2, new_length, new_length2,          &
                               min_length, min_length2, buffer,                &
                               status, alloc_status, 'VARRAY' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'VARRAY' ; status = - 12 ; GO TO 980 ; END IF
            len2_varray = new_length2
          END IF

          IF ( narray > len2_carray ) THEN
            used_length = 2 ; new_length = 2 ; min_length = 2
            used_length2 = narray - 1 ; min_length2 = narray
            new_length2 = increase_n * min_length2 / increase_d + 1
            CALL EXTEND_array( CARRAY, 2, len2_carray, used_length,            &
                               used_length2, new_length, new_length2,          &
                               min_length, min_length2, buffer,                &
                               status, alloc_status, 'CARRAY' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'CARRAY' ; status = - 12 ; GO TO 980 ; END IF
            len2_carray = new_length2
          END IF

          IF ( narray > len2_array ) THEN
            used_length = 3 ; new_length = 3 ; min_length = 3
            used_length2 = narray - 1 ; min_length2 = narray
            new_length2 = increase_n * min_length2 / increase_d + 1
            CALL EXTEND_array( ARRAY, 3, len2_array, used_length,              &
                               used_length2, new_length, new_length2,          &
                               min_length, min_length2, buffer,                &
                               status, alloc_status, 'ARRAY' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'ARRAY' ; status = - 12 ; GO TO 980 ; END IF
            len2_array = new_length2
          END IF

          IF ( narray > len3_iarray ) THEN
            used_length = 5 ; new_length = 5 ; min_length = 5
            used_length2 = 3 ; new_length2 = 3 ; min_length2 = 3
            used_length3 = narray - 1 ; min_length3 = narray
            new_length3 = increase_n * min_length3 / increase_d + 1
            CALL EXTEND_array( IARRAY, 5, 3, len3_iarray, used_length,         &
                               used_length2, used_length3,                     &
                               new_length, new_length2, new_length3,           &
                               min_length, min_length2, min_length3,           &
                               buffer, status, alloc_status, 'IARRAY' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'IARRAY' ; status = - 12 ; GO TO 980 ; END IF
            len3_iarray = new_length3
          END IF

          IF ( level == 1 ) THEN
            CALL DECODE_array_instruction( 1,                                  &
                       ninstr1, niival, nrival, narray,                        &
                       intype, debug, grp1st,                                  &
                       field1, field2, field3, field5,                         &
                       NULINE( 25 : 36 ), NULINE( 50 : 61 ),                   &
                       INSTR1( 1, ninstr1 ),                                   &
                       IARRAY( 1, 1, narray ), VARRAY( 1, narray ),            &
                       FARRAY( narray ), len_rival, RIVAL,                     &
                       IINAMES, len_rinames, RINAMES,                          &
                       ARRAY( 1, narray ), CARRAY( 1, narray ),                &
                       length, TABLE, KEY, INLIST, out, status )
            RVALUE1( ninstr1 ) = zero
          ELSE IF ( level == 2 ) THEN
            CALL DECODE_array_instruction( 2,                                  &
                       ninstr2, niival, nrival, narray,                        &
                       intype, debug, grp1st,                                  &
                       field1, field2, field3, field5,                         &
                       NULINE( 25 : 36 ), NULINE( 50 : 61 ),                   &
                       INSTR2( 1, ninstr2 ),                                   &
                       IARRAY( 1, 1, narray ), VARRAY( 1, narray ),            &
                       FARRAY( narray ), len_rival, RIVAL,                     &
                       IINAMES, len_rinames, RINAMES,                          &
                       ARRAY( 1, narray ), CARRAY( 1, narray ),                &
                       length, TABLE, KEY, INLIST, out, status )
            RVALUE2( ninstr2 ) = zero
          ELSE
            CALL DECODE_array_instruction( 3,                                  &
                       ninstr3, niival, nrival, narray,                        &
                       intype, debug, grp1st,                                  &
                       field1, field2, field3, field5,                         &
                       NULINE( 25 : 36 ), NULINE( 50 : 61 ),                   &
                       INSTR3( 1, ninstr3 ),                                   &
                       IARRAY( 1, 1, narray ), VARRAY( 1, narray ),            &
                       FARRAY( narray ), len_rival, RIVAL,                     &
                       IINAMES, len_rinames, RINAMES,                          &
                       ARRAY( 1, narray ), CARRAY( 1, narray ),                &
                       length, TABLE, KEY, INLIST, out, status )
            RVALUE3( ninstr3 ) = zero
          END IF
          IF ( status > 0 ) GO TO 800
          IF ( status < 0 ) GO TO 700
        END IF

!  the array definition is complete

        GO TO 100

!  execute a non-do-loop instruction
! ==================================

      ELSE

!  the instruction is an array instruction

        IF ( field1 == 'IE' .OR. field1 == 'IA' .OR.                           &
             field1 == 'IS' .OR. field1 == 'IM' .OR.                           &
             field1 == 'ID' .OR. field1 == 'IR' .OR.                           &
             field1 == 'I=' .OR. field1 == 'I+' .OR.                           &
             field1 == 'I-' .OR. field1 == 'I*' .OR.                           &
             field1 == 'I/' .OR. field1 == 'RE' .OR.                           &
             field1 == 'RA' .OR. field1 == 'RS' .OR.                           &
             field1 == 'RM' .OR. field1 == 'RD' .OR.                           &
             field1 == 'RI' .OR. field1 == 'RF' .OR.                           &
             field1 == 'R=' .OR. field1 == 'R+' .OR.                           &
             field1 == 'R-' .OR. field1 == 'R*' .OR.                           &
             field1 == 'R/' .OR. field1 == 'R(' ) THEN

!  1) an arithmetic instruction. decode the instruction

          CALL DECODE_scalar_instruction( niival, nrival, 0, 1, debug,         &
                       RVALUE1( 1 ), len_iival, IIVAL, len_rival, RIVAL,       &
                       len_iinames, IINAMES, len_rinames, RINAMES,             &
                       INSTR1( 1, 1 ),                                         &
                       field1, field2, field3, field5, NULINE( 25 : 36 ),      &
                       length, TABLE, KEY, INLIST, out, status )
          IF ( status > 0 ) GO TO 800
          IF ( status < 0 ) GO TO 700

!  execute the instruction

          IF ( field1( 1 : 1 ) == 'I' ) THEN
            CALL EVALUATE_integer( niival, nrival, IIVAL, RIVAL,               &
                                   INSTR1( 1, 1 ) )
            IF ( debug .AND. out > 0 ) WRITE( out, 5010 ) 0, 1,                &
                IINAMES( INSTR1( 2, 1 ) ), IIVAL( INSTR1( 2, 1 ) )
          ELSE
            CALL EVALUATE_real( niival, nrival, IIVAL, RIVAL,                  &
                                RVALUE1( 1 ), INSTR1( 1, 1 ), status )
            IF ( status > 0 ) GO TO 800
            IF ( debug .AND. out > 0 )                                         &
              WRITE( out, 5020 ) 0, 1, RINAMES( INSTR1( 2, 1 ) ),              &
              RIVAL( INSTR1( 2, 1 ) )
          END IF
          GO TO 100

!  2) an array definition. decode the instruction

        ELSE
          IF ( field1( 1 : 1 ) == 'X' .OR. field1( 1 : 1 ) == 'Z' .OR.         &
               field1( 1 : 1 ) == 'A' ) THEN
            CALL DECODE_array_instruction( 0, 1,                               &
                         niival, nrival, 1, intype, debug, grp1st,             &
                         field1, field2, field3, field5,                       &
                         NULINE( 25 : 36 ), NULINE( 50 : 61 ),                 &
                         INSTR1( 1, 1 ), IARRAY( 1, 1, 1 ), VARRAY( 1, 1 ),    &
                         FARRAY( 1 ), len_rival, RIVAL,                        &
                         IINAMES, len_rinames, RINAMES,                        &
                         ARRAY( 1, 1 ), CARRAY( 1, 1 ),                        &
                         length, TABLE, KEY, INLIST, out, status )
            RVALUE1( 1 ) = zero
            IF ( status > 0 ) GO TO 800
            IF ( status < 0 ) GO TO 700

!  execute the instruction

            CALL GET_line( niival, nrival, IIVAL,                              &
                           IARRAY( 1, 1, 1 ), VARRAY( 1, 1 ),                  &
                           ARRAY( 1, 1 ), CARRAY( 1, 1 ),                      &
                           FARRAY( 1 ), RIVAL, IINAMES, novals,                &
                           INSTR1( 1, 1 ), field1,                             &
                           field2, field3, value4, field5, value6,             &
                           length, TABLE, KEY, INLIST, out, status )
            IF ( status > 0 ) GO TO 800
            IF ( status < 0 ) GO TO 700
            IF ( debug .AND. out > 0 ) WRITE( out, 5060 )                      &
               0, 1, field1, field2, field3, value4, field5, value6
               GO TO 400

!  the instruction is not an array instruction
!  check to see if there is are any numerical values to be read

          ELSE
            novals = 0
            IF ( ( field3 /= '          ' .AND.                                &
                   NULINE( 15 : 15 ) /= '$' ) .OR. intype == mobbnd ) THEN
              novals = novals + 1
              IF (   intype <= mqhess .OR. intype == mobbnd .OR.               &
                ( ( intype == meuses .OR. intype == mguses )                   &
                    .AND. field1 == 'P ' ) .OR. ( intype == mguses             &
                          .AND. field1 == 'E ' ) ) THEN
                IF ( intype == mguses .AND. field1 == 'E '.AND.                &
                  NULINE( 25 : 36 ) == '            ' ) THEN
                  value4 = one
                ELSE
                   CALL GET_value( NULINE( 25 : 36 ), value4 )
                END IF
                IF ( intype == mrange ) value4 = ABS( value4 )
              END IF
              IF ( field5 /= '          ' .AND. NULINE( 40 : 40 ) /= '$' ) THEN
                novals = novals + 1
                IF (  intype <= mqhess .OR.                                    &
                    ( ( intype == meuses .OR. intype == mguses )               &
                         .AND. field1 == 'P ' ) .OR. ( intype ==               &
                              mguses .AND. field1 == 'E ' ) ) THEN
                  IF ( intype == mguses .AND. field1 == 'E ' .AND.             &
                        NULINE( 50 : 61 ) == '            ' ) THEN
                     value6 = one
                  ELSE
                    CALL GET_value( NULINE( 50 : 61 ), value6 )
                  END IF
                  IF ( intype == mrange ) value6 = ABS( value6 )
                  IF ( intype < mconst ) THEN
                    IF ( value6 == zero ) novals = 1
                  END IF
                END IF
              END IF

!  remove fields with numerical values of zero

              IF ( intype < mconst ) THEN
                IF ( value4 == zero ) THEN
                  IF ( novals == 2 ) THEN
                    value4 = value6
                    field3 = field5
                  END IF
                  novals = novals - 1
                END IF
              END IF
              IF ( field3 == '''SCALE''   ' .OR.  field3 == ' ''SCALE''  ') THEN
                novals = 0
                value4 = ABS( value4 )
              END IF
            END IF
            IF ( debug .AND. out > 0 ) WRITE( out, 5060 )                      &
              0, 1, field1, field2, field3, value4, field5, value6
          END IF
        END IF
      END IF
  400 CONTINUE

!  execute real parameter array card

      IF ( field1 == 'AE' .OR. field1 == 'AA' .OR.                             &
           field1 == 'AS' .OR. field1 == 'AM' .OR.                             &
           field1 == 'AD' .OR. field1 == 'AI' .OR.                             &
           field1 == 'AF' .OR. field1 == 'A=' .OR.                             &
           field1 == 'A+' .OR. field1 == 'A-' .OR.                             &
           field1 == 'A*' .OR. field1 == 'A/' .OR.                             &
           field1 == 'A(' ) THEN
        CALL EXECUTE_array_instruction( niival, nrival,                        &
                     IIVAL, len_rival, RIVAL, len_rinames, RINAMES,            &
                     field1, field2, field3, field5, value4,                   &
                     length, TABLE, KEY, INLIST, out, status )
        IF ( status > 0 ) GO TO 800
        IF ( status < 0 ) GO TO 700
        IF ( doloop ) GO TO 600
        GO TO 100
      END IF

!  branch depending on the current indicator card
! ===============================================

      GO TO ( 100, 100, 100, 100, 420, 420, 420, 430, 430, 430,                &
              430, 430, 430, 440, 450, 455, 455, 455, 455, 455,                &
              455, 460, 470, 480, 490, 500, 900 ), intype

!  indicator card is groups/rows/constraints
!  ------------------------------------------

  420 CONTINUE
      IF ( grp1st ) THEN
        CALL SGRP1( ng, nobj, novals, len2_idrows, IDROWS,                     &
                    len_gstate, GSTATE, len_gtype, GTYPE, len2_rdrows, RDROWS, &
                    len_rscale, RSCALE, field1, field2, field3, value4,        &
                    field5, value6, len_gnames, GNAMES, len_onames, ONAMES,    &
                    length, TABLE, KEY, INLIST, out, status )

      ELSE
        CALL SGRP2( ng, nnza, nobj, novals, len2_idrows, IDROWS,               &
                    len_gstate, GSTATE, len_gtype, GTYPE,                      &
                    len_a, A_row, A_col, A_val, len2_rdrows, RDROWS,           &
                    len_rscale, RSCALE, field1, field2, field3, value4,        &
                    field5, value6, len_gnames, GNAMES, len_onames, ONAMES,    &
                    length, TABLE, KEY, INLIST, out, status )
      END IF
      IF ( status == 0 ) THEN
        IF ( doloop ) GO TO 600
        GO TO 100
      END IF
      IF ( status > 0 ) GO TO 800
      GO TO 700

!  indicator card is columns/variables, constants/rhs/rhs' or ranges
!  ------------------------------------------------------------------

  430 CONTINUE
      IF ( intype == mcols  ) colfie = 'VA'
      IF ( intype == mconst ) colfie = 'CO'
      IF ( intype == mrange ) colfie = 'RA'
      IF ( grp1st .OR. intype /= mcols ) THEN
        CALL SVAR2( nnza, nvar, novals, nrival, intype == mcols, colfie,       &
                    len_gstate, GSTATE, len_typev, TYPEV,                      &
                    len_a, A_row, A_col, A_val,                                &
                    len_cscale, CSCALE, RIVAL, len_default, DEFAULT,           &
                    field1, field2, field3, value4, field5, value6,            &
                    len_vnames, VNAMES, length, TABLE, KEY, INLIST,            &
                    out, status )
      ELSE
        CALL SVAR1( nvar, colfie, len_typev, TYPEV, len_cscale, CSCALE,        &
                    field2, field3, value4, len_vnames, VNAMES,                &
                    length, TABLE, KEY, INLIST, out, status )
      END IF
      IF ( status == 0 ) THEN
         IF ( doloop ) GO TO 600
         GO TO 100
      END IF
      IF ( status > 0 ) GO TO 800
      GO TO 700

!  indicator card is bounds
!  -------------------------

  440 CONTINUE
      CALL SBOUND( nlvars, nbnd, ncol, nrival, defaut,                         &
                   len1_blu, len2_blu, B_l, B_u, RIVAL,                        &
                   field1, field2, field3, value4, field5, len_bnames, BNAMES, &
                   len_blu_default, B_l_default, B_u_default,                  &
                   length, TABLE, KEY, INLIST, out, status )
      IF ( status == 0 ) THEN
        IF ( doloop ) GO TO 600
        GO TO 100
      END IF
      IF ( status > 0 ) GO TO 800
      GO TO 700

!  indicator card is start point
!  ------------------------------

  450 CONTINUE
      CALL SSTART( nlvars, ng, novals, nstart, ncol, nrival, defaut,           &
                   len1_vstart, len2_vstart, VSTART,                           &
                   len1_cstart, len2_cstart, CSTART, RIVAL,                    &
                   field1, field2, field3, value4, field5, value6,             &
                   len_snames, SNAMES,                                         &
                   length, TABLE, KEY, INLIST, out, status )
      IF ( status == 0 ) THEN
        IF ( doloop ) GO TO 600
        GO TO 100
      END IF
      IF ( status > 0 ) GO TO 800
      GO TO 700

!  indicator card is qhess
!  ------------------------

  455 CONTINUE
      CALL SQHESS( ng, nobj, ngrupe, novals, nevnames, nivnames, nepnames,     &
                   neltype, nlisep, nelvar, nelnum, neling,                    &
                   iptype, istype, inrep, qgroup, qsqr, qprod,                 &
                   len_elv, ELV, len_inv, INV, len_elp, ELP, len_gtype, GTYPE, &
                   len_eling_el, ELING_el, len_eling_g, ELING_g,               &
                   len_ev_ptr, EV_ptr, len_elvar, ELVAR,                       &
                   len_gstate, GSTATE, len_typee, TYPEE, len_ep_ptr, EP_ptr,   &
                   len1_cstart, nstart, CSTART, len_rscale, RSCALE,            &
                   field1, field2, field3, value4, field5, value6,             &
                   len_evnames, EVNAMES, len_ivnames, IVNAMES,                 &
                   len_gnames, GNAMES, len_etypes, ETYPES, len_onames, ONAMES, &
                   len_lnames, LNAMES, len_weight, WEIGHT,                     &
                   length, TABLE, KEY, INLIST, out, status )
      IF ( status == 0 ) THEN
        IF ( doloop ) GO TO 600
        GO TO 100
      END IF
      IF ( status > 0 ) GO TO 800
      GO TO 700

!  indicator card is element type
!  -------------------------------

  460 CONTINUE
      CALL SETYPE( novals, nevnames, nivnames, nepnames, neltype, inrep,       &
                   len_elv, ELV, len_inv, INV, len_elp, ELP,                   &
                   field1, field2, field3, field5,                             &
                   len_evnames, EVNAMES, len_ivnames, IVNAMES,                 &
                   len_epnames, EPNAMES, len_etypes, ETYPES,                   &
                   length, TABLE, KEY, INLIST, out, status )
      IF ( status == 0 ) THEN
        IF ( doloop ) GO TO 600
        GO TO 100
      END IF
      IF ( status > 0 ) GO TO 800
      GO TO 700

!  indicator card is element uses
!  --------------------------------

  470 CONTINUE
      CALL SEUSES( neltype, nevnames, nepnames, nelvar, nlisep, novals,        &
                   nelnum, nelmnt, n, elmnt,                                   &
                   ELV, ELP, len_typee, TYPEE, len_elvar, ELVAR,               &
                   len_ev_ptr, EV_ptr, len_ep_ptr, EP_ptr,                     &
                   delset, detype, field1, field2, field3, value4, field5,     &
                   value6, len_ep_val, EP_val, EVNAMES, EPNAMES,               &
                   len_lnames, LNAMES, len_vnames, VNAMES,                     &
                   length, TABLE, KEY, INLIST, out, status )
      IF ( status == 0 ) THEN
        IF ( doloop ) GO TO 600
        GO TO 100
      END IF
      IF ( status > 0 ) GO TO 800
      GO TO 700

!  indicator card is group type
!  -----------------------------

  480 CONTINUE
      CALL SGTYPE( novals, ngtype, ngpnames, setana,                           &
                   len_gtypesp_ptr, GTYPESP_ptr,                               &
                   field1, field2, field3, field5,                             &
                   len_ganames, GANAMES, len_gtypes, GTYPES,                   &
                   len_gpnames, GPNAMES,                                       &
                   length, TABLE, KEY, INLIST, out, status )
      IF ( status == 0 ) THEN
        IF ( doloop ) GO TO 600
        GO TO 100
      END IF
      IF ( status > 0 ) GO TO 800
      GO TO 700

!  indicator card is group uses
!  -----------------------------

  490 CONTINUE
      CALL SGUSES( ng, ngtype, ngpnames, ngrupe, nlisgp,                       &
                   novals, neling, ndtype, start_group_uses_section, grupe,    &
                   GTYPESP_ptr, GTYPE,                                         &
                   len_eling_el, ELING_el, len_eling_g, ELING_g, GP_ptr,       &
                   GSTATE, dgrset, dgtype,                                     &
                   field1, field2, field3, value4, field5, value6,             &
                   len_gp_val_orig, GP_val_orig, GPNAMES,                      &
                   len_weight, WEIGHT,                                         &
                   length, TABLE, KEY, INLIST, out, status )
      IF ( status == 0 ) THEN
        IF ( doloop ) GO TO 600
        GO TO 100
      END IF
      IF ( status > 0 ) GO TO 800
      GO TO 700

!  indicator card is object bound
!  -------------------------------

  500 CONTINUE
      CALL SOBBND( nobbnd, nrival, len_fbound, FBOUND_l, FBOUND_u, RIVAL,      &
                   field1, field2, value4, field5, len_obbname, OBBNAME,       &
                   length, TABLE, KEY, INLIST, single, out, status )
      IF ( status == 0 ) THEN
        IF ( doloop ) GO TO 600
        GO TO 100
      END IF
      IF ( status > 0 ) GO TO 800
      GO TO 700

!  branch back into do loops at appropriate point

  600 CONTINUE
      IF ( ijump == 1 ) THEN
        GO TO 230
      ELSE IF ( ijump == 2 ) THEN
        GO TO 250
      ELSE
        GO TO 270
      END IF

!  insufficient space

  700 CONTINUE
      IF ( out > 0 ) WRITE( out, 2000 )
      RETURN

!  data card inconsistency

  800 CONTINUE
      IF ( out > 0 ) THEN
        IF ( doloop ) THEN
          WRITE( out, 2960 )                                                   &
            lineno, field1, field2, field3, value4, field5, value6
        ELSE
          WRITE( out, 2990 ) lineno, nuline
        END IF
      END IF
      GO TO 960

!  missing/incomplete data card

  810 CONTINUE
      IF ( .NOT. defnam ) THEN
        status = 74
        IF ( out > 0 ) WRITE( out, 2740 )
      ELSE
        status = 75
        IF ( out > 0 ) WRITE( out, 2750 )
      END IF
      GO TO 960

!  successful return

  900 CONTINUE
      status = 0
      IF ( debug .AND. out > 0 ) THEN
        WRITE( out, 3000 ) ( GNAMES( j ), j = 1, ng )
        WRITE( out, 3010 ) ( VNAMES( j ), j = 1, n )
        WRITE( out, 3020 )                                                     &
            ( A_row( j ), A_col( j ), A_val( j ), j = 1, nnza )
        WRITE( out, 3030 )                                                     &
         ( ( i, j, B_l( j, i ), B_u( j, i ), j = 1, nlvars ), i = 1, nbnd)
        WRITE( out, 3100 )                                                     &
         ( ( i, j, VSTART( j, i ), j = 1, nlvars ), i = 1, nstart )
        WRITE( out, 3040 ) ( RSCALE( j ), j = 1, ng )
        WRITE( out, 3050 ) ( CSCALE( j ), j = 1, n )
        IF ( neltype > 0 ) WRITE( out, 3060 )                                  &
           ( ETYPES( i ), ELV( i + 1 ) - ELV( i ), INV( i + 1 ) - INV( i ),    &
             ELP( i + 1 ) - ELP( i ), i = 1, neltype )
        IF ( ngtype > 0 )                                                      &
           WRITE( out, 3110 ) ( GTYPES( i ), GANAMES( i ),                     &
             GTYPESP_ptr( i + 1 ) - GTYPESP_ptr( i ), i = 1, ngtype )
        WRITE( out, 3070 )
        DO i = 1, ng
          k1 = ELING_ptr( i )
          k2 = ELING_ptr( i + 1 ) - 1
          is = GTYPE( i )
          IF ( k1 <= k2 ) THEN
            IF ( is == 0 ) THEN
              DO k = k1, k2
                l = ELING_el( k )
                WRITE( out, 3080 ) GNAMES( i ), 'TRIVIAL   ', LNAMES( l ),     &
                  ETYPES( TYPEE( l ) ), ( VNAMES( ELVAR( j ) ),                &
                  j = EV_ptr( l ), EV_ptr( l + 1 ) - 1 )
              END DO
            ELSE
              DO k = k1, k2
                l = ELING_el( k )
                WRITE( out, 3080 ) GNAMES( i ), GTYPES( is ), LNAMES( l ),     &
                  ETYPES( TYPEE( l ) ), ( VNAMES( ELVAR( j ) ),                &
                  j = EV_ptr( l ), EV_ptr( l + 1 ) - 1 )
              END DO
            END IF
          ELSE
            IF ( is == 0 ) THEN
              WRITE( out, 3090 ) GNAMES( i ), 'TRIVIAL   '
            ELSE
              WRITE( out, 3090 ) GNAMES( i ), GTYPES( is )
            END IF
          END IF
        END DO
      END IF

  960 CONTINUE
      IF ( debug .AND. out > 0 ) THEN
        DO i = 1, niival
          WRITE( out, 4000 ) i, IINAMES( i ), IIVAL( i )
        END DO
        DO i = 1, nrival
          WRITE( out, 4100 ) i, RINAMES( i ), RIVAL( i )
        END DO
      END IF
      RETURN

!  allocation errors

  980 CONTINUE
      WRITE( out, "( ' ** Message from INTERPRET_gpsmps-',                     &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  non-executable statements

 1000 FORMAT( A65 )
 1010 FORMAT( A160 )
 2000 FORMAT( ' ** Exit from INTERPRET_gpsmps - insufficient memory',          &
              ' available to enlarge hash table or allocatble arrays' )
 2010 FORMAT( ' ** Exit from INTERPRET_gpsmps - first card is not NAME ' )
 2020 FORMAT( ' ** Exit from INTERPRET_gpsmps - indicator card not recognised' )
 2030 FORMAT( ' ** Exit from INTERPRET_gpsmps - index parameter name ', A10,   &
              ' not recognised ' )
 2060 FORMAT( ' ** Exit from INTERPRET_gpsmps - non array defn. within do-loop')
 2130 FORMAT( ' ** Exit from INTERPRET_gpsmps - do loop level greater than 3 ' )
 2160 FORMAT( ' ** Exit from INTERPRET_gpsmps - element ', A10, ' variable ',  &
              A10, ' not set ' )
 2210 FORMAT( ' ** Exit from INTERPRET_gpsmps -',                              &
              ' groups and variables sections mixed')
 2250 FORMAT( ' ** Exit from INTERPRET_gpsmps - no group-type arg. given ' )
 2280 FORMAT( ' ** Exit from INTERPRET_gpsmps - element ', A10, ' parameter ', &
              A10, ' not set ' )
 2340 FORMAT( ' ** Exit from INTERPRET_gpsmps - group ', A10, ' parameter ',   &
              A10, ' not set ' )
 2380 FORMAT( ' ** Exit from INTERPRET_gpsmps - do loop not completed ' )
 2740 FORMAT( ' ** Exit from INTERPRET_gpsmps - data file empty ' )
 2750 FORMAT( ' ** Exit from INTERPRET_gpsmps - data file incomplete.',        &
              ' No ENDATA card ' )
 2760 FORMAT( ' ** Exit from INTERPRET_gpsmps - #internal vars >= #elementals' )
 2960 FORMAT( /, ' From within do loop ending on line ', i5,                   &
              ', current line is ', /,                                         &
              2X, A2, 1X, A10, A10, 1P, D12.4, 3X, A10, D12.4 )
 2970 FORMAT( ' Line ', i5, 4X, A160 )
 2980 FORMAT( ' Line ', i5, '.', i2, 1X, A65 )
 2990 FORMAT( ' Line ', i5, 4X, A65 )
 3000 FORMAT( /, ' Row names ', /, ' --------- ', /, 8( 1X, A8 ) )
 3010 FORMAT( /, ' Column names ', /, ' ------------', /, 8( 1X, A8 ) )
 3020 FORMAT( /, 3('  Col   Row    Value  '),                                  &
              /, 3('  ---   ---    -----  '),                                  &
              /, ( 3( 2I5, 1P, D12.4 ) ) )
 3030 FORMAT( /, 2(' No. var.  Lower bnd   upper bnd '),                       &
              /, 2(' --- ----  ---------   --------- '),                       &
              /,  ( 2( I3, I6, 1P, 2E12.4 ) ) )
 3040 FORMAT( /, ' Row scaling ', /, ( 1P, D12.4 ) )
 3050 FORMAT( /, ' Column scaling ', /, ( 1P, D12.4 ) )
 3060 FORMAT( /, '    Element type No. el. vars. No. in. vars.',               &
                 ' No. parameters ',                                           &
              /, '    ------------ ------------- ------------- ',              &
                 ' -------------- ', /, ( 5X, A10, I12, 2( 2X, I12 ) ) )
 3070 FORMAT( /, ' Group      Gr. type    Element   El. type',                 &
                 '     Variables ',                                            &
              /, ' -----      --------    -------   --------',                 &
                 '     --------- ' )
 3080 FORMAT( 1X, A10, 1X, A10, 2X, A10, A10, 4X, 5A10,                        &
              /, ( 48X, 5A10 ) )
 3090 FORMAT( 1X, A10, 1X, A10, 2X, '   -    ' )
 3100 FORMAT( /, 2(' No. var.  Start point '),                                 &
              /, 2(' --- ----  ----------- '),                                 &
              /,  ( 2( i3, i6, 1P, D12.4 ) ) )
 3110 FORMAT( /, '    Group type   Argument   No. parameters',                 &
              /, '    ----------   --------   -------------- ',                &
              /, ( 5X, 2A10, I14 ) )
 4000 FORMAT( ' Int. par. num. ', i5, ' Name = ', A10, ' Value = ', I12)
 4010 FORMAT( ' Level-', i1, ' Instruction ', i4, ' Starting do-loop ' )
 4020 FORMAT( ' Level-', i1, ' Instruction ', i4, ' Ending do-loop ' )
 4030 FORMAT( ' Level-', i1, ' Instruction ', i4,                              &
              ' Incrementing do-loop ' )
 4100 FORMAT( ' Real par. num. ', i5, ' Name = ', A10,' Value = ',             &
                1P, D12.4)
 5000 FORMAT( /, ' Level-', i1, ' loop index ', A10, ' = ', I12 )
 5010 FORMAT( ' Level-', i1, ' instruction ', i3,                              &
              ' Index ', A10, ' = ', I12 )
 5020 FORMAT( ' Level-', i1, ' instruction ', i3,                              &
              ' Index ', A10, ' = ', 1P, D12.4 )
 5060 FORMAT( ' Level-', i1, ' instruction ', i3, ' Set line ', /,             &
              '    field1 = ', A12, ' field2 = ', A10, ' field3 = ',           &
              A10, /, '    value4 = ', 1P, D12.4, ' field5 = ', A10,           &
              ' value6 = ', 1P, D12.4 )

!  end of subroutine INTERPRET_gpsmps

      END SUBROUTINE INTERPRET_gpsmps

!-*-*-*-*-*-*- S I F D E C O D E   S G R P 1   S U B R O U T I N E -*-*-*-*-*-*-

      SUBROUTINE SGRP1( ng, nobj, novals, len2_idrows, IDROWS,                 &
                        len_gstate, GSTATE, len_gtype, GTYPE,                  &
                        len2_rdrows, RDROWS, len_rscale, RSCALE,               &
                        field1, field2, field3, value4, field5, value6,        &
                        len_gnames, GNAMES, len_onames, ONAMES,                &
                        length, TABLE, KEY, INLIST, out, status )
      INTEGER :: ng, nobj, novals, length, out, status
      INTEGER :: len2_idrows, len_gstate, len_gtype
      INTEGER :: len2_rdrows, len_rscale, len_onames, len_gnames
      REAL ( KIND = wp ) :: value4, value6
      CHARACTER ( LEN = 2 ) :: field1
      CHARACTER ( LEN = 10 ) :: field2, field3, field5
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: GSTATE, GTYPE
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TABLE, INLIST
      INTEGER, ALLOCATABLE, DIMENSION( : , : ) :: IDROWS
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: RSCALE
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : , : ) :: RDROWS
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: GNAMES, ONAMES
      CHARACTER ( LEN = 12 ), ALLOCATABLE, DIMENSION( : ) :: KEY

!  --------------------------------------------------------
!  indicator card is groups/rows/constraints

!  the groups section appears before the variables section
!  --------------------------------------------------------

!  local variables

      INTEGER :: i, ifree, ifield, is, j
      INTEGER :: used_length, new_length, min_length, alloc_status
      INTEGER :: used_length2, new_length2, min_length2
      CHARACTER ( LEN = 12 ) :: field
      CHARACTER ( LEN = 24 ) :: bad_alloc

!  ignore 'marker' cards

      IF ( field3 == '''MARKER''  ' ) RETURN

!  find a place to insert the new group name in the hash-table

      CALL HASH_enlarge_and_insert( length, 12, field2 // 'GR',                &
                                    TABLE, KEY, INLIST, ifree )
      IF ( ifree <= 0 ) THEN
        IF ( ifree == 0 ) THEN
          status = - 1
          RETURN
        END IF

!  the group has appeared before as the j-th in the list

        j = INLIST( - ifree )

!  mark any objective function rows as special

      ELSE
        IF ( field1 == 'N ' .OR. field1 == ' N' .OR.                           &
             field1 == 'DN' .OR. field1 == 'XN' .OR. field1 == 'ZN' ) THEN
          nobj = nobj + 1
          IF ( nobj > len_onames ) THEN
            used_length = nobj - 1 ; min_length = nobj
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( ONAMES, len_gnames, used_length, new_length,    &
                               min_length, buffer, status, alloc_status,       &
                               'ONAMES' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'ONAMES' ; status = - 5 ; GO TO 980 ; END IF
            len_onames = new_length
          END IF
          ONAMES ( nobj ) = field2
        END IF

!  the group is the ng-th encountered; ensure there is sufficient space

        ng = ng + 1
        IF ( ng > len_gstate ) THEN
          used_length = ng - 1 ; min_length = ng
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( GSTATE, len_gstate, used_length, new_length,      &
                             min_length, buffer, status, alloc_status,         &
                             'GSTATE' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'GSTATE' ; status = - 6 ; GO TO 980 ; END IF
          len_gstate = new_length
        END IF

        IF ( ng > len_gtype ) THEN
          used_length = ng - 1 ; min_length = ng
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( GTYPE, len_gtype, used_length, new_length,        &
                             min_length, buffer, status, alloc_status,         &
                             'GTYPE' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'GTYPE' ; status = - 6 ; GO TO 980 ; END IF
          len_gtype = new_length
        END IF

        IF ( ng > len_gnames ) THEN
          used_length = ng - 1 ; min_length = ng
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( GNAMES, len_gnames, used_length, new_length,      &
                             min_length, buffer, status, alloc_status,         &
                             'GNAMES' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'GNAMES' ; status = - 6 ; GO TO 980 ; END IF
          len_gnames = new_length
        END IF

        IF ( ng > len_rscale ) THEN
          used_length = ng - 1 ; min_length = ng
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( RSCALE, len_rscale, used_length, new_length,      &
                             min_length, buffer, status, alloc_status,         &
                             'RSCALE' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'RSCALE' ; status = - 6 ; GO TO 980 ; END IF
          len_rscale = new_length
        END IF

        IF ( ng > len2_idrows ) THEN
          used_length = 2 ; new_length = 2 ; min_length = 2
          used_length2 = ng - 1 ; min_length2 = ng
          new_length2 = increase_n * min_length2 / increase_d + 1
          CALL EXTEND_array( IDROWS, 2, len2_idrows, used_length,              &
                             used_length2, new_length, new_length2,            &
                             min_length, min_length2, buffer,                  &
                             status, alloc_status, 'IDROWS' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'IDROWS' ; status = - 6 ; GO TO 980 ; END IF
          len2_idrows = new_length2
        END IF

        IF ( ng > len2_rdrows ) THEN
          used_length = 2 ; new_length = 2 ; min_length = 2
          used_length2 = ng - 1 ; min_length2 = ng
          new_length2 = increase_n * min_length2 / increase_d + 1
          CALL EXTEND_array( RDROWS, 2, len2_rdrows, used_length,              &
                             used_length2, new_length, new_length2,            &
                             min_length, min_length2, buffer,                  &
                             status, alloc_status, 'RDROWS' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'RDROWS' ; status = - 6 ; GO TO 980 ; END IF
          len2_rdrows = new_length2
        END IF

!  record the position of the new group in the table, record its
!  name and initialise its type as trivial

        j = ng
        INLIST( ifree ) = ng
        GNAMES( ng ) = field2
        GTYPE( ng ) = - 1
        RSCALE( ng ) = one
        IDROWS( 1, ng ) = 0
        IDROWS( 2, ng ) = 0
        RDROWS( 1, ng ) = zero
        RDROWS( 2, ng ) = zero
        is = 0

!  record the status, GSTATE, of the group. GSTATE( ng ) = :
!  1 the group is an objective function type
!  2 the group is an equality function type
!  3 the group is a less-than-or-equal-to type
!  4 the group is a greater-than-or-equal-to type
!  5 the group is of d-type and an objective function type
!  6 the group is of d-type and an equality function type
!  7 the group is of d-type and a less-than-or-equal-to type
!  8 the group is of d-type and a greater-than-or-equal-to type

        IF ( field1 == 'N ' .OR. field1 == ' N' .OR.                           &
             field1 == 'XN' .OR. field1 == 'ZN' ) is = 1
        IF ( field1 == 'E ' .OR. field1 == ' E' .OR.                           &
             field1 == 'XE' .OR. field1 == 'ZE' ) is = 2
        IF ( field1 == 'L ' .OR. field1 == ' L' .OR.                           &
             field1 == 'XL' .OR. field1 == 'ZL' ) is = 3
        IF ( field1 == 'G ' .OR. field1 == ' G' .OR.                           &
             field1 == 'XG' .OR. field1 == 'ZG' ) is = 4
        IF ( field1 == 'DN' ) is = 5
        IF ( field1 == 'DE' ) is = 6
        IF ( field1 == 'DL' ) is = 7
        IF ( field1 == 'DG' ) is = 8
        IF ( is == 0 ) THEN
          status = 10
          IF ( out > 0 ) WRITE( out, 2100 ) field1
          RETURN
        ENDIF
        GSTATE( ng ) = is
      END IF

!  include group scale factors

      IF ( field3 == '''SCALE''   ' .OR. field3 == ' ''SCALE''  ' )            &
        RSCALE( j ) = value4

!  mark 'd'-type groups and record their multiplicative factors
!  idrows(1, ), idrows(2, ) give the numbers of the groups referred
!  to by the new d-type group and rdrows(1, ) and rdrows(2, ) give
!  the multiplicative factors

      IF ( field1( 1 : 1 ) == 'D' ) THEN
        IF ( GSTATE( j ) <= 4 ) THEN
          status = 22
          IF ( out > 0 ) WRITE( out, 2220 )
          RETURN
        END IF
        DO i = 1, 2
          IF ( i > novals ) THEN
            IDROWS( i, j ) = 1
            RDROWS( i, j ) = zero

!  check that the groups referred to when constructing a d-type
!  group already exist

          ELSE
            IF ( i == 1 ) THEN
              field = field3 // 'GR'
            ELSE
              field = field5 // 'GR'
            END IF
            CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
            IF ( ifield > 0 ) THEN
              IDROWS( i, j ) = INLIST( ifield )
              IF ( i == 1 ) THEN
                RDROWS( i, j ) = value4
              ELSE
                RDROWS( i, j ) = value6
              END IF
            ELSE
              status = 4
              IF ( out > 0 ) WRITE( out, 2040 ) FIELD( 1 : 10 )
              RETURN
            END IF
          END IF
        END DO
      END IF
      status = 0
      RETURN

!  allocation errors

  980 CONTINUE
      WRITE( out, "( ' ** Message from INTERPRET_gpsmps-',                     &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  non-executable statements

 2040 FORMAT( ' ** Exit from INTERPRET_gpsmps - group/row name not',           &
              ' recognised: name is ', A10 )
 2100 FORMAT( ' ** Exit from INTERPRET_gpsmps - field 1 ', A2,                 &
              '  not recognised in GROUPS section ' )
 2220 FORMAT( ' ** Exit from INTERPRET_gpsmps -',                              &
              ' conflicting field 1 on GROUPS card')

!  end of subroutine SGRP1

      END SUBROUTINE SGRP1

!-*-*-*-*-*-*- S I F D E C O D E   S G R P 2   S U B R O U T I N E -*-*-*-*-*-*-

      SUBROUTINE SGRP2( ng, nnza, nobj, novals, len2_idrows, IDROWS,           &
                        len_gstate, GSTATE, len_gtype, GTYPE,                  &
                        len_a, A_row, A_col, A_val,                            &
                        len2_rdrows, RDROWS, len_rscale, RSCALE,               &
                        field1, field2, field3, value4, field5, value6,        &
                        len_gnames, GNAMES, len_onames, ONAMES,                &
                        length, TABLE, KEY, INLIST, out, status )
      INTEGER :: ng, nnza, nobj, novals, length, out, status
      INTEGER :: len2_idrows, len_gstate, len_gtype, len_a
      INTEGER :: len2_rdrows, len_rscale, len_onames, len_gnames
      REAL ( KIND = wp ) :: value4, value6
      CHARACTER ( LEN = 2 ) :: field1
      CHARACTER ( LEN = 10 ) :: field2, field3, field5
      INTEGER, ALLOCATABLE, DIMENSION( : , : ) :: IDROWS
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: GSTATE, GTYPE
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: A_row, A_col
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TABLE, INLIST
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: A_val, RSCALE
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : , : ) :: RDROWS
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: GNAMES, ONAMES
      CHARACTER ( LEN = 12 ), ALLOCATABLE, DIMENSION( : ) :: KEY

!  ------------------------------------------------------
!  indicator card is groups/rows/constraints

!  the groups section appears after the variables section
!  ------------------------------------------------------

!  local variables

      INTEGER :: i, ifree, ifield, is, j
      INTEGER :: used_length, new_length, min_length, alloc_status
      INTEGER :: used_length2, new_length2, min_length2
      CHARACTER ( LEN = 12 ) :: field
      CHARACTER ( LEN = 24 ) :: bad_alloc

!  ignore 'marker' cards

      IF ( field3 == '''MARKER''  ' ) RETURN

!  find a place to insert the new group name in the hash-table

      CALL HASH_enlarge_and_insert( length, 12, field2 // 'GR',                &
                                    TABLE, KEY, INLIST, ifree )

!  the name already exists. it is the j-th name in the list

      IF ( ifree <= 0 ) THEN
        IF ( ifree == 0 ) THEN
          status = - 1
          RETURN
        END IF
        j = INLIST( - ifree )

!  mark any objective function rows as special

      ELSE
        IF ( field1 == 'N ' .OR. field1 == ' N' .OR.                           &
             field1 == 'DN' .OR. field1 == 'XN' .OR. field1 == 'ZN' ) THEN
          nobj = nobj + 1
          IF ( nobj > len_onames ) THEN
            used_length = nobj - 1 ; min_length = nobj
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( ONAMES, len_gnames, used_length, new_length,    &
                               min_length, buffer, status, alloc_status,       &
                               'ONAMES' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'ONAMES' ; status = - 5 ; GO TO 980 ; END IF
            len_onames = new_length
          END IF
          ONAMES ( nobj ) = field2
        END IF

!  the group is the ng-th encountered; ensure there is sufficient space

        ng = ng + 1
        IF ( ng > len_gstate ) THEN
          used_length = ng - 1 ; min_length = ng
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( GSTATE, len_gstate, used_length, new_length,      &
                             min_length, buffer, status, alloc_status,         &
                             'GSTATE' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'GSTATE' ; status = - 6 ; GO TO 980 ; END IF
          len_gstate = new_length
        END IF

        IF ( ng > len_gtype ) THEN
          used_length = ng - 1 ; min_length = ng
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( GTYPE, len_gtype, used_length, new_length,        &
                             min_length, buffer, status, alloc_status,         &
                             'GTYPE' )
          IF ( status /= 0 ) THEN
             bad_alloc = 'GTYPE' ; status = - 6 ; GO TO 980 ; END IF
          len_gtype = new_length
        END IF

        IF ( ng > len_gnames ) THEN
          used_length = ng - 1 ; min_length = ng
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( GNAMES, len_gnames, used_length, new_length,      &
                             min_length, buffer, status, alloc_status,         &
                             'GNAMES' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'GNAMES' ; status = - 6 ; GO TO 980 ; END IF
          len_gnames = new_length
        END IF

        IF ( ng > len_rscale ) THEN
          used_length = ng - 1 ; min_length = ng
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( RSCALE, len_rscale, used_length, new_length,      &
                             min_length, buffer, status, alloc_status,         &
                             'RSCALE' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'RSCALE' ; status = - 6 ; GO TO 980 ; END IF
          len_rscale = new_length
        END IF

        IF ( ng > len2_idrows ) THEN
          used_length = 2 ; new_length = 2 ;  min_length = 2
          used_length2 = ng - 1 ; min_length2 = ng
          new_length2 = increase_n * min_length2 / increase_d + 1
          CALL EXTEND_array( IDROWS, 2, len2_idrows, used_length,              &
                             used_length2, new_length, new_length2,            &
                             min_length, min_length2, buffer,                  &
                             status, alloc_status, 'IDROWS' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'IDROWS' ; status = - 6 ; GO TO 980 ; END IF
          len2_idrows = new_length2
        END IF

        IF ( ng > len2_rdrows ) THEN
          used_length = 2 ; new_length = 2 ;  min_length = 2
          used_length2 = ng - 1 ; min_length2 = ng
          new_length2 = increase_n * min_length2 / increase_d + 1
          CALL EXTEND_array( RDROWS, 2, len2_rdrows, used_length,              &
                             used_length2, new_length, new_length2,            &
                             min_length, min_length2, buffer,                  &
                             status, alloc_status, 'RDROWS' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'RDROWS' ; status = - 6 ; GO TO 980 ; END IF
          len2_rdrows = new_length2
        END IF

!  record the position of the new group in the table, record its
!  name and initialise its type as trivial

        j = ng
        INLIST( ifree ) = ng
        GNAMES( ng ) = field2
        GTYPE( ng ) = - 1
        RSCALE( ng ) = one
        IDROWS( 1, ng ) = 0
        IDROWS( 2, ng ) = 0
        RDROWS( 1, ng ) = zero
        RDROWS( 2, ng ) = zero
        is = 0

!  record the status, GSTATE, of the group. GSTATE( ng ) = :
!  1 the group is an objective function type
!  2 the group is an equality function type
!  3 the group is a less-than-or-equal-to type
!  4 the group is a greater-than-or-equal-to type
!  5 the group is of d-type and an objective function type
!  6 the group is of d-type and an equality function type
!  7 the group is of d-type and a less-than-or-equal-to type
!  8 the group is of d-type and a greater-than-or-equal-to type

        IF ( field1 == 'N ' .OR. field1 == ' N' .OR.                           &
             field1 == 'XN' .OR. field1 == 'ZN' ) is = 1
        IF ( field1 == 'E ' .OR. field1 == ' E' .OR.                           &
             field1 == 'XE' .OR. field1 == 'ZE' ) is = 2
        IF ( field1 == 'L ' .OR. field1 == ' L' .OR.                           &
             field1 == 'XL' .OR. field1 == 'ZL' ) is = 3
        IF ( field1 == 'G ' .OR. field1 == ' G' .OR.                           &
             field1 == 'XG' .OR. field1 == 'ZG' ) is = 4
        IF ( field1 == 'DN' ) is = 5
        IF ( field1 == 'DE' ) is = 6
        IF ( field1 == 'DL' ) is = 7
        IF ( field1 == 'DG' ) is = 8
        IF ( is == 0 ) THEN
          status = 10
          IF ( out > 0 ) WRITE( out, 2100 ) field1
          RETURN
        ENDIF
        GSTATE( ng ) = is
      END IF

!  include group scale factors

      IF ( field3 == '''SCALE''   ' .OR. field3 == ' ''SCALE''  ' ) THEN
        RSCALE( j ) = value4
        RETURN
      END IF

!  mark 'd'-type groups and record their multiplicative factors
!  idrows(1, ), idrows(2, ) give the numbers of the groups referred
!  to by the new d-type group and rdrows(1, ) and rdrows(2, ) give
!  the multiplicative factors

      IF ( field1( 1 : 1 ) == 'D' ) THEN
        IF ( GSTATE( j ) <= 4 ) THEN
          status = 22
          IF ( out > 0 ) WRITE( out, 2220 )
          RETURN
        END IF
        DO i = 1, 2
          IF ( i > novals ) THEN
            IDROWS( i, j ) = 1
            RDROWS( i, j ) = zero

!  check that the groups referred to when constructing a d-type
!  group already exist

          ELSE
            IF ( i == 1 ) THEN
              field = field3 // 'GR'
            ELSE
              field = field5 // 'GR'
            END IF
            CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
            IF ( ifield > 0 ) THEN
              IDROWS( i, j ) = INLIST( ifield )
              IF ( i == 1 ) THEN
                RDROWS( i, j ) = value4
              ELSE
                RDROWS( i, j ) = value6
              END IF
            ELSE
              status = 4
              IF ( out > 0 ) WRITE( out, 2040 ) FIELD( 1 : 10 )
              RETURN
            END IF
          END IF
        END DO
      ELSE
        IF ( novals > 0 ) THEN

!  check that data has not been specified for a 'D'-group

          IF ( GSTATE( j ) >= 5 ) THEN
            status = 8
            IF ( out > 0 ) WRITE( out, 2080 )
            RETURN
          END IF

!  entries for the linear element for group j are to be specified. Find the
!  variable numbers for the input entries

          DO i = 1, novals
            IF ( i == 1 ) THEN
               field = field3 // 'VA'
            ELSE
               field = field5 // 'VA'
            END IF
            CALL HASH_search( length, 12, field, TABLE, KEY, ifield )

!  the nnza-th nonzero has been specified

            IF ( ifield > 0 ) THEN
              nnza = nnza + 1
              IF ( nnza >= len_a ) THEN
                used_length = nnza - 1 ; min_length = nnza
                new_length = increase_n * min_length / increase_d + 1
                CALL EXTEND_array( A_row, len_a, used_length, new_length,      &
                                   min_length, buffer, status, alloc_status,   &
                                   'A_row' )
                IF ( status /= 0 ) THEN
                  bad_alloc = 'A_row' ; status = - 2 ; GO TO 980 ; END IF

                used_length = nnza - 1 ; min_length = nnza
                new_length = increase_n * min_length / increase_d + 1
                CALL EXTEND_array( A_col, len_a, used_length, new_length,      &
                                   min_length, buffer, status, alloc_status,   &
                                   'A_col' )
                IF ( status /= 0 ) THEN
                  bad_alloc = 'A_col' ; status = - 2 ; GO TO 980 ; END IF

                used_length = nnza - 1 ; min_length = nnza
                new_length = increase_n * min_length / increase_d + 1
                CALL EXTEND_array( A_val, len_a, used_length, new_length,      &
                                   min_length, buffer, status, alloc_status,   &
                                   'A_val' )
                IF ( status /= 0 ) THEN
                  bad_alloc = 'A_val' ; status = - 2 ; GO TO 980 ; END IF
                len_a = new_length
              END IF

!  the nonzero is for group A_row() and variable A_col() with value A_val()

              A_row( nnza ) = j
              A_col( nnza ) = INLIST( ifield )
              IF ( i == 1 ) THEN
                A_val( nnza ) = value4
              ELSE
                A_val( nnza ) = value6
              END IF
            ELSE

!  the variable name is unknown

              status = 5
              IF ( out > 0 ) WRITE( out, 2050 ) FIELD( 1 : 10 )
              RETURN
            END IF
          END DO
        END IF
      END IF
      status = 0
      RETURN

!  allocation errors

  980 CONTINUE
      WRITE( out, "( ' ** Message from INTERPRET_gpsmps-',                     &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  non-executable statements

 2040 FORMAT( ' ** Exit from INTERPRET_gpsmps - group/row name not',           &
              ' recognised: name is ', A10 )
 2050 FORMAT( ' ** Exit from INTERPRET_gpsmps - column/var name not',          &
              ' recognised: name is ', A10 )
 2080 FORMAT( ' ** Exit from INTERPRET_gpsmps - ''D'' group/row contains data' )
 2100 FORMAT( ' ** Exit from INTERPRET_gpsmps - field 1 ', A2,                 &
              '  not recognised in GROUPS section ' )
 2220 FORMAT( ' ** Exit from INTERPRET_gpsmps -',                              &
              ' conflicting field 1 on GROUPS card')

!  end of subroutine SGRP2

      END SUBROUTINE SGRP2

!-*-*-*-*-*-*- S I F D E C O D E   S V A R 1   S U B R O U T I N E -*-*-*-*-*-*-

      SUBROUTINE SVAR1( nvar, colfie, len_typev, TYPEV, len_cscale, CSCALE,    &
                        field2, field3, value4, len_vnames, VNAMES,            &
                        length, TABLE, KEY, INLIST, out, status )
      INTEGER :: out, status, length, nvar
      INTEGER :: len_typev, len_cscale, len_vnames
      REAL ( KIND = wp ) :: value4
      CHARACTER ( LEN = 2 ) :: colfie
      CHARACTER ( LEN = 10 ) :: field2, field3
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TABLE, INLIST
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TYPEV
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: CSCALE
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: VNAMES
      CHARACTER ( LEN = 12 ), ALLOCATABLE, DIMENSION( : ) :: KEY

!  -----------------------------------------------------------------
!  indicator card is columns/variables, constants/rhs/rhs' or ranges

!  the variables section before the groups section
!  -----------------------------------------------------------------

!  local variables

      INTEGER :: ifree, j
      INTEGER :: used_length, new_length, min_length, alloc_status
      CHARACTER ( LEN = 24 ) :: bad_alloc
!  ignore 'marker' cards

      IF ( field3 == '''MARKER''  ' ) RETURN

!  find a place to insert the new variable name in the hash-table
!  if it has not already been entered

      CALL HASH_enlarge_and_insert( length, 12, field2 // colfie,              &
                                    TABLE, KEY, INLIST, ifree )

!  the variable name already exists. it is the j-th named variable

      IF ( ifree <= 0 ) THEN
        IF ( ifree == 0 ) THEN
          status = - 1
          RETURN
        END IF
        j = INLIST( - ifree )

!  the variable name is new. the variable is the nvar-th encountered and
!  it occurs in position ifree in the table

      ELSE
        nvar = nvar + 1
        IF ( nvar > len_vnames ) THEN
          used_length = nvar - 1 ; min_length = nvar
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( VNAMES, len_vnames, used_length, new_length,      &
                             min_length, buffer, status, alloc_status,         &
                             'VNAMES' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'VNAMES' ; status = - 7 ; GO TO 980 ; END IF
          len_vnames = new_length
        END IF

        IF ( nvar > len_cscale ) THEN
          used_length = nvar - 1 ; min_length = nvar
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( CSCALE, len_cscale, used_length, new_length,      &
                             min_length, buffer, status, alloc_status,         &
                             'CSCALE' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'CSCALE' ; status = - 7 ; GO TO 980 ; END IF
          len_cscale = new_length
        END IF

        IF ( nvar > len_typev ) THEN
          used_length = nvar - 1 ; min_length = nvar
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( TYPEV, len_typev, used_length, new_length,        &
                             min_length, buffer, status, alloc_status,         &
                             'TYPEV' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'TYPEV' ; status = - 7 ; GO TO 980 ; END IF
          len_typev = new_length
        END IF
        j = nvar
        INLIST( ifree ) = nvar
        VNAMES( nvar ) = field2
        TYPEV( nvar ) = 0
        CSCALE( nvar ) = one
      END IF

!  include column scale factors if they are allowed

      IF ( field3 == '''SCALE''   ' .OR. field3 == ' ''SCALE''  ' )            &
        CSCALE( j ) = value4

!  mark zero-one and integer variables

      IF ( field3 == '''ZERO-ONE''' ) TYPEV( j ) = 1
      IF ( field3 == '''INTEGER'' ' ) TYPEV( j ) = 2
      status = 0
      RETURN

!  allocation errors

  980 CONTINUE
      WRITE( out, "( ' ** Message from INTERPRET_gpsmps-',                     &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  end of subroutine SVAR1

      END SUBROUTINE SVAR1

!-*-*-*-*-*-*- S I F D E C O D E   S V A R 2   S U B R O U T I N E -*-*-*-*-*-*-

      SUBROUTINE SVAR2( nnza, nvar, novals, nrival, varsec, colfie,            &
                        len_gstate, GSTATE, len_typev, TYPEV,                  &
                        len_a, A_row, A_col, A_val,                            &
                        len_cscale, CSCALE, RIVAL, len_default, DEFAULT,       &
                        field1, field2, field3, value4, field5, value6,        &
                        len_vnames, VNAMES,                                    &
                        length, TABLE, KEY, INLIST, out, status )
      INTEGER :: out, status, length
      INTEGER :: len_gstate, nnza, nvar, novals, nrival
      INTEGER :: len_a, len_default, len_typev, len_cscale, len_vnames
      REAL ( KIND = wp ) :: value4, value6
      LOGICAL :: varsec
      CHARACTER ( LEN = 2 ) :: field1, colfie
      CHARACTER ( LEN = 10 ) :: field2, field3, field5
      INTEGER, DIMENSION( len_gstate ) :: GSTATE
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: A_row, A_col
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TABLE, INLIST
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TYPEV
      REAL ( KIND = wp ), DIMENSION( nrival ) :: RIVAL
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: A_val, DEFAULT, CSCALE
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: VNAMES
      CHARACTER ( LEN = 12 ), ALLOCATABLE, DIMENSION( : ) :: KEY

!  -----------------------------------------------------------------
!  indicator card is columns/variables, constants/rhs/rhs' or ranges

!  the variables section appears after the groups section
!  -----------------------------------------------------------------

!  local variables

      INTEGER :: i, ifree, ifield, j
      INTEGER :: used_length, new_length, min_length, alloc_status
      CHARACTER ( LEN = 12 ) :: field
      CHARACTER ( LEN = 24 ) :: bad_alloc

!  ignore 'marker' cards

      IF ( field3 == '''MARKER''  ' ) RETURN

!  find a place to insert the new variable name in the hash-table
!  if it has not already been entered

      CALL HASH_enlarge_and_insert( length, 12, field2 // colfie,              &
                                    TABLE, KEY, INLIST, ifree )

!  the variable name already exists. it is the j-th named variable

      IF ( ifree <= 0 ) THEN
        IF ( ifree == 0 ) THEN
          status = - 1
          RETURN
        END IF
        j = INLIST( - ifree )

!  the variable name is new. the variable is the nvar-th encountered and
!  it occurs in position ifree in the table

      ELSE
        nvar = nvar + 1
        IF ( nvar > len_vnames ) THEN
          used_length = nvar - 1 ; min_length = nvar
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( VNAMES, len_vnames, used_length, new_length,      &
                             min_length, buffer, status, alloc_status,         &
                             'VNAMES' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'VNAMES' ; status = - 7 ; GO TO 980 ; END IF
          len_vnames = new_length
        END IF

        IF ( nvar > len_cscale ) THEN
          used_length = nvar - 1 ; min_length = nvar
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( CSCALE, len_cscale, used_length, new_length,      &
                             min_length, buffer, status, alloc_status,         &
                             'CSCALE' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'CSCALE' ; status = - 7 ; GO TO 980 ; END IF
          len_cscale = new_length
        END IF

        IF ( nvar > len_typev ) THEN
          used_length = nvar - 1 ; min_length = nvar
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( TYPEV, len_typev, used_length, new_length,        &
                             min_length, buffer, status, alloc_status,         &
                             'TYPEV' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'TYPEV' ; status = - 7 ; GO TO 980 ; END IF
          len_typev = new_length
        END IF

        IF ( nvar > len_default ) THEN
          used_length = nvar - 1 ; min_length = nvar
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( DEFAULT, len_default, used_length, new_length,    &
                             min_length, buffer, status, alloc_status,         &
                             'DEFAULT' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'DEFAULT' ; status = - 7 ; GO TO 980 ; END IF
          len_default = new_length
        END IF
        j = nvar
        INLIST( ifree ) = nvar
        VNAMES( nvar ) = field2
        TYPEV( nvar ) = 0
        CSCALE( nvar ) = one
        IF ( colfie == 'CO' ) DEFAULT( nvar ) = zero
        IF ( colfie == 'RA' ) DEFAULT( nvar ) = biginf
      END IF

!  include column scale factors if they are allowed

      IF ( field3 == '''SCALE''   ' .OR. field3 == ' ''SCALE''  ' ) THEN
        IF ( .NOT. varsec ) THEN
          status = 7
          IF ( out > 0 ) WRITE( out, 2070 )
        ELSE
          CSCALE( j ) = value4
        END IF
        RETURN
      END IF

!  mark zero-one and integer variables

      IF ( field3 == '''ZERO-ONE''' ) THEN
        IF ( .NOT. varsec ) THEN
          status = 7
          IF ( out > 0 ) WRITE( out, 2060 )
        ELSE
           TYPEV( j ) = 1
        END IF
        RETURN
      END IF
      IF ( field3 == '''INTEGER'' ' ) THEN
        IF ( .NOT. varsec ) THEN
          status = 7
          IF ( out > 0 ) WRITE( out, 2060 )
        ELSE
          TYPEV( j ) = 2
        END IF
        RETURN
      END IF

!  a nontrivial default value has been specified for a constant or range vector

      IF ( field3 == '''DEFAULT'' ' .AND. .NOT. varsec ) THEN
        IF ( field1 == 'Z ' ) THEN
          CALL HASH_search( length, 12,  FIELD5( 1 : 10 ) // 'RI',             &
                            TABLE, KEY, ifield )
          IF ( ifield <= 0 ) THEN
            status = 3
            IF ( out > 0 ) WRITE( out, 2030 ) FIELD5( 1 : 10 )
            RETURN
          END IF
          value4 = RIVAL( INLIST( ifield ) )
        END IF
        DEFAULT( nvar ) = value4

!  find the group numbers for the input nonzero(s)

      ELSE
        IF ( novals > 0 ) THEN
          DO i = 1, novals
            IF ( i == 1 ) THEN
              field = field3 // 'GR'
            ELSE
              field = field5 // 'GR'
            END IF
            CALL HASH_search( length, 12, field, TABLE, KEY, ifield )

!  check that data has not been specified for a 'd'-group

            IF ( ifield > 0 ) THEN
              IF ( GSTATE( INLIST( ifield ) ) >= 5 .AND. varsec ) THEN
                status = 8
                IF ( out > 0 ) WRITE( out, 2080 )
                RETURN
              END IF

!  the nnza-th nonzero has been specified

              nnza = nnza + 1
              IF ( nnza >= len_a ) THEN
                used_length = nnza - 1 ; min_length = nnza
                new_length = increase_n * min_length / increase_d + 1
                CALL EXTEND_array( A_row, len_a, used_length, new_length,      &
                                   min_length, buffer, status, alloc_status,   &
                                   'A_row' )
                IF ( status /= 0 ) THEN
                  bad_alloc = 'A_row' ; status = - 2 ; GO TO 980 ; END IF

                used_length = nnza - 1 ; min_length = nnza
                new_length = increase_n * min_length / increase_d + 1
                CALL EXTEND_array( A_col, len_a, used_length, new_length,      &
                                   min_length, buffer, status, alloc_status,   &
                                   'A_col' )
                IF ( status /= 0 ) THEN
                  bad_alloc = 'A_col' ; status = - 2 ; GO TO 980 ; END IF

                used_length = nnza - 1 ; min_length = nnza
                new_length = increase_n * min_length / increase_d + 1
                CALL EXTEND_array( A_val, len_a, used_length, new_length,      &
                                   min_length, buffer, status, alloc_status,   &
                                   'A_val' )
                IF ( status /= 0 ) THEN
                  bad_alloc = 'A_val' ; status = - 2 ; GO TO 980 ; END IF
                len_a = new_length
              END IF

!  the nonzero is for group A_row() and variable A_col() with value A_val()

              A_row( nnza ) = INLIST( ifield )
              A_col( nnza ) = j
              IF ( i == 1 ) THEN
                A_val( nnza ) = value4
              ELSE
                A_val( nnza ) = value6
              END IF
            ELSE

!  the group name is unknown

              status = 4
              IF ( out > 0 ) WRITE( out, 2040 ) FIELD( 1 : 10 )
              RETURN
            END IF
          END DO
        END IF
      END IF
      status = 0
      RETURN

!  allocation errors

  980 CONTINUE
      WRITE( out, "( ' ** Message from INTERPRET_gpsmps-',                     &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  non-executable statements

 2030 FORMAT( ' ** Exit from INTERPRET_gpsmps - index parameter name ', A10,   &
              ' not recognised ' )
 2040 FORMAT( ' ** Exit from INTERPRET_gpsmps - group/row name not',           &
              ' recognised: name is ', A10 )
 2060 FORMAT( ' ** Exit from INTERPRET_gpsmps - type given for RHS or RANGES' )
 2070 FORMAT( ' ** Exit from INTERPRET_gpsmps - scale given for RHS or RANGES' )
 2080 FORMAT( ' ** Exit from INTERPRET_gpsmps - ''D'' group/row contains data' )

!  end of subroutine SVAR2

      END SUBROUTINE SVAR2

!-*-*-*-*-*- S I F D E C O D E   S B O U N D    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE SBOUND( nlvars, nbnd, ncol, nrival, defaut,                   &
                         len1_blu, len2_blu, B_l, B_u, RIVAL,                  &
                         field1, field2, field3, value4, field5,               &
                         len_bnames, BNAMES,                                   &
                         len_blu_default, B_l_default, B_u_default,            &
                         length, TABLE, KEY, INLIST, out, status )
      INTEGER :: out, status, length, nlvars, nbnd, ncol, nrival
      INTEGER :: len1_blu, len2_blu, len_blu_default, len_bnames
      LOGICAL :: defaut
      REAL ( KIND = wp ) :: value4
      CHARACTER ( LEN = 2 ) :: field1
      CHARACTER ( LEN = 10 ) :: field2, field3, field5
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TABLE, INLIST
      REAL ( KIND = wp ), DIMENSION( nrival ) :: RIVAL
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: B_l_default
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: B_u_default
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : , : ) :: B_l, B_u
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: BNAMES
      CHARACTER ( LEN = 12 ), ALLOCATABLE, DIMENSION( : ) :: KEY

!  ------------------------
!  indicator card is bounds
!  ------------------------

!  local variables

      INTEGER :: i, ifield
      INTEGER :: used_length, new_length, min_length, alloc_status
      INTEGER :: used_length2, new_length2, min_length2
      CHARACTER ( LEN = 24 ) :: bad_alloc

!  the first pair of bound vectors are to be assigned

      IF ( nbnd == 0 ) THEN
        nbnd = 1
        BNAMES( nbnd ) = field2
        defaut = .TRUE.
        B_l_default( nbnd ) = zero
        B_u_default( nbnd ) = biginf
        DO i = 1, nlvars
          B_l( i, nbnd ) = zero
          B_u( i, nbnd ) = biginf
        END DO
      END IF

!  a new pair of bound vectors are to be assigned

      IF ( field2 /= BNAMES( nbnd ) ) THEN
        nbnd = nbnd + 1
        IF ( nbnd > len2_blu ) THEN
          used_length = nlvars ; min_length = len1_blu
          new_length = len1_blu
          used_length2 = nbnd - 1 ; min_length2 = nbnd
          new_length2 = increase_n * min_length2 / increase_d + 1
          CALL EXTEND_array( B_l, len1_blu, len2_blu, used_length,             &
                             used_length2, new_length, new_length2,            &
                             min_length, min_length2, buffer,                  &
                             status, alloc_status, 'B_l' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'B_l' ; status = - 13 ; GO TO 980 ; END IF

          used_length = nlvars ; min_length = len1_blu
          new_length = len1_blu
          used_length2 = nbnd - 1 ; min_length2 = nbnd
          new_length2 = increase_n * min_length2 / increase_d + 1
          CALL EXTEND_array( B_u, len1_blu, len2_blu, used_length,             &
                             used_length2, new_length, new_length2,            &
                             min_length, min_length2, buffer,                  &
                             status, alloc_status, 'B_u' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'B_u' ; status = - 13 ; GO TO 980 ; END IF
          len2_blu = new_length2
        END IF

        IF ( nbnd >= len_blu_default ) THEN
          used_length = nbnd - 1 ; min_length = nbnd
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( B_l_default, len_blu_default, used_length,        &
                             new_length, min_length, buffer,                   &
                             status, alloc_status, 'B_l_default' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'B_l_default' ; status = - 13 ; GO TO 980 ; END IF

          used_length = nbnd - 1 ; min_length = nbnd
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( B_u_default, len_blu_default, used_length,        &
                             new_length, min_length, buffer,                   &
                             status, alloc_status, 'B_u_default' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'B_u_default' ; status = - 13 ; GO TO 980 ; END IF
          len_blu_default = new_length
        END IF

        IF ( nbnd >= len_bnames ) THEN
          used_length = nbnd - 1 ; min_length = nbnd
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( BNAMES, len_bnames, used_length, new_length,      &
                             min_length, buffer, status, alloc_status,         &
                             'BNAMES' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'BNAMES' ; status = - 13 ; GO TO 980 ; END IF
          len_bnames = new_length
        END IF

        BNAMES( nbnd ) = field2
        defaut = .TRUE.
        B_l_default( nbnd ) = zero
        B_u_default( nbnd ) = biginf
        DO i = 1, nlvars
          B_l( i, nbnd ) = zero
          B_u( i, nbnd ) = biginf
        END DO
      END IF

!  ensure that default values are assigned first

      IF ( field3 == '''DEFAULT'' ' ) THEN
        IF ( .NOT. defaut ) THEN
          status = 20
          IF ( out > 0 ) WRITE( out, 2200 )
          RETURN
        END IF
        IF ( field1 == 'ZL' .OR. field1 == 'ZU' .OR.                           &
             field1 == 'ZX' ) THEN
          CALL HASH_search( length, 12,  FIELD5( 1 : 10 ) // 'RI',             &
                             TABLE, KEY, ifield )
          IF ( ifield <= 0 ) THEN
            status = 3
            IF ( out > 0 ) WRITE( out, 2030 ) FIELD5( 1 : 10 )
            RETURN
          END IF
          value4 = RIVAL( INLIST( ifield ) )
        END IF
        IF ( field1 == 'LO' .OR. field1 == 'UP' .OR. field1 == 'FX' .OR.       &
             field1 == 'FR' .OR. field1 == 'MI' .OR. field1 == 'PL' .OR.       &
             field1 == 'XL' .OR. field1 == 'XU' .OR. field1 == 'XX' .OR.       &
             field1 == 'XR' .OR. field1 == 'XM' .OR. field1 == 'XP' .OR.       &
             field1 == 'ZL' .OR. field1 == 'ZU' .OR. field1 == 'ZX' ) THEN

!  assign default lower bounds for variables

          IF ( ( field1( 2 : 2 ) == 'L' .AND. field1 /= 'PL' ) .OR.            &
                 field1( 2 : 2 ) == 'X' .OR. field1( 2 : 2 ) == 'R' .OR.       &
                 field1( 2 : 2 ) == 'M' .OR. field1 == 'LO' .OR.               &
                 field1 == 'MI' ) THEN

!  a finite lower bound is specified

            IF ( field1( 2 : 2 ) == 'L' .OR. field1( 2 : 2 ) == 'X' .OR.       &
                 field1 == 'LO' ) THEN
              B_l_default( nbnd ) = value4
              DO i = 1, nlvars
                B_l( i, nbnd ) = value4
              END DO

!  an infinite lower bound is specified

            ELSE
              B_l_default( nbnd ) = - biginf
              DO i = 1, nlvars
                B_l( i, nbnd ) = - biginf
              END DO
              IF ( field1( 2 : 2 ) == 'M' .OR. field1 == 'MI' ) THEN
                B_u_default( nbnd ) = zero
                DO i = 1, nlvars
                  B_u( i, nbnd ) = zero
                END DO
              END IF
            END IF
          END IF

!  assign default upper bounds for variables

          IF ( field1( 2 : 2 ) == 'U' .OR. field1( 2 : 2 ) == 'X' .OR.         &
               field1( 2 : 2 ) == 'R' .OR. field1( 2 : 2 ) == 'P' ) THEN

!  a finite upper bound is specified

            IF ( field1( 2 : 2 ) == 'U' .OR. field1( 2 : 2 ) == 'X' .OR.       &
                 field1 == 'UP' ) THEN
              IF ( ( field1( 2 : 2 ) == 'U' .OR. field1 == 'UP' ) .AND.        &
                   value4 == zero .AND. B_l_default( nbnd ) == zero .AND.      &
                   B_u_default( nbnd ) == biginf ) THEN
                B_l_default( nbnd ) = - biginf
                DO i = 1, nlvars
                  B_l( i, nbnd ) = - biginf
                END DO
              END IF
              B_u_default( nbnd ) = value4
              DO i = 1, nlvars
                B_u( i, nbnd ) = value4
              END DO

!  an infinite upper bound is specified

            ELSE
              B_u_default( nbnd ) = biginf
              DO i = 1, nlvars
                B_u( i, nbnd ) = biginf
              END DO
            END IF
          END IF

!  field 1 is not recognised

        ELSE
          status = 10
          IF ( out > 0 ) WRITE( out, 2100 ) field1
          RETURN
        END IF

!  an individual bound is to be assigned

      ELSE
        defaut = .FALSE.
        IF ( field1 == 'LO' .OR. field1 == 'XL' .OR. field1 == 'UP' .OR.       &
             field1 == 'XU' .OR. field1 == 'FX' .OR. field1 == 'XX' .OR.       &
             field1 == 'FR' .OR. field1 == 'XR' .OR. field1 == 'MI' .OR.       &
             field1 == 'XM' .OR. field1 == 'PL' .OR. field1 == 'XP' .OR.       &
             field1 == 'ZL' .OR. field1 == 'ZU' .OR. field1 == 'ZX' ) THEN

!  find which variable is being assigned

          CALL HASH_search( length, 12, FIELD3//'VA', TABLE, KEY, ifield )
          IF ( ifield > 0 ) THEN
            ncol = INLIST( ifield )

!  assign a lower bound for this variable

            IF ( field1 == 'LO' .OR. field1 == 'XL' .OR. field1 == 'FX' .OR.   &
                 field1 == 'XX' .OR. field1 == 'FR' .OR. field1 == 'XR' .OR.   &
                 field1 == 'MI' .OR. field1 == 'XM' .OR. field1 == 'ZL' .OR.   &
                 field1 == 'ZX' ) THEN

!  a finite lower bound is specified

              IF ( field1 == 'LO' .OR. field1 == 'XL' .OR. field1 == 'ZL' .OR. &
                   field1 == 'ZX' .OR. field1 == 'FX' .OR. field1 == 'XX' ) THEN
                B_l( ncol, nbnd ) = value4

!  an infinite lower bound is specified

              ELSE
                IF ( ( field1 == 'MI' .OR. field1 == 'XM' ) .AND.              &
                       B_l( ncol, nbnd ) == zero  .AND.                        &
                       B_u( ncol, nbnd ) == biginf )                           &
                    B_u( ncol, nbnd ) = zero
                B_l( ncol, nbnd ) = - biginf
              END IF
            END IF

!  assign an upper bound for the variable

            IF ( field1 == 'UP' .OR. field1 == 'XU' .OR. field1 == 'FX' .OR.   &
                 field1 == 'XX' .OR. field1 == 'FR' .OR. field1 == 'XR' .OR.   &
                 field1 == 'PL' .OR. field1 == 'XP' .OR. field1 == 'ZU' .OR.   &
                 field1 == 'ZX' ) THEN

!  a finite upper bound is specified

              IF ( field1 == 'UP' .OR. field1 == 'XU' .OR.                     &
                   field1 == 'ZU' .OR. field1 == 'ZX' .OR.                     &
                   field1 == 'FX' .OR. field1 == 'XX' ) THEN
                IF ( ( field1 == 'UP' .OR. field1 == 'XU' .OR.                 &
                       field1 == 'ZU' ) .AND. value4 == zero .AND.             &
                       B_l( ncol, nbnd ) == zero .AND.                         &
                       B_u( ncol, nbnd ) == biginf )                           &
                    B_l( ncol, nbnd ) = - biginf
                B_u( ncol, nbnd ) = value4


!  an infinite upper bound is specified

              ELSE
                B_u( ncol, nbnd ) = biginf
              END IF
            END IF
          ELSE
            status = 5
            IF ( out > 0 ) WRITE( out, 2050 ) field3
            RETURN
          END IF

!  field 1 is not recognised

        ELSE
          status = 10
          IF ( out > 0 ) WRITE( out, 2100 ) field1
          RETURN
        END IF
      END IF
      status = 0
      RETURN

!  allocation errors

  980 CONTINUE
      WRITE( out, "( ' ** Message from INTERPRET_gpsmps-',                     &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  non-executable statements

 2030 FORMAT( ' ** Exit from INTERPRET_gpsmps - index parameter name ', A10,   &
              ' not recognised ' )
 2050 FORMAT( ' ** Exit from INTERPRET_gpsmps - column/var name not',          &
              ' recognised: name is ', A10 )
 2100 FORMAT( ' ** Exit from INTERPRET_gpsmps - field 1 ', A2,                 &
              '  not recognised in BOUNDS section ' )
 2200 FORMAT( ' ** Exit from INTERPRET_gpsmps - default specified out of order')

!  end of subroutine SBOUND

      END SUBROUTINE SBOUND

!-*-*-*-*-*- S I F D E C O D E   S S T A R T    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE SSTART( nlvars, ng, novals, nstart, ncol, nrival, defaut,     &
                         len1_vstart, len2_vstart, VSTART,                     &
                         len1_cstart, len2_cstart, CSTART, RIVAL,              &
                         field1, field2, field3, value4, field5, value6,       &
                         len_snames, SNAMES,                                   &
                         length, TABLE, KEY, INLIST, out, status )
      INTEGER :: length, novals, ng, ncol, nrival, out, status
      INTEGER :: nlvars, nstart, len1_vstart, len2_vstart
      INTEGER :: len1_cstart, len2_cstart, len_snames
      REAL ( KIND = wp ) :: value4, value6
      LOGICAL :: defaut
      CHARACTER ( LEN = 2 ) :: field1
      CHARACTER ( LEN = 10 ) :: field2, field3, field5
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TABLE, INLIST
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : , : ) :: VSTART, CSTART
      REAL ( KIND = wp ), DIMENSION( nrival ) :: RIVAL
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: SNAMES
      CHARACTER ( LEN = 12 ), ALLOCATABLE, DIMENSION( : ) :: KEY

!  -----------------------------
!  indicator card is start point
!  -----------------------------

!  local variables

      INTEGER :: i, ifield
      INTEGER :: used_length, new_length, min_length, alloc_status
      INTEGER :: used_length2, new_length2, min_length2
      CHARACTER ( LEN = 10 ) :: field
      CHARACTER ( LEN = 24 ) :: bad_alloc

!  the starting vector is  to be assigned

      IF ( nstart == 0 ) THEN
        nstart = 1
        SNAMES( nstart ) = field2
        defaut = .TRUE.
        DO i = 1, nlvars
          VSTART( i, nstart ) = zero
        END DO
        DO i = 1, ng
          CSTART( i, nstart ) = zero
        END DO
      END IF

!  a new starting vector is to be assigned; ensure that there is enough space

      IF ( field2 /= SNAMES( nstart ) ) THEN
        nstart = nstart + 1
        IF ( nstart > len2_vstart ) THEN
          used_length = nlvars ; min_length = len1_vstart
          new_length = len1_vstart
          used_length2 = nstart - 1 ; min_length2 = nstart
          new_length2 = increase_n * min_length2 / increase_d + 1
          CALL EXTEND_array( VSTART, len1_vstart, len2_vstart, used_length,    &
                             used_length2, new_length, new_length2,            &
                             min_length, min_length2, buffer,                  &
                             status, alloc_status, 'VSTART' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'VSTART' ; status = - 8 ; GO TO 980 ; END IF
          len2_vstart = new_length2
        END IF
        IF ( nstart > len2_cstart ) THEN
          used_length = ng ; new_length = len1_cstart ; min_length = len1_cstart
          used_length2 = nstart - 1 ; min_length2 = nstart
          new_length2 = increase_n * min_length2 / increase_d + 1
          CALL EXTEND_array( CSTART, len1_cstart, len2_cstart, used_length,    &
                             used_length2, new_length, new_length2,            &
                             min_length, min_length2, buffer,                  &
                             status, alloc_status, 'CSTART' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'CSTART' ; status = - 8 ; GO TO 980 ; END IF
          len2_cstart = new_length2
        END IF
        IF ( nstart > len_snames ) THEN
          used_length = nstart - 1 ; min_length = nstart
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( SNAMES, len_snames, used_length, new_length,      &
                             min_length, buffer, status, alloc_status,         &
                             'SNAMES' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'SNAMES' ; status = - 8 ; GO TO 980 ; END IF
          len_snames = new_length
        END IF
        SNAMES( nstart ) = field2
        defaut = .TRUE.

!  assign a default value of zero to the variables

        VSTART( : nlvars, nstart ) = zero

!  assign a default value of zero to the lagrange multipliers

        CSTART( : ng, nstart ) = zero
      END IF

!  ensure that default values are assigned first

      IF ( field3 == '''DEFAULT'' ' ) THEN
        IF ( .NOT. defaut ) THEN
          status = 20
          IF ( out > 0 ) WRITE( out, 2200 )
          RETURN
        END IF
        IF ( field1( 1 : 1 ) == 'Z' ) THEN
          CALL HASH_search( length, 12,  FIELD5( 1 : 10 ) // 'RI',             &
                             TABLE, KEY, ifield )
          IF ( ifield <= 0 ) THEN
            status = 3
            IF ( out > 0 ) WRITE( out, 2030 ) FIELD5( 1 : 10 )
            RETURN
          END IF
          value4 = RIVAL( INLIST( ifield ) )
        END IF

!  assign default values to the starting point

        IF ( field1 == '  ' .OR. field1 == 'V ' .OR. field1 == 'X ' .OR.       &
             field1 == 'Z ' .OR. field1 == 'XV' .OR. field1 == 'ZV' )          &
          VSTART( : nlvars, nstart ) = value4

!  assign default values to the lagrange multipliers

        IF ( field1 == '  ' .OR. field1 == 'M ' .OR. field1 == 'X ' .OR.       &
             field1 == 'Z ' .OR. field1 == 'XM' .OR. field1 == 'ZM' )          &
          CSTART( : ng, nstart ) = value4
      ELSE

!  an individual starting value is to be assigned

        IF ( field1 == 'X ' .OR. field1 == '  ' .OR. field1 == 'Z ' ) THEN
          defaut = .FALSE.

!  find which value is, or values are, being assigned

          DO i = 1, novals
            IF ( i == 1 ) THEN
              field = field3
            ELSE
              field = field5
            END IF

!  see if the name belongs to a variable

            CALL HASH_search( length, 12, field // 'VA', TABLE, KEY, ifield )
            IF ( ifield > 0 ) THEN
              ncol = INLIST( ifield )

!  assign the starting value for this variable

              IF ( i == 1 ) THEN
                VSTART( ncol, nstart ) = value4
              ELSE
                VSTART( ncol, nstart ) = value6
              END IF

!  see if the name belongs to a group

            ELSE
              CALL HASH_search( length, 12, field // 'GR', TABLE, KEY, ifield )
              IF ( ifield > 0 ) THEN
                ncol = INLIST( ifield )

!  assign the starting value for the lagrange multiplier for this group

                IF ( i == 1 ) THEN
                  CSTART( ncol, nstart ) = value4
                ELSE
                  CSTART( ncol, nstart ) = value6
                END IF
              ELSE
                status = 5
                IF ( i == 1 ) THEN
                  IF ( out > 0 ) WRITE( out, 2050 ) field3
                ELSE
                  IF ( out > 0 ) WRITE( out, 2050 ) field5
                END IF
                RETURN
              END IF
            END IF
          END DO
        ELSE

!  an individual starting value for a variable is to be assigned

          IF ( field1 == 'V ' .OR. field1 == 'XV' .OR. field1 == 'ZV' ) THEN
            defaut = .FALSE.

!  find which value is, or values are, being assigned

            DO i = 1, novals
              IF ( i == 1 ) THEN
                field = field3
              ELSE
                field = field5
              END IF

!  see if the name belongs to a variable

              CALL HASH_search( length, 12, field // 'VA', TABLE, KEY, ifield )
              IF ( ifield > 0 ) THEN
                ncol = INLIST( ifield )

!  assign the starting value for this variable

                IF ( i == 1 ) THEN
                  VSTART( ncol, nstart ) = value4
                ELSE
                  VSTART( ncol, nstart ) = value6
                END IF
              ELSE
                status = 4
                IF ( i == 1 ) THEN
                  IF ( out > 0 ) WRITE( out, 2040 ) field3
                ELSE
                  IF ( out > 0 ) WRITE( out, 2040 ) field5
                END IF
                RETURN
              END IF
            END DO
          ELSE

!  an individual starting lagrange multiplier value is to be assigned

            IF ( field1 == 'M ' .OR. field1 == 'XM' .OR. field1 == 'ZM' ) THEN
              defaut = .FALSE.

!  find which value is, or values are, being assigned

              DO i = 1, novals
                IF ( i == 1 ) THEN
                  field = field3
                ELSE
                  field = field5
                END IF

!  see if the name belongs to a group

                CALL HASH_search( length, 12, field // 'GR', TABLE, KEY,      &
                                  ifield )
                IF ( ifield > 0 ) THEN
                  ncol = INLIST( ifield )

!  assign the starting value for the lagrange multiplier for this group

                  IF ( i == 1 ) THEN
                     CSTART( ncol, nstart ) = value4
                  ELSE
                     CSTART( ncol, nstart ) = value6
                  END IF
                ELSE
                  status = 5
                  IF ( i == 1 ) THEN
                    IF ( out > 0 ) WRITE( out, 2050 ) field3
                   ELSE
                    IF ( out > 0 ) WRITE( out, 2050 ) field5
                  END IF
                  RETURN
                END IF
              END DO

!  field 1 is not recognised

            ELSE
              status = 10
              IF ( out > 0 ) WRITE( out, 2100 ) field1
              RETURN
            END IF
          END IF
        END IF
      END IF
      status = 0
      RETURN

!  allocation errors

  980 CONTINUE
      WRITE( out, "( ' ** Message from INTERPRET_gpsmps-',                     &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  non-executable statements

 2030 FORMAT( ' ** Exit from INTERPRET_gpsmps - index parameter name ', A10,   &
              ' not recognised ' )
 2040 FORMAT( ' ** Exit from INTERPRET_gpsmps - group/row name not',           &
              ' recognised: name is ', A10 )
 2050 FORMAT( ' ** Exit from INTERPRET_gpsmps - column/var name not',          &
              ' recognised: name is ', A10 )
 2100 FORMAT( ' ** Exit from INTERPRET_gpsmps - field 1 ', A2,                 &
              '  not recognised in START POINT section ' )
 2200 FORMAT( ' ** Exit from INTERPRET_gpsmps - default specified out of order')

!  end of subroutine SSTART

      END SUBROUTINE SSTART

!-*-*-*-*-*- S I F D E C O D E   S Q H E S S    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE SQHESS( ng, nobj, ngrupe, novals, nevnames, nivnames,         &
                         nepnames, neltype, nlisep, nelvar, nelnum,            &
                         neling, iptype, istype, inrep, qgroup, qsqr, qprod,   &
                         len_elv, ELV, len_inv, INV, len_elp, ELP,             &
                         len_gtype, GTYPE, len_eling_el, ELING_el,             &
                         len_eling_g, ELING_g, len_ev_ptr, EV_ptr,             &
                         len_elvar, ELVAR, len_gstate, GSTATE,                 &
                         len_typee, TYPEE,  len_ep_ptr, EP_ptr,                &
                         len1_cstart, nstart, CSTART, len_rscale, RSCALE,      &
                         field1, field2, field3, value4, field5, value6,       &
                         len_evnames, EVNAMES, len_ivnames, IVNAMES,           &
                         len_gnames, GNAMES, len_etypes, ETYPES,               &
                         len_onames, ONAMES,                                   &
                         len_lnames, LNAMES, len_weight, WEIGHT,               &
                         length, TABLE, KEY, INLIST, out, status )
      INTEGER :: out, status, length, iptype, istype
      INTEGER :: nobj, nevnames, nivnames, nepnames, neltype, nelnum
      INTEGER :: ng, neling, novals, ngrupe, nlisep, len_elvar, nelvar
      INTEGER :: len_elv, len_inv, len_elp, len_etypes
      INTEGER :: len_evnames, len_ivnames
      INTEGER :: len_gstate, len_gtype, len_gnames, len_onames, len_rscale
      INTEGER :: len_lnames, len_ev_ptr, len_typee, len_ep_ptr
      INTEGER :: len_eling_el, len_eling_g, len_weight
      INTEGER :: len1_cstart, nstart
      REAL ( KIND = wp ) :: value4, value6
      LOGICAL :: qgroup, qsqr, qprod, inrep
      CHARACTER ( LEN = 2 ) :: field1
      CHARACTER ( LEN = 10 ) :: field2, field3, field5
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TABLE, INLIST
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: ELV, INV, ELP
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: GSTATE, GTYPE, ELING_el, ELING_g
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: EV_ptr, TYPEE, EP_ptr, ELVAR
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: RSCALE, WEIGHT
      REAL ( KIND = wp ), DIMENSION( len1_cstart, nstart  ) :: CSTART
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: GNAMES, ONAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: LNAMES, ETYPES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: EVNAMES, IVNAMES
      CHARACTER ( LEN = 12 ), ALLOCATABLE, DIMENSION( : ) :: KEY

!  ---------------------------
!  indicator card is quadratic
!  ---------------------------

!  local variables

      INTEGER :: i, ifield, k, nevars, ncol1, ncol2, nterms, ifree
      INTEGER :: used_length, new_length, min_length, alloc_status
      REAL ( KIND = wp ) :: value
      CHARACTER ( LEN = 24 ) :: bad_alloc
      CHARACTER ( LEN = 12 ) :: field

!  find the first variable

      CALL HASH_search( length, 12, field2 // 'VA', TABLE, KEY, ifield )
      IF ( ifield > 0 ) THEN
        ncol1 = INLIST( ifield )
      ELSE
        status = 5
        IF ( out > 0 ) WRITE( out, 2050 ) field2
        RETURN
      END IF

!  find the second variable

      IF ( field1 == 'Z ' ) THEN
        nterms = 1
      ELSE
        nterms = novals
      END IF


      DO i = 1, nterms
        IF ( i == 1 ) THEN
          field = field3 // 'VA'
          value = value4
        ELSE
          field = field5 // 'VA'
          value = value6
        END IF
        IF ( value /= 0.0D+0 ) THEN
          CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
          IF ( ifield > 0 ) THEN
            ncol2 = INLIST( ifield )
          ELSE
            status = 5
            IF ( out > 0 ) WRITE( out, 2050 ) FIELD( 1 : 10 )
            RETURN
          END IF

!  this is the first Hessian term. make it a new group. Find a place to
!  insert the new group name in the hash-table

          IF ( .NOT. qgroup ) THEN
            CALL HASH_enlarge_and_insert( length, 12, cqgrou // 'GR',          &
                                          TABLE, KEY, INLIST, ifree )
            IF ( ifree <= 0 ) THEN
              status = - 1
              RETURN

!  mark this as an objective function group

            ELSE
              nobj = nobj + 1
              IF ( nobj > len_onames ) THEN
                used_length = nobj - 1 ; min_length = nobj
                new_length = increase_n * min_length / increase_d + 1
                CALL EXTEND_array( ONAMES, len_gnames, used_length, new_length,&
                                   min_length, buffer, status, alloc_status,   &
                                   'ONAMES' )
                IF ( status /= 0 ) THEN
                  bad_alloc = 'ONAMES' ; status = - 5 ; GO TO 980 ; END IF
                len_onames = new_length
              END IF
              ONAMES ( nobj ) =  cqgrou

!  the group is the ng-th encountered; ensure there is sufficient space

              ng = ng + 1
              IF ( ng >= len_gstate ) THEN
                used_length = ng - 1 ; min_length = ng
                new_length = increase_n * min_length / increase_d + 1
                CALL EXTEND_array( GSTATE, len_gstate, used_length,            &
                                   new_length, min_length, buffer,             &
                                   status, alloc_status, 'GSTATE' )
                IF ( status /= 0 ) THEN
                  bad_alloc = 'GSTATE' ; status = - 6 ; GO TO 980 ; END IF
                len_gstate = new_length
              END IF
              IF ( ng >= len_gtype ) THEN
                used_length = ng - 1 ; min_length = ng
                new_length = increase_n * min_length / increase_d + 1
                CALL EXTEND_array( GTYPE, len_gtype, used_length,              &
                                   new_length, min_length, buffer,             &
                                   status, alloc_status, 'GTYPE' )
                IF ( status /= 0 ) THEN
                  bad_alloc = 'GTYPE' ; status = - 6 ; GO TO 980 ; END IF
                len_gtype = new_length
              END IF
              IF ( ng >= len_gnames ) THEN
                used_length = ng - 1 ; min_length = ng
                new_length = increase_n * min_length / increase_d + 1
                CALL EXTEND_array( GNAMES, len_gnames, used_length,            &
                                   new_length, min_length, buffer,             &
                                   status, alloc_status, 'GNAMES' )
                IF ( status /= 0 ) THEN
                  bad_alloc = 'GNAMES' ; status = - 6 ; GO TO 980 ; END IF
                len_gnames = new_length
              END IF
              IF ( ng >= len_rscale ) THEN
                used_length = ng - 1 ; min_length = ng
                new_length = increase_n * min_length / increase_d + 1
                CALL EXTEND_array( RSCALE, len_rscale, used_length,            &
                                   new_length, min_length, buffer,             &
                                   status, alloc_status, 'RSCALE' )
                IF ( status /= 0 ) THEN
                  bad_alloc = 'RSCALE' ; status = - 6 ; GO TO 980 ; END IF
                len_rscale = new_length
              END IF

!  record the position of the new group in the table, record its
!  name and initialise its type as trivial

              ngrupe = ng
              INLIST( ifree ) = ng
              GNAMES( ng ) = cqgrou
              GTYPE( ng ) = 0
              GSTATE( ng ) = 1
              RSCALE( ng ) = one
              CSTART( ng, : nstart ) = zero
              qgroup = .TRUE.
            END IF
          END IF
          IF ( ncol1 == ncol2 ) THEN

!  check if this is the first occurence of a diagonal term

            IF ( .NOT. qsqr ) THEN

!  check if this is the first element type

              IF ( neltype == 0 ) THEN
                istype = 1
                nevnames = 1
                nivnames = 0
                nepnames = 0
                neltype = 1
              ELSE
                istype = 2
                neltype = neltype + 1
                IF ( neltype > len_elv ) THEN
                  used_length = neltype - 1 ; min_length = neltype + 1
                  new_length = increase_n * min_length / increase_d + 1
                  CALL EXTEND_array( ELV, len_elv, used_length, new_length,    &
                                     min_length, buffer, status, alloc_status, &
                                     'ELV' )
                  IF ( status /= 0 ) THEN
                    bad_alloc = 'ELV' ; status = - 3 ; GO TO 980 ; END IF
                  len_elv = new_length
                END IF

                IF ( neltype > len_inv ) THEN
                  used_length = neltype - 1 ; min_length = neltype + 1
                  new_length = increase_n * min_length / increase_d + 1
                  CALL EXTEND_array( INV, len_inv, used_length, new_length,    &
                                     min_length, buffer, status, alloc_status, &
                                     'INV' )
                  IF ( status /= 0 ) THEN
                    bad_alloc = 'INV' ; status = - 3 ; GO TO 980 ; END IF
                  len_inv = new_length
                END IF

                IF ( neltype > len_elp ) THEN
                  used_length = neltype - 1 ; min_length = neltype + 1
                  new_length = increase_n * min_length / increase_d + 1
                  CALL EXTEND_array( ELP, len_elp, used_length, new_length,    &
                                     min_length, buffer, status, alloc_status, &
                                     'ELP' )
                  IF ( status /= 0 ) THEN
                    bad_alloc = 'ELP' ; status = - 3 ; GO TO 980 ; END IF
                  len_elp = new_length
                END IF

                IF ( neltype > len_etypes ) THEN
                  used_length = neltype - 1 ; min_length = neltype
                  new_length = increase_n * min_length / increase_d + 1
                  CALL EXTEND_array( ETYPES, len_etypes, used_length,          &
                                     new_length, min_length, buffer,           &
                                     status, alloc_status, 'ETYPES' )
                  IF ( status /= 0 ) THEN
                    bad_alloc = 'ETYPES' ; status = - 3 ; GO TO 980 ; END IF
                  len_etypes = new_length
                END IF

                nevnames = nevnames + 1
                IF ( nevnames > len_evnames ) THEN
                  used_length = nevnames - 1 ; min_length = nevnames
                  new_length = increase_n * min_length / increase_d + 1
                  CALL EXTEND_array( EVNAMES, len_evnames, used_length,        &
                                     new_length, min_length, buffer,           &
                                     status, alloc_status, 'EVNAMES' )
                  IF ( status /= 0 ) THEN
                    bad_alloc = 'EVNAMES' ; status = - 14 ; GO TO 980 ; END IF
                  len_evnames = new_length
                END IF
              END IF

!  input the names of the element type

              CALL HASH_enlarge_and_insert( length, 12, cqsqr // 'ET',         &
                                            TABLE, KEY, INLIST, ifree )
              IF ( ifree <= 0 ) THEN
                IF ( ifree == 0 ) THEN
                  status = - 1
                  RETURN
                END IF
                status = 18
                IF ( out > 0 ) WRITE( out, 2180 )
                RETURN
              END IF
              INLIST( ifree ) = neltype
              ELV( neltype ) = nevnames
              INV( neltype ) = nivnames + 1
              ELP( neltype ) = nepnames + 1
              ETYPES( neltype ) = cqsqr

!  insert the element variable

              EVNAMES( nevnames ) = 'X         '
              nivnames = nivnames + 1
              IF ( nivnames > len_ivnames ) THEN
                used_length = nivnames - 1 ; min_length = nivnames
                new_length = increase_n * min_length / increase_d + 1
                CALL EXTEND_array( IVNAMES, len_ivnames, used_length,          &
                                   new_length, min_length, buffer,             &
                                   status, alloc_status, 'IVNAMES' )
                IF ( status /= 0 ) THEN
                  bad_alloc = 'IVNAMES' ; status = - 16 ; GO TO 980 ; END IF
                len_ivnames = new_length
              END IF
              IVNAMES( nivnames ) = EVNAMES( nevnames )
              inrep = .FALSE.
              qsqr = .TRUE.
            END IF

!  the new element is the nelnum-th nonlinear element

            nelnum = nelnum + 1
            IF ( nelnum > len_lnames ) THEN
              used_length = nelnum - 1 ; min_length = nelnum
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( LNAMES, len_lnames, used_length, new_length,  &
                                 min_length, buffer, status, alloc_status,     &
                                 'LNAMES' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'LNAMES' ; status = - 9 ; GO TO 980 ; END IF
              len_lnames = new_length
            END IF

            IF ( nelnum > len_typee ) THEN
              used_length = nelnum - 1 ; min_length = nelnum
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( TYPEE, len_typee, used_length, new_length,    &
                                 min_length, buffer, status, alloc_status,     &
                                 'TYPEE' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'TYPEE' ; status = - 9 ; GO TO 980 ; END IF
              len_typee = new_length
            END IF

            IF ( nelnum + 1 > len_ep_ptr ) THEN
              used_length = nelnum - 1 ; min_length = nelnum + 1
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( EP_ptr, len_ep_ptr, used_length, new_length,  &
                                 min_length, buffer, status, alloc_status,     &
                                 'EP_ptr' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'EP_ptr' ; status = - 9 ; GO TO 980 ; END IF
              len_ep_ptr = new_length
            END IF

            IF ( nelnum + 1 > len_ev_ptr ) THEN
              used_length = nelnum - 1 ; min_length = nelnum + 1
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( EV_ptr, len_ev_ptr, used_length, new_length,  &
                                 min_length, buffer, status, alloc_status,     &
                                 'EV_ptr' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'EV_ptr' ; status = - 9 ; GO TO 980 ; END IF
              len_ev_ptr = new_length
            END IF

!  insert the name into the table

            WRITE( UNIT = field, FMT = "( '%', I9, 'EL' )" ) 123456789 - nelnum
            CALL HASH_enlarge_and_insert( length, 12, field,                   &
                                          TABLE, KEY, INLIST, ifree )

!  record the elements position in the table along with its name

            INLIST( ifree ) = nelnum
            LNAMES( nelnum ) = FIELD( 1 : 10 )

!  determine the number of the element type, k, the starting addresses for the
!  parameters and variables for the element and the number of parameters and
!  variables involved

            k = istype
            TYPEE( nelnum ) = k
            EP_ptr( nelnum ) = nlisep + 1
            EV_ptr( nelnum ) = nelvar + 1

            nevars = 1
            IF ( nelvar + nevars > len_elvar ) THEN
              used_length = nelvar ; min_length = nelvar + nevars
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( ELVAR, len_elvar, used_length, new_length,    &
                                 min_length, buffer, status, alloc_status,     &
                                 'ELVAR' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'ELVAR' ; status = - 15 ; GO TO 980 ; END IF
              len_elvar = new_length
            END IF
            ELVAR( nelvar + 1 ) = ncol1
            nelvar = nelvar + nevars

!  assign the element as neling in group ngrupe

            neling = neling + 1
            IF ( neling > len_eling_el ) THEN
              used_length = neling - 1 ; min_length = neling
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( ELING_el, len_eling_el, used_length,          &
                                 new_length, min_length, buffer,               &
                                 status, alloc_status, 'ELING_el' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'ELING_el' ; status = - 9 ; GO TO 980 ; END IF
              len_eling_el = new_length
            END IF

            IF ( neling > len_eling_g ) THEN
              used_length = neling - 1 ; min_length = neling
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( ELING_g, len_eling_g, used_length,            &
                                 new_length, min_length, buffer,               &
                                 status, alloc_status, 'ELING_g' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'ELING_g' ; status = - 9 ; GO TO 980 ; END IF
              len_eling_g = new_length
            END IF

            IF ( neling > len_weight ) THEN
              used_length = neling - 1 ; min_length = neling
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( WEIGHT, len_weight, used_length,              &
                                 new_length, min_length, buffer,               &
                                 status, alloc_status, 'WEIGHT' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'WEIGHT' ; status = - 9 ; GO TO 980 ; END IF
              len_weight = new_length
            END IF

!  the element is ELING_el and the group is given by ELING_g

            ELING_el( neling ) = nelnum
            ELING_g( neling ) = ngrupe

!  the element is weighted by the constant in WEIGHT

            WEIGHT( neling ) = value
          ELSE
            IF ( .NOT. qprod ) THEN

!  check if this is the first element type

              IF ( neltype == 0 ) THEN
                iptype = 1
                nevnames = 1
                nivnames = 0
                nepnames = 0
                neltype = 1
              ELSE
                iptype = 2
                neltype = neltype + 1
                IF ( neltype > len_elv ) THEN
                  used_length = neltype - 1 ; min_length = neltype + 1
                  new_length = increase_n * min_length / increase_d + 1
                  CALL EXTEND_array( ELV, len_elv, used_length, new_length,    &
                                     min_length, buffer, status, alloc_status, &
                                     'ELV' )
                  IF ( status /= 0 ) THEN
                    bad_alloc = 'ELV' ; status = - 3 ; GO TO 980 ; END IF
                  len_elv = new_length
                END IF

                IF ( neltype > len_inv ) THEN
                  used_length = neltype - 1 ; min_length = neltype + 1
                  new_length = increase_n * min_length / increase_d + 1
                  CALL EXTEND_array( INV, len_inv, used_length, new_length,    &
                                     min_length, buffer, status, alloc_status, &
                                     'INV' )
                  IF ( status /= 0 ) THEN
                    bad_alloc = 'INV' ; status = - 3 ; GO TO 980 ; END IF
                  len_inv = new_length
                END IF

                IF ( neltype > len_elp ) THEN
                  used_length = neltype - 1 ; min_length = neltype + 1
                  new_length = increase_n * min_length / increase_d + 1
                  CALL EXTEND_array( ELP, len_elp, used_length, new_length,    &
                                     min_length, buffer, status, alloc_status, &
                                     'ELP' )
                  IF ( status /= 0 ) THEN
                    bad_alloc = 'ELP' ; status = - 3 ; GO TO 980 ; END IF
                  len_elp = new_length
                END IF

                IF ( neltype > len_etypes ) THEN
                  used_length = neltype - 1 ; min_length = neltype
                  new_length = increase_n * min_length / increase_d + 1
                  CALL EXTEND_array( ETYPES, len_etypes, used_length,          &
                                     new_length, min_length, buffer,           &
                                     status, alloc_status, 'ETYPES' )
                  IF ( status /= 0 ) THEN
                    bad_alloc = 'ETYPES' ; status = - 3 ; GO TO 980 ; END IF
                  len_etypes = new_length
                END IF

                nevnames = nevnames + 1
                IF ( nevnames > len_evnames ) THEN
                  used_length = nevnames - 1 ; min_length = nevnames
                  new_length = increase_n * min_length / increase_d + 1
                  CALL EXTEND_array( EVNAMES, len_evnames, used_length,        &
                                     new_length, min_length, buffer,           &
                                     status, alloc_status, 'EVNAMES' )
                  IF ( status /= 0 ) THEN
                    bad_alloc = 'EVNAMES' ; status = - 14 ; GO TO 980 ; END IF
                  len_evnames = new_length
                END IF
              END IF

!  input the names of the element type

              CALL HASH_enlarge_and_insert( length, 12, cqprod // 'ET',        &
                                            TABLE, KEY, INLIST, ifree )
              IF ( ifree <= 0 ) THEN
                 IF ( ifree == 0 ) THEN
                    status = - 1
                    RETURN
                 END IF
                 status = 18
                 IF ( out > 0 ) WRITE( out, 2180 )
                 RETURN
              END IF
              INLIST( ifree ) = neltype
              ELV( neltype ) = nevnames
              INV( neltype ) = nivnames + 1
              ELP( neltype ) = nepnames + 1
              ETYPES( neltype ) = cqprod

!  insert the element variable

              EVNAMES( nevnames ) = 'X         '
              nivnames = nivnames + 1
              IF ( nivnames + 1 > len_ivnames ) THEN
                used_length = nivnames - 1 ; min_length = nivnames + 1
                new_length = increase_n * min_length / increase_d + 1
                CALL EXTEND_array( IVNAMES, len_ivnames, used_length,          &
                                   new_length, min_length, buffer,             &
                                   status, alloc_status, 'IVNAMES' )
                IF ( status /= 0 ) THEN
                  bad_alloc = 'IVNAMES' ; status = - 16 ; GO TO 980 ; END IF
                len_ivnames = new_length
              END IF
              IVNAMES( nivnames ) = EVNAMES( nevnames )
              nevnames = nevnames + 1
              IF ( nevnames > len_evnames ) THEN
                used_length = nevnames - 1 ; min_length = nevnames
                new_length = increase_n * min_length / increase_d + 1
                CALL EXTEND_array( EVNAMES, len_evnames, used_length,          &
                                   new_length, min_length, buffer,             &
                                   status, alloc_status, 'EVNAMES' )
                IF ( status /= 0 ) THEN
                  bad_alloc = 'EVNAMES' ; status = - 14 ; GO TO 980 ; END IF
                len_evnames = new_length
              END IF
              EVNAMES( nevnames ) = 'Y         '
              nivnames = nivnames + 1
              IVNAMES( nivnames ) = EVNAMES( nevnames )
              inrep = .FALSE.
              qprod = .TRUE.
            END IF

!  the new element is the nelnum-th nonlinear element

            nelnum = nelnum + 1
            IF ( nelnum > len_lnames ) THEN
              used_length = nelnum - 1 ; min_length = nelnum
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( LNAMES, len_lnames, used_length, new_length,  &
                                 min_length, buffer, status, alloc_status,     &
                                 'LNAMES' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'LNAMES' ; status = - 9 ; GO TO 980 ; END IF
              len_lnames = new_length
            END IF

            IF ( nelnum > len_typee ) THEN
              used_length = nelnum - 1 ; min_length = nelnum
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( TYPEE, len_typee, used_length, new_length,    &
                                 min_length, buffer, status, alloc_status,     &
                                 'TYPEE' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'TYPEE' ; status = - 9 ; GO TO 980 ; END IF
              len_typee = new_length
            END IF

            IF ( nelnum + 1 > len_ep_ptr ) THEN
              used_length = nelnum - 1 ; min_length = nelnum + 1
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( EP_ptr, len_ep_ptr, used_length, new_length,  &
                                 min_length, buffer, status, alloc_status,     &
                                 'EP_ptr' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'EP_ptr' ; status = - 9 ; GO TO 980 ; END IF
              len_ep_ptr = new_length
            END IF

            IF ( nelnum + 1 > len_ev_ptr ) THEN
              used_length = nelnum - 1 ; min_length = nelnum + 1
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( EV_ptr, len_ev_ptr, used_length, new_length,  &
                                 min_length, buffer, status, alloc_status,     &
                                 'EV_ptr' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'EV_ptr' ; status = - 9 ; GO TO 980 ; END IF
              len_ev_ptr = new_length
            END IF

!  insert the name into the table

            WRITE( UNIT = field, FMT = "( '%', I9, 'EL' )" ) 123456789 - nelnum
            CALL HASH_enlarge_and_insert( length, 12, field,                   &
                                          TABLE, KEY, INLIST, ifree )

!  record the elements position in the table along with its name

            INLIST( ifree ) = nelnum
            LNAMES( nelnum ) = FIELD( 1 : 10 )

!  determine the number of the element type, k, the starting
!  addresses for the parameters and variables for the element
!  and the number of parameters and variables involved

            k = iptype
            TYPEE( nelnum ) = k
            EP_ptr ( nelnum ) = nlisep + 1
            EV_ptr( nelnum ) = nelvar + 1

            nevars = 2
            IF ( nelvar + nevars > len_elvar ) THEN
              used_length = nelvar ; min_length = nelvar + nevars
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( ELVAR, len_elvar, used_length, new_length, &
                                 min_length, buffer, status, alloc_status,     &
                                 'ELVAR' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'ELVAR' ; status = - 15 ; GO TO 980 ; END IF
              len_elvar = new_length
            END IF
            ELVAR( nelvar + 1 ) = ncol1
            ELVAR( nelvar + 2 ) = ncol2
            nelvar = nelvar + nevars

!  assign the element as neling in group ngrupe

            neling = neling + 1
            IF ( neling > len_eling_el ) THEN
              used_length = neling - 1 ; min_length = neling
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( ELING_el, len_eling_el, used_length,          &
                                 new_length, min_length, buffer,               &
                                 status, alloc_status, 'ELING_el' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'ELING_el' ; status = - 9 ; GO TO 980 ; END IF
              len_eling_el = new_length
            END IF

            IF ( neling > len_eling_g ) THEN
              used_length = neling - 1 ; min_length = neling
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( ELING_g, len_eling_g, used_length,            &
                                 new_length, min_length, buffer,               &
                                 status, alloc_status, 'ELING_g' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'ELING_g' ; status = - 9 ; GO TO 980 ; END IF
              len_eling_g = new_length
            END IF

            IF ( neling > len_weight ) THEN
              used_length = neling - 1 ; min_length = neling
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( WEIGHT, len_weight, used_length,              &
                                 new_length, min_length, buffer,               &
                                 status, alloc_status, 'WEIGHT' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'WEIGHT' ; status = - 9 ; GO TO 980 ; END IF
              len_weight = new_length
            END IF

!  the element is ELING_el and the group is given by ELING_g

            ELING_el( neling ) = nelnum
            ELING_g( neling ) = ngrupe

!  the element is weighted by the constant in WEIGHT

            WEIGHT( neling ) = value
          END IF
        END IF
      END DO
      status = 0
      RETURN

!  allocation errors

  980 CONTINUE
      WRITE( out, "( ' ** Message from INTERPRET_gpsmps-',                     &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  non-executable statements

 2050 FORMAT( ' ** Exit from INTERPRET_gpsmps - column/var name not',          &
              ' recognised: name is ', A10 )
 2180 FORMAT( ' ** Exit from INTERPRET_gpsmps - duplicate element-type name ' )

!  end of subroutine SQHESS

      END SUBROUTINE SQHESS

!-*-*-*-*-*- S I F D E C O D E   S E T Y P E    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE SETYPE( novals, nevnames, nivnames, nepnames, neltype, inrep, &
                         len_elv, ELV, len_inv, INV, len_elp, ELP,             &
                         field1, field2, field3, field5,                       &
                         len_evnames, EVNAMES, len_ivnames, IVNAMES,           &
                         len_epnames, EPNAMES, len_etypes, ETYPES,             &
                         length, TABLE, KEY, INLIST, out, status )
      INTEGER :: out, status, length
      INTEGER :: novals, nevnames, nivnames, neltype, nepnames
      INTEGER :: len_elv, len_inv, len_elp, len_etypes
      INTEGER :: len_evnames, len_ivnames, len_epnames
      LOGICAL :: inrep
      CHARACTER ( LEN = 2 ) :: field1
      CHARACTER ( LEN = 10 ) :: field2, field3, field5
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TABLE, INLIST
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: ELV, INV, ELP
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: EVNAMES, IVNAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: EPNAMES, ETYPES
      CHARACTER ( LEN = 12 ), ALLOCATABLE, DIMENSION( : ) :: KEY

!  ------------------------------
!  indicator card is element type
!  ------------------------------

!  local variables

      INTEGER :: i, ifree, k
      INTEGER :: used_length, new_length, min_length, alloc_status
      CHARACTER ( LEN = 24 ) :: bad_alloc

!  check if this is the first element type

      IF ( neltype == 0 ) THEN
        CALL HASH_enlarge_and_insert( length, 12, field2 // 'ET',              &
                                      TABLE, KEY, INLIST, ifree )
        IF ( ifree <= 0 ) THEN
          IF ( ifree == 0 ) THEN
            status = - 1
            RETURN
          END IF
          status = 18
          IF ( out > 0 ) WRITE( out, 2180 )
          RETURN
        END IF
        neltype = 1
        nevnames = 0
        nivnames = 0
        nepnames = 0
        inrep = .FALSE.
        INLIST( ifree ) = neltype
        ELV( neltype ) = nevnames + 1
        INV( neltype ) = nivnames + 1
        ELP( neltype ) = nepnames + 1
        ETYPES( neltype ) = field2
      END IF

!  check if the column is new

      IF ( field2 /= ETYPES( neltype ) ) THEN

!  if the previous element has no explicit internal representation, use its
!  elemental representation

        IF ( ETYPES( neltype ) /= cqsqr .AND. ETYPES( neltype ) /= cqprod ) THEN
          IF ( .NOT. inrep ) THEN
            i = nevnames - ELV( neltype ) + 1
            IF ( nivnames + i > len_ivnames ) THEN
              used_length = nivnames ; min_length = nivnames + i
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( IVNAMES, len_ivnames, used_length, new_length,&
                                 min_length, buffer, status, alloc_status,     &
                                 'IVNAMES' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'IVNAMES' ; status = - 16 ; GO TO 980
              END IF
              len_ivnames = new_length
            END IF

            DO k = ELV( neltype ), nevnames
              nivnames = nivnames + 1
              IVNAMES( nivnames ) = EVNAMES( k )
            END DO
          ELSE
            IF ( nivnames - INV( neltype ) >= nevnames - ELV( neltype ) ) THEN
              status = 76
              IF ( out > 0 ) WRITE( out, 2760 )
              RETURN
            END IF
          END IF
        END IF

!  record the name and starting address of the new element type

        CALL HASH_enlarge_and_insert( length, 12, field2 // 'ET',              &
                                      TABLE, KEY, INLIST, ifree )
        IF ( ifree <= 0 ) THEN
          IF ( ifree == 0 ) THEN
            status = - 1
            RETURN
          END IF
          status = 18
          IF ( out > 0 ) WRITE( out, 2180 )
          RETURN
        END IF
        neltype = neltype + 1
        inrep = .FALSE.

        IF ( neltype > len_elv ) THEN
          used_length = neltype - 1 ; min_length = neltype + 1
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( ELV, len_elv, used_length, new_length,            &
                             min_length, buffer, status, alloc_status,         &
                             'ELV' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'ELV' ; status = - 3 ; GO TO 980 ; END IF
          len_elv = new_length
        END IF

        IF ( neltype > len_inv ) THEN
          used_length = neltype - 1 ; min_length = neltype + 1
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( INV, len_inv, used_length, new_length,            &
                             min_length, buffer, status, alloc_status,         &
                             'INV' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'INV' ; status = - 3 ; GO TO 980 ; END IF
          len_inv = new_length
        END IF

        IF ( neltype > len_elp ) THEN
          used_length = neltype - 1 ; min_length = neltype + 1
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( ELP, len_elp, used_length, new_length,            &
                             min_length, buffer, status, alloc_status,         &
                             'ELP' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'ELP' ; status = - 3 ; GO TO 980 ; END IF
          len_elp = new_length
        END IF

        IF ( neltype > len_etypes ) THEN
          used_length = neltype - 1 ; min_length = neltype
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( ETYPES, len_etypes, used_length, new_length,      &
                             min_length, buffer, status, alloc_status,         &
                             'ETYPES' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'ETYPES' ; status = - 3 ; GO TO 980 ; END IF
          len_etypes = new_length
        END IF

        INLIST( ifree ) = neltype
        ELV( neltype ) = nevnames + 1
        INV( neltype ) = nivnames + 1
        ELP( neltype ) = nepnames + 1
        ETYPES( neltype ) = field2
      END IF

!  input the name of an internal variable

      IF ( field1 == 'IV' ) THEN
        IF ( novals > 0 ) THEN
          inrep = .TRUE.

          IF ( nivnames + novals > len_ivnames ) THEN
            used_length = nivnames ; min_length = nivnames + novals
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( IVNAMES, len_ivnames, used_length, new_length,  &
                               min_length, buffer, status, alloc_status,       &
                               'IVNAMES' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'IVNAMES' ; status = - 16 ; GO TO 980
            END IF
            len_ivnames = new_length
          END IF

          DO i = 1, novals

!  check the name has not already been used in the current element

            DO k = INV( neltype ), nivnames
              IF ( ( i == 1 .AND. field3 == IVNAMES( k ) ) .OR.                &
                   ( i == 2 .AND. field5 == IVNAMES( k ) ) ) THEN
                status = 12
                IF ( out > 0 ) WRITE( out, 2120 )
                RETURN
              END IF
            END DO

!  the name is new. record it in the array IVNAMES

            nivnames = nivnames + 1
            IF ( i == 1 ) THEN
              IVNAMES( nivnames ) = field3
            ELSE
              IVNAMES( nivnames ) = field5
            END IF
          END DO
        END IF
      ELSE

!  input the name of an elemental variable

        IF ( field1 == 'EV' ) THEN
          IF ( novals > 0 ) THEN
            DO i = 1, novals

!  check the name has not already been used in the current element

              DO k = ELV( neltype ), nevnames
                IF ( ( i == 1 .AND. field3 == EVNAMES( k ) ) .OR.              &
                     ( i == 2 .AND. field5 == EVNAMES( k ) ) ) THEN
                  status = 11
                  IF ( out > 0 ) WRITE( out, 2110 )
                  RETURN
                END IF
              END DO

!  the name is new. record it in the array EVNAMES

              nevnames = nevnames + 1
              IF ( nevnames > len_evnames ) THEN
                used_length = nevnames - 1 ; min_length = nevnames
                new_length = increase_n * min_length / increase_d + 1
                CALL EXTEND_array( EVNAMES, len_evnames, used_length,          &
                                   new_length, min_length, buffer,             &
                                   status, alloc_status, 'EVNAMES' )
                IF ( status /= 0 ) THEN
                  bad_alloc = 'EVNAMES' ; status = - 14 ; GO TO 980 ; END IF
                len_evnames = new_length
              END IF
              IF ( i == 1 ) THEN
                EVNAMES( nevnames ) = field3
              ELSE
                EVNAMES( nevnames ) = field5
              END IF
            END DO
          END IF

!  input the name of an element parameter

        ELSE
          IF ( field1 == 'EP' ) THEN
            IF ( novals > 0 ) THEN
              DO i = 1, novals

!  check the name has not already been used in the current element

                DO k = ELP( neltype ), nepnames
                  IF ( ( i == 1 .AND. field3 == EPNAMES( k ) ) .OR.            &
                       ( i == 2 .AND. field5 == EPNAMES( k ) ) ) THEN
                    status = 23
                    IF ( out > 0 ) WRITE( out, 2230 )
                    RETURN
                  END IF
                END DO

!  the name is new. record it in the array EPNAMES

                nepnames = nepnames + 1
                IF ( nepnames > len_epnames ) THEN
                  used_length = nepnames - 1 ; min_length = nepnames
                  new_length = increase_n * min_length / increase_d + 1
                  CALL EXTEND_array( EPNAMES, len_epnames, used_length,        &
                                     new_length, min_length, buffer,           &
                                     status, alloc_status, 'EPNAMES' )
                  IF ( status /= 0 ) THEN
                    bad_alloc = 'EPNAMES' ; status = - 19 ; GO TO 980 ; END IF
                  len_epnames = new_length
                END IF
                IF ( i == 1 ) THEN
                  EPNAMES( nepnames ) = field3
                ELSE
                  EPNAMES( nepnames ) = field5
                END IF
              END DO
            END IF

!  field1 not recognised

          ELSE
            status = 10
            IF ( out > 0 ) WRITE( out, 2100 ) field1
            RETURN
          END IF
        END IF
      END IF
      status = 0
      RETURN

!  allocation errors

  980 CONTINUE
      WRITE( out, "( ' ** Message from INTERPRET_gpsmps-',                     &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  non-executable statements

 2100 FORMAT( ' ** Exit from INTERPRET_gpsmps - field 1 ', A2,                 &
              '  not recognised in ELEMENT TYPE section ' )
 2110 FORMAT( ' ** Exit from INTERPRET_gpsmps - duplicate elemental var. name' )
 2120 FORMAT( ' ** Exit from INTERPRET_gpsmps - duplicate internal var. name ' )
 2180 FORMAT( ' ** Exit from INTERPRET_gpsmps - duplicate element-type name ' )
 2230 FORMAT( ' ** Exit from INTERPRET_gpsmps - duplicate elemental',          &
              ' parameter name')
 2760 FORMAT( ' ** Exit from INTERPRET_gpsmps - #internal vars >= #elementals' )

!  end of subroutine SETYPE

      END SUBROUTINE SETYPE

!-*-*-*-*-*- S I F D E C O D E   S E U S E S    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE SEUSES( neltype, nevnames, nepnames, nelvar, nlisep,          &
                         novals, nelnum, nelmnt, n, elmnt, ELV, ELP,           &
                         len_typee, TYPEE, len_elvar, ELVAR,                   &
                         len_ev_ptr, EV_ptr, len_ep_ptr, EP_ptr,               &
                         delset, detype,                                       &
                         field1, field2, field3, value4, field5, value6,       &
                         len_ep_val, EP_val, EVNAMES, EPNAMES,                 &
                         len_lnames, LNAMES, len_vnames, VNAMES,               &
                         length, TABLE, KEY, INLIST, out, status )
      INTEGER :: out, status, length
      INTEGER :: neltype, nevnames, nepnames, nelvar
      INTEGER :: nelnum, nlisep, len_elvar, len_ep_val, len_vnames
      INTEGER :: novals, nelmnt, n
      INTEGER :: len_lnames, len_ev_ptr, len_typee, len_ep_ptr
      REAL ( KIND = wp ) :: value4, value6
      LOGICAL :: delset
      CHARACTER ( LEN = 2 ) :: field1
      CHARACTER ( LEN = 10 ) :: field2, field3, field5, elmnt, detype
      INTEGER, DIMENSION( neltype + 1 ) :: ELV, ELP
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TABLE, INLIST
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: ELVAR, TYPEE, EV_ptr, EP_ptr
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: EP_val
      CHARACTER ( LEN = 10 ), DIMENSION( nevnames ) :: EVNAMES
      CHARACTER ( LEN = 10 ), DIMENSION( nepnames ) :: EPNAMES
      CHARACTER ( LEN = 12 ), ALLOCATABLE, DIMENSION( : ) :: KEY
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: LNAMES, VNAMES

!  ------------------------------
!  indicator card is element uses
!  ------------------------------

!  local variables

      INTEGER :: i, ifree, ifield, ip, is, j, k, mlisep, mlisev, nepars, nevars
      INTEGER :: used_length, new_length, min_length, alloc_status
      CHARACTER ( LEN = 12 ) :: field
      CHARACTER ( LEN = 24 ) :: bad_alloc

!  the current card defines a default type

      IF ( field2 == '''DEFAULT'' ' ) THEN
        IF ( delset ) THEN
          status = 26
          IF ( out > 0 ) WRITE( out, 2260 )
        END IF
        delset = .TRUE.
        detype = field3

!  if the element named in field2 is not that of the previous card,
!  determine the characteristics of the element

      ELSE
        IF ( elmnt /= field2 ) THEN

!  look the name up in the dictionary to see if it already exists

          CALL HASH_enlarge_and_insert( length, 12, field2 // 'EL',            &
                                        TABLE, KEY, INLIST, ifree )

!  if the element name is recognised, recover its characteristics

          IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
              status = - 1
              RETURN
            END IF
            nelmnt = INLIST( - ifree )

!  the new element is the nelnum-th nonlinear element

          ELSE
            nelnum = nelnum + 1
            IF ( nelnum > len_lnames ) THEN
              used_length = nelnum - 1 ; min_length = nelnum
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( LNAMES, len_lnames, used_length, new_length,  &
                                 min_length, buffer, status, alloc_status,     &
                                 'LNAMES' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'LNAMES' ; status = - 9 ; GO TO 980 ; END IF
              len_lnames = new_length
            END IF

            IF ( nelnum > len_typee ) THEN
              used_length = nelnum - 1 ; min_length = nelnum
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( TYPEE, len_typee, used_length, new_length,    &
                                 min_length, buffer, status, alloc_status,     &
                                 'TYPEE' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'TYPEE' ; status = - 9 ; GO TO 980 ; END IF
              len_typee = new_length
            END IF

            IF ( nelnum + 1 > len_ep_ptr ) THEN
              used_length = nelnum - 1 ; min_length = nelnum + 1
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( EP_ptr, len_ep_ptr, used_length, new_length,  &
                                 min_length, buffer, status, alloc_status,     &
                                 'EP_ptr' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'EP_ptr' ; status = - 9 ; GO TO 980 ; END IF
              len_ep_ptr = new_length
            END IF

            IF ( nelnum + 1 > len_ev_ptr ) THEN
              used_length = nelnum - 1 ; min_length = nelnum + 1
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( EV_ptr, len_ev_ptr, used_length, new_length,  &
                                 min_length, buffer, status, alloc_status,     &
                                 'EV_ptr' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'EV_ptr' ; status = - 9 ; GO TO 980 ; END IF
              len_ev_ptr = new_length
            END IF

!  record the elements position in the table along with its name

            INLIST( ifree ) = nelnum
            LNAMES( nelnum ) = field2
            nelmnt = nelnum
          END IF

!  record the nonlinear element's name

          elmnt = field2

!  if the element has not yet been allocated a type, set it

          IF ( field1 == 'T ' .OR. field1 == 'XT' .OR. ifree > 0 ) THEN

!  record the element type

            IF ( field1 == 'T ' .OR. field1 == 'XT' ) THEN
              field = field3 // 'ET'
            ELSE
              IF ( delset ) THEN
                field = detype // 'ET'

!  the element name is new. check that if a default element type
!  is required, a default has been set

              ELSE
                 status = 41
                 IF ( out > 0 ) WRITE( out, 2410 )
                 RETURN
               END IF
            END IF

!  if the group is non-trivial, determine its characteristics

            CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
            IF ( ifield <= 0 ) THEN
              status = 9
              IF ( out > 0 ) WRITE( out, 2090 ) field3
              RETURN
            END IF

!  determine the number of the element type, k, the starting addresses for
!  the parameters and variables for the element and the number of parameters
!  and variables involved

            k = INLIST( ifield )
            TYPEE( nelnum ) = k
            EP_ptr ( nelnum ) = nlisep + 1
            EV_ptr( nelnum ) = nelvar + 1

            nepars = ELP( k + 1 ) - ELP( k )
            nevars = ELV( k + 1 ) - ELV( k )
            IF ( nelvar + nevars > len_elvar ) THEN
              used_length = nelvar ; min_length = nelvar + nevars
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( ELVAR, len_elvar, used_length, new_length,    &
                                 min_length, buffer, status, alloc_status,     &
                                 'ELVAR' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'ELVAR' ; status = - 15 ; GO TO 980 ; END IF
              len_elvar = new_length
            END IF

            IF ( nlisep + nepars > len_ep_val ) THEN
              used_length = nlisep ; min_length =  nlisep + nepars
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( EP_val, len_ep_val, used_length, new_length,  &
                                 min_length, buffer, status, alloc_status,     &
                                 'EP_val' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'EP_val' ; status = - 17 ; GO TO 980 ; END IF
              len_ep_val = new_length
            END IF

!  initialize the set of problem variables

            DO i = 1, nevars
              ELVAR( nelvar + i ) = 0
            END DO

!  initialize the set of parameter values

            DO i = 1, nepars
              EP_val( nlisep + i ) = biginf
            END DO

!  find the starting addresses for the lists of the elements
!  parameters and variables

            nlisep = nlisep + nepars
            nelvar = nelvar + nevars
            IF ( field1 == 'T ' .OR. field1 == 'XT' ) RETURN
          END IF
        END IF

!  check that the cards are in the correct order

        IF ( field1 == 'T ' .OR. field1 == 'XT' ) THEN
          status = 27
          IF ( out > 0 ) WRITE( out, 2270 )
          RETURN
        END IF

!  determine the number of the element type, k, the starting
!  addresses for the parameters and variables for the element
!  and the number of parameters and variables involved

        k = TYPEE( nelmnt )
        nepars = ELP( k + 1 ) - ELP( k )
        nevars = ELV( k + 1 ) - ELV( k )
        mlisep = EP_ptr( nelmnt ) - 1
        mlisev = EV_ptr( nelmnt ) - 1
        ip = ELP( k ) - 1
        is = ELV( k ) - 1

!  the card contains names of elemental variables

        IF ( field1 == 'V ' .OR. field1 == 'ZV' ) THEN

!  the elemental variable is defined in field3

          DO i = 1, nevars
            IF ( field3 == EVNAMES( is + i ) ) GO TO 120
          END DO

!  the elemental variable name is not recognised

          status = 15
          IF ( out > 0 ) WRITE( out, 2150 )
          RETURN

!  check that the variable has not already been set

  120     CONTINUE
          IF ( ELVAR( mlisev + i ) /= 0 ) THEN
            status = 30
            IF ( out > 0 ) WRITE( out, 2300 )
            RETURN
          END IF

!  search the table for the name of the input variable

          CALL HASH_enlarge_and_insert( length, 12, FIELD5//'VA',              &
                                        TABLE, KEY, INLIST, ifree )
          IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
              status = - 1
              RETURN
            END IF

!  the variable has appeared before. store its number

            ELVAR( mlisev + i ) = INLIST( - ifree )

!  the variable is completely new (and thus nonlinear)
!  it will be recorded as variable n

          ELSE
            n = n + 1
            IF ( n > len_vnames ) THEN
              used_length = n - 1 ; min_length = n
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( VNAMES, len_vnames, used_length, new_length,  &
                                 min_length, buffer, status, alloc_status,     &
                                 'VNAMES' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'VNAMES' ; status = - 7 ; GO TO 980 ; END IF
              len_vnames = new_length
            END IF

!  record the position of the new group in the table, record its
!  name, initialise its type as trivial and record its status as
!  an equality group

            INLIST( ifree ) = n
            VNAMES( n ) = field5
            ELVAR( mlisev + i ) = n
          END IF

!  the card contains names and values of elemental parameters

        ELSE
          IF ( field1 == 'P ' .OR. field1 == 'XP' .OR. field1 == 'ZP' ) THEN
            IF ( novals > 0 ) THEN
              DO j = 1, novals

!  check the name has not already been used in the current element
!  the parameter name occurs in field3 or field5

                DO i = 1, nepars
                  IF ( ( j == 1 .AND. field3 == EPNAMES( ip + i ) ) .OR.       &
                       ( j == 2 .AND. field5 == EPNAMES( ip + i ) ) ) GO TO 220
                END DO

!  the elemental parameter name is not recognised

                status = 28
                IF ( out > 0 )                                                 &
                  WRITE( out, 2280 ) LNAMES( nelnum ), EPNAMES( ip + i )
                RETURN

!  the elemental parameter name is the i-th parameter in the list

  220           CONTINUE

!  check that the value has not already been set

                IF ( EP_val( mlisep + i ) < biginf ) THEN
                  status = 29
                  IF ( out > 0 ) WRITE( out, 2290 )
                  RETURN
                END IF

!  read the associated value from field4 or field 6

                IF ( j == 1 ) THEN
                  EP_val( mlisep + i ) = value4
                ELSE
                  EP_val( mlisep + i ) = value6
                END IF
              END DO
            END IF

!  field1 not recognised

          ELSE
            status = 10
            IF ( out > 0 ) WRITE( out, 2100 ) field1
            RETURN
          END IF
        END IF
      END IF
      status = 0
      RETURN

!  allocation errors

  980 CONTINUE
      WRITE( out, "( ' ** Message from INTERPRET_gpsmps-',                     &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  non-executable statements

 2090 FORMAT( ' ** Exit from INTERPRET_gpsmps - element type not recognised:', &
              ' name is ', A10 )
 2100 FORMAT( ' ** Exit from INTERPRET_gpsmps - field 1 ', A2,                 &
              '  not recognised in ELEMENT USES section ' )
 2150 FORMAT( ' ** Exit from INTERPRET_gpsmps - element variable unrecognised' )
 2260 FORMAT( ' ** Exit from INTERPRET_gpsmps - duplicate default element type')
 2270 FORMAT( ' ** Exit from INTERPRET_gpsmps - type for element already set ' )
 2280 FORMAT( ' ** Exit from INTERPRET_gpsmps - element ', A10, ' parameter ', &
              A10, ' unrecognised ' )
 2290 FORMAT( ' ** Exit from INTERPRET_gpsmps - element parameter already set' )
 2300 FORMAT( ' ** Exit from INTERPRET_gpsmps - element variable already set' )
 2410 FORMAT( ' ** Exit from INTERPRET_gpsmps - element type unrecognised ' )

!  end of subroutine SEUSES

      END SUBROUTINE SEUSES

!-*-*-*-*-*- S I F D E C O D E   S G T Y P E    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE SGTYPE( novals, ngtype, ngpnames, setana,                     &
                         len_gtypesp_ptr, GTYPESP_ptr,                         &
                         field1, field2, field3, field5,                       &
                         len_ganames, GANAMES, len_gtypes, GTYPES,             &
                         len_gpnames, GPNAMES,                                 &
                         length, TABLE, KEY, INLIST, out, status )
      INTEGER :: out, status, novals, length
      INTEGER :: ngpnames
      INTEGER :: ngtype, len_gtypesp_ptr, len_ganames, len_gtypes, len_gpnames
      LOGICAL :: setana
      CHARACTER ( LEN = 2 ) :: field1
      CHARACTER ( LEN = 10 ) :: field2, field3, field5
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: GTYPESP_ptr
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TABLE, INLIST
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: GANAMES, GTYPES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: GPNAMES
      CHARACTER ( LEN = 12 ), ALLOCATABLE, DIMENSION( : ) :: KEY

!  ----------------------------
!  indicator card is group type
!  ----------------------------

!  local variables

      INTEGER :: i, ifree, k
      INTEGER :: used_length, new_length, min_length, alloc_status
      CHARACTER ( LEN = 24 ) :: bad_alloc

!  check if this is the first group type

      IF ( ngtype == 0 ) THEN
        CALL HASH_enlarge_and_insert( length, 12, field2 // 'GT',              &
                                      TABLE, KEY, INLIST, ifree )
        IF ( ifree <= 0 ) THEN
          IF ( ifree == 0 ) THEN
            status = - 1
            RETURN
          END IF
          status = 17
          IF ( out > 0 ) WRITE( out, 2170 )
          RETURN
        END IF
        ngtype = 1
        ngpnames = 0
        setana = .FALSE.
        INLIST( ifree ) = ngtype
        GTYPESP_ptr( ngtype ) = ngpnames + 1
        GTYPES( ngtype ) = field2
      END IF

!  check if the group-type is new

      IF ( field2 /= GTYPES( ngtype ) ) THEN

!  check that the argument for the previous group-type has been set

        IF ( .NOT. setana ) THEN
          status = 25
          IF ( out > 0 ) WRITE( out, 2250 )
          RETURN
        END IF

!  record the name and starting address of the new group type

        CALL HASH_enlarge_and_insert( length, 12, field2 // 'GT',              &
                                      TABLE, KEY, INLIST, ifree )
        IF ( ifree <= 0 ) THEN
           IF ( ifree == 0 ) THEN
              status = - 1
              RETURN
           END IF
           status = 17
           IF ( out > 0 ) WRITE( out, 2170 )
           RETURN
        END IF
        ngtype = ngtype + 1
        setana = .FALSE.
        IF ( ngtype > len_gtypesp_ptr ) THEN
          used_length = ngtype - 1 ; min_length = ngtype + 1
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( GTYPESP_ptr, len_gtypesp_ptr, used_length,        &
                             new_length, min_length, buffer, status,           &
                             alloc_status, 'GTYPESP_ptr' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'GTYPESP_ptr' ; status = - 4 ; GO TO 980 ; END IF
          len_gtypesp_ptr = new_length
        END IF
        IF ( ngtype > len_gtypes ) THEN
          used_length = ngtype - 1 ; min_length = ngtype
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( GTYPES, len_gtypes, used_length, new_length,      &
                             min_length, buffer, status, alloc_status,         &
                             'GTYPES' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'GTYPES' ; status = - 4 ; GO TO 980 ; END IF
          len_gtypes = new_length
        END IF
        IF ( ngtype > len_ganames ) THEN
          used_length = ngtype - 1 ; min_length = ngtype
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( GANAMES, len_ganames, used_length, new_length,    &
                             min_length, buffer, status, alloc_status,         &
                             'GANAMES' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'GANAMES' ; status = - 4 ; GO TO 980 ; END IF
          len_ganames = new_length
        END IF
        INLIST( ifree ) = ngtype
        GTYPESP_ptr( ngtype ) = ngpnames + 1
        GTYPES( ngtype ) = field2
      END IF

!  input the name of the group-type argument

      IF ( field1 == 'GV' ) THEN
        setana = .TRUE.
        GANAMES( ngtype ) = field3
      ELSE

!  input the name of an group parameter

        IF ( field1 == 'GP' ) THEN
          IF ( novals > 0 ) THEN
            DO i = 1, novals

!  check the name has not already been used in the current group

              DO k = GTYPESP_ptr( ngtype ), ngpnames
                IF ( ( i == 1 .AND. field3 == GPNAMES( k ) ) .OR.              &
                     ( i == 2 .AND. field5 == GPNAMES( k ) ) ) THEN
                  status = 24
                  IF ( out > 0 ) WRITE( out, 2240 )
                  RETURN
                END IF
              END DO

!  the name is new. record it in the array GPNAMES

              ngpnames = ngpnames + 1
              IF ( ngpnames > len_gpnames ) THEN
                used_length = ngpnames - 1 ; min_length = ngpnames
                new_length = increase_n * min_length / increase_d + 1
                CALL EXTEND_array( GPNAMES, len_ganames, used_length,          &
                                   new_length, min_length, buffer,             &
                                   status, alloc_status, 'GPNAMES' )
                IF ( status /= 0 ) THEN
                  bad_alloc = 'GPNAMES' ; status = - 20 ; GO TO 980 ; END IF
                len_gpnames = new_length
              END IF
              IF ( i == 1 ) THEN
                GPNAMES( ngpnames ) = field3
              ELSE
                GPNAMES( ngpnames ) = field5
              END IF
            END DO
          END IF

!  field1 not recognised

        ELSE
          status = 10
          IF ( out > 0 ) WRITE( out, 2100 ) field1
          RETURN
        END IF
      END IF
      RETURN

!  allocation errors

  980 CONTINUE
      WRITE( out, "( ' ** Message from INTERPRET_gpsmps-',                     &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  non-executable statements

 2170 FORMAT( ' ** Exit from INTERPRET_gpsmps - duplicate group-type name ' )
 2100 FORMAT( ' ** Exit from INTERPRET_gpsmps - field 1 ', A2,                 &
              '  not recognised in GROUP TYPE section ' )
 2240 FORMAT( ' ** Exit from INTERPRET_gpsmps - duplicate group param. name ' )
 2250 FORMAT( ' ** Exit from INTERPRET_gpsmps - no group-type argument given ' )

!  end of subroutine SGTYPE

      END SUBROUTINE SGTYPE

!-*-*-*-*-*- S I F D E C O D E   S G U S E S    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE SGUSES( ng, ngtype, ngpnames, ngrupe, nlisgp, novals,         &
                         neling, ndtype, start_group_uses_section, grupe,      &
                         GTYPESP_ptr,                                          &
                         GTYPE, len_eling_el, ELING_el, len_eling_g, ELING_g,  &
                         GP_ptr, GSTATE, dgrset, dgtype,                       &
                         field1, field2, field3, value4, field5, value6,       &
                         len_gp_val_orig, GP_val_orig, GPNAMES,                &
                         len_weight, WEIGHT,                                   &
                         length, TABLE, KEY, INLIST, out, status )
      INTEGER :: length, out, status
      INTEGER :: ng, nlisgp, neling, novals, ngrupe, ndtype
      INTEGER :: ngtype, ngpnames, len_gp_val_orig
      INTEGER :: len_weight, len_eling_el, len_eling_g
      REAL ( KIND = wp ) :: value4, value6
      LOGICAL :: dgrset, start_group_uses_section
      CHARACTER ( LEN = 2 ) :: field1
      CHARACTER ( LEN = 10 ) :: field2, field3, field5, grupe, dgtype
      INTEGER, DIMENSION( ngtype + 1 ) :: GTYPESP_ptr
      INTEGER, DIMENSION( ng ) :: GTYPE, GSTATE
      INTEGER, DIMENSION( ng + 1 ) :: GP_ptr
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TABLE, INLIST
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: ELING_el, ELING_g
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: GP_val_orig
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: WEIGHT
      CHARACTER ( LEN = 10 ), DIMENSION( ngpnames ) :: GPNAMES
      CHARACTER ( LEN = 12 ), ALLOCATABLE, DIMENSION( : ) :: KEY

!  ----------------------------
!  indicator card is group uses
!  ----------------------------

!  local variables

      INTEGER :: i, ifield, ip, j, k, mlisgp, ngpars
      INTEGER :: used_length, new_length, min_length, alloc_status
      CHARACTER ( LEN = 12 ) :: field
      CHARACTER ( LEN = 24 ) :: bad_alloc

!  the current card defines a default type

      IF ( field2 == '''DEFAULT'' ' ) THEN
        IF ( dgrset ) THEN
          status = 42
          IF ( out > 0 ) WRITE( out, 2420 )
          RETURN
        END IF
        dgrset = .TRUE.
        dgtype = field3

!  find the number allocated to the group type

        CALL HASH_search( length, 12, dgtype // 'GT', TABLE, KEY, ifield )
        IF ( ifield <= 0 ) THEN
          status = 19
          IF ( out > 0 ) WRITE( out, 2190 ) field3
          RETURN
        END IF

!  reset the defaults for each of the groups allocated in previous
!  sections

        ndtype = INLIST( ifield )
        DO i = 1, ng
          IF ( GTYPE( i ) == - 1 ) GTYPE( i ) = - ndtype - 1
        END DO
        RETURN
      END IF

!  if the group named in field2 is not that of the previous card,
!  determine the characteristics of the group

      IF ( .NOT. start_group_uses_section .OR. grupe /= field2 ) THEN
        start_group_uses_section = .TRUE.

!  look the name up in the dictionary to see if it exists

        CALL HASH_search( length, 12, field2 // 'GR', TABLE, KEY, ifield )
        IF ( ifield > 0 ) THEN
          ngrupe = INLIST( ifield )
          GSTATE( ngrupe ) = - ABS( GSTATE( ngrupe ) )

!  the group name is unknown

        ELSE
          IF ( out > 0 ) WRITE( out, 2040 ) field2
          status = 4
          RETURN
        END IF

!  record the group's name

        grupe = field2

!  record the group type

        IF ( field1 == 'T ' .OR. field1 == 'XT' ) THEN
          field = field3 // 'GT'

!  if the group is non-trivial, determine its characteristics

          CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
          IF ( ifield <= 0 ) THEN
            status = 19
            IF ( out > 0 ) WRITE( out, 2190 ) field3
            RETURN
          END IF

!  determine the number of the group type, k, the starting addresses for the
!  parameters for the group and the number of parameters involved

          k = INLIST( ifield )
          IF ( k == 0 ) THEN
            ngpars = 0
          ELSE
            ngpars = GTYPESP_ptr( k + 1 ) - GTYPESP_ptr( k )
          END IF
          GTYPE( ngrupe ) = k
          GP_ptr( ngrupe ) = nlisgp + 1
          IF ( nlisgp + ngpars > len_gp_val_orig ) THEN
            used_length = nlisgp ; min_length = nlisgp + ngpars
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( GP_val_orig, len_gp_val_orig, used_length,      &
                               new_length, min_length, buffer,                 &
                               status, alloc_status, 'GP_val_orig' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'GTYPES' ; status = - 18 ; GO TO 980 ; END IF
            len_gp_val_orig = new_length
          END IF

!  initialize the set of parameter values

          DO i = 1, ngpars
            GP_val_orig( nlisgp + i ) = biginf
          END DO

!  find the starting addresses for the lists of the group parameters

          nlisgp = nlisgp + ngpars
          RETURN

!  the group is new and of default type. determine the starting addresses for
!  the parameters for the group and the number of parameters involved

        ELSE
          k = ndtype
          IF ( k == 0 ) THEN
            ngpars = 0
          ELSE
            ngpars = GTYPESP_ptr( k + 1 ) - GTYPESP_ptr( k )
          END IF
          GTYPE( ngrupe ) = k
          GP_ptr ( ngrupe ) = nlisgp + 1

          IF ( nlisgp + ngpars > len_gp_val_orig ) THEN
            used_length = nlisgp ; min_length = nlisgp + ngpars
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( GP_val_orig, len_gp_val_orig, used_length,      &
                               new_length, min_length, buffer,                 &
                               status, alloc_status,  'GP_val_orig' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'GTYPES' ; status = - 18 ; GO TO 980 ; END IF
            len_gp_val_orig = new_length
          END IF

!  initialize the set of parameter values

          DO i = 1, ngpars
            GP_val_orig( nlisgp + i ) = biginf
          END DO

!  find the starting addresses for the lists of the group parameters

          nlisgp = nlisgp + ngpars
        END IF
      END IF

!  check that the cards are in the correct order

      IF ( field1 == 'T ' .OR. field1 == 'XT' ) THEN
        status = 31
        IF ( out > 0 ) WRITE( out, 2310 )
        RETURN
      END IF

!  the card contains names of nonlinear elements

      IF ( field1 == 'E ' .OR. field1 == 'XE' .OR.                             &
           field1 == 'ZE' ) THEN
        IF ( novals > 0 ) THEN

!  check the name has not already been used in the current element
!  the parameter name occurs in field3 or field5

          DO i = 1, novals
            IF ( i == 1 ) THEN
               field = field3 // 'EL'
            ELSE
               field = field5 // 'EL'
            END IF
            CALL HASH_search( length, 12, field, TABLE, KEY, IFIELD)

!  the neling-th element has been assigned

            IF ( ifield > 0 ) THEN
              neling = neling + 1
              IF ( neling > len_eling_el ) THEN
                used_length = neling - 1 ; min_length = neling
                new_length = increase_n * min_length / increase_d + 1
                CALL EXTEND_array( ELING_el, len_eling_el, used_length,        &
                                   new_length, min_length, buffer,             &
                                   status, alloc_status, 'ELING_el' )
                IF ( status /= 0 ) THEN
                  bad_alloc = 'ELING_el' ; status = - 9 ; GO TO 980 ; END IF
                len_eling_el = new_length
              END IF

              IF ( neling > len_eling_g ) THEN
                used_length = neling - 1 ; min_length = neling
                new_length = increase_n * min_length / increase_d + 1
                CALL EXTEND_array( ELING_g, len_eling_g, used_length,          &
                                   new_length, min_length, buffer,             &
                                   status, alloc_status, 'ELING_g' )
                IF ( status /= 0 ) THEN
                  bad_alloc = 'ELING_g' ; status = - 9 ; GO TO 980 ; END IF
                len_eling_g = new_length
              END IF

              IF ( neling > len_weight ) THEN
                used_length = neling - 1 ; min_length = neling
                new_length = increase_n * min_length / increase_d + 1
                CALL EXTEND_array( WEIGHT, len_weight, used_length,            &
                                   new_length, min_length, buffer,             &
                                   status, alloc_status, 'WEIGHT' )
                IF ( status /= 0 ) THEN
                  bad_alloc = 'WEIGHT' ; status = - 9 ; GO TO 980 ; END IF
                len_weight = new_length
              END IF

!  the element is ELING_el and the group is given by ELING_g

              ELING_el( neling ) = INLIST( ifield )
              ELING_g( neling ) = ngrupe

!  the element is weighted by the constant in WEIGHT

              IF ( i == 1 ) THEN
                WEIGHT( neling ) = value4
              ELSE
                WEIGHT( neling ) = value6
              END IF

!  the element name is unknown

            ELSE
              status = 43
              IF ( out > 0 ) WRITE( out, 2430 )
              RETURN
            END IF
          END DO
        END IF

!  the card contains names and values of elemental parameters

      ELSE
        IF ( field1 == 'P ' .OR. field1 == 'XP' .OR. field1 == 'ZP' ) THEN

!  determine the number of the group type, k, the starting
!  addresses for the parameters for the group and the
!  number of parameters involved

          IF ( GTYPE( ngrupe ) < 0 ) GTYPE( ngrupe ) = - GTYPE( ngrupe ) - 1
          GTYPE( ngrupe ) = ABS( GTYPE( ngrupe ) )
          k = GTYPE( ngrupe )
          ngpars = GTYPESP_ptr( k + 1 ) - GTYPESP_ptr( k )
          ip = GTYPESP_ptr( k ) - 1
          mlisgp = GP_ptr( ngrupe ) - 1
          IF ( novals > 0 ) THEN

!  check the name has not already been used in the current element
!  the parameter name occurs in field3 or field5

            DO j = 1, novals
              DO i = 1, ngpars
                IF ( ( j == 1 .AND. field3 == GPNAMES( ip + i ) ) .OR.         &
                     ( j == 2 .AND. field5 == GPNAMES( ip + i ) ) ) GO TO 220
              END DO

!  the group parameter name is not recognised

              status = 33
              IF ( out > 0 ) WRITE( out, 2330 )
              RETURN

!  the group parameter name is the i-th parameter in the list

  220         CONTINUE

!  check that the value has not already been set

              IF ( GP_val_orig( mlisgp + i ) < biginf ) THEN
                status = 32
                IF ( out > 0 ) WRITE( out, 2320 )
                RETURN
              END IF

!  read the associated value from field4 or field 6

              IF ( j == 1 ) THEN
                GP_val_orig( mlisgp + i ) = value4
              ELSE
                GP_val_orig( mlisgp + i ) = value6
              END IF
            END DO
          END IF

!  field1 not recognised

        ELSE
          status = 10
          IF ( out > 0 ) WRITE( out, 2100 ) field1
          RETURN
        END IF
      END IF
      status = 0
      RETURN

!  allocation errors

  980 CONTINUE
      WRITE( out, "( ' ** Message from INTERPRET_gpsmps-',                     &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  non-executable statements

 2040 FORMAT( ' ** Exit from INTERPRET_gpsmps - group/row name not',           &
              ' recognised: name is ', A10 )
 2100 FORMAT( ' ** Exit from INTERPRET_gpsmps - field 1 ', A2,                 &
              '  not recognised in GROUP USES section' )
 2190 FORMAT( ' ** Exit from INTERPRET_gpsmps - group type not recognised:',   &
              ' name is ', A10 )
 2310 FORMAT( ' ** Exit from INTERPRET_gpsmps - type for group already set' )
 2320 FORMAT( ' ** Exit from INTERPRET_gpsmps - group parameter already set' )
 2330 FORMAT( ' ** Exit from INTERPRET_gpsmps - group parameter unrecognised' )
 2420 FORMAT( ' ** Exit from INTERPRET_gpsmps - default group type already set')
 2430 FORMAT( ' ** Exit from INTERPRET_gpsmps - element name not recognised' )

!  end of subroutine SGUSES

      END SUBROUTINE SGUSES

!-*-*-*-*-*- S I F D E C O D E   S O B B N D    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE SOBBND( nobbnd, nrival, len_fbound, FBOUND_l, FBOUND_u,       &
                         RIVAL, field1, field2, value4, field5,                &
                         len_obbname, OBBNAME,                                 &
                         length, TABLE, KEY, INLIST, single, out, status )
      INTEGER :: out, status, length, nobbnd, nrival
      INTEGER :: len_fbound, len_obbname
      REAL ( KIND = wp ) :: value4
      LOGICAL :: single
      CHARACTER ( LEN = 2 ) :: field1
      CHARACTER ( LEN = 10 ) :: field2, field5
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TABLE, INLIST
      REAL ( KIND = wp ), DIMENSION( nrival ) :: RIVAL
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: FBOUND_l, FBOUND_u
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: OBBNAME
      CHARACTER ( LEN = 12 ), ALLOCATABLE, DIMENSION( : ) :: KEY

!  ------------------------------
!  indicator card is object bound
!  ------------------------------

!  local variables

      INTEGER :: ifree, ifield, j
      INTEGER :: used_length, new_length, min_length, alloc_status
      REAL ( KIND = wp ) :: big
      CHARACTER ( LEN = 24 ) :: bad_alloc

      IF ( single ) THEN
        big = 9.0D-1 * HUGE( 1.0_sp )
      ELSE
        big = 9.0D-1 * HUGE( one )
      END IF

!  find a place to insert the objective bound name in the hash-table

      CALL HASH_enlarge_and_insert( length, 12, field2 // 'OB',                &
                                    TABLE, KEY, INLIST, ifree )

!  the name already exists. it is the j-th name in the list

      IF ( ifree <= 0 ) THEN
        IF ( ifree == 0 ) THEN
          status = - 1
          RETURN
        END IF
        j = INLIST( - ifree )

!  the objective function bound is the nobbnd-th specified

      ELSE
        nobbnd = nobbnd + 1 ; j = nobbnd
        IF ( nobbnd > len_obbname ) THEN
          used_length = nobbnd - 1 ; min_length = nobbnd
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( OBBNAME, len_obbname, used_length, new_length,    &
                             min_length, buffer, status, alloc_status,         &
                             'OBBNAME' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'OBBNAME' ; status = - 23 ; GO TO 980 ; END IF
          len_obbname = new_length
        END IF

        IF ( nobbnd > len_fbound ) THEN
          used_length = nobbnd - 1 ; min_length = nobbnd
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( FBOUND_l, len_fbound, used_length, new_length,    &
                             min_length, buffer, status, alloc_status,         &
                             'FBOUND_l' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'FBOUND_l' ; status = - 23 ; GO TO 980 ; END IF

          used_length = nobbnd - 1 ; min_length = nobbnd
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( FBOUND_u, len_fbound, used_length, new_length,    &
                             min_length, buffer, status, alloc_status,         &
                             'FBOUND_u' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'FBOUND_u' ; status = - 23 ; GO TO 980 ; END IF
          len_fbound = new_length
        END IF

!  record the default lower and upper bounds

        FBOUND_l( nobbnd ) = - big
        FBOUND_u( nobbnd ) = big

!  record the position of the new bound in the table and record its name

        INLIST( ifree ) = nobbnd
        OBBNAME( nobbnd ) = field2
      END IF

!  record the bound given

      IF ( field1( 1 : 1 ) == 'Z' ) THEN
        CALL HASH_search( length, 12,  FIELD5( 1 : 10 ) // 'RI', TABLE, KEY,   &
                          ifield )
        IF ( ifield <= 0 ) THEN
          status = 3
          IF ( out > 0 ) WRITE( out, 2030 ) FIELD5( 1 : 10 )
          RETURN
        END IF
        value4 = RIVAL( INLIST( ifield ) )
      END IF
      IF ( field1 == 'XL' .OR. field1 == 'ZL' .OR. field1 == 'LO' )            &
        FBOUND_l( j ) = value4
      IF ( field1 == 'XU' .OR. field1 == 'ZU' .OR. field1 == 'UP' )            &
        FBOUND_u( j ) = value4
      status = 0
      RETURN

!  allocation errors

  980 CONTINUE
      WRITE( out, "( ' ** Message from INTERPRET_gpsmps-',                     &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  non-executable statements

 2030 FORMAT( ' ** Exit from INTERPRET_gpsmps - index parameter name ', A10,   &
              ' not recognised ' )

!  end of subroutine SOBBND

      END SUBROUTINE SOBBND

!-   D E C O D E _ s c a l a r _ i n s t r u c t i o n    S U B R O U T I N E  -

      SUBROUTINE DECODE_scalar_instruction( niival, nrival,                    &
                         level, ninstr, debug, rvalue,                         &
                         len_iival, IIVAL, len_rival, RIVAL,                   &
                         len_iinames, IINAMES, len_rinames, RINAMES, INSTR,    &
                         field1, field2, field3, field5, field4,               &
                         length, TABLE, KEY, INLIST, out, status )
      INTEGER :: length, niival, nrival, level, ninstr
      INTEGER :: len_iival, len_rival, len_iinames, len_rinames
      INTEGER :: status, out
      LOGICAL :: debug
      REAL ( KIND = wp ) :: rvalue
      CHARACTER ( LEN =  2 ) :: field1
      CHARACTER ( LEN = 10 ) :: field2, field3, field5
      CHARACTER ( LEN = 12 ) :: field4
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: IIVAL
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: RIVAL
      INTEGER, DIMENSION( 5 ) :: INSTR
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TABLE, INLIST
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: IINAMES, RINAMES
      CHARACTER ( LEN = 12 ), ALLOCATABLE, DIMENSION( : ) :: KEY

!  ------------------------------------------------------
!  decode scalar integer and real arithmetic instructions
!  ------------------------------------------------------

!  local variables

      INTEGER :: i, ifield, ifree
      INTEGER :: used_length, new_length, min_length, alloc_status
      CHARACTER ( LEN = 12 ) :: field
      CHARACTER ( LEN = 24 ) :: bad_alloc

      rvalue = zero
      INSTR( 1 : 5 ) = (/ 0, 0, 0, 0, 0 /)

!  decide what sort of instruction is to be performed: integer instructions

      IF ( field1 == 'IE' .OR. field1 == 'IA' .OR.                             &
           field1 == 'IS' .OR. field1 == 'IM' .OR.                             &
           field1 == 'ID' .OR. field1 == 'IR' .OR.                             &
           field1 == 'I=' .OR. field1 == 'I+' .OR.                             &
           field1 == 'I-' .OR. field1 == 'I*' .OR.                             &
           field1 == 'I/' ) THEN
        IF ( field1 == 'IE' ) INSTR( 1 ) = 21
        IF ( field1 == 'IA' ) INSTR( 1 ) = 22
        IF ( field1 == 'IS' ) INSTR( 1 ) = 23
        IF ( field1 == 'IM' ) INSTR( 1 ) = 24
        IF ( field1 == 'ID' ) INSTR( 1 ) = 25
        IF ( field1 == 'IR' ) INSTR( 1 ) = 26
        IF ( field1 == 'I=' ) INSTR( 1 ) = 31
        IF ( field1 == 'I+' ) INSTR( 1 ) = 32
        IF ( field1 == 'I-' ) INSTR( 1 ) = 33
        IF ( field1 == 'I*' ) INSTR( 1 ) = 34
        IF ( field1 == 'I/' ) INSTR( 1 ) = 35

!  read the integer value, ivalue, from field 4

        IF ( field1 == 'IE' .OR. field1 == 'IA' .OR. field1 == 'IS' .OR.       &
             field1 == 'IM' .OR. field1 == 'ID' ) THEN
          CALL GET_integer( field4, INSTR( 4 ) )

!  obtain the integer value, ivalue, as the value of the index in
!  field 5, first ensuring that the index exists

        ELSE
          IF ( field1 /= 'I=' .AND. field1 /= 'IR' ) THEN
            field = FIELD5( 1 : 10 ) // 'II'
            CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
            IF ( ifield <= 0 ) THEN
              status = 3
              IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
              RETURN
            END IF
            INSTR( 4 ) = INLIST( ifield )
          END IF
        END IF

!  if a definition is to be made from a previously defined index,
!  ensure that the index exists

        IF ( field1 /= 'IE' ) THEN
          IF ( field1 /= 'IR' ) THEN
            field = FIELD3( 1 : 10 ) // 'II'
            CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
            IF ( ifield <= 0 ) THEN
              status = 3
              IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
              RETURN
            END IF
            INSTR( 3 ) = INLIST( ifield )
          ELSE
            field = FIELD3( 1 : 10 ) // 'RI'
            CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
            IF ( ifield <= 0 ) THEN
              status = 3
              IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
              RETURN
            END IF
            INSTR( 3 ) = INLIST( ifield )
          END IF
        END IF

!  record the address of the index which is to be set

        field = FIELD2( 1 : 10 ) // 'II'
        CALL HASH_enlarge_and_insert( length, 12, field,                       &
                                      TABLE, KEY, INLIST, ifree )
        IF ( ifree <= 0 ) THEN
          IF ( ifree == 0 ) THEN
            status = - 1
            RETURN
          END IF
          ifree = - ifree
        ELSE
          niival = niival + 1
          IF ( niival > len_iival ) THEN
            used_length = niival - 1 ; min_length = niival
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( IIVAL, len_iival, used_length, new_length,      &
                               min_length, buffer, status, alloc_status,       &
                               'IIVAL' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'IIVAL' ; status = - 21 ; GO TO 980 ; END IF
            len_iival = new_length
          END IF

          IF ( niival > len_iinames ) THEN
            used_length = niival - 1 ; min_length = niival
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( IINAMES, len_iinames, used_length, new_length,  &
                               min_length, buffer, status, alloc_status,       &
                               'IINAMES' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'IINAMES' ; status = - 21 ; GO TO 980 ; END IF
            len_iinames = new_length
          END IF
          INLIST( ifree ) = niival
          IINAMES( niival ) = FIELD( 1 : 10 )
        END IF
        INSTR( 2 ) = INLIST( ifree )

!  print details of the instruction

        IF ( debug .AND. out > 0 ) THEN
          IF ( INSTR( 1 ) == 21 )                                              &
             WRITE( out, 4030 ) level, ninstr,                                 &
             IINAMES( INSTR( 2 ) ), INSTR( 4 )
          IF ( INSTR( 1 ) == 22 )                                              &
             WRITE( out, 4040 ) level, ninstr,                                 &
             IINAMES( INSTR( 2 ) ), IINAMES( INSTR( 3 ) ), INSTR( 4 )
          IF ( INSTR( 1 ) == 23 )                                              &
             WRITE( out, 4041 ) level, ninstr,                                 &
             IINAMES( INSTR( 2 ) ), IINAMES( INSTR( 3 ) ), INSTR( 4 )
          IF ( INSTR( 1 ) == 24 )                                              &
             WRITE( out, 4050 ) level, ninstr,                                 &
             IINAMES( INSTR( 2 ) ), IINAMES( INSTR( 3 ) ), INSTR( 4 )
          IF ( INSTR( 1 ) == 25 )                                              &
             WRITE( out, 4051 ) level, ninstr,                                 &
             IINAMES( INSTR( 2 ) ), INSTR( 4 ), IINAMES( INSTR( 3 ) )
          IF ( INSTR( 1 ) == 26 )                                              &
             WRITE( out, 4055 ) level, ninstr,                                 &
             IINAMES( INSTR( 2 ) ), RINAMES( INSTR( 3 ) )
          IF ( INSTR( 1 ) == 31 )                                              &
             WRITE( out, 4059 ) level, ninstr,                                 &
             IINAMES( INSTR( 2 ) ), IINAMES( INSTR( 3 ) )
          IF ( INSTR( 1 ) == 32 )                                              &
             WRITE( out, 4060 ) level, ninstr,                                 &
             IINAMES( INSTR( 2 ) ), IINAMES( INSTR( 3 ) ), IINAMES( INSTR( 4 ) )
          IF ( INSTR( 1 ) == 33 )                                              &
             WRITE( out, 4061 ) level, ninstr,                                 &
             IINAMES( INSTR( 2 ) ), IINAMES( INSTR( 4 ) ), IINAMES( INSTR( 3 ) )
          IF ( INSTR( 1 ) == 34 )                                              &
             WRITE( out, 4070 ) level, ninstr,                                 &
             IINAMES( INSTR( 2 ) ), IINAMES( INSTR( 3 ) ), IINAMES( INSTR( 4 ) )
          IF ( INSTR( 1 ) == 35 )                                              &
             WRITE( out, 4071 ) level, ninstr,                                 &
             IINAMES( INSTR( 2 ) ), IINAMES( INSTR( 3 ) ), IINAMES( INSTR( 4 ) )
        END IF

!  real instructions

      ELSE
        IF ( field1 == 'RE' ) INSTR( 1 ) = 51
        IF ( field1 == 'RA' ) INSTR( 1 ) = 52
        IF ( field1 == 'RS' ) INSTR( 1 ) = 53
        IF ( field1 == 'RM' ) INSTR( 1 ) = 54
        IF ( field1 == 'RD' ) INSTR( 1 ) = 55
        IF ( field1 == 'RI' ) INSTR( 1 ) = 56
        IF ( field1 == 'RF' ) INSTR( 1 ) = 57
        IF ( field1 == 'R=' ) INSTR( 1 ) = 61
        IF ( field1 == 'R+' ) INSTR( 1 ) = 62
        IF ( field1 == 'R-' ) INSTR( 1 ) = 63
        IF ( field1 == 'R*' ) INSTR( 1 ) = 64
        IF ( field1 == 'R/' ) INSTR( 1 ) = 65
        IF ( field1 == 'R(' ) INSTR( 1 ) = 67

!  read the real value, rvalue, from field 4

        IF ( field1 == 'RE' .OR. field1 == 'RA' .OR. field1 == 'RS' .OR.       &
             field1 == 'RM' .OR. field1 == 'RD' .OR. field1 == 'RF' )          &
          CALL GET_value( field4, rvalue )

!  obtain the real value, rvalue, as the value associated with the real
!  index in field 5, first ensuring that the index exists

        IF ( field1 == 'R+' .OR. field1 == 'R-' .OR. field1 == 'R*' .OR.       &
             field1 == 'R/' .OR. field1 == 'R(' ) THEN
          field = FIELD5( 1 : 10 ) // 'RI'
          CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
          IF ( ifield <= 0 ) THEN
            status = 3
            IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
            RETURN
          END IF
          INSTR( 4 ) = INLIST( ifield )
        END IF

!  if a definition is to be made from a previously defined index,
!  ensure that the index exists

        IF ( field1 == 'RI' ) THEN
          field = FIELD3( 1 : 10 ) // 'II'
          CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
          IF ( ifield <= 0 ) THEN
            status = 3
            IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
            RETURN
          END IF
          INSTR( 3 ) = INLIST( ifield )
        ELSE
          IF ( field1 /= 'RF' .AND. field1 /= 'R(' .AND.                       &
               field1 /= 'RE' ) THEN
             field = FIELD3( 1 : 10 ) // 'RI'
             CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
             IF ( ifield <= 0 ) THEN
                status = 3
                IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
                RETURN
             END IF
             INSTR( 3 ) = INLIST( ifield )

!  the value is to be obtained using a special function. determine
!  which one

          ELSE
            IF ( field1 /= 'RE' ) THEN
              DO i = 1, nfunct
                IF ( FIELD3( 1 : 10 ) == FUNCTN( i ) ) GO TO 20
              END DO
              status = 39
              IF ( out > 0 ) WRITE( out, 2390 ) FIELD3( 1 : 10 )
              RETURN
   20         CONTINUE
              INSTR( 3 ) = i
            END IF
          END IF
        END IF

!  record the address of the index which is to be set

        field = FIELD2( 1 : 10 ) // 'RI'
        CALL HASH_enlarge_and_insert( length, 12, field,                       &
                                      TABLE, KEY, INLIST, ifree )
        IF ( ifree <= 0 ) THEN
          IF ( ifree == 0 ) THEN
            status = - 1
            RETURN
          END IF
          ifree = - ifree
        ELSE
          nrival = nrival + 1
          IF ( nrival > len_rival ) THEN
            used_length = nrival - 1 ; min_length = nrival
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( RIVAL, len_rival, used_length, new_length,      &
                               min_length, buffer, status, alloc_status,       &
                               'RIVAL' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'RIVAL' ; status = - 22 ; GO TO 980 ; END IF
            len_rival = new_length
          END IF

          IF ( nrival > len_rinames ) THEN
            used_length = nrival - 1 ; min_length = nrival
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( RINAMES, len_rinames, used_length, new_length,  &
                               min_length, buffer, status, alloc_status,       &
                               'RINAMES' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'RINAMES' ; status = - 22 ; GO TO 980 ; END IF
            len_rinames = new_length
          END IF
          INLIST( ifree ) = nrival
          RINAMES( nrival ) = FIELD( 1 : 10 )
        END IF
        INSTR( 2 ) = INLIST( ifree )

!  print details of the instruction

        IF ( debug .AND. out > 0 ) THEN
          IF ( INSTR( 1 ) == 51 )                                              &
             WRITE( out, 4130 ) level, ninstr,                                 &
             RINAMES( INSTR( 2 ) ), rvalue
          IF ( INSTR( 1 ) == 52 )                                              &
             WRITE( out, 4140 ) level, ninstr,                                 &
             RINAMES( INSTR( 2 ) ), RINAMES( INSTR( 3 ) ), rvalue
          IF ( INSTR( 1 ) == 53 )                                              &
             WRITE( out, 4141 ) level, ninstr,                                 &
             RINAMES( INSTR( 2 ) ), RINAMES( INSTR( 3 ) ), rvalue
          IF ( INSTR( 1 ) == 54 )                                              &
             WRITE( out, 4150 ) level, ninstr,                                 &
             RINAMES( INSTR( 2 ) ), RINAMES( INSTR( 3 ) ), rvalue
          IF ( INSTR( 1 ) == 55 )                                              &
             WRITE( out, 4151 ) level, ninstr,                                 &
             RINAMES( INSTR( 2 ) ), rvalue, RINAMES( INSTR( 3 ) )
          IF ( INSTR( 1 ) == 56 )                                              &
             WRITE( out, 4180 ) level, ninstr,                                 &
             RINAMES( INSTR( 2 ) ), IINAMES( INSTR( 3 ) )
          IF ( INSTR( 1 ) == 57 )                                              &
             WRITE( out, 4110 ) level, ninstr,                                 &
             RINAMES( INSTR( 2 ) ), FUNCTN( INSTR( 3 ) ), rvalue
          IF ( INSTR( 1 ) == 61 )                                              &
             WRITE( out, 4159 ) level, ninstr,                                 &
             RINAMES( INSTR( 2 ) ), RINAMES( INSTR( 3 ) )
          IF ( INSTR( 1 ) == 62 )                                              &
             WRITE( out, 4160 ) level, ninstr,                                 &
             RINAMES( INSTR( 2 ) ), RINAMES( INSTR( 3 ) ), RINAMES( INSTR( 4 ) )
          IF ( INSTR( 1 ) == 63 )                                              &
             WRITE( out, 4161 ) level, ninstr,                                 &
             RINAMES( INSTR( 2 ) ), RINAMES( INSTR( 4 ) ), RINAMES( INSTR( 3 ) )
          IF ( INSTR( 1 ) == 64 )                                              &
             WRITE( out, 4170 ) level, ninstr,                                 &
             RINAMES( INSTR( 2 ) ), RINAMES( INSTR( 3 ) ), RINAMES( INSTR( 4 ) )
          IF ( INSTR( 1 ) == 65 )                                              &
             WRITE( out, 4171 ) level, ninstr,                                 &
             RINAMES( INSTR( 2 ) ), RINAMES( INSTR( 3 ) ), RINAMES( INSTR( 4 ) )
          IF ( INSTR( 1 ) == 67 )                                              &
             WRITE( out, 4120 ) level, ninstr, RINAMES( INSTR( 2 ) ),          &
             FUNCTN( INSTR( 3 ) ), RINAMES( INSTR( 4 ) )
        END IF
      END IF
      RETURN

!  allocation errors

  980 CONTINUE
      WRITE( out, "( ' ** Message from INTERPRET_gpsmps-',                     &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  non-executable statements

 2030 FORMAT( ' ** Exit from INTERPRET_gpsmps - index parameter name ', A10,   &
              ' not recognised ' )
 2390 FORMAT( ' ** Exit from INTERPRET_gpsmps - specified function name ',     &
              A10, ' not recognised ' )
 4030 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' to the value ', i6 )
 4040 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by adding ', A10, ' to the value ', i6 )
 4041 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by subtracting ', A10, ' from the value ', i6 )
 4050 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by multiplying ', A10, ' by the value ', i6 )
 4051 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by dividing the value ', i6, ' by ', A10 )
 4055 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' to the integer equivalent of ', A10 )
 4059 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' to ', A10 )
 4060 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by adding ', A10, ' to ', A10 )
 4061 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by subtracting ', A10, ' from ', A10 )
 4070 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by multiplying ', A10, ' and ', A10 )
 4071 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by dividing ', A10, ' by ', A10 )
 4110 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' to the value ', A6, '(', 1P, D12.4, ')' )
 4120 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' to the value ', A6, '(', A10, ')' )
 4130 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' to the value ', 1P, D12.4 )
 4140 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by adding ', A10, ' to the value ', 1P, D12.4 )
 4141 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by subtracting ', A10, ' from the value ', 1P, D12.4 )
 4150 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by multiplying ', A10, ' by the value ', 1P, D12.4 )
 4151 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by dividing the value ', 1P, D12.4, ' by ', A10 )
 4159 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' to ', A10 )
 4160 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by adding ', A10, ' to ', A10 )
 4161 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by subtracting ', A10, ' from ', A10 )
 4170 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by multiplying ', A10, ' and ', A10 )
 4171 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by dividing ', A10, ' by ', A10 )
 4180 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' to the fl. pt. value of ', A10 )

!  end of subroutine DECODE_scalar_instruction

      END SUBROUTINE DECODE_scalar_instruction

!-   D E C O D E _ a r r a y _ i n s t r u c t i o n    S U B R O U T I N E   -

      SUBROUTINE DECODE_array_instruction(                                     &
                         level, ninstr, niival, nrival,                        &
                         narray, intype, debug, grp1st,                        &
                         field1, field2, field3, field5, field4, field6,       &
                         INSTR, IARRAY, VARRAY, farray,                        &
                         len_rival, RIVAL, IINAMES, len_rinames, RINAMES,      &
                         ARRAY, CARRAY,                                        &
                         length, TABLE, KEY, INLIST, out, status )
      INTEGER :: level, length, status, out
      INTEGER :: len_rival, len_rinames
      INTEGER :: ninstr, niival, nrival, intype, narray
      LOGICAL :: debug, grp1st
      CHARACTER ( LEN =  2 ) :: farray, field1
      CHARACTER ( LEN = 10 ) :: field2, field3, field5
      CHARACTER ( LEN = 12 ) :: field4, field6
      INTEGER, DIMENSION( 5 ) :: INSTR
      INTEGER, DIMENSION( 5, 3 ) :: IARRAY
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TABLE, INLIST
      REAL ( KIND = wp ), DIMENSION( 2 ) :: VARRAY
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: RIVAL
      CHARACTER ( LEN = 10 ), DIMENSION( niival ) :: IINAMES
      CHARACTER ( LEN = 10 ), DIMENSION( 2 ) :: CARRAY
      CHARACTER ( LEN = 10 ), DIMENSION( 3 ) :: ARRAY
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: RINAMES
      CHARACTER ( LEN = 12 ), ALLOCATABLE, DIMENSION( : ) :: KEY

!  -----------------------------------------
!  decode array real arithmetic instructions
!  -----------------------------------------

!  parameter definitions

      INTEGER, PARAMETER :: mblank = 1, mfixed = 2, mfree = 3, mname = 4
      INTEGER, PARAMETER :: mrows = 5, mgroup = 6, mcnstr =  7, mcols =  8
      INTEGER, PARAMETER :: mvars =  9, mconst = 10, mrhs = 11, mrhsp = 12
      INTEGER, PARAMETER :: mrange = 13, mbound = 14, mstart = 15, mqhess = 16
      INTEGER, PARAMETER :: mquadr = 17, mquads = 18, mquado = 19, mqsect = 20
      INTEGER, PARAMETER :: mqmatr = 21, metype = 22, meuses = 23, mgtype = 24
      INTEGER, PARAMETER :: mguses = 25, mobbnd = 26, mendat = 27

!  local variables

      INTEGER :: i, kindar, ifree
      INTEGER :: used_length, new_length, min_length, alloc_status
      CHARACTER ( LEN = 24 ) :: bad_alloc

      INSTR( 1 : 5 ) = (/ 0, 0, 0, 0, 0 /)

!  determine how much information must be saved by determining
!  the kind of array definition being made

      kindar = - 1

!  real index array definitions

      IF ( field1 == 'AE' .OR. field1 == 'AA' .OR.                             &
           field1 == 'AS' .OR. field1 == 'AM' .OR.                             &
           field1 == 'AD' .OR. field1 == 'AI' .OR.                             &
           field1 == 'A=' .OR. field1 == 'A+' .OR.                             &
           field1 == 'A-' .OR. field1 == 'A*' .OR.                             &
           field1 == 'A/' .OR. field1 == 'AF' .OR.                             &
           field1 == 'A(' ) THEN
        IF ( field1 == 'AE' ) kindar = 107
        IF ( field1 == 'AF' ) kindar = 108
        IF ( field1 == 'A(' ) kindar = 105
        IF ( field1 == 'AA' ) kindar = 101
        IF ( field1 == 'AS' ) kindar = 101
        IF ( field1 == 'AM' ) kindar = 101
        IF ( field1 == 'AD' ) kindar = 101
        IF ( field1 == 'A=' ) kindar = 103
        IF ( field1 == 'A+' ) kindar = 104
        IF ( field1 == 'A-' ) kindar = 104
        IF ( field1 == 'A*' ) kindar = 104
        IF ( field1 == 'A/' ) kindar = 104
        IF ( field1 == 'AI' ) kindar = 106

!  groups section

      ELSE
        IF ( intype == mrows  .OR. intype == mgroup .OR. intype == mcnstr ) THEN
          IF ( field1( 2 : 2 ) == 'N' .OR. field1( 2 : 2 ) == 'G' .OR.         &
               field1( 2 : 2 ) == 'L' .OR. field1( 2 : 2 ) == 'E' ) THEN
            IF ( grp1st ) THEN
              IF ( FIELD3( 1 : 7 ) == '''SCALE''' ) THEN
                IF ( field1( 1 : 1 )  == 'Z' ) THEN
                  kindar = 115
                ELSE
                  kindar = 108
                END IF
              ELSE
                kindar = 100
              END IF
            ELSE
              IF ( FIELD3( 1 : 7 ) == '''SCALE''' ) THEN
                IF ( field1( 1 : 1 )  == 'Z' ) THEN
                  kindar = 115
                ELSE
                   kindar = 108
                END IF
              ELSE
                IF ( FIELD3( 1 : 10 ) == '          ' ) THEN
                  kindar = 100
                ELSE
                  IF ( field1( 1 : 1 )  == 'Z' ) THEN
                    kindar = 113
                  ELSE
                    IF ( FIELD5( 1 : 10 ) == '          ' ) THEN
                      kindar = 101
                    ELSE
                      kindar = 102
                    END IF
                  END IF
                END IF
              END IF
            END IF
          END IF
        END IF

!  variables section

        IF ( intype == mcols  .OR. intype == mvars ) THEN
          IF ( grp1st ) THEN
            IF ( FIELD3( 1 : 7 ) == '''SCALE''' ) THEN
              IF ( field1( 1 : 1 )  == 'Z' ) THEN
                kindar = 115
              ELSE
                kindar = 108
              END IF
            ELSE IF ( field3 == '''ZERO-ONE''' .OR. field3 == '''INTEGER'' ' ) &
                THEN
              kindar = 106
            ELSE
              IF ( FIELD3( 1 : 10 ) == '          ' ) THEN
                kindar = 100
              ELSE
                IF ( field1( 1 : 1 )  == 'Z' ) THEN
                  kindar = 113
                ELSE
                  IF ( FIELD5( 1 : 10 ) == '          ' ) THEN
                    kindar = 101
                  ELSE
                    kindar = 102
                  END IF
                END IF
              END IF
            END IF
          ELSE
            IF ( FIELD3( 1 : 7 ) == '''SCALE''' ) THEN
               IF ( field1( 1 : 1 )  == 'Z' ) THEN
                  kindar = 115
               ELSE
                  kindar = 108
               END IF
            ELSE IF ( field3 == '''ZERO-ONE''' .OR. field3 == '''INTEGER'' ' ) &
                THEN
              kindar = 106
            ELSE
              kindar = 100
            END IF
          END IF
        END IF

!  constants section

        IF ( intype == mconst .OR. intype == mrhs  .OR. intype == mrhsp  ) THEN
          IF ( field1( 1 : 1 )  == 'Z' ) THEN
            kindar = 116
          ELSE
            IF ( FIELD5( 1 : 10 ) == '          ' ) THEN
              kindar = 111
            ELSE
              kindar = 112
            END IF
          END IF
        END IF

!  ranges section

        IF ( intype == mrange ) THEN
          IF ( field1( 1 : 1 )  == 'Z' ) THEN
            kindar = 116
          ELSE
            IF ( FIELD5( 1 : 10 ) == '          ' ) THEN
              kindar = 111
            ELSE
              kindar = 112
            END IF
          END IF
        END IF

!  bounds section

        IF ( intype == mbound ) THEN
          IF ( field1( 1 : 1 )  == 'Z' ) THEN
            kindar = 116
          ELSE
            IF ( field1( 2 : 2 ) == 'R' .OR. field1( 2 : 2 ) == 'M' .OR.       &
                 field1( 2 : 2 ) == 'P' ) kindar = 110
            IF ( field1( 2 : 2 ) == 'L' .OR. field1( 2 : 2 ) == 'U' .OR.       &
                 field1( 2 : 2 ) == 'X' ) kindar = 111
          END IF
        END IF

!  start point section

        IF ( intype == mstart ) THEN
          IF ( field1( 1 : 1 )  == 'Z' ) THEN
            kindar = 116
          ELSE
            IF ( FIELD5( 1 : 10 ) == '          ' ) THEN
              kindar = 111
            ELSE
              kindar = 112
            END IF
          END IF
        END IF

!  Hessian section

        IF ( intype == mquadr .OR. intype == mquads .OR. intype == mquado .OR. &
             intype == mqsect .OR. intype == mqhess .OR. intype == MQMATR) THEN
          IF ( field1( 1 : 1 )  == 'Z' ) THEN
            kindar = 113
          ELSE
            IF ( FIELD5( 1 : 10 ) == '          ' ) THEN
              kindar = 101
            ELSE
              kindar = 102
            END IF
          END IF
        END IF

!  element uses section

        IF ( intype == meuses ) THEN
          IF ( field1( 2 : 2 ) == 'T' ) kindar = 106
          IF ( field1( 2 : 2 ) == 'V' ) kindar = 105
          IF ( field1( 2 : 2 ) == 'P' ) THEN
            IF ( field1( 1 : 1 )  == 'Z' ) THEN
              kindar = 115
            ELSE
              IF ( FIELD5( 1 : 10 ) == '          ' ) THEN
                kindar = 108
              ELSE
                kindar = 109
              END IF
            END IF
          END IF
        END IF

!  group uses section

        IF ( intype == mguses ) THEN
          IF ( field1( 2 : 2 ) == 'T' ) kindar = 106
          IF ( field1( 2 : 2 ) == 'E' ) THEN
            IF ( field1( 1 : 1 )  == 'Z' ) THEN
              kindar = 113
            ELSE
              IF ( FIELD5( 1 : 10 ) == '          ' ) THEN
                kindar = 101
              ELSE
                kindar = 102
                IF ( FIELD6( 1 : 12 ) == '            ' )                      &
                       FIELD6( 1 : 3 ) = '1.0'
              END IF
              IF ( FIELD4( 1 : 12 ) == '            ' ) FIELD4( 1 : 3 ) = '1.0'
            END IF
          END IF
          IF ( field1( 2 : 2 ) == 'P' ) THEN
            IF ( field1( 1 : 1 )  == 'Z' ) THEN
              kindar = 115
            ELSE
              IF ( FIELD5( 1 : 10 ) == '          ' ) THEN
                kindar = 108
              ELSE
                kindar = 109
              END IF
            END IF
          END IF
        END IF

!  ranges section

        IF ( intype == mobbnd ) THEN
          IF ( field1( 1 : 1 )  == 'Z' ) THEN
            kindar = 116
          ELSE
            IF ( FIELD5( 1 : 10 ) == '          ' ) THEN
              kindar = 111
            ELSE
              kindar = 112
            END IF
          END IF
        END IF
      END IF

!  check that the type of array definition has been recognised

      IF ( kindar < 0 ) THEN
        IF ( out > 0 ) WRITE( out, 2140 )
        status = 14
        RETURN
      ELSE
        farray = field1
        INSTR( 1 ) = kindar
        INSTR( 2 ) = narray
      END IF

!  an array name occurs in field 2. interpret the contents of this
!  field

      IF ( ( kindar >= 100 .AND. kindar <= 109 ) .OR.                          &
           ( kindar >= 113 .AND. kindar <= 115 ) ) THEN
        CALL INTERPRET_field( 12, field2, ARRAY( 1 ), IARRAY( 1, 1 ),          &
                              length, TABLE, KEY, INLIST, out, status )
        IF ( status /= 0 ) RETURN
        IF ( debug .AND. out > 0 ) WRITE( out, 4080 ) level, ninstr,           &
           ARRAY( 1 ), ( IINAMES( IARRAY( 2 + i, 1 ) ), i = 1, IARRAY( 2, 1 ) )

!  if the array name is just a scalar name, record its name

        IF ( field1 == 'AE' .OR. field1 == 'AA' .OR. field1 == 'AS' .OR.       &
             field1 == 'AM' .OR. field1 == 'AD' .OR. field1 == 'AI' .OR.       &
             field1 == 'A=' .OR. field1 == 'A+' .OR. field1 == 'A-' .OR.       &
             field1 == 'A*' .OR. field1 == 'A/' .OR. field1 == 'AF' .OR.       &
             field1 == 'A(' ) THEN
          IF ( IARRAY( 2, 1 ) == 0 ) THEN
            CALL HASH_enlarge_and_insert( length, 12, ARRAY( 1 ) // 'RI',      &
                                          TABLE, KEY, INLIST,      &
                              ifree )
            IF ( ifree <= 0 ) THEN
              IF ( ifree == 0 ) THEN
                status = - 1
                RETURN
              END IF
            ELSE
              nrival = nrival + 1
              IF ( nrival > len_rival ) THEN
                used_length = nrival - 1 ; min_length = nrival
                new_length = increase_n * min_length / increase_d + 1
                CALL EXTEND_array( RIVAL, len_rival, used_length, new_length,  &
                                   min_length, buffer, status, alloc_status,   &
                                   'RIVAL' )
                IF ( status /= 0 ) THEN
                  bad_alloc = 'RIVAL' ; status = - 22 ; GO TO 980 ; END IF
                len_rival = new_length
              END IF

              IF ( nrival > len_rinames ) THEN
                used_length = nrival - 1 ; min_length = nrival
                new_length = increase_n * min_length / increase_d + 1
                CALL EXTEND_array( RINAMES, len_rinames, used_length,          &
                                   new_length, min_length, buffer,             &
                                   status, alloc_status, 'RINAMES' )
                IF ( status /= 0 ) THEN
                  bad_alloc = 'RINAMES' ; status = - 22 ; GO TO 980 ; END IF
                len_rinames = new_length
              END IF
              INLIST( ifree ) = nrival
              RINAMES( nrival ) = ARRAY( 1 )
            END IF
          END IF
        END IF
      END IF

!  an array name occurs in field 3. interpret the contents of this field

      IF ( ( kindar >= 101 .AND. kindar <= 104 ) .OR.                          &
           ( kindar >= 110 .AND. kindar <= 112 ) .OR.                          &
             kindar == 113 .OR.  kindar == 116 ) THEN
        CALL INTERPRET_field( 12, field3, ARRAY( 2 ), IARRAY( 1, 2 ),          &
                              length, TABLE, KEY, INLIST, out, status )
        IF ( status /= 0 ) RETURN
        IF ( debug .AND. out > 0 ) WRITE( out, 4090 ) level, ninstr,           &
          ARRAY( 2 ), ( IINAMES( IARRAY( 2 + i, 2 ) ), i = 1, IARRAY( 2, 2 ) )
      END IF

!  an array name occurs in field 5. interpret the contents of this field

      IF ( kindar == 102 .OR.  kindar == 104 .OR.                              &
           kindar == 105 .OR.  kindar == 112 .OR.                              &
         ( kindar >= 113 .AND. kindar <= 116 ) ) THEN
        CALL INTERPRET_field( 12, field5, ARRAY( 3 ), IARRAY( 1, 3 ),          &
                              length, TABLE, KEY, INLIST, out, status )
        IF ( status /= 0 ) RETURN
        IF ( debug .AND. out > 0 ) WRITE( out, 4100 ) level, ninstr,           &
          ARRAY( 3 ), ( IINAMES( IARRAY( 2 + i, 3 ) ), i = 1, IARRAY( 2, 3 ) )
      END IF

!  an name occurs in field 2

      IF ( ( kindar >= 110 .AND. kindar <= 112 ) .OR. kindar == 116 ) THEN
        CARRAY( 1 ) = field2
        IF ( debug .AND. out > 0 ) WRITE( out, 4110 ) level, ninstr, CARRAY( 1 )
      END IF

!  an name occurs in field 3

      IF ( kindar == 105 .OR. kindar == 106 .OR. kindar == 108 .OR.            &
           kindar == 109 .OR. kindar == 115 ) THEN
        CARRAY( 1 ) = field3
        IF ( debug .AND. out > 0 ) WRITE( out, 4120 ) level, ninstr, CARRAY( 1 )
      END IF

!  an name occurs in field 5

      IF ( kindar == 109 ) THEN
        CARRAY( 2 ) = field5
        IF ( debug .AND. out > 0 ) WRITE( out, 4130 ) level, ninstr, CARRAY( 2 )
      END IF

!  a numerical value occurs in field 4

      IF ( kindar == 101 .OR. kindar == 102 .OR. kindar == 107 .OR.            &
           kindar == 108 .OR. kindar == 109 .OR. kindar == 111 .OR.            &
           kindar == 112 ) THEN
        CALL GET_value( field4, VARRAY( 1 ) )
        IF ( debug .AND. out > 0 ) WRITE( out, 4140 ) level, ninstr, VARRAY( 1 )
      END IF

!  a numerical value occurs in field 6

      IF ( kindar == 102 .OR. kindar == 109 .OR. kindar == 112 ) THEN
        CALL GET_value( field6, VARRAY( 2 ) )
        IF ( debug .AND. out > 0 ) WRITE( out, 4150 ) level, ninstr, VARRAY( 2 )
      END IF
      RETURN

!  allocation errors

  980 CONTINUE
      WRITE( out, "( ' ** Message from INTERPRET_gpsmps-',                     &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  non-executable statements

 2140 FORMAT( ' ** Exit from INTERPRET_gpsmps - type of array definition',     &
              ' unrecognised')
 4080 FORMAT( ' Level ', i2, ' instruction ', i4, ' field 2 array ',           &
                A10, ' indices ', 3( A10, 1X ) )
 4090 FORMAT( ' Level ', i2, ' instruction ', i4, ' field 3 array ',           &
                A10, ' indices ', 3( A10, 1X ) )
 4100 FORMAT( ' Level ', i2, ' instruction ', i4, ' field 5 array ',           &
                A10, ' indices ', 3( A10, 1X ) )
 4110 FORMAT( ' Level ', i2, ' instruction ', i4, ' field 2 name ', A10)
 4120 FORMAT( ' Level ', i2, ' instruction ', i4, ' field 3 name ', A10)
 4130 FORMAT( ' Level ', i2, ' instruction ', i4, ' field 5 name ', A10)
 4140 FORMAT( ' Level ', i2, ' instruction ', i4, ' field 4 value ',           &
              1P, D12.4 )
 4150 FORMAT( ' Level ', i2, ' instruction ', i4, ' field 6 value ',           &
              1P, D12.4 )

!  end of subroutine DECODE_array_instruction

      END SUBROUTINE DECODE_array_instruction

!-   E X E C U T E _ a r r a y _ i n s t r u c t i o n    S U B R O U T I N E  -

      SUBROUTINE EXECUTE_array_instruction( niival, nrival,                    &
                         IIVAL, len_rival, RIVAL, len_rinames, RINAMES,        &
                         field1, field2, field3, field5, rvalue,               &
                         length, TABLE, KEY, INLIST, out, status )
      INTEGER :: length, niival, nrival, status, out
      INTEGER :: len_rival, len_rinames
      REAL ( KIND = wp ) :: rvalue
      CHARACTER ( LEN =  2 ) :: field1
      CHARACTER ( LEN = 10 ) :: field2, field3, field5
      INTEGER, DIMENSION( niival ) :: IIVAL
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TABLE, INLIST
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: RIVAL
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: RINAMES
      CHARACTER ( LEN = 12 ), ALLOCATABLE, DIMENSION( : ) :: KEY

!  --------------------------------------------------------------------------
!  construct and execute a list of do-loop real array arithmetic instructions
!  --------------------------------------------------------------------------

!  local variables

      INTEGER :: i, ifield, ifree, instr2, instr3, instr4
      INTEGER :: used_length, new_length, min_length, alloc_status
      CHARACTER ( LEN = 12 ) :: field
      CHARACTER ( LEN = 24 ) :: bad_alloc

      IF ( field1 == 'A+' .OR. field1 == 'A-' .OR. field1 == 'A*' .OR.         &
           field1 == 'A/' .OR. field1 == 'A(' ) THEN

!  obtain the real value, rvalue, as the value associated with the real
!  index in field 5, first ensuring that the index exists

        field = FIELD5( 1 : 10 ) // 'RI'
        CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
        IF ( ifield <= 0 ) THEN
          status = 3
          IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
          RETURN
        END IF
        instr4 = INLIST( ifield )
      END IF

!  if a definition is to be made from a previously defined index,
!  ensure that the index exists

      IF ( field1 == 'AI' ) THEN
        field = FIELD3( 1 : 10 ) // 'II'
        CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
        IF ( ifield <= 0 ) THEN
          status = 3
          IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
          RETURN
        END IF
        instr3 = INLIST( ifield )
      ELSE
        IF ( field1 /= 'AF' .AND. field1 /= 'A(' .AND. field1 /= 'AE' ) THEN
          field = FIELD3( 1 : 10 ) // 'RI'
          CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
          IF ( ifield <= 0 ) THEN
             status = 3
             IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
             RETURN
          END IF
          instr3 = INLIST( ifield )

!  the value is to be obtained using a special function. determine which one

        ELSE
          IF ( field1 /= 'AE' ) THEN
            DO i = 1, nfunct
              IF ( FIELD3( 1 : 10 ) == FUNCTN( i ) ) GO TO 20
            END DO
            status = 39
            IF ( out > 0 ) WRITE( out, 2390 ) FIELD3( 1 : 10 )
            RETURN
   20       CONTINUE
            instr3 = i
          END IF
        END IF
      END IF

!  record the address of the index which is to be set

      field = FIELD2( 1 : 10 ) // 'RI'
      CALL HASH_enlarge_and_insert( length, 12, field,                         &
                                    TABLE, KEY, INLIST, ifree )
      IF ( ifree <= 0 ) THEN
        IF ( ifree == 0 ) THEN
          status = - 1
          RETURN
        END IF
        ifree = - ifree
      ELSE
        nrival = nrival + 1
        IF ( nrival > len_rival ) THEN
          used_length = nrival - 1 ; min_length = nrival
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( RIVAL, len_rival, used_length, new_length,        &
                             min_length, buffer, status, alloc_status,         &
                             'RIVAL' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'RIVAL' ; status = - 22 ; GO TO 980 ; END IF
          len_rival = new_length
        END IF

        IF ( nrival > len_rinames ) THEN
          used_length = nrival - 1 ; min_length = nrival
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( RINAMES, len_rinames, used_length, new_length,    &
                             min_length, buffer, status, alloc_status,         &
                             'RINAMES' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'RINAMES' ; status = - 22 ; GO TO 980 ; END IF
          len_rinames = new_length
        END IF
        INLIST( ifree ) = nrival
        RINAMES( nrival ) = FIELD( 1 : 10 )
      END IF
      instr2 = INLIST( ifree )
      IF ( field1 == 'AE' ) RIVAL( instr2 ) = rvalue
      IF ( field1 == 'AA' ) RIVAL( instr2 ) =                                  &
                              rvalue + RIVAL( instr3 )
      IF ( field1 == 'AS' ) RIVAL( instr2 ) =                                  &
                              rvalue - RIVAL( instr3 )
      IF ( field1 == 'AM' ) RIVAL( instr2 ) =                                  &
                              rvalue * RIVAL( instr3 )
      IF ( field1 == 'AD' ) RIVAL( instr2 ) =                                  &
                              rvalue / RIVAL( instr3 )
      IF ( field1 == 'AI' ) RIVAL( instr2 ) =                                  &
                              FLOAT( IIVAL( instr3 ) )
      IF ( field1 == 'AF' ) CALL EVALUATE_function( RIVAL( instr2 ),           &
                                         rvalue, instr3, status )
      IF ( field1 == 'A=' ) RIVAL( instr2 ) = RIVAL( instr3 )
      IF ( field1 == 'A+' ) RIVAL( instr2 ) =                                  &
                              RIVAL( instr3 ) + RIVAL( instr4 )
      IF ( field1 == 'A-' ) RIVAL( instr2 ) =                                  &
                              RIVAL( instr3 ) - RIVAL( instr4 )
      IF ( field1 == 'A*' ) RIVAL( instr2 ) =                                  &
                              RIVAL( instr3 ) * RIVAL( instr4 )
      IF ( field1 == 'A/' ) RIVAL( instr2 ) =                                  &
                              RIVAL( instr3 ) / RIVAL( instr4 )
      IF ( field1 == 'A(' ) CALL EVALUATE_function( RIVAL( instr2 ),           &
                                         RIVAL( instr4 ), instr3, status )
      RETURN

!  allocation errors

  980 CONTINUE
      WRITE( out, "( ' ** Message from INTERPRET_gpsmps-',                     &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  non-executable statements

 2030 FORMAT( ' ** Exit from INTERPRET_gpsmps - index parameter name ', A10,   &
              ' not recognised ' )
 2390 FORMAT( ' ** Exit from INTERPRET_gpsmps - specified function name ',     &
              A10, ' not recognised ' )

!  end of subroutine EXECUTE_array_instruction

      END SUBROUTINE EXECUTE_array_instruction

!- S I F D E C O D E   E V A L U A T E _ I N T E G E R     S U B R O U T I N E -

      SUBROUTINE EVALUATE_integer( niival, nrival, IIVAL, RIVAL, INSTR )
      INTEGER :: niival, nrival
      INTEGER :: IIVAL( niival ), INSTR( 4 )
      REAL ( KIND = wp ) :: RIVAL( nrival )

!  ----------------------------------
!  execute integer index instructions
!  ----------------------------------

      IF ( INSTR( 1 ) == 21 ) IIVAL( INSTR( 2 ) ) = INSTR( 4 )
      IF ( INSTR( 1 ) == 22 ) IIVAL( INSTR( 2 ) ) =                            &
         INSTR( 4 ) + IIVAL( INSTR( 3 ) )
      IF ( INSTR( 1 ) == 23 ) IIVAL( INSTR( 2 ) ) =                            &
         INSTR( 4 ) - IIVAL( INSTR( 3 ) )
      IF ( INSTR( 1 ) == 24 ) IIVAL( INSTR( 2 ) ) =                            &
         INSTR( 4 ) * IIVAL( INSTR( 3 ) )
      IF ( INSTR( 1 ) == 25 ) IIVAL( INSTR( 2 ) ) =                            &
         INSTR( 4 ) / IIVAL( INSTR( 3 ) )
      IF ( INSTR( 1 ) == 26 ) IIVAL( INSTR( 2 ) ) =                            &
         INT( RIVAL( INSTR( 3 ) ) )
      IF ( INSTR( 1 ) == 31 ) IIVAL( INSTR( 2 ) ) =                            &
         IIVAL( INSTR( 3 ) )
      IF ( INSTR( 1 ) == 32 ) IIVAL( INSTR( 2 ) ) =                            &
         IIVAL( INSTR( 3 ) ) + IIVAL( INSTR( 4 ) )
      IF ( INSTR( 1 ) == 33 ) IIVAL( INSTR( 2 ) ) =                            &
         IIVAL( INSTR( 3 ) ) - IIVAL( INSTR( 4 ) )
      IF ( INSTR( 1 ) == 34 ) IIVAL( INSTR( 2 ) ) =                            &
         IIVAL( INSTR( 3 ) ) * IIVAL( INSTR( 4 ) )
      IF ( INSTR( 1 ) == 35 ) IIVAL( INSTR( 2 ) ) =                            &
         IIVAL( INSTR( 3 ) ) / IIVAL( INSTR( 4 ) )
      RETURN

!  end of subroutine EVALUATE_integer

      END SUBROUTINE EVALUATE_integer

!-*-  S I F D E C O D E   E V A L U A T E _ R E A L     S U B R O U T I N E  -*-

      SUBROUTINE EVALUATE_real( niival, nrival, IIVAL, RIVAL, rvalue,          &
                                INSTR, status )
      INTEGER :: niival, nrival, status
      REAL ( KIND = wp ) ::  rvalue
      INTEGER :: INSTR( 4 ), IIVAL( niival )
      REAL ( KIND = wp ) ::  RIVAL( nrival )

!  -------------------------------
!  execute real index instructions
!  -------------------------------

!  local variables

      INTEGER :: i

      status = 0
      i = INSTR( 1 ) - 50
      GO TO ( 110, 120, 130, 140, 150, 160, 170, 300, 300, 300,                &
              210, 220, 230, 240, 250, 300, 270, 300, 300 ), i
  110 CONTINUE
      RIVAL( INSTR( 2 ) ) = rvalue
      RETURN
  120 CONTINUE
      RIVAL( INSTR( 2 ) ) = rvalue + RIVAL( INSTR( 3 ) )
      RETURN
  130 CONTINUE
      RIVAL( INSTR( 2 ) ) = rvalue - RIVAL( INSTR( 3 ) )
      RETURN
  140 CONTINUE
      RIVAL( INSTR( 2 ) ) = rvalue * RIVAL( INSTR( 3 ) )
      RETURN
  150 CONTINUE
      RIVAL( INSTR( 2 ) ) = rvalue / RIVAL( INSTR( 3 ) )
      RETURN
  160 CONTINUE
      RIVAL( INSTR( 2 ) ) = FLOAT( IIVAL( INSTR( 3 ) ) )
      RETURN
  170 CONTINUE
      CALL EVALUATE_function( RIVAL( INSTR( 2 ) ), rvalue, INSTR( 3 ), status )
      RETURN
  210 CONTINUE
      RIVAL( INSTR( 2 ) ) = RIVAL( INSTR( 3 ) )
      RETURN
  220 CONTINUE
      RIVAL( INSTR( 2 ) ) = RIVAL( INSTR( 3 ) ) + RIVAL( INSTR( 4 ) )
      RETURN
  230 CONTINUE
      RIVAL( INSTR( 2 ) ) = RIVAL( INSTR( 3 ) ) - RIVAL( INSTR( 4 ) )
      RETURN
  240 CONTINUE
      RIVAL( INSTR( 2 ) ) = RIVAL( INSTR( 3 ) ) * RIVAL( INSTR( 4 ) )
      RETURN
  250 CONTINUE
      RIVAL( INSTR( 2 ) ) = RIVAL( INSTR( 3 ) ) / RIVAL( INSTR( 4 ) )
      RETURN
  270 CONTINUE
      CALL EVALUATE_function( RIVAL( INSTR( 2 ) ),                             &
                              RIVAL( INSTR( 4 ) ), INSTR( 3 ), status )
      RETURN
  300 CONTINUE
      RETURN

!  end of subroutine EVALUATE_real

      END SUBROUTINE EVALUATE_real

!- S I F D E C O D E   E V A L U A T E _ F U C T I O N     S U B R O U T I N E -

      SUBROUTINE EVALUATE_function( evalue, value, ivalue, status )
      INTEGER :: ivalue, status
      REAL ( KIND = wp ) :: value, evalue

!  -------------------------------------------------------
!  returns the value of the appropriate intrinsic function
!  -------------------------------------------------------

!  local variables

      REAL ( KIND = wp ) :: dvalue

      dvalue = DBLE( value )
      GO TO ( 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140 ),   &
        ivalue
   10 CONTINUE
      evalue = ABS( dvalue )
      RETURN
   20 CONTINUE
      IF ( value < 0.0 ) THEN
        status = 40
        WRITE( 6, 2400 ) value, 'SQRT  '
      ELSE
        evalue = SQRT( dvalue )
      END IF
      RETURN
   30 CONTINUE
      evalue = EXP( dvalue )
      RETURN
   40 CONTINUE
      IF ( value <= 0.0 ) THEN
        status = 40
        WRITE( 6, 2400 ) value, 'LOG   '
      ELSE
        evalue = LOG( dvalue )
      END IF
      RETURN
   50 CONTINUE
      IF ( value <= 0.0 ) THEN
        status = 40
        WRITE( 6, 2400 ) value, 'LOG10 '
      ELSE
        evalue = LOG10( dvalue )
      END IF
      RETURN
   60 CONTINUE
      evalue = SIN( dvalue )
      RETURN
   70 CONTINUE
      evalue = COS( dvalue )
      RETURN
   80 CONTINUE
      evalue = TAN( dvalue )
      RETURN
   90 CONTINUE
      IF ( ABS( value ) > 1.0 ) THEN
        status = 40
        WRITE( 6, 2400 ) value, 'ASIN  '
      ELSE
        evalue = ASIN( dvalue )
      END IF
      RETURN
  100 CONTINUE
      IF ( ABS( value ) > 1.0 ) THEN
        status = 40
        WRITE( 6, 2400 ) value, 'ACOS  '
      ELSE
         evalue = ACOS( dvalue )
      END IF
      RETURN
  110 CONTINUE
      evalue = ATAN( dvalue )
      RETURN
  120 CONTINUE
      evalue = SINH( dvalue )
      RETURN
  130 CONTINUE
      evalue = COSH( dvalue )
      RETURN
  140 CONTINUE
      evalue = TANH( dvalue )
      RETURN
 2400 FORMAT( ' ** Exit from INTERPRET_gpsmps - argument value ', 1P, D9.1,    &
              ' is illegal for function ', A6 )

!  end of subroutine EVALUATE_function

      END SUBROUTINE EVALUATE_function

!-*-*-*- S I F D E C O D E   G E T _ I N T E G E R   S U B R O U T I N E -*-*-*-

      SUBROUTINE GET_integer( field, ivalue )
      INTEGER :: ivalue
      CHARACTER ( LEN = 12 ) :: field

!  ------------------------------------------------------------
!  read the integer number ivalue stored in the character field
!  ------------------------------------------------------------

!  local variables

      INTEGER :: i, j
      CHARACTER ( LEN = 12 ) :: field2

!  right-shift the field, eliminating blanks

      field2 = '            '
      j = 12
      DO i = 12, 1, - 1
        IF ( FIELD( i : i ) == ' ' ) CYCLE
        FIELD2( j : j ) = FIELD( i : i )
        j = j - 1
      END DO
      READ( UNIT = field2, FMT = "( I12 )" ) ivalue
      RETURN

!  end of subroutine GET_integer

      END SUBROUTINE GET_integer

!-*-*-*-*- S I F D E C O D E   G E T _ L I N E   S U B R O U T I N E -*-*-*-*-

      SUBROUTINE GET_line( niival, nrival, IIVAL, IARRAY, VARRAY, ARRAY,       &
                           CARRAY, farray, RIVAL, IINAMES, novals, kindar,     &
                           field1, field2, field3, value4, field5, value6,     &
                           length, TABLE, KEY, INLIST, out, status )
      INTEGER :: niival, nrival, kindar, novals, out, status
      INTEGER :: length
      REAL ( KIND = wp ) :: value4, value6
      CHARACTER ( LEN =  2 ) :: field1, farray
      CHARACTER ( LEN = 10 ) :: field2, field3, field5
      INTEGER, DIMENSION( niival ) :: IIVAL
      INTEGER, DIMENSION( 5, 3 ) :: IARRAY
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TABLE, INLIST
      REAL ( KIND = wp ), DIMENSION( 2 ) :: VARRAY
      REAL ( KIND = wp ), DIMENSION( nrival ) :: RIVAL
      CHARACTER ( LEN = 7 ), DIMENSION( niival ) :: IINAMES
      CHARACTER ( LEN = 10 ), DIMENSION( 2 ) :: CARRAY
      CHARACTER ( LEN = 10 ), DIMENSION( 3 ) :: ARRAY
      CHARACTER ( LEN = 12 ), ALLOCATABLE, DIMENSION( : ) :: KEY

!  -----------------------------------------------------------------
!  translate the contents of an array card to its scalar values
!  the expected contents are at the control of the parameter kindar
!  as follows: (n=name,a=array name, v=numerical value,
!               r=real index array value)

!  kindar       field2    field3    field4     field5     field6
!  ------       ------    ------    ------     ------     ------
!   100            a
!   101            a         a         v
!   102            a         a         v          a          v
!   103            a         a
!   104            a         a                    a
!   105            a         n                    a
!   106            a         n
!   107            a                   v
!   108            a         n         v
!   109            a         n         v          n          v
!   110            n         a
!   111            n         a         v
!   112            n         a         v          a          v
!   113            a         a          < ------- r
!   114            a                    < ------- r
!   115            a         n          < ------- r
!   116            n         a          < ------- r
!  -----------------------------------------------------------------

!  local variables

      INTEGER :: i, ifield
      CHARACTER ( LEN = 12 ) :: field

      field1 = farray
      novals = 0

!  an array name occurs in field 2. interpret the contents of this
!  field

      IF ( ( kindar >= 100 .AND. kindar <= 109 ) .OR.                          &
           ( kindar >= 113 .AND. kindar <= 115 ) ) THEN
        CALL GET_field( niival, IIVAL, IARRAY( 1, 1 ),                         &
                        ARRAY( 1 ), field2, status )
        IF ( status /= 0 ) THEN
          IF ( out > 0 ) WRITE( out, 2350 )                                    &
            ARRAY( 1 )( 1 : IARRAY( 1, 1 ) ), ( IINAMES( IARRAY( 2 + i, 1 ) ), &
              IIVAL( IARRAY( 2 + i, 1 ) ), i = 1, IARRAY( 2, 1 ) )
          status = 35
          RETURN
        END IF
      ELSE
        field2 = '          '
      END IF

!  an array name occurs in field 3. interpret the contents of this
!  field

      IF ( ( kindar >= 101 .AND. kindar <= 104 ) .OR.                          &
           ( kindar >= 110 .AND. kindar <= 112 ) .OR.                          &
             kindar == 113 .OR.  kindar == 116 ) THEN
        CALL GET_field( niival, IIVAL, IARRAY( 1, 2 ),                         &
                        ARRAY( 2 ), field3, status )
        IF ( status /= 0 ) THEN
          IF ( out > 0 ) WRITE( out, 2350 )                                    &
            ARRAY( 2 )( 1 : IARRAY( 1, 2 ) ), ( IINAMES( IARRAY( 2 + i, 2 ) ), &
              IIVAL( IARRAY( 2 + i, 2 ) ), i = 1, IARRAY( 2, 2 ) )
          status = 35
          RETURN
        END IF
      ELSE
        field3 = '          '
      END IF
      IF ( kindar == 103 ) novals = 1

!  an array name occurs in field 5. interpret the contents of this
!  field

      IF ( kindar == 102 .OR.  kindar == 104 .OR.                              &
           kindar == 105 .OR.  kindar == 112 .OR.                              &
         ( kindar >= 113 .AND. kindar <= 116 ) ) THEN
        CALL GET_field( niival, IIVAL, IARRAY( 1, 3 ),                         &
                        ARRAY( 3 ), field5, status )
        IF ( status /= 0 ) THEN
          IF ( out > 0 ) WRITE( out, 2350 )                                    &
            ARRAY( 3 )( 1 : IARRAY( 1, 3 ) ), ( IINAMES( IARRAY( 2 + i, 3 ) ), &
              IIVAL( IARRAY( 2 + i, 3 ) ), i = 1, IARRAY( 2, 3 ) )
          status = 35
          RETURN
        END IF
      ELSE
        field5 = '          '
      END IF
      IF ( kindar == 104 ) novals = 2

!  an name occurs in field 2

      IF ( ( kindar >= 110 .AND. kindar <= 112 ) .OR. kindar == 116 )          &
        field2 = CARRAY( 1 )

!  an name occurs in field 3

      IF ( kindar == 105 .OR. kindar == 106 .OR. kindar == 108 .OR.            &
           kindar == 109 .OR. kindar == 115 ) field3 = CARRAY( 1 )

!  an name occurs in field 5

      IF ( kindar == 109 ) field5 = CARRAY( 2 )

!  a numerical value occurs in field 4

      IF ( kindar == 101 .OR. kindar == 102 .OR. kindar == 107 .OR.            &
           kindar == 108 .OR. kindar == 109 .OR. kindar == 111 .OR.            &
           kindar == 112 ) THEN
        value4 = VARRAY( 1 )
        novals = 1

!  a real index value is to be places in field 4

      ELSE
        IF ( kindar == 113 .OR. kindar == 114 .OR.                             &
             kindar == 115 .OR. kindar == 116 ) THEN
          field = FIELD5( 1 : 10 ) // 'RI'
          CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
          IF ( ifield <= 0 ) THEN
            status = 3
            IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
            RETURN
          END IF
          value4 = RIVAL( INLIST( ifield ) )
          novals = 1
        ELSE
          value4 = 0.0D+0
        END IF
      END IF

!  a numerical value occurs in field 6

      IF ( kindar == 102 .OR. kindar == 109 .OR. kindar == 112 ) THEN
        value6 = VARRAY( 2 )
        novals = 2
      ELSE
        value6 = 0.0D+0
      END IF
      status = 0
      RETURN

!  non-executable statements

 2030 FORMAT( ' ** Exit from INTERPRET_gpsmps - index parameter name ', A10,   &
              ' not recognised ' )
 2350 FORMAT( ' ** Exit from INTERPRET_gpsmps - expanded array name > ',       &
              '10 chars. Array name = ', A9, /, ( '    Index ', A10,           &
              ' has the value ', I0, : ) )

!  end of subroutine GET_line

      END SUBROUTINE GET_line

!-*-*-*-*- S I F D E C O D E   G E T _ F I E L D   S U B R O U T I N E -*-*-*-*-

      SUBROUTINE GET_field( niival, IIVAL, IARRAY, array, field, status )
      INTEGER :: niival, status
      INTEGER :: IIVAL( niival )
      INTEGER :: IARRAY( 5 )
      CHARACTER ( LEN = 10 ) :: array, field

!  -----------------------------------------------------------
!  construct an expanded array name from its constituent parts
!  -----------------------------------------------------------

!  local variables

      INTEGER ::I, indces, ivalue, j, ndigit
      CHARACTER ( LEN = 9 ) :: field9

      j = IARRAY( 1 )
      field = ARRAY( 1 : j )
      j = j + 1
      indces = IARRAY( 2 )
      DO i = 1, indces
        ivalue = IIVAL( IARRAY( 2 + i ) )
        IF ( ivalue == 0 ) THEN
          ndigit = 1
        ELSE
          ndigit = INT( LOG10( ABS( FLOAT( ivalue ) ) ) ) + 1
          IF ( ivalue < 0 ) ndigit = ndigit + 1
        END IF
        IF ( ( i < indces .AND. j + ndigit > 10 ) .OR.                         &
             ( i == indces .AND. j + ndigit > 11 ) ) THEN
          status = 35
          RETURN
        END IF
        WRITE( UNIT = field9, FMT = 2000 ) ivalue
        FIELD( J: j + ndigit - 1 ) = FIELD9( 10 - NDIGIT: 9 )
        j = j + ndigit
        IF ( i < indces ) THEN
          FIELD( J: j ) = ','
          j = j + 1
        END IF
      END DO
      status = 0
      RETURN

!  non-executable statements

 2000 FORMAT( I9 )

!  end of subroutine GET_field

      END SUBROUTINE GET_field

!-*-*-*-*- S I F D E C O D E   G E T _ V A L U E   S U B R O U T I N E -*-*-*-*-

      SUBROUTINE GET_value( field, value )
      REAL ( KIND = wp ) :: value
      CHARACTER ( LEN = 12 ) :: field

!  --------------------------------------------------------
!  read the real number value stored in the character field
!  --------------------------------------------------------

      READ( UNIT = field, FMT = "( BN, F12.0 )" ) value
      RETURN

!  end of subroutine GET_value

      END SUBROUTINE GET_value

!-*- S I F D E C O D E   I N T E R P R E T _ F I E L D   S U B R O U T I N E -*-

      SUBROUTINE INTERPRET_field( nchar, fielda, array, IARRAY,                &
                                  length, TABLE, KEY, INLIST, out, status )
      INTEGER :: length, nchar, out, status
      CHARACTER ( LEN = 10 ) :: fielda, array
      INTEGER, DIMENSION( 5 ) :: IARRAY
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TABLE, INLIST
      CHARACTER ( LEN = 12 ), ALLOCATABLE, DIMENSION( : ) :: KEY

!  --------------------------------
!  interpret the contents of fielda
!  --------------------------------

!  local variables

      INTEGER :: i, ifield, indces, j
      CHARACTER ( LEN = 12 ) :: field

!  first find the array name by searching the field for the string '('

      DO i = 1, 10
        IF ( FIELDA( i : i ) == '(' ) GO TO 20
      END DO
      IARRAY( 1 ) = 10
      IARRAY( 2 ) = 0
      array = fielda
      status = 0
      RETURN

!  the string '(' occurs in position i

   20 CONTINUE
      j = i - 1
      IARRAY( 1 ) = j
      array = FIELDA( 1 : j )
      indces = 0

!  now find the array indices. search for one of the strings ')' or ','

   30 CONTINUE
         j = i + 1
         DO i = j, 10
           IF ( FIELDA( i : i ) == ',' .OR. FIELDA( i : i ) == ')' ) GO TO 50
         END DO
         IF ( out > 0 ) WRITE( out, 2370 ) fielda
         status = 37
         RETURN

!  the string ',' or ')' occurs in position j

   50    CONTINUE
         IF ( i /= j ) THEN
           indces = indces + 1
           IF ( indces > 3 ) THEN
             IF ( out > 0 ) WRITE( out, 2360 )
             status = 36
             RETURN
           END IF
           FIELD( 1 : 12 ) = '          II'
           FIELD( 1 : i - j ) = FIELDA( J: i - 1 )

!  check that the array index exists and determine its address

           CALL HASH_search( length, nchar, field, TABLE, KEY, ifield )
           IF ( ifield <= 0 ) THEN
              status = 3
              IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
              RETURN
           END IF
           IARRAY( 2 + indces ) = INLIST( ifield )
         END IF
         IF ( FIELDA( i : i ) == ',' ) GO TO 30

!  the array definition is complete

      IARRAY( 2 ) = indces
      status = 0
      RETURN

!  non-executable statements

 2030 FORMAT( ' ** Exit from INTERPRET_gpsmps - index parameter name ', A10,   &
              ' not recognised ' )
 2370 FORMAT( ' ** Exit from INTERPRET_gpsmps - incorrect array name', A10,    &
              ' in do-loop ')
 2360 FORMAT( ' ** Exit from INTERPRET_gpsmps - > 3 array name indices ' )

!  end of subroutine INTERPRET_field

      END SUBROUTINE INTERPRET_field

!-*-*-*-*-*-*- S I F D E C O D E   O N L Y 1    F U N C T I O N -*-*-*-*-*-*-*-

      FUNCTION ONLY1( num )
      INTEGER :: only1
      INTEGER :: num

!  -------------------------------------------------
!  returns the value 1 if num is one and 2 otherwise
!  -------------------------------------------------

      ONLY1 = 2
      IF ( num == 1 ) ONLY1 = 1
      RETURN

!  end of FUNCTION ONLY1

      END FUNCTION ONLY1

!-*-*- S I F D E C O D E   M A K E _ o u t s d i f    S U B R O U T I N E -*-*-

      SUBROUTINE MAKE_outsdif( n, nlvars, ng, nelnum, neling, nobj,            &
                         nelvar, nlisgp, nlisep,                               &
                         nbnd, nnza, nconst, nstart, nrange, nobjgr, nobbnd,   &
                         neltype, ngtype, pname, nameob, namerh, namera,       &
                         namebn, namest, nameof, ELING_ptr, ELVAR, EV_ptr,     &
                         ABYROW_col, ABYROW_ptr, A_row, A_col,                 &
                         length, TABLE, KEY, INLIST,                           &
                         GSTATE, IDROWS, ELV, INV, GTYPESP_ptr, ELING_el,      &
                         EP_ptr, GP_ptr, TYPEE, GTYPE, TYPEV, IWK, A_val,      &
                         len1_blu, B_l, B_u, len1_vstart, VSTART,              &
                         len1_cstart, CSTART, RSCALE, CSCALE,                  &
                         RDROWS, DEFAULT, WEIGHT, B_l_default, B_u_default,    &
                         GP_val, EP_val, FBOUND_l, FBOUND_u,                   &
                         ABYROW_val, B, BL, BU, X,                             &
                         ESCALE, GSCALE, VSCALE,                               &
                         GNAMES, VNAMES, BNAMES, SNAMES, ONAMES,               &
                         ETYPES, GTYPES, OBBNAME, ialgor, iauto,               &
                         out, outda, single, status, debug  )

      INTEGER :: n, ng, length, nelvar, neling
      INTEGER :: nlvars
      INTEGER :: nlisgp, nlisep
      INTEGER :: nbnd, nnza, nconst, nstart, nrange, nobjgr
      INTEGER :: ialgor, out, outda, status, nobbnd
      INTEGER :: neltype, ngtype, nobj, nelnum
      INTEGER :: len1_blu, len1_vstart, len1_cstart
      INTEGER :: iauto
      LOGICAL :: single, debug
      CHARACTER ( LEN = 10 ) :: pname
      CHARACTER ( LEN = 10 ) :: nameob, namerh, namera, namebn, namest, nameof
      INTEGER :: ELING_ptr( ng + 1 ), ELVAR( nelvar )
      INTEGER :: EV_ptr( nelnum + 1 )
      INTEGER :: GSTATE( ng  ), IDROWS( 2, ng )
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TABLE, INLIST
      INTEGER :: A_row( nnza ), A_col( nnza )
      INTEGER :: ELV( neltype + 1 ), INV( neltype + 1 )
      INTEGER :: EP_ptr ( nelnum + 1 ), GP_ptr( ng + 1 )
      INTEGER :: ELING_el( neling ), GTYPESP_ptr( ngtype + 1 )
      INTEGER :: TYPEV( n + ng ), TYPEE( nelnum ), GTYPE( ng ), IWK( ng + 1 )
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: ABYROW_ptr, ABYROW_col
      REAL ( KIND = wp ) :: A_val( nnza )
      REAL ( KIND = wp ) :: RDROWS( 2, ng ), WEIGHT( neling )
      REAL ( KIND = wp ) :: B_l( len1_blu, nbnd ), B_u( len1_blu, nbnd )
      REAL ( KIND = wp ) :: B_l_default( nbnd ), B_u_default( nbnd )
      REAL ( KIND = wp ) :: VSTART( len1_vstart, nstart )
      REAL ( KIND = wp ) :: CSTART( len1_cstart, nstart )
      REAL ( KIND = wp ) :: RSCALE( ng ), CSCALE( n )
      REAL ( KIND = wp ) :: GP_val( nlisgp ), EP_val( nlisep )
      REAL ( KIND = wp ) :: DEFAULT( nlvars + nconst + 1 )
      REAL ( KIND = wp ) :: FBOUND_l( nobbnd ), FBOUND_u( nobbnd )
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: X
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: ESCALE, GSCALE, VSCALE
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: B, BL, BU, ABYROW_val
      CHARACTER ( LEN = 10 ) :: GNAMES( ng ), BNAMES( nbnd )
      CHARACTER ( LEN = 10 ) :: ONAMES( nobj ), OBBNAME( nobbnd )
      CHARACTER ( LEN = 10 ) :: VNAMES( n + ng ), SNAMES( nstart )
      CHARACTER ( LEN = 10 ) :: ETYPES( neltype ), GTYPES( ngtype )
!     CHARACTER ( LEN = 12 ) :: KEY( length  )
      CHARACTER ( LEN = 12 ), ALLOCATABLE, DIMENSION( : ) :: KEY

!  ---------------------------------------------------------------------
!  convert the output from INTERPRET_gpsmps into a form suitable for any
!  of the GALAHAD programs SBMIN (ialgor = 1), AUGLG (ialgor = 2) or
!  others (ialgor = 3 )

!  formerly INLANC in SiFDec
!  ---------------------------------------------------------------------

!  local variables

      INTEGER :: nnz, nslack, jobbnd, jstart, nel1, jcol, jconst, jrange
      INTEGER :: i, ic, ifield, ig, irow, is, itype, j, jbnd, k, len_abyrow
      INTEGER :: k1, k2, nelv, ninv, ng1, nel, ngpv, ngr, alloc_status
      REAL ( KIND = wp ) :: avalue, rrow
      REAL ( KIND = wp ) :: OBFBND( 2 )
      CHARACTER ( LEN = 12 ) :: field
      CHARACTER ( LEN = 24 ) :: bad_alloc

!  automatic arrays

      INTEGER, DIMENSION( nelnum ) :: INTVAR
      REAL ( KIND = wp ), DIMENSION( ng ) :: CLMULT
      REAL ( KIND = wp ), DIMENSION( ( MAX( n, nlisgp ) ) ) :: WK
      LOGICAL, DIMENSION( ng ) :: GXEQX
      LOGICAL, DIMENSION( nelnum ) :: INTREP

!  decide which optimization method to use
!  ---------------------------------------

      IF ( ialgor <= 0 ) THEN
        ialgor = 1
        DO ig = 1, ng
          IF ( ABS( GSTATE( ig ) ) >= 2 ) ialgor = 2
        END DO
      END IF
      GSTATE( : ng ) = ABS( GSTATE( : ng ) )

!  select the bounds on the variables
!  -----------------------------------

      IF ( nbnd == 1 ) THEN
        jbnd = 1

!  find the key word in the list of bounds

      ELSE
        DO jbnd = 1, nbnd
          IF ( namebn == BNAMES( jbnd ) ) GO TO 120
        END DO
        status = 47
        IF ( out > 0 ) WRITE( out, 2470 ) namebn
        GO TO 800
  120   CONTINUE
      END IF

!  compute the number of slack variables

      nslack = 0
      IF ( ialgor <= 2 ) THEN
        DO ig = 1, ng
          is = GSTATE( ig )
          IF ( is > 4 ) is = is - 4
          IF ( is /= 1 .AND. is /= 2 ) nslack = nslack  + 1
        END DO
      END IF

      nnz = n + nslack
      CALL ALLOCATE_array( X, nnz, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'X' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( VSCALE, nnz, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'VSCALE' ; GO TO 980 ; END IF

      nnz = n + ng
      CALL ALLOCATE_array( BL, nnz, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'BL' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( BU, nnz, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'BU' ; GO TO 980 ; END IF

!  the required vector of bounds is column jbnd of bnd. copy this
!  vector into bl( ) and bu( ). Record the scale factors

      DO j = 1, nlvars
        BL( j ) = B_l( j, jbnd )
        BU( j ) = B_u( j, jbnd )
        VSCALE( j ) = CSCALE( j )
      END DO

!  the bounds on the nonlinear variables are set to default values

      DO j = nlvars + 1, n
        BL( j ) = B_l_default( jbnd )
        BU( j ) = B_u_default( jbnd )
      END DO

!  select the constant/rhs and ranges
!  -----------------------------------

!  find the named constant (R.H.S.) vector in the list

      IF ( nconst == 1 ) THEN
        jconst = nlvars + 1
      ELSE
        field = namerh // 'CO'
        CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
        IF ( ifield > 0 ) THEN
           jconst = INLIST( ifield )
        ELSE
          status = 46
          IF ( out > 0 ) WRITE( out, 2460 ) namerh
          GO TO 800
        END IF
      END IF

!  find the named range vector in the list

      IF ( nrange > 0 ) THEN
        IF ( nrange == 1 ) THEN
          jrange = nlvars + nconst + 1
        ELSE
          field = namera // 'RA'
          CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
          IF ( ifield > 0 ) THEN
            jrange = INLIST( ifield )
          ELSE
            status = 48
            IF ( out > 0 ) WRITE( out, 2480 ) namera
            GO TO 800
          END IF
        END IF
      ELSE
        jrange = 0
      END IF

      CALL ALLOCATE_array( ESCALE, neling, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'ESCALE' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( GSCALE, ng, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'GSCALE' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( B, ng, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'B' ; GO TO 980 ; END IF

!  initialize the vector of constants, b, as its default value

      DO i = 1, ng
        IF ( GNAMES ( i ) == cqgrou ) THEN
          B( i ) = zero
          BL( n + i ) = zero
          BU( n + i ) = biginf
          GSCALE( i ) = one
        ELSE
          B( i ) = DEFAULT( jconst )

!  initialize lower and upper bounds on slack variables as zero and the
!  default respectively

          BL( n + i ) = zero
          IF ( nrange == 0 ) THEN
            BU( n + i ) = biginf
          ELSE
            BU( n + i ) = DEFAULT( jrange )
          END IF

!  record the group scale factors

          GSCALE( i ) = one / RSCALE( i )
        END IF
      END DO

!  sweep through the entries of A. Look for entries in columns jconst and
!  jrange. Subsequently remove all constant/rhs and range columns to leave
!  only entries corresponding to linear elements

      nnz = 0
      DO k = 1, nnza
        i = A_row( k )
        j = A_col( k )
        avalue = A_val( k )

!  see if the entry belongs to the selected constant/rhs vector

        IF ( j == jconst ) B( i ) = avalue

!  see if the entry belongs to the selected range vector

        IF ( j == jrange ) BU( n + i ) = avalue

!  check if the entry belongs to a linear element

        IF ( j <= nlvars ) THEN
          nnz = nnz + 1

!  record the coordinates and value of the entry from the linear element

          A_row( nnz ) = i
          A_col( nnz ) = j
          A_val( nnz ) = avalue
        END IF
      END DO
      nnza = nnz

!  the matrix is stored in coordinate form. Resort it so that it is stored
!  by rows

      ng1 = ng + 1

!  allocate space for the row storage

      CALL ALLOCATE_array( ABYROW_ptr, ng1, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'ABYROW_ptr' ; GO TO 980 ; END IF

      len_abyrow = nnza + nslack
      CALL ALLOCATE_array( ABYROW_val, len_abyrow, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'ABYROW_val' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( ABYROW_col, len_abyrow, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'ABYROW_col' ; GO TO 980 ; END IF

      IF ( nnza > 0 ) THEN
        CALL REORDER( ng, nnza, A_col, A_row, A_val, ABYROW_ptr, IWK )
      ELSE
        ABYROW_ptr( : ng1 ) = 1
      END IF

!  decode the 'd'-groups/rows. set the workspace array WK to zero

      nnz = 0
      nslack = 0
      WK( : n ) = zero

!  GXEQX is true if the group function is trivial

      DO ig = 1, ng
        GXEQX( ig ) = GTYPE( ig ) == 0
      END DO

!  set the coefficients of the linear elements
!  --------------------------------------------

!  consider the groups in order

      DO ig = 1, ng
        k1 = ABYROW_ptr( ig )
        ABYROW_ptr( ig ) = nnz + 1

!  first pass: determine the nonzeros in the row

        IF ( GSTATE( ig ) <= 4 ) THEN
          DO k = k1, ABYROW_ptr( ig + 1 ) - 1
            j = A_col( k )
            IF ( WK( j ) /= zero ) THEN
              WK( j ) = WK( j ) + A_val( k )
            ELSE
              WK( j ) = A_val( k )
            END IF
          END DO

!  second pass: only record nonzeros

          DO k = k1, ABYROW_ptr( ig + 1 ) - 1
            j = A_col( k )
            IF ( WK( j ) /= zero ) THEN
              nnz = nnz + 1
              ABYROW_col( nnz ) = j
              ABYROW_val( nnz ) = WK( j )
              WK( j ) = zero
            END IF
          END DO

!  the ig-th group is a 'd'-group. construct the new group from its
!  two donors. consider each donor row in turn. form the new row in WK

        ELSE
          DO i = 1, 2
            irow = IDROWS( i, ig )
            rrow = RDROWS( i, ig )
            DO k = ABYROW_ptr( irow ), ABYROW_ptr( irow + 1 ) - 1
              ic = ABYROW_col( k )
              IF ( ic <= nlvars ) WK( ic ) = WK( ic ) + ABYROW_val( k ) * rrow
            END DO
          END DO

!  move the new row into ABYROW_val, resetting WK to zero as we proceed

          DO i = 1, 2
            irow = IDROWS( i, ig )
            DO k = ABYROW_ptr( irow ), ABYROW_ptr( irow + 1 ) - 1
              ic = ABYROW_col( k )
              IF ( ic <= nlvars ) THEN
                IF ( WK( ic ) /= zero ) THEN
                  nnz = nnz + 1
                  ABYROW_col( nnz ) = ic
                  ABYROW_val( nnz ) = WK( ic )
                  WK( ic ) = zero
                END IF
              END IF
            END DO
          END DO
        END IF

!  if the group is of type 'l' or 'g', insert a slack variable in the
!  linear element

        is = GSTATE( ig )
        IF ( is > 4 ) is = is - 4
        IF ( ialgor <= 2 ) THEN
          IF ( is /= 1 .AND. is /= 2 ) THEN
            IF ( .NOT. GXEQX( ig ) ) THEN
              WRITE( out, 1990 )
              status = 51
              GO TO 800
            END IF
            nnz = nnz + 1
            nslack = nslack  + 1
            jcol = n + nslack
            ABYROW_col( nnz ) = jcol
            IF ( is == 3 ) THEN
               ABYROW_val( nnz ) =   one
            ELSE
               ABYROW_val( nnz ) = - one
            END IF

!  give the slack variable the same name as its corresponding group

            VNAMES( jcol ) = GNAMES( ig )
            TYPEV( jcol ) = 0

!  assign the correct bounds for the slack variable

            BL( jcol ) = BL( n + ig )
            BU( jcol ) = BU( n + ig )
          END IF
        ELSE
          IF ( is == 3 ) BL( n + ig ) = - BU( n + ig )
          IF ( is == 1 .OR. is == 2 .OR. is == 3 ) BU( n + ig ) = zero
        END IF
        GSTATE( ig ) = is
      END DO

!  reset the number of variables to include the slacks

      n = n + nslack
      nnza = nnz + 1
      ng1 = ng + 1
      ABYROW_ptr( ng1 ) = nnza
      IF ( debug .AND. out > 0 ) WRITE( out, 3020 ) ( ( i, ABYROW_col( k ),    &
            ABYROW_val( k ), k = ABYROW_ptr( i ), ABYROW_ptr( i + 1 ) - 1 ),   &
              i = 1, ng )

!  select the starting point for the minimization
!  ----------------------------------------------

      IF ( nstart == 1 ) THEN
        jstart = 1

!  find the key word in the list of starting points

      ELSE
        DO jstart = 1, nstart
          IF ( namest == SNAMES( jstart ) ) GO TO 420
        END DO
        status = 49
        IF ( out > 0 ) WRITE( out, 2490 ) namest
        GO TO 800
  420   CONTINUE
      END IF

!  record the starting point

      X( 1 : nlvars ) = VSTART( 1 : nlvars, jstart )

!  initialize all slack and nonlinear variables as zero, with weight 1

      X( nlvars + 1 : n ) = zero
      VSCALE( nlvars + 1 : n ) = one

!  record the lagrange multipliers, if any

      IF ( ialgor >= 2 ) CLMULT( : ng ) = CSTART( : ng, jstart )

!  nonlinear element information
!  ------------------------------

!  the parameter values for the nonlinear elements may be unordered. Order
!  the parameters so that those for group i precede those from group i+1.
!  Place the reordered set in wk

      j = 1
      DO ig = 1, ng
        k = GTYPE( ig )
        GP_ptr( ig ) = j
        IF ( k > 0 ) THEN
          k1 = GTYPESP_ptr( k + 1 ) - GTYPESP_ptr( k )
          k2 = GP_ptr( ig ) - 1
          DO i = 1, k1
            WK( j ) = GP_val( k2 + i )
            j = j + 1
          END DO
        END IF
      END DO
      GP_ptr( ng1 ) = j

!  overwrite GP_val with wk

      GP_val( : j - 1 ) = WK( : j - 1 )

!  record the scale factors for the nonlinear elements

      DO i = 1, ELING_ptr( ng1 ) - 1
        ESCALE( i ) = WEIGHT( i )
      END DO

!  determine whether the nonlinear elements have internal
!  representations

      DO i = 1, nelnum
        itype = TYPEE( i )
        nelv = ELV( itype + 1 ) - ELV( itype )
        ninv = INV( itype + 1 ) - INV( itype )
        INTREP( i ) = ninv < nelv

!  store the number of internal variables for each element

        INTVAR( i ) = ninv
      END DO
      IF ( ialgor >= 2 ) THEN

!  select the objective function group
!  ------------------------------------

!  find the named objective function group in the list. Mark the remaining
!  objective function groups for removal

        IF ( oneobj ) THEN
          nobjgr = 0
          DO i = 1, nobj
            field = ONAMES( i ) // 'GR'
            CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
            k = INLIST( ifield )
            IF ( nameof == ONAMES( i ) ) THEN
              nobjgr = k
              IF ( out > 0 .AND. debug ) WRITE( out, 3010 ) ONAMES( i ), k
            ELSE
              ELING_ptr( k ) = - ELING_ptr( k )
              IF ( out > 0 .AND. debug ) WRITE( out, 3000 ) ONAMES( i )
            END IF
          END DO

!  remove redundant group information

          IF ( nobj > 1 .OR. ( nobj == 1 .AND. nobjgr == 0 ) ) THEN
            nnz = 1
            nel = 1
            ngpv = 1
            ngr = 0
            DO i = 1, ng
              IF ( ELING_ptr( i ) > 0 ) THEN
                ngr = ngr + 1
                IF ( i == nobjgr ) nobjgr = ngr

!  shift the group status, name, type, constant, triviality indicator,
!  Lagrange multiplier and weight

                IF ( ialgor >= 2 ) GSTATE( ngr ) = GSTATE( i )
                GNAMES( ngr ) = GNAMES( i )
                GTYPE( ngr ) = GTYPE( i )
                B( ngr ) = B( i )
                GXEQX( ngr ) = GXEQX( i )
                IF ( ialgor >= 2 ) CLMULT( ngr ) = CLMULT( i )
                GSCALE( ngr ) = GSCALE( i )

!  shift the list of elements and weights in the i-th group

                k1 = ELING_ptr( i )
                ELING_ptr( ngr ) = nel
                DO k = k1, ABS( ELING_ptr( i + 1 ) ) - 1
                  ELING_el( nel ) = ELING_el( k )
                  ESCALE( nel ) = ESCALE( k )
                  nel = nel + 1
                END DO

!  shift the list of parameters in the i-th group

                k1 = GP_ptr( i )
                GP_ptr( ngr ) = ngpv
                DO k = k1, GP_ptr( i + 1 ) - 1
                  GP_val( ngpv ) = GP_val( k )
                  ngpv = ngpv + 1
                END DO

!  shift the list of coefficients and positions of the nonzeros for the
!  linear element in the i-th group

                k1 = ABYROW_ptr( i )
                ABYROW_ptr( ngr ) = nnz
                DO k = k1, ABYROW_ptr( i + 1 ) - 1
                   ABYROW_val( nnz ) = ABYROW_val( k )
                   ABYROW_col( nnz ) = ABYROW_col( k )
                   nnz = nnz + 1
                END DO
              END IF
            END DO
            ng = ngr
            ELING_ptr( ng + 1 ) = nel
            GP_ptr ( ng + 1 ) = ngpv
            ABYROW_ptr( ng + 1 ) = nnz
          END IF
        END IF
      END IF

!  set the required lower and upper bounds on the objective function

      IF ( nobbnd == 0 ) THEN
        OBFBND( 1 ) = - biginf
        OBFBND( 2 ) =   biginf

!  find the key word in the list of starting points

      ELSE
        DO jobbnd = 1, nobbnd
          IF ( nameob == OBBNAME( jobbnd ) ) GO TO 670
        END DO
        status = 50
        IF ( out > 0 ) WRITE( out, 2500 ) nameob
        GO TO 800
  670   CONTINUE
        OBFBND( 1 ) = FBOUND_l( jobbnd )
        OBFBND( 2 ) = FBOUND_u( jobbnd )
      END IF

!  if no output is required, exit

      IF ( outda <= 0 ) GO TO 900
      nel1 = nelnum + 1
      WRITE( outda, 3180 ) n, ng, nelnum, ELING_ptr( ng1  ) - 1,               &
                           EV_ptr( nel1 ) - 1, ABYROW_ptr( ng1  ) - 1,         &
                           GP_ptr( ng1  ) - 1, EP_ptr( nel1 ) - 1,             &
                           neltype, ngtype

!  print out problem data. output the number of variables, groups and
!  elements and, perhaps, the identity of the objective function group

      WRITE( outda, 3100 ) ialgor, pname, iauto
      IF ( ialgor == 2 ) WRITE( outda, 3170 ) nslack, nobjgr

!  output the starting addresses of the elements in each group,
!  of the parameters used for each group and
!  of the nonzeros of the linear element in each group

      WRITE( outda, 3110 ) ( ELING_ptr( i ), i = 1, ng1 )
      WRITE( outda, 3110 ) ( GP_ptr ( i ), i = 1, ng1 )
      WRITE( outda, 3110 ) ( ABYROW_ptr( i ), i = 1, ng1 )

!  output the starting addresses of the variables and parameters
!  in each element

      WRITE( outda, 3110 ) ( EV_ptr( i ), i = 1, nel1 )
      WRITE( outda, 3110 ) ( EP_ptr( i ), i = 1, nel1 )

!  output the group type of each group and its status

      WRITE( outda, 3110 ) ( GTYPE( i ), i = 1, ng )
      IF ( ialgor >= 2 ) WRITE( outda, 3110 )( GSTATE( i ), i = 1, ng )

!  output the element type of each element

      WRITE( outda, 3110 ) ( TYPEE( i ), i = 1, nelnum )

!  output the element type of each element
!  and its number of internal variables

      WRITE( outda, 3110 ) ( INTVAR( i ), i = 1, nelnum )

!  output the identity of each individual element

      WRITE( outda, 3110 ) ( ELING_el( i ), i = 1, ELING_ptr( ng1 ) - 1 )

!  output the variables in each group's elements

      WRITE( outda, 3110 ) ( ELVAR( i ), i = 1, EV_ptr( nel1 ) - 1 )

!  output the column addresses of the nonzeros in each linear element

      nnza = ABYROW_ptr( ng1 ) - 1
      WRITE( outda, 3110 ) ( ABYROW_col( i ), i = 1, nnza )

!  write single precision format

      IF ( single ) THEN

!  output the values of the nonzeros in each linear element, the
!  constant term in each group, the lower and upper bounds on
!  the variables and the starting point for the minimization

        WRITE( outda, 3121 ) ( ABYROW_val( i ), i = 1, nnza )
        WRITE( outda, 3121 ) ( B( i ), i = 1, ng )
        IF ( ialgor <= 2 ) THEN
           WRITE( outda, 3121 ) ( BL( i ), i = 1, n )
           WRITE( outda, 3121 ) ( BU( i ), i = 1, n )
        ELSE
           WRITE( outda, 3121 ) ( BL( i ), i = 1, n + ng )
           WRITE( outda, 3121 ) ( BU( i ), i = 1, n + ng )
        END IF
        WRITE( outda, 3121 ) ( X( i ), i = 1, n )
        IF ( ialgor >= 2 ) WRITE( outda, 3121 )( CLMULT( i ), i = 1, ng )

!  output the parameters in each group

        WRITE( outda, 3121 ) ( GP_val( i ), i = 1, GP_ptr( ng1 ) - 1 )

!  output the parameters in each individual element

        WRITE( outda, 3121 ) ( EP_val( i ), i = 1, EP_ptr( nel1 ) - 1 )

!  output the scale factors for the nonlinear elements

        WRITE( outda, 3121 ) ( ESCALE( i ), i = 1, ELING_ptr( ng1 ) - 1 )

!  output the scale factors for the groups

        WRITE( outda, 3121 ) ( GSCALE( i ), i = 1, ng )

!  output the scale factors for the variables

         WRITE( outda, 3121 ) ( VSCALE( i ), i = 1, n )

!  output the lower and upper bounds on the objective function

        WRITE( outda, 3161 ) OBFBND( 1 ), OBFBND( 2 )

!  write REAL ( KIND = wp ) :: format

!  output the values of the nonzeros in each linear element, the
!  constant term in each group, the lower and upper bounds on
!  the variables and the starting point for the minimization

      ELSE

        WRITE( outda, 3120 ) ( ABYROW_val( i ), i = 1, nnza )
        WRITE( outda, 3120 ) ( B( i ), i = 1, ng )
        IF ( ialgor <= 2 ) THEN
           WRITE( outda, 3120 ) ( BL( i ), i = 1, n )
           WRITE( outda, 3120 ) ( BU( i ), i = 1, n )
        ELSE
           WRITE( outda, 3120 ) ( BL( i ), i = 1, n + ng )
           WRITE( outda, 3120 ) ( BU( i ), i = 1, n + ng )
        END IF
        WRITE( outda, 3120 ) ( X( i ), i = 1, n )
        IF ( ialgor >= 2 ) WRITE( outda, 3120 )( CLMULT( i ), i = 1, ng )

!  output the parameters in each group

        WRITE( outda, 3120 ) ( GP_val( i ), i = 1, GP_ptr( ng1 ) - 1 )

!  output the parameters in each individual element

        WRITE( outda, 3120 ) ( EP_val( i ), i = 1, EP_ptr( nel1 ) - 1 )

!  output the scale factors for the nonlinear elements

        WRITE( outda, 3120 ) ( ESCALE( i ), i = 1, ELING_ptr( ng1 ) - 1 )

!  output the scale factors for the groups

        WRITE( outda, 3120 ) ( GSCALE( i ), i = 1, ng )

!  output the scale factors for the variables

        WRITE( outda, 3120 ) ( VSCALE( i ), i = 1, n )

!  output the lower and upper bounds on the objective function

        WRITE( outda, 3160 ) OBFBND( 1 ), OBFBND( 2 )
      END IF

!  output a logical array which says whether an element has internal
!  variables

      WRITE( outda, 3130 ) ( INTREP( i ), i = 1, nelnum )

!  output a logical array which says whether a group is trivial

      WRITE( outda, 3130 ) ( GXEQX( i ), i = 1, ng )

!  output the names given to the groups and to the variables

      WRITE( outda, 3140 ) ( GNAMES( i ), i = 1, ng )
      WRITE( outda, 3140 ) ( VNAMES( i ), i = 1, n )

!  output the names given to the element and group types

      WRITE( outda, 3140 ) ( ETYPES( i ), i = 1, neltype )
      WRITE( outda, 3140 ) ( GTYPES( i ), i = 1, ngtype )

!  output the type of each variable

      WRITE( outda, 3110 ) ( TYPEV( i ), i = 1, n )
      GO TO 900

!  incorrect data specified

  800 CONTINUE
      RETURN

!  successful return

  900 CONTINUE
      status = 0
      RETURN

!  allocation errors

  980 CONTINUE
      WRITE( out, "( ' ** Message from INTERPRET_outsdif-',                    &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  non-executable statements

 1990 FORMAT( ' ** Exit from MAKE_outsdif - ', /,                              &
              ' Although the manual may suggest otherwise,',                   &
              ' non-trivial',/,                                                &
              ' groups are not allowed for inequality constraints',/)
 2460 FORMAT( ' ** Exit from MAKE_outsdif - constant name ', A10,              &
              ' not recognised ' )
 2470 FORMAT( ' ** Exit from MAKE_outsdif - bound name ', A10,                 &
              ' not recognised ' )
 2480 FORMAT( ' ** Exit from MAKE_outsdif - range name ', A10,                 &
              ' not recognised ' )
 2490 FORMAT( ' ** Exit from MAKE_outsdif - start point name ', A10,           &
              ' not recognised ' )
 2500 FORMAT( ' ** Exit from MAKE_outsdif - obj. bound name ', A10,            &
              ' not recognised ' )
 3000 FORMAT( ' Group ', A10, ' removed as a redundant objective ' )
 3010 FORMAT( /, ' Objective function ', A10, ' is group number ', I8 )
 3020 FORMAT( /, 3('  Row   Col    Value  '),                                  &
              /, 3('  ---   ---    -----  '),                                  &
              /, ( 3( 2I5, 1P, D12.4 ) ) )
 3100 FORMAT( I2, A10, I2 )
 3110 FORMAT( ( 10I8 ) )
 3120 FORMAT( ( 1P, 4D16.8 ) )
 3121 FORMAT( ( 1P, 4E16.8 ) )
 3130 FORMAT( ( 72L1 ) )
 3140 FORMAT( ( 8A10 ) )
 3160 FORMAT( 1P, 2D16.8 )
 3161 FORMAT( 1P, 2E16.8 )
 3170 FORMAT( 2I10 )
 3180 FORMAT( 10I10 )

!  end of subroutine MAKE_outsdif

      END SUBROUTINE MAKE_outsdif

!-*-*-*-*-*- S I F D E C O D E   P R I N T P    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE PRINT_details( n, ng, nelvar, neltype, nevnames, nepnames,    &
                                ngpnames, ngtype, nlvars, nelnum, neling,      &
                                nlisgp, nlisep, nnza,                          &
                                GSTATE, ELING_ptr, ELVAR, GTYPE, TYPEE,        &
                                ELV, INV, ELP, GTYPESP_ptr,                    &
                                GP_ptr, EP_ptr, EV_ptr, ELING_el, TYPEV, IWK,  &
                                ABYROW_ptr, ABYROW_col, ABYROW_val,            &
                                B, BL, BU, X, EP_val, GP_val,                  &
                                GSCALE, ESCALE, VSCALE, pname, VNAMES, GNAMES, &
                                LNAMES, ETYPES, EVNAMES, GANAMES, EPNAMES,     &
                                GPNAMES, GTYPES, out, print_level )
      INTEGER :: out, print_level, nlisgp, nlisep
      INTEGER :: nelvar, neltype, ngtype, nevnames, nepnames, ngpnames
      INTEGER :: n, ng, nlvars, nelnum, neling, nnza
      REAL ( KIND = wp ) :: EP_val( nlisep ), GP_val( nlisgp )
      INTEGER :: ELING_el( neling )
      INTEGER :: GSTATE( ng ), ABYROW_ptr( ng + 1 ), ABYROW_col( nnza )
      INTEGER :: ELV( neltype + 1 ), INV( neltype + 1 )
      INTEGER :: ELVAR( nelvar ), TYPEV( n )
      INTEGER :: ELING_ptr( ng + 1 ), GTYPE( ng ), TYPEE( nelnum )
      INTEGER :: ELP( neltype + 1 ), GTYPESP_ptr( ngtype + 1 ), IWK( nelnum )
      INTEGER :: EP_ptr( nelnum + 1 ), EV_ptr( nelnum + 1 ), GP_ptr( ng + 1 )
      CHARACTER ( LEN = 10 ) :: pname
      CHARACTER ( LEN = 10 ) :: GNAMES( ng ), VNAMES( n )
      CHARACTER ( LEN = 10 ) :: ETYPES( neltype ), LNAMES( nelnum )
      CHARACTER ( LEN = 10 ) :: EVNAMES( nevnames )
      CHARACTER ( LEN = 10 ) :: EPNAMES( nepnames ), GPNAMES( ngpnames )
      CHARACTER ( LEN = 10 ) :: GANAMES( ngtype ), GTYPES( ngtype )
!     REAL ( KIND = wp ) :: B( ng ), BL( n + ng ), BU( n + ng ), X( n )
      REAL ( KIND = wp ) :: B( ng ), BL( n ), BU( n ), X( n )
      REAL ( KIND = wp ) :: GSCALE( ng ), ESCALE( neling ), VSCALE( n )
      REAL ( KIND = wp ) :: ABYROW_val( nnza )

!  --------------------------------------------------------------------
!  print details of the problem previously specified in an SIF file

!  formerly PRINTP in SiFDec

! the level of printing performed is determined by the value of iprint
!  possible values are:

!  >= 1, a simple summary of the problem name, the number of variables,
!        groups and elements
!  >= 2, a list of the variables used
!  >= 3, a breakdown of the groups. a list of the nonlinear elements
!        used, the types of each group, the status of the group and
!        a statement that the group uses or does not use a linear
!        element
! = 4, further details of each group. the name of the group-type
!        variable and a list of the values associated with each
!        parameter
!  >= 5, details of each element. the numbers of elemental and
!        internal variables and the numbers of parameters
!  >= 6, further details of each element. the names of the
!        elemental variables together with their associated
!        problem variables, a list of the values associated
!        with each parameter and the variables involved in
!        the linear element
!  >= 7, details of the coefficients of the linear elements
!  >= 8, full details of the variables used including their lower
!        and upper bounds and starting values
!  >= 9, all of the above
!  --------------------------------------------------------------------

!  local variables

      INTEGER :: i, iel, ig, is, j, k, k1, k2, k3, k4, k5, k6, l, ieltyp

!  pararmeter definitions

      CHARACTER ( LEN = 3 ), DIMENSION( 3 ), PARAMETER :: VARTYP               &
        = (/ '   ', '0-1', 'int' /)
      CHARACTER ( LEN = 12 ), DIMENSION( 4 ), PARAMETER :: CONSTRAINT_status   &
        = (/ 'an objective', 'an equality ', 'a negativity', 'a positivity' /)

      IF ( out <= 0 ) RETURN
      IF ( print_level >= 1 ) THEN
        IWK( : nelnum ) = 0
        WRITE( out, 2000 ) pname, n, n - nlvars, ng, nelnum

!  list of variables

        IF ( print_level >= 2 ) THEN
          WRITE( out, 2010 ) ( VNAMES( j ), j = 1, nlvars )
          IF ( nlvars < n ) WRITE( out, 2020 ) ( VNAMES( j ), j = nlvars + 1, n)

!  group details

          IF ( print_level >= 3 ) THEN
            DO ig = 1, ng
              is = GTYPE( ig )
              IF ( is == 0 ) THEN
                   IF ( GSTATE( ig ) == 1 ) THEN
                     WRITE( out, 2030 ) ig, GNAMES( ig ),                      &
                     CONSTRAINT_status( 1 ), 'TRIVIAL   ', GSCALE( ig )
                   ELSE
                     WRITE( out, 2030 ) ig, GNAMES( ig ),                      &
                     CONSTRAINT_status( GSTATE( ig ) ),                        &
                     'TRIVIAL   ', GSCALE( ig )
                   END IF
              ELSE
                   IF ( ABS( GSTATE( ig ) ) == 1 ) THEN
                     WRITE( out, 2030 ) ig, GNAMES( ig ),                      &
                     CONSTRAINT_status( 1 ), GTYPES( is ), GSCALE( ig )
                   ELSE
                     WRITE( out, 2030 ) ig, GNAMES( ig ),                      &
                     CONSTRAINT_status( GSTATE( ig ) ),                        &
                     GTYPES( is ), GSCALE( ig )
                   END IF
              END IF
              k1 = ELING_ptr( ig )
              k2 = ELING_ptr( ig + 1 ) - 1
              l = k2 - k1 + 1
              IF ( k1 <= k2 ) THEN
                IF ( k1 == k2 ) THEN
                  WRITE( out, 2060 ) LNAMES( ELING_el( k1 ) )
                ELSE
                  WRITE( out, 2070 ) l, ( LNAMES( ELING_el( k ) ), k = k1, k2 )
                END IF
              ELSE
                WRITE( out, 2080 )
              END IF

!  further group details

              IF ( print_level == 4 .OR. print_level >= 7 ) THEN
                IF ( is > 0 ) THEN
                  k3 = GTYPESP_ptr( is ) - 1
                  k4 = GP_ptr( ig ) - 1
                  l = GP_ptr( ig + 1 ) - k4 - 1
                  IF ( is > 0 ) THEN
                    IF ( l == 1 ) THEN
                      WRITE( out, 2090 ) GANAMES( is ), 'is ', l, '. '
                    ELSE
                      WRITE( out, 2090 ) GANAMES( is ), 'are', l, 's.'
                    END IF
                  END IF
                  IF ( l > 0 ) WRITE( out, 2100 ) ( GPNAMES( k3 + i ),         &
                                                    GP_val( k4 + i ), i = 1, l )
                END IF
              END IF

!  element details

              IF ( print_level >= 5 ) THEN
                DO 400 k = k1, k2
                  iel = ELING_el( k )
                  ieltyp = TYPEE( iel )
                  IF ( IWK( iel ) == 0 ) THEN
                    WRITE( out, 2110 ) LNAMES( iel ), iel, ETYPES( ieltyp ),   &
                            ELV( ieltyp + 1 ) - ELV( ieltyp ),               &
                            INV( ieltyp + 1 ) - INV( ieltyp ),               &
                            ELP( ieltyp + 1 ) - ELP( ieltyp ), ESCALE( k )
                     IF ( print_level < 6 ) IWK( iel ) = 1
                  ELSE
                    WRITE( out, 2120 ) LNAMES( iel ), iel, ESCALE( k )
                  END IF

!  further element details

                  IF ( print_level >= 6 ) THEN
                    IF ( IWK( iel ) == 0 ) THEN
                      IWK( iel ) = 1
                      k3 = ELV ( ieltyp ) - 1
                      k4 = EV_ptr( iel ) - 1
                      l = EV_ptr( iel + 1 ) - k4 - 1
                      WRITE( out, 2130 )                                       &
                      ( EVNAMES( k3 + i ), VNAMES( ELVAR( k4 + i ) ), i = 1, l )
                      k3 = ELP( ieltyp )  - 1
                      k4 = EP_ptr( iel ) - 1
                      l = EP_ptr( iel + 1 ) - k4 - 1
                      IF ( l > 0 ) WRITE( out, 2150 )                          &
                              ( EPNAMES( k3 + i ), EP_val( k4 + i ), i = 1, l )
                    END IF
                  END IF
  400           CONTINUE
              END IF

!  linear element details

              k5 = ABYROW_ptr( ig )
              k6 = ABYROW_ptr( ig + 1 ) - 1
              IF ( print_level == 6 ) THEN
                IF ( k5 <= k6 ) THEN
                  IF ( k5 == k6 ) THEN
                    WRITE( out, 2040 ) k6 - k5 + 1, '. '
                    IF ( print_level >= 6 ) WRITE( out, 2140 ) ' ',            &
                      ( VNAMES( ABYROW_col( k ) ), k = k5, k6 )
                  ELSE
                    WRITE( out, 2040 ) k6 - k5 + 1, 's.'
                    IF ( print_level >= 6 ) WRITE( out, 2140 ) 's',            &
                      ( VNAMES( ABYROW_col( k ) ), k = k5, k6 )
                  END IF
                ELSE
                  IF ( ABS( B( ig ) ) < 1.0D-12 ) THEN
                    WRITE( out, 2050 )
                  ELSE
                    WRITE( out, 2160 )
                  END IF
                END IF
              ELSE

!  further linear element details

                IF ( k5 <= k6 ) THEN
                  IF ( k5 == k6 ) THEN
                    WRITE( out, 2040 ) k6 - k5 + 1, '. '
                    IF ( print_level >= 6 ) WRITE( out, 2200 ) ' ', ') ',      &
                      ( VNAMES( ABYROW_col( k ) ), ABYROW_val( k ), k = k5, k6 )
                  ELSE
                    WRITE( out, 2040 ) k6 - k5 + 1, 's.'
                    IF ( print_level >= 6 ) WRITE( out, 2200 ) 's', 's)',      &
                      ( VNAMES( ABYROW_col( k ) ), ABYROW_val( k ), k = k5, k6 )
                  END IF
                  IF ( ABS( B( ig ) ) < 1.0D-12 ) THEN
                    WRITE( out, 2230 )
                  ELSE
                    WRITE( out, 2220 ) B( ig )
                  END IF
                ELSE
                  IF ( ABS( B( ig ) ) < 1.0D-12 ) THEN
                    WRITE( out, 2050 )
                  ELSE
                    WRITE( out, 2210 ) B( ig )
                  END IF
                END IF
              END IF
            END DO
          END IF
        END IF
      END IF
      IF ( print_level >= 8 ) THEN
        WRITE( out, 2170 )
        DO i = 1, n
          WRITE( out, 2180 ) i, VNAMES( i ), BL( i ), X( i ), BU( i ),         &
                                VSCALE( i ), VARTYP( TYPEV( i ) + 1 )
        END DO
      END IF
      RETURN

!  non-executable statements

 2000 FORMAT( /, ' Problem name ', A10, /,                                     &
              /, ' There are ', I8, ' VARIABLES of which ', I8,                &
                 ' are artificials',                                           &
              /, ' There are ', I8, ' GROUPS',                                 &
              /, ' There are ', I8, ' NONLINEAR ELEMENTS ' )
 2010 FORMAT( /, ' Names of problem variables ',                               &
              /, ' ----- -- ------- --------- ',                               &
              /, 7( 1X, A10 ) )
 2020 FORMAT( /, ' Names of artificial variables ',                            &
              /, ' ----- -- ---------- --------- ',                            &
              /, 7( 1X, A10 ) )
 2030 FORMAT( /, ' Group ', I8, ' is named ', A10, /, '  * It is ',            &
              A12, ' group of type ', A10,                                     &
              /, '  * The group is scaled by the factor ', 1P, D12.4 )
 2040 FORMAT( '  * The group has a LINEAR ELEMENT with ', i6,                  &
              ' variable', A2 )
 2050 FORMAT( '  * The group has no LINEAR ELEMENT. ' )
 2060 FORMAT( '  * The group uses a single NONLINEAR',                         &
                 ' ELEMENT.  This is element ', A10 )
 2070 FORMAT( '  * The group uses ', i5, ' NONLINEAR',                         &
              ' ELEMENTS. These are elements ', A10,                           &
               /, ( 3X, 6( 1X, A10 ) ) )
 2080 FORMAT( '  * The group uses no NONLINEAR ELEMENTS. ' )
 2090 FORMAT( '  * The group-type argument is ', A10, ' and there ', A3,       &
                 I8, ' parameter', A2 )
 2100 FORMAT( ( '    * Group parameter ', A10, ' has the value ',              &
                1P, D12.4, '.' ) )
 2110 FORMAT(  '  * Group uses nonlinear element ', A10,                       &
               ' number ', i5, ' of type ', A10, /,                            &
               '    * No. elemental variables =', I8,                          &
               '. No. internal variables =', I8, '.',                          &
               /, '    * No. parameter values =', I8, '.', /,                  &
               '    * The element is scaled by the factor ', 1P, D12.4 )
 2120 FORMAT( '  * Group uses nonlinear element ', A10,                        &
              ' number ', i5, ' described above. ', /,                         &
              '    * The element is scaled by the factor ', 1P, D12.4 )
 2130 FORMAT( ( '    * Elemental variable ', A10, '  is assigned',             &
                ' problem variable ', A10 ) )
 2140 FORMAT( '    * The linear element uses variable', A1, 1X, 3A10,          &
              /, ( 6X, 6A10 ) )
 2150 FORMAT( ( '    * Elemental parameter ', A10, ' has the value ',          &
                1P, D12.4, '.' ) )
 2160 FORMAT( '  * The group has a constant LINEAR ELEMENT. ' )
 2170 FORMAT( /, '  #  variable name ',                                        &
                 'lower bound start value upper bound scale factor',           &
              ' type', /, '   -  ------------- ',                              &
                 '----------- ----------- ----------- ------------',           &
              ' ----' )
 2180 FORMAT( i5, 2X, A10, 1X, 1P, 4D12.4, 3X, A3 )
 2200 FORMAT( '    * The linear element uses variable', A1,                    &
              ' (with coefficient', A2,                                        &
              /, ( 6X, 3( A10, 1X, 1P, D10.2, 1X ) ) )
 2210 FORMAT( '    * The group has a constant LINEAR ELEMENT with',            &
              ' coefficient ', 1P, D10.2 )
 2220 FORMAT( '    * The constant term for the LINEAR ELEMENT has',            &
              ' coefficient ', 1P, D10.2 )
 2230 FORMAT( '    * There is no constant term for the LINEAR ELEMENT' )

!  end of subroutine PRINT_details

      END SUBROUTINE PRINT_details

!-*-*-  S I F D E C O D E   M A K E _ e l f u n    S U B R O U T I N E  -*-*-

      SUBROUTINE MAKE_elfun( input, out, outfn, outra, status,                 &
                             nevnames, nivnames, nepnames, neltype,            &
                             pname, EVNAMES, IVNAMES,                          &
                             len_renames, RENAMES, len_innames, INNAMES,       &
                             len_lonames, LONAMES, len_minames, MINAMES,       &
                             len_exnames, EXNAMES, DEFINED,                    &
                             length, TABLE, KEY, INLIST,                       &
                             ETYPES, ELV, INV, EPNAMES, ELP, debug, single,    &
                             nuline, gotlin, print_level )
      INTEGER :: input, out, outfn, outra, print_level, length, status
      INTEGER :: neltype
      INTEGER :: nevnames, nivnames, nepnames
      INTEGER :: len_renames, len_innames, len_lonames, len_minames, len_exnames
      LOGICAL :: debug, single, gotlin
      CHARACTER ( LEN = max_record_length ) :: nuline
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TABLE, INLIST
      INTEGER, DIMENSION( neltype + 1 ) :: ELV, INV, ELP
      LOGICAL, DIMENSION( neltype ) :: DEFINED
      CHARACTER ( LEN = 10 ) :: pname
      CHARACTER ( LEN = 10 ) :: EVNAMES( nevnames ), IVNAMES( nivnames )
      CHARACTER ( LEN = 10 ) :: EPNAMES( nepnames ), ETYPES( neltype )
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: RENAMES, INNAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: LONAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: MINAMES, EXNAMES
      CHARACTER ( LEN = 12 ), ALLOCATABLE, DIMENSION( : ) :: KEY

!  ----------------------------------------------------------------
!  make a function evaluation subroutine and a range transformation
!  subroutine from a gps function data file

!  function indicator cards
!  -------------------------

!  definition   purpose
!  ----------   --------
!  ELEMENTS     problem name
!  TEMPORARIES  names of additional parameters used in function defs
!  GLOBALS      general parameter assignments
!  INDIVIDUALS  define the transformation from the elemental to the
!               internal variables for all elements with internal vars
!               set function and derivative values and make
!               element specific parameter assignments
!  ENDATA       end of input data

!  data card description
!  ----------------------

!  see 'The SIF reference report', Chapter 7 in
!       A. R. Conn, N. I. M. Gould and Ph. L. Toint,
!       LANCELOT A Fortran Package for Large-Scale Nonlinear Optimization
!       (RElease A), Springer Series in Computational Mathematics 17,
!       Springer Verlag 1992

!  see also http://www.cuter.rl.ac.uk/sifdec/Doc/sif.pdf
!  and      http://www.numerical.rl.ac.uk/lancelot/sif/sifhtml.html

!  returns with negative values of status indicate that insufficient
!  array space has been allowed, as follows:

!    status = - 1  when length not large enough
!    status = - 2  when RENAMES, INNAMES, LONAMES, MINAMES or EXNAMES cannot be
!                  extended further
!  ----------------------------------------------------------------

!  parameter definitions

      INTEGER, PARAMETER :: mblank = 1, mfixed = 2, mfree = 3, mname = 4
      INTEGER, PARAMETER :: mtemp = 5, mglob = 6, mindiv = 7, mendat = 8
      INTEGER, PARAMETER :: iires = 32
      INTEGER, PARAMETER :: maxnul = 20
      INTEGER, DIMENSION( mendat ), PARAMETER :: LENIND                        &
        = (/ 0, 12, 11, 8, 11, 7, 11, 6 /)
      CHARACTER ( LEN = 12 ), DIMENSION( mendat ), PARAMETER :: INDIC8         &
        = (/ '            ', 'FIXED FORMAT', 'FREE FORMAT ', 'ELEMENTS    ',   &
             'TEMPORARIES ', 'GLOBALS     ', 'INDIVIDUALS ', 'ENDATA      '  /)
      CHARACTER ( LEN = 8 ), DIMENSION( iires ), PARAMETER :: FIELDI           &
        = (/ 'ELFUN   ', 'LFUVAL  ', 'FUVALS  ', 'XVALUE  ', 'NCALCF  ',       &
             'ITYPEE  ', 'ISTAEV  ', 'IELVAR  ', 'INTVAR  ', 'ISTADH  ',       &
             'ICALCF  ', 'IFFLAG  ', 'IELEMN  ', 'IELTYP  ', 'IHSTRT  ',       &
             'ILSTRT  ', 'IGSTRT  ', 'EPVALU  ', 'ISTEPA  ', 'IPSTRT  ',       &
             'JCALCF  ', 'LTYPEE  ', 'LSTAEV  ', 'LELVAR  ', 'LNTVAR  ',       &
             'LSTADH  ', 'LSTEPA  ', 'LCALCF  ', 'LFVALU  ', 'LXVALU  ',       &
             'LEPVLU  ', 'IFSTAT  ' /)

!  local variables

      INTEGER :: ifield, ifree, ihvar, ivar, intype, nminames, nlines, nlonames
      INTEGER :: jvar, k, k1, k2, nh, nhess, nvars, isetty, lineno, ilines
      INTEGER :: i, niname, ninnames, ninvar, nloop, nrenames, nexnames, npname
      INTEGER :: itype, j, is, js, novals, nelv, ninv, nn, nename
      INTEGER :: used_length, new_length, min_length, alloc_status
      LOGICAL :: nointe, defnam, endpar, endgen, firstl, setran
      LOGICAL :: startf, startg, starth, startp, qprod
      LOGICAL :: endoff, endofg, endofh, fixed, nomorg
      CHARACTER ( LEN = 2 ) :: field1
      CHARACTER ( LEN = 8 ) :: field3
      CHARACTER ( LEN = 10 ) :: field2
      CHARACTER ( LEN = 12 ) :: field, header
      CHARACTER ( LEN = 24 ) :: bad_alloc
      CHARACTER ( LEN = 41 ) :: field7
      CHARACTER ( LEN = max_record_length ) :: blnkln
      REAL ( KIND = wp ), DIMENSION( 2 ) :: VALUES
      CHARACTER ( LEN = 8 ), DIMENSION( 2 ) :: FIELDS
      INTEGER :: IJUMP( neltype  )
      CHARACTER ( LEN = 65 ), DIMENSION( maxnul ) :: NULINA
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: U
      LOGICAL, ALLOCATABLE, DIMENSION( : ) :: SETVEC

      IF ( out > 0 ) WRITE( out, 2900 )

!  set initial values for integer variables

      ninnames = 0 ; nrenames = 0 ; nlonames = 0 ; nminames = 0 ; nexnames = 0
      lineno = 0 ; nloop = neltype + 1 ; intype = 1 ; ilines = 0 ; nlines = 0

!  set initial values for logical variables

      defnam = .FALSE. ; endpar = .FALSE. ; starth = .FALSE. ; startp = .FALSE.
      endgen = .FALSE. ; firstl = .TRUE. ; nointe = .FALSE. ; fixed = .TRUE.
      gotlin = .FALSE.

!  allocate space to hold the largest range transformation matrix possible

      nn = 0 ; nhess = 0
      DO itype = 1, neltype
        nelv = ELV( itype + 1 ) - ELV( itype )
        ninv = INV( itype + 1 ) - INV( itype )
        nn = MAX( nn, ninv * nelv )
        nhess = MAX( nhess, ninv * ( ninv + 1 ) / 2 )
      END DO
      CALL ALLOCATE_array( U, nn, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'U' ; GO TO 980 ; END IF

      CALL ALLOCATE_array( SETVEC, nhess, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'SETVEC' ; GO TO 980 ; END IF

!  create a dictionary of the internal variable names used

      niname = INV( neltype + 1 ) - 1
      DO i = 1, niname
        field = IVNAMES( i ) // 'PF'
        CALL HASH_enlarge_and_insert( length, 12, field,                       &
                                      TABLE, KEY, INLIST, ifree )
        IF ( ifree <= 0 ) THEN
          IF ( ifree == 0 ) THEN
            status = - 1
            GO TO 700
          END IF
        ELSE
          nrenames = nrenames + 1
          IF ( nrenames > len_renames ) THEN
            used_length = nrenames - 1 ; min_length = nrenames
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( RENAMES, len_renames, used_length,              &
                               new_length, min_length, buffer,                 &
                               status, alloc_status, 'RENAMES' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'RENAMES' ; status = - 2 ; GO TO 980
            END IF
            len_renames = new_length
          END IF
          RENAMES( nrenames ) = IVNAMES( i )
        END IF
      END DO

!  include the names of the elemental variables used in this dictionary

      nename = ELV( neltype + 1 ) - 1
      DO i = 1, nename
        field = EVNAMES( i ) // 'PF'
        CALL HASH_enlarge_and_insert( length, 12, field,                       &
                                      TABLE, KEY, INLIST, ifree )
        IF ( ifree <= 0 ) THEN
          IF ( ifree == 0 ) THEN
            status = - 1
            GO TO 700
          END IF
        ELSE
          nrenames = nrenames + 1
          IF ( nrenames > len_renames ) THEN
            used_length = nrenames - 1 ; min_length = nrenames
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( RENAMES, len_renames, used_length,              &
                               new_length, min_length, buffer,                 &
                               status, alloc_status, 'RENAMES' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'RENAMES' ; status = - 2 ; GO TO 980
            END IF
            len_renames = new_length
          END IF
          RENAMES( nrenames ) = EVNAMES( i )
        END IF
      END DO

!  include the names of the elemental parameters used
!  in this dictionary

      npname = ELP( neltype + 1 ) - 1
      DO i = 1, npname
        field = EPNAMES( i ) // 'PF'
        CALL HASH_enlarge_and_insert( length, 12, field,                       &
                                      TABLE, KEY, INLIST, ifree )
        IF ( ifree <= 0 ) THEN
          IF ( ifree == 0 ) THEN
            status = - 1
            GO TO 700
          END IF
        ELSE
          nrenames = nrenames + 1
          IF ( nrenames > len_renames ) THEN
            used_length = nrenames - 1 ; min_length = nrenames
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( RENAMES, len_renames, used_length,              &
                               new_length, min_length, buffer,                 &
                               status, alloc_status, 'RENAMES' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'RENAMES' ; status = - 2 ; GO TO 980
            END IF
            len_renames = new_length
          END IF
          RENAMES( nrenames ) = EPNAMES( i )
        END IF
      END DO

!  find which element types have an internal representation

      isetty = 0
      DO itype = 1, neltype
        DEFINED( itype ) = .FALSE.
        IF ( ELV( itype + 1 ) - ELV( itype ) ==                                &
             INV( itype + 1 ) - INV( itype ) ) THEN
          IJUMP( itype ) = 99998
          nointe = .TRUE.
        ELSE
          IJUMP( itype ) = itype
        END IF
      END DO

!  set a blank line

      DO i = 1, max_record_length
        BLNKLN( i : i ) = ' '
      END DO

!  read next line

  100 CONTINUE
      IF ( ilines + 1 > nlines ) THEN

!  read next line from the input file

        lineno = lineno + 1
        nuline = blnkln
        IF ( fixed ) THEN
          READ( input, 1000, END = 590, ERR = 590 ) nuline
          IF ( out > 0 .AND. debug ) WRITE( out, 2990 ) lineno, nuline
        ELSE
          READ( input, 1010, END = 590, ERR = 590 ) nuline
          IF ( out > 0 .AND. debug ) WRITE( out, 2970 ) lineno, nuline

!  if the card is in free format, translate it into fixed format

          CALL FREE_format( nuline, max_record_length, mendat, INDIC8,         &
                            LENIND, NULINA, maxnul, nlines, .FALSE.,           &
                            status, out )
          IF ( status > 0 ) GO TO 800

!  if there are non-blank lines on the free format card, read the first

          IF ( nlines > 0 ) THEN
            ilines = 1
            nuline = blnkln
            nuline = NULINA( ilines )
            IF ( out > 0 .AND. debug ) WRITE( out, 2980 ) lineno, ilines, nuline

!  there are only blank lines on the free format card

          ELSE
            GO TO 100
          END IF
        END IF
      ELSE

!  read next line from the last encountered free format card

        ilines = ilines + 1
        nuline = blnkln
        nuline = NULINA( ilines )
        IF ( out > 0 .AND. debug ) WRITE( out, 2980 ) lineno, ilines, nuline
      END IF

!  consider the header part of the card

      header = NULINE( 1 : 12 )

!  ignore blank lines

      IF ( header == INDIC8( mblank ) ) GO TO 100
      IF ( NULINE( 1 : 1 ) /= ' ' ) THEN

!  ignore comment cards

        IF ( NULINE( 1 : 1 ) == '*' ) GO TO 100

!  check if we have entered fixed-format input

        IF ( header == INDIC8( mfixed ) ) THEN
          fixed = .TRUE.
          GO TO 100
        END IF

!  check if we have entered free-format input

        IF ( header == INDIC8( mfree ) ) THEN
          fixed = .FALSE.
          GO TO 100
        END IF

!  check that the first encountered indicator card is the elements card

        IF ( .NOT. defnam  ) THEN
          IF ( header /= INDIC8( mname ) ) THEN
!           IF ( neltype > 0 ) GO TO 930
            IF ( neltype > 0 ) THEN
              BACKSPACE( input, ERR = 590 )
              GO TO 590
            END IF
            IF ( out > 0 .AND. print_level /= 0 ) WRITE( out, 2010)
            gotlin = .TRUE.
            GO TO 600

!  indicator card is elements
!  ---------------------------

          ELSE
            IF ( pname  /= NULINE( 15 : 24 ) ) THEN
              status = 51
              IF ( out > 0 ) WRITE( out, 2510 )
              GO TO 800
            ELSE
              defnam = .TRUE.

!  -------- set up subroutine call for range routine

              IF ( single ) THEN
                WRITE( outra, 4001 ) pname, TRIM( version )
              ELSE
                WRITE( outra, 4000 ) pname, TRIM( version )
              END IF
              IF ( neltype > 1 ) THEN
                WRITE( outra, 4040 ) ( IJUMP( i ), i = 1, neltype )
                WRITE( outra, 4050 )
              END IF
              GO TO 100
            END IF
          END IF
        END IF

!  an indicator card has been found

        DO i = intype, mendat
          IF ( header == INDIC8( i ) ) THEN
            intype = i
            GO TO 120
          END IF
        END DO

!  the indicator card is not recognised

        status = 2
        IF ( out > 0 ) WRITE( out, 2020 )
        GO TO 800
  120   CONTINUE

!  the parameter values have been completed. write out the
!  first part of the generated subroutine

        IF ( intype >= mglob .AND. .NOT. endpar ) THEN
          endpar = .TRUE.

!  insert the list of reserved integer/real/logical variables into
!  the dictionary

          DO i = 1, iires
            field = FIELDI( i ) // '  PF'
            CALL HASH_enlarge_and_insert( length, 12, field,                   &
                                          TABLE, KEY, INLIST, ifree )
            IF ( ifree <= 0 ) THEN
              IF ( ifree == 0 ) THEN
                status = - 1
                GO TO 700
              END IF
              status = 59
              IF ( out > 0 ) WRITE( out, 2590 ) FIELDI( i )
              GO TO 800
            END IF
          END DO

!  -------- set up subroutine call and reserved parameter declarations

          IF ( single ) THEN
             WRITE( outfn, 3001 ) FIELDI( 1 )( 1 : 6 ),                        &
               FIELDI(  3 )( 1 : 6 ), FIELDI(  4 )( 1 : 6 ),                   &
               FIELDI( 18 )( 1 : 6 ),                                          &
             ( FIELDI(  i )( 1 : 6 ), i = 5, 10 ), FIELDI( 19 )( 1 : 6 ),      &
               FIELDI( 11 )( 1 : 6 ), ( FIELDI(  i )( 1 : 6 ), i = 22, 31 ),   &
               FIELDI( 12 )( 1 : 6 ), FIELDI( 32 )( 1 : 6 ),                   &
               FIELDI(  5 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),                   &
             ( FIELDI(  i )( 1 : 6 ), i = 22, 32 ),                            &
               FIELDI(  6 )( 1 : 6 ), FIELDI( 22 )( 1 : 6 ),                   &
               FIELDI(  7 )( 1 : 6 ), FIELDI( 23 )( 1 : 6 ),                   &
               FIELDI(  8 )( 1 : 6 ), FIELDI( 24 )( 1 : 6 ),                   &
               FIELDI(  9 )( 1 : 6 ), FIELDI( 25 )( 1 : 6 ),                   &
               FIELDI( 10 )( 1 : 6 ), FIELDI( 26 )( 1 : 6 ),                   &
               FIELDI( 19 )( 1 : 6 ), FIELDI( 27 )( 1 : 6 ),                   &
               FIELDI( 11 )( 1 : 6 ), FIELDI( 28 )( 1 : 6 ),                   &
               FIELDI(  3 )( 1 : 6 ), FIELDI( 29 )( 1 : 6 ),                   &
               FIELDI(  4 )( 1 : 6 ), FIELDI( 30 )( 1 : 6 ),                   &
               FIELDI( 18 )( 1 : 6 ), FIELDI( 31 )( 1 : 6 ),                   &
               pname, TRIM( version ), FIELDI( 13 )( 1 : 6 ),                  &
               FIELDI( 14 )( 1 : 6 ),                                          &
               FIELDI( 15 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),                   &
               FIELDI( 17 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),                   &
               FIELDI( 21 )( 1 : 6 )
          ELSE
            WRITE( outfn, 3000 ) FIELDI( 1 )( 1 : 6 ),                         &
               FIELDI(  3 )( 1 : 6 ), FIELDI(  4 )( 1 : 6 ),                   &
               FIELDI( 18 )( 1 : 6 ),                                          &
             ( FIELDI(  i )( 1 : 6 ), i = 5, 10 ), FIELDI( 19 )( 1 : 6 ),      &
               FIELDI( 11 )( 1 : 6 ), ( FIELDI(  i )( 1 : 6 ), i = 22, 31 ),   &
               FIELDI( 12 )( 1 : 6 ), FIELDI( 32 )( 1 : 6 ),                   &
               FIELDI(  5 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),                   &
             ( FIELDI(  i )( 1 : 6 ), i = 22, 32 ),                            &
               FIELDI(  6 )( 1 : 6 ), FIELDI( 22 )( 1 : 6 ),                   &
               FIELDI(  7 )( 1 : 6 ), FIELDI( 23 )( 1 : 6 ),                   &
               FIELDI(  8 )( 1 : 6 ), FIELDI( 24 )( 1 : 6 ),                   &
               FIELDI(  9 )( 1 : 6 ), FIELDI( 25 )( 1 : 6 ),                   &
               FIELDI( 10 )( 1 : 6 ), FIELDI( 26 )( 1 : 6 ),                   &
               FIELDI( 19 )( 1 : 6 ), FIELDI( 27 )( 1 : 6 ),                   &
               FIELDI( 11 )( 1 : 6 ), FIELDI( 28 )( 1 : 6 ),                   &
               FIELDI(  3 )( 1 : 6 ), FIELDI( 29 )( 1 : 6 ),                   &
               FIELDI(  4 )( 1 : 6 ), FIELDI( 30 )( 1 : 6 ),                   &
               FIELDI( 18 )( 1 : 6 ), FIELDI( 31 )( 1 : 6 ),                   &
               pname, TRIM( version ), FIELDI( 13 )( 1 : 6 ),                  &
               FIELDI( 14 )( 1 : 6 ),                                          &
               FIELDI( 15 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),                   &
               FIELDI( 17 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),                   &
               FIELDI( 21 )( 1 : 6 )
          END IF

! --------- insert integer declarations

          IF ( ninnames > 0 )                                                &
            WRITE( outfn, 3010 ) ( INNAMES( i ), i = 1, ninnames )

! --------- insert real declarations

          IF ( nrenames > 0 ) THEN
            IF ( single ) THEN
              WRITE( outfn, 3019 ) ( RENAMES( i ), i = 1, nrenames )
            ELSE
              WRITE( outfn, 3020 ) ( RENAMES( i ), i = 1, nrenames )
            END IF
          END IF

! --------- insert logical declarations

          IF ( nlonames > 0 )                                                  &
             WRITE( outfn, 3023 ) ( LONAMES( i ), i = 1, nlonames )

! --------- insert intrinsic declarations

          IF ( nminames > 0 )                                                  &
             WRITE( outfn, 3021 ) ( MINAMES( i ), i = 1, nminames )

! --------- insert external declarations

          IF ( nexnames > 0 )                                                  &
            WRITE( outfn, 3022 ) ( EXNAMES( i ), i = 1, nexnames )
          WRITE( outfn, 3009 ) FIELDI( 32 )( 1 : 6 )
        END IF

!  the general parameter assignments have been completed
!  continue with the construction of the generated subroutine

        IF ( intype >= mindiv .AND. .NOT. endgen ) THEN
          endgen = .TRUE.

! --------- start loop over elements

          WRITE( outfn, 3050 ) nloop,                                          &
                 FIELDI( 21 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),                 &
                 FIELDI( 13 )( 1 : 6 ), FIELDI( 11 )( 1 : 6 ),                 &
                 FIELDI( 21 )( 1 : 6 ),                                        &
                 FIELDI( 16 )( 1 : 6 ), FIELDI(  7 )( 1 : 6 ),                 &
                 FIELDI( 13 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),                 &
                 FIELDI(  9 )( 1 : 6 ), FIELDI( 13 )( 1 : 6 ),                 &
                 FIELDI( 20 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),                 &
                 FIELDI( 13 )( 1 : 6 ),                                        &
                 FIELDI( 12 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),                 &
                 FIELDI( 10 )( 1 : 6 ), FIELDI( 13 )( 1 : 6 )
          IF ( neltype > 1 ) THEN
             WRITE( outfn, 3051 )                                              &
                FIELDI( 14 )( 1 : 6 ), FIELDI(  6 )( 1 : 6 ),                  &
                FIELDI( 13 )( 1 : 6 ), ( i, i = 1, neltype )
             WRITE( outfn, 3052 ) FIELDI( 14 )( 1 : 6 )
          END IF

!  make sure that quadratic Hessian terms are included

          DO itype = 1, MIN( 2, neltype )

!  diagonal term

            IF ( ETYPES( itype ) == cqsqr ) THEN
              WRITE( outfn, 3060 ) ETYPES( itype )
              IF ( neltype > 1 ) WRITE( outfn, 3061 ) itype
              IF ( single ) THEN
                WRITE( outfn, 3053 ) 'E', 'E'
              ELSE
                WRITE( outfn, 3053 ) 'D', 'D'
              END IF
              DEFINED( itype ) = .TRUE.
              isetty = isetty + 1
              IF ( isetty < neltype ) WRITE( outfn, 3191 ) nloop

!  off-diagonal term

            ELSE IF ( ETYPES( itype ) == cqprod ) THEN
              WRITE( outfn, 3060 ) ETYPES( itype )
              IF ( neltype > 1 ) WRITE( outfn, 3061 ) itype
              IF ( single ) THEN
                WRITE( outfn, 3054 ) 'E', 'E', 'E'
              ELSE
                WRITE( outfn, 3054 ) 'D', 'D', 'D'
              END IF
              DEFINED( itype ) = .TRUE.
              isetty = isetty + 1
              IF ( isetty < neltype ) WRITE( outfn, 3191 ) nloop
            END IF
          END DO
        END IF

!  indicator card is endata
!  -------------------------

        IF ( intype == mendat ) GO TO 900
        GO TO 100
      ELSE

!  check that the first non commment card is the elements indicator card

        IF ( .NOT. defnam  ) THEN
!         IF ( neltype > 0 ) GO TO 930
          IF ( neltype > 0 ) THEN
            BACKSPACE( input, ERR = 590 )
            GO TO 590
          END IF
          IF ( out > 0 .AND. print_level /= 0 ) WRITE( out, 2010 )
          gotlin = .TRUE.
          GO TO 600
        END IF

!  a data card has been found
!  read the character fields 1 and 2 from the card

        field1 = NULINE(  2 :  3 )
        field2 = NULINE(  5 : 14 )
        IF ( intype == mindiv .AND. field1 == 'R ' ) THEN

!  read the character fields 3 and 5 from the card

          FIELDS( 1 ) = NULINE( 15 : 22 )
          FIELDS( 2 ) = NULINE( 40 : 47 )

!  check to see if there is are any numerical values to be read

          novals = 0
          IF ( FIELDS( 1 ) /= '        ' .AND. NULINE( 15 : 15 ) /= '[' ) THEN
            novals = 1
            CALL GET_value( NULINE( 25: 36 ), VALUES( 1 ) )
            IF ( FIELDS( 2 ) /= '        ' .AND. NULINE( 40 : 40 ) /= '[' ) THEN
              novals = 2
              CALL GET_value( NULINE( 50 : 61 ), VALUES( 2 ) )

!  remove fields with numerical values of zero

              IF ( VALUES( 2 ) == zero ) THEN
                 novals = 1
              END IF
            END IF
            IF ( VALUES( 1 ) == zero ) THEN
               IF ( novals == 2 ) THEN
                  VALUES( 1 ) = VALUES( 2 )
                  FIELDS( 1 ) = FIELDS( 2 )
               END IF
               novals = novals - 1
            END IF
          END IF

!  read the character fields 3 and 7 from the card

        ELSE
          field3 = NULINE( 15 : 22 )
          field7 = NULINE( 25 : 65 )

!  check that field3 is blank on 'a', 'f' and 'g' cards

          IF ( field1( 1 : 1 ) == 'A' .OR. field1( 1 : 1 ) == 'F' .OR.         &
               field1( 1 : 1 ) == 'G' ) THEN
            IF ( field3 /= '       ' ) THEN
              status = 72
              IF ( out > 0 ) WRITE( out, 2720 )
              GO TO 800
            END IF
          END IF
        END IF
      END IF

!  branch on the value of intype

      GO TO ( 100, 100, 100, 100, 200, 300, 400, 900 ), intype

!  indicator card is temporaries
!  ------------------------------

  200 CONTINUE

!  check to see if the parameter is integer, real, logical or a function

      IF ( field1 /= 'I ' .AND. field1 /= 'R ' .AND. field1 /= 'M ' .AND.      &
           field1 /= 'F ' .AND. field1 /= 'L ' ) THEN
        status = 54
        IF ( out > 0 ) WRITE( out, 2540 )
        GO TO 800
      END IF

!  if the parameter is a function, check to see that the name has
!  not already been used

      IF ( field1 == 'F ' ) THEN
        field = field2 // 'FU'
        CALL HASH_enlarge_and_insert( length, 12, field,                       &
                                      TABLE, KEY, INLIST, ifree )
        IF ( ifree <= 0 ) THEN
          IF ( ifree == 0 ) THEN
            status = - 1
            GO TO 700
          END IF
        ELSE
          nexnames = nexnames + 1
          IF ( nrenames > len_renames ) THEN
            used_length = nexnames - 1 ; min_length = nexnames
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( EXNAMES, len_exnames, used_length,              &
                               new_length, min_length, buffer,                 &
                               status, alloc_status, 'EXNAMES' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'EXNAMES' ; status = - 2 ; GO TO 980 ; END IF
            len_exnames = new_length
          END IF
          EXNAMES( nexnames ) = field2
        END IF

!  check to see that the parameter name has not already been used

      ELSE
        field = field2 // 'PF'
        CALL HASH_enlarge_and_insert( length, 12, field,                      &
                                      TABLE, KEY, INLIST, ifree )
        IF ( ifree <= 0 ) THEN
           IF ( ifree == 0 ) THEN
              status = - 1
              GO TO 700
           END IF
        ELSE
          IF ( field1 == 'R ' ) THEN
            nrenames = nrenames + 1
            IF ( nrenames > len_renames ) THEN
              used_length = nrenames - 1 ; min_length = nrenames
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( RENAMES, len_renames, used_length,            &
                                 new_length, min_length, buffer,               &
                                 status, alloc_status, 'RENAMES' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'RENAMES' ; status = - 2 ; GO TO 980 ; END IF
              len_renames = new_length
            END IF
            RENAMES( nrenames ) = field2
          ELSE IF ( field1 == 'M ' ) THEN
            nminames = nminames + 1
            IF ( nminames > len_minames ) THEN
              used_length = nminames - 1 ; min_length = nminames
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( MINAMES, len_minames, used_length,            &
                                 new_length, min_length, buffer,               &
                                 status, alloc_status, 'MINAMES' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'MINAMES' ; status = - 2 ; GO TO 980 ; END IF
              len_minames = new_length
            END IF
            MINAMES( nminames ) = field2
          ELSE IF ( field1 == 'L ' ) THEN
            nlonames = nlonames + 1
            IF ( nlonames > len_lonames ) THEN
              used_length = nlonames - 1 ; min_length = nlonames
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( LONAMES, len_lonames, used_length,            &
                                 new_length, min_length, buffer,               &
                                 status, alloc_status, 'LONAMES' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'LONAMES' ; status = - 2 ; GO TO 980 ; END IF
              len_lonames = new_length
            END IF
            LONAMES( nlonames ) = field2
          ELSE
            ninnames = ninnames + 1
            IF ( ninnames > len_innames ) THEN
              used_length = ninnames - 1 ; min_length = ninnames
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( INNAMES, len_innames, used_length,            &
                                 new_length, min_length, buffer,               &
                                 status, alloc_status, 'INNAMES' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'INNAMES' ; status = - 2 ; GO TO 980 ; END IF
              len_innames = new_length
            END IF
            INNAMES( ninnames ) = field2
          END IF
        END IF
      END IF
      GO TO 100

!  indicator card is globals
!  --------------------------

  300 CONTINUE
      IF ( field1 == 'A ' .OR. field1 == 'I ' .OR. field1 == 'E ' ) THEN
        startp = .TRUE.

!  start a parameter assignment. check to see that the parameter has
!  been defined

        field = field2 // 'PF'
        CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
        IF ( ifield <= 0 ) THEN
          IF ( ifree == 0 ) THEN
            status = - 1
            GO TO 700
          END IF
          status = 57
          IF ( out > 0 ) WRITE( out, 2570 )
          GO TO 800
        END IF

! --------- make general parameter assignments

        IF ( field1 == 'A ' ) THEN
          WRITE( outfn, 3030 ) FIELD2( 1 : 6 ), field7

! --------- make conditional parameter assignments

        ELSE

!  check that the logical variable has been defined

          field = field3 // '  PF'
          CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
          IF ( ifield <= 0 ) THEN
             IF ( ifree == 0 ) THEN
                status = - 1
                GO TO 700
             END IF
             status = 57
             IF ( out > 0 ) WRITE( out, 2570 )
             GO TO 800
          END IF
          IF ( field1 == 'I ' ) THEN
             WRITE( outfn, 3031 ) FIELD2( 1 : 6 ), FIELD3( 1 : 6 ), field7
          ELSE
             WRITE( outfn, 3032 ) FIELD2( 1 : 6 ), FIELD3( 1 : 6 ), field7
          END IF
        END IF

! -------- continue a parameter assignment

      ELSE
        IF ( field1( 2 : 2 ) == '+' .AND. startp ) THEN
          WRITE( outfn, 3040 ) field7
        ELSE
          status = 55
          IF ( out > 0 ) WRITE( out, 2550 )
          GO TO 800
        END IF
      END IF
      GO TO 100

!  indicator card is individuals
!  ------------------------------

  400 CONTINUE

!  check if a new element has been encountered

      IF ( field1 == 'T ' ) THEN

!  check to see if the range of a new element is to be defined

        IF ( firstl ) THEN

!  check if this is the first element

          firstl = .FALSE.

!  finish of the previous element, if any

        ELSE

! --------- set a component of h

          IF ( starth ) THEN
            IF ( .NOT. endofh ) THEN
              DO ihvar = 1, nhess
                IF ( .NOT. SETVEC( ihvar ) ) THEN
                  IF ( single ) THEN
                    WRITE( outfn, 3162 ) FIELDI(  3 )( 1 : 6 ),                &
                                         FIELDI( 15 )( 1 : 6 ), ihvar
                  ELSE
                    WRITE( outfn, 3161 ) FIELDI(  3 )( 1 : 6 ),                &
                                         FIELDI( 15 )( 1 : 6 ), ihvar
                  END IF
                END IF
              END DO
              endofh = .TRUE.
            END IF

! ---------- wind up h

            WRITE( outfn, 3180 )
          END IF

!  set the remaining gradient components to zero

          IF ( startg ) THEN
            IF ( .NOT. endofg ) THEN

! --------- set a component of g

              DO ivar = 1, ninvar
                IF ( .NOT. SETVEC( ivar ) ) THEN
                  IF ( single ) THEN
                    WRITE( outfn, 3132 ) FIELDI(  3 )( 1 : 6 ),                &
                                         FIELDI( 17 )( 1 : 6 ), ivar
                  ELSE
                    WRITE( outfn, 3131 ) FIELDI(  3 )( 1 : 6 ),                &
                                         FIELDI( 17 )( 1 : 6 ), ivar
                  END IF
                 END IF
              END DO
              endofg = .TRUE.
            END IF

! ---------- wind up f and g

          END IF
          IF ( startf ) THEN
            WRITE( outfn, 3190 )
          ELSE
            status = 61
            IF ( out > 0 ) WRITE( out, 2610 )
            GO TO 800
          END IF
          IF ( isetty < neltype ) WRITE( outfn, 3191 ) nloop
        END IF

!  find itype, the element type

        field = field2 // 'ET'
        CALL HASH_search( length, 12, field, TABLE, KEY, ifield )

!  the element type is unknown

        IF ( ifield <= 0 ) THEN
          status = 9
          IF ( out > 0 ) WRITE( out, 2090 )
          GO TO 800
        END IF

! --------- find type of current element

        itype = INLIST( ifield )
        WRITE( outfn, 3060 ) field2
        IF ( neltype > 1 ) WRITE( outfn, 3061 ) itype
        IF ( DEFINED( itype ) ) THEN
          status = 67
          IF ( out > 0 ) WRITE( out, 2670 )
          GO TO 800
        ELSE
          DEFINED( itype ) = .TRUE.
          isetty = isetty + 1
        END IF

!  find the row and column dimensions (ninv and nelv, resp.) of the
!  transformation matrix u. u is stored in vector form by columns

        is = INV( itype ) - 1
        js = ELV( itype ) - 1
        nelv = ELV( itype + 1 ) - ELV( itype )
        ninv = INV( itype + 1 ) - INV( itype )
        nn = ninv * nelv

! --------- find type of current element

        IF ( nelv > ninv ) WRITE( outra, 4060 ) field2, itype

!  initialize u as the zero matrix

        U( : nn ) = zero
        setran = nelv > ninv

! --------- set elemental variables

        k1 = ELV( itype ) ; k2 = ELV( itype + 1 ) - 1
        DO k = k1, k2
          ivar = k - k1 + 1
          WRITE( outfn, 3070 ) EVNAMES( k ), FIELDI(  4 )( 1 : 6 ),           &
              FIELDI(  8 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ), ivar
        END DO

! --------- set elemental parameters

        k1 = ELP( itype ) ; k2 = ELP( itype + 1 ) - 1
        DO k = k1, k2
          ivar = k - k1 + 1
          WRITE( outfn, 3071 ) EPNAMES( k ), FIELDI( 18 )( 1 : 6 ),           &
                               FIELDI( 20 )( 1 : 6 ), ivar
        END DO

!  find the number of internal variables and the required size of
!  the lower triangular portion of the Hessian matrix

        k1 = INV( itype ) ; k2 = INV( itype + 1 ) - 1
        ninvar = k2 - k1 + 1
        nhess = ninvar * ( ninvar + 1 ) / 2
        nvars = 0
        nh = 0
        startp = .FALSE. ; startf = .FALSE.
        startg = .FALSE. ; starth = .FALSE.
        endoff = .TRUE. ; endofg = .TRUE.
        endofh = .TRUE. ; nomorg = .FALSE.

!  the range transformation matrix u is now defined, entry by entry
!  determine which internal variable is given in field2

      ELSE
        IF ( field1 == 'R ' ) THEN
          DO i = 1, ninv
            IF ( field2 == IVNAMES( is + i ) ) GO TO 450
          END DO

!  the internal variable name is unrecognised

          status = 65
          IF ( out > 0 ) WRITE( out, 2650 )
          GO TO 800

!  the internal variable is the i-th in the list

  450     CONTINUE

!  determine which elemental variable(s) occur in fields

          IF ( novals > 0 ) THEN
            DO k = 1, novals
              DO j = 1, nelv
               IF ( FIELDS( k ) == EVNAMES( js + j ) ) GO TO 470
              END DO

!  the elemental variable name is unrecognised

              status = 66
              IF ( out > 0 ) WRITE( out, 2660 )
              GO TO 800

!  the elemental variable is the j-th in the list

  470         CONTINUE

!  insert the value of the new nonzero into u

              U( ninv * ( j - 1 ) + i ) = VALUES( k )
            END DO
          END IF
        ELSE
          IF ( field1( 1 : 1 ) == 'A' .OR. field1( 1 : 1 ) == 'I' .OR.         &
               field1( 1 : 1 ) == 'E' ) THEN
            IF ( startf ) THEN
              IF ( .NOT. endoff ) THEN
                WRITE( outfn, 3120 )
                endoff = .TRUE.
              END IF
            END IF
            IF ( startg ) THEN
              IF ( endofg .AND. .NOT. nomorg ) THEN
                WRITE( outfn, 3150 ) FIELDI( 12 )( 1 : 6 )
                nomorg = .TRUE.
              END IF
            END IF

!  start a parameter assignment

            IF ( field1( 2 : 2 ) == ' ' ) THEN
              startp = .TRUE.

!  set up the transformations for the element

              IF ( setran ) THEN
                CALL OUTRANGE( nelv, ninv, U, outfn, outra,                    &
                               EVNAMES( js + 1 ), IVNAMES( is + 1 ), single )
                setran = .FALSE.
              END IF

!  check to see that the parameter has been defined

              field = field2 // 'PF'
              CALL HASH_search(LENGTH, 12, field, TABLE, KEY, IFIELD)
              IF ( ifield <= 0 ) THEN
                 status = 58
                 IF ( out > 0 ) WRITE( out, 2580 )
                 GO TO 800
              END IF

! --------- make element-specific parameter assignments

              IF ( field1( 1 : 1 ) == 'A' ) THEN
                IF ( .NOT. startf ) THEN
                  WRITE( outfn, 3080 ) FIELD2( 1 : 6 ), field7
                ELSE
                  WRITE( outfn, 3083 ) FIELD2( 1 : 6 ), field7
                END IF

! --------- make conditional parameter assignments

              ELSE

!  check that the logical variable has been defined

                field = field3 // '  PF'
                CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
                IF ( ifield <= 0 ) THEN
                  IF ( ifree == 0 ) THEN
                    status = - 1
                    GO TO 700
                  END IF
                  status = 58
                  IF ( out > 0 ) WRITE( out, 2580 )
                  GO TO 800
                END IF
                IF ( field1( 1 : 1 ) == 'I' ) THEN
                  IF ( .NOT. startf ) THEN
                    WRITE( outfn, 3081 ) FIELD2( 1 : 6 ),                      &
                                         FIELD3( 1 : 6 ), field7
                  ELSE
                    WRITE( outfn, 3084 ) FIELD2( 1 : 6 ),                      &
                                         FIELD3( 1 : 6 ), field7
                  END IF
                ELSE
                  IF ( .NOT. startf ) THEN
                       WRITE( outfn, 3082 ) FIELD2( 1 : 6 ),                   &
                                            FIELD3( 1 : 6 ), field7
                  ELSE
                       WRITE( outfn, 3085 ) FIELD2( 1 : 6 ),                   &
                                            FIELD3( 1 : 6 ), field7
                  END IF
                END IF
              END IF

! --------- continuation of a parameter assignment

            ELSE
              IF ( field1( 2 : 2 ) == '+' ) THEN
                IF ( startp ) THEN
                  IF ( .NOT. startf ) THEN
                     WRITE( outfn, 3090 ) field7
                  ELSE
                     WRITE( outfn, 3091 ) field7
                  END IF
                ELSE
                  status = 56
                  IF ( out > 0 ) WRITE( out, 2560 )
                  GO TO 800
                END IF
              END IF
            END IF
          ELSE
            startp = .FALSE.
            IF ( field1( 1 : 1 ) == 'F' ) THEN

!  set the function value

              IF ( field1( 2 : 2 ) == ' ' ) THEN
                startf = .TRUE.
                endoff = .FALSE.

!  set up the transformations for the element

                IF ( setran ) THEN
                  CALL OUTRANGE( nelv, ninv, U, outfn, outra,                  &
                                 EVNAMES( js + 1 ), IVNAMES( is + 1 ), single )
                  setran = .FALSE.
                END IF

! --------- start f

                WRITE( outfn, 3100 ) FIELDI( 12 )( 1 : 6 ),                    &
                  FIELDI( 3 )( 1 : 6 ), FIELDI( 13 )( 1 : 6 ), field7

! --------- continuation of f

              ELSE
                IF ( field1( 2 : 2 ) == '+' ) THEN
                  IF ( startf ) THEN
                    WRITE( outfn, 3110 ) field7
                  ELSE
                    status = 56
                    IF ( out > 0 ) WRITE( out, 2560 )
                    GO TO 800
                  END IF
                END IF
              END IF

!  no function value has been specified

            ELSE
              IF ( field1( 1 : 1 ) == 'G' ) THEN
                IF ( .NOT. startf ) THEN
                  status = 61
                  IF ( out > 0 ) WRITE( out, 2610 )
                  GO TO 800
                END IF

!  set the gradient values

                IF ( field1( 2 : 2 ) == ' ' ) THEN
                  IF ( .NOT. startg ) THEN
                    startg = .TRUE.
                    endofg = .FALSE.

!  use the logical array setvec to ensure that all gradients are set

                    SETVEC( : ninvar ) = .FALSE.

! --------- start g

                    IF ( .NOT. endoff ) THEN
                      WRITE( outfn, 3120 )
                      endoff = .TRUE.
                    END IF
                  END IF

!  find which component is to be set

                  DO k = k1, k2
                    ivar = k - k1 + 1
                    IF ( field2 == IVNAMES( k ) ) GO TO 525
                  END DO

!  the component name is unrecognised

                  status = 60
                  IF ( out > 0 ) WRITE( out, 2600 )
                  GO TO 800
  525             CONTINUE

! --------- set a component of g

                  IF ( SETVEC( ivar ) ) THEN
                    status = 69
                    IF ( out > 0 ) WRITE( out, 2690 )
                    GO TO 800
                  END IF
                  SETVEC( ivar ) = .TRUE.
                  nvars = nvars + 1
                  endofg = nvars == ninvar
                  WRITE( outfn, 3130 ) FIELDI(  3 )( 1 : 6 ),                  &
                                       FIELDI( 17 )( 1 : 6 ), ivar, field7
                ELSE
                  IF ( field1( 2 : 2 ) == '+' ) THEN

! --------- continuation of g

                    IF ( startg .AND. .NOT. nomorg ) THEN
                      WRITE( outfn, 3140 ) field7
                    ELSE
                      status = 56
                      IF ( out > 0 ) WRITE( out, 2560 )
                      GO TO 800
                    END IF
                  END IF
                END IF

!  set the Hessian values

              ELSE
                IF ( field1( 1 : 1 ) == 'H' ) THEN
                  IF ( field1( 2 : 2 ) == ' ' ) THEN
                    IF ( .NOT. starth ) THEN

!  set the remaining gradient components to zero

                      IF ( .NOT. startg ) THEN

! --------- set a component of g

                        DO ivar = 1, ninvar
                          IF ( single ) THEN
                            WRITE( outfn, 3132 ) FIELDI(  3 )( 1 : 6 ),        &
                                                 FIELDI( 17 )( 1 : 6 ), ivar
                          ELSE
                            WRITE( outfn, 3131 ) FIELDI(  3 )( 1 : 6 ),        &
                                                 FIELDI( 17 )( 1 : 6 ), ivar
                          END IF
                        END DO
                        startg = .TRUE.
                        endofg = .TRUE.
                      END IF

! --------- set a component of g

                      IF ( .NOT. endofg ) THEN
                        DO ivar = 1, ninvar
                          IF ( .NOT. SETVEC( ivar ) ) THEN
                            IF ( single ) THEN
                              WRITE( outfn, 3132 ) FIELDI(  3 )( 1 : 6 ),      &
                                                   FIELDI( 17 )( 1 : 6 ), ivar
                            ELSE
                              WRITE( outfn, 3131 ) FIELDI(  3 )( 1 : 6 ),      &
                                                   FIELDI( 17 )( 1 : 6 ), ivar
                            END IF
                          END IF
                        END DO
                        endofg = .TRUE.
                      END IF
                      IF ( .NOT. nomorg ) THEN
                        WRITE( outfn, 3150 ) FIELDI( 12 )( 1 : 6 )
                        nomorg = .TRUE.
                      END IF
                      starth = .TRUE.
                      endofh = .FALSE.

!  use the logical array setvec to ensure that all Hessians are set

                      SETVEC( : nhess ) = .FALSE.
                    END IF

! ---------  start h


!  find which component is to be set

                    DO k = k1, k2
                      ivar = k - k1 + 1
                      IF ( field2 == IVNAMES( k ) ) GO TO 560
                    END DO

!  the component name field2 is unrecognised

                    status = 71
                    IF ( out > 0 ) WRITE( out, 2710 )
                    GO TO 800
  560               CONTINUE
                    DO k = k1, k2
                      jvar = k - k1 + 1
                      IF ( field3 == IVNAMES( k ) ) GO TO 580
                    END DO

!  the component name field3 is unrecognised

                    status = 71
                    IF ( out > 0 ) WRITE( out, 2710 )
                    GO TO 800
  580               CONTINUE

!  find the address of the component of the Hessian. the matrix is
!  stored as an upper triangle by rows

                    IF ( ivar > jvar ) THEN
                      i = ivar
                      ivar = jvar
                      jvar = i
                    END IF
                    ihvar = ivar + jvar * ( jvar - 1 ) / 2

!  ensure that the component has not already been set

                    IF ( SETVEC( ihvar ) ) THEN
                      status = 70
                      IF ( out > 0 ) WRITE( out, 2700 )
                      GO TO 800
                    END IF
                    SETVEC( ihvar ) = .TRUE.
                    nh = nh + 1
                    endofh = nh == nhess

! --------- set a component of h

                    WRITE( outfn, 3160 ) FIELDI(  3 )( 1 : 6 ),                &
                                         FIELDI( 15 )( 1 : 6 ), ihvar, field7

! --------- continuation of h

                  ELSE
                    IF ( field1( 2 : 2 ) == '+' ) THEN
                      IF ( starth ) THEN
                        WRITE( outfn, 3170 ) field7
                      ELSE
                        status = 56
                        IF ( out > 0 ) WRITE( out, 2560 )
                        GO TO 800
                      END IF
                    END IF
                  END IF
                ELSE
                  status = 56
                  IF ( out > 0 ) WRITE( out, 2560 )
                  GO TO 800
                END IF
              END IF
            END IF
          END IF
        END IF
      END IF
      GO TO 100

!  the end of the input file has been reached before the endata card

  590 CONTINUE

!  if the elements card has not been encountered, exit

      IF ( defnam ) THEN
        status = 52
        IF ( out > 0 ) WRITE( out, 2520 )
        RETURN
      END IF
      qprod = .FALSE.
      DO itype = 1, MIN( 2, neltype )
        IF ( ETYPES( itype ) /= cqsqr .AND.                                   &
             ETYPES( itype ) /= cqprod ) GO TO 930
        IF ( ETYPES( itype ) == cqprod ) qprod = .TRUE.
      END DO
      IF ( out > 0 .AND. print_level /= 0 ) WRITE( out, 2010 )

!  a dummy routine will be substituted

  600 CONTINUE

!  write a dummy elfuns routine

      IF ( neltype == 0 ) THEN
        IF ( single ) THEN
          WRITE( outfn, 3003 ) FIELDI( 1 )( 1 : 6 ),                           &
          FIELDI(  3 )( 1 : 6 ), FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ), &
         ( FIELDI(  i )( 1 : 6 ), i = 5, 10 ), FIELDI( 19 )( 1 : 6 ),          &
           FIELDI( 11 )( 1 : 6 ), ( FIELDI(  i )( 1 : 6 ), i = 22, 31 ),       &
           FIELDI( 12 )( 1 : 6 ), FIELDI( 32 )( 1 : 6 ),                       &
           FIELDI(  5 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),                       &
         ( FIELDI(  i )( 1 : 6 ), i = 22, 32 ),                                &
           FIELDI(  6 )( 1 : 6 ), FIELDI( 22 )( 1 : 6 ),                       &
           FIELDI(  7 )( 1 : 6 ), FIELDI( 23 )( 1 : 6 ),                       &
           FIELDI(  8 )( 1 : 6 ), FIELDI( 24 )( 1 : 6 ),                       &
           FIELDI(  9 )( 1 : 6 ), FIELDI( 25 )( 1 : 6 ),                       &
           FIELDI( 10 )( 1 : 6 ), FIELDI( 26 )( 1 : 6 ),                       &
           FIELDI( 19 )( 1 : 6 ), FIELDI( 27 )( 1 : 6 ),                       &
           FIELDI( 11 )( 1 : 6 ), FIELDI( 28 )( 1 : 6 ),                       &
           FIELDI(  3 )( 1 : 6 ), FIELDI( 29 )( 1 : 6 ),                       &
           FIELDI(  4 )( 1 : 6 ), FIELDI( 30 )( 1 : 6 ),                       &
           FIELDI( 18 )( 1 : 6 ), FIELDI( 31 )( 1 : 6 ), pname, TRIM( version )
        ELSE
          WRITE( outfn, 3002 ) FIELDI( 1 )( 1 : 6 ),                           &
          FIELDI(  3 )( 1 : 6 ), FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ), &
         ( FIELDI(  i )( 1 : 6 ), i = 5, 10 ), FIELDI( 19 )( 1 : 6 ),          &
           FIELDI( 11 )( 1 : 6 ), ( FIELDI(  i )( 1 : 6 ), i = 22, 31 ),       &
           FIELDI( 12 )( 1 : 6 ), FIELDI( 32 )( 1 : 6 ),                       &
           FIELDI(  5 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),                       &
         ( FIELDI(  i )( 1 : 6 ), i = 22, 32 ),                                &
           FIELDI(  6 )( 1 : 6 ), FIELDI( 22 )( 1 : 6 ),                       &
           FIELDI(  7 )( 1 : 6 ), FIELDI( 23 )( 1 : 6 ),                       &
           FIELDI(  8 )( 1 : 6 ), FIELDI( 24 )( 1 : 6 ),                       &
           FIELDI(  9 )( 1 : 6 ), FIELDI( 25 )( 1 : 6 ),                       &
           FIELDI( 10 )( 1 : 6 ), FIELDI( 26 )( 1 : 6 ),                       &
           FIELDI( 19 )( 1 : 6 ), FIELDI( 27 )( 1 : 6 ),                       &
           FIELDI( 11 )( 1 : 6 ), FIELDI( 28 )( 1 : 6 ),                       &
           FIELDI(  3 )( 1 : 6 ), FIELDI( 29 )( 1 : 6 ),                       &
           FIELDI(  4 )( 1 : 6 ), FIELDI( 30 )( 1 : 6 ),                       &
           FIELDI( 18 )( 1 : 6 ), FIELDI( 31 )( 1 : 6 ), pname, TRIM( version )
         END IF
         WRITE( outfn, 3009 ) FIELDI( 32 )( 1 : 6 )
         WRITE( outfn, 3201 )
      ELSE
        IF ( single ) THEN
          WRITE( outfn, 3001 ) FIELDI( 1 )( 1 : 6 ),                           &
          FIELDI(  3 )( 1 : 6 ), FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ), &
         ( FIELDI(  i )( 1 : 6 ), i = 5, 10 ), FIELDI( 19 )( 1 : 6 ),          &
           FIELDI( 11 )( 1 : 6 ), ( FIELDI(  i )( 1 : 6 ), i = 22, 31 ),       &
           FIELDI( 12 )( 1 : 6 ), FIELDI( 32 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),&
           FIELDI( 12 )( 1 : 6 ), ( FIELDI(  i )( 1 : 6 ), i = 22, 32 ),       &
           FIELDI(  6 )( 1 : 6 ), FIELDI( 22 )( 1 : 6 ),                       &
           FIELDI(  7 )( 1 : 6 ), FIELDI( 23 )( 1 : 6 ),                       &
           FIELDI(  8 )( 1 : 6 ), FIELDI( 24 )( 1 : 6 ),                       &
           FIELDI(  9 )( 1 : 6 ), FIELDI( 25 )( 1 : 6 ),                       &
           FIELDI( 10 )( 1 : 6 ), FIELDI( 26 )( 1 : 6 ),                       &
           FIELDI( 19 )( 1 : 6 ), FIELDI( 27 )( 1 : 6 ),                       &
           FIELDI( 11 )( 1 : 6 ), FIELDI( 28 )( 1 : 6 ),                       &
           FIELDI(  3 )( 1 : 6 ), FIELDI( 29 )( 1 : 6 ),                       &
           FIELDI(  4 )( 1 : 6 ), FIELDI( 30 )( 1 : 6 ),                       &
           FIELDI( 18 )( 1 : 6 ), FIELDI( 31 )( 1 : 6 ),                       &
           pname, TRIM( version ),                                             &
           FIELDI( 13 )( 1 : 6 ), FIELDI( 14 )( 1 : 6 ),                       &
           FIELDI( 15 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),                       &
           FIELDI( 17 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ), FIELDI( 21 )( 1 : 6 )
            IF ( qprod ) THEN
               WRITE( outfn, 3019 ) 'X     ', 'Y     '
            ELSE
               WRITE( outfn, 3019 ) 'X     '
            END IF
        ELSE
          WRITE( outfn, 3000 ) FIELDI( 1 )( 1 : 6 ),                           &
          FIELDI(  3 )( 1 : 6 ), FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ), &
         ( FIELDI(  i )( 1 : 6 ), i = 5, 10 ), FIELDI( 19 )( 1 : 6 ),          &
           FIELDI( 11 )( 1 : 6 ), ( FIELDI(  i )( 1 : 6 ), i = 22, 31 ),       &
           FIELDI( 12 )( 1 : 6 ), FIELDI( 32 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),&
           FIELDI( 12 )( 1 : 6 ), ( FIELDI(  i )( 1 : 6 ), i = 22, 32 ),       &
           FIELDI(  6 )( 1 : 6 ), FIELDI( 22 )( 1 : 6 ),                       &
           FIELDI(  7 )( 1 : 6 ), FIELDI( 23 )( 1 : 6 ),                       &
           FIELDI(  8 )( 1 : 6 ), FIELDI( 24 )( 1 : 6 ),                       &
           FIELDI(  9 )( 1 : 6 ), FIELDI( 25 )( 1 : 6 ),                       &
           FIELDI( 10 )( 1 : 6 ), FIELDI( 26 )( 1 : 6 ),                       &
           FIELDI( 19 )( 1 : 6 ), FIELDI( 27 )( 1 : 6 ),                       &
           FIELDI( 11 )( 1 : 6 ), FIELDI( 28 )( 1 : 6 ),                       &
           FIELDI(  3 )( 1 : 6 ), FIELDI( 29 )( 1 : 6 ),                       &
           FIELDI(  4 )( 1 : 6 ), FIELDI( 30 )( 1 : 6 ),                       &
           FIELDI( 18 )( 1 : 6 ), FIELDI( 31 )( 1 : 6 ),                       &
           pname, TRIM( version ),                                             &
           FIELDI( 13 )( 1 : 6 ), FIELDI( 14 )( 1 : 6 ),                       &
           FIELDI( 15 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),                       &
           FIELDI( 17 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ), FIELDI( 21 )( 1 : 6 )
            IF ( qprod ) THEN
               WRITE( outfn, 3020 ) 'X     ', 'Y     '
            ELSE
               WRITE( outfn, 3020 ) 'X     '
            END IF
        END IF
        WRITE( outfn, 3009 ) FIELDI( 32 )( 1 : 6 )
        WRITE( outfn, 3050 ) nloop,                                            &
                FIELDI( 21 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),                  &
                FIELDI( 13 )( 1 : 6 ), FIELDI( 11 )( 1 : 6 ),                  &
                FIELDI( 21 )( 1 : 6 ),                                         &
                FIELDI( 16 )( 1 : 6 ), FIELDI(  7 )( 1 : 6 ),                  &
                FIELDI( 13 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),                  &
                FIELDI(  9 )( 1 : 6 ), FIELDI( 13 )( 1 : 6 ),                  &
                FIELDI( 20 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),                  &
                FIELDI( 13 )( 1 : 6 ),                                         &
                FIELDI( 12 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),                  &
                FIELDI( 10 )( 1 : 6 ), FIELDI( 13 )( 1 : 6 )
        IF ( neltype > 1 ) THEN
         WRITE( outfn, 3051 ) FIELDI( 14 )( 1 : 6 ), FIELDI(  6 )( 1 : 6 ),    &
                              FIELDI( 13 )( 1 : 6 ), ( i, i = 1, neltype )
          WRITE( outfn, 3052 ) FIELDI( 14 )( 1 : 6 )
        END IF

!  make sure that quadratic Hessian terms are included

        DO itype = 1, MIN( 2, neltype )

!  diagonal term

          IF ( ETYPES( itype ) == cqsqr ) THEN
            WRITE( outfn, 3060 ) ETYPES( itype )
            IF ( neltype > 1 ) WRITE( outfn, 3061 ) itype
            IF ( single ) THEN
              WRITE( outfn, 3053 ) 'E', 'E'
            ELSE
              WRITE( outfn, 3053 ) 'D', 'D'
            END IF
            DEFINED( itype ) = .TRUE.
            isetty = isetty + 1
            IF ( isetty < neltype ) WRITE( outfn, 3191 ) nloop

!  off-diagonal term

          ELSE IF ( ETYPES( itype ) == cqprod ) THEN
            WRITE( outfn, 3060 ) ETYPES( itype )
            IF ( neltype > 1 ) WRITE( outfn, 3061 ) itype
            IF ( single ) THEN
              WRITE( outfn, 3054 ) 'E', 'E', 'E'
            ELSE
              WRITE( outfn, 3054 ) 'D', 'D', 'D'
            END IF
            DEFINED( itype ) = .TRUE.
            isetty = isetty + 1
            IF ( isetty < neltype ) WRITE( outfn, 3191 ) nloop
          END IF
        END DO
        WRITE( outfn, 3200 ) nloop
      END IF

!  write a dummy range routine

      IF ( single ) THEN
         WRITE( outra, 4003 ) pname, TRIM( version )
      ELSE
         WRITE( outra, 4002 ) pname, TRIM( version )
      END IF
      WRITE( outra, 4080 )
      WRITE( outra, 4090 )
      status = 0
      RETURN

!  insufficient space to continue construction

  700 CONTINUE
      IF ( out > 0 ) WRITE( out, 2000 )
      RETURN

!  subroutine incomplete

  800 CONTINUE
      IF ( out > 0 ) WRITE( out, 2990 ) lineno, nuline
      RETURN

!  subroutine successfully completed

  900 CONTINUE
      IF ( .NOT. firstl ) THEN

!  finish of the previous element, if any

        IF ( starth ) THEN
          IF ( .NOT. endofh ) THEN
            DO ihvar = 1, nhess

! --------- set a component of h

              IF ( .NOT. SETVEC( ihvar ) ) THEN
                 IF ( single ) THEN
                   WRITE( outfn, 3162 ) FIELDI(  3 )( 1 : 6 ),                 &
                                        FIELDI( 15 )( 1 : 6 ), ihvar
                 ELSE
                   WRITE( outfn, 3161 ) FIELDI(  3 )( 1 : 6 ),                 &
                                        FIELDI( 15 )( 1 : 6 ), ihvar
                 END IF
              END IF
            END DO
            endofh = .TRUE.
          END IF

! ---------- wind up h

          WRITE( outfn, 3180 )
        END IF

!  set the remaining gradient components to zero

        IF ( startg ) THEN

! --------- set a component of g

          IF ( .NOT. endofg ) THEN
            DO ivar = 1, ninvar
              IF ( .NOT. SETVEC( ivar ) ) THEN
                IF ( single ) THEN
                  WRITE( outfn, 3132 ) FIELDI(  3 )( 1 : 6 ),                  &
                                       FIELDI( 17 )( 1 : 6 ), ivar
                ELSE
                  WRITE( outfn, 3131 ) FIELDI(  3 )( 1 : 6 ),                  &
                                       FIELDI( 17 )( 1 : 6 ), ivar
                END IF
              END IF
            END DO
            endofg = .TRUE.
          END IF

! ---------- wind up f and g

        END IF
        IF ( startf ) THEN
          WRITE( outfn, 3190 )
        ELSE
          status = 61
          IF ( out > 0 ) WRITE( out, 2610 )
          GO TO 800
        END IF
        IF ( isetty < neltype ) WRITE( outfn, 3191 ) nloop
      END IF

! ---------- successful run. wind up output

      status = 0
      WRITE( outfn, 3200 ) nloop
      IF ( nointe ) WRITE( outra, 4070 )
      IF ( neltype == 0 ) WRITE( outra, 4080 )
      WRITE( outra, 4090 )

!   check that all element types have been defined

  930 CONTINUE
      DO itype = 1, neltype
        IF ( .NOT. DEFINED( itype ) ) THEN
          status = 68
          IF ( out > 0 ) WRITE( out, 2680 ) ETYPES( itype )
        END IF
      END DO
      RETURN

!  allocation errors

  980 CONTINUE
      WRITE( out, "( ' ** Message from MAKE_elfun-',                           &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  non-executable statements

 1000 FORMAT( A72 )
 1010 FORMAT( A160 )
 2000 FORMAT( ' ** Exit from MAKE_elfun - insufficient memory available',      &
              ' to enlarge hash table' )
 2010 FORMAT( ' ** Exit from MAKE_elfun - warning.',                           &
              ' First card not elements. ', /, '    A dummy',                  &
              ' routine will be substituted ' )
 2020 FORMAT( ' ** Exit from MAKE_elfun - indicator card not recognised ' )
 2090 FORMAT( ' ** Exit from MAKE_elfun - element type not recognised ' )
 2510 FORMAT( ' ** Exit from MAKE_elfun -',                                    &
              ' name on card not that specified on input ' )
 2520 FORMAT( ' ** Exit from MAKE_elfun - data file incomplete.',              &
              ' No ENDATA card ' )
 2540 FORMAT( ' ** Exit from MAKE_elfun -',                                    &
              ' unrecognised field 1 in TEMPORARIES section' )
 2550 FORMAT( ' ** Exit from MAKE_elfun -',                                    &
              ' unrecognised field 1 in GLOBALS section' )
 2560 FORMAT( ' ** Exit from MAKE_elfun -',                                    &
              ' unrecognised field 1 in INDIVIDUALS section' )
 2570 FORMAT( ' ** Exit from MAKE_elfun -',                                    &
              ' undefined parameter in GLOBALS section' )
 2580 FORMAT( ' ** Exit from MAKE_elfun -',                                    &
              ' undefined parameter in INDIVIDUALS section' )
 2590 FORMAT( ' ** Exit from MAKE_elfun - repeated parameter name ', A8 )
 2600 FORMAT( ' ** Exit from MAKE_elfun - unknown component of gradient ' )
 2610 FORMAT( ' ** Exit from MAKE_elfun - function not set '  )
 2650 FORMAT( ' ** Exit from MAKE_elfun -',                                    &
              ' internal variable not recognised ' )
 2660 FORMAT( ' ** Exit from MAKE_elfun -',                                    &
              ' elemental variable not recognised ' )
 2670 FORMAT( ' ** Exit from MAKE_elfun - element type already defined ' )
 2680 FORMAT( ' ** Exit from MAKE_elfun - warning, element type ', A10,        &
              ' undefined ' )
 2690 FORMAT( ' ** Exit from MAKE_elfun -',                                    &
              ' gradient component already defined ' )
 2700 FORMAT( ' ** Exit from MAKE_elfun -',                                    &
              ' Hessian component already defined ' )
 2710 FORMAT( ' ** Exit from MAKE_elfun - unknown component of Hessian '  )
 2720 FORMAT( ' ** Exit from MAKE_elfun - field 3 not blank on',               &
              ' A, F or G card ' )
 2900 FORMAT( ' ' )
 2970 FORMAT( ' Line ', i5, 4X, A160 )
 2980 FORMAT( ' Line ', i5, '.', i2, 1X, A65 )
 2990 FORMAT( ' Line ', i5, 4X, A65 )
 3000 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', A6, /,                                         &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', A6, '(', A6, ')', /,                           &
              '      DOUBLE PRECISION ', A6, '(', A6, '), ',                   &
                                         A6, '(', A6, '), ',                   &
                                         A6, '(', A6, ')', /,                  &
              'C', /, 'C  Problem name : ', A10, /,                            &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
              '      INTEGER ', 5( A6, ', ' ), A6, /,                          &
              '      INTEGER ', A6 )
 3001 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', A6, /,                                         &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', A6, '(', A6, ')', /,                           &
              '      REAL             ', A6, '(', A6, '), ',                   &
                                         A6, '(', A6, '), ',                   &
                                         A6, '(', A6, ')', /,                  &
              'C', /, 'C  Problem name : ', A10, /,                            &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
              '      INTEGER ', 5( A6, ', ' ), A6, /,                          &
              '      INTEGER ', A6 )
 3002 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', A6, /,                                         &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', A6, '(', A6, ')', /,                           &
              '      DOUBLE PRECISION ', A6, '(', A6, '), ',                   &
                                         A6, '(', A6, '), ',                   &
                                         A6, '(', A6, ')', /,                  &
              'C', /, 'C  Problem name : ', A10, /,                            &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C' )
 3003 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', A6, /,                                         &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', A6, '(', A6, ')', /,                           &
              '      REAL             ', A6, '(', A6, '), ',                   &
                                         A6, '(', A6, '), ',                   &
                                         A6, '(', A6, ')', /,                  &
              'C', /, 'C  Problem name : ', A10, /,                            &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C' )
 3009 FORMAT( '      ', A6, ' = 0' )
 3010 FORMAT( ( '      INTEGER ', A6, :, 4( ', ', A6, : ) ) )
 3019 FORMAT( ( '      REAL             ', A6, :, 4( ', ', A6, : ) ) )
 3020 FORMAT( ( '      DOUBLE PRECISION ', A6, :, 4( ', ', A6, : ) ) )
 3021 FORMAT( ( '      INTRINSIC ', A6, :, 4( ', ', A6, : ) ) )
 3022 FORMAT( ( '      EXTERNAL ', A6, :, 4( ', ', A6, : ) ) )
 3023 FORMAT( ( '      LOGICAL ', A6, 4( :, ', ', A6 ) ) )
 3030 FORMAT( '      ', A6, ' = ', A41 )
 3031 FORMAT( '      IF (', A6, ') ', A6, ' = ', A41 )
 3032 FORMAT( '      IF (.NOT.', A6, ') ', A6, ' = ', A41 )
 3040 FORMAT( '     *         ', A41 )
 3050 FORMAT( '      DO ', i5, 1X, A6, ' = 1, ', A6, /,                        &
              '       ', A6, ' = ', A6, '(', A6, ') ', /,                      &
              '       ', A6, ' = ', A6, '(', A6, ') - 1', /,                   &
              '       ', A6, ' = ', A6, '(', A6, ') - 1', /,                   &
              '       ', A6, ' = ', A6, '(', A6, ') - 1', /,                   &
              '       IF ( ', A6, ' == 3 ) ',                                  &
                      A6, ' = ', A6, '(', A6, ') - 1' )
 3051 FORMAT( '       ', A6, ' = ', A6, '(', A6, ')', /,                       &
              '       GO TO (', 8( i5, :, ',' ), /,                            &
            ( '     *        ', 8( i5, :, ',' ) ) )
 3052 FORMAT( '     *        ', 48X, '), ', A6 )
 3053 FORMAT( '       X = XVALUE(IELVAR(ILSTRT+     1))', /,                   &
              '       IF ( IFFLAG == 1 ) THEN', /,                             &
              '        FUVALS(IELEMN)= 5.0', A1, '-1 * X * X', /,              &
              '       ELSE', /,                                                &
              '        FUVALS(IGSTRT+     1)= X', /,                           &
              '        IF ( IFFLAG == 3 ) THEN', /,                            &
              '         FUVALS(IHSTRT+     1)= 1.0', A1, '+0', /,              &
              '        END IF', /,                                             &
              '       END IF' )
 3054 FORMAT( '       X = XVALUE(IELVAR(ILSTRT+     1))', /,                   &
              '       Y = XVALUE(IELVAR(ILSTRT+     2))', /,                   &
              '       IF ( IFFLAG == 1 ) THEN', /,                             &
              '        FUVALS(IELEMN)= X * Y', /,                              &
              '       ELSE', /,                                                &
              '        FUVALS(IGSTRT+     1)= Y', /,                           &
              '        FUVALS(IGSTRT+     2)= X', /,                           &
              '        IF ( IFFLAG == 3 ) THEN', /,                            &
              '         FUVALS(IHSTRT+     1)= 0.0', A1, '+0', /,              &
              '         FUVALS(IHSTRT+     2)= 1.0', A1, '+0', /,              &
              '         FUVALS(IHSTRT+     3)= 0.0', A1, '+0', /,              &
              '        END IF', /,                                             &
              '       END IF' )
 3060 FORMAT( 'C', /, 'C  Element type : ', A10, /, 'C' )
 3061 FORMAT( i5, '  CONTINUE' )
 3070 FORMAT( '       ', A6, ' = ', A6, '(', A6, '(', A6, '+', i6, '))')
 3071 FORMAT( '       ', A6, ' = ', A6, '(', A6, '+', i6, ')')
 3080 FORMAT( '       ', A6, ' = ', A41 )
 3081 FORMAT( '       IF (', A6, ') ', A6, ' = ', A41 )
 3082 FORMAT( '       IF (.NOT.', A6, ') ', A6, '=', A41 )
 3083 FORMAT( '        ', A6, ' = ', A41 )
 3084 FORMAT( '        IF (', A6, ') ', A6, ' = ', A41 )
 3085 FORMAT( '        IF (.NOT.', A6, ')', A6, '=', A41 )
 3090 FORMAT( '     *          ', A41 )
 3091 FORMAT( '     *           ', A41 )
 3100 FORMAT( '       IF ( ', A6, ' == 1 ) THEN', /,                           &
              '        ', A6, '(', A6, ')= ', A41 )
 3110 FORMAT( '     *                  ', A41 )
 3120 FORMAT( '       ELSE' )
 3130 FORMAT( '        ', A6, '(', A6, '+', i6, ')= ', A41 )
 3131 FORMAT( '        ', A6, '(', A6, '+', i6, ')= 0.0D+0' )
 3132 FORMAT( '        ', A6, '(', A6, '+', i6, ')= 0.0E+0' )
 3140 FORMAT( '     *                         ', A41 )
 3150 FORMAT( '        IF ( ', A6, ' == 3 ) THEN' )
 3160 FORMAT( '         ', A6, '(', A6, '+', i6, ')=', A41 )
 3161 FORMAT( '         ', A6, '(', A6, '+', i6, ')=0.0D+0' )
 3162 FORMAT( '         ', A6, '(', A6, '+', i6, ')=0.0E+0' )
 3170 FORMAT( '     *                         ', A41 )
 3180 FORMAT( '        END IF' )
 3190 FORMAT( '       END IF' )
 3191 FORMAT( '       GO TO', i6 )
 3200 FORMAT( i5,  ' CONTINUE', /, '      RETURN', /,                          &
              '      END' )
 3201 FORMAT( '      RETURN', /,                                               &
              '      END' )
 4000 FORMAT( '      SUBROUTINE RANGE( IELEMN, TRANSP, W1,',                   &
              ' W2, nelvar, ninvar,', /,                                       &
              '     *                  itype, LW1, LW2 )', /,                  &
              '      INTEGER IELEMN, nelvar, ninvar, itype,',                  &
              ' LW1, LW2', /,                                                  &
              '      LOGICAL TRANSP', /,                                       &
              '      DOUBLE PRECISION W1( LW1 ), W2( LW2 )', /,                &
              'C', /, 'C  Problem name : ', A10, /,                            &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
              'C  TRANSP = .FALSE. <=> W2 = U * W1', /,                        &
              'C  TRANSP = .TRUE.  <=> W2 = U(transpose) * W1', /,             &
              'C', /,                                                          &
              '      INTEGER I' )
 4001 FORMAT( '      SUBROUTINE RANGE( IELEMN, TRANSP, W1,',                   &
              ' W2, nelvar, ninvar,', /,                                       &
              '     *                  itype, LW1, LW2 )', /,                  &
              '      INTEGER IELEMN, nelvar, ninvar, itype,',                  &
              ' LW1, LW2', /,                                                  &
              '      LOGICAL TRANSP', /,                                       &
              '      REAL             W1( LW1 ), W2( LW2 )', /,                &
              'C', /, 'C  Problem name : ', A10, /,                            &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
              'C  TRANSP = .FALSE. <=> W2 = U * W1', /,                        &
              'C  TRANSP = .TRUE.  <=> W2 = U(transpose) * W1', /,             &
              'C', /,                                                          &
              '      INTEGER I' )
 4002 FORMAT( '      SUBROUTINE RANGE( IELEMN, TRANSP, W1,',                   &
              ' W2, nelvar, ninvar,', /,                                       &
              '     *                  itype, LW1, LW2 )', /,                  &
              '      INTEGER IELEMN, nelvar, ninvar, itype,',                  &
              ' LW1, LW2', /,                                                  &
              '      LOGICAL TRANSP', /,                                       &
              '      DOUBLE PRECISION W1( LW1 ), W2( LW2 )', /,                &
              'C', /, 'C  Problem name : ', A10, /,                            &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
              'C  TRANSP = .FALSE. <=> W2 = U * W1', /,                        &
              'C  TRANSP = .TRUE.  <=> W2 = U(transpose) * W1', /,             &
              'C' )
 4003 FORMAT( '      SUBROUTINE RANGE( IELEMN, TRANSP, W1,',                   &
              ' W2, nelvar, ninvar,', /,                                       &
              '     *                  itype, LW1, LW2 )', /,                  &
              '      INTEGER IELEMN, nelvar, ninvar, itype,',                  &
              ' LW1, LW2', /,                                                  &
              '      LOGICAL TRANSP', /,                                       &
              '      REAL             W1( LW1 ), W2( LW2 )', /,                &
              'C', /, 'C  Problem name : ', A10, /,                            &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
              'C  TRANSP = .FALSE. <=> W2 = U * W1', /,                        &
              'C  TRANSP = .TRUE.  <=> W2 = U(transpose) * W1', /,             &
              'C' )
 4040 FORMAT( '      GO TO (', 8( i5, :, ',' ), /,                             &
            ( '     *       ', 8( i5, :, ',' ) ) )
 4050 FORMAT( '     *        ', 48X, '), ITYPE' )
 4060 FORMAT( 'C', /, 'C  Element type : ', A10, /, 'C', /,                    &
              i5, ' CONTINUE', /,                                              &
              '      IF ( TRANSP ) THEN' )
 4070 FORMAT( 'C', /,                                                          &
              'C  Elements without internal variables.', /,                    &
              'C', /,                                                          &
              '99998 CONTINUE', /,                                             &
              '      DO 99999 i = 1, nelvar', /,                               &
              '         W2( i ) = W1( i )', /,                                 &
              '99999 CONTINUE', /,                                             &
              '      RETURN' )
 4080 FORMAT( '      RETURN' )
 4090 FORMAT( '      END' )

!  end of subroutine MAKE_elfun

      END SUBROUTINE MAKE_elfun

!-*-  S I F D E C O D E   M A K E _ e l f u n _ a d    S U B R O U T I N E  -*-

      SUBROUTINE MAKE_elfun_ad( input, out, outff, outfd, outra, outem,        &
                                status, nevnames, nivnames, nepnames,          &
                                neltype, pname, EVNAMES, IVNAMES,              &
                                len_renames, RENAMES, len_innames, INNAMES,    &
                                len_lonames, LONAMES, len_minames, MINAMES,    &
                                len_exnames, EXNAMES, DEFINED,                 &
                                length, TABLE, KEY, INLIST,                    &
                                ETYPES, ELV, INV, EPNAMES, ELP, debug, single, &
                                nuline, gotlin, iauto, iad0, print_level )
      INTEGER :: input, out, outff, outra, outfd, outem, status
      INTEGER :: nevnames, nivnames, nepnames, neltype
      INTEGER :: length, print_level, iauto, iad0
      INTEGER :: len_renames, len_innames, len_lonames, len_minames, len_exnames
      LOGICAL :: debug, single, gotlin
      CHARACTER ( LEN = max_record_length ) :: nuline
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TABLE, INLIST
      INTEGER, DIMENSION( neltype + 1 ) :: ELV, INV, ELP
      LOGICAL, DIMENSION( neltype ) :: DEFINED
      CHARACTER ( LEN = 10 ) :: pname
      CHARACTER ( LEN = 10 ) :: EVNAMES( nevnames ), IVNAMES( nivnames )
      CHARACTER ( LEN = 10 ) :: EPNAMES( nepnames ), ETYPES( neltype  )
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: RENAMES, INNAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: LONAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: MINAMES, EXNAMES
      CHARACTER ( LEN = 12 ), ALLOCATABLE, DIMENSION( : ) :: KEY

!  --------------------------------------------------------------------
!  make a function evaluation subroutine, suitable for automatic
!  differentiation, and a range transformation subroutine from a
!  gps function data file

!  function indicator cards
!  -------------------------

!  definition   purpose
!  ----------   --------
!  ELEMENTS     problem name
!  TEMPORARIES  names of additional parameters used in function defs
!  GLOBALS      general parameter assignments
!  INDIVIDUALS  define the transformation from the elemental to the
!               internal variables for all elements with internal
!               vars, set function and derivative values and make
!               element specific parameter assignments
!  ENDATA       end of input data

!  data card description
!  ----------------------

!  see 'The SIF reference report', Chapter 7 in
!       A. R. Conn, N. I. M. Gould and Ph. L. Toint,
!       LANCELOT A Fortran Package for Large-Scale Nonlinear Optimization
!       (RElease A), Springer Series in Computational Mathematics 17,
!       Springer Verlag 1992

!  see also http://www.cuter.rl.ac.uk/sifdec/Doc/sif.pdf
!  and      http://www.numerical.rl.ac.uk/lancelot/sif/sifhtml.html

!  returns with negative values of status indicate that insufficient
!  array space has been allowed, as follows:

!    status = - 1  when length not large enough
!    status = - 2  when RENAMES, INNAMES, LONAMES, MINAMES or EXNAMES cannot be
!                  extended further
!  -------------------------------------------------------------------

!  parameter definitions

      INTEGER, PARAMETER :: mblank = 1, mfixed = 2, mfree = 3, mname = 4
      INTEGER, PARAMETER :: mtemp = 5, mglob = 6, mindiv = 7, mendat = 8
      INTEGER, PARAMETER :: iires = 33
      INTEGER, PARAMETER :: maxnul = 20
      INTEGER, DIMENSION( mendat ), PARAMETER :: LENIND                        &
        = (/ 0, 12, 11, 8, 11, 7, 11, 6 /)
      CHARACTER ( LEN = 12 ), DIMENSION( mendat ), PARAMETER :: INDIC8         &
        = (/ '            ', 'FIXED FORMAT', 'FREE FORMAT ', 'ELEMENTS    ',   &
             'TEMPORARIES ', 'GLOBALS     ', 'INDIVIDUALS ', 'ENDATA      '  /)
      CHARACTER ( LEN = 8 ), DIMENSION( iires ), PARAMETER :: FIELDI           &
        = (/ 'ELFUNF  ', 'LFUVAL  ', 'FUVALS  ', 'XVALUE  ', 'NCALCF  ',       &
             'ITYPEE  ', 'ISTAEV  ', 'IELVAR  ', 'INTVAR  ', 'ISTADH  ',       &
             'ICALCF  ', 'IFFLAG  ', 'IELEMN  ', 'IELTYP  ', 'IHSTRT  ',       &
             'ILSTRT  ', 'IGSTRT  ', 'EPVALU  ', 'ISTEPA  ', 'IPSTRT  ',       &
             'JCALCF  ', 'LTYPEE  ', 'LSTAEV  ', 'LELVAR  ', 'LNTVAR  ',       &
             'LSTADH  ', 'LSTEPA  ', 'LCALCF  ', 'LFVALU  ', 'LXVALU  ',       &
             'LEPVLU  ', 'IFSTAT  ', 'ELFUN   ' /)

!  local variables

      INTEGER :: niname, ninnames, ninvar, nloop, nrenames, nexnames, nlonames
      INTEGER :: i, i1, i2, i3, i4, i5, i6
      INTEGER :: ifield, ifree, ihvar, ivar, intype, nminames, k, k1, k2, isetty
      INTEGER :: itype, j, is, js, novals, nelv, ninv, nn, nename, nlines
      INTEGER :: npname, lineno, ilines, maxnel, maxnin, ntem, nrenm1, nrenm2
      INTEGER :: used_length, new_length, min_length, alloc_status
      LOGICAL :: nointe, defnam, endpar, endgen, firstl, setran
      LOGICAL :: startf, startp, startv, qprod
      LOGICAL :: endoff, fixed, cqsqrt, cqprdt, loutff
      CHARACTER ( LEN = 4 ) :: ad0
      CHARACTER ( LEN = 2 ) :: field1
      CHARACTER ( LEN = 6 ) :: xvar, yvar
      CHARACTER ( LEN = 8 ) :: field3
      CHARACTER ( LEN = 10 ) :: field2, ctemp
      CHARACTER ( LEN = 12 ) :: field, header
      CHARACTER ( LEN = 15 ) :: aorb
      CHARACTER ( LEN = 24 ) :: bad_alloc
      CHARACTER ( LEN = 41 ) :: field7
      CHARACTER ( LEN = 72 ) :: ctem
      CHARACTER ( LEN = max_record_length ) :: blnkln
      REAL ( KIND = wp ), DIMENSION( 2 ) :: VALUES
      CHARACTER ( LEN = 8 ), DIMENSION( 2 ) :: FIELDS
      INTEGER :: IJUMP( neltype  )
      CHARACTER ( LEN = 65 ), DIMENSION( maxnul ) :: NULINA
      REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: U

      IF ( out > 0 ) WRITE( out, 2900 )

!  set initial values for integer variables

      ninnames = 0 ; nrenames = 0 ; nlonames = 0 ; nminames = 0 ; nexnames = 0
      ntem = 0
      lineno = 0 ; nloop = neltype + 1 ; intype = 1 ; ilines = 0 ; nlines = 0

!  set initial values for logical variables

      defnam = .FALSE. ; endpar = .FALSE. ; startp = .FALSE. ; fixed = .TRUE.
      endgen = .FALSE. ; firstl = .TRUE. ;  nointe = .FALSE. ; gotlin = .FALSE.

      loutff = outff > 0

!  assign unique names to variables from quadratic terms

      i1 = 0 ; i2 = 1 ; i3 = 1 ; i4 = 1 ; i5 = 1 ; i6 = 1
      cqsqrt = .FALSE. ; cqprdt = .FALSE.
      DO itype = 1, MIN( 2, neltype )
        IF ( ETYPES( itype ) == cqsqr ) cqsqrt = .TRUE.
        IF ( ETYPES( itype ) == cqprod ) cqprdt = .TRUE.
      END DO
      IF ( cqprdt ) THEN
        yvar = new_name( i1, i2, i3, i4, i5, i6, iires,                        &
                         nrenames, ninnames, nlonames,                         &
                         nminames, nexnames, neltype, '      ',                &
                         FIELDI, RENAMES, INNAMES, LONAMES,                    &
                         MINAMES, EXNAMES, ETYPES )
      ELSE
        yvar = '      '
      END IF
      IF ( cqsqrt .OR.  cqprdt ) THEN
        xvar = new_name( i1, i2, i3, i4, i5, i6, iires,                        &
                         nrenames, ninnames, nlonames,                         &
                         nminames, nexnames, neltype, yvar,                    &
                         FIELDI, RENAMES, INNAMES, LONAMES,                    &
                         MINAMES, EXNAMES, ETYPES )
      ELSE
        xvar = '      '
      END IF

!  allocate space to hold the largest range transformation matrix possible

      nn = 0
      DO itype = 1, neltype
        nelv = ELV( itype + 1 ) - ELV( itype )
        ninv = INV( itype + 1 ) - INV( itype )
        nn = MAX( nn, ninv * nelv )
      END DO
      CALL ALLOCATE_array( U, nn, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'U' ; GO TO 980 ; END IF

!  create a dictionary of the internal variable names used

      niname = INV( neltype + 1 ) - 1
      DO i = 1, niname
        field = IVNAMES( i ) // 'PF'
        CALL HASH_enlarge_and_insert( length, 12, field,                       &
                                      TABLE, KEY, INLIST, ifree )
        IF ( ifree <= 0 ) THEN
          IF ( ifree == 0 ) THEN
            status = - 1
            GO TO 700
          END IF
        ELSE
          nrenames = nrenames + 1
          IF ( nrenames > len_renames ) THEN
            used_length = nrenames - 1 ; min_length = nrenames
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( RENAMES, len_renames, used_length,              &
                               new_length, min_length, buffer,                 &
                               status, alloc_status, 'RENAMES' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'RENAMES' ; status = - 2 ; GO TO 980
            END IF
            len_renames = new_length
          END IF
          RENAMES( nrenames ) = IVNAMES( i )
        END IF
      END DO

!  include the names of the elemental variables used in this dictionary

      nename = ELV( neltype + 1 ) - 1
      DO i = 1, nename
        field = EVNAMES( i ) // 'PF'
        CALL HASH_enlarge_and_insert( length, 12, field,                       &
                                      TABLE, KEY, INLIST, ifree )
        IF ( ifree <= 0 ) THEN
          IF ( ifree == 0 ) THEN
            status = - 1
            GO TO 700
          END IF
        ELSE
          nrenames = nrenames + 1
          IF ( nrenames > len_renames ) THEN
            used_length = nrenames - 1 ; min_length = nrenames
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( RENAMES, len_renames, used_length,              &
                               new_length, min_length, buffer,                 &
                               status, alloc_status, 'RENAMES' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'RENAMES' ; status = - 2 ; GO TO 980
            END IF
            len_renames = new_length
          END IF
          RENAMES( nrenames ) = EVNAMES( i )
        END IF
      END DO
!     netnam = nrenames

!  include the names of the elemental parameters used
!  in this dictionary

      npname = ELP( neltype + 1 ) - 1
      DO i = 1, npname
        field = EPNAMES( i ) // 'PF'
        CALL HASH_enlarge_and_insert( length, 12, field,                       &
                                      TABLE, KEY, INLIST, ifree )
        IF ( ifree <= 0 ) THEN
          IF ( ifree == 0 ) THEN
            status = - 1
            GO TO 700
          END IF
        ELSE
          nrenames = nrenames + 1
          IF ( nrenames > len_renames ) THEN
            used_length = nrenames - 1 ; min_length = nrenames
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( RENAMES, len_renames, used_length,              &
                               new_length, min_length, buffer,                 &
                               status, alloc_status, 'RENAMES' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'RENAMES' ; status = - 2 ; GO TO 980
            END IF
            len_renames = new_length
          END IF
          RENAMES( nrenames ) = EPNAMES( i )
        END IF
      END DO

!  find which element types have an internal representation

      maxnin = 1 ; maxnel = 1 ;isetty = 0
      DO itype = 1, neltype
        DEFINED( itype ) = .FALSE.
        IF ( ELV( itype + 1 ) - ELV( itype ) ==                                &
             INV( itype + 1 ) - INV( itype ) ) THEN
          IJUMP( itype ) = 99998
          nointe = .TRUE.
        ELSE
          IJUMP( itype ) = itype
        END IF
        maxnin = MAX( maxnin, INV( itype + 1 ) - INV( itype ) )
        maxnel = MAX( maxnel, ELV( itype + 1 ) - ELV( itype ) )
      END DO

!  set a blank line

      DO i = 1, max_record_length
        BLNKLN( i : i ) = ' '
      END DO

!  read next line

  100 CONTINUE
      IF ( ilines + 1 > nlines ) THEN

!  read next line from the input file

        lineno = lineno + 1
        nuline = blnkln
        IF ( fixed ) THEN
          READ ( input, 1000, END = 590, ERR = 590 ) nuline
          IF ( out > 0 .AND. debug ) WRITE( out, 2990 ) lineno, nuline
        ELSE
          READ ( input, 1010, END = 590, ERR = 590 ) nuline
          IF ( out > 0 .AND. debug ) WRITE( out, 2970 ) lineno, nuline

!  if the card is in free format, translate it into fixed format

          CALL FREE_format( nuline, max_record_length, mendat, INDIC8,         &
                            LENIND, NULINA, maxnul, nlines, .FALSE.,           &
                            status, out )
          IF ( status > 0 ) GO TO 800

!  if there are non-blank lines on the free format card, read the first

          IF ( nlines > 0 ) THEN
            ilines = 1
            nuline = blnkln
            nuline = NULINA( ilines )
            IF ( out > 0 .AND. debug ) WRITE( out, 2980 ) lineno, ilines, nuline

!  there are only blank lines on the free format card

          ELSE
            GO TO 100
          END IF
        END IF

!  read next line from the last encountered free format card

      ELSE
        ilines = ilines + 1
        nuline = blnkln
        nuline = NULINA( ilines )
        IF ( out > 0 .AND. debug ) WRITE( out, 2980 ) lineno, ilines, nuline
      END IF

!  consider the header part of the card

      header = NULINE( 1 : 12 )

!  ignore blank lines

      IF ( header == INDIC8( mblank ) ) GO TO 100
      IF ( NULINE( 1 : 1 ) /= ' ' ) THEN

!  ignore comment cards

        IF ( NULINE( 1 : 1 ) == '*' ) GO TO 100

!  check if we have entered fixed-format input

        IF ( header == INDIC8( mfixed ) ) THEN
          fixed = .TRUE.
          GO TO 100
        END IF

!  check if we have entered free-format input

        IF ( header == INDIC8( mfree ) ) THEN
          fixed = .FALSE.
          GO TO 100
        END IF

!  check that the first encountered indicator card is the elements card

        IF ( .NOT. defnam  ) THEN
          IF ( header /= INDIC8( mname ) ) THEN
!           IF ( neltype > 0 ) GO TO 930
            IF ( neltype > 0 ) THEN
              BACKSPACE( input, ERR = 590 )
              GO TO 590
            END IF
            IF ( out > 0 .AND. print_level /= 0 ) WRITE( out, 2010)
            gotlin = .TRUE.
            GO TO 600

!  indicator card is elements
!  ---------------------------

          ELSE
            IF ( pname  /= NULINE( 15 : 24 ) ) THEN
              status = 51
              IF ( out > 0 ) WRITE( out, 2510 )
              GO TO 800
            ELSE
              defnam = .TRUE.

!  -------- set up subroutine call for range routine

              IF ( single ) THEN
                 WRITE( outra, 4001 ) pname, TRIM( version )
              ELSE
                 WRITE( outra, 4000 ) pname, TRIM( version )
              END IF
              IF ( neltype > 1 ) THEN
                 WRITE( outra, 4040 ) ( IJUMP( i ), i = 1, neltype )
                 WRITE( outra, 4050 )
              END IF
              GO TO 100
            END IF
          END IF
        END IF

!  an indicator card has been found

        DO i = intype, mendat
          IF ( header == INDIC8( i ) ) THEN
            intype = i
            GO TO 120
          END IF
        END DO

!  the indicator card is not recognised

        status = 2
        IF ( out > 0 ) WRITE( out, 2020 )
        GO TO 800
  120   CONTINUE

!  the parameter values have been completed. write out the
!  first part of the generated subroutine

        IF ( intype >= mglob .AND. .NOT. endpar ) THEN
          endpar = .TRUE.

!  insert the list of reserved integer/real/logical variables into
!  the dictionary

          DO i = 1, iires
            field = FIELDI( i ) // '  PF'
            CALL HASH_enlarge_and_insert( length, 12, field,                   &
                                          TABLE, KEY, INLIST, ifree )
            IF ( ifree <= 0 ) THEN
              IF ( ifree == 0 ) THEN
                status = - 1
                GO TO 700
              END IF
              status = 59
              IF ( out > 0 ) WRITE( out, 2590 ) FIELDI( i )
              GO TO 800
            END IF
          END DO

!  -------- set up subroutine call and reserved parameter declarations

          IF ( iauto == 1 ) THEN
            IF ( single ) THEN
              aorb = 'FORWARD_SINGLE '
            ELSE
              aorb = 'FORWARD_DOUBLE '
            END IF
          ELSE
            IF ( single ) THEN
              aorb = 'BACKWARD_SINGLE'
            ELSE
              aorb = 'BACKWARD_DOUBLE'
            END IF
          END IF
          IF ( iad0 == 1 ) THEN
            ad0 = 'AD01'
          ELSE
            ad0 = 'AD02'
          END IF
          IF ( single ) THEN
            IF ( loutff ) WRITE( outff, 3001 ) FIELDI( 1 )( 1 : 6 ),           &
          FIELDI(  3 )( 1 : 6 ), FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ), &
         ( FIELDI(  i )( 1 : 6 ), i = 5, 10 ), FIELDI( 19 )( 1 : 6 ),          &
           FIELDI( 11 )( 1 : 6 ), ( FIELDI(  i )( 1 : 6 ), i = 22, 31 ),       &
           FIELDI( 12 )( 1 : 6 ), FIELDI( 32 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),&
           FIELDI( 12 )( 1 : 6 ), ( FIELDI(  i )( 1 : 6 ), i = 22, 32 ),       &
           FIELDI(  6 )( 1 : 6 ), FIELDI( 22 )( 1 : 6 ),                       &
           FIELDI(  7 )( 1 : 6 ), FIELDI( 23 )( 1 : 6 ),                       &
           FIELDI(  8 )( 1 : 6 ), FIELDI( 24 )( 1 : 6 ),                       &
           FIELDI(  9 )( 1 : 6 ), FIELDI( 25 )( 1 : 6 ),                       &
           FIELDI( 10 )( 1 : 6 ), FIELDI( 26 )( 1 : 6 ),                       &
           FIELDI( 19 )( 1 : 6 ), FIELDI( 27 )( 1 : 6 ),                       &
           FIELDI( 11 )( 1 : 6 ), FIELDI( 28 )( 1 : 6 ),                       &
           FIELDI(  3 )( 1 : 6 ), FIELDI( 29 )( 1 : 6 ),                       &
           FIELDI(  4 )( 1 : 6 ), FIELDI( 30 )( 1 : 6 ),                       &
           FIELDI( 18 )( 1 : 6 ), FIELDI( 31 )( 1 : 6 ),                       &
           pname, TRIM( version ),                                             &
           FIELDI( 13 )( 1 : 6 ), FIELDI( 14 )( 1 : 6 ),                       &
           FIELDI( 15 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),                       &
           FIELDI( 17 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ), FIELDI( 21 )( 1 : 6 )
            WRITE( outfd, 3005 ) FIELDI( 33 )( 1 : 6 ),                        &
          FIELDI(  3 )( 1 : 6 ), FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ), &
        ( FIELDI(  i )( 1 : 6 ), i = 5, 10 ), FIELDI( 19 )( 1 : 6 ),           &
          FIELDI( 11 )( 1 : 6 ), ( FIELDI(  i )( 1 : 6 ), i = 22, 31 ),        &
          FIELDI( 12 )( 1 : 6 ), FIELDI( 32 )( 1 : 6 ), ad0, aorb,             &
          FIELDI(  5 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),                        &
        ( FIELDI(  i )( 1 : 6 ), i = 22, 32 ), FIELDI(  6 )( 1 : 6 ),          &
          FIELDI( 22 )( 1 : 6 ), FIELDI(  7 )( 1 : 6 ), FIELDI( 23 )( 1 : 6 ), &
          FIELDI(  8 )( 1 : 6 ), FIELDI( 24 )( 1 : 6 ),                        &
          FIELDI(  9 )( 1 : 6 ), FIELDI( 25 )( 1 : 6 ),                        &
          FIELDI( 10 )( 1 : 6 ), FIELDI( 26 )( 1 : 6 ),                        &
          FIELDI( 19 )( 1 : 6 ), FIELDI( 27 )( 1 : 6 ),                        &
          FIELDI( 11 )( 1 : 6 ), FIELDI( 28 )( 1 : 6 ),                        &
          FIELDI(  3 )( 1 : 6 ), FIELDI( 29 )( 1 : 6 ),                        &
          FIELDI(  4 )( 1 : 6 ), FIELDI( 30 )( 1 : 6 ),                        &
          FIELDI( 18 )( 1 : 6 ), FIELDI( 31 )( 1 : 6 ),                        &
          pname, TRIM( version ),                                              &
          FIELDI( 13 )( 1 : 6 ), FIELDI( 14 )( 1 : 6 ),                        &
          FIELDI( 15 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),                        &
          FIELDI( 17 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),                        &
          FIELDI( 21 )( 1 : 6 ), maxnin
          ELSE
            IF ( loutff ) WRITE( outff, 3000 ) FIELDI( 1 )( 1 : 6 ),           &
          FIELDI(  3 )( 1 : 6 ), FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ), &
        ( FIELDI(  i )( 1 : 6 ), i = 5, 10 ), FIELDI( 19 )( 1 : 6 ),           &
          FIELDI( 11 )( 1 : 6 ), ( FIELDI(  i )( 1 : 6 ), i = 22, 31 ),        &
          FIELDI( 12 )( 1 : 6 ), FIELDI( 32 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ), &
          FIELDI( 12 )( 1 : 6 ), ( FIELDI(  i )( 1 : 6 ), i = 22, 32 ),        &
          FIELDI(  6 )( 1 : 6 ), FIELDI( 22 )( 1 : 6 ),                        &
          FIELDI(  7 )( 1 : 6 ), FIELDI( 23 )( 1 : 6 ),                        &
          FIELDI(  8 )( 1 : 6 ), FIELDI( 24 )( 1 : 6 ),                        &
          FIELDI(  9 )( 1 : 6 ), FIELDI( 25 )( 1 : 6 ),                        &
          FIELDI( 10 )( 1 : 6 ), FIELDI( 26 )( 1 : 6 ),                        &
          FIELDI( 19 )( 1 : 6 ), FIELDI( 27 )( 1 : 6 ),                        &
          FIELDI( 11 )( 1 : 6 ), FIELDI( 28 )( 1 : 6 ),                        &
          FIELDI(  3 )( 1 : 6 ), FIELDI( 29 )( 1 : 6 ),                        &
          FIELDI(  4 )( 1 : 6 ), FIELDI( 30 )( 1 : 6 ),                        &
          FIELDI( 18 )( 1 : 6 ), FIELDI( 31 )( 1 : 6 ),                        &
          pname, TRIM( version ),                                              &
          FIELDI( 13 )( 1 : 6 ), FIELDI( 14 )( 1 : 6 ),                        &
          FIELDI( 15 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),                        &
          FIELDI( 17 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ), FIELDI( 21 )( 1 : 6 )
            WRITE( outfd, 3004 ) FIELDI( 33 )( 1 : 6 ),                        &
          FIELDI(  3 )( 1 : 6 ), FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ), &
        ( FIELDI(  i )( 1 : 6 ), i = 5, 10 ), FIELDI( 19 )( 1 : 6 ),           &
          FIELDI( 11 )( 1 : 6 ), ( FIELDI(  i )( 1 : 6 ), i = 22, 31 ),        &
          FIELDI( 12 )( 1 : 6 ), FIELDI( 32 )( 1 : 6 ), ad0, aorb,             &
          FIELDI(  5 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),                        &
        ( FIELDI(  i )( 1 : 6 ), i = 22, 32 ),                                 &
          FIELDI(  6 )( 1 : 6 ), FIELDI( 22 )( 1 : 6 ), FIELDI(  7 )( 1 : 6 ), &
          FIELDI( 23 )( 1 : 6 ), FIELDI(  8 )( 1 : 6 ), FIELDI( 24 )( 1 : 6 ), &
          FIELDI(  9 )( 1 : 6 ), FIELDI( 25 )( 1 : 6 ),                        &
          FIELDI( 10 )( 1 : 6 ), FIELDI( 26 )( 1 : 6 ),                        &
          FIELDI( 19 )( 1 : 6 ), FIELDI( 27 )( 1 : 6 ),                        &
          FIELDI( 11 )( 1 : 6 ), FIELDI( 28 )( 1 : 6 ),                        &
          FIELDI(  3 )( 1 : 6 ), FIELDI( 29 )( 1 : 6 ),                        &
          FIELDI(  4 )( 1 : 6 ), FIELDI( 30 )( 1 : 6 ),                        &
          FIELDI( 18 )( 1 : 6 ), FIELDI( 31 )( 1 : 6 ),                        &
          pname, TRIM( version ),                                              &
          FIELDI( 13 )( 1 : 6 ), FIELDI( 14 )( 1 : 6 ),                        &
          FIELDI( 15 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),                        &
          FIELDI( 17 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),                        &
          FIELDI( 21 )( 1 : 6 ), maxnin
          END IF
          IF ( iad0 == 1 ) THEN
            WRITE( outfd, 3024 ) maxnel, maxnin
          ELSE
            WRITE( outfd, 3025 ) maxnel, maxnin
          END IF

! --------- insert integer declarations

          IF ( ninnames > 0 .AND. loutff )                                     &
            WRITE( outff, 3010 ) ( INNAMES( i ), i = 1, ninnames )
          IF ( ninnames > 0 )                                                  &
            WRITE( outfd, 3010 ) ( INNAMES( i ), i = 1, ninnames )

!  order the real values so that the list of variables which belong
!  to intrinsic or external functions follow those which do not

          IF ( nrenames > 0 ) THEN
            nrenm1 = 0
            nrenm2 = nrenames + 1
  140       CONTINUE
            IF ( nrenm1 + 1 == nrenm2 ) GO TO 180
            DO i = 1, nminames
               IF ( RENAMES( nrenm1 + 1 ) == MINAMES( i ) ) GO TO 170
            END DO
            DO i = 1, nexnames
               IF ( RENAMES( nrenm1 + 1 ) == EXNAMES( i ) ) GO TO 170
            END DO
            nrenm1 = nrenm1 + 1
            GO TO 140
  170       CONTINUE
            nrenm2 = nrenm2 - 1
            ctemp = RENAMES( nrenm2 )
            RENAMES( nrenm2 ) = RENAMES( nrenm1 + 1 )
            RENAMES( nrenm1 + 1 ) = ctemp
            GO TO 140
  180       CONTINUE

! --------- insert real declarations

            IF ( loutff ) THEN
              IF ( single ) THEN
                WRITE( outff, 3019 ) ( RENAMES( i ), i = 1, nrenames )
              ELSE
                WRITE( outff, 3020 ) ( RENAMES( i ), i = 1, nrenames )
              END IF
            END IF
            IF ( iad0 == 1 ) THEN
              IF ( nrenm1 > 0 ) WRITE( outfd, 3018 )                           &
                   ( RENAMES( i ), i = 1, nrenm1 )
            ELSE
              IF ( nrenm1 > 0 ) WRITE( outfd, 3017 )                           &
                   ( ad0, RENAMES( i ), i = 1, nrenm1 )
            END IF
            IF ( nrenm2 <= nrenames ) WRITE( outfd, 3017 )                     &
                 ( ad0, RENAMES( i ), i = nrenm2, nrenames )
          END IF

! --------- insert logical declarations

          IF ( nlonames > 0 .AND. loutff )                                     &
            WRITE( outff, 3023 ) ( LONAMES( i ), i = 1, nlonames )
          IF ( nlonames > 0 )                                                  &
            WRITE( outfd, 3023 ) ( LONAMES( i ), i = 1, nlonames )

! --------- insert intrinsic declarations

          IF ( nminames > 0 .AND. loutff )                                     &
            WRITE( outff, 3021 ) ( MINAMES( i ), i = 1, nminames )

! --------- insert external declarations

          IF ( nexnames > 0 .AND. loutff )                                     &
            WRITE( outff, 3022 ) ( EXNAMES( i ), i = 1, nexnames )
          IF ( nexnames > 0 )                                                  &
            WRITE( outfd, 3022 ) ( EXNAMES( i ), i = 1, nexnames )

! --------- insert variables for quadratic terms (if any)

          IF ( xvar /= '      ' ) THEN
            IF ( yvar /= '      ' ) THEN
              IF ( single ) THEN
                IF ( loutff ) WRITE( outff, 3019 ) xvar, yvar
                WRITE( outfd, 3019 ) xvar, yvar
              ELSE
                IF ( loutff ) WRITE( outff, 3020 ) xvar, yvar
                WRITE( outfd, 3020 ) xvar, yvar
              END IF
            ELSE
              IF ( single ) THEN
                IF ( loutff ) WRITE( outff, 3019 ) xvar
                WRITE( outfd, 3019 ) xvar
              ELSE
                IF ( loutff ) WRITE( outff, 3020 ) xvar
                WRITE( outfd, 3020 ) xvar
              END IF
            END IF
          END IF
          IF ( loutff ) WRITE( outff, 3009 ) FIELDI( 32 )( 1 : 6 )
          WRITE( outfd, 3009 ) FIELDI( 32 )( 1 : 6 )
        END IF

!  the general parameter assignments have been completed
!  continue with the construction of the generated subroutine

        IF ( intype >= mindiv .AND. .NOT. endgen ) THEN
          endgen = .TRUE.

! --------- start loop over elements

          IF ( loutff ) WRITE( outff, 3050 ) nloop,                            &
                   FIELDI( 21 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),               &
                   FIELDI( 13 )( 1 : 6 ), FIELDI( 11 )( 1 : 6 ),               &
                   FIELDI( 21 )( 1 : 6 ),                                      &
                   FIELDI( 16 )( 1 : 6 ), FIELDI(  7 )( 1 : 6 ),               &
                   FIELDI( 13 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),               &
                   FIELDI(  9 )( 1 : 6 ), FIELDI( 13 )( 1 : 6 ),               &
                   FIELDI( 20 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),               &
                   FIELDI( 13 )( 1 : 6 ),                                      &
                   FIELDI( 12 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),               &
                   FIELDI( 10 )( 1 : 6 ), FIELDI( 13 )( 1 : 6 )
          IF ( iad0 == 1 ) THEN
            WRITE( outfd, 3008 )
          ELSE
            WRITE( outfd, 3011 )
            DO i = 1, nrenm1
               WRITE( outfd, 3016 ) ad0, RENAMES( i )
            END DO
          END IF
          WRITE( outfd, 3050 ) nloop,                                          &
                   FIELDI( 21 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),               &
                   FIELDI( 13 )( 1 : 6 ), FIELDI( 11 )( 1 : 6 ),               &
                   FIELDI( 21 )( 1 : 6 ),                                      &
                   FIELDI( 16 )( 1 : 6 ), FIELDI(  7 )( 1 : 6 ),               &
                   FIELDI( 13 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),               &
                   FIELDI(  9 )( 1 : 6 ), FIELDI( 13 )( 1 : 6 ),               &
                   FIELDI( 20 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),               &
                   FIELDI( 13 )( 1 : 6 ),                                      &
                   FIELDI( 12 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),               &
                   FIELDI( 10 )( 1 : 6 ), FIELDI( 13 )( 1 : 6 )
          IF ( neltype > 1 ) THEN
            IF ( loutff ) WRITE( outff, 3051 )                                 &
               FIELDI( 14 )( 1 : 6 ), FIELDI(  6 )( 1 : 6 ),                   &
               FIELDI( 13 )( 1 : 6 ), ( i, i = 1, neltype )
            WRITE( outfd, 3051 )                                               &
               FIELDI( 14 )( 1 : 6 ), FIELDI(  6 )( 1 : 6 ),                   &
               FIELDI( 13 )( 1 : 6 ), ( i, i = 1, neltype )
            IF ( loutff ) WRITE( outff, 3052 ) FIELDI( 14 )( 1 : 6 )
            WRITE( outfd, 3052 ) FIELDI( 14 )( 1 : 6 )
          END IF

!  make sure that quadratic Hessian terms are included

          DO itype = 1, MIN( 2, neltype )

!  diagonal term

            IF ( ETYPES( itype ) == cqsqr ) THEN
              IF ( loutff ) WRITE( outff, 3060 ) ETYPES( itype )
              WRITE( outfd, 3060 ) ETYPES( itype )
              IF ( neltype > 1 ) THEN
                 IF ( loutff ) WRITE( outff, 3061 ) itype
                 WRITE( outfd, 3061 ) itype
              END IF
              IF ( single ) THEN
                 IF ( loutff ) WRITE( outff, 3055 ) 'E'
                 WRITE( outfd, 3057 ) xvar, 'E', xvar, xvar, xvar, 'E'
              ELSE
                 IF ( loutff ) WRITE( outff, 3055 ) 'D'
                 WRITE( outfd, 3057 ) xvar, 'D', xvar, xvar, xvar, 'D'
              END IF
              DEFINED( itype ) = .TRUE.
              isetty = isetty + 1
              IF ( isetty < neltype ) THEN
                 IF ( loutff ) WRITE( outff, 3191 ) nloop
                 WRITE( outfd, 3191 ) nloop
              END IF

!  off-diagonal term

            ELSE IF ( ETYPES( itype ) == cqprod ) THEN
              IF ( loutff ) WRITE( outff, 3060 ) ETYPES( itype )
              WRITE( outfd, 3060 ) ETYPES( itype )
              IF ( neltype > 1 ) THEN
                 IF ( loutff ) WRITE( outff, 3061 ) itype
                 WRITE( outfd, 3061 ) itype
              END IF
              IF ( single ) THEN
                IF ( loutff ) WRITE( outff, 3056 )
                WRITE( outfd, 3058 )                                           &
                  xvar, yvar, xvar, yvar, yvar, xvar, 'E', 'E', 'E'
              ELSE
                IF ( loutff ) WRITE( outff, 3056 )
                WRITE( outfd, 3058 )                                           &
                  xvar, yvar, xvar, yvar, yvar, xvar, 'D', 'D', 'D'
              END IF
              DEFINED( itype ) = .TRUE.
              isetty = isetty + 1
              IF ( isetty < neltype ) THEN
                IF ( loutff ) WRITE( outff, 3191 ) nloop
                WRITE( outfd, 3191 ) nloop
              END IF
            END IF
          END DO
        END IF

!  indicator card is endata
!  -------------------------

        IF ( intype == mendat ) GO TO 900
        GO TO 100

!  check that the first non commment card is the elements indicator card

      ELSE
        IF ( .NOT. defnam  ) THEN
!         IF ( neltype > 0 ) GO TO 930
          IF ( neltype > 0 ) THEN
            BACKSPACE( input, ERR = 590 )
            GO TO 590
          END IF
          IF ( out > 0 .AND. print_level /= 0 ) WRITE( out, 2010 )
          gotlin = .TRUE.
          GO TO 600
        END IF

!  a data card has been found
!  read the character fields 1 and 2 from the card

        field1 = NULINE(  2 :  3 )
        field2 = NULINE(  5 : 14 )

!  read the character fields 3 and 5 from the card

        IF ( intype == mindiv .AND. field1 == 'R ' ) THEN
          FIELDS( 1 ) = NULINE( 15 : 22 )
          FIELDS( 2 ) = NULINE( 40 : 47 )

!  check to see if there is are any numerical values to be read

          novals = 0
          IF ( FIELDS( 1 ) /= '        ' .AND. NULINE( 15 : 15 ) /= '[' ) THEN
            novals = 1
            CALL GET_value( NULINE( 25 : 36 ), VALUES( 1 ) )
            IF ( FIELDS( 2 ) /= '        ' .AND. NULINE( 40 : 40 ) /= '[' ) THEN
              novals = 2
              CALL GET_value( NULINE( 50 : 61 ), VALUES( 2 ) )

!  remove fields with numerical values of zero

              IF ( VALUES( 2 ) == zero ) THEN
                novals = 1
              END IF
            END IF
            IF ( VALUES( 1 ) == zero ) THEN
               IF ( novals == 2 ) THEN
                  VALUES( 1 ) = VALUES( 2 )
                  FIELDS( 1 ) = FIELDS( 2 )
               END IF
               novals = novals - 1
            END IF
          END IF

!  read the character fields 3 and 7 from the card

        ELSE
          field3 = NULINE( 15 : 22 )
          field7 = NULINE( 25 : 65 )

!  check that field3 is blank on 'a', 'f' and 'g' cards

          IF ( field1( 1 : 1 ) == 'A' .OR. field1( 1 : 1 ) == 'F' .OR.         &
               field1( 1 : 1 ) == 'G' ) THEN
            IF ( field3 /= '       ' ) THEN
               status = 72
               IF ( out > 0 ) WRITE( out, 2720 )
               GO TO 800
            END IF
          END IF
        END IF
      END IF

!  branch on the value of intype

      GO TO ( 100, 100, 100, 100, 200, 300, 400, 900 ), intype

!  indicator card is temporaries
!  ------------------------------

  200 CONTINUE

!  check to see if the parameter is integer, real, logical or a function

      IF ( field1 /= 'I ' .AND. field1 /= 'R ' .AND.                           &
           field1 /= 'M ' .AND. field1 /= 'F ' .AND.                           &
           field1 /= 'L ' ) THEN
        status = 54
        IF ( out > 0 ) WRITE( out, 2540 )
        GO TO 800
      END IF

!  if the parameter is a function, check to see that the name has
!  not already been used

      IF ( field1 == 'F ' ) THEN
        field = field2 // 'FU'
        CALL HASH_enlarge_and_insert( length, 12, field,                       &
                                      TABLE, KEY, INLIST, ifree )
        IF ( ifree <= 0 ) THEN
          IF ( ifree == 0 ) THEN
            status = - 1
            GO TO 700
          END IF
        ELSE
          nexnames = nexnames + 1
          IF ( nrenames > len_renames ) THEN
            used_length = nexnames - 1 ; min_length = nexnames
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( EXNAMES, len_exnames, used_length,              &
                               new_length, min_length, buffer,                 &
                               status, alloc_status, 'EXNAMES' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'EXNAMES' ; status = - 2 ; GO TO 980 ; END IF
            len_exnames = new_length
          END IF
          EXNAMES( nexnames ) = field2
        END IF

!  check to see that the parameter name has not already been used

      ELSE
        field = field2 // 'PF'
        CALL HASH_enlarge_and_insert( length, 12, field,                       &
                                      TABLE, KEY, INLIST, ifree )
        IF ( ifree <= 0 ) THEN
           IF ( ifree == 0 ) THEN
              status = - 1
              GO TO 700
           END IF
        ELSE
          IF ( field1 == 'R ' ) THEN
            nrenames = nrenames + 1
            IF ( nrenames > len_renames ) THEN
              used_length = nrenames - 1 ; min_length = nrenames
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( RENAMES, len_renames, used_length,            &
                                 new_length, min_length, buffer,               &
                                 status, alloc_status, 'RENAMES' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'RENAMES' ; status = - 2 ; GO TO 980 ; END IF
              len_renames = new_length
            END IF
            RENAMES( nrenames ) = field2
          ELSE IF ( field1 == 'M ' ) THEN
            nminames = nminames + 1
            IF ( nminames > len_minames ) THEN
              used_length = nminames - 1 ; min_length = nminames
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( MINAMES, len_minames, used_length,            &
                                 new_length, min_length, buffer,               &
                                 status, alloc_status, 'MINAMES' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'MINAMES' ; status = - 2 ; GO TO 980 ; END IF
              len_minames = new_length
            END IF
            MINAMES( nminames ) = field2
          ELSE IF ( field1 == 'L ' ) THEN
            nlonames = nlonames + 1
            IF ( nlonames > len_lonames ) THEN
              used_length = nlonames - 1 ; min_length = nlonames
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( LONAMES, len_lonames, used_length,            &
                                 new_length, min_length, buffer,               &
                                 status, alloc_status, 'LONAMES' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'LONAMES' ; status = - 2 ; GO TO 980 ; END IF
              len_lonames = new_length
            END IF
            LONAMES( nlonames ) = field2
          ELSE
            ninnames = ninnames + 1
            IF ( ninnames > len_innames ) THEN
              used_length = ninnames - 1 ; min_length = ninnames
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( INNAMES, len_innames, used_length,            &
                                 new_length, min_length, buffer,               &
                                 status, alloc_status, 'INNAMES' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'INNAMES' ; status = - 2 ; GO TO 980 ; END IF
              len_innames = new_length
            END IF
            INNAMES( ninnames ) = field2
          END IF
        END IF
      END IF
      GO TO 100

!  indicator card is globals
!  --------------------------

  300 CONTINUE

!  start a parameter assignment. check to see that the parameter has been
!  defined

      IF ( field1 == 'A ' .OR. field1 == 'I ' .OR. field1 == 'E ' ) THEN
        startp = .TRUE.
        field = field2 // 'PF'
        CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
        IF ( ifield <= 0 ) THEN
          IF ( ifree == 0 ) THEN
            status = - 1
            GO TO 700
          END IF
          status = 57
          IF ( out > 0 ) WRITE( out, 2570 )
          GO TO 800
        END IF

! --------- make general parameter assignments

        IF ( field1 == 'A ' ) THEN
          IF ( loutff ) WRITE( outff, 3030 ) FIELD2( 1 : 6 ), field7
          ntem = ntem + 1
          WRITE( outem, 3080 ) FIELD2( 1 : 6 ), field7

! --------- make conditional parameter assignments

        ELSE

!  check that the logical variable has been defined

          field = field3 // '  PF'
          CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
          IF ( ifield <= 0 ) THEN
            IF ( ifree == 0 ) THEN
              status = - 1
              GO TO 700
            END IF
            status = 57
            IF ( out > 0 ) WRITE( out, 2570 )
            GO TO 800
          END IF
          IF ( field1 == 'I ' ) THEN
            IF ( loutff ) WRITE( outff, 3031 ) FIELD2( 1 : 6 ),                &
                                               FIELD3( 1 : 6 ), field7
            ntem = ntem + 1
            WRITE( outem, 3081 ) FIELD2( 1 : 6 ),                              &
                                 FIELD3( 1 : 6 ), field7
          ELSE
            IF ( loutff ) WRITE( outff, 3032 ) FIELD2( 1 : 6 ),                &
                                               FIELD3( 1 : 6 ), field7
            ntem = ntem + 1
            WRITE( outem, 3082 ) FIELD2( 1 : 6 ),                              &
                                 FIELD3( 1 : 6 ), field7
          END IF
        END IF

! --------- continue a parameter assignment

      ELSE
        IF ( field1( 2 : 2 ) == '+' .AND. startp ) THEN
          IF ( loutff ) WRITE( outff, 3040 ) field7
          ntem = ntem + 1
          WRITE( outem, 3040 ) field7
        ELSE
          status = 55
          IF ( out > 0 ) WRITE( out, 2550 )
          GO TO 800
        END IF
      END IF
      GO TO 100

!  indicator card is individuals
!  ------------------------------

  400 CONTINUE

!  check if a new element has been encountered

      IF ( field1 == 'T ' ) THEN

!  check to see if the range of a new element is to be defined

        IF ( firstl ) THEN

!  check if this is the first element

          firstl = .FALSE.

!  finish of the previous element, if any

        ELSE
          IF ( startf ) THEN
            IF ( .NOT. endoff ) THEN
              IF ( loutff ) THEN
                WRITE( outff, 3120 )
                WRITE( outff, 3121 )
              END IF
              IF ( iad0 == 1 ) THEN
                WRITE( outfd, 3122 )                                           &
                     FIELDI( 12 )( 1 : 6 ), FIELDI( 3  )( 1 : 6 ),             &
                     FIELDI( 13 )( 1 : 6 ), FIELDI( 3  )( 1 : 6 ),             &
                     FIELDI( 17 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),             &
                     ninvar
              ELSE
                WRITE( outfd, 3123 )                                           &
                     FIELDI( 12 )( 1 : 6 ), FIELDI( 3  )( 1 : 6 ),             &
                     FIELDI( 13 )( 1 : 6 ), FIELDI( 3  )( 1 : 6 ),             &
                     FIELDI( 17 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),             &
                     ninvar
              END IF
              WRITE( outfd, 3150 ) FIELDI( 12 )( 1 : 6 )
              IF ( iad0 == 1 ) THEN
                WRITE( outfd, 3151 )
              ELSE
                WRITE( outfd, 3152 )
              END IF
              DO js = 1, ninvar
                DO is = 1, js
                  ihvar = ( js * ( js - 1 ) ) / 2 + is
                  IF ( is == js ) THEN
                    WRITE( outfd, 3163 ) FIELDI(  3 )( 1 : 6 ),                &
                          FIELDI( 15 )( 1 : 6 ), ihvar, ihvar
                  ELSE
                    WRITE( outfd, 3164 ) FIELDI(  3 )( 1 : 6 ),                &
                          FIELDI( 15 )( 1 : 6 ), ihvar, ihvar
                  END IF
                END DO
              END DO
              endoff = .TRUE.
            END IF
            IF ( loutff ) WRITE( outff, 3190 )
            WRITE( outfd, 3180 )
            WRITE( outfd, 3190 )
          ELSE
            status = 61
            IF ( out > 0 ) WRITE( out, 2610 )
            GO TO 800
          END IF
          IF ( isetty < neltype .AND. loutff ) WRITE( outff, 3191 ) nloop
          IF ( isetty < neltype ) WRITE( outfd, 3191 ) nloop
        END IF

!  find itype, the element type

        field = field2 // 'ET'
        CALL HASH_search( length, 12, field, TABLE, KEY, ifield )

!  the element type is unknown

        IF ( ifield <= 0 ) THEN
          status = 9
          IF ( out > 0 ) WRITE( out, 2090 )
          GO TO 800
        END IF

! --------- find type of current element

        itype = INLIST( ifield )
        IF ( loutff ) WRITE( outff, 3060 ) field2
        WRITE( outfd, 3060 ) field2
        IF ( neltype > 1 .AND. loutff ) WRITE( outff, 3061 ) itype
        IF ( neltype > 1 ) WRITE( outfd, 3061 ) itype
        IF ( DEFINED( itype ) ) THEN
          status = 67
          IF ( out > 0 ) WRITE( out, 2670 )
          GO TO 800
        ELSE
          DEFINED( itype ) = .TRUE.
          isetty = isetty + 1
        END IF

!  find the row and column dimensions (ninv and nelv, resp.) of the
!  transformation matrix u. u is stored in vector form by columns

        is = INV( itype ) - 1
        js = ELV( itype ) - 1
        nelv = ELV( itype + 1 ) - ELV( itype )
        ninv = INV( itype + 1 ) - INV( itype )
        nn = ninv * nelv

! --------- find type of current element

        IF ( nelv > ninv ) WRITE( outra, 4060 ) field2, itype

!  initialize u as the zero matrix

        U( : nn ) = zero
        setran = nelv > ninv

! --------- set elemental variables

        k1 = ELV( itype )
        k2 = ELV( itype + 1 ) - 1
        IF ( setran ) THEN
          IF ( iad0 == 1 ) THEN
             WRITE( outfd, 3230 ) nelv,                                        &
                    FIELDI(  4 )( 1 : 6 ), FIELDI(  8 )( 1 : 6 ),              &
                    FIELDI( 16 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ), nelv
          ELSE
             WRITE( outfd, 3231 ) nelv,                                        &
                    FIELDI(  4 )( 1 : 6 ), FIELDI(  8 )( 1 : 6 ),              &
                    FIELDI( 16 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ), nelv
          END IF
          DO k = k1, k2
            ivar = k - k1 + 1
            IF ( loutff )                                                      &
            WRITE( outff, 3070 ) EVNAMES( k ), FIELDI(  4 )( 1 : 6 ),          &
                FIELDI(  8 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ), ivar
            WRITE( outfd, 3220 ) EVNAMES( k ), ivar
          END DO
        ELSE
          IF ( iad0 == 1 ) THEN
             WRITE( outfd, 3210 ) FIELDI( 12 )( 1 : 6 ), ninv,                 &
                    FIELDI(  4 )( 1 : 6 ), FIELDI(  8 )( 1 : 6 ),              &
                    FIELDI( 16 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ), ninv
          ELSE
             WRITE( outfd, 3211 ) FIELDI( 12 )( 1 : 6 ), ninv,                 &
                    FIELDI(  4 )( 1 : 6 ), FIELDI(  8 )( 1 : 6 ),              &
                    FIELDI( 16 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ), ninv
          END IF
          DO k = k1, k2
            ivar = k - k1 + 1
            IF ( loutff )                                                      &
            WRITE( outff, 3070 ) EVNAMES( k ), FIELDI(  4 )( 1 : 6 ),          &
                FIELDI(  8 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ), ivar
            WRITE( outfd, 3220 ) EVNAMES( k ), ivar
          END DO
        END IF

!  find the number of internal variables and the required size of
!  the lower triangular portion of the Hessian matrix

        k1 = INV( itype ) ; k2 = INV( itype + 1 ) - 1
        ninvar = k2 - k1 + 1
        startp = .FALSE.
        startf = .FALSE.
        endoff = .TRUE.
        startv = .FALSE.

!  the range transformation matrix u is now defined, entry by entry
!  determine which internal variable is given in field2

      ELSE
        IF ( field1 == 'R ' ) THEN
          DO i = 1, ninv
            IF ( field2 == IVNAMES( is + i ) ) GO TO 450
          END DO

!  the internal variable name is unrecognised

          status = 65
          IF ( out > 0 ) WRITE( out, 2650 )
          GO TO 800

!  the internal variable is the i-th in the list

  450     CONTINUE

!  determine which elemental variable(s) occur in fields

          IF ( novals > 0 ) THEN
            DO k = 1, novals
              DO j = 1, nelv
                IF ( FIELDS( k ) == EVNAMES( js + j ) ) GO TO 470
              END DO

!  the elemental variable name is unrecognised

              status = 66
              IF ( out > 0 ) WRITE( out, 2660 )
              GO TO 800

!  the elemental variable is the j-th in the list

  470         CONTINUE

!  insert the value of the new nonzero into u

              U( ninv * ( j - 1 ) + i ) = VALUES( k )
            END DO
          END IF

!  finish the function assignment

        ELSE
          IF ( field1( 1 : 1 ) == 'A' .OR. field1( 1 : 1 ) == 'I' .OR.         &
               field1( 1 : 1 ) == 'E' ) THEN
             IF ( startf .AND. .NOT. endoff ) THEN
               IF ( loutff ) THEN
                 WRITE( outff, 3120 )
                 WRITE( outff, 3121 )
               END IF
               IF ( iad0 == 1 ) THEN
                 WRITE( outfd, 3122 )                                          &
                     FIELDI( 12 )( 1 : 6 ), FIELDI( 3  )( 1 : 6 ),             &
                     FIELDI( 13 )( 1 : 6 ), FIELDI( 3  )( 1 : 6 ),             &
                     FIELDI( 17 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ), ninvar
               ELSE
                 WRITE( outfd, 3123 )                                          &
                     FIELDI( 12 )( 1 : 6 ), FIELDI( 3  )( 1 : 6 ),             &
                     FIELDI( 13 )( 1 : 6 ), FIELDI( 3  )( 1 : 6 ),             &
                     FIELDI( 17 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ), ninvar
               END IF
               WRITE( outfd, 3150 ) FIELDI( 12 )( 1 : 6 )
               IF ( iad0 == 1 ) THEN
                 WRITE( outfd, 3151 )
               ELSE
                 WRITE( outfd, 3152 )
               END IF
               DO js = 1, ninvar
                 DO  is = 1, js
                   ihvar = ( js * ( js - 1 ) ) / 2 + is
                   IF ( is == js ) THEN
                     WRITE( outfd, 3163 ) FIELDI(  3 )( 1 : 6 ),               &
                       FIELDI( 15 )( 1 : 6 ), ihvar, ihvar
                   ELSE
                     WRITE( outfd, 3164 ) FIELDI(  3 )( 1 : 6 ),               &
                       FIELDI( 15 )( 1 : 6 ), ihvar, ihvar
                   END IF
                 END DO
               END DO
               endoff = .TRUE.
             END IF

!  start a parameter assignment

             IF ( .NOT. startf ) THEN

!  set up the transformations for the element

               IF ( field1( 2 : 2 ) == ' ' ) THEN
                 startp = .TRUE.
                 IF ( setran ) THEN
                   CALL OUTRANGE_ad( nelv, ninv, U, outff, outfd,              &
                                     outra, EVNAMES( js + 1 ),                 &
                                     IVNAMES( is + 1 ), single, ad0 )
                   setran = .FALSE.
                 END IF

! --------- set elemental parameters

                 k1 = ELP( itype )
                 DO k = k1, ELP( itype + 1 ) - 1
                   ivar = k - k1 + 1
                   IF ( loutff ) WRITE( outff, 3071 ) EPNAMES( k ),            &
                     FIELDI( 18 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ), ivar
                   WRITE( outfd, 3071 ) EPNAMES( k ),                          &
                     FIELDI( 18 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ), ivar
                 END DO

!  include the global parameters

                 IF ( .NOT. startv ) THEN
                   REWIND( outem )
                   DO i = 1, ntem
                     READ( outem, 1000 ) ctem
                     WRITE( outfd, 1000 ) ctem
                   END DO
                   startv = .TRUE.
                 END IF

!  check to see that the parameter has been defined

                 field = field2 // 'PF'
                 CALL HASH_search(LENGTH, 12, field, TABLE, KEY, IFIELD)
                 IF ( ifield <= 0 ) THEN
                   status = 58
                   IF ( out > 0 ) WRITE( out, 2580 )
                   GO TO 800
                 END IF

! --------- make element-specific parameter assignments

                 IF ( field1( 1 : 1 ) == 'A' ) THEN
                   IF ( .NOT. startf ) THEN
                     IF ( loutff ) WRITE( outff, 3080 ) FIELD2( 1 : 6 ), field7
                     WRITE( outfd, 3080 ) FIELD2( 1 : 6 ), field7
                   ELSE
                     IF ( loutff ) WRITE( outff, 3083 ) FIELD2( 1 : 6 ), field7
                     WRITE( outfd, 3083 ) FIELD2( 1 : 6 ), field7
                   END IF

! --------- make conditional parameter assignments

                 ELSE

!  check that the logical variable has been defined

                   field = field3 // '  PF'
                   CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
                   IF ( ifield <= 0 ) THEN
                     IF ( ifree == 0 ) THEN
                       status = - 1
                       GO TO 700
                     END IF
                     status = 58
                     IF ( out > 0 ) WRITE( out, 2580 )
                     GO TO 800
                   END IF
                   IF ( field1( 1 : 1 ) == 'I' ) THEN
                     IF ( .NOT. startf ) THEN
                       IF ( loutff ) WRITE( outff, 3081 ) FIELD2( 1 : 6 ),     &
                          FIELD3( 1 : 6 ), field7
                        WRITE( outfd, 3081 ) FIELD2( 1 : 6 ),                  &
                          FIELD3( 1 : 6 ), field7
                     ELSE
                       IF ( loutff ) WRITE( outff, 3084 ) FIELD2( 1 : 6 ),     &
                          FIELD3( 1 : 6 ), field7
                        WRITE( outfd, 3084 ) FIELD2( 1 : 6 ),                  &
                          FIELD3( 1 : 6 ), field7
                     END IF
                   ELSE
                   IF ( .NOT. startf ) THEN
                         IF ( loutff ) WRITE( outff, 3082 ) FIELD2( 1 : 6 ),   &
                           FIELD3( 1 : 6 ), field7
                         WRITE( outfd, 3082 ) FIELD2( 1 : 6 ),                 &
                           FIELD3( 1 : 6 ), field7
                      ELSE
                         IF ( loutff ) WRITE( outff, 3085 ) FIELD2( 1 : 6 ),   &
                           FIELD3( 1 : 6 ), field7
                         WRITE( outfd, 3085 ) FIELD2( 1 : 6 ),                 &
                           FIELD3( 1 : 6 ), field7
                      END IF
                   END IF
                 END IF

! --------- continuation of a parameter assignment

               ELSE
                 IF ( field1( 2 : 2 ) == '+' ) THEN
                   IF ( startp ) THEN
                     IF ( .NOT. startf ) THEN
                       IF ( loutff ) WRITE( outff, 3090 ) field7
                       WRITE( outfd, 3090 ) field7
                     ELSE
                       IF ( loutff ) WRITE( outff, 3091 ) field7
                       WRITE( outfd, 3091 ) field7
                     END IF
                   ELSE
                     status = 56
                     IF ( out > 0 ) WRITE( out, 2560 )
                     GO TO 800
                   END IF
                 END IF
              END IF
           END IF

!  set the function value

         ELSE
           startp = .FALSE.
           IF ( field1( 1 : 1 ) == 'F' ) THEN
             IF ( field1( 2 : 2 ) == ' ' ) THEN
               startf = .TRUE.
               endoff = .FALSE.

!  set up the transformations for the element

               IF ( setran ) THEN
                 CALL OUTRANGE_ad( nelv, ninv, U, outff, outfd,                &
                                   outra, EVNAMES( js + 1 ),                   &
                                   IVNAMES( is + 1 ), single, ad0 )
                 setran = .FALSE.
               END IF

! --------- set elemental parameters

               k1 = ELP( itype )
               DO k = k1, ELP( itype + 1 ) - 1
                 ivar = k - k1 + 1
                 IF ( loutff ) WRITE( outff, 3071 ) EPNAMES( k ),              &
                   FIELDI( 18 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ), ivar
                 WRITE( outfd, 3071 ) EPNAMES( k ),                            &
                   FIELDI( 18 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ), ivar
               END DO

!  include the global parameters

               IF ( .NOT. startv ) THEN
                 REWIND( outem )
                 DO i = 1, ntem
                   READ( outem, 1000 ) ctem
                   WRITE( outfd, 1000 ) ctem
                 END DO
                 startv = .TRUE.
               END IF

! --------- start f

               IF ( loutff ) WRITE( outff, 3100 ) FIELDI( 12 )( 1 : 6 ),       &
                 FIELDI( 3 )( 1 : 6 ), FIELDI( 13 )( 1 : 6 ), field7
                 WRITE( outfd, 3101 ) field7

! --------- continuation of f

               ELSE
                 IF ( field1( 2 : 2 ) == '+' ) THEN
                   IF ( startf ) THEN
                     IF ( loutff ) WRITE( outff, 3110 ) field7
                     WRITE( outfd, 3110 ) field7
                   ELSE
                     status = 56
                     IF ( out > 0 ) WRITE( out, 2560 )
                     GO TO 800
                   END IF
                 END IF
              END IF
            ELSE IF ( field1( 1 : 1 ) == 'G' .OR. field1( 1 : 1 ) == 'H' ) THEN
              IF ( startf .AND. .NOT. endoff ) THEN
                IF ( loutff ) THEN
                  WRITE( outff, 3120 )
                  WRITE( outff, 3121 )
                END IF
                IF ( iad0 == 1 ) THEN
                   WRITE( outfd, 3122 )                                        &
                      FIELDI( 12 )( 1 : 6 ), FIELDI( 3  )( 1 : 6 ),            &
                      FIELDI( 13 )( 1 : 6 ), FIELDI( 3  )( 1 : 6 ),            &
                      FIELDI( 17 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ), ninvar
                ELSE
                   WRITE( outfd, 3123 )                                        &
                      FIELDI( 12 )( 1 : 6 ), FIELDI( 3  )( 1 : 6 ),            &
                      FIELDI( 13 )( 1 : 6 ), FIELDI( 3  )( 1 : 6 ),            &
                      FIELDI( 17 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ), ninvar
                END IF
                WRITE( outfd, 3150 ) FIELDI( 12 )( 1 : 6 )
                IF ( iad0 == 1 ) THEN
                  WRITE( outfd, 3151 )
                ELSE
                  WRITE( outfd, 3152 )
                END IF
                DO js = 1, ninvar
                  DO is = 1, js
                    ihvar = ( js * ( js - 1 ) ) / 2 + is
                    IF ( is == js ) THEN
                      WRITE( outfd, 3163 ) FIELDI(  3 )( 1 : 6 ),              &
                          FIELDI( 15 )( 1 : 6 ), ihvar, ihvar
                    ELSE
                      WRITE( outfd, 3164 ) FIELDI(  3 )( 1 : 6 ),              &
                          FIELDI( 15 )( 1 : 6 ), ihvar, ihvar
                    END IF
                  END DO
                END DO
                endoff = .TRUE.
              END IF
            ELSE
              status = 56
              IF ( out > 0 ) WRITE( out, 2560 )
              GO TO 800
            END IF
          END IF
        END IF
      END IF
      GO TO 100

!  the end of the input file has been reached before the endata card

  590 CONTINUE

!  if the elements card has not been encountered, exit

      IF ( defnam ) THEN
        status = 52
        IF ( out > 0 ) WRITE( out, 2520 )
        RETURN
      END IF
!     if ( neltype > 0 ) go to 930
      qprod = .FALSE.
      DO itype = 1, MIN( 2, neltype )
        IF ( ETYPES( itype ) /= cqsqr .AND.                                    &
             ETYPES( itype ) /= cqprod ) GO TO 930
        IF ( ETYPES( itype ) == cqprod ) qprod = .TRUE.
      END DO
      IF ( out > 0 .AND. print_level /= 0 ) WRITE( out, 2010 )

!  a dummy routine will be substituted

  600 CONTINUE

!  write a dummy elfuns routine

      IF ( iauto == 1 ) THEN
        IF ( single ) THEN
          aorb = 'FORWARD_SINGLE '
        ELSE
          aorb = 'FORWARD_DOUBLE '
        END IF
      ELSE
        IF ( single ) THEN
          aorb = 'BACKWARD_SINGLE'
        ELSE
          aorb = 'BACKWARD_DOUBLE'
        END IF
      END IF
      IF ( iad0 == 1 ) THEN
        ad0 = 'AD01'
      ELSE
        ad0 = 'AD02'
      END IF
      IF ( neltype == 0 ) THEN
        IF ( single ) THEN
          IF ( loutff ) WRITE( outff, 3003 ) FIELDI( 1 )( 1 : 6 ),             &
           FIELDI(  3 )( 1 : 6 ), FIELDI(  4 )( 1 : 6 ),                       &
           FIELDI( 18 )( 1 : 6 ), ( FIELDI(  i )( 1 : 6 ), i = 5, 10 ),        &
           FIELDI( 19 )( 1 : 6 ), FIELDI( 11 )( 1 : 6 ),                       &
         ( FIELDI(  i )( 1 : 6 ), i = 22, 31 ),                                &
           FIELDI( 12 )( 1 : 6 ), FIELDI( 32 )( 1 : 6 ),                       &
           FIELDI(  5 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),                       &
         ( FIELDI(  i )( 1 : 6 ), i = 22, 32 ),                                &
           FIELDI(  6 )( 1 : 6 ), FIELDI( 22 )( 1 : 6 ),                       &
           FIELDI(  7 )( 1 : 6 ), FIELDI( 23 )( 1 : 6 ),                       &
           FIELDI(  8 )( 1 : 6 ), FIELDI( 24 )( 1 : 6 ),                       &
           FIELDI(  9 )( 1 : 6 ), FIELDI( 25 )( 1 : 6 ),                       &
           FIELDI( 10 )( 1 : 6 ), FIELDI( 26 )( 1 : 6 ),                       &
           FIELDI( 19 )( 1 : 6 ), FIELDI( 27 )( 1 : 6 ),                       &
           FIELDI( 11 )( 1 : 6 ), FIELDI( 28 )( 1 : 6 ),                       &
           FIELDI(  3 )( 1 : 6 ), FIELDI( 29 )( 1 : 6 ),                       &
           FIELDI(  4 )( 1 : 6 ), FIELDI( 30 )( 1 : 6 ),                       &
           FIELDI( 18 )( 1 : 6 ), FIELDI( 31 )( 1 : 6 ), pname, TRIM( version )
          WRITE( outfd, 3007 ) FIELDI( 33 )( 1 : 6 ),                          &
           FIELDI(  3 )( 1 : 6 ), FIELDI(  4 )( 1 : 6 ),                       &
           FIELDI( 18 )( 1 : 6 ), ( FIELDI(  i )( 1 : 6 ), i = 5, 10 ),        &
           FIELDI( 19 )( 1 : 6 ), FIELDI( 11 )( 1 : 6 ),                       &
         ( FIELDI(  i )( 1 : 6 ), i = 22, 31 ),                                &
           FIELDI( 12 )( 1 : 6 ), FIELDI( 32 )( 1 : 6 ), ad0, aorb,            &
           FIELDI(  5 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),                       &
         ( FIELDI(  i )( 1 : 6 ), i = 22, 32 ),                                &
           FIELDI(  6 )( 1 : 6 ), FIELDI( 22 )( 1 : 6 ),                       &
           FIELDI(  7 )( 1 : 6 ), FIELDI( 23 )( 1 : 6 ),                       &
           FIELDI(  8 )( 1 : 6 ), FIELDI( 24 )( 1 : 6 ),                       &
           FIELDI(  9 )( 1 : 6 ), FIELDI( 25 )( 1 : 6 ),                       &
           FIELDI( 10 )( 1 : 6 ), FIELDI( 26 )( 1 : 6 ),                       &
           FIELDI( 19 )( 1 : 6 ), FIELDI( 27 )( 1 : 6 ),                       &
           FIELDI( 11 )( 1 : 6 ), FIELDI( 28 )( 1 : 6 ),                       &
           FIELDI(  3 )( 1 : 6 ), FIELDI( 29 )( 1 : 6 ),                       &
           FIELDI(  4 )( 1 : 6 ), FIELDI( 30 )( 1 : 6 ),                       &
           FIELDI( 18 )( 1 : 6 ), FIELDI( 31 )( 1 : 6 ), pname, TRIM( version )
        ELSE
          IF ( loutff ) WRITE( outff, 3002 ) FIELDI( 1 )( 1 : 6 ),             &
          FIELDI(  3 )( 1 : 6 ), FIELDI(  4 )( 1 : 6 ),                        &
          FIELDI( 18 )( 1 : 6 ), ( FIELDI(  i )( 1 : 6 ), i = 5, 10 ),         &
          FIELDI( 19 )( 1 : 6 ), FIELDI( 11 )( 1 : 6 ),                        &
        ( FIELDI(  i )( 1 : 6 ), i = 22, 31 ),                                 &
          FIELDI( 12 )( 1 : 6 ), FIELDI( 32 )( 1 : 6 ),                        &
          FIELDI(  5 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),                        &
        ( FIELDI(  i )( 1 : 6 ), i = 22, 32 ),                                 &
          FIELDI(  6 )( 1 : 6 ), FIELDI( 22 )( 1 : 6 ),                        &
          FIELDI(  7 )( 1 : 6 ), FIELDI( 23 )( 1 : 6 ),                        &
          FIELDI(  8 )( 1 : 6 ), FIELDI( 24 )( 1 : 6 ),                        &
          FIELDI(  9 )( 1 : 6 ), FIELDI( 25 )( 1 : 6 ),                        &
          FIELDI( 10 )( 1 : 6 ), FIELDI( 26 )( 1 : 6 ),                        &
          FIELDI( 19 )( 1 : 6 ), FIELDI( 27 )( 1 : 6 ),                        &
          FIELDI( 11 )( 1 : 6 ), FIELDI( 28 )( 1 : 6 ),                        &
          FIELDI(  3 )( 1 : 6 ), FIELDI( 29 )( 1 : 6 ),                        &
          FIELDI(  4 )( 1 : 6 ), FIELDI( 30 )( 1 : 6 ),                        &
          FIELDI( 18 )( 1 : 6 ), FIELDI( 31 )( 1 : 6 ), pname, TRIM( version )
          WRITE( outfd, 3006 ) FIELDI( 33 )( 1 : 6 ),                          &
          FIELDI(  3 )( 1 : 6 ), FIELDI(  4 )( 1 : 6 ),                        &
          FIELDI( 18 )( 1 : 6 ), ( FIELDI(  i )( 1 : 6 ), i = 5, 10 ),         &
          FIELDI( 19 )( 1 : 6 ), FIELDI( 11 )( 1 : 6 ),                        &
        ( FIELDI(  i )( 1 : 6 ), i = 22, 31 ),                                 &
          FIELDI( 12 )( 1 : 6 ), FIELDI( 32 )( 1 : 6 ), ad0, aorb,             &
          FIELDI(  5 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),                        &
        ( FIELDI(  i )( 1 : 6 ), i = 22, 32 ),                                 &
          FIELDI(  6 )( 1 : 6 ), FIELDI( 22 )( 1 : 6 ),                        &
          FIELDI(  7 )( 1 : 6 ), FIELDI( 23 )( 1 : 6 ),                        &
          FIELDI(  8 )( 1 : 6 ), FIELDI( 24 )( 1 : 6 ),                        &
          FIELDI(  9 )( 1 : 6 ), FIELDI( 25 )( 1 : 6 ),                        &
          FIELDI( 10 )( 1 : 6 ), FIELDI( 26 )( 1 : 6 ),                        &
          FIELDI( 19 )( 1 : 6 ), FIELDI( 27 )( 1 : 6 ),                        &
          FIELDI( 11 )( 1 : 6 ), FIELDI( 28 )( 1 : 6 ),                        &
          FIELDI(  3 )( 1 : 6 ), FIELDI( 29 )( 1 : 6 ),                        &
          FIELDI(  4 )( 1 : 6 ), FIELDI( 30 )( 1 : 6 ),                        &
          FIELDI( 18 )( 1 : 6 ), FIELDI( 31 )( 1 : 6 ), pname, TRIM( version )
        END IF
        IF ( loutff ) WRITE( outff, 3009 ) FIELDI( 32 )( 1 : 6 )
        WRITE( outfd, 3009 ) FIELDI( 32 )( 1 : 6 )
        IF ( loutff ) WRITE( outff, 3201 )
        WRITE( outfd, 3201 )
      ELSE
        IF ( single ) THEN
          IF ( loutff ) WRITE( outff, 3001 ) FIELDI( 1 )( 1 : 6 ),             &
       FIELDI(  3 )( 1 : 6 ), FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),    &
       ( FIELDI(  i )( 1 : 6 ), i = 5, 10 ), FIELDI( 19 )( 1 : 6 ),            &
        FIELDI( 11 )( 1 : 6 ), ( FIELDI(  i )( 1 : 6 ), i = 22, 31 ),          &
       FIELDI( 12 )( 1 : 6 ), FIELDI( 32 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),    &
       FIELDI( 12 )( 1 : 6 ), ( FIELDI(  i )( 1 : 6 ), i = 22, 32 ),           &
       FIELDI(  6 )( 1 : 6 ), FIELDI( 22 )( 1 : 6 ),                           &
       FIELDI(  7 )( 1 : 6 ), FIELDI( 23 )( 1 : 6 ),                           &
       FIELDI(  8 )( 1 : 6 ), FIELDI( 24 )( 1 : 6 ),                           &
       FIELDI(  9 )( 1 : 6 ), FIELDI( 25 )( 1 : 6 ),                           &
       FIELDI( 10 )( 1 : 6 ), FIELDI( 26 )( 1 : 6 ),                           &
       FIELDI( 19 )( 1 : 6 ), FIELDI( 27 )( 1 : 6 ),                           &
       FIELDI( 11 )( 1 : 6 ), FIELDI( 28 )( 1 : 6 ),                           &
       FIELDI(  3 )( 1 : 6 ), FIELDI( 29 )( 1 : 6 ),                           &
       FIELDI(  4 )( 1 : 6 ), FIELDI( 30 )( 1 : 6 ),                           &
       FIELDI( 18 )( 1 : 6 ), FIELDI( 31 )( 1 : 6 ),                           &
       pname, TRIM( version ), FIELDI( 13 )( 1 : 6 ), FIELDI( 14 )( 1 : 6 ),   &
       FIELDI( 15 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),                           &
       FIELDI( 17 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ), FIELDI( 21 )( 1 : 6 )
          WRITE( outfd, 3001 ) FIELDI( 33 )( 1 : 6 ),                          &
       FIELDI(  3 )( 1 : 6 ), FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),    &
       ( FIELDI(  i )( 1 : 6 ), i = 5, 10 ), FIELDI( 19 )( 1 : 6 ),            &
       FIELDI( 11 )( 1 : 6 ), ( FIELDI(  i )( 1 : 6 ), i = 22, 31 ),           &
       FIELDI( 12 )( 1 : 6 ), FIELDI( 32 )( 1 : 6 ),                           &
       FIELDI(  5 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),                           &
       ( FIELDI(  i )( 1 : 6 ), i = 22, 32 ),                                  &
       FIELDI(  6 )( 1 : 6 ), FIELDI( 22 )( 1 : 6 ),                           &
       FIELDI(  7 )( 1 : 6 ), FIELDI( 23 )( 1 : 6 ),                           &
       FIELDI(  8 )( 1 : 6 ), FIELDI( 24 )( 1 : 6 ),                           &
       FIELDI(  9 )( 1 : 6 ), FIELDI( 25 )( 1 : 6 ),                           &
       FIELDI( 10 )( 1 : 6 ), FIELDI( 26 )( 1 : 6 ),                           &
       FIELDI( 19 )( 1 : 6 ), FIELDI( 27 )( 1 : 6 ),                           &
       FIELDI( 11 )( 1 : 6 ), FIELDI( 28 )( 1 : 6 ),                           &
       FIELDI(  3 )( 1 : 6 ), FIELDI( 29 )( 1 : 6 ),                           &
       FIELDI(  4 )( 1 : 6 ), FIELDI( 30 )( 1 : 6 ),                           &
       FIELDI( 18 )( 1 : 6 ), FIELDI( 31 )( 1 : 6 ),                           &
       pname, TRIM( version ), FIELDI( 13 )( 1 : 6 ), FIELDI( 14 )( 1 : 6 ),   &
       FIELDI( 15 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),                           &
       FIELDI( 17 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ), FIELDI( 21 )( 1 : 6 )
          IF ( qprod ) THEN
            IF ( loutff ) WRITE( outff, 3019 ) 'X     ', 'Y     '
            WRITE( outfd, 3019 ) 'X     ', 'Y     '
          ELSE
            IF ( loutff ) WRITE( outff, 3019 ) 'X     '
             WRITE( outfd, 3019 ) 'X     '
          END IF
        ELSE
          IF ( loutff ) WRITE( outff, 3000 ) FIELDI( 1 )( 1 : 6 ),             &
       FIELDI(  3 )( 1 : 6 ), FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),    &
       ( FIELDI(  i )( 1 : 6 ), i = 5, 10 ), FIELDI( 19 )( 1 : 6 ),            &
       FIELDI( 11 )( 1 : 6 ), ( FIELDI(  i )( 1 : 6 ), i = 22, 31 ),           &
       FIELDI( 12 )( 1 : 6 ), FIELDI( 32 )( 1 : 6 ),                           &
       FIELDI(  5 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),                           &
       ( FIELDI(  i )( 1 : 6 ), i = 22, 32 ), FIELDI(  6 )( 1 : 6 ),           &
       FIELDI( 22 )( 1 : 6 ), FIELDI(  7 )( 1 : 6 ), FIELDI( 23 )( 1 : 6 ),    &
       FIELDI(  8 )( 1 : 6 ), FIELDI( 24 )( 1 : 6 ),                           &
       FIELDI(  9 )( 1 : 6 ), FIELDI( 25 )( 1 : 6 ),                           &
       FIELDI( 10 )( 1 : 6 ), FIELDI( 26 )( 1 : 6 ),                           &
       FIELDI( 19 )( 1 : 6 ), FIELDI( 27 )( 1 : 6 ),                           &
       FIELDI( 11 )( 1 : 6 ), FIELDI( 28 )( 1 : 6 ),                           &
       FIELDI(  3 )( 1 : 6 ), FIELDI( 29 )( 1 : 6 ),                           &
       FIELDI(  4 )( 1 : 6 ), FIELDI( 30 )( 1 : 6 ),                           &
       FIELDI( 18 )( 1 : 6 ), FIELDI( 31 )( 1 : 6 ),                           &
       pname, TRIM( version ), FIELDI( 13 )( 1 : 6 ), FIELDI( 14 )( 1 : 6 ),   &
       FIELDI( 15 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),                           &
       FIELDI( 17 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),                           &
       FIELDI( 21 )( 1 : 6 )
          WRITE( outfd, 3000 ) FIELDI( 33 )( 1 : 6 ),                          &
       FIELDI(  3 )( 1 : 6 ), FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),    &
       ( FIELDI(  i )( 1 : 6 ), i = 5, 10 ), FIELDI( 19 )( 1 : 6 ),            &
       FIELDI( 11 )( 1 : 6 ), ( FIELDI(  i )( 1 : 6 ), i = 22, 31 ),           &
       FIELDI( 12 )( 1 : 6 ), FIELDI( 32 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),    &
       FIELDI( 12 )( 1 : 6 ), ( FIELDI(  i )( 1 : 6 ), i = 22, 32 ),           &
       FIELDI(  6 )( 1 : 6 ), FIELDI( 22 )( 1 : 6 ),                           &
       FIELDI(  7 )( 1 : 6 ), FIELDI( 23 )( 1 : 6 ),                           &
       FIELDI(  8 )( 1 : 6 ), FIELDI( 24 )( 1 : 6 ),                           &
       FIELDI(  9 )( 1 : 6 ), FIELDI( 25 )( 1 : 6 ),                           &
       FIELDI( 10 )( 1 : 6 ), FIELDI( 26 )( 1 : 6 ),                           &
       FIELDI( 19 )( 1 : 6 ), FIELDI( 27 )( 1 : 6 ),                           &
       FIELDI( 11 )( 1 : 6 ), FIELDI( 28 )( 1 : 6 ),                           &
       FIELDI(  3 )( 1 : 6 ), FIELDI( 29 )( 1 : 6 ),                           &
       FIELDI(  4 )( 1 : 6 ), FIELDI( 30 )( 1 : 6 ),                           &
       FIELDI( 18 )( 1 : 6 ), FIELDI( 31 )( 1 : 6 ),                           &
       pname, TRIM( version ), FIELDI( 13 )( 1 : 6 ), FIELDI( 14 )( 1 : 6 ),   &
       FIELDI( 15 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),                           &
       FIELDI( 17 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ), FIELDI( 21 )( 1 : 6 )
          IF ( qprod ) THEN
            IF ( loutff ) WRITE( outff, 3020 ) 'X     ', 'Y     '
            WRITE( outfd, 3020 ) 'X     ', 'Y     '
          ELSE
            IF ( loutff ) WRITE( outff, 3020 ) 'X     '
            WRITE( outfd, 3020 ) 'X     '
          END IF
        END IF
        IF ( loutff ) WRITE( outff, 3009 ) FIELDI( 32 )( 1 : 6 )
        WRITE( outfd, 3009 ) FIELDI( 32 )( 1 : 6 )
        IF ( loutff ) WRITE( outff, 3050 ) nloop,                              &
                FIELDI( 21 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),                  &
                FIELDI( 13 )( 1 : 6 ), FIELDI( 11 )( 1 : 6 ),                  &
                FIELDI( 21 )( 1 : 6 ),                                         &
                FIELDI( 16 )( 1 : 6 ), FIELDI(  7 )( 1 : 6 ),                  &
                FIELDI( 13 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),                  &
                FIELDI(  9 )( 1 : 6 ), FIELDI( 13 )( 1 : 6 ),                  &
                FIELDI( 20 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),                  &
                FIELDI( 13 )( 1 : 6 ),                                         &
                FIELDI( 12 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),                  &
                FIELDI( 10 )( 1 : 6 ), FIELDI( 13 )( 1 : 6 )
        WRITE( outfd, 3050 ) nloop,                                            &
                FIELDI( 21 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),                  &
                FIELDI( 13 )( 1 : 6 ), FIELDI( 11 )( 1 : 6 ),                  &
                FIELDI( 21 )( 1 : 6 ),                                         &
                FIELDI( 16 )( 1 : 6 ), FIELDI(  7 )( 1 : 6 ),                  &
                FIELDI( 13 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),                  &
                FIELDI(  9 )( 1 : 6 ), FIELDI( 13 )( 1 : 6 ),                  &
                FIELDI( 20 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),                  &
                FIELDI( 13 )( 1 : 6 ),                                         &
                FIELDI( 12 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),                  &
                FIELDI( 10 )( 1 : 6 ), FIELDI( 13 )( 1 : 6 )
        IF ( neltype > 1 ) THEN
          IF ( loutff ) WRITE( outff, 3051 )                                   &
             FIELDI( 14 )( 1 : 6 ), FIELDI(  6 )( 1 : 6 ),                     &
             FIELDI( 13 )( 1 : 6 ), ( i, i = 1, neltype )
          WRITE( outfd, 3051 )                                                 &
             FIELDI( 14 )( 1 : 6 ), FIELDI(  6 )( 1 : 6 ),                     &
             FIELDI( 13 )( 1 : 6 ), ( i, i = 1, neltype )
          IF ( loutff ) WRITE( outff, 3052 ) FIELDI( 14 )( 1 : 6 )
           WRITE( outfd, 3052 ) FIELDI( 14 )( 1 : 6 )
        END IF

!  make sure that quadratic Hessian terms are included

        DO itype = 1, MIN( 2, neltype )

!  diagonal term

          IF ( ETYPES( itype ) == cqsqr ) THEN
            IF ( loutff ) WRITE( outff, 3060 ) ETYPES( itype )
            WRITE( outfd, 3060 ) ETYPES( itype )
            IF ( neltype > 1 ) WRITE( outff, 3061 ) itype
            IF ( neltype > 1 ) WRITE( outfd, 3061 ) itype
            IF ( single ) THEN
              IF ( loutff ) WRITE( outff, 3055 ) 'E'
              WRITE( outfd, 3053 ) 'E', 'E'
            ELSE
              IF ( loutff ) WRITE( outff, 3055 ) 'D'
              WRITE( outfd, 3053 ) 'D', 'D'
            END IF
            DEFINED( itype ) = .TRUE.
            isetty = isetty + 1
            IF ( isetty < neltype ) THEN
              IF ( loutff ) WRITE( outff, 3191 ) nloop
              WRITE( outfd, 3191 ) nloop
            END IF

!  off-diagonal term

          ELSE IF ( ETYPES( itype ) == cqprod ) THEN
            IF ( loutff ) WRITE( outff, 3060 ) ETYPES( itype )
            WRITE( outfd, 3060 ) ETYPES( itype )
            IF ( neltype > 1 ) THEN
              IF ( loutff ) WRITE( outff, 3061 ) itype
              WRITE( outfd, 3061 ) itype
            END IF
            IF ( single ) THEN
              IF ( loutff ) WRITE( outff, 3056 )
              WRITE( outfd, 3054 ) 'E', 'E', 'E'
            ELSE
              IF ( loutff ) WRITE( outff, 3056 )
              WRITE( outfd, 3054 ) 'D', 'D', 'D'
            END IF
            DEFINED( itype ) = .TRUE.
            isetty = isetty + 1
            IF ( isetty < neltype ) THEN
               IF ( loutff ) WRITE( outff, 3191 ) nloop
               WRITE( outfd, 3191 ) nloop
            END IF
          END IF
        END DO
        IF ( loutff ) WRITE( outff, 3200 ) nloop
        IF ( iad0 == 2 ) THEN
          WRITE( outfd, 3202 ) nloop
        ELSE
          WRITE( outfd, 3200 ) nloop
        END IF
      END IF

!  write a dummy range routine

      IF ( single ) THEN
        WRITE( outra, 4003 ) pname, TRIM( version )
      ELSE
        WRITE( outra, 4002 ) pname, TRIM( version )
      END IF
      WRITE( outra, 4080 )
      WRITE( outra, 4090 )
      status = 0
      RETURN

!  insufficient space to continue construction

  700 CONTINUE
      IF ( out > 0 ) WRITE( out, 2000 )
      RETURN

!  subroutine incomplete

  800 CONTINUE
      IF ( out > 0 ) WRITE( out, 2990 ) lineno, nuline
      RETURN

!  subroutine successfully completed

  900 CONTINUE

!  finish of the previous element, if any

      IF ( .NOT. firstl ) THEN
        IF ( startf ) THEN
          IF ( .NOT. endoff ) THEN
            IF ( loutff ) THEN
              WRITE( outff, 3120 )
              WRITE( outff, 3121 )
            END IF
            IF ( iad0 == 1 ) THEN
              WRITE( outfd, 3122 )                                             &
                  FIELDI( 12 )( 1 : 6 ), FIELDI( 3  )( 1 : 6 ),                &
                  FIELDI( 13 )( 1 : 6 ), FIELDI( 3  )( 1 : 6 ),                &
                  FIELDI( 17 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ), ninvar
            ELSE
              WRITE( outfd, 3123 )                                             &
                  FIELDI( 12 )( 1 : 6 ), FIELDI( 3  )( 1 : 6 ),                &
                  FIELDI( 13 )( 1 : 6 ), FIELDI( 3  )( 1 : 6 ),                &
                  FIELDI( 17 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ), ninvar
            END IF
            WRITE( outfd, 3150 ) FIELDI( 12 )( 1 : 6 )
            IF ( iad0 == 1 ) THEN
              WRITE( outfd, 3151 )
            ELSE
              WRITE( outfd, 3152 )
            END IF
            DO js = 1, ninvar
              DO is = 1, js
                ihvar = ( js * ( js - 1 ) ) / 2 + is
                IF ( is == js ) THEN
                  WRITE( outfd, 3163 ) FIELDI(  3 )( 1 : 6 ),               &
                      FIELDI( 15 )( 1 : 6 ), ihvar, ihvar
                ELSE
                  WRITE( outfd, 3164 ) FIELDI(  3 )( 1 : 6 ),               &
                      FIELDI( 15 )( 1 : 6 ), ihvar, ihvar
                END IF
              END DO
            END DO
            endoff = .TRUE.
          END IF
         IF ( loutff ) WRITE( outff, 3190 )
          WRITE( outfd, 3180 )
          WRITE( outfd, 3190 )
        ELSE
          status = 61
          IF ( out > 0 ) WRITE( out, 2610 )
          GO TO 800
        END IF
        IF ( isetty < neltype .AND. loutff ) WRITE( outff, 3191 ) nloop
        IF ( isetty < neltype ) WRITE( outfd, 3191 ) nloop
      END IF

! ---------- successful run. wind up output

      status = 0
      IF ( loutff ) WRITE( outff, 3200 ) nloop
      IF ( iad0 == 2 ) THEN
        WRITE( outfd, 3202 ) nloop
      ELSE
        WRITE( outfd, 3200 ) nloop
      END IF
      IF ( nointe ) WRITE( outra, 4070 )
      IF ( neltype == 0 ) WRITE( outra, 4080 )
      WRITE( outra, 4090 )

!   check that all element types have been defined

  930 CONTINUE
      DO itype = 1, neltype
        IF ( .NOT. DEFINED( itype ) ) THEN
          status = 68
          IF ( out > 0 ) WRITE( out, 2680 ) ETYPES( itype )
        END IF
      END DO
      RETURN

!  allocation errors

  980 CONTINUE
      WRITE( out, "( ' ** Message from MAKE_elfun_ad-',                        &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  non-executable statements

 1000 FORMAT( A72 )
 1010 FORMAT( A160 )
 2000 FORMAT( ' ** Exit from MAKE_elfun_ad - insufficient memory available',   &
              ' to enlarge hash table' )
 2010 FORMAT( ' ** Exit from MAKE_elfun_ad - warning.',                        &
              ' First card not elements. ', /, '    A dummy',                  &
              ' routine will be substituted ' )
 2020 FORMAT( ' ** Exit from MAKE_elfun_ad - indicator card not recognised ' )
 2090 FORMAT( ' ** Exit from MAKE_elfun_ad - element type not recognised ' )
 2510 FORMAT( ' ** Exit from MAKE_elfun_ad -',                                 &
              ' name on card not that specified on input ' )
 2520 FORMAT( ' ** Exit from MAKE_elfun_ad - data file incomplete.',           &
              ' No ENDATA card ' )
 2540 FORMAT( ' ** Exit from MAKE_elfun_ad -',                                 &
              ' unrecognised field 1 in TEMPORARIES section' )
 2550 FORMAT( ' ** Exit from MAKE_elfun_ad -',                                 &
              ' unrecognised field 1 in GLOBALS section' )
 2560 FORMAT( ' ** Exit from MAKE_elfun_ad -',                                 &
              ' unrecognised field 1 in INDIVIDUALS section' )
 2570 FORMAT( ' ** Exit from MAKE_elfun_ad -',                                 &
              ' undefined parameter in GLOBALS section' )
 2580 FORMAT( ' ** Exit from MAKE_elfun_ad -',                                 &
              ' undefined parameter in INDIVIDUALS section' )
 2590 FORMAT( ' ** Exit from MAKE_elfun_ad - repeated parameter name ', A8 )
 2610 FORMAT( ' ** Exit from MAKE_elfun_ad - function not set '  )
 2650 FORMAT( ' ** Exit from MAKE_elfun_ad -',                                 &
              ' internal variable not recognised ' )
 2660 FORMAT( ' ** Exit from MAKE_elfun_ad -',                                 &
              ' elemental variable not recognised ' )
 2670 FORMAT( ' ** Exit from MAKE_elfun_ad - element type already defined ' )
 2680 FORMAT( ' ** Exit from MAKE_elfun_ad - warning, element type ', A10,     &
              ' undefined ' )
 2720 FORMAT( ' ** Exit from MAKE_elfun_ad - field 3 not blank on',            &
              ' A, F or G card ' )
 2900 FORMAT( ' ' )
 2970 FORMAT( ' Line ', i5, 4X, A160 )
 2980 FORMAT( ' Line ', i5, '.', i2, 1X, A65 )
 2990 FORMAT( ' Line ', i5, 4X, A65 )
 3000 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
        '     *                   ', 5( A6, ', ' ), /,                         &
        '     *                   ', 5( A6, ', ' ), /,                         &
        '     *                   ', 5( A6, ', ' ), /,                         &
        '     *                   ', 2( A6, ', ' ), A6, ' )', /,               &
        '      INTEGER ', 5( A6, ', ' ),  A6, /,                               &
        '      INTEGER ', 5( A6, ', ' ),  A6, /,                               &
        '      INTEGER ', 2( A6, '(', A6, '), ' ),                             &
                             A6, '(', A6, ')', /,                              &
        '      INTEGER ', 2( A6, '(', A6, '), ' ),                             &
                             A6, '(', A6, ')', /,                              &
        '      INTEGER ', A6, '(', A6, ')', /,                                 &
        '      DOUBLE PRECISION ', A6, '(', A6, '), ',                         &
                                   A6, '(', A6, '), ',                         &
                                   A6, '(', A6, ')', /,                        &
        'C', /, 'C  Problem name : ', A10, /,                                  &
        'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,                  &
        '      INTEGER ', 5( A6, ', ' ), A6, /,                                &
        '      INTEGER ', A6 )
 3001 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
        '     *                   ', 5( A6, ', ' ), /,                         &
        '     *                   ', 5( A6, ', ' ), /,                         &
        '     *                   ', 5( A6, ', ' ), /,                         &
        '     *                   ', 2( A6, ', ' ), A6, ' )', /,               &
        '      INTEGER ', 5( A6, ', ' ),  A6, /,                               &
        '      INTEGER ', 5( A6, ', ' ),  A6, /,                               &
        '      INTEGER ', A6, /,                                               &
        '      INTEGER ', 2( A6, '(', A6, '), ' ),                             &
                             A6, '(', A6, ')', /,                              &
        '      INTEGER ', 2( A6, '(', A6, '), ' ),                             &
                             A6, '(', A6, ')', /,                              &
        '      INTEGER ', A6, '(', A6, ')', /,                                 &
        '      REAL             ', A6, '(', A6, '), ',                         &
                                   A6, '(', A6, '), ',                         &
                                   A6, '(', A6, ')', /,                        &
        'C', /, 'C  Problem name : ', A10, /,                                  &
        'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,                  &
        '      INTEGER ', 5( A6, ', ' ), A6, /,                                &
        '      INTEGER ', A6 )
 3002 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', A6, /,                                         &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', A6, '(', A6, ')', /,                           &
              '      DOUBLE PRECISION ', A6, '(', A6, '), ',                   &
                                         A6, '(', A6, '), ',                   &
                                         A6, '(', A6, ')', /,                  &
              'C', /, 'C  Problem name : ', A10, /,                            &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C' )
 3003 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', A6, /,                                         &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', A6, '(', A6, ')', /,                           &
              '      REAL             ', A6, '(', A6, '), ',                   &
                                         A6, '(', A6, '), ',                   &
                                         A6, '(', A6, ')', /,                  &
              'C', /, 'C  Problem name : ', A10, /,                            &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C' )
 3004 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,         &
       '      USE HSL_', A4, '_', A15, /, '      INTEGER ',                    &
        5( A6, ', ' ),  A6, /, '      INTEGER ', 5( A6, ', ' ),  A6, /,        &
       '      INTEGER ', A6, /, '      INTEGER ', 2( A6, '(',                  &
       A6, '), ' ), A6, '(', A6, ')', /,                                       &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', A6, '(', A6, ')', /,                           &
              '      DOUBLE PRECISION ', A6, '(', A6, '), ',                   &
                                         A6, '(', A6, '), ',                   &
                                         A6, '(', A6, ')', /,                  &
              'C', /, 'C  Problem name : ', A10, /,                            &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
       '      INTEGER ', 5( A6, ', ' ), A6, /, '      INTEGER ', A6, /,        &
              '      INTEGER, POINTER :: H_index( : ) ', /,                    &
              '      DOUBLE PRECISION, POINTER :: H_result( : ) ', /,          &
              '      DOUBLE PRECISION X_int( ', i6, ') ' )
 3005 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,         &
       '      USE HSL_', A4, '_', A15, /, '      INTEGER ', 5( A6,             &
       ', ' ),  A6, /, '      INTEGER ', 5( A6, ', ' ),  A6, /,                &
       '      INTEGER ', A6, /, '      INTEGER ', 2( A6, '(', A6,              &
       '), ' ), A6, '(', A6, ')', /,                                           &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', A6, '(', A6, ')', /,                           &
              '      REAL             ', A6, '(', A6, '), ',                   &
                                         A6, '(', A6, '), ',                   &
                                         A6, '(', A6, ')', /,                  &
              'C', /, 'C  Problem name : ', A10, /,                            &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
       '      INTEGER ', 5( A6, ', ' ), A6, /, '      INTEGER ', A6, /,        &
              '      INTEGER, POINTER :: H_index( : ) ', /,                    &
              '      REAL, POINTER :: H_result( : ) ', /,                      &
              '      REAL X_int( ', i6, ') ' )
 3006 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,         &
              '      USE HSL_', A4, '_', A15, /,                               &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', A6, /,                                         &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', A6, '(', A6, ')', /,                           &
              '      DOUBLE PRECISION ', A6, '(', A6, '), ',                   &
                                         A6, '(', A6, '), ',                   &
                                         A6, '(', A6, ')', /,                  &
              'C', /, 'C  Problem name : ', A10, /,                            &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C' )
 3007 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,         &
              '      USE HSL_', A4, '_', A15, /,                               &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', A6, /,                                         &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', A6, '(', A6, ')', /,                           &
              '      REAL             ', A6, '(', A6, '), ',                   &
                                         A6, '(', A6, '), ',                   &
                                         A6, '(', A6, ')', /,                  &
              'C', /, 'C  Problem name : ', A10, /,                            &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C' )
 3008 FORMAT( '      X_AD01_int = AD01_UNDEFINED' )
!3008 format( '      call ad01_undefine( x_ad01_int ) ' )
 3009 FORMAT( '      ', A6, ' = 0' )
 3010 FORMAT( ( '      INTEGER ', A6, :, 4( ', ', A6, : ) ) )
!3011 format( '      nullify( data_ad02 )', /,
!    *        '      call ad02_initialize(ifflag-1, x_value(1),', /,
!    *        '     *          xvalue(ielvar(istaev(1)+1)),', /,
!    *        '     *                      data_ad02, 0)' )
 3011 FORMAT( '      CALL AD02_INITIALIZE_DATA(DATA_AD02, ERROR_AD02)' )
!3015 format( '       call ', a4, '_undefine( ', a6,
!    *        ', data_ad02 )' )
 3016 FORMAT( '      CALL ', A4, '_UNDEFINE( ', A6,                            &
              ', DATA_AD02 )' )
 3017 FORMAT( ( '      TYPE (', A4, '_REAL) :: ', A6 ) )
 3018 FORMAT( ( '      TYPE (AD01_REAL) :: ', A6,                              &
                ' = AD01_UNDEFINED' ) )
 3019 FORMAT( ( '      REAL             ', A6, :, 4( ', ', A6, : ) ) )
 3020 FORMAT( ( '      DOUBLE PRECISION ', A6, :, 4( ', ', A6, : ) ) )
 3021 FORMAT( ( '      INTRINSIC ', A6, :, 4( ', ', A6, : ) ) )
 3022 FORMAT( ( '      EXTERNAL ', A6, :, 4( ', ', A6, : ) ) )
 3023 FORMAT( ( '      LOGICAL ', A6, 4( :, ', ', A6 ) ) )
 3024 FORMAT( '      TYPE (AD01_REAL) :: F_value = AD01_UNDEFINED', /,         &
              '      TYPE (AD01_REAL) :: X_value(', i6, '),',                  &
                   ' X_AD01_int(',I6, ')' )
 3025 FORMAT( '      INTEGER :: ERROR_AD02', /,                                &
              '      TYPE (AD02_REAL) :: F_value', /,                          &
              '      TYPE (AD02_REAL) :: X_value(', i6, '),',                  &
                   ' X_AD02_int(',I6, ')', /,                                  &
              '      TYPE (AD02_DATA), POINTER :: DATA_AD02' )
 3030 FORMAT( '      ', A6, ' = ', A41 )
 3031 FORMAT( '      IF (', A6, ') ', A6, ' = ', A41 )
 3032 FORMAT( '      IF (.NOT.', A6, ') ', A6, ' = ', A41 )
 3040 FORMAT( '     *         ', A41 )
 3050 FORMAT( '      DO ', i5, 1X, A6, ' = 1, ', A6, /,                        &
              '       ', A6, ' = ', A6, '(', A6, ') ', /,                      &
              '       ', A6, ' = ', A6, '(', A6, ') - 1', /,                   &
              '       ', A6, ' = ', A6, '(', A6, ') - 1', /,                   &
              '       ', A6, ' = ', A6, '(', A6, ') - 1', /,                   &
              '       IF ( ', A6, ' == 3 ) ',                                  &
                      A6, ' = ', A6, '(', A6, ') - 1' )
 3051 FORMAT( '       ', A6, ' = ', A6, '(', A6, ')', /,                       &
              '       GO TO (', 8( i5, :, ',' ), /,                            &
            ( '     *        ', 8( i5, :, ',' ) ) )
 3052 FORMAT( '     *        ', 48X, '), ', A6 )
 3053 FORMAT( '       X = XVALUE(IELVAR(ILSTRT+     1))', /,                   &
              '       IF ( IFFLAG == 1 ) THEN', /,                             &
              '        FUVALS(IELEMN)= 5.0', A1, '-1 * X * X', /,              &
              '       ELSE', /,                                                &
              '        FUVALS(IGSTRT+     1)= X', /,                           &
              '        IF ( IFFLAG == 3 ) THEN', /,                            &
              '         FUVALS(IHSTRT+     1)= 1.0', A1, '+0', /,              &
              '        END IF', /,                                             &
              '       END IF' )
 3054 FORMAT( '       X = XVALUE(IELVAR(ILSTRT+     1))', /,                   &
              '       Y = XVALUE(IELVAR(ILSTRT+     2))', /,                   &
              '       IF ( IFFLAG == 1 ) THEN', /,                             &
              '        FUVALS(IELEMN)= X * Y', /,                              &
              '       ELSE', /,                                                &
              '        FUVALS(IGSTRT+     1)= Y', /,                           &
              '        FUVALS(IGSTRT+     2)= X', /,                           &
              '        IF ( IFFLAG == 3 ) THEN', /,                            &
              '         FUVALS(IHSTRT+     1)= 0.0', A1, '+0', /,              &
              '         FUVALS(IHSTRT+     2)= 1.0', A1, '+0', /,              &
              '         FUVALS(IHSTRT+     3)= 0.0', A1, '+0', /,              &
              '        END IF', /,                                             &
              '       END IF' )
 3055 FORMAT( '       X = XVALUE(IELVAR(ILSTRT+     1))', /,                   &
              '       IF ( IFFLAG == 1 ) THEN', /,                             &
              '        FUVALS(IELEMN)= 5.0', A1, '-1 * X * X', /,              &
              '       ELSE', /,                                                &
              '        WRITE(6,*) '' impossible value IFFLAG = '', ',          &
              'IFFLAG, '' in ELFUNF '' ', /,                                   &
              '       END IF' )
 3056 FORMAT( '       X = XVALUE(IELVAR(ILSTRT+     1))', /,                   &
              '       Y = XVALUE(IELVAR(ILSTRT+     2))', /,                   &
              '       IF ( IFFLAG == 1 ) THEN', /,                             &
              '        FUVALS(IELEMN)= X * Y', /,                              &
              '       ELSE', /,                                                &
              '        WRITE(6,*) '' impossible value IFFLAG = '', ',          &
              'IFFLAG, '' in ELFUNF '' ', /,                                   &
              '       END IF' )
 3057 FORMAT( '       ', A6, ' = XVALUE(IELVAR(ILSTRT+     1))', /,            &
              '       IF ( IFFLAG == 1 ) THEN', /,                             &
              '        FUVALS(IELEMN)= 5.0', A1, '-1 * ', A6,                  &
              ' * ', A6, /,                                                    &
              '       ELSE', /,                                                &
              '        FUVALS(IGSTRT+     1)= ', A6, /,                        &
              '        IF ( IFFLAG == 3 ) THEN', /,                            &
              '         FUVALS(IHSTRT+     1)= 1.0', A1, '+0', /,              &
              '        END IF', /,                                             &
              '       END IF' )
 3058 FORMAT( '       ', A6, ' = XVALUE(IELVAR(ILSTRT+     1))', /,            &
              '       ', A6, ' = XVALUE(IELVAR(ILSTRT+     2))', /,            &
              '       IF ( IFFLAG == 1 ) THEN', /,                             &
              '        FUVALS(IELEMN)= ', A6, ' * ', A6, /,                    &
              '       ELSE', /,                                                &
              '        FUVALS(IGSTRT+     1)= ', A6, /,                        &
              '        FUVALS(IGSTRT+     2)= ', A6, /,                        &
              '        IF ( IFFLAG == 3 ) THEN', /,                            &
              '         FUVALS(IHSTRT+     1)= 0.0', A1, '+0', /,              &
              '         FUVALS(IHSTRT+     2)= 1.0', A1, '+0', /,              &
              '         FUVALS(IHSTRT+     3)= 0.0', A1, '+0', /,              &
              '        END IF', /,                                             &
              '       END IF' )
 3060 FORMAT( 'C', /, 'C  ELEMENT TYPE : ', A10, /, 'C' )
 3061 FORMAT( i5, '  CONTINUE' )
 3070 FORMAT( '       ', A6, ' = ', A6, '(', A6, '(', A6, '+', i6, '))')
 3071 FORMAT( '       ', A6, ' = ', A6, '(', A6, '+', i6, ')')
 3080 FORMAT( '       ', A6, ' = ', A41 )
 3081 FORMAT( '       IF (', A6, ') ', A6, ' = ', A41 )
 3082 FORMAT( '       IF (.NOT.', A6, ') ', A6, '=', A41 )
 3083 FORMAT( '        ', A6, ' = ', A41 )
 3084 FORMAT( '        IF (', A6, ') ', A6, ' = ', A41 )
 3085 FORMAT( '        IF (.NOT.', A6, ')', A6, '=', A41 )
 3090 FORMAT( '     *          ', A41 )
 3091 FORMAT( '     *           ', A41 )
 3100 FORMAT( '       IF ( ', A6, ' == 1 ) THEN', /,                           &
              '        ', A6, '(', A6, ')= ', A41 )
 3101 FORMAT( '       F_value = ', A41 )
 3110 FORMAT( '     *                  ', A41 )
 3120 FORMAT( '       ELSE' )
 3121 FORMAT( '        WRITE(6,*) '' impossible value IFFLAG = '', ',          &
              'IFFLAG, '' in ELFUNF '' ' )
 3122 FORMAT( '       IF ( ', A6, ' == 1 ) THEN', /,                           &
              '        CALL AD01_VALUE(F_value, ', A6, '(', A6, '))', /,       &
              '       ELSE',/,                                                 &
              '        CALL AD01_GRAD(F_value, ', A6, '(', A6, '+1:',          &
              A6, '+', i6, '))' )
 3123 FORMAT( '       IF ( ', A6, ' == 1 ) THEN', /,                           &
              '        CALL AD02_VALUE(F_value, ', A6, '(', A6, '),',          &
              ' ERROR_AD02)', /,                                               &
              '       ELSE',/,                                                 &
              '        CALL AD02_GRAD(F_value, ', A6, '(', A6, '+1:',          &
              A6, '+', i6, '),', /,                                            &
              '     *                 ERROR_AD02)' )
 3150 FORMAT( '        IF ( ', A6, ' == 3 ) THEN' )
 3151 FORMAT( '         CALL AD01_DERIVS(F_value, 2,',                         &
              ' H_index, H_result)' )
 3152 FORMAT( '         CALL AD02_DERIVS(F_value, 2,',                         &
              ' H_index, H_result, ERROR_AD02)' )
 3163 FORMAT( '         ', A6, '(', A6, '+', i6, ')=2.0*H_result(',            &
              i6, ')')
 3164 FORMAT( '         ', A6, '(', A6, '+', i6, ')=H_result(', i6, ')')
 3180 FORMAT( '        END IF' )
 3190 FORMAT( '       END IF' )
 3191 FORMAT( '       GO TO', i6 )
 3200 FORMAT( i5,  ' CONTINUE', /, '      RETURN', /,                          &
              '      END' )
 3201 FORMAT( '      RETURN', /,                                               &
              '      END' )
 3202 FORMAT( i5,  ' CONTINUE', /,                                             &
              '      CALL AD02_FINALIZE_DATA(DATA_AD02, ERROR_AD02)', /,       &
              '      RETURN', /,   '      END' )
 3210 FORMAT( '       CALL AD01_INITIALIZE(',A6,' - 1, X_value(:', i6,         &
                     '),', /, '     *', 22X,                                   &
                     A6,'(',A6,'(',A6,'+1:',A6,'+', i6, ')), 0) ' )
 3211 FORMAT( '       CALL AD02_INITIALIZE_COMP(',A6,' - 1, X_value(:',        &
                     i6, '),', /, '     *', 22X,                               &
                     A6,'(',A6,'(',A6,'+1:',A6,'+', i6, ')),', /,              &
              '     *                      DATA_AD02, ERROR_AD02, 0)' )
 3220 FORMAT( '       ', A6, ' = X_value(', i6, ')' )
 3230 FORMAT( '       CALL AD01_INITIALIZE(0, X_value(:', i6,                  &
                     '),', /, '     *', 22X,                                   &
                     A6,'(',A6,'(',A6,'+1:',A6,'+', i6, ')), 0) ' )
 3231 FORMAT( '       CALL AD02_INITIALIZE_COMP(0, X_value(:', i6,             &
                     '),', /, '     *', 22X,                                   &
                     A6,'(',A6,'(',A6,'+1:',A6,'+', i6, ')),', /,              &
              '     *                      DATA_AD02, ERROR_AD02, 0)' )
 4000 FORMAT( '      SUBROUTINE RANGE( IELEMN, TRANSP, W1,',                   &
              ' W2, nelvar, ninvar,', /,                                       &
              '     *                  itype, LW1, LW2 )', /,                  &
              '      INTEGER IELEMN, nelvar, ninvar, itype,',                  &
              ' LW1, LW2', /,                                                  &
              '      LOGICAL TRANSP', /,                                       &
              '      DOUBLE PRECISION W1( LW1 ), W2( LW2 )', /,                &
              'C', /, 'C  Problem name : ', A10, /,                            &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
              'C  TRANSP = .FALSE. <=> W2 = U * W1', /,                        &
              'C  TRANSP = .TRUE.  <=> W2 = U(transpose) * W1', /,             &
              'C', /,                                                          &
              '      INTEGER I' )
 4001 FORMAT( '      SUBROUTINE RANGE( IELEMN, TRANSP, W1,',                   &
              ' W2, nelvar, ninvar,', /,                                       &
              '     *                  itype, LW1, LW2 )', /,                  &
              '      INTEGER IELEMN, nelvar, ninvar, itype,',                  &
              ' LW1, LW2', /,                                                  &
              '      LOGICAL TRANSP', /,                                       &
              '      REAL             W1( LW1 ), W2( LW2 )', /,                &
              'C', /, 'C  Problem name : ', A10, /,                            &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
              'C  TRANSP = .FALSE. <=> W2 = U * W1', /,                        &
              'C  TRANSP = .TRUE.  <=> W2 = U(transpose) * W1', /,             &
              'C', /,                                                          &
              '      INTEGER I' )
 4002 FORMAT( '      SUBROUTINE RANGE( IELEMN, TRANSP, W1,',                   &
              ' W2, nelvar, ninvar,', /,                                       &
              '     *                  itype, LW1, LW2 )', /,                  &
              '      INTEGER IELEMN, nelvar, ninvar, itype,',                  &
              ' LW1, LW2', /,                                                  &
              '      LOGICAL TRANSP', /,                                       &
              '      DOUBLE PRECISION W1( LW1 ), W2( LW2 )', /,                &
              'C', /, 'C  Problem name : ', A10, /,                            &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
              'C  TRANSP = .FALSE. <=> W2 = U * W1', /,                        &
              'C  TRANSP = .TRUE.  <=> W2 = U(transpose) * W1', /,             &
              'C' )
 4003 FORMAT( '      SUBROUTINE RANGE( IELEMN, TRANSP, W1,',                   &
              ' W2, nelvar, ninvar,', /,                                       &
              '     *                  itype, LW1, LW2 )', /,                  &
              '      INTEGER IELEMN, nelvar, ninvar, itype,',                  &
              ' LW1, LW2', /,                                                  &
              '      LOGICAL TRANSP', /,                                       &
              '      REAL             W1( LW1 ), W2( LW2 )', /,                &
              'C', /, 'C  Problem name : ', A10, /,                            &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
              'C  TRANSP = .FALSE. <=> W2 = U * W1', /,                        &
              'C  TRANSP = .TRUE.  <=> W2 = U(transpose) * W1', /,             &
              'C' )
 4040 FORMAT( '      GO TO (', 8( i5, :, ',' ), /,                             &
            ( '     *       ', 8( i5, :, ',' ) ) )
 4050 FORMAT( '     *        ', 48X, '), ITYPE' )
 4060 FORMAT( 'C', /, 'C  Element type : ', A10, /, 'C', /,                    &
              i5, ' CONTINUE', /,                                              &
              '      IF ( TRANSP ) THEN' )
 4070 FORMAT( 'C', /,                                                          &
              'C  Elements without internal variables.', /,                    &
              'C', /,                                                          &
              '99998 CONTINUE', /,                                             &
              '      DO 99999 i = 1, nelvar', /,                               &
              '         W2( i ) = W1( i )', /,                                 &
              '99999 CONTINUE', /,                                             &
              '      RETURN' )
 4080 FORMAT( '      RETURN' )
 4090 FORMAT( '      END' )

!  end of subroutine MAKE_elfun_ad

      END SUBROUTINE MAKE_elfun_ad

!-*-*-*-  S I F D E C O D E   M A K E _ g r o u p   S U B R O U T I N E  -*-*-*-

      SUBROUTINE MAKE_group( input, out, outgr, status, ngtype, ngpnames,      &
                             pname, GANAMES,                                   &
                             len_renames, RENAMES, len_innames, INNAMES,       &
                             len_lonames, LONAMES, len_minames, MINAMES,       &
                             len_exnames, EXNAMES, GPNAMES, DEFINED,           &
                             GTYPES, GTYPESP_ptr, debug, length,               &
                             TABLE, KEY, INLIST, single, nuline, gotlin,       &
                             print_level )
      INTEGER :: input, out, outgr, status, length, print_level
      INTEGER :: ngtype, ngpnames
      INTEGER :: len_renames, len_innames, len_lonames, len_minames, len_exnames
      LOGICAL :: gotlin, debug, single
      CHARACTER ( LEN = 10 ) :: pname
      CHARACTER ( LEN = max_record_length ) :: nuline
      INTEGER :: GTYPESP_ptr( ngtype + 1 )
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TABLE, INLIST
      LOGICAL, DIMENSION( ngtype ) :: DEFINED
      CHARACTER ( LEN = 10 ) :: GTYPES( ngtype )
      CHARACTER ( LEN = 10 ) :: GANAMES( ngtype ), GPNAMES( ngpnames )
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: RENAMES, INNAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: LONAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: MINAMES, EXNAMES
      CHARACTER ( LEN = 12 ), ALLOCATABLE, DIMENSION( : ) :: KEY

!  -------------------------------------------------------------------
!  make a group function evaluation subroutine from a gps group
!  function data file

!  function indicator cards
!  -------------------------

!  definition   purpose
!  ----------   --------
!  GROUPS       problem name
!  TEMPORARIES  names of additional parameters used in function defs
!  GLOBALS      general parameter assignments
!  INDIVIDUALS  set function and derivative values for each group-type
!  ENDATA       end of input data

!  data card description
!  ----------------------

!  see 'The SIF reference report', Chapter 7 in
!       A. R. Conn, N. I. M. Gould and Ph. L. Toint,
!       LANCELOT A Fortran Package for Large-Scale Nonlinear Optimization
!       (RElease A), Springer Series in Computational Mathematics 17,
!       Springer Verlag 1992

!  see also http://www.cuter.rl.ac.uk/sifdec/Doc/sif.pdf
!  and      http://www.numerical.rl.ac.uk/lancelot/sif/sifhtml.html

!  returns with negative values of status indicate that insufficient
!  array space has been allowed, as follows:

!    status = - 1  when length not large enough
!    status = - 2  when RENAMES, INNAMES, LONAMES, MINAMES or EXNAMES cannot be
!                  extended further
!  -------------------------------------------------------------------

!  parameter definitions

      INTEGER, PARAMETER :: mblank = 1, mfixed = 2, mfree = 3, mname = 4
      INTEGER, PARAMETER :: mtemp = 5, mglob = 6, mindiv = 7, mendat = 8
      INTEGER, PARAMETER :: iires = 20
      INTEGER, PARAMETER :: maxnul = 20
      INTEGER, DIMENSION( mendat ), PARAMETER :: LENIND                        &
        = (/ 0, 12, 11, 6, 11, 7, 11, 6 /)
      CHARACTER ( LEN = 12 ), DIMENSION( mendat ), PARAMETER :: INDIC8         &
        = (/ '            ', 'FIXED FORMAT', 'FREE FORMAT ', 'GROUPS      ',   &
             'TEMPORARIES ', 'GLOBALS     ', 'INDIVIDUALS ', 'ENDATA      '  /)
      CHARACTER ( LEN = 8 ), DIMENSION( iires ), PARAMETER :: FIELDI           &
        = (/ 'GROUP   ', 'GVALUE  ', 'LGVALU  ', 'FVALUE  ', 'NCALCG  ',       &
             'ITYPEG  ', 'ICALCG  ', 'DERIVS  ', 'IGRTYP  ', 'IGROUP  ',       &
             'GPVALU  ', 'ISTGPA  ', 'IPSTRT  ', 'JCALCG  ', 'LTYPEG  ',       &
             'LSTGPA  ', 'LCALCG  ', 'LFVALU  ', 'LGPVLU  ', 'IGSTAT  ' /)

!  local variables

      INTEGER :: i, ifield, ifree, itype, intype, ivar, k1, k2
      INTEGER :: ninnames, nloop, nrenames, nminames, npname
      INTEGER :: nlonames, nexnames, lineno, k, ilines, nlines
      INTEGER :: used_length, new_length, min_length, alloc_status
      LOGICAL :: defnam, endpar, endgen, firstg
      LOGICAL :: setf, setg, seth, startp, fixed, endf
      CHARACTER ( LEN = 2 ) :: field1
      CHARACTER ( LEN = 8 ) :: field2, field3
      CHARACTER ( LEN = 12 ) :: field, header
      CHARACTER ( LEN = 24 ) :: bad_alloc
      CHARACTER ( LEN = 41 ) :: field7
      CHARACTER ( LEN = max_record_length ) :: blnkln
      CHARACTER ( LEN = 65 ), DIMENSION( maxnul ) :: NULINA

      IF ( out > 0 ) WRITE( out, 2900 )

!  set initial values for integer variables

      ninnames = 0 ; nrenames = 0 ; nlonames = 0 ; nexnames = 0 ; nminames = 0
      lineno = 0 ; intype = 1 ; ilines = 0 ; nlines = 0

!  set initial values for logical variables

      defnam = .FALSE. ; endpar = .FALSE. ; seth = .FALSE. ; startp = .FALSE.
      endgen = .FALSE. ; firstg = .TRUE. ; fixed = .TRUE.

!  find which group-types are nontrivial

      DEFINED( : ngtype ) = .FALSE.

!  insert the list of group-type arguments into the dictionary

      DO itype = 1, ngtype
        field = GANAMES( itype ) // 'PG'
        CALL HASH_enlarge_and_insert( length, 12, field,                       &
                                      TABLE, KEY, INLIST, ifree )
        IF ( ifree <= 0 ) THEN
          IF ( ifree == 0 ) THEN
            status = - 1
            GO TO 700
          END IF
        ELSE
          nrenames = nrenames + 1
          IF ( nrenames > len_renames ) THEN
            used_length = nrenames - 1 ; min_length = nrenames
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( RENAMES, len_renames, used_length,              &
                               new_length, min_length, buffer,                 &
                               status, alloc_status, 'RENAMES' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'RENAMES' ; status = - 2 ; GO TO 980
            END IF
            len_renames = new_length
          END IF
          RENAMES( nrenames ) = GANAMES( itype )
        END IF
      END DO

!  include the names of the group parameters used
!  in this dictionary

      IF ( ngtype > 0 ) THEN
        npname = GTYPESP_ptr( ngtype + 1 ) - 1
        DO i = 1, npname
          field = GPNAMES( i ) // 'PG'
          CALL HASH_enlarge_and_insert( length, 12, field,                     &
                                        TABLE, KEY, INLIST, ifree )
          IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
              status = - 1
              GO TO 700
            END IF
          ELSE
            IF ( nrenames > len_renames ) THEN
              used_length = nrenames - 1 ; min_length = nrenames
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( RENAMES, len_renames, used_length,            &
                                 new_length, min_length, buffer,               &
                                 status, alloc_status, 'RENAMES' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'RENAMES' ; status = - 2 ; GO TO 980
              END IF
              len_renames = new_length
            END IF
            nrenames = nrenames + 1
            RENAMES( nrenames ) = GPNAMES( i )
          END IF
        END DO
      END IF

!  set a blank line

      DO i = 1, max_record_length
        BLNKLN( i : i ) = ' '
      END DO

!  read next line

  100 CONTINUE

!  read next line from the input file

      IF ( ilines + 1 > nlines ) THEN
        lineno = lineno + 1
        IF ( fixed ) THEN
          IF ( gotlin ) THEN
            gotlin = .FALSE.
          ELSE
            nuline = blnkln
            READ ( input, 1000, END = 590, ERR = 590 ) nuline
          END IF
          IF ( out > 0 .AND. debug ) WRITE( out, 2990 ) lineno, nuline
        ELSE
          IF ( gotlin ) THEN
            gotlin = .FALSE.
          ELSE
            nuline = blnkln
            READ ( input, 1010, END = 590, ERR = 590 ) nuline
          END IF
          IF ( out > 0 .AND. debug ) WRITE( out, 2970 ) lineno, nuline

!  if the card is in free format, translate it into fixed format

          CALL FREE_format( nuline, max_record_length, mendat, INDIC8,         &
                            LENIND, NULINA, maxnul, nlines, .FALSE.,           &
                            status, out )
          IF ( status > 0 ) GO TO 800

!  if there are non-blank lines on the free format card, read the first

          IF ( nlines > 0 ) THEN
            ilines = 1
            nuline = blnkln
            nuline = NULINA( ilines )
            IF ( out > 0 .AND. debug ) WRITE( out, 2980 ) lineno, ilines, nuline

!  there are only blank lines on the free format card

          ELSE
            GO TO 100
          END IF
        END IF

!  read next line from the last encountered free format card

      ELSE
        ilines = ilines + 1
        nuline = blnkln
        nuline = NULINA( ilines )
        IF ( out > 0 .AND. debug ) WRITE( out, 2980 ) lineno, ilines, nuline
      END IF

!  consider the header part of the card

      header = NULINE( 1 : 12 )

!  ignore blank lines

      IF ( header == INDIC8( mblank ) ) GO TO 100
      IF ( NULINE( 1 : 1 ) /= ' ' ) THEN

!  ignore comment cards

        IF ( NULINE( 1 : 1 ) == '*' ) GO TO 100

!  check if we have entered fixed-format input

        IF ( header == INDIC8( mfixed ) ) THEN
          fixed = .TRUE.
          GO TO 100
        END IF

!  check if we have entered free-format input

        IF ( header == INDIC8( mfree ) ) THEN
          fixed = .FALSE.
          GO TO 100
        END IF

!  check that the first encountered indicator card is the groups card

        IF ( .NOT. defnam  ) THEN
          IF ( header /= INDIC8( mname ) ) THEN
            IF ( ngtype > 0 ) GO TO 930
            IF ( out > 0 .AND. print_level /= 0 ) WRITE( out, 2010)
            gotlin = .TRUE.
            GO TO 600

!  indicator card is groups
!  -------------------------

          ELSE
             IF ( pname  /= NULINE( 15 : 24 ) ) THEN
               status = 51
               IF ( out > 0 ) WRITE( out, 2510 )
               GO TO 800
             ELSE
               defnam = .TRUE.
               GO TO 100
             END IF
          END IF
        END IF

!  an indicator card has been found

        DO i = intype, mendat
          IF ( header == INDIC8( i ) ) THEN
            intype = i
            GO TO 120
          END IF
        END DO

!  the indicator card is not recognised

        status = 2
        IF ( out > 0 ) WRITE( out, 2020 )
        GO TO 800

!  the parameter values have been completed. write out the
!  first part of the generated subroutine

  120   CONTINUE
        IF ( intype >= mglob .AND. .NOT. endpar ) THEN
          endpar = .TRUE.
          nloop = ngtype + 1

!  insert the list of reserved integer/real/logical variables into
!  the dictionary

          DO i = 1, iires
            field = FIELDI( i ) // '  PG'
            CALL HASH_enlarge_and_insert( length, 12, field,                   &
                                          TABLE, KEY, INLIST, ifree )
            IF ( ifree <= 0 ) THEN
              IF ( ifree == 0 ) THEN
                status = - 1
                GO TO 700
              END IF
              status = 59
              IF ( out > 0 ) WRITE( out, 2590 ) FIELDI( i )
              GO TO 800
            END IF
          END DO

!  -------- set up subroutine call and reserved parameter declarations

          IF ( single ) THEN
            WRITE( outgr, 3001 ) ( FIELDI( i )( 1 : 6 ), i = 1, 4 ),           &
                      FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),            &
                      FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),            &
                      FIELDI(  7 )( 1 : 6 ),                                   &
                    ( FIELDI(  i )( 1 : 6 ), i = 15, 19 ),                     &
                      FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),            &
                      FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),            &
                    ( FIELDI(  i )( 1 : 6 ), i = 15, 20 ),                     &
                      FIELDI(  8 )( 1 : 6 ),                                   &
                      FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),            &
                      FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),            &
                      FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),            &
                      FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),            &
                      FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),            &
                      FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),            &
                      pname, TRIM( version )
          ELSE
            WRITE( outgr, 3000 ) ( FIELDI( i )( 1 : 6 ), i = 1, 4 ),           &
                      FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),            &
                      FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),            &
                      FIELDI(  7 )( 1 : 6 ),                                   &
                    ( FIELDI(  i )( 1 : 6 ), i = 15, 19 ),                     &
                      FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),            &
                      FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),            &
                    ( FIELDI(  i )( 1 : 6 ), i = 15, 20 ),                     &
                      FIELDI(  8 )( 1 : 6 ),                                   &
                      FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),            &
                      FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),            &
                      FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),            &
                      FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),            &
                      FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),            &
                      FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),            &
                      pname, TRIM( version )
          END IF
          IF ( ngtype == 0 ) THEN
            WRITE( outgr, 3009 ) FIELDI( 20 )( 1 : 6 )
            GO TO 910
          END IF
          WRITE( outgr, 3002 ) FIELDI(  9 )( 1 : 6 ),                          &
                        FIELDI( 10 )( 1 : 6 ), FIELDI( 13 )( 1 : 6 ),          &
                        FIELDI( 14 )( 1 : 6 )

! --------- insert integer declarations

          IF ( ninnames > 0 )                                                  &
            WRITE( outgr, 3010 ) ( INNAMES( i ), i = 1, ninnames )

! --------- insert real declarations

          IF ( nrenames > 0 ) THEN
            IF ( single ) THEN
              WRITE( outgr, 3019 ) ( RENAMES( i ), i = 1, nrenames )
            ELSE
              WRITE( outgr, 3020 ) ( RENAMES( i ), i = 1, nrenames )
            END IF
          END IF

! --------- insert logical declarations

          IF ( nlonames > 0 )                                                  &
            WRITE( outgr, 3023 ) ( LONAMES( i ), i = 1, nlonames )

! --------- insert intrinsic declarations

          IF ( nminames > 0 )                                                  &
            WRITE( outgr, 3021 ) ( MINAMES( i ), i = 1, nminames )

! --------- insert external declarations

          IF ( nexnames > 0 )                                                  &
            WRITE( outgr, 3022 ) ( EXNAMES( i ), i = 1, nexnames )
          WRITE( outgr, 3009 ) FIELDI( 20 )( 1 : 6 )
        END IF

!  the general parameter assignments have been completed
!  continue with the construction of the generated subroutine

        IF ( intype >= mindiv .AND. .NOT. endgen ) THEN
          endgen = .TRUE.

! --------- start loop over groups

          WRITE( outgr, 3050 ) nloop,  FIELDI( 14 )( 1 : 6 ),                  &
                   FIELDI(  5 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),               &
                   FIELDI(  7 )( 1 : 6 ), FIELDI( 14 )( 1 : 6 ),               &
                   FIELDI(  9 )( 1 : 6 ),                                      &
                   FIELDI(  6 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),               &
                   FIELDI(  9 )( 1 : 6 ), nloop,                               &
                   FIELDI( 13 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),               &
                   FIELDI( 10 )( 1 : 6 )
          IF ( ngtype > 1 ) THEN
            WRITE( outgr, 3051 ) ( i, i = 1, ngtype )
            WRITE( outgr, 3052 ) FIELDI(  9 )( 1 : 6 )
          END IF
        END IF

!  indicator card is endata
!  -------------------------

        IF ( intype == mendat ) GO TO 900
        GO TO 100

!  check that the first non comment card is the groups indicator card

      ELSE
        IF ( .NOT. defnam  ) THEN
          IF ( ngtype > 0 ) GO TO 930
          IF ( out > 0 .AND. print_level /= 0 ) WRITE( out, 2010 )
          gotlin = .TRUE.
          GO TO 600
        END IF

!  a data card has been found
!  read the character fields 1, 2, 3 and 7 from the card

        field1 = NULINE(  2 :  3 )
        field2 = NULINE(  5 : 12 )
        field3 = NULINE( 15 : 22 )
        field7 = NULINE( 25 : 65 )

!  check that field3 is blank on 'a', 'f' and 'g' cards

        IF ( field1( 1 : 1 ) == 'A' .OR. field1( 1 : 1 ) == 'F' .OR.           &
            field1( 1 : 1 ) == 'G' .OR. field1( 1 : 1 ) == 'H' ) THEN
          IF ( ( field1( 1 : 1 ) /= 'A' .AND. field2 /=                        &
              '       ' ) .OR.FIELD3 /= '       ' ) THEN
            status = 73
            IF ( out > 0 ) WRITE( out, 2730 )
            GO TO 800
          END IF
        END IF
      END IF

!  branch on the value of intype

      GO TO ( 100, 100, 100, 100, 290, 300, 400, 900 ), intype

!  indicator card is temporaries
!  ------------------------------

  290 CONTINUE

!  check to see if the parameter is integer, real, logical or a function

      IF ( field1 /= 'I ' .AND. field1 /= 'R ' .AND. field1 /= 'M ' .AND.      &
           field1 /= 'F ' .AND. field1 /= 'L' ) THEN
        status = 54
        IF ( out > 0 ) WRITE( out, 2540 )
        GO TO 800
      END IF

!  if the parameter is a function, check to see that the name has
!  not already been used

      IF ( field1 == 'F ' ) THEN
        field = field2 // '  GU'
        CALL HASH_enlarge_and_insert( length, 12, field,                       &
                                      TABLE, KEY, INLIST, ifree )
        IF ( ifree <= 0 ) THEN
          IF ( ifree == 0 ) THEN
            status = - 1
            GO TO 700
          END IF
        ELSE
           nexnames = nexnames + 1
           IF ( nrenames > len_renames ) THEN
             used_length = nexnames - 1 ; min_length = nexnames
             new_length = increase_n * min_length / increase_d + 1
             CALL EXTEND_array( EXNAMES, len_exnames, used_length,             &
                                new_length, min_length, buffer,                &
                                status, alloc_status, 'EXNAMES' )
             IF ( status /= 0 ) THEN
               bad_alloc = 'EXNAMES' ; status = - 2 ; GO TO 980 ; END IF
             len_exnames = new_length
           END IF
           EXNAMES( nexnames ) = field2
        END IF

!  check to see that the parameter name has not already been used

      ELSE
        field = field2 // '  PG'
        CALL HASH_enlarge_and_insert( length, 12, field,                       &
                                      TABLE, KEY, INLIST, ifree )
        IF ( ifree <= 0 ) THEN
          IF ( ifree == 0 ) THEN
            status = - 1
            GO TO 700
          END IF
        ELSE
          IF ( field1 == 'R ' ) THEN
            nrenames = nrenames + 1
            IF ( nrenames > len_renames ) THEN
              used_length = nrenames - 1 ; min_length = nrenames
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( RENAMES, len_renames, used_length,            &
                                 new_length, min_length, buffer,               &
                                 status, alloc_status, 'RENAMES' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'RENAMES' ; status = - 2 ; GO TO 980 ; END IF
              len_renames = new_length
            END IF
            RENAMES( nrenames ) = field2
          ELSE IF ( field1 == 'M ' ) THEN
            nminames = nminames + 1
            IF ( nminames > len_minames ) THEN
              used_length = nminames - 1 ; min_length = nminames
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( MINAMES, len_minames, used_length,            &
                                 new_length, min_length, buffer,               &
                                 status, alloc_status, 'MINAMES' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'MINAMES' ; status = - 2 ; GO TO 980 ; END IF
              len_minames = new_length
            END IF
            MINAMES( nminames ) = field2
          ELSE IF ( field1 == 'L ' ) THEN
            nlonames = nlonames + 1
            IF ( nlonames > len_lonames ) THEN
              used_length = nlonames - 1 ; min_length = nlonames
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( LONAMES, len_lonames, used_length,            &
                                 new_length, min_length, buffer,               &
                                 status, alloc_status, 'LONAMES' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'LONAMES' ; status = - 2 ; GO TO 980 ; END IF
              len_lonames = new_length
            END IF
            LONAMES( nlonames ) = field2
          ELSE
            ninnames = ninnames + 1
            IF ( ninnames > len_innames ) THEN
              used_length = ninnames - 1 ; min_length = ninnames
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( INNAMES, len_innames, used_length,            &
                                 new_length, min_length, buffer,               &
                                 status, alloc_status, 'INNAMES' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'INNAMES' ; status = - 2 ; GO TO 980 ; END IF
            END IF
            INNAMES( ninnames ) = field2
          END IF
        END IF
      END IF
      GO TO 100

!  indicator card is global
!  -------------------------

  300 CONTINUE
      IF ( field1 == 'A ' .OR. field1 == 'I ' .OR. field1 == 'E ' ) THEN
        startp = .TRUE.

!  start a parameter assignment. check to see that the parameter has
!  been defined

        field = field2 // '  PG'
        CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
        IF ( ifield <= 0 ) THEN
          IF ( ifree == 0 ) THEN
            status = - 1
            GO TO 700
          END IF
          status = 57
          IF ( out > 0 ) WRITE( out, 2570 )
          GO TO 800
        END IF

! --------- make general parameter assignments

        IF ( field1 == 'A ' ) THEN
          WRITE( outgr, 3030 ) FIELD2( 1 : 6 ), field7

! --------- make conditional parameter assignments

        ELSE

!  check that the logical variable has been defined

          field = field3 // '  PG'
          CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
          IF ( ifield <= 0 ) THEN
            IF ( ifree == 0 ) THEN
              status = - 1
              GO TO 700
            END IF
            status = 57
            IF ( out > 0 ) WRITE( out, 2570 )
            GO TO 800
          END IF
          IF ( field1 == 'I ' ) THEN
            WRITE( outgr, 3031 ) FIELD2( 1 : 6 ), FIELD3( 1 : 6 ), field7
          ELSE
            WRITE( outgr, 3032 ) FIELD2( 1 : 6 ), FIELD3( 1 : 6 ), field7
          END IF
        END IF

! --------- continue a parameter assignment

      ELSE
        IF ( field1( 2 : 2 ) == '+' .AND. startp ) THEN
          WRITE( outgr, 3040 ) field7
        ELSE
          status = 55
          IF ( out > 0 ) WRITE( out, 2550 )
          GO TO 800
        END IF
      END IF
      GO TO 100

!  indicator card is individuals
!  ------------------------------

  400 CONTINUE

!  check if a new group has been encountered

      IF ( field1 == 'T ' ) THEN
        IF ( firstg ) THEN

!  check if this is the first group-type

          firstg = .FALSE.

!  finish of the previous group, if any

        ELSE
          IF ( .NOT. seth ) THEN
            status = 63
            IF ( out > 0 ) WRITE( out, 2630 )
            GO TO 800
          END IF
          IF ( .NOT. setg ) THEN
            status = 62
            IF ( out > 0 ) WRITE( out, 2620 )
            GO TO 800
          END IF

! ---------- wind up f and g

          IF ( setf ) THEN
            WRITE( outgr, 3190 )
          ELSE
            status = 61
            IF ( out > 0 ) WRITE( out, 2610 )
            GO TO 800
          END IF
          IF ( itype < ngtype ) WRITE( outgr, 3191 ) nloop
        END IF

!  find itype, the group-type

        field = field2 // '  GT'
        CALL HASH_search( length, 12, field, TABLE, KEY, ifield )

!  the group-type is unknown

        IF ( ifield <= 0 ) THEN
          status = 19
          IF ( out > 0 ) WRITE( out, 2190 )
          GO TO 800
        END IF

! --------- find type of current group

        itype = INLIST( ifield )
        WRITE( outgr, 3060 ) field2
        IF ( ngtype > 1 ) WRITE( outgr, 3061 ) itype
        WRITE( outgr, 3062 ) GANAMES( itype )( 1 : 6 ),                        &
                             FIELDI(  4 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )

! --------- set group parameters

        k1 = GTYPESP_ptr( itype ) ; k2 = GTYPESP_ptr( itype + 1 ) - 1
        DO k = k1, k2
          ivar = k - k1 + 1
          WRITE( outgr, 3063 ) GPNAMES( k ), FIELDI( 11 )( 1 : 6 ),            &
                              FIELDI( 13 )( 1 : 6 ), ivar
        END DO
        IF ( DEFINED( itype ) ) THEN
          status = 64
          IF ( out > 0 ) WRITE( out, 2640 )
          GO TO 800
        ELSE
          DEFINED( itype ) = .TRUE.
        END IF

!  initialize logicals which determine whether the data has been
!  input in the correct order

         startp = .FALSE. ; endf = .TRUE.
         setf = .FALSE. ; setg = .FALSE. ; seth = .FALSE.
      ELSE
        IF ( field1( 1 : 1 ) == 'A' .OR. field1( 1 : 1 ) == 'I' .OR.           &
                field1( 1 : 1 ) == 'E' ) THEN
          IF ( setf ) THEN
            IF ( .NOT. endf ) THEN
              WRITE( outgr, 3120 )
              endf = .TRUE.
            END IF
          END IF

!  start a parameter assignment. check to see that the parameter has
!  been defined

          IF ( field1( 2 : 2 ) == ' ' ) THEN
            startp = .TRUE.
            field = field2 // '  PG'
            CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
            IF ( ifield <= 0 ) THEN
              status = 57
              IF ( out > 0 ) WRITE( out, 2570 )
              GO TO 800
            END IF

! --------- make group-specific parameter assignments

            IF ( field1( 1 : 1 ) == 'A' ) THEN
              IF ( .NOT. setf ) THEN
                WRITE( outgr, 3080 ) FIELD2( 1 : 6 ), field7
              ELSE
                WRITE( outgr, 3083 ) FIELD2( 1 : 6 ), field7
              END IF

! --------- make conditional parameter assignments

            ELSE

!  check that the logical variable has been defined

              field = field3 // '  PG'
              CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
              IF ( ifield <= 0 ) THEN
                IF ( ifree == 0 ) THEN
                  status = - 1
                  GO TO 700
                END IF
                status = 58
                IF ( out > 0 ) WRITE( out, 2580 )
                GO TO 800
              END IF
              IF ( field1( 1 : 1 ) == 'I' ) THEN
                IF ( .NOT. setf ) THEN
                  WRITE( outgr, 3081 ) FIELD2( 1 : 6 ),                        &
                                     FIELD3( 1 : 6 ), field7
                ELSE
                  WRITE( outgr, 3084 ) FIELD2( 1 : 6 ),                        &
                                     FIELD3( 1 : 6 ), field7
                END IF
              ELSE
                IF ( .NOT. setf ) THEN
                  WRITE( outgr, 3082 ) FIELD2( 1 : 6 ),                        &
                                     FIELD3( 1 : 6 ), field7
                ELSE
                   WRITE( outgr, 3085 ) FIELD2( 1 : 6 ),                       &
                                      FIELD3( 1 : 6 ), field7
                END IF
              END IF
            END IF

! --------- continuation of a parameter assignment

          ELSE
            IF ( field1( 2 : 2 ) == '+' ) THEN
              IF ( startp ) THEN
                IF ( .NOT. setf ) THEN
                  WRITE( outgr, 3090 ) field7
                ELSE
                  WRITE( outgr, 3091 ) field7
                END IF
              ELSE
                status = 56
                IF ( out > 0 ) WRITE( out, 2560 )
                GO TO 800
              END IF
            END IF
          END IF

!  set the function value

        ELSE
          startp = .FALSE.
          IF ( field1( 1 : 1 ) == 'F' ) THEN
            IF ( field1( 2 : 2 ) == ' ' ) THEN
              setf = .TRUE.
              endf = .FALSE.

! --------- start g

              WRITE( outgr, 3100 ) FIELDI(  8 )( 1 : 6 ),                      &
                FIELDI( 2 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ), field7

! --------- continuation of g

            ELSE
              IF ( field1( 2 : 2 ) == '+' ) THEN
                IF ( setf ) THEN
                  WRITE( outgr, 3110 ) field7
                ELSE
                  status = 56
                  IF ( out > 0 ) WRITE( out, 2560 )
                  GO TO 800
                END IF
              END IF
            END IF

!  no function value has been specified

          ELSE
            IF ( field1( 1 : 1 ) == 'G' ) THEN
              IF ( .NOT. setf ) THEN
                status = 61
                IF ( out > 0 ) WRITE( out, 2610 )
                GO TO 800
              END IF

!  set the first derivative value

              IF ( field1( 2 : 2 ) == ' ' ) THEN

! --------- start gdash

                IF ( .NOT. setg ) THEN
                  setg = .TRUE.
                  IF ( .NOT. endf ) THEN
                    WRITE( outgr, 3120 )
                    endf = .TRUE.
                  END IF
                END IF
                WRITE( outgr, 3130 ) FIELDI( 2 )( 1 : 6 ),                     &
                                     FIELDI( 10 )( 1 : 6 ), 2, field7

! --------- continuation of gdash

              ELSE
                IF ( field1( 2 : 2 ) == '+' ) THEN
                  IF ( setg ) THEN
                    WRITE( outgr, 3140 ) field7
                  ELSE
                    status = 56
                    IF ( out > 0 ) WRITE( out, 2560 )
                    GO TO 800
                  END IF
                END IF
              END IF

!  set the second derivative value

            ELSE
              IF ( field1( 1 : 1 ) == 'H' ) THEN
                IF ( field1( 2 : 2 ) == ' ' ) THEN

!  the first derivative has not been set

                  IF ( .NOT. seth ) THEN
                    IF ( .NOT. setg ) THEN
                      status = 62
                      IF ( out > 0 ) WRITE( out, 2620 )
                      GO TO 800
                    END IF
                    seth = .TRUE.
                  END IF

! --------- set g2dash

                  WRITE( outgr, 3130 ) FIELDI( 2 )( 1 : 6 ),                   &
                                       FIELDI( 10 )( 1 : 6 ), 3, field7

! --------- continuation of g2dash

                ELSE
                  IF ( field1( 2 : 2 ) == '+' ) THEN
                    IF ( seth ) THEN
                      WRITE( outgr, 3140 ) field7
                    ELSE
                      status = 56
                      IF ( out > 0 ) WRITE( out, 2560 )
                      GO TO 800
                    END IF
                  END IF
                END IF
              ELSE
                status = 56
                IF ( out > 0 ) WRITE( out, 2560 )
                GO TO 800
              END IF
            END IF
          END IF
        END IF
      END IF
      GO TO 100

!  the end of the input file has been reached before the endata card

  590 CONTINUE

!  if the elements card has not been encountered, exit

      IF ( defnam ) THEN
        status = 52
        IF ( out > 0 ) WRITE( out, 2520 )
        RETURN
      END IF
      IF ( ngtype > 0 ) GO TO 930
      IF ( out > 0 .AND. print_level /= 0 ) WRITE( out, 2010 )

!  a dummy routine will be substituted

  600 CONTINUE

!  write a dummy groups routine

      IF ( single ) THEN
         WRITE( outgr, 3001 ) ( FIELDI( i )( 1 : 6 ), i = 1, 4 ),              &
                    FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),              &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),              &
                    FIELDI(  7 )( 1 : 6 ),                                     &
                  ( FIELDI(  i )( 1 : 6 ), i = 15, 19 ),                       &
                    FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),              &
                    FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),              &
                  ( FIELDI(  i )( 1 : 6 ), i = 15, 20 ),                       &
                    FIELDI(  8 )( 1 : 6 ),                                     &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),              &
                    FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),              &
                    FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),              &
                    FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),              &
                    FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),              &
                    FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),              &
                    pname, TRIM( version )
      ELSE
         WRITE( outgr, 3000 ) ( FIELDI( i )( 1 : 6 ), i = 1, 4 ),              &
                    FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),              &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),              &
                    FIELDI(  7 )( 1 : 6 ),                                     &
                  ( FIELDI(  i )( 1 : 6 ), i = 15, 19 ),                       &
                    FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),              &
                    FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),              &
                  ( FIELDI(  i )( 1 : 6 ), i = 15, 20 ),                       &
                    FIELDI(  8 )( 1 : 6 ),                                     &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),              &
                    FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),              &
                    FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),              &
                    FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),              &
                    FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),              &
                    FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),              &
                    pname, TRIM( version )
      END IF
      WRITE( outgr, 3009 ) FIELDI( 20 )( 1 : 6 )
      WRITE( outgr, 3210 )
      status = 0
      RETURN

!  insufficient space to continue construction

  700 CONTINUE
      IF ( out > 0 ) WRITE( out, 2000 )
      RETURN

!  subroutine incomplete

  800 CONTINUE
      IF ( out > 0 ) WRITE( out, 2990 ) lineno, nuline
      RETURN

!  subroutine successfully completed

  900 CONTINUE
      IF ( .NOT. firstg ) THEN

!  finish of the previous group, if any

        IF ( .NOT. seth ) THEN
          status = 63
          IF ( out > 0 ) WRITE( out, 2630 )
          GO TO 800
        END IF
        IF ( .NOT. setg ) THEN
          status = 62
          IF ( out > 0 ) WRITE( out, 2620 )
          GO TO 800
        END IF

! ---------- wind up f and g

        IF ( setf ) THEN
          WRITE( outgr, 3190 )
        ELSE
          status = 61
          IF ( out > 0 ) WRITE( out, 2610 )
          GO TO 800
        END IF
        IF ( itype < ngtype ) WRITE( outgr, 3191 ) nloop
      END IF

! ---------- end do loop

      WRITE( outgr, 3200 ) nloop
  910 CONTINUE

! ---------- successful run. wind up output

      WRITE( outgr, 3210 )
      status = 0

!   check that all element types have been defined

  930 CONTINUE
      DO itype = 1, ngtype
        IF ( .NOT. DEFINED( itype ) ) THEN
          status = 53
          IF ( out > 0 ) WRITE( out, 2530 ) GTYPES( itype )
        END IF
      END DO
      RETURN

!  allocation errors

  980 CONTINUE
      WRITE( out, "( ' ** Message from INTERPRET_gpsmps-',                     &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  non-executable statements

 1000 FORMAT( A72 )
 1010 FORMAT( A160 )
 2000 FORMAT( ' ** Exit from MAKE_group - insufficient memory available',      &
              ' to enlarge hash table' )
 2010 FORMAT( ' ** Exit from MAKE_group - warning.',                           &
              ' First card not groups. ', /, '    A dummy',                    &
              ' routine will be substituted ' )
 2020 FORMAT( ' ** Exit from MAKE_group - indicator card not recognised ' )
 2190 FORMAT( ' ** Exit from MAKE_group - group type not recognised:',         &
              ' name is ', A10 )
 2510 FORMAT( ' ** Exit from MAKE_group -',                                    &
              ' Name on card not that specified on input ' )
 2520 FORMAT( ' ** Exit from MAKE_group -',                                    &
              ' data file incomplete. No ENDATA card ' )
 2530 FORMAT( ' ** Exit from MAKE_group - warning, group type ', A8,           &
              ' undefined ' )
 2540 FORMAT( ' ** Exit from MAKE_group -',                                    &
              ' unrecognised field 1 in TEMPORARIES section' )
 2550 FORMAT( ' ** Exit from MAKE_group -',                                    &
              ' unrecognised field 1 in GLOBALS section' )
 2560 FORMAT( ' ** Exit from MAKE_group -',                                    &
              ' unrecognised field 1 in INDIVIDUAL section' )
 2570 FORMAT( ' ** Exit from MAKE_group -',                                    &
              ' undefined parameter in GLOBALS section' )
 2580 FORMAT( ' ** Exit from MAKE_group -',                                    &
              ' undefined parameter in INDIVIDUALS section' )
 2590 FORMAT( ' ** Exit from MAKE_group - repeated parameter name ', A8 )
 2610 FORMAT( ' ** Exit from MAKE_group - function not set '  )
 2620 FORMAT( ' ** Exit from MAKE_group -',                                    &
              ' one or more first derivatives not set ' )
 2630 FORMAT( ' ** Exit from MAKE_group -',                                    &
              ' one or more second derivatives not set ' )
 2640 FORMAT( ' ** Exit from MAKE_group - group type already defined ' )
 2730 FORMAT( ' ** Exit from MAKE_group - field 2 or 3 not blank on',          &
              ' A, F, G or H card ' )
 2900 FORMAT( ' ' )
 2970 FORMAT( ' Line ', i5, 4X, A160 )
 2980 FORMAT( ' Line ', i5, '.', i2, 1X, A65 )
 2990 FORMAT( ' Line ', i5, 4X, A65 )
 3000 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 4( A6, ', ' ), A6, ' )', /,         &
              '      INTEGER ', 3( A6, ', ' ), A6, /,                          &
              '      INTEGER ', 3( A6, ', ' ), A6, /,                          &
              '      LOGICAL ', A6, /,                                         &
              '      INTEGER ', A6, '(', A6, '), ', A6, '(', A6,               &
                             '), ', A6, '(', A6, ')', /,                       &
              '      DOUBLE PRECISION ', A6, '(', A6, ',3), ',                 &
                                         A6, '(', A6, '), ', A6,               &
                                      '(', A6, ')', /,                         &
              'C', /, 'C  Problem name : ', A10, /,                            &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C' )
 3001 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 4( A6, ', ' ), A6, ' )', /,         &
              '      INTEGER ', 3( A6, ', ' ), A6, /,                          &
              '      INTEGER ', 3( A6, ', ' ), A6, /,                          &
              '      LOGICAL ', A6, /,                                         &
              '      INTEGER ', A6, '(', A6, '), ', A6, '(', A6,               &
                             '), ', A6, '(', A6, ')', /,                       &
              '      REAL             ', A6, '(', A6, ',3), ',                 &
                                         A6, '(', A6, '), ', A6,               &
                                      '(', A6, ')', /,                         &
              'C', /, 'C  Problem name : ', A10, /,                            &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C' )
 3002 FORMAT( '      INTEGER ', A6, ', ', A6, ', ', A6, ', ', A6 )
 3009 FORMAT( '      ', A6, ' = 0' )
 3010 FORMAT( ( '      INTEGER ', A6, 4( :, ', ', A6 ) ) )
 3019 FORMAT( ( '      REAL             ', A6, 4( :, ', ', A6 ) ) )
 3020 FORMAT( ( '      DOUBLE PRECISION ', A6, 4( :, ', ', A6 ) ) )
 3021 FORMAT( ( '      INTRINSIC ', A6, 4( :, ', ', A6 ) ) )
 3022 FORMAT( ( '      EXTERNAL ', A6, 4( :, ', ', A6 ) ) )
 3023 FORMAT( ( '      LOGICAL ', A6, 4( :, ', ', A6 ) ) )
 3030 FORMAT( '      ', A6, ' = ', A41 )
 3031 FORMAT( '      IF (', A6, ') ', A6, ' = ', A41 )
 3032 FORMAT( '      IF (.NOT.', A6, ') ', A6, ' = ', A41 )
 3040 FORMAT( '     *         ', A41 )
 3050 FORMAT( '      DO ', i5, 1X, A6, ' = 1, ', A6, /,                        &
              '       ', A6, ' = ', A6, '(', A6, ')', /,                       &
              '       ', A6, ' = ', A6, '(', A6, ')', /,                       &
              '       IF ( ', A6, ' == 0 ) GO TO ', i5, /,                     &
              '       ', A6, ' = ', A6, '(', A6, ') - 1' )
 3051 FORMAT( '       GO TO (', 8( i5, :, ',' ), /,                            &
            ( '     *        ', 8( i5, :, ',' ) ) )
 3052 FORMAT( '     *        ', 48X, '), ', A6 )
 3060 FORMAT( 'C', /, 'C  Group type : ', A8, /, 'C' )
 3061 FORMAT( i5, '  CONTINUE ' )
 3062 FORMAT( '       ', A6, '= ', A6, '(', A6, ')' )
 3063 FORMAT( '       ', A6, '= ', A6, '(', A6, '+', i6, ')' )
 3080 FORMAT( '       ', A6, '= ', A41 )
 3081 FORMAT( '       IF (', A6, ') ', A6, ' = ', A41 )
 3082 FORMAT( '       IF (.NOT.', A6, ') ', A6, '=', A41 )
 3083 FORMAT( '        ', A6, ' = ', A41 )
 3084 FORMAT( '        IF (', A6, ') ', A6, ' = ', A41 )
 3085 FORMAT( '        IF (.NOT.', A6, ')', A6, '=', A41 )
 3090 FORMAT( '     *          ', A41 )
 3091 FORMAT( '     *           ', A41 )
 3100 FORMAT( '       IF ( .NOT. ', A6, ' ) THEN', /,                          &
              '        ', A6, '(', A6, ',1)= ', A41 )
 3110 FORMAT( '     *                  ', A41 )
 3120 FORMAT( '       ELSE' )
 3130 FORMAT( '        ', A6, '(', A6, ',', i1, ')= ', A41 )
 3140 FORMAT( '     *                         ', A41 )
 3190 FORMAT( '       END IF' )
 3191 FORMAT( '       GO TO', i6 )
 3200 FORMAT( i5,  ' CONTINUE' )
 3210 FORMAT( '      RETURN', /,  '      END' )

!  end of subroutine MAKE_group

      END SUBROUTINE MAKE_group

!-*-  S I F D E C O D E   M A K E _ g r o u p _ a d    S U B R O U T I N E  -*-

      SUBROUTINE MAKE_group_ad( input, out, outgf, outgd, outem, status,       &
                                 ngtype, ngpnames, pname, GANAMES,             &
                                 len_renames, RENAMES, len_innames, INNAMES,   &
                                 len_lonames, LONAMES, len_minames, MINAMES,   &
                                 len_exnames, EXNAMES, GPNAMES,                &
                                 DEFINED, GTYPES, GTYPESP_ptr,                 &
                                 debug, length, TABLE, KEY, INLIST, single,    &
                                 nuline, gotlin, iauto, iad0, print_level )
      INTEGER :: input, out, outgf, status, length
      INTEGER :: ngtype, ngpnames
      INTEGER :: print_level, outgd, outem, iauto, iad0
      INTEGER :: len_renames, len_innames, len_lonames, len_minames, len_exnames
      LOGICAL :: gotlin, debug, single
      CHARACTER ( LEN = 10 ) ::  pname
      CHARACTER ( LEN = max_record_length ) :: nuline
      INTEGER :: GTYPESP_ptr( ngtype + 1 )
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TABLE, INLIST
      LOGICAL, DIMENSION( ngtype ) :: DEFINED
      CHARACTER ( LEN = 10 ) :: GPNAMES( ngpnames ), GTYPES( ngtype )
      CHARACTER ( LEN = 10 ) :: GANAMES( ngtype )
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: RENAMES, INNAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: LONAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: MINAMES, EXNAMES
      CHARACTER ( LEN = 12 ), ALLOCATABLE, DIMENSION( : ) :: KEY

!  --------------------------------------------------------------------
!  make a group function evaluation subroutine, suitable for
!  automatic differentiation from a gps function data file

!  function indicator cards
!  -------------------------

!  definition   purpose
!  ----------   --------
!  GROUPS       problem name
!  TEMPORARIES  names of additional parameters used in function defs
!  GLOBALS      general parameter assignments
!  INDIVIDUALS  set function and derivative values for each group-type
!  ENDATA       end of input data

!  data card description
!  ----------------------

!  see 'The SIF reference report', Chapter 7 in
!       A. R. Conn, N. I. M. Gould and Ph. L. Toint,
!       LANCELOT A Fortran Package for Large-Scale Nonlinear Optimization
!       (RElease A), Springer Series in Computational Mathematics 17,
!       Springer Verlag 1992

!  see also http://www.cuter.rl.ac.uk/sifdec/Doc/sif.pdf
!  and      http://www.numerical.rl.ac.uk/lancelot/sif/sifhtml.html

!  returns with negative values of status indicate that insufficient
!  array space has been allowed, as follows:

!    status = - 1  when length not large enough
!    status = - 2  when RENAMES, INNAMES, LONAMES, MINAMES or EXNAMES cannot be
!                  extended further
!  -------------------------------------------------------------------

!  parameter definitions

      INTEGER, PARAMETER :: mblank = 1, mfixed = 2, mfree = 3, mname = 4
      INTEGER, PARAMETER :: mtemp = 5, mglob = 6, mindiv = 7, mendat = 8
      INTEGER, PARAMETER :: iires = 21
      INTEGER, PARAMETER :: maxnul = 20
      INTEGER, DIMENSION( mendat ), PARAMETER :: LENIND                        &
        = (/ 0, 12, 11, 6, 11, 7, 11, 6 /)
      CHARACTER ( LEN = 12 ), DIMENSION( mendat ), PARAMETER :: INDIC8         &
        = (/ '            ', 'FIXED FORMAT', 'FREE FORMAT ', 'GROUPS      ',   &
             'TEMPORARIES ', 'GLOBALS     ', 'INDIVIDUALS ', 'ENDATA      '  /)
      CHARACTER ( LEN = 8 ), DIMENSION( iires ), PARAMETER :: FIELDI           &
        = (/ 'GROUPF  ', 'GVALUE  ', 'LGVALU  ', 'FVALUE  ', 'NCALCG  ',       &
             'ITYPEG  ', 'ICALCG  ', 'DERIVS  ', 'IGRTYP  ', 'IGROUP  ',       &
             'GPVALU  ', 'ISTGPA  ', 'IPSTRT  ', 'JCALCG  ', 'LTYPEG  ',       &
             'LSTGPA  ', 'LCALCG  ', 'LFVALU  ', 'LGPVLU  ', 'IGSTAT  ',       &
             'GROUP   ' /)

!  local variables

      INTEGER :: i, ifield, ifree, itype, intype, ivar, k1, k2
      INTEGER :: ninnames, nloop, nrenames, nminames, npname, nrenm1
      INTEGER :: nexnames, lineno, k, ilines, nlines, ntem, nlonames, nrenm2
      INTEGER :: used_length, new_length, min_length, alloc_status
      LOGICAL :: defnam, endpar, endgen, firstg
      LOGICAL :: setf, startp, fixed, startv, endf, loutgf
      CHARACTER ( LEN = 2 ) :: field1
      CHARACTER ( LEN = 4 ) :: ad0
      CHARACTER ( LEN = 8 ) :: field2, field3
      CHARACTER ( LEN = 10 ) :: ctemp
      CHARACTER ( LEN = 12 ) :: field, header
      CHARACTER ( LEN = 15 ) :: aorb
      CHARACTER ( LEN = 24 ) :: bad_alloc
      CHARACTER ( LEN = 41 ) :: field7
      CHARACTER ( LEN = 72 ) :: ctem
      CHARACTER ( LEN = max_record_length ) :: blnkln
      CHARACTER ( LEN = 65 ), DIMENSION( maxnul ) :: NULINA

      IF ( out > 0 ) WRITE( out, 2900 )

!  set initial values for integer variables

      ninnames = 0 ; nrenames = 0 ; nlonames = 0 ; nexnames = 0 ; nminames = 0
      lineno = 0 ; intype = 1 ; ilines = 0 ; nlines = 0 ; ntem = 0
      loutgf = outgf > 0

!  set initial values for logical variables

      defnam = .FALSE. ; endpar = .FALSE. ; startp = .FALSE. ; endgen = .FALSE.
      firstg = .TRUE. ; fixed = .TRUE.

!  find which group-types are nontrivial

      DEFINED( : ngtype ) = .FALSE.

!  insert the list of group-type arguments into the dictionary

      DO itype = 1, ngtype
        field = GANAMES( itype ) // 'PG'
        CALL HASH_enlarge_and_insert( length, 12, field,                       &
                                      TABLE, KEY, INLIST, ifree )
        IF ( ifree <= 0 ) THEN
          IF ( ifree == 0 ) THEN
            status = - 1
            GO TO 700
          END IF
        ELSE
          nrenames = nrenames + 1
          IF ( nrenames > len_renames ) THEN
            used_length = nrenames - 1 ; min_length = nrenames
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( RENAMES, len_renames, used_length,              &
                               new_length, min_length, buffer,                 &
                               status, alloc_status, 'RENAMES' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'RENAMES' ; status = - 2 ; GO TO 980
            END IF
            len_renames = new_length
          END IF
          RENAMES( nrenames ) = GANAMES( itype )
        END IF
      END DO

!  include the names of the group parameters used
!  in this dictionary

      IF ( ngtype > 0 ) THEN
        npname = GTYPESP_ptr( ngtype + 1 ) - 1
        DO i = 1, npname
          field = GPNAMES( i ) // 'PG'
          CALL HASH_enlarge_and_insert( length, 12, field,                     &
                                        TABLE, KEY, INLIST, ifree )
          IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
              status = - 1
              GO TO 700
            END IF
          ELSE
            nrenames = nrenames + 1
            IF ( nrenames > len_renames ) THEN
              used_length = nrenames - 1 ; min_length = nrenames
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( RENAMES, len_renames, used_length,            &
                                 new_length, min_length, buffer,               &
                                 status, alloc_status, 'RENAMES' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'RENAMES' ; status = - 2 ; GO TO 980
              END IF
              len_renames = new_length
            END IF
            RENAMES( nrenames ) = GPNAMES( i )
          END IF
        END DO
      END IF

!  set a blank line

      DO i = 1, max_record_length
        BLNKLN( i : i ) = ' '
      END DO

!  read next line

  100 CONTINUE
      IF ( ilines + 1 > nlines ) THEN

!  read next line from the input file

        lineno = lineno + 1
        IF ( fixed ) THEN
          IF ( gotlin ) THEN
            gotlin = .FALSE.
          ELSE
            nuline = blnkln
            READ ( input, 1000, END = 590, ERR = 590 ) nuline
          END IF
          IF ( out > 0 .AND. debug ) WRITE( out, 2990 ) lineno, nuline
        ELSE
          IF ( gotlin ) THEN
            gotlin = .FALSE.
          ELSE
            nuline = blnkln
            READ ( input, 1010, END = 590, ERR = 590 ) nuline
          END IF
          IF ( out > 0 .AND. debug ) WRITE( out, 2970 ) lineno, nuline

!  if the card is in free format, translate it into fixed format

          CALL FREE_format( nuline, max_record_length, mendat, INDIC8,         &
                            LENIND, NULINA, maxnul, nlines, .FALSE.,           &
                            status, out )
          IF ( status > 0 ) GO TO 800

!  if there are non-blank lines on the free format card, read the first

          IF ( nlines > 0 ) THEN
            ilines = 1
            nuline = blnkln
            nuline = NULINA( ilines )
            IF ( out > 0 .AND. debug ) WRITE( out, 2980 ) lineno, ilines, nuline

!  there are only blank lines on the free format card

          ELSE
            GO TO 100
          END IF
        END IF
      ELSE

!  read next line from the last encountered free format card

         ilines = ilines + 1
         nuline = blnkln
         nuline = NULINA( ilines )
         IF ( out > 0 .AND. debug ) WRITE( out, 2980 ) lineno, ilines, nuline
      END IF

!  consider the header part of the card

      header = NULINE( 1 : 12 )

!  ignore blank lines

      IF ( header == INDIC8( mblank ) ) GO TO 100
      IF ( NULINE( 1 : 1 ) /= ' ' ) THEN

!  ignore comment cards

        IF ( NULINE( 1 : 1 ) == '*' ) GO TO 100

!  check if we have entered fixed-format input

        IF ( header == INDIC8( mfixed ) ) THEN
          fixed = .TRUE.
           GO TO 100
        END IF

!  check if we have entered free-format input

        IF ( header == INDIC8( mfree ) ) THEN
          fixed = .FALSE.
          GO TO 100
        END IF

!  check that the first encountered indicator card is the groups card

        IF ( .NOT. defnam  ) THEN
          IF ( header /= INDIC8( mname ) ) THEN
            IF ( ngtype > 0 ) GO TO 930
            IF ( out > 0 .AND. print_level /= 0 ) WRITE( out, 2010)
            gotlin = .TRUE.
            GO TO 600

!  indicator card is groups
!  -------------------------

          ELSE
            IF ( pname  /= NULINE( 15 : 24 ) ) THEN
              status = 51
              IF ( out > 0 ) WRITE( out, 2510 )
              GO TO 800
            ELSE
              defnam = .TRUE.
              GO TO 100
            END IF
          END IF
        END IF

!  an indicator card has been found

        DO i = intype, mendat
          IF ( header == INDIC8( i ) ) THEN
            intype = i
            GO TO 120
          END IF
        END DO

!  the indicator card is not recognised

        status = 2
        IF ( out > 0 ) WRITE( out, 2020 )
        GO TO 800

!  the parameter values have been completed. write out the
!  first part of the generated subroutine

  120   CONTINUE
        IF ( intype >= mglob .AND. .NOT. endpar ) THEN
          endpar = .TRUE.
          nloop = ngtype + 1

!  insert the list of reserved integer/real/logical variables into
!  the dictionary

          DO i = 1, iires
            field = FIELDI( i ) // '  PG'
            CALL HASH_enlarge_and_insert( length, 12, field,                   &
                                          TABLE, KEY, INLIST, ifree )
            IF ( ifree <= 0 ) THEN
              IF ( ifree == 0 ) THEN
                status = - 1
                GO TO 700
              END IF
              status = 59
              IF ( out > 0 ) WRITE( out, 2590 ) FIELDI( i )
              GO TO 800
            END IF
          END DO

!  -------- set up subroutine call and reserved parameter declarations

          IF ( iauto == 1 ) THEN
            IF ( single ) THEN
              aorb = 'FORWARD_SINGLE '
            ELSE
              aorb = 'FORWARD_DOUBLE '
            END IF
          ELSE
            IF ( single ) THEN
              aorb = 'BACKWARD_SINGLE'
            ELSE
              aorb = 'BACKWARD_DOUBLE'
            END IF
          END IF
          IF ( iad0 == 1 ) THEN
            ad0 = 'AD01'
          ELSE
            ad0 = 'AD02'
          END IF
          IF ( single ) THEN
            IF ( loutgf )                                                      &
              WRITE( outgf, 3001 ) ( FIELDI( i )( 1 : 6 ), i = 1, 4 ),         &
                            FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),      &
                            FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),      &
                            FIELDI(  7 )( 1 : 6 ),                             &
                          ( FIELDI(  i )( 1 : 6 ), i = 15, 19 ),               &
                            FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),      &
                            FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),      &
                          ( FIELDI(  i )( 1 : 6 ), i = 15, 20 ),               &
                            FIELDI(  8 )( 1 : 6 ),                             &
                            FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),      &
                            FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),      &
                            FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),      &
                            FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),      &
                            FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),      &
                            FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),      &
                            pname, TRIM( version )
            WRITE( outgd, 3005 ) FIELDI( 21 )( 1 : 6 ),                        &
                        ( FIELDI( i )( 1 : 6 ), i = 2, 4 ),                    &
                          FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),        &
                          FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),        &
                          FIELDI(  7 )( 1 : 6 ),                               &
                        ( FIELDI(  i )( 1 : 6 ), i = 15, 19 ),                 &
                          FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),        &
               ad0, aorb, FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),        &
                        ( FIELDI(  i )( 1 : 6 ), i = 15, 20 ),                 &
                          FIELDI(  8 )( 1 : 6 ),                               &
                          FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),        &
                          FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),        &
                          FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),        &
                          FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),        &
                          FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),        &
                          FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),        &
                          pname, TRIM( version )
          ELSE
            IF ( loutgf )                                                      &
              WRITE( outgf, 3000 ) ( FIELDI( i )( 1 : 6 ), i = 1, 4 ),         &
                            FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),      &
                            FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),      &
                            FIELDI(  7 )( 1 : 6 ),                             &
                          ( FIELDI(  i )( 1 : 6 ), i = 15, 19 ),               &
                            FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),      &
                            FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),      &
                          ( FIELDI(  i )( 1 : 6 ), i = 15, 20 ),               &
                            FIELDI(  8 )( 1 : 6 ),                             &
                            FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),      &
                            FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),      &
                            FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),      &
                            FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),      &
                            FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),      &
                            FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),      &
                            pname, TRIM( version )
            WRITE( outgd, 3004 ) FIELDI( 21 )( 1 : 6 ),                        &
                        ( FIELDI( i )( 1 : 6 ), i = 2, 4 ),                    &
                          FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),        &
                          FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),        &
                          FIELDI(  7 )( 1 : 6 ),                               &
                        ( FIELDI(  i )( 1 : 6 ), i = 15, 19 ),                 &
                          FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),        &
               ad0, aorb, FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),        &
                        ( FIELDI(  i )( 1 : 6 ), i = 15, 20 ),                 &
                          FIELDI(  8 )( 1 : 6 ),                               &
                          FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),        &
                          FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),        &
                          FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),        &
                          FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),        &
                          FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),        &
                          FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),        &
                          pname, TRIM( version )
          END IF
          IF ( iad0 == 1 ) THEN
             WRITE( outgd, 3006 )
          ELSE
             WRITE( outgd, 3007 )
          END IF
          IF ( ngtype == 0 ) THEN
             IF ( loutgf ) WRITE( outgf, 3009 ) FIELDI( 20 )( 1 : 6 )
             WRITE( outgd, 3009 ) FIELDI( 20 )( 1 : 6 )
             GO TO 910
          END IF
          IF ( loutgf ) WRITE( outgf, 3002 ) FIELDI(  9 )( 1 : 6 ),            &
                          FIELDI( 10 )( 1 : 6 ), FIELDI( 13 )( 1 : 6 ),        &
                          FIELDI( 14 )( 1 : 6 )
          WRITE( outgd, 3002 ) FIELDI(  9 )( 1 : 6 ),                          &
                          FIELDI( 10 )( 1 : 6 ), FIELDI( 13 )( 1 : 6 ),        &
                          FIELDI( 14 )( 1 : 6 )

! --------- insert integer declarations

          IF ( ninnames > 0 .AND. loutgf )                                     &
            WRITE( outgf, 3010 ) ( INNAMES( i ), i = 1, ninnames )
          IF ( ninnames > 0 )                                                  &
            WRITE( outgd, 3010 ) ( INNAMES( i ), i = 1, ninnames )

!  order the real values so that the list of variables which belong
!  to intrinsic or external functions follow those which do not

          IF ( nrenames > 0 ) THEN
            nrenm1 = 0
            nrenm2 = nrenames + 1
  140       CONTINUE
            IF ( nrenm1 + 1 == nrenm2 ) GO TO 180
            DO i = 1, nminames
              IF ( RENAMES( nrenm1 + 1 ) == MINAMES( i ) ) GO TO 170
            END DO
            DO i = 1, nexnames
              IF ( RENAMES( nrenm1 + 1 ) == EXNAMES( i ) ) GO TO 170
            END DO
            nrenm1 = nrenm1 + 1
            GO TO 140
  170       CONTINUE
            nrenm2 = nrenm2 - 1
            ctemp = RENAMES( nrenm2 )
            RENAMES( nrenm2 ) = RENAMES( nrenm1 + 1 )
            RENAMES( nrenm1 + 1 ) = ctemp
            GO TO 140
  180       CONTINUE

! --------- insert real declarations

            IF ( single ) THEN
              IF ( loutgf )                                                    &
                WRITE( outgf, 3019 ) ( RENAMES( i ), i = 1, nrenames )
            ELSE
             IF ( loutgf )                                                     &
               WRITE( outgf, 3020 ) ( RENAMES( i ), i = 1, nrenames )
            END IF
            IF ( iad0 == 1 ) THEN
               IF ( nrenm1 > 0 ) WRITE( outgd, 3018 )                          &
                 ( RENAMES( i ), i = 1, nrenm1 )
            ELSE
               IF ( nrenm1 > 0 ) WRITE( outgd, 3017 )                          &
                 ( ad0, RENAMES( i ), i = 1, nrenm1 )
            END IF
            IF ( nrenm2 <= nrenames ) WRITE( outgd, 3017 )                     &
                 ( ad0, RENAMES( i ), i = nrenm2, nrenames )
          END IF

! --------- insert logical declarations

          IF ( nlonames > 0 .AND. loutgf )                                     &
            WRITE( outgf, 3023 ) ( LONAMES( i ), i = 1, nlonames )
          IF ( nlonames > 0 )                                                  &
            WRITE( outgd, 3023 ) ( LONAMES( i ), i = 1, nlonames )

! --------- insert intrinsic declarations

          IF ( nminames > 0 .AND. loutgf )                                     &
            WRITE( outgf, 3021 ) ( MINAMES( i ), i = 1, nminames )

! --------- insert external declarations

          IF ( nexnames > 0 .AND. loutgf )                                     &
            WRITE( outgf, 3022 ) ( EXNAMES( i ), i = 1, nexnames )
          IF ( nexnames > 0 )                                                  &
            WRITE( outgd, 3022 ) ( EXNAMES( i ), i = 1, nexnames )
          IF ( loutgf ) WRITE( outgf, 3009 ) FIELDI( 20 )( 1 : 6 )
          WRITE( outgd, 3009 ) FIELDI( 20 )( 1 : 6 )
        END IF

!  the general parameter assignments have been completed
!  continue with the construction of the generated subroutine

        IF ( intype >= mindiv .AND. .NOT. endgen ) THEN
          endgen = .TRUE.

! --------- start loop over groups

          IF ( loutgf )                                                        &
            WRITE( outgf, 3050 ) nloop,  FIELDI( 14 )( 1 : 6 ),                &
                 FIELDI(  5 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),                 &
                 FIELDI(  7 )( 1 : 6 ), FIELDI( 14 )( 1 : 6 ),                 &
                 FIELDI(  9 )( 1 : 6 ),                                        &
                 FIELDI(  6 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),                 &
                 FIELDI(  9 )( 1 : 6 ), nloop,                                 &
                 FIELDI( 13 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),                 &
                 FIELDI( 10 )( 1 : 6 )
          IF ( iad0 == 2 ) THEN
            WRITE( outgd, 3011 )
!           do i = 1, ngtnam
            DO i = 1, nrenm1
               WRITE( outgd, 3016 ) ad0, RENAMES( i )
            END DO
          END IF
          WRITE( outgd, 3050 ) nloop,  FIELDI( 14 )( 1 : 6 ),                  &
                 FIELDI(  5 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),                 &
                 FIELDI(  7 )( 1 : 6 ), FIELDI( 14 )( 1 : 6 ),                 &
                 FIELDI(  9 )( 1 : 6 ),                                        &
                 FIELDI(  6 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),                 &
                 FIELDI(  9 )( 1 : 6 ), nloop,                                 &
                 FIELDI( 13 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),                 &
                 FIELDI( 10 )( 1 : 6 )
          IF ( ngtype > 1 ) THEN
             IF ( loutgf ) WRITE( outgf, 3051 ) ( i, i = 1, ngtype )
             WRITE( outgd, 3051 ) ( i, i = 1, ngtype )
             IF ( loutgf ) WRITE( outgf, 3052 ) FIELDI(  9 )( 1 : 6 )
             WRITE( outgd, 3052 ) FIELDI(  9 )( 1 : 6 )
          END IF
        END IF

!  indicator card is endata
!  -------------------------

        IF ( intype == mendat ) GO TO 900
        GO TO 100

!  check that the first non comment card is the groups indicator card

      ELSE
        IF ( .NOT. defnam  ) THEN
          IF ( ngtype > 0 ) GO TO 930
          IF ( out > 0 .AND. print_level /= 0 ) WRITE( out, 2010 )
          gotlin = .TRUE.
          GO TO 600
        END IF

!  a data card has been found
!  read the character fields 1, 2, 3 and 7 from the card

        field1 = NULINE(  2 :  3 )
        field2 = NULINE(  5 : 12 )
        field3 = NULINE( 15 : 22 )
        field7 = NULINE( 25 : 65 )

!  check that field3 is blank on 'a', 'f' and 'g' cards

        IF ( field1( 1 : 1 ) == 'A' .OR. field1( 1 : 1 ) == 'F' .OR.           &
             field1( 1 : 1 ) == 'G' .OR. field1( 1 : 1 ) == 'H' ) THEN
          IF ( ( field1( 1 : 1 ) /= 'A' .AND. field2 /=                        &
                '       ' ) .OR.FIELD3 /= '       ' ) THEN
            status = 73
            IF ( out > 0 ) WRITE( out, 2730 )
            GO TO 800
          END IF
        END IF
      END IF

!  branch on the value of intype

      GO TO ( 100, 100, 100, 100, 290, 300, 400, 900 ), intype

!  indicator card is temporaries
!  ------------------------------

  290 CONTINUE

!  check to see if the parameter is integer, real, logical or a function

      IF ( field1 /= 'I ' .AND. field1 /= 'R ' .AND. field1 /= 'M ' .AND.      &
           field1 /= 'F ' .AND. field1 /= 'L' ) THEN
        status = 54
        IF ( out > 0 ) WRITE( out, 2540 )
        GO TO 800
      END IF

!  if the parameter is a function, check to see that the name has
!  not already been used

      IF ( field1 == 'F ' ) THEN
        field = field2 // '  GU'
        CALL HASH_enlarge_and_insert( length, 12, field,                       &
                                      TABLE, KEY, INLIST, ifree )
        IF ( ifree <= 0 ) THEN
          IF ( ifree == 0 ) THEN
            status = - 1
            GO TO 700
          END IF
        ELSE
          nexnames = nexnames + 1
          IF ( nrenames > len_renames ) THEN
            used_length = nexnames - 1 ; min_length = nexnames
            new_length = increase_n * min_length / increase_d + 1
            CALL EXTEND_array( EXNAMES, len_exnames, used_length,              &
                               new_length, min_length, buffer,                 &
                               status, alloc_status, 'EXNAMES' )
            IF ( status /= 0 ) THEN
              bad_alloc = 'EXNAMES' ; status = - 2 ; GO TO 980 ; END IF
            len_exnames = new_length
          END IF
          EXNAMES( nexnames ) = field2
        END IF

!  check to see that the parameter name has not already been used

      ELSE
        field = field2 // '  PG'
        CALL HASH_enlarge_and_insert( length, 12, field,                      &
                                      TABLE, KEY, INLIST, ifree )
        IF ( ifree <= 0 ) THEN
          IF ( ifree == 0 ) THEN
            status = - 1
            GO TO 700
          END IF
        ELSE
          IF ( field1 == 'R ' ) THEN
            nrenames = nrenames + 1
            IF ( nrenames > len_renames ) THEN
              used_length = nrenames - 1 ; min_length = nrenames
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( RENAMES, len_renames, used_length,            &
                                 new_length, min_length, buffer,               &
                                 status, alloc_status, 'RENAMES' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'RENAMES' ; status = - 2 ; GO TO 980 ; END IF
              len_renames = new_length
            END IF
            RENAMES( nrenames ) = field2
          ELSE IF ( field1 == 'M ' ) THEN
            nminames = nminames + 1
            IF ( nminames > len_minames ) THEN
              used_length = nminames - 1 ; min_length = nminames
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( MINAMES, len_minames, used_length,            &
                                 new_length, min_length, buffer,               &
                                 status, alloc_status, 'MINAMES' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'MINAMES' ; status = - 2 ; GO TO 980 ; END IF
              len_minames = new_length
            END IF
            MINAMES( nminames ) = field2
          ELSE IF ( field1 == 'L ' ) THEN
            nlonames = nlonames + 1
            IF ( nlonames > len_lonames ) THEN
              used_length = nlonames - 1 ; min_length = nlonames
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( LONAMES, len_lonames, used_length,            &
                                 new_length, min_length, buffer,               &
                                 status, alloc_status, 'LONAMES' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'LONAMES' ; status = - 2 ; GO TO 980 ; END IF
              len_lonames = new_length
            END IF
            LONAMES( nlonames ) = field2
          ELSE
            ninnames = ninnames + 1
            IF ( ninnames > len_innames ) THEN
              used_length = ninnames - 1 ; min_length = ninnames
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( INNAMES, len_innames, used_length,            &
                                 new_length, min_length, buffer,               &
                                 status, alloc_status, 'INNAMES' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'INNAMES' ; status = - 2 ; GO TO 980 ; END IF
            END IF
            INNAMES( ninnames ) = field2
          END IF
        END IF
      END IF
      GO TO 100

!  indicator card is global
!  -------------------------

  300 CONTINUE
      IF ( field1 == 'A ' .OR. field1 == 'I ' .OR. field1 == 'E ' ) THEN
        startp = .TRUE.

!  start a parameter assignment. check to see that the parameter has
!  been defined

        field = field2 // '  PG'
        CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
        IF ( ifield <= 0 ) THEN
          IF ( ifree == 0 ) THEN
            status = - 1
            GO TO 700
          END IF
          status = 57
          IF ( out > 0 ) WRITE( out, 2570 )
          GO TO 800
        END IF

! --------- make general parameter assignments

        IF ( field1 == 'A ' ) THEN
          IF ( loutgf ) WRITE( outgf, 3030 ) FIELD2( 1 : 6 ), field7
          ntem = ntem + 1
          WRITE( outem, 3080 ) FIELD2( 1 : 6 ), field7

! --------- make conditional parameter assignments

        ELSE

!  check that the logical variable has been defined

          field = field3 // '  PG'
          CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
          IF ( ifield <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               GO TO 700
            END IF
            status = 57
            IF ( out > 0 ) WRITE( out, 2570 )
            GO TO 800
          END IF
          IF ( field1 == 'I ' ) THEN
            IF ( loutgf ) WRITE( outgf, 3031 ) FIELD2( 1 : 6 ),                &
                                               FIELD3( 1 : 6 ), field7
            ntem = ntem + 1
            WRITE( outem, 3081 ) FIELD2( 1 : 6 ),                              &
                                 FIELD3( 1 : 6 ), field7
          ELSE
            IF ( loutgf ) WRITE( outgf, 3032 ) FIELD2( 1 : 6 ),                &
                                               FIELD3( 1 : 6 ), field7
            ntem = ntem + 1
            WRITE( outem, 3082 ) FIELD2( 1 : 6 ),                              &
                                 FIELD3( 1 : 6 ), field7
          END IF
        END IF

! --------- continue a parameter assignment

      ELSE
        IF ( field1( 2 : 2 ) == '+' .AND. startp ) THEN
          IF ( loutgf ) WRITE( outgf, 3040 ) field7
          ntem = ntem + 1
          WRITE( outem, 3040 ) field7
        ELSE
          status = 55
          IF ( out > 0 ) WRITE( out, 2550 )
          GO TO 800
        END IF
      END IF
      GO TO 100

!  indicator card is individuals
!  ------------------------------

  400 CONTINUE

!  check if a new group has been encountered

      IF ( field1 == 'T ' ) THEN
        IF ( firstg ) THEN

!  check if this is the first group-type

          firstg = .FALSE.

! ---------- wind up f and g

        ELSE
          IF ( setf ) THEN
            IF ( .NOT. endf ) THEN
               IF ( iad0 == 1 ) THEN
                 WRITE( outgd, 3122 ) FIELDI( 8 )( 1 : 6 ),                    &
                    FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),              &
                    FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),              &
                    FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
              ELSE
                 WRITE( outgd, 3123 ) FIELDI( 8 )( 1 : 6 ),                    &
                    FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),              &
                    FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),              &
                    FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
              END IF
              endf = .TRUE.
            END IF
            WRITE( outgd, 3190 )
          ELSE
            status = 61
            IF ( out > 0 ) WRITE( out, 2610 )
            GO TO 800
          END IF
          IF ( itype < ngtype .AND. loutgf ) WRITE( outgf, 3191 ) nloop
          IF ( itype < ngtype ) WRITE( outgd, 3191 ) nloop
        END IF

!  find itype, the group-type

        field = field2 // '  GT'
        CALL HASH_search( length, 12, field, TABLE, KEY, ifield )

!  the group-type is unknown

        IF ( ifield <= 0 ) THEN
          status = 19
          IF ( out > 0 ) WRITE( out, 2190 )
          GO TO 800
        END IF

! --------- find type of current group

        itype = INLIST( ifield )
        IF ( loutgf ) WRITE( outgf, 3060 ) field2
        WRITE( outgd, 3060 ) field2
        IF ( ngtype > 1 .AND. loutgf ) WRITE( outgf, 3061 ) itype
        IF ( ngtype > 1 ) WRITE( outgd, 3061 ) itype
        IF ( loutgf ) WRITE( outgf, 3062 ) GANAMES( itype )( 1 : 6 ),          &
                  FIELDI(  4 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
        IF ( iad0 == 1 ) THEN
          WRITE( outgd, 3064 ) FIELDI(  4 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),   &
                               GANAMES( itype )( 1 : 6 )
        ELSE
          WRITE( outgd, 3065 ) FIELDI(  4 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),   &
                               GANAMES( itype )( 1 : 6 )
        END IF

! --------- set group parameters

        k1 = GTYPESP_ptr( itype ) ; k2 = GTYPESP_ptr( itype + 1 ) - 1
        DO k = k1, k2
          ivar = k - k1 + 1
          IF ( loutgf )                                                        &
          WRITE( outgf, 3063 ) GPNAMES( k ), FIELDI( 11 )( 1 : 6 ),            &
                               FIELDI( 13 )( 1 : 6 ), ivar
!         if ( iad0 == 2 ) write( ioutgd, 3015 ) ad0, GPNAMES( k )
          WRITE( outgd, 3063 ) GPNAMES( k ), FIELDI( 11 )( 1 : 6 ),            &
               FIELDI( 13 )( 1 : 6 ), ivar
        END DO
        IF ( DEFINED( itype ) ) THEN
          status = 64
          IF ( out > 0 ) WRITE( out, 2640 )
          GO TO 800
        ELSE
          DEFINED( itype ) = .TRUE.
        END IF

!  initialize logicals which determine whether the data has been
!  input in the correct order

        startp = .FALSE.
        setf = .FALSE.
        endf = .TRUE.
        startv = .FALSE.
      ELSE
        IF ( field1( 1 : 1 ) == 'A' .OR. field1( 1 : 1 )  == 'I' .OR.          &
             field1( 1 : 1 ) == 'E' ) THEN

!  finish off the function assignment

          IF ( setf ) THEN
            IF ( .NOT. endf ) THEN
              IF ( iad0 == 1 ) THEN
                WRITE( outgd, 3122 ) FIELDI( 8 )( 1 : 6 ),                     &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),          &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),          &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
              ELSE
                 WRITE( outgd, 3123 ) FIELDI( 8 )( 1 : 6 ),                &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),          &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),          &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
              END IF
              endf = .TRUE.
            END IF
          END IF

!  start a parameter assignment. check to see that the parameter has
!  been defined

          IF ( .NOT. setf ) THEN
            IF ( field1( 2 : 2 ) == ' ' ) THEN
              startp = .TRUE.

!  include the global parameters

              IF ( .NOT. startv ) THEN
                REWIND( outem )
                DO i = 1, ntem
                  READ( outem, 1000 ) ctem
                  WRITE( outgd, 1000 ) ctem
                END DO
                startv = .TRUE.
              END IF
              field = field2 // '  PG'
              CALL HASH_search( length, 12, field, TABLE, KEY, ifield )
              IF ( ifield <= 0 ) THEN
                status = 57
                IF ( out > 0 ) WRITE( out, 2570 )
                GO TO 800
              END IF

! --------- make group-specific parameter assignments

              IF ( field1( 1 : 1 ) == 'A' ) THEN
                IF ( .NOT. setf ) THEN
                  IF ( loutgf ) WRITE( outgf, 3080 ) FIELD2( 1 : 6 ), field7
                   WRITE( outgd, 3080 ) FIELD2( 1 : 6 ), field7
                ELSE
                  IF ( loutgf ) WRITE( outgf, 3083 ) FIELD2( 1 : 6 ), field7
                  WRITE( outgd, 3083 ) FIELD2( 1 : 6 ), field7
                END IF

! --------- make conditional parameter assignments

              ELSE

!  check that the logical variable has been defined

                field = field3 // '  PG'
                CALL HASH_search( length, 12, field, TABLE, KEY, IFIELD)
                IF ( ifield <= 0 ) THEN
                  IF ( ifree == 0 ) THEN
                     status = - 1
                     GO TO 700
                  END IF
                  status = 58
                  IF ( out > 0 ) WRITE( out, 2580 )
                  GO TO 800
                END IF
                IF ( field1( 1 : 1 ) == 'I' ) THEN
                  IF ( .NOT. setf ) THEN
                    IF ( loutgf ) WRITE( outgf, 3081 ) FIELD2( 1 : 6 ),        &
                                                       FIELD3( 1 : 6 ), field7
                    WRITE( outgd, 3081 ) FIELD2( 1 : 6 ),                      &
                                         FIELD3( 1 : 6 ), field7
                  ELSE
                    IF ( loutgf ) WRITE( outgf, 3084 ) FIELD2( 1 : 6 ),        &
                                         FIELD3( 1 : 6 ), field7
                    WRITE( outgd, 3084 ) FIELD2( 1 : 6 ),                      &
                                         FIELD3( 1 : 6 ), field7
                  END IF
                ELSE
                  IF ( .NOT. setf ) THEN
                    IF ( loutgf ) WRITE( outgf, 3082 ) FIELD2( 1 : 6 ),        &
                                                       FIELD3( 1 : 6 ), field7
                    WRITE( outgd, 3082 ) FIELD2( 1 : 6 ),                      &
                                         FIELD3( 1 : 6 ), field7
                  ELSE
                    IF ( loutgf ) WRITE( outgf, 3085 ) FIELD2( 1 : 6 ),        &
                                                       FIELD3( 1 : 6 ), field7
                    WRITE( outgd, 3085 ) FIELD2( 1 : 6 ),                      &
                                         FIELD3( 1 : 6 ), field7
                  END IF
                END IF
              END IF

! --------- continuation of a parameter assignment

            ELSE
              IF ( field1( 2 : 2 ) == '+' ) THEN
                IF ( startp ) THEN
                  IF ( .NOT. setf ) THEN
                    IF ( loutgf ) WRITE( outgf, 3090 ) field7
                    WRITE( outgd, 3090 ) field7
                  ELSE
                    IF ( loutgf ) WRITE( outgf, 3091 ) field7
                    WRITE( outgd, 3091 ) field7
                  END IF
                ELSE
                  status = 56
                  IF ( out > 0 ) WRITE( out, 2560 )
                  GO TO 800
                END IF
              END IF
            END IF
          END IF
        ELSE
          startp = .FALSE.
          IF ( field1( 1 : 1 ) == 'F' ) THEN

!  set the function value

            IF ( field1( 2 : 2 ) == ' ' ) THEN
              setf = .TRUE.
              endf = .FALSE.

!  include the global parameters

              IF ( .NOT. startv ) THEN
                REWIND( outem )
                DO i = 1, ntem
                  READ( outem, 1000 ) ctem
                  WRITE( outgd, 1000 ) ctem
                END DO
                startv = .TRUE.
              END IF

! --------- start g

              IF ( loutgf ) WRITE( outgf, 3100 ) FIELDI(  8 )( 1 : 6 ),        &
                  FIELDI( 2 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ), field7
              WRITE( outgd, 3101 ) field7

! --------- continuation of g

            ELSE
              IF ( field1( 2 : 2 ) == '+' ) THEN
                IF ( setf ) THEN
                  IF ( loutgf ) WRITE( outgf, 3110 ) field7
                  WRITE( outgd, 3110 ) field7
                ELSE
                  status = 56
                  IF ( out > 0 ) WRITE( out, 2560 )
                  GO TO 800
                END IF
              END IF
            END IF
          ELSE IF ( field1( 1 : 1 ) == 'G' .OR. field1( 1 : 1 ) == 'H' ) THEN
            IF ( setf ) THEN
              IF ( .NOT. endf ) THEN
                IF ( iad0 == 1 ) THEN
                  WRITE( outgd, 3122 ) FIELDI( 8 )( 1 : 6 ),                   &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),          &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),          &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
                ELSE
                  WRITE( outgd, 3123 ) FIELDI( 8 )( 1 : 6 ),                   &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),          &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),          &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
                END IF
                endf = .TRUE.
              END IF
            END IF
          ELSE
            status = 56
            IF ( out > 0 ) WRITE( out, 2560 )
            GO TO 800
          END IF
        END IF
      END IF
      GO TO 100

!  the end of the input file has been reached before the endata card

  590 CONTINUE

!  if the elements card has not been encountered, exit

      IF ( defnam ) THEN
        status = 52
        IF ( out > 0 ) WRITE( out, 2520 )
        RETURN
      END IF
      IF ( ngtype > 0 ) GO TO 930
      IF ( out > 0 .AND. print_level /= 0 ) WRITE( out, 2010 )

!  a dummy routine will be substituted

  600 CONTINUE

!  write a dummy groups routine

      IF ( iauto == 1 ) THEN
        IF ( single ) THEN
          aorb = 'FORWARD_SINGLE '
        ELSE
          aorb = 'FORWARD_DOUBLE '
        END IF
      ELSE
        IF ( single ) THEN
          aorb = 'BACKWARD_SINGLE'
        ELSE
          aorb = 'BACKWARD_DOUBLE'
        END IF
      END IF
      IF ( iad0 == 1 ) THEN
        ad0 = 'AD01'
      ELSE
        ad0 = 'AD02'
      END IF
      IF ( single ) THEN
        IF ( loutgf )                                                          &
          WRITE( outgf, 3001 ) ( FIELDI( i )( 1 : 6 ), i = 1, 4 ),             &
                    FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),              &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),              &
                    FIELDI(  7 )( 1 : 6 ),                                     &
                  ( FIELDI(  i )( 1 : 6 ), i = 15, 19 ),                       &
                    FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),              &
                    FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),              &
                  ( FIELDI(  i )( 1 : 6 ), i = 15, 20 ),                       &
                    FIELDI(  8 )( 1 : 6 ),                                     &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),              &
                    FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),              &
                    FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),              &
                    FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),              &
                    FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),              &
                    FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),              &
                    pname, TRIM( version )
        WRITE( outgd, 3005 ) FIELDI( 21 )( 1 : 6 ),                            &
                  ( FIELDI( i )( 1 : 6 ), i = 2, 4 ),                          &
                    FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),              &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),              &
                    FIELDI(  7 )( 1 : 6 ),                                     &
                  ( FIELDI(  i )( 1 : 6 ), i = 15, 19 ),                       &
                    FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),              &
         ad0, aorb, FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),              &
                  ( FIELDI(  i )( 1 : 6 ), i = 15, 20 ),                       &
                    FIELDI(  8 )( 1 : 6 ),                                     &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),              &
                    FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),              &
                    FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),              &
                    FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),              &
                    FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),              &
                    FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),              &
                    pname, TRIM( version )
      ELSE
        IF ( loutgf )                                                          &
          WRITE( outgf, 3000 ) ( FIELDI( i )( 1 : 6 ), i = 1, 4 ),             &
                    FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),              &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),              &
                    FIELDI(  7 )( 1 : 6 ),                                     &
                  ( FIELDI(  i )( 1 : 6 ), i = 15, 19 ),                       &
                    FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),              &
                    FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),              &
                  ( FIELDI(  i )( 1 : 6 ), i = 15, 20 ),                       &
                    FIELDI(  8 )( 1 : 6 ),                                     &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),              &
                    FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),              &
                    FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),              &
                    FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),              &
                    FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),              &
                    FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),              &
                    pname, TRIM( version )
        WRITE( outgd, 3004 ) FIELDI( 21 )( 1 : 6 ),                            &
                  ( FIELDI( i )( 1 : 6 ), i = 2, 4 ),                          &
                    FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),              &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),              &
                    FIELDI(  7 )( 1 : 6 ),                                     &
                  ( FIELDI(  i )( 1 : 6 ), i = 15, 19 ),                       &
                    FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),              &
         ad0, aorb, FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),              &
                  ( FIELDI(  i )( 1 : 6 ), i = 15, 20 ),                       &
                    FIELDI(  8 )( 1 : 6 ),                                     &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),              &
                    FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),              &
                    FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),              &
                    FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),              &
                    FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),              &
                    FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),              &
                    pname, TRIM( version )
      END IF
      IF ( loutgf ) WRITE( outgf, 3009 ) FIELDI( 20 )( 1 : 6 )
      WRITE( outgd, 3009 ) FIELDI( 20 )( 1 : 6 )
      IF ( loutgf ) WRITE( outgf, 3210 )
      WRITE( outgd, 3210 )
      status = 0
      RETURN

!  insufficient space to continue construction

  700 CONTINUE
      IF ( out > 0 ) WRITE( out, 2000 )
      RETURN

!  subroutine incomplete

  800 CONTINUE
      IF ( out > 0 ) WRITE( out, 2990 ) lineno, nuline
      RETURN

!  subroutine successfully completed

  900 CONTINUE
      IF ( .NOT. firstg ) THEN

! ---------- wind up f and g

        IF ( setf ) THEN
          IF ( .NOT. endf ) THEN
            IF ( iad0 == 1 ) THEN
              WRITE( outgd, 3122 ) FIELDI( 8 )( 1 : 6 ),                       &
                     FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),             &
                     FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),             &
                     FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
            ELSE
              WRITE( outgd, 3123 ) FIELDI( 8 )( 1 : 6 ),                       &
                     FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),             &
                     FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),             &
                     FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
            END IF
            endf = .TRUE.
          END IF
          WRITE( outgd, 3190 )
        ELSE
           status = 61
           IF ( out > 0 ) WRITE( out, 2610 )
           GO TO 800
        END IF
        IF ( itype < ngtype .AND. loutgf ) WRITE( outgf, 3191 ) nloop
        IF ( itype < ngtype ) WRITE( outgd, 3191 ) nloop
      END IF

! ---------- end do loop

      IF ( loutgf ) WRITE( outgf, 3200 ) nloop
      WRITE( outgd, 3200 ) nloop
      IF ( iad0 == 2 ) WRITE( outgd, 3192 )

! ---------- successful run. wind up output

  910 CONTINUE
      IF ( loutgf ) WRITE( outgf, 3210 )
      WRITE( outgd, 3210 )
      status = 0

!   check that all element types have been defined

  930 CONTINUE
      DO itype = 1, ngtype
        IF ( .NOT. DEFINED( itype ) ) THEN
          status = 53
          IF ( out > 0 ) WRITE( out, 2530 ) GTYPES( itype )
        END IF
      END DO
      RETURN

!  allocation errors

  980 CONTINUE
      WRITE( out, "( ' ** Message from INTERPRET_gpsmps-',                     &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  non-executable statements

 1000 FORMAT( A72 )
 1010 FORMAT( A160 )
 2000 FORMAT( ' ** Exit from MAKE_group_ad - insufficient memory available',   &
              ' to enlarge hash table' )
 2010 FORMAT( ' ** Exit from MAKE_group_ad - warning.',                        &
              ' First card not groups. ', /, '    A dummy',                    &
              ' routine will be substituted ' )
 2020 FORMAT( ' ** Exit from MAKE_group_ad - indicator card not recognised ' )
 2190 FORMAT( ' ** Exit from MAKE_group_ad - group type not recognised:',      &
              ' name is ', A10 )
 2510 FORMAT( ' ** Exit from MAKE_group_ad -',                                 &
              ' Name on card not that specified on input ' )
 2520 FORMAT( ' ** Exit from MAKE_group_ad -',                                 &
              ' data file incomplete. No ENDATA card ' )
 2530 FORMAT( ' ** Exit from MAKE_group_ad - warning, group type ', A8,        &
              ' undefined ' )
 2540 FORMAT( ' ** Exit from MAKE_group_ad -',                                 &
              ' unrecognised field 1 in TEMPORARIES section' )
 2550 FORMAT( ' ** Exit from MAKE_group_ad -',                                 &
              ' unrecognised field 1 in GLOBALS section' )
 2560 FORMAT( ' ** Exit from MAKE_group_ad -',                                 &
              ' unrecognised field 1 in INDIVIDUAL section' )
 2570 FORMAT( ' ** Exit from MAKE_group_ad -',                                 &
              ' undefined parameter in GLOBALS section' )
 2580 FORMAT( ' ** Exit from MAKE_group_ad -',                                 &
              ' undefined parameter in INDIVIDUALS section' )
 2590 FORMAT( ' ** Exit from MAKE_group_ad - repeated parameter name ', A8 )
 2610 FORMAT( ' ** Exit from MAKE_group_ad - function not set '  )
 2640 FORMAT( ' ** Exit from MAKE_group_ad - group type already defined ' )
 2730 FORMAT( ' ** Exit from MAKE_group_ad - field 2 or 3 not blank on',       &
              ' A, F, G or H card ' )
 2900 FORMAT( ' ' )
 2970 FORMAT( ' Line ', i5, 4X, A160 )
 2980 FORMAT( ' Line ', i5, '.', i2, 1X, A65 )
 2990 FORMAT( ' Line ', i5, 4X, A65 )
 3000 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 4( A6, ', ' ), A6, ' )', /,         &
              '      INTEGER ', 3( A6, ', ' ), A6, /,                          &
              '      INTEGER ', 3( A6, ', ' ), A6, /,                          &
              '      LOGICAL ', A6, /,                                         &
              '      INTEGER ', A6, '(', A6, '), ', A6, '(', A6,               &
                             '), ', A6, '(', A6, ')', /,                       &
              '      DOUBLE PRECISION ', A6, '(', A6, ',3), ',                 &
                                         A6, '(', A6, '), ', A6,               &
                                      '(', A6, ')', /,                         &
              'C', /, 'C  Problem name : ', A10, /,                            &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C' )
 3001 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 4( A6, ', ' ), A6, ' )', /,         &
              '      INTEGER ', 3( A6, ', ' ), A6, /,                          &
              '      INTEGER ', 3( A6, ', ' ), A6, /,                          &
              '      LOGICAL ', A6, /,                                         &
              '      INTEGER ', A6, '(', A6, '), ', A6, '(', A6,               &
                             '), ', A6, '(', A6, ')', /,                       &
              '      REAL             ', A6, '(', A6, ',3), ',                 &
                                         A6, '(', A6, '), ', A6,               &
                                      '(', A6, ')', /,                         &
              'C', /, 'C  Problem name : ', A10, /,                            &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C' )
 3002 FORMAT( '      INTEGER ', A6, ', ', A6, ', ', A6, ', ', A6 )
 3004 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 4( A6, ', ' ), A6, ' )', /,         &
              '      USE HSL_', A4, '_', A15, /,                               &
              '      INTEGER ', 3( A6, ', ' ), A6, /,                          &
              '      INTEGER ', 3( A6, ', ' ), A6, /,                          &
              '      LOGICAL ', A6, /,                                         &
              '      INTEGER ', A6, '(', A6, '), ', A6, '(', A6,               &
                             '), ', A6, '(', A6, ')', /,                       &
              '      DOUBLE PRECISION ', A6, '(', A6, ',3), ',                 &
                                         A6, '(', A6, '), ', A6,               &
                                      '(', A6, ')', /,                         &
              'C', /, 'C  Problem name : ', A10, /,                            &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
              '      INTEGER, POINTER :: H_index( : ) ', /,                    &
              '      DOUBLE PRECISION, POINTER :: H_result( : ) ', /,          &
              '      DOUBLE PRECISION :: A_int( 1 ) ' )
 3005 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 4( A6, ', ' ), A6, ' )', /,         &
              '      USE HSL_', A4, '_', A15, /,                               &
              '      INTEGER ', 3( A6, ', ' ), A6, /,                          &
              '      INTEGER ', 3( A6, ', ' ), A6, /,                          &
              '      LOGICAL ', A6, /,                                         &
              '      INTEGER ', A6, '(', A6, '), ', A6, '(', A6,               &
                             '), ', A6, '(', A6, ')', /,                       &
              '      REAL             ', A6, '(', A6, ',3), ',                 &
                                         A6, '(', A6, '), ', A6,               &
                                      '(', A6, ')', /,                         &
              'C', /, 'C  Problem name : ', A10, /,                            &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
              '      INTEGER, POINTER :: H_index( : ) ', /,                    &
              '      REAL, POINTER :: H_result( : ) ', /,                      &
              '      REAL :: A_int( 1 ) ' )
 3006 FORMAT( '      TYPE (AD01_REAL) :: G_value = AD01_UNDEFINED', /,         &
              '      TYPE (AD01_REAL) :: A_value( 1 )' )
 3007 FORMAT( '      INTEGER :: ERROR_AD02', /,                                &
              '      TYPE (AD02_REAL) :: G_value', /,                          &
              '      TYPE (AD02_REAL) :: A_value( 1 )', /,                     &
              '      TYPE (AD02_DATA), POINTER :: DATA_AD02' )
 3009 FORMAT( '      ', A6, ' = 0' )
 3010 FORMAT( ( '      INTEGER ', A6, 4( :, ', ', A6 ) ) )
!3011 format( '      nullify( data_ad02 )', /,
!    *        '      call ad02_initialize(2, g_value,', /,
!    *        '     *                     gvalue(1,1),', /,
!    *        '     *                     data_ad02, 0)' )
 3011 FORMAT( '      CALL AD02_INITIALIZE_DATA(DATA_AD02, ERROR_AD02)' )
!3015 format( '       call ', a4, '_undefine( ', a6,
!    *        ', data_ad02 )' )
 3016 FORMAT( '      CALL ', A4, '_UNDEFINE( ', A6,                            &
              ', DATA_AD02 )' )
 3017 FORMAT( ( '      TYPE (', A4, '_REAL) :: ', A6 ) )
 3018 FORMAT( ( '      TYPE (AD01_REAL) :: ', A6,                              &
                ' = AD01_UNDEFINED' ) )
 3019 FORMAT( ( '      REAL             ', A6, 4( :, ', ', A6 ) ) )
 3020 FORMAT( ( '      DOUBLE PRECISION ', A6, 4( :, ', ', A6 ) ) )
 3021 FORMAT( ( '      INTRINSIC ', A6, 4( :, ', ', A6 ) ) )
 3022 FORMAT( ( '      EXTERNAL ', A6, 4( :, ', ', A6 ) ) )
 3023 FORMAT( ( '      LOGICAL ', A6, 4( :, ', ', A6 ) ) )
 3030 FORMAT( '      ', A6, ' = ', A41 )
 3031 FORMAT( '      IF (', A6, ') ', A6, ' = ', A41 )
 3032 FORMAT( '      IF (.NOT.', A6, ') ', /,                                  &
              '     *   ', A6, ' = ', A41 )
 3040 FORMAT( '     *         ', A41 )
 3050 FORMAT( '      DO ', i5, 1X, A6, ' = 1, ', A6, /,                        &
              '       ', A6, ' = ', A6, '(', A6, ')', /,                       &
              '       ', A6, ' = ', A6, '(', A6, ')', /,                       &
              '       IF ( ', A6, ' == 0 ) GO TO ', i5, /,                     &
              '       ', A6, ' = ', A6, '(', A6, ') - 1' )
 3051 FORMAT( '       GO TO (', 8( i5, :, ',' ), /,                            &
            ( '     *        ', 8( i5, :, ',' ) ) )
 3052 FORMAT( '     *        ', 48X, '), ', A6 )
 3060 FORMAT( 'C', /, 'C  Group type : ', A8, /, 'C' )
 3061 FORMAT( i5, '  CONTINUE ' )
 3062 FORMAT( '       ', A6, '= ', A6, '(', A6, ')' )
 3063 FORMAT( '       ', A6, '= ', A6, '(', A6, '+', i6, ')' )
 3064 FORMAT( '       A_int( 1 ) = ', A6, '(', A6, ')', /,                     &
              '       CALL AD01_INITIALIZE(2, A_value( : 1 ),',                &
                      ' A_int( : 1 ), 0) ', /,                                 &
              '       ', A6, ' = A_value( 1 ) ' )
 3065 FORMAT( '       A_int( 1 ) = ', A6, '(', A6, ')', /,                     &
              '       CALL AD02_INITIALIZE_COMP(2, A_value( : 1 ),',           &
                      ' A_int( : 1 ),', /,                                     &
              '     *                      DATA_AD02, ERROR_AD02, 0)',         &
           /, '       ', A6, ' = A_value( 1 ) ' )
 3080 FORMAT( '       ', A6, '= ', A41 )
 3081 FORMAT( '       IF (', A6, ') ', A6, ' = ', A41 )
 3082 FORMAT( '       IF (.NOT.', A6, ') ', A6, ' = ', A41 )
 3083 FORMAT( '        ', A6, ' = ', A41 )
 3084 FORMAT( '        IF (', A6, ') ', A6, ' = ', A41 )
 3085 FORMAT( '        IF (.NOT.', A6, ') ', A6, ' = ', A41 )
 3090 FORMAT( '     *          ', A41 )
 3091 FORMAT( '     *           ', A41 )
 3100 FORMAT( '       IF ( .NOT. ', A6, ' ) ',A6,'(', A6, ',1)= ', A41 )
 3101 FORMAT( '       G_value = ', A41 )
 3110 FORMAT( '     *                  ', A41 )
 3122 FORMAT( '       IF ( ', A6, ' ) THEN', /,                                &
              '        CALL AD01_HESSIAN(G_value, ',                           &
              A6, '(', A6, ',3), ', A6, '(', A6, ',2))', /,                    &
              '       ELSE',/,                                                 &
              '        CALL AD01_VALUE(G_value, ',A6,'(', A6, ',1))' )
 3123 FORMAT( '       IF ( ', A6, ' ) THEN', /,                                &
              '        CALL AD02_HESSIAN(G_value, ',                           &
              A6, '(', A6, ',3), ERROR_AD02,', /,                              &
              '     *                    ', A6, '(', A6, ',2))', /,            &
              '       ELSE',/,                                                 &
              '        CALL AD02_VALUE(G_value, ',A6,'(', A6, ',1),',          &
              ' ERROR_AD02)' )
 3190 FORMAT( '       END IF' )
 3191 FORMAT( '       GO TO', i6 )
 3192 FORMAT( '      CALL AD02_FINALIZE_DATA(DATA_AD02, ERROR_AD02)' )
 3200 FORMAT( i5,  ' CONTINUE' )
 3210 FORMAT( '      RETURN', /,  '      END' )

!  end of subroutine MAKE_group_ad

      END SUBROUTINE MAKE_group_ad

!-*-*-*-*- S I F D E C O D E   O U T R A N G E   S U B R O U T I N E -*-*-*-*-

      SUBROUTINE OUTRANGE( nelv, ninv, U, outfn, outra, EVNAMES, IVNAMES,      &
                           single )
      INTEGER :: nelv, ninv, outfn, outra
      LOGICAL :: single
      REAL ( KIND = wp ) :: U( ninv, nelv )
      CHARACTER ( LEN = 10 ) :: EVNAMES( * ), IVNAMES( * )

!  --------------------------------------------------------------------
!  print out the gather and scatter part of the generated range routine
!  and the gather part of the generated function evaluation routine
!  --------------------------------------------------------------------

!  local variables

      INTEGER :: i, j, k
      REAL ( KIND = wp ) :: uij
      LOGICAL :: anynnz
      CHARACTER ( LEN = 6 ) :: evname, ivname

!  print out the scatter part

      DO j = 1, nelv
        k = 0
        anynnz = .FALSE.
        DO i = 1, ninv
          uij = U( i, j )

!  ignore zero entries

          IF ( DABS( uij ) <= epsmch ) CYCLE
          k = k + 1
          IF ( uij > zero ) THEN

!  the nonzero is positive

            IF ( DABS( uij - one ) <= epsmch ) THEN

!  special case if nonzero has the value 1

              IF ( anynnz ) THEN
                WRITE( outra, 4030 ) i
              ELSE
                WRITE( outra, 4040 ) j, i
              END IF

!  nonzero has a value other than 1

            ELSE
              IF ( anynnz ) THEN
                WRITE( outra, 4050 ) i, uij
              ELSE
                WRITE( outra, 4060 ) j, i, uij
              END IF
            END IF

!  the nonzero is negative

          ELSE
            IF ( DABS( - uij - one ) <= epsmch ) THEN

!  special case if nonzero has the value - 1

              IF ( anynnz ) THEN
                WRITE( outra, 4070 ) i
              ELSE
                WRITE( outra, 4080 ) j, i
              END IF

!  nonzero has a value other than - 1

            ELSE
              IF ( anynnz ) THEN
                WRITE( outra, 4090 ) i, - uij
              ELSE
                WRITE( outra, 4100 ) j, i, - uij
              END IF
            END IF
          END IF
          anynnz = .TRUE.
          IF ( MOD( k, 19 ) == 0 ) WRITE( outra, 4112 ) j, j
        END DO
        IF ( .NOT. anynnz ) THEN
          IF ( single ) THEN
            WRITE( outra, 4111 ) j
          ELSE
            WRITE( outra, 4110 ) j
          END IF
        END IF
      END DO

!  ----- the scatter has been completed; start the gather

      WRITE( outra, 4010 )

!  print out the gather part

      DO i = 1, ninv
        k = 0
        anynnz = .FALSE.
        ivname = IVNAMES( i )( 1 : 6 )
        DO j = 1, nelv
          evname = EVNAMES( j )( 1 : 6 )
          uij = U( i, j )

!  ignore zero entries

          IF ( DABS( uij ) <= epsmch ) CYCLE
          k = k + 1
          IF ( uij > zero ) THEN

!  the nonzero is positive

            IF ( DABS( uij - one ) <= epsmch ) THEN

!  special case if nonzero has the value 1

              IF ( anynnz ) THEN
                WRITE( outfn, 3030 ) evname
                WRITE( outra, 4030 ) j
              ELSE
                WRITE( outfn, 3040 ) ivname, evname
                WRITE( outra, 4040 ) i, j
              END IF
            ELSE

!  nonzero has a value other than 1

              IF ( anynnz ) THEN
                WRITE( outfn, 3050 ) evname, uij
                WRITE( outra, 4050 ) j, uij
              ELSE
                WRITE( outfn, 3060 ) ivname, evname, uij
                WRITE( outra, 4060 ) i, j, uij
              END IF
            END IF

!  the nonzero is negative

          ELSE
            IF ( DABS( - uij - one ) <= epsmch ) THEN

!  special case if nonzero has the value - 1

              IF ( anynnz ) THEN
                WRITE( outfn, 3070 ) evname
                WRITE( outra, 4070 ) j
              ELSE
                WRITE( outfn, 3080 ) ivname, evname
                WRITE( outra, 4080 ) i, j
              END IF

!  nonzero has a value other than - 1

            ELSE
              IF ( anynnz ) THEN
                WRITE( outfn, 3090 ) evname, - uij
                WRITE( outra, 4090 ) j, - uij
              ELSE
                WRITE( outfn, 3100 ) ivname, evname, - uij
                WRITE( outra, 4100 ) i, j, - uij
              END IF
            END IF
          END IF
          anynnz = .TRUE.
          IF ( MOD( k, 19 ) == 0 ) THEN
            WRITE( outfn, 3040 ) ivname, ivname
            WRITE( outra, 4112 ) i, i
          END IF
        END DO
        IF ( .NOT. anynnz ) THEN
          IF ( single ) THEN
            WRITE( outfn, 3111 ) ivname
            WRITE( outra, 4111 ) i
          ELSE
            WRITE( outfn, 3110 ) ivname
            WRITE( outra, 4110 ) i
          END IF
        END IF
      END DO

!  ----- the gather has been completed; wind up the element

      WRITE( outra, 4020 )
      RETURN

!  non-executable statements

 3030 FORMAT( '     *          + ', A6 )
 3040 FORMAT( '       ', A6, ' =   ', A6 )
 3050 FORMAT( '     *          + ', A6, ' * ', F12.5 )
 3060 FORMAT( '       ', A6, ' =   ', A6, ' * ', F12.5 )
 3070 FORMAT( '     *          - ', A6 )
 3080 FORMAT( '       ', A6, ' = - ', A6 )
 3090 FORMAT( '     *          - ', A6, ' * ', F12.5 )
 3100 FORMAT( '       ', A6, ' = - ', A6, ' * ', F12.5 )
 3110 FORMAT( '       ', A6, ' = 0.0D+0 ' )
 3111 FORMAT( '       ', A6, ' = 0.0E+0 ' )
 4010 FORMAT( '      ELSE' )
 4020 FORMAT( '      END IF', /, '      RETURN' )
 4030 FORMAT( '     *                 + W1(', i6, ' ) ' )
 4040 FORMAT( '         W2(', i6, ' ) =   W1(', i6, ' ) ' )
 4050 FORMAT( '     *                 + W1(', i6, ' ) * ', F12.5 )
 4060 FORMAT( '         W2(', i6, ' ) =   W1(', i6, ' ) * ', F12.5 )
 4070 FORMAT( '     *                 - W1(', i6, ' ) ' )
 4080 FORMAT( '         W2(', i6, ' ) = - W1(', i6, ' ) ' )
 4090 FORMAT( '     *                 - W1(', i6, ' ) * ', F12.5 )
 4100 FORMAT( '         W2(', i6, ' ) = - W1(', i6, ' ) * ', F12.5 )
 4110 FORMAT( '         W2(', i6, ' ) = 0.0D+0 ' )
 4111 FORMAT( '         W2(', i6, ' ) = 0.0E+0 ' )
 4112 FORMAT( '         W2(', i6, ' ) =   W2(', i6, ' ) ' )

!  end of subroutine OUTRANGE

      END SUBROUTINE OUTRANGE

!-*-*- S I F D E C O D E   O U T R A N G E _ a d    S U B R O U T I N E -*-*-

      SUBROUTINE OUTRANGE_ad( nelv, ninv, U, outff, outfd, outra,              &
                              EVNAMES, IVNAMES, single, ad0 )
      INTEGER :: nelv, ninv, outff, outfd, outra
      LOGICAL :: single
      CHARACTER ( LEN = 4 ) :: ad0
      REAL ( KIND = wp ) :: U( ninv, nelv )
      CHARACTER ( LEN = 10 ) :: EVNAMES( * ), IVNAMES( * )

!  --------------------------------------------------------------------
!  print out the gather and scatter part of the generated range routine
!  and the gather part of the generated function evaluation routine
!  --------------------------------------------------------------------

!  local variables

      INTEGER :: i, j, k
      REAL ( KIND = wp ) :: uij
      LOGICAL :: anynnz, loutff
      CHARACTER ( LEN = 6 ) :: evname, ivname

      loutff = outff > 0

!  print out the scatter part

      DO j = 1, nelv
        k = 0
        anynnz = .FALSE.
        DO i = 1, ninv
          uij = U( i, j )

!  ignore zero entries

          IF ( DABS( uij ) <= epsmch ) CYCLE
          k = k + 1
          IF ( uij > zero ) THEN

!  the nonzero is positive

            IF ( DABS( uij - one ) <= epsmch ) THEN

!  special case if nonzero has the value 1

              IF ( anynnz ) THEN
                WRITE( outra, 4030 ) i
              ELSE
                WRITE( outra, 4040 ) j, i
              END IF

!  nonzero has a value other than 1

            ELSE
              IF ( anynnz ) THEN
                WRITE( outra, 4050 ) i, uij
              ELSE
                WRITE( outra, 4060 ) j, i, uij
              END IF
            END IF

!  the nonzero is negative

          ELSE
            IF ( DABS( - uij - one ) <= epsmch ) THEN

!  special case if nonzero has the value - 1

              IF ( anynnz ) THEN
                WRITE( outra, 4070 ) i
              ELSE
                WRITE( outra, 4080 ) j, i
              END IF
            ELSE

!  nonzero has a value other than - 1

              IF ( anynnz ) THEN
                WRITE( outra, 4090 ) i, - uij
              ELSE
                WRITE( outra, 4100 ) j, i, - uij
              END IF
            END IF
          END IF
          anynnz = .TRUE.
          IF ( MOD( k, 19 ) == 0 ) WRITE( outra, 4112 ) j, j
        END DO
        IF ( .NOT. anynnz ) THEN
          IF ( single ) THEN
            WRITE( outra, 4111 ) j
          ELSE
            WRITE( outra, 4110 ) j
          END IF
        END IF
      END DO

!  ----- the scatter has been completed; start the gather

      WRITE( outra, 4010 )

!  print out the gather part

      DO i = 1, ninv
        k = 0
        anynnz = .FALSE.
        ivname = IVNAMES( i )( 1 : 6 )
        DO j = 1, nelv
          evname = EVNAMES( j )( 1 : 6 )
          uij = U( i, j )

!  ignore zero entries

          IF ( DABS( uij ) <= epsmch ) CYCLE
          k = k + 1
          IF ( uij > zero ) THEN

!  the nonzero is positive

            IF ( DABS( uij - one ) <= epsmch ) THEN

!  special case if nonzero has the value 1

              IF ( anynnz ) THEN
                IF ( loutff ) WRITE( outff, 3030 ) evname
                WRITE( outfd, 3030 ) evname
                WRITE( outra, 4030 ) j
              ELSE
                IF ( loutff ) WRITE( outff, 3040 ) ivname, evname
                WRITE( outfd, 3041 ) ad0, i, evname
                WRITE( outra, 4040 ) i, j
              END IF

!  nonzero has a value other than 1

            ELSE
              IF ( anynnz ) THEN
                IF ( loutff ) WRITE( outff, 3050 ) evname, uij
                WRITE( outfd, 3050 ) evname, uij
                WRITE( outra, 4050 ) j, uij
              ELSE
                IF ( loutff ) WRITE( outff, 3060 ) ivname, evname, uij
                WRITE( outfd, 3061 ) ad0, i, evname, uij
                WRITE( outra, 4060 ) i, j, uij
              END IF
            END IF

!  the nonzero is negative

          ELSE
            IF ( DABS( - uij - one ) <= epsmch ) THEN

!  special case if nonzero has the value - 1

              IF ( anynnz ) THEN
                IF ( loutff ) WRITE( outff, 3070 ) evname
                WRITE( outfd, 3070 ) evname
                WRITE( outra, 4070 ) j
              ELSE
                IF ( loutff ) WRITE( outff, 3080 ) ivname, evname
                WRITE( outfd, 3081 ) ad0, i, evname
                WRITE( outra, 4080 ) i, j
              END IF
            ELSE

!  nonzero has a value other than - 1

              IF ( anynnz ) THEN
                IF ( loutff ) WRITE( outff, 3090 ) evname, - uij
                WRITE( outfd, 3090 ) evname, - uij
                WRITE( outra, 4090 ) j, - uij
              ELSE
                IF ( loutff ) WRITE( outff, 3100 ) ivname, evname, - uij
                WRITE( outfd, 3101 ) ad0, i, evname, - uij
                WRITE( outra, 4100 ) i, j, - uij
              END IF
            END IF
          END IF
          anynnz = .TRUE.
          IF ( MOD( k, 19 ) == 0 ) THEN
            IF ( loutff ) WRITE( outff, 3040 ) ivname, ivname
            WRITE( outfd, 3041 ) ad0, i, ivname
            WRITE( outra, 4112 ) i, i
          END IF
        END DO
        IF ( .NOT. anynnz ) THEN
          IF ( single ) THEN
            IF ( loutff ) WRITE( outff, 3120 ) ivname
            WRITE( outfd, 3121 ) ad0, i
            WRITE( outra, 4111 ) i
          ELSE
            IF ( loutff ) WRITE( outff, 3110 ) ivname
            WRITE( outfd, 3111 ) ad0, i
            WRITE( outra, 4110 ) i
          END IF
        END IF
        IF ( ad0 == 'AD01' ) THEN
           WRITE( outfd, 3150 ) i, i
        ELSE
           WRITE( outfd, 3151 ) i, i
        END IF
      END DO

!  ----- the gather has been completed; wind up the element

      WRITE( outra, 4020 )
      IF ( ad0 == 'AD01' ) THEN
         WRITE( outfd, 3130 ) ninv, ninv
      ELSE
         WRITE( outfd, 3131 ) ninv, ninv
      END IF
      DO i = 1, ninv
         ivname = IVNAMES( i )( 1 : 6 )
         WRITE( outfd, 3140 ) ivname, i
      END DO
      RETURN

!  non-executable statements

 3030 FORMAT( '     *          + ', A6 )
 3040 FORMAT( '       ', A6, ' =   ', A6 )
 3041 FORMAT( '       X_', A4, '_int(', i6, ') =   ', A6 )
 3050 FORMAT( '     *          + ', A6, ' * ', F12.5 )
 3060 FORMAT( '       ', A6, ' =   ', A6, ' * ', F12.5 )
 3061 FORMAT( '       X_', A4, '_int(', i6, ') =   ', A6, ' * ', F12.5 )
 3070 FORMAT( '     *          - ', A6 )
 3080 FORMAT( '       ', A6, ' = - ', A6 )
 3081 FORMAT( '       X_', A4, '_int(', i6, ') = - ', A6 )
 3090 FORMAT( '     *          - ', A6, ' * ', F12.5 )
 3100 FORMAT( '       ', A6, ' = - ', A6, ' * ', F12.5 )
 3101 FORMAT( '       X_', A4, '_int(', i6, ') = - ', A6, ' * ', F12.5 )
 3110 FORMAT( '       ', A6, ' = 0.0D+0 ' )
 3111 FORMAT( '       X_', A4, '_int(', i6, ') = 0.0D+0 ' )
 3120 FORMAT( '       ', A6, ' = 0.0E+0 ' )
 3121 FORMAT( '       X_', A4, '_int(', i6, ') = 0.0E+0 ' )
 3130 FORMAT( '       CALL AD01_INITIALIZE(IFFLAG - 1, X_value(:', i6,         &
                     '),', /, '     *', 22X, 'X_int(:', i6, '), 0) ')
 3131 FORMAT( '       CALL AD02_INITIALIZE_COMP(IFFLAG - 1, X_value(:',        &
                      i6, '),', /, '     *', 22X, 'X_int(:', i6, '),',         &
              ' DATA_AD02, ERROR_AD02, 0)' )
 3140 FORMAT( '       ', A6, ' = X_value(', i6, ')' )
 3150 FORMAT( '       CALL AD01_VALUE(X_AD01_int(', i6,                        &
                      '), X_int(', i6, '))' )
 3151 FORMAT( '       CALL AD02_VALUE(X_AD02_int(', i6,                        &
                      '), X_int(', i6, '), ERROR_AD02)' )
 4010 FORMAT( '      ELSE' )
 4020 FORMAT( '      END IF', /, '      RETURN' )
 4030 FORMAT( '     *                 + W1(', i6, ' ) ' )
 4040 FORMAT( '         W2(', i6, ' ) =   W1(', i6, ' ) ' )
 4050 FORMAT( '     *                 + W1(', i6, ' ) * ', F12.5 )
 4060 FORMAT( '         W2(', i6, ' ) =   W1(', i6, ' ) * ', F12.5 )
 4070 FORMAT( '     *                 - W1(', i6, ' ) ' )
 4080 FORMAT( '         W2(', i6, ' ) = - W1(', i6, ' ) ' )
 4090 FORMAT( '     *                 - W1(', i6, ' ) * ', F12.5 )
 4100 FORMAT( '         W2(', i6, ' ) = - W1(', i6, ' ) * ', F12.5 )
 4110 FORMAT( '         W2(', i6, ' ) = 0.0D+0 ' )
 4111 FORMAT( '         W2(', i6, ' ) = 0.0E+0 ' )
 4112 FORMAT( '         W2(', i6, ' ) =   W2(', i6, ' ) ' )

!  end of subroutine OUTRANGE_ad

      END SUBROUTINE OUTRANGE_ad

!-*-*-*-*-*- S I F D E C O D E   N E W _ N A M E    F U N C T I O N -*-*-*-*-*-

      FUNCTION new_name( i1, i2, i3, i4, i5, i6, iires, nrenames, ninnames,    &
                         nlonames, nminames, nexnames, neltype, yname, FIELDI, &
                         RENAMES, INNAMES, LONAMES, MINAMES, EXNAMES, ETYPES )
      CHARACTER ( LEN = 6 ) :: new_name
      INTEGER :: i1, i2, i3, i4, i5, i6, iires
      INTEGER :: nrenames, ninnames, nlonames, nminames, nexnames, neltype
      CHARACTER ( LEN = 6 ) :: yname
      CHARACTER ( LEN = 8 ) :: FIELDI( iires )
      CHARACTER ( LEN = 10 ) :: RENAMES( nrenames ), INNAMES( ninnames )
      CHARACTER ( LEN = 10 ) :: LONAMES( nlonames )
      CHARACTER ( LEN = 10 ) :: MINAMES( nminames ), EXNAMES( nexnames )
      CHARACTER ( LEN = 10 ) :: ETYPES( neltype )

!  -------------------------------------------------
!  find a name that does not occur in any other list
!  -------------------------------------------------

!  local variables

      INTEGER :: i
      CHARACTER ( LEN = 1 ) :: CHARAC( 36 )
      DATA CHARAC / 'Z', 'Y', 'X', 'W', 'V', 'U', 'T', 'S', 'R',               &
                    'Q', 'P', 'O', 'N', 'M', 'L', 'K', 'J', 'I',               &
                    'H', 'G', 'F', 'E', 'D', 'C', 'B', 'A', '0',               &
                    '9', '8', '7', '6', '5', '4', '3', '2', '1' /

   10 CONTINUE

!  find the next name in the list

      i1 = i1 + 1
      IF ( i1 == 27 ) THEN
        i1 = 1
        i2 = i2 + 1
        IF ( i2 == 37 ) THEN
          i2 = 1
          i3 = i3 + 1
          IF ( i3 == 37 ) THEN
            i3 = 1
            i4 = i4 + 1
            IF ( i4 == 37 ) THEN
              i4 = 1
              i5 = i5 + 1
              IF ( i5 == 37 ) THEN
                i5 = 1
                i6 = i6 + 1
                IF ( i6 == 37 ) THEN
                  WRITE( 6, * ) ' no characters left '
                END IF
              END IF
            END IF
          END IF
        END IF
      END IF
      new_name = CHARAC( i1 ) // CHARAC( i2 ) // CHARAC( i3 ) //               &
                 CHARAC( i4 ) // CHARAC( i5 ) // CHARAC( i6 )

!  see if the name has already been used

      DO i = 1, nrenames
        IF ( RENAMES( i )( 1 : 6 ) == new_name ) GO TO 10
      END DO
      DO i = 1, ninnames
        IF ( INNAMES( i )( 1 : 6 ) == new_name ) GO TO 10
      END DO
      DO i = 1, nlonames
        IF ( LONAMES( i )( 1 : 6 ) == new_name ) GO TO 10
      END DO
      DO i = 1, nminames
        IF ( MINAMES( i )( 1 : 6 ) == new_name ) GO TO 10
      END DO
      DO i = 1, nexnames
        IF ( EXNAMES( i )( 1 : 6 ) == new_name ) GO TO 10
      END DO
      DO i = 1, neltype
        IF ( ETYPES( i )( 1 : 6 ) == new_name ) GO TO 10
      END DO
      DO i = 1, iires
        IF ( FIELDI( i )( 1 : 6 ) == new_name ) GO TO 10
      END DO
      IF ( new_name == yname ) GO TO 10
      RETURN

!  end of function new_name

      END FUNCTION new_name

!-*-*-*- S I F D E C O D E   F R E E _ F O R M A T   S U B R O U T I N E -*-*-*-

      SUBROUTINE FREE_format( nuline, leline, mendat, INDIC8, LENIND, NULINA,  &
                              maxnul, nlines, issdif, status, out )
      INTEGER :: leline, mendat, maxnul, nlines, status, out
      LOGICAL :: issdif
      INTEGER :: LENIND( mendat )
      CHARACTER ( LEN = 1 ) :: NULINE( leline )
      CHARACTER ( LEN = 12 ) :: INDIC8( mendat )
      CHARACTER ( LEN = 65 ) :: NULINA( maxnul )

!  ----------------------------------------------------
!  construct a fixed format line from a free format one
!  ----------------------------------------------------

!  local variables

      INTEGER :: i, j, k, nfield, nstfie, lfield, len, icard
      LOGICAL :: field, nextl, waitnl, spsal2
      INTEGER, DIMENSION( 6, 2 ), PARAMETER :: LENFIE =                        &
        RESHAPE( (/ 2, 10, 10, 12, 10, 12, 2, 10, 10, 41, 0, 0 /), (/ 6, 2 /) )
      INTEGER, DIMENSION( 6, 2 ), PARAMETER :: ISTFIE =                        &
        RESHAPE( (/ 2, 5, 15, 25, 40, 50, 2, 5, 15, 25, 65, 65 /), (/ 6, 2 /) )
      INTEGER, DIMENSION( 2 ), PARAMETER :: NFIE = (/ 6, 4 /)

!  if issdif is .true., the call is made from subroutine gps, where the
!  card length is 61. otherwise, the card length is 65

      IF ( issdif ) THEN
        icard = 1
      ELSE
        icard = 2
      END IF
      nfield = 0
      nlines = 0
      field = .FALSE.
      nextl = .FALSE.
      spsal2 = .FALSE.
      waitnl = .TRUE.

!  process the next character on the card

      DO 500 i = 1, leline

!  copy comments unchanged

        IF ( waitnl .AND. NULINE( i ) == '$' ) THEN
          nlines = nlines + 1
          DO j = 1, 65
            NULINA( nlines )( j : j ) = ' '
          END DO
          NULINA( nlines )( 1 : 1 ) = '*'
          DO j = 2, leline + 1 - i
            NULINA( nlines )( j : j ) = NULINE( i + j - 1 )
          END DO
          GO TO 600
        END IF

!  if we are looking for an end of line marker, check whether we have
!  found it

        IF ( nextl ) THEN
          IF ( NULINE( i ) == ';' ) THEN
            nextl = .FALSE.
            waitnl = .TRUE.

!  reset the card type to 2 when a special card has been finished

            IF ( spsal2 ) THEN
              spsal2 = .FALSE.
              icard = 2
            END IF
          END IF
          GO TO 500
        END IF

!  next check whether we have found an end of line marker anyway

        IF ( NULINE( i ) == ';' ) THEN
          waitnl = .TRUE.

!  finish off the current line

          j = ISTFIE( nfield, icard ) - 1
          DO k = 1, lfield
            NULINA( nlines )( j + K: j + k ) = NULINE( nstfie + k )
          END DO
          field = .FALSE.

!  reset the card type to 2 when a special card has been finished

          IF ( spsal2 ) THEN
            spsal2 = .FALSE.
            icard = 2
          END IF
          GO TO 500
        END IF

!  a field has been started

        IF ( field ) THEN

!  the field has now come to an end

          IF ( ( NULINE( i ) == ' ' .AND. .NOT.                                &
               ( icard == 2 .AND. nfield == 4 ) ) .OR. NULINE( i ) == '_' ) THEN
            field = .FALSE.

!  store the field in its correct position in nulina

            j = ISTFIE( nfield, icard ) - 1
            DO k = 1, lfield
              NULINA( nlines )( j + K: j + k ) = NULINE( nstfie + k )
            END DO

!  the field has now come to an end and a blank field follows

            IF ( NULINE( i ) == '_' ) THEN
              nfield = nfield + 1
              lfield = 0
              IF ( nfield > NFIE( icard ) ) THEN
                 status = 45
                 WRITE( out, 2450 )
                 RETURN
              END IF
            END IF
          ELSE

!  an extra character has been added to the field

            lfield = lfield + 1

!  check that the field has not exceeded its allowed space
!  this may happen if a) there is an error on the card, b) the
!  card is of type 2 and only blanks remain or c) the field is
!  actually an indicator card. check which

            IF ( LENFIE( nfield, icard ) < lfield ) THEN

!  if we are considering field 4 when the card is of type 2 and
!  all the space has been exhausted, finish the line

              IF ( icard == 2 .AND. nfield == 4 ) THEN
                waitnl = .TRUE.
                j = ISTFIE( nfield, icard ) - 1
                DO k = 1, lfield
                  NULINA( nlines )( j + K: j + k ) = NULINE( nstfie + k )
                END DO
                field = .FALSE.
                GO TO 500
              END IF

!  there is an error in the current field

              IF ( nfield > 1 ) THEN
                status = 44
                WRITE( out, 2440 ) nfield
                RETURN

!  the first field may be an indicator card. check

              ELSE
                DO 70 j = 2, mendat
                  LEN = LENIND( j )
                  DO k = 1, len
                    IF ( NULINE( nstfie + k ) /= INDIC8( j )( K: k ) ) GO TO 70
                  END DO
                  GO TO 80
   70           CONTINUE

!  the indicator card is unknown. exit with an error message

                status = 2
                WRITE( out, 2020 )
                RETURN

!  the indicator card is recognised. output this card as the next
!  line and await a further line. (the title card is an exception as
!  the title has still to arrive)

   80           CONTINUE
                IF ( j /= 4 ) THEN
                  field = .FALSE.
                  nextl = .TRUE.
                  NULINA( nlines )( 1 : 12 ) = INDIC8( j )( 1 : 12 )
                 END IF
              END IF
            END IF
          END IF

!  we are between fields

        ELSE

!  a new field has started

          IF ( NULINE( i ) /= ' ' ) THEN

!  it is the first field

            IF ( waitnl ) THEN
              waitnl = .FALSE.
              nlines = nlines + 1

!  initialize the new line, nulina( nlines ), as a blank line

              DO j = 1, 65
                NULINA( nlines )( J: j ) = ' '
              END DO
              nfield = 1
              IF ( NULINE( i ) == '_' ) lfield = 0

!  if a special card occurs (that is, a card on which the range
!  transformation is specified within MAKE_elfun), mark it

              IF ( NULINE( i ) == 'R' .AND. icard == 2 ) THEN
                spsal2 = .TRUE.
                icard = 1
              END IF
            ELSE

!  it is not the first field

              nfield = nfield + 1
              IF ( nfield > NFIE( icard ) ) THEN
                status = 45
                WRITE( out, 2450 )
                RETURN
              END IF

!  if the string is in fields 3 or 5 and starts with a '$', the
!  remainder of the card is considered to be a comment

              IF ( ( nfield == 3 .OR. nfield == 5 ) .AND. NULINE( i ) == '$' ) &
                THEN
                j = ISTFIE( nfield, icard ) - 1
                DO k = 1, 66 - i
                  NULINA( nlines )( j + k : j + k ) = NULINE( i + k - 1 )
                END DO
                GO TO 600
              END IF
            END IF

!  skip a field if a '_' is encountered

            IF ( NULINE( i ) == '_' ) THEN
              lfield = 0
            ELSE

!  set the current length of the field, lfield, the starting address
!  in nuline - 1, nstfie, and the field number, nfield

              field = .TRUE.
              lfield = 1
              nstfie = i - 1
            END IF
          END IF
        END IF
  500 CONTINUE
  600 CONTINUE

!  finish off the last line

      IF ( field ) THEN
        j = ISTFIE( nfield, icard ) - 1
        DO k = 1, lfield
          NULINA( nlines )( j + k : j + k ) = NULINE( nstfie + k )
        END DO
      END IF
      RETURN

!  non-executable statements

 2020 FORMAT( ' ** Exit from INTERPRET_gpsmps - indicator card not recognised' )
 2440 FORMAT( ' ** Exit from INTERPRET_gpsmps - field ', i1, ' on free-form',  &
              ' card too long' )
 2450 FORMAT( ' ** Exit from INTERPRET_gpsmps - too many fields on free-form', &
              ' card' )

!  end of subroutine FREE_format

      END SUBROUTINE FREE_format

!-*-*-*-*-*- S I F D E C O D E   T R A N S    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE TRANSLATE_for_ad( out, status, input, output, tempry, single, &
                                   iauto, iad0, len_rinames, RINAMES,  &
                                   len_dummy, DUMMY )
      INTEGER :: out, status, input, output, tempry, iauto, iad0
      INTEGER :: len_rinames, len_dummy
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: RINAMES, DUMMY
      LOGICAL :: single

!  -------------------------------------------------------
!  translate a fortran 77 program so that it is capable of
!  accepting AD01 or AD02 reals instead of ordinary reals

!  formerly TRANS in SiFDec
!  -------------------------------------------------------

!  local variables

      INTEGER :: i, i1, i2, ii, j, jj, nreal, nre, lre, mre, ndummy
      LOGICAL :: intri, sub, fun, nofiel, ebrack
      LOGICAL :: freal, endlin, startr
      INTEGER :: used_length, new_length, min_length, alloc_status
      CHARACTER ( LEN = 24 ) :: bad_alloc
      CHARACTER ( LEN = 1 ) :: card
      CHARACTER ( LEN = 4 ) :: ad0
      CHARACTER ( LEN = 10 ) :: blank, field
      CHARACTER ( LEN = 15 ) :: adtype
      CHARACTER ( LEN = 72 ) :: nuline, oldlin, bl72
      CHARACTER ( LEN = 72 ) :: RELINE( 20 )

      CHARACTER ( LEN = 6 ), PARAMETER :: lsp = ' real '
      CHARACTER ( LEN = 6 ), PARAMETER :: usp = ' REAL '
      CHARACTER ( LEN = 10 ), PARAMETER :: lf = ' function '
      CHARACTER ( LEN = 10 ), PARAMETER :: uf = ' FUNCTION '
      CHARACTER ( LEN = 11 ), PARAMETER :: li = ' intrinsic '
      CHARACTER ( LEN = 11 ), PARAMETER :: ui = ' INTRINSIC '
      CHARACTER ( LEN = 13 ), PARAMETER :: ls = ' subroutine '
      CHARACTER ( LEN = 13 ), PARAMETER :: us = ' SUBROUTINE '
      CHARACTER ( LEN = 18 ), PARAMETER :: ldp = ' double precision '
      CHARACTER ( LEN = 18 ), PARAMETER :: udp = ' DOUBLE PRECISION '
      CHARACTER ( LEN = 15 ), PARAMETER :: un = '=AD01_UNDEFINED'

!  create a blank name

      DO i = 1, 10
        BLANK( i : i ) = ' '
      END DO
      DO i = 1, 72
        BL72( i : i ) = ' '
      END DO

!  determine the type of automatic derivative to be used

      IF ( iauto == 1 ) THEN
        IF ( single ) THEN
          adtype = 'FORWARD_SINGLE '
        ELSE
          adtype = 'FORWARD_DOUBLE '
        END IF
      ELSE
        IF ( single ) THEN
          adtype = 'BACKWARD_SINGLE'
        ELSE
          adtype = 'BACKWARD_DOUBLE'
        END IF
      END IF

!  determine the ad routine to be used

      IF ( iad0 == 1 ) THEN
        ad0 = 'AD01'
      ELSE
        ad0 = 'AD02'
      END IF

!  initialize logicals

      intri = .FALSE.
      sub = .FALSE.
      fun = .FALSE.
      startr = .FALSE.
      REWIND( input )
      REWIND( tempry )
   10 CONTINUE

!  read a new line

      READ ( input, 1000, END = 600, ERR = 600 ) nuline
      IF ( sub .OR. fun ) THEN

!  ignore comments

        IF ( NULINE( 1 : 1 ) == 'C' .OR. NULINE( 1 : 1 ) == 'c' ) GO TO 400

!  is the line a continuation?

        IF ( NULINE( 6: 6 ) /= ' ' ) THEN
          IF ( card == 'I' ) GO TO 10
          ii = 7
          IF ( card == 'B' .OR. card == 'E' )                                  &
            CALL GET_list( nuline, ii, ndummy, len_dummy, DUMMY, blank,        &
                           out, status )
            IF ( status /= 0 ) RETURN

!  find what kind of line this is. find its first nonzero character
!  find the start of the name

        ELSE
          DO i = 7, 72
            IF ( NULINE( i : i ) /= ' ' ) GO TO 30
          END DO
          GO TO 10
   30     CONTINUE
          card = ' '
          IF ( NULINE( i : i + 6 ) == 'INTEGER' ) GO TO 400
          IF ( NULINE( i : i + 6 ) == 'COMPLEX' ) GO TO 400
          IF ( NULINE( i : i + 6 ) == 'LOGICAL' ) GO TO 400
          IF ( NULINE( i : i + 8 ) == 'CHARACTER' ) GO TO 400
          IF ( NULINE( i : i + 8 ) == 'DIMENSION' ) GO TO 400
          IF ( NULINE( i : i + 7 ) == 'IMPLICIT' ) GO TO 400
          IF ( NULINE( i : i + 10 ) == 'EQUIVALENCE' ) GO TO 400
          IF ( NULINE( i : i + 7 ) == 'EXTERNAL' ) THEN
            card = 'E'
            CALL GET_list( nuline, i + 8, ndummy, len_dummy, DUMMY, blank,     &
                           out, status )
            IF ( status /= 0 ) RETURN
            GO TO 400
          END IF
          IF ( single ) THEN
            IF ( NULINE( i : i + 15 ) == 'DOUBLE PRECISION' ) GO TO 400
            IF ( NULINE( i : i + 3 ) == 'REAL' ) THEN
              card = 'R'
              ii = i + 4
              GO TO 200
            END IF
          ELSE
            IF ( NULINE( i : i + 3 ) == 'REAL' ) GO TO 400
            IF ( NULINE( i : i + 15 ) == 'DOUBLE PRECISION' ) THEN
              card = 'R'
              ii = i + 16
              GO TO 200
            END IF
          END IF
          IF ( NULINE( i : i + 3 ) == 'SAVE' ) THEN
            WRITE( out, 2100 ) ad0
            card = 'S'
            ii = i + 4
            GO TO 200
          END IF
          IF ( NULINE( i : i + 8 ) == 'INTRINSIC' ) THEN
            card = 'I'
            GO TO 10
          END IF
          IF ( NULINE( i : i + 5 ) == 'COMMON' ) THEN
            WRITE( out, 2110 ) ad0
            card = 'C'
            ii = i + 6
            GO TO 200
          END IF
          IF ( NULINE( i : i + 8 ) == 'PARAMETER' ) THEN
            card = 'P'
            ii = i + 9
            GO TO 200
          END IF
          IF ( NULINE( i : i + 3 ) == 'DATA' ) THEN
            card = 'D'
            ii = i + 4
            GO TO 200
          END IF

!  the body of the procedure has been found. complete the
!  introduction
!
          REWIND( tempry )
          card = 'B'
   60     CONTINUE
          READ ( tempry, 1000, END = 190, ERR = 190 ) oldlin

!  write out comments

          IF ( OLDLIN( 1 : 1 ) == 'C' .OR. OLDLIN( 1 : 1 ) == 'c' ) GO TO 180

!  search for cards defining appropriate real values

          IF ( OLDLIN( 6: 6 ) /= ' ' ) THEN
            IF ( card /= 'R' ) GO TO 180
            ii = 7
          ELSE
            IF ( card == 'B' ) THEN
              card = 'F'
              GO TO 180
            END IF
            IF ( card == 'F' ) WRITE( output, 2020 ) ad0, adtype

!  write out the previous set of real values
!
            IF ( startr ) THEN
              IF ( nre > 0 ) THEN
                DO i = 1, mre - 1
                   CALL WRITE_line( RELINE( i ), 72, output )
                END DO
                CALL WRITE_line( RELINE( mre ), lre, output )
              END IF
              startr = .FALSE.
            END IF
            DO i = 7, 72
              IF ( OLDLIN( i : i ) /= ' ' ) GO TO 80
            END DO
            GO TO 60
   80       CONTINUE
            card = ' '
            IF ( OLDLIN( i : i + 3 ) == 'SAVE' ) GO TO 180
            IF ( OLDLIN( i : i + 3 ) == 'DATA' ) GO TO 180
            IF ( OLDLIN( i : i + 5 ) == 'COMMON' ) GO TO 180
            IF ( OLDLIN( i : i + 6 ) == 'INTEGER' ) GO TO 180
            IF ( OLDLIN( i : i + 6 ) == 'COMPLEX' ) GO TO 180
            IF ( OLDLIN( i : i + 6 ) == 'LOGICAL' ) GO TO 180
            IF ( OLDLIN( i : i + 7 ) == 'EXTERNAL' ) GO TO 180
            IF ( OLDLIN( i : i + 7 ) == 'IMPLICIT' ) GO TO 180
            IF ( OLDLIN( i : i + 8 ) == 'CHARACTER' ) GO TO 180
            IF ( OLDLIN( i : i + 8 ) == 'DIMENSION' ) GO TO 180
            IF ( OLDLIN( i : i + 8 ) == 'PARAMETER' ) GO TO 180
            IF ( OLDLIN( i : i + 10 ) == 'EQUIVALENCE' ) GO TO 180
            IF ( single ) THEN
              IF ( OLDLIN( i : i + 15 ) == 'DOUBLE PRECISION' ) GO TO 180
              ii = i + 4
              RELINE( 1 ) = bl72
              RELINE( 1 )( 1 : 11 ) = '      REAL '
              lre = 11
              mre = 1
            ELSE
              IF ( OLDLIN( i : i + 3 ) == 'REAL' ) GO TO 180
              ii = i + 16
              RELINE( 1 ) = bl72
              RELINE( 1 )( 1 : 23 ) = '      DOUBLE PRECISION '
              mre = 1
              lre = 23
            END IF
            nre = 0
            card = 'R'
            startr = .TRUE.
          END IF
  110     CONTINUE
          CALL GET_string( ii, i1, i2, endlin, oldlin, blank,                  &
                           field, nofiel, ebrack, .FALSE. )
          IF ( nofiel ) GO TO 60

!  the parameter will be of type ad01_real or ad02_real

          j = ii - i1
          DO i = 1, nreal
            IF ( field == RINAMES( i ) ) THEN
              DO j = 1, ndummy
                IF ( field == DUMMY( j ) ) THEN
                  WRITE( output, 2060 ) ad0,                                   &
                    ( OLDLIN( jj : jj ), jj = i1, ii - 1 )
                  GO TO 130
                END IF
              END DO
              IF ( iad0 == 1 ) THEN
                WRITE( output, 2060 ) ad0,                                     &
                   ( OLDLIN( jj : jj ), jj = i1, ii - 1 ),                     &
                   ( UN( jj : jj ), jj = 1, 15 )
              ELSE
                WRITE( output, 2060 ) ad0,                                     &
                   ( OLDLIN( jj : jj ), jj = i1, ii - 1 )
              END IF
              GO TO 130
            END IF
          END DO

!  the parameter will be of type real

          IF ( nre > 0 ) THEN
            IF ( lre + 1 > 72 ) THEN
              mre = mre + 1
              RELINE( mre ) = bl72
              RELINE( mre )( 1 : 8 ) = '     * ,'
              lre = 8
            ELSE
              RELINE( mre )( lre + 1 : lre + 1 ) = ','
              lre = lre + 1
            END IF
          END IF
          IF ( lre + j + 1 > 72 ) THEN
            mre = mre + 1
            RELINE( mre ) = bl72
            RELINE( mre )( 1 : 7 ) = '     * '
            lre = 7
          END IF
          RELINE( mre )( lre + 1 : lre + j ) = OLDLIN( i1 : ii - 1 )
          lre = lre + j
          nre = nre + 1
  130     CONTINUE
          IF ( endlin ) GO TO 60

!  find the next parameter

          GO TO 110

!  output the current line

  180     CONTINUE
          CALL WRITE_line( oldlin, 72, output )
          GO TO 60

!  write out any unfinished set of real values
!
  190     CONTINUE
          IF ( startr ) THEN
            IF ( nre > 0 ) THEN
              DO i = 1, mre - 1
                CALL WRITE_line( RELINE( i ), 72, output )
              END DO
              CALL WRITE_line( RELINE( mre ), lre, output )
            END IF
            startr = .FALSE.
          END IF

!  the introduction is complete

          IF ( sub ) THEN
            sub = .FALSE.
          ELSE
            fun = .FALSE.
            IF ( freal .AND. iad0 == 1 )                                       &
              WRITE( output, 2030 ) RINAMES( 1 )( 1 : 6 )
            IF ( freal .AND. iad0 == 2 )                                       &
              WRITE( output, 2040 ) ad0, RINAMES( 1 )( 1 : 6 )
          END IF
          REWIND( tempry )
          GO TO 410
        END IF

!  find all variables mentioned on the current string

  200   CONTINUE

!  add the variable to the list

        IF ( card == 'R' ) THEN
  210     CONTINUE
            CALL GET_string( ii, i1, i2, endlin, nuline, blank,                &
                             field, nofiel, ebrack, .FALSE. )
            IF ( nofiel ) GO TO 400
            DO i = 1, nreal
              IF ( field == RINAMES( i ) ) GO TO 230
            END DO
            nreal = nreal + 1
            IF ( nreal > len_rinames ) THEN
              used_length = nreal - 1 ; min_length = nreal
              new_length = increase_n * min_length / increase_d + 1
              CALL EXTEND_array( RINAMES, len_rinames, used_length,            &
                                 new_length, min_length, buffer,               &
                                 status, alloc_status, 'RINAMES' )
              IF ( status /= 0 ) THEN
                bad_alloc = 'RINAMES' ; status = - 22 ; GO TO 980 ; END IF
              len_rinames = new_length
            END IF
            RINAMES( nreal ) = field
  230       CONTINUE
            IF ( endlin ) GO TO 400
          GO TO 210
        END IF

!  remove the variable from the list

        IF ( card == 'C' .OR. card == 'D' .OR.                                 &
             card == 'P' .OR. card == 'S' ) THEN
  310     CONTINUE
          CALL GET_string( ii, i1, i2, endlin, nuline, blank,                  &
                           field, nofiel, ebrack, .FALSE. )
          IF ( nofiel ) GO TO 400
          DO i = 1, nreal
            IF ( field == RINAMES( i ) ) THEN
              RINAMES( i ) = RINAMES( nreal )
              nreal = nreal - 1
              EXIT
            END IF
          END DO
          IF ( endlin ) GO TO 400

!  for parameter statements, skip the segments after the "="

          IF ( card == 'P' ) THEN
            DO i = ii, 72
              IF ( NULINE( i : i ) == ',' .OR. NULINE( i : i ) == ')' ) THEN
                 ii = i + 1
                 GO TO 310
              END IF
            END DO
          END IF
          GO TO 310
        END IF
  400   CONTINUE
        WRITE( tempry, 2000 ) nuline
        GO TO 10
      END IF
  410 CONTINUE

!  ignore comments

      IF ( .NOT. ( sub .OR. fun ) ) THEN
        IF ( NULINE( 1 : 1 ) == 'C' .OR. NULINE( 1 : 1 ) == 'c' ) GO TO 500

!  remove lines mentioning intrinsic functions

        IF ( intri ) THEN
           IF ( NULINE( 6: 6 ) /= ' ' ) GO TO 10
           intri = .FALSE.
        END IF
        DO i = 1, 62
          IF ( NULINE( i : i + 10 ) == ui .OR.                                 &
               NULINE( i : i + 10 ) == li ) THEN
            intri = .TRUE.
            GO TO 10
          END IF
        END DO

!  hunt for the start of a subroutine

        card = ' '
        DO i = 1, 60
          IF ( NULINE( i : i + 11 ) == us .OR.                                 &
               NULINE( i : i + 11 ) == ls ) THEN
            ii = i + 12
            sub = .TRUE.
            card = 'B'
            nreal = 0
            ndummy = 0
            CALL GET_list( nuline, ii, ndummy, len_dummy, DUMMY, blank,        &
                           out, status )
            IF ( status /= 0 ) RETURN
            WRITE( tempry, 2000 ) nuline
            GO TO 10
          END IF
        END DO

!  hunt for the start of a function

        DO i = 1, 63
          IF ( NULINE( i : i + 9 ) == uf .OR.                                  &
               NULINE( i : i + 9 ) == lf ) THEN
            ii = i + 10
            fun = .TRUE.
            card = 'B'

!  find what kind of function this is

            freal = .FALSE.

!  hunt for the string ' real '

            IF ( single ) THEN
              DO j = 1, i
                IF ( NULINE( j : j + 5 ) == usp .OR.                           &
                     NULINE( j : j + 5 ) == lsp ) THEN
                  freal = .TRUE.
                  NULINE( j : j + 5 ) = '      '
                  NULINE( 6 : 6 ) = '*'
                  WRITE( tempry, 2010 ) ad0
                  GO TO 433
                END IF
              END DO

!  hunt for the string ' double precision '

            ELSE
              DO j = 1, i
                IF ( NULINE( j : j + 17 ) == udp .OR.                          &
                     NULINE( j : j + 17 ) == ldp ) THEN
                  IF ( iad0 == 1 ) THEN
                    NULINE( j : j + 17 ) = ' TYPE (' // ad0 // '_REAL) '
                  ELSE
                    NULINE( j : j + 17 ) = '                           '
                    ad0 = 'AD02'
                  END IF
                  freal = .TRUE.
                  GO TO 433
                END IF
              END DO
            END IF
            WRITE( tempry, 2000 ) nuline
            ndummy = 0
            CALL GET_list( nuline, ii, ndummy, len_dummy, DUMMY, blank,        &
                           out, status )
            IF ( status /= 0 ) RETURN
            GO TO 10

!  the function will be of type ad01_real or ad02_real. find its name

  433       CONTINUE
            WRITE( tempry, 2000 ) nuline

!  find the start of the name

            DO j = ii, 72
              IF ( NULINE( j : j ) /= ' ' ) GO TO 435
            END DO

!  no name has been found so far. read the next card
!
            READ ( input, 1000, END = 600, ERR = 600 ) nuline
            ii = 7
            GO TO 433

!  find the end of the name

  435       CONTINUE
            DO jj = j + 1, MIN( 72, j + 5 )
              IF ( .NOT. ( letter( NULINE( jj : jj ) ) .OR.                    &
                           number( NULINE( jj : jj ) ) ) ) GO TO 437
            END DO
            jj = MIN( 72, j + 6 )
  437       CONTINUE
            nreal = 1
            RINAMES( nreal ) = blank
            RINAMES( nreal )( 1 : jj - j ) = NULINE( j : jj - 1 )
            ndummy = 0
            CALL GET_list( nuline, jj, ndummy, len_dummy, DUMMY, blank,        &
                           out, status )
            IF ( status /= 0 ) RETURN
            GO TO 10
          END IF
        END DO

!  hunt for the start of a subroutine

  500   CONTINUE
        CALL WRITE_line( nuline, 72, output )
        GO TO 10
      END IF
  600 CONTINUE
      RETURN

!  allocation errors

  980 CONTINUE
      WRITE( out, "( ' ** Message from TRANSLATE_for_ad-',                     &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  non-executable statements

 1000 FORMAT( A72 )
 2000 FORMAT( A72 )
 2010 FORMAT( '      TYPE ( ', A4, '_REAL )' )
 2020 FORMAT( '      USE HSL_', A4, '_', A15 )
!2030 FORMAT( '      call ad01_undefine(', a6, ')' )
 2030 FORMAT( '      ', A6, ' = AD01_UNDEFINED' )
 2040 FORMAT( '      TYPE (', A4, '_REAL) :: ', A6 )
 2060 FORMAT( '      TYPE (', A4, '_REAL) :: ', 46( A1, : ), /,                &
              ( '     *', 66( A1, : ) ) )
 2100 FORMAT( ' ** Warning: a user-supplied external procedure',               &
              ' SAVEs parameters.', /,                                         &
              '    This is not allowed by the automatic',                      &
              ' differentiation package ', A4, '.' )
 2110 FORMAT( ' ** Warning: a user-supplied external procedure',               &
              ' saves parameters via common.', /,                              &
              '    This is not allowed by the automatic',                      &
              ' differentiation package ', A4, '.' )

!  end of subroutine TRANSLATE_for_ad

      END SUBROUTINE TRANSLATE_for_ad

!-*-*-*-*-*-*- S I F D E C O D E   C H A R A    F U N C T I O N -*-*-*-*-*-*-*-

      LOGICAL FUNCTION letter( c )
      CHARACTER ( LEN = 1 ) :: c

!  ----------------------------
!  is the character c a letter?
!  ----------------------------

!  local variables

      INTEGER :: i

      DO i = 1, 26
        IF ( c == LCHARS( i ) .OR. c == UCHARS( i ) ) THEN
          letter = .TRUE.
          RETURN
        END IF
      END DO
      letter = .FALSE.
      RETURN

!  end of function letter

      END FUNCTION letter

!-*-*-*-*-*-*- S I F D E C O D E   N U M B A    F U N C T I O N -*-*-*-*-*-*-*-

      LOGICAL FUNCTION number( c )
      CHARACTER ( LEN = 1 ) :: c

!  ----------------------------
!  is the character c a number?
!  ----------------------------

!  local variables

      INTEGER :: i

      DO i = 1, 10
        IF ( c == CHARS( i ) ) THEN
          number = .TRUE.
          RETURN
        END IF
      END DO
      number = .FALSE.
      RETURN

!  end of function number

      END FUNCTION number

!-*-*-*-*-*- S I F D E C O D E   U P P E R    S U B R O U T I N E -*-*-*-*-*-*-

      SUBROUTINE UPPER( c, n )
      INTEGER :: n
      CHARACTER ( LEN = * ) :: c

!  -------------------------------
!  convert character to upper case
!  -------------------------------

!  local variables

      INTEGER :: i, j

      DO 20 j = 1, n
        DO i = 1, 26
          IF ( c( j : j ) == LCHARS( i ) ) THEN
            c( j : j ) = UCHARS( i )
            GO TO 20
          END IF
        END DO
   20 CONTINUE
      RETURN

!  end of subroutine UPPER

      END SUBROUTINE UPPER

!-*-*-*-*-*- S I F D E C O D E   L O W E R    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE LOWER( c, n )
      INTEGER :: n
      CHARACTER ( LEN = * ) :: c

!  -------------------------------
!  convert character to lower case
!  -------------------------------

!  local variables

      INTEGER :: i, j

      DO 20 j = 1, n
        DO i = 1, 26
          IF ( c( j : j ) == UCHARS( i ) ) THEN
            c( j : j ) = LCHARS( i )
            GO TO 20
          END IF
        END DO
   20 CONTINUE
      RETURN

!  end of subroutine LOWER

      END SUBROUTINE LOWER

!-*-*-*- S I F D E C O D E   G E T _ S T R I N G    S U B R O U T I N E -*-*-*-

      SUBROUTINE GET_string( ii, i1, i2, endlin, nuline, blank,                &
                             field, nofiel, ebrack, ignorb )
      INTEGER :: ii, i1, i2
      LOGICAL :: endlin, nofiel, ebrack, ignorb
      CHARACTER ( LEN = 72 ) :: nuline
      CHARACTER ( LEN = 10 ) :: blank, field

!  ------------------------------------------------------------
!  find the first character string in nuline beyond position ii
!  ------------------------------------------------------------

!  local variables

      INTEGER :: i
      LOGICAL :: arrays

      ebrack = .TRUE.
      endlin = .FALSE.
      DO i = ii, 72
        IF ( letter( NULINE( i : i ) ) ) THEN
          nofiel = .FALSE.
          i1 = i
          GO TO 30
        END IF
      END DO
      nofiel = .TRUE.
      RETURN

!  next, find its last character

   30 CONTINUE
      DO i = i1 + 1, MIN( 72, i1 + 6 )
        IF ( .NOT. letter( NULINE( i : i ) ) .AND.                             &
             .NOT. number( NULINE( i : i ) ) ) THEN
          i2 = i - 1
          ii = i
          IF ( ignorb ) GO TO 70
          GO TO 50
        END IF
      END DO
      i2 = MIN( 72, i1 + 6 )
      endlin = .TRUE.

!  last, check to see if it is an array

   50 CONTINUE
      arrays = .FALSE.
      DO i = i2 + 1, 72
        IF ( .NOT. arrays ) THEN
          IF ( NULINE( i : i ) /= ' ' ) THEN
            IF ( NULINE( i : i ) /= '(' ) THEN
              EXIT
            ELSE
              arrays = .TRUE.
              ebrack = .FALSE.
            END IF
          END IF
        ELSE
          IF ( NULINE( i : i ) == ')' ) THEN
            ebrack = .TRUE.
            ii = i + 1
            EXIT
          END IF
        END IF
      END DO

   70 CONTINUE
!     write( 6, * ) ' line, i1, i2, ii ', line, i1, i2, ii
      field = blank
      field( 1 : i2 - i1 + 1 ) = nuline( i1 : i2 )
      CALL UPPER( field, 10 )
      RETURN

!  end of subroutine GET_string

      END SUBROUTINE GET_string

!-*-*-*-*- S I F D E C O D E   G E T _ L I S T    S U B R O U T I N E -*-*-*-*-

      SUBROUTINE GET_list( nuline, i, ndummy, len_dummy, DUMMY, blank,         &
                           out, status )
      INTEGER :: i, ndummy, len_dummy, out, status
      CHARACTER ( LEN = 10 ) :: blank
      CHARACTER ( LEN = 72 ) :: nuline
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: DUMMY

!  ------------------------------------------------------------------
!  determine the list of variables beyond column i on the line nuline
!  ------------------------------------------------------------------

!  local variables

      INTEGER :: idummy, j, j1, j2
      INTEGER :: used_length, new_length, min_length, alloc_status
      CHARACTER ( LEN = 10 ) :: field
      CHARACTER ( LEN = 24 ) :: bad_alloc
      LOGICAL nofiel, ebrack, endlin

      j = i
   10 CONTINUE
        CALL GET_string( j, j1, j2, endlin, nuline, blank,                     &
                         field, nofiel, ebrack, .TRUE. )
        IF ( nofiel ) RETURN
        DO idummy = 1, ndummy
          IF ( field == DUMMY( idummy ) ) GO TO 30
        END DO
        ndummy = ndummy + 1
        IF ( ndummy > len_dummy ) THEN
          used_length = ndummy - 1 ; min_length = ndummy
          new_length = increase_n * min_length / increase_d + 1
          CALL EXTEND_array( DUMMY, len_dummy, used_length, new_length,        &
                             min_length, buffer, status, alloc_status,         &
                             'DUMMY' )
          IF ( status /= 0 ) THEN
            bad_alloc = 'DUMMY' ; status = - 22 ; GO TO 980 ; END IF
          len_dummy = new_length
        END IF
        DUMMY( ndummy ) = field
   30   CONTINUE
        IF ( endlin ) RETURN
        GO TO 10

!  allocation errors

  980 CONTINUE
      WRITE( out, "( ' ** Message from TRANSLATE_for_ad-',                     &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  end of subroutine GET_list

      END SUBROUTINE GET_list

!-*-*-*- S I F D E C O D E   W R I T E _ L I N E     S U B R O U T I N E -*-*-*-

      SUBROUTINE WRITE_line( nuline, iend, output )
      INTEGER :: iend, output
      CHARACTER ( LEN = 72 ) :: nuline

!  -------------------------------------------------------
!  write out the current line, cutting off trailing blanks
!  -------------------------------------------------------

!  local variables

      INTEGER :: i, i2

      DO i2 = MIN( iend, 72 ), 1, - 1
        IF ( NULINE( i2 : i2 ) /= ' ' ) GO TO 20
      END DO
      i2 = 1
   20 CONTINUE
      WRITE( output, "( 72( A1 : ) )" ) ( NULINE( i : i ), i = 1, i2 )
      RETURN

!  end of subroutine WRITE_line

      END SUBROUTINE WRITE_line

!-*-*-*-*- S I F D E C O D E   R E O R D E R    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE REORDER( nc, nnz, A_row, A_col, A_val, IP, IW )
      INTEGER :: nc, nnz
      INTEGER :: A_row( nnz ), A_col( nnz )
      INTEGER :: IW( nc + 1 ), IP( nc + 1 )
      REAL ( KIND = wp ) ::   A_val( nnz  )

!  ---------------------------------------------------------
!  sort a sparse matrix from arbitrary order to column order
!  ---------------------------------------------------------

!  local variables

      INTEGER :: i, j, k, l, ic
      INTEGER :: ncp1, itemp, jtemp,  locat
      REAL ( KIND = wp ) :: anext, atemp

!  initialize the workspace as zero

      ncp1 = nc + 1
      IW( 1 : ncp1 ) = 0

!  pass 1. count the number of elements in each column

      DO k = 1, nnz
        j = A_col( k )
        IW( j ) = IW( j ) + 1
      END DO

!  put the positions where each column begins in a compressed collection into
!  ip and iw

      IP( 1 ) = 1
      DO j = 2, ncp1
        IP( j ) = IW( j - 1 ) + IP( j - 1 )
        IW( j - 1 ) = IP( j - 1 )
      END DO

!  pass 2. reorder the elements into column order. Fill in each column in turn

      DO ic = 1, nc

!  consider the next unfilled position in column ic

        DO k = IW( ic ), IP( ic + 1 ) - 1

!  the entry should be placed in column j

          i = A_row( k )
          j = A_col( k )
          anext = A_val( k )
          DO l = 1, nnz

!  see if the entry is already in place

            IF ( j == ic ) EXIT
            locat = IW( j )
!
!  as a new entry is placed in column j, increase the pointer iw( j ) by one
!
            IW( j  ) = locat + 1

!  record details of the entry which currently occupies location locat

            itemp = A_row( locat ) ; jtemp = A_col( locat )
            atemp = A_val( locat )

!  move the new entry to it correct place.

            A_row( locat ) = i ; A_col( locat ) = j ; A_val( locat ) = anext

!  make the displaced entry the new entry

            i = itemp ; j = jtemp ; anext = atemp
          END DO

!  move the new entry to it correct place.

          A_col( k ) = j ; A_row( k ) = i ; A_val( k ) = anext
        END DO
      END DO
      RETURN

!  end of subroutine REORDER

      END SUBROUTINE REORDER

!-  S I F D E C O D E   H A S H _ i n i t i a l i z e    S U B R O U T I N E  -

      SUBROUTINE HASH_initialize( length, TABLE )
      INTEGER :: length
      INTEGER :: TABLE( length )

!  ------------------------------------------------------------
!  set up initial scatter table (Williams, CACM 2, 21-24, 1959)

!  TABLE(i) gives the status of table entry i.
!  If status = - (length+1), the entry is unused
!  ------------------------------------------------------------

!  local variables

      INTEGER :: prime

      hash_empty = length + 1

!  Find an appropriate prime number for the hash function. Compute the largest
!  prime smaller than length

      prime = 2 * ( ( length + 1 ) / 2 ) - 1
   10 CONTINUE

!  Is prime prime?

      IF ( .NOT. HASH_is_prime( prime ) ) THEN
        prime = prime - 2
        GO TO 10
      END IF
      hash_prime = prime

!  initialize each table entry as unfilled

      TABLE( : length ) = - hash_empty
      RETURN

!  end of subroutine HASH_initialize

      END SUBROUTINE HASH_initialize

!-*-*- S I F D E C O D E   H A S H _ i n s e r t    S U B R O U T I N E -*-*-

      SUBROUTINE HASH_insert( length, nchar, FIELD, TABLE, KEY, ifree )
      INTEGER :: nchar, ifree, length
      INTEGER :: TABLE( length )
      CHARACTER ( LEN = 1 ) :: FIELD( nchar ), KEY( nchar, length )

!  -------------------------------------------------------------------
!  Insert in a chained scatter table (Williams, CACM 2, 21-24, 1959)

!  FIELD(i) gives the ith component of the field to be inserted
!  TABLE(i) gives the status of table entry i
!  if TABLE(i) = - (length+1), the entry is unused
!  if TABLE(i) = - k, the entry was used but has been deleted.
!                k gives the index of the next entry in the chain
!  if TABLE(i) = 0, the entry is used and lies at the end of a chain
!  if TABLE(i) = k, the entry is used. k gives the index of the next
!                entry in the chain
!  ifree > 0 indicates that the field has been inserted as entry ifree
!        < 0 indicates that the field already occurs in entry - ifree
!        = 0 the table is full and the field has not been inserted
!  -------------------------------------------------------------------

!  local variables

      INTEGER :: i, j, k
      CHARACTER ( LEN = 1 ) :: BFIELD( nbytes )
      INTEGER :: IVALUE( 2 )

!  find a starting position, ifree, for the insertion. Perform the hashing on
!  8 characters of field at a time

      ifree = 0
      DO j = 1, nchar, nbytes
        DO i = 1, nbytes
          k = j + i - 1
          IF ( k <= nchar ) THEN
            BFIELD( i ) = FIELD( k )
          ELSE
            BFIELD( i ) = ' '
          END IF
        END DO

!  convert the character string into two integer numbers

        IVALUE( 1 ) = ICHAR( BFIELD( 1 ) ) / 2
        IVALUE( 2 ) = ICHAR( BFIELD( nbytes_by_2 + 1 ) ) / 2
        DO i = 2, nbytes_by_2
          IVALUE( 1 ) = 256 * IVALUE( 1 ) + ICHAR( BFIELD( i ) )
          IVALUE( 2 ) = 256 * IVALUE( 2 ) + ICHAR( BFIELD( nbytes_by_2 + i ) )
        END DO

!  convert the character string into a double precision number

!       READ( UNIT = FIELD8, FMT = 1000 ) value

!  hash and add the result to ifree

        ifree = ifree + HASH_value( IVALUE( 1 ), hash_prime )
      END DO

!  ensure that ifree lies within the allowed range

      ifree = MOD( ifree, IDINT( hash_prime ) ) + 1

!  is there a list?

      IF ( TABLE( ifree ) >= 0 ) THEN

!  compare to see if the key has been found

   40   CONTINUE
        DO i = 1, nchar
          IF ( FIELD( i ) .NE. KEY( i, ifree ) ) GO TO 60
        END DO

!  the key already exists and therefore cannot be inserted

        IF ( TABLE( ifree ) >= 0 ) THEN
          ifree = - ifree
          RETURN
        END IF

!  the key used to exist but has been deleted and must be restored

        GO TO 100

!  advance along the chain to the next entry

   60   CONTINUE
        IF ( TABLE( ifree ) /= 0 ) THEN
          ifree = IABS( TABLE( ifree ) )
          GO TO 40
        END IF

!  the end of the chain has been reached. Find empty entry in the table

   70   CONTINUE
        hash_empty = hash_empty - 1
        IF ( hash_empty == 0 ) THEN
          ifree = 0
          RETURN
        END IF
        IF ( TABLE( hash_empty ) >= - length ) GO TO 70
        TABLE( ifree ) = hash_empty
        ifree = hash_empty

!  the starting entry for the chain is unused

      ELSE
        IF ( TABLE( ifree ) >= - length ) THEN
           TABLE( ifree ) = - TABLE( ifree )
           GO TO 100
        END IF
      END IF

!  there is no link from the newly inserted field

      TABLE( ifree ) = 0

!  insert new key

  100 CONTINUE
      KEY( : nchar, ifree ) = FIELD( : nchar )
      RETURN

!  end of subroutine HASH_insert

      END SUBROUTINE HASH_insert

!-*-*- S I F D E C O D E   H A S H _ s e a r c h    S U B R O U T I N E -*-*-

      SUBROUTINE HASH_search( length, nchar, field, TABLE, KEY, ifree )
      INTEGER :: length, nchar, ifree
      INTEGER :: TABLE( length )
      CHARACTER ( LEN = 1 ) :: FIELD( nchar ), KEY( nchar, length )

!  -------------------------------------------------------------------
!  search within chained scatter table (Williams, CACM 2, 21-24, 1959)

!  FIELD(i) gives the ith component of the field to be searched for
!  TABLE(i) gives the status of table entry i
!  if TABLE(i) = - (length+1), the entry is unused
!  if TABLE(i) = - k, the entry was used but has been deleted.
!                k gives the index of the next entry in the chain
!  if TABLE(i) = 0, the entry is used and lies at the end of a chain
!  if TABLE(i) = k, the entry is used. k gives the index of the next
!                entry in the chain
!  IFIELD(i) gives the field key for used entries in the table
!  ifree > 0 indicates that the field occurs as entry ifree
!        < 0 indicates that the field once occured as entry - ifree
!            but has been deleted in the interim
!        = 0 the table is full and the field has not been inserted
!  -------------------------------------------------------------------

!  local variables

      INTEGER :: i, j, k
      CHARACTER ( LEN = 1 ) :: BFIELD( nbytes )
      INTEGER :: IVALUE( 2 )

!  find a starting position, ifree, for the chain leading to the required
!  location. Perform the hashing on nbytes characters of field at a time

      ifree = 0
      DO j = 1, nchar, nbytes
        DO i = 1, nbytes
          k = j + i - 1
          IF ( k <= nchar ) THEN
            BFIELD( i ) = FIELD( k )
          ELSE
            BFIELD( i ) = ' '
          END IF
        END DO

!  convert the character string into two integer numbers

        IVALUE( 1 ) = ICHAR( BFIELD( 1 ) ) / 2
        IVALUE( 2 ) = ICHAR( BFIELD( nbytes_by_2 + 1 ) ) / 2
        DO i = 2, nbytes_by_2
          IVALUE( 1 ) = 256 * IVALUE( 1 ) + ICHAR( BFIELD( i ) )
          IVALUE( 2 ) = 256 * IVALUE( 2 ) + ICHAR( BFIELD( nbytes_by_2 + i ) )
        END DO

!  convert the character string into a double precision number

!        READ( UNIT = FIELD8, FMT = 1000 ) value

!  hash and add the result to ifree

        ifree = ifree + HASH_value( IVALUE( 1 ), hash_prime )
      END DO

!  ensure that ifree lies within the allowed range

      ifree = MOD( ifree, IDINT( hash_prime ) ) + 1

!  is there a list?

      IF ( TABLE( ifree ) < - length ) THEN
        ifree = 0
        RETURN
      END IF

!  compare to see if the key has been found

   40 CONTINUE
      DO i = 1, nchar

!  advance to next

        IF ( FIELD( i ) /= KEY( i, ifree ) ) THEN
          IF ( TABLE( ifree ) == 0 ) THEN
            ifree = 0
            RETURN
          END IF
          ifree = IABS( TABLE( ifree ) )
          GO TO 40
        END IF
      END DO

!  check that the table item has not been removed

      IF ( TABLE( ifree ) < 0 ) ifree = - ifree
      RETURN

!  end of subroutine HASH_search

      END SUBROUTINE HASH_search

!-*-*- S I F D E C O D E   H A S H _ r e b u i l d    S U B R O U T I N E -*-*-

      SUBROUTINE HASH_rebuild( len_table, nchar, TABLE, KEY,                   &
                               len_table_new, TABLE_new, KEY_new, inform )
      INTEGER :: nchar, len_table, len_table_new, inform
      INTEGER :: TABLE( len_table )
      INTEGER :: TABLE_new( len_table_new )
      CHARACTER ( LEN = 1 ) :: KEY( nchar, len_table )
      CHARACTER ( LEN = 1 ) :: KEY_new( nchar, len_table_new )

!  -------------------------------------------------------------------
!  rebuild the chained scatter table (Williams, CACM 2, 21-24, 1959)
!  to account for an increase in its length

!  TABLE_new(i) gives the status of new table entry i
!  if TABLE_new(i) = - (length+1), the entry is unused
!  if TABLE_new(i) = - k, the entry was used but has been deleted.
!                    k gives the index of the next entry in the chain
!  if TABLE_new(i) = 0, the entry is used and lies at the end of a chain
!  if TABLE_new(i) = k, the entry is used. k gives the index of the next
!                    entry in the chain
!  -------------------------------------------------------------------


!  local variables

      INTEGER :: ifree, k
      CHARACTER ( LEN = 1 ), DIMENSION( nchar ) :: FIELD

!  reinitialize the scatter table

      CALL HASH_initialize( len_table_new, TABLE_new )

!  run through the entries in the old table seeing if the kth is empty

      DO k = 1, len_table

!  if the kth entry was previously occupied, copy its key into the new table

        IF ( TABLE( k ) >= 0 ) THEN
          FIELD( : nchar ) = KEY( : nchar, k )
          CALL HASH_insert( len_table_new, nchar, FIELD, TABLE_new, KEY_new,   &
                            ifree )

!  check that there is sufficient space

          IF ( ifree == 0 ) THEN
            inform = - 1
            RETURN
          END IF
        END IF
      END DO
      inform = 0
      RETURN

!  end of subroutine HASH_rebuild

      END SUBROUTINE HASH_rebuild

! -*-  H A S H _ e n l a r g e _ a n d _ i n s e r t    S U B R O U T I N E  -*-

      SUBROUTINE HASH_enlarge_and_insert( length, nchar, FIELD,                &
                                          TABLE, KEY, INLIST, ifree )
      INTEGER :: nchar, ifree, length
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: TABLE, INLIST
      CHARACTER ( LEN = nchar ), ALLOCATABLE, DIMENSION( : ) :: KEY
      CHARACTER ( LEN = 1 ) :: FIELD( nchar )

!  ----------------------------------------------------------------------
!  Insert in a chained scatter table (Williams, CACM 2, 21-24, 1959)
!  having first enlarged the table if it isn't currently large enough

!  FIELD(i) gives the ith component of the field to be inserted
!  TABLE(i) gives the status of table entry i
!  if TABLE(i) = - (length+1), the entry is unused
!  if TABLE(i) = - k, the entry was used but has been deleted.
!                k gives the index of the next entry in the chain
!  if TABLE(i) = 0, the entry is used and lies at the end of a chain
!  if TABLE(i) = k, the entry is used. k gives the index of the next
!                entry in the chain
!  ifree > 0 indicates that the field occurs as entry ifree
!        < 0 indicates that the field once occured as entry - ifree
!            but has been deleted in the interim
!        = 0 the table was full and attempts to enlarge the table failed
!  ---------------------------------------------------------------------

!  local variables

      INTEGER :: i, k, new_length, current, alloc_status
      LOGICAL :: file_open
      CHARACTER ( LEN = nchar ) :: key_temp
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: INLIST_old
      CHARACTER ( LEN = nchar ), ALLOCATABLE, DIMENSION( : ) :: KEY_old

!  try inserting in the current table

      CALL HASH_insert( length, nchar, field, TABLE, KEY, ifree )

!  exit if the insertion was successful

      IF ( ifree /= 0 ) RETURN

!  count the current number of table entries

     current = COUNT( TABLE( : length ) >= 0 )

!write(6,*) ' length, current ', length, current

!  allocate temporary arrays to hold the old values of KEY and INLIST

      ALLOCATE( INLIST_old( current ), KEY_old( current ), STAT = alloc_status )

!  check that the allocation succeeded

      IF ( alloc_status /= 0 ) THEN

!  there is insufficient space to hold a copy of the current table.
!  Use buffered i/o instead

        IF ( ALLOCATED( INLIST_old ) ) THEN
          DEALLOCATE( INLIST_old, STAT = alloc_status )
          IF ( alloc_status /= 0 ) GO TO 900
        END IF
        IF ( ALLOCATED( KEY_old ) ) THEN
          DEALLOCATE( KEY_old, STAT = alloc_status )
          IF ( alloc_status /= 0 ) GO TO 900
        END IF

!  rewind the buffer i/o unit

        INQUIRE( UNIT = buffer, OPENED = file_open )
        IF ( file_open ) THEN
          REWIND( UNIT = buffer )
        ELSE
          OPEN( UNIT = buffer )
        END IF

!  write the current table to the buffer

        current = 0
        DO k = 1, length
          IF ( TABLE( k ) >= 0 ) THEN
            current = current + 1
            WRITE( UNIT = buffer, FMT = "( I10, 1X, A12 )" )                   &
              INLIST( k ), KEY( k )
          END IF
        END DO

!  deallocate the space used for the old table

        DEALLOCATE( TABLE, INLIST, KEY, STAT = alloc_status )

!  check that the deallocation succeeded

        IF ( alloc_status /= 0 ) GO TO 900
        GO TO 100
      END IF

!  make a temporary copy of KEY and INLIST

      current = 0
      DO k = 1, length
        IF ( TABLE( k ) >= 0 ) THEN
          current = current + 1
          INLIST_old(  current ) = INLIST( k )
          KEY_old( current ) = KEY( k )
        END IF
      END DO

!  now enlarge the table. Pick the new length

      new_length = increase_n * length / increase_d + 1

!  deallocate the space used for the old table

      DEALLOCATE( TABLE, INLIST, KEY, STAT = alloc_status )

!  check that the deallocation succeeded

      IF ( alloc_status /= 0 ) GO TO 900

!  try to allocate arrays to hold the new values of TABLE, KEY and INLIST

   10 CONTINUE
      ALLOCATE( TABLE( new_length ), INLIST( new_length), KEY( new_length ),   &
                STAT = alloc_status )

!  check that the allocation succeeded

      IF ( alloc_status /= 0 ) THEN
        IF ( ALLOCATED( TABLE ) ) THEN
          DEALLOCATE( TABLE, STAT = alloc_status )
          IF ( alloc_status /= 0 ) GO TO 900
        END IF
        IF ( ALLOCATED( INLIST ) ) THEN
          DEALLOCATE( INLIST, STAT = alloc_status )
          IF ( alloc_status /= 0 ) GO TO 900
        END IF
        IF ( ALLOCATED( KEY ) ) THEN
          DEALLOCATE( KEY, STAT = alloc_status )
          IF ( alloc_status /= 0 ) GO TO 900
        END IF
        new_length = length + ( new_length - length ) / 2

!  if there is insufficient space to create a copy of the new table, use
!  buffered i/o instead. Rewind the buffer i/o unit

        IF ( new_length <= length ) THEN
          INQUIRE( UNIT = buffer, OPENED = file_open )
          IF ( file_open ) THEN
            REWIND( UNIT = buffer )
          ELSE
            OPEN( UNIT = buffer )
          END IF

!  write the current table to the buffer

          DO k = 1, current
            WRITE( UNIT = buffer, FMT =  "( I10, 1X, A12 )" )                  &
              INLIST_old( k ), KEY_old( k )
          END DO

!  deallocate the space used for the old table

          DEALLOCATE( INLIST_old, KEY_old, STAT = alloc_status )

!  check that the deallocation succeeded

          IF ( alloc_status /= 0 ) GO TO 900
          GO TO 100
        END IF
        GO TO 10
      ENDIF

!  initialize the new table

      CALL HASH_initialize( new_length, TABLE )

!  run through the entries in the old table seeing if the kth is empty

      DO k = 1, current

!  if the kth entry was previously occupied, copy its key into the new table

        key_temp = KEY_old( k )
        CALL HASH_insert( new_length, nchar, key_temp, TABLE, KEY, ifree )
        INLIST( ifree ) = INLIST_old( k )
      END DO

 !  record the new length

!write(6,*) ' hash table length increased from ', length, ' to ', new_length

      length = new_length

!  deallocate the space used for the old table

      DEALLOCATE( INLIST_old, KEY_old, STAT = alloc_status )

!  check that the deallocation succeeded

      IF ( alloc_status /= 0 ) GO TO 900

!  try once again inserting the new value into the new current table

      CALL HASH_insert( length, nchar, field, TABLE, KEY, ifree )
      RETURN

!  Use buffered i/o

  100 CONTINUE

!  now enlarge the table. Pick the new length

      new_length = increase_n * length / increase_d + 1

!  try to allocate arrays to hold the new values of TABLE, KEY and INLIST

  110 CONTINUE
      ALLOCATE( TABLE( new_length ), INLIST( new_length), KEY( new_length ),   &
                STAT = alloc_status )

!  check that the allocation succeeded

      IF ( alloc_status /= 0 ) THEN
        IF ( ALLOCATED( TABLE ) ) THEN
          DEALLOCATE( TABLE, STAT = alloc_status )
          IF ( alloc_status /= 0 ) GO TO 900
        END IF
        IF ( ALLOCATED( INLIST ) ) THEN
          DEALLOCATE( INLIST, STAT = alloc_status )
          IF ( alloc_status /= 0 ) GO TO 900
        END IF
        IF ( ALLOCATED( KEY ) ) THEN
          DEALLOCATE( KEY, STAT = alloc_status )
          IF ( alloc_status /= 0 ) GO TO 900
        END IF
        new_length = length + ( new_length - length ) / 2

!  if there is still insufficient space to create the new table, exit

        IF ( new_length <= length ) GO TO 900
        GO TO 110
      ENDIF

!  initialize the new table

      CALL HASH_initialize( new_length, TABLE )

!  copy the old table to the new one

      REWIND( UNIT = buffer )
      DO k = 1, current
        READ( UNIT = buffer, FMT = "( I10, 1X, A12 )" ) i, key_temp

!  if the kth entry was previously occupied, copy its key into the new table

        CALL HASH_insert( new_length, nchar, key_temp, TABLE, KEY, ifree )
        INLIST( ifree ) = i
      END DO

 !  record the new length

!write(6,*) ' hash table length increased from ', length, ' to ', new_length

      length = new_length

!  try once again inserting the new value into the new current table

      CALL HASH_insert( length, nchar, field, TABLE, KEY, ifree )
      RETURN

!  fatal errors

  900 CONTINUE
      ifree = 0

!write(6,*) ' enlargement failed '

      RETURN

!  end of subroutine HASH_enlarge_and_insert

      END SUBROUTINE HASH_enlarge_and_insert

!-*-*-*-*- S I F D E C O D E   H A S H _ v a l u e    F U N C T I O N -*-*-*-*-

      INTEGER FUNCTION HASH_value( ivalue, hash_prime )
      INTEGER :: IVALUE( 2 )
      REAL ( KIND = wp ) :: hash_prime

!  -------------------------------------
!  a hash function proposed by John Reid
!  -------------------------------------

      HASH_value = INT( DMOD( DBLE( IVALUE( 1 ) ) + IVALUE( 2 ), hash_prime ) )
      HASH_value = ABS( HASH_value ) + 1
      RETURN

!  end of function HASH_value

      END FUNCTION HASH_value

!-*-*- S I F D E C O D E   H A S H  _ i s _ p r i m e    F U N C T I O N -*-*-

      LOGICAL FUNCTION HASH_is_prime( prime )
      INTEGER :: prime

!  -------------------------------------------
!  returns the value .TRUE. if prime is prime
!  -------------------------------------------

!  local variables

      INTEGER :: i

      HASH_is_prime  = .FALSE.
      IF ( MOD( prime, 2 ) == 0 ) RETURN
      DO i = 3, INT( DSQRT( DBLE( prime ) ) ), 2
        IF ( MOD( prime, i ) == 0 ) RETURN
      END DO
      HASH_is_prime  = .TRUE.
      RETURN

!  end of function HASH_is_prime

      END FUNCTION HASH_is_prime

! -*-*- a l l o c a t e _ a r r a y _ i n t e g e r  S U B R O U T I N E -*-*-

     SUBROUTINE ALLOCATE_array_integer( ARRAY, new_length, alloc_status )

!  -----------------------------------------------------------------------
!  reallocate an integer array so that its length is at least new_length.
!  If the array is lready allocated and of length at least new_length, the
!  allocation will be skipped and new_length replaced by SIZE(ARRAY)
!  -----------------------------------------------------------------------

!  History -
!   fortran 2003 version first released in SIFDECODE/CUTEst, 26th November 2012

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

     INTEGER, INTENT( OUT ) :: alloc_status
     INTEGER, INTENT( INOUT ) :: new_length
     INTEGER, ALLOCATABLE, DIMENSION( : ) :: ARRAY

     IF ( ALLOCATED( ARRAY ) ) THEN
       IF ( SIZE( ARRAY ) < new_length ) THEN
         DEALLOCATE( ARRAY, STAT = alloc_status )
         IF ( alloc_status /= 0 ) RETURN
       ELSE
         new_length = SIZE( ARRAY )
         alloc_status = 0
         RETURN
       END IF
     END IF
     ALLOCATE( ARRAY( new_length ), STAT = alloc_status )

     RETURN

!  end of subroutine ALLOCATE_array_integer

     END SUBROUTINE ALLOCATE_array_integer

! -*-*-*-  A L L O C A T E _ a r r a y _ r e a l  S U B R O U T I N E  -*-*-*-*-

     SUBROUTINE ALLOCATE_array_real( ARRAY, new_length, alloc_status )

!  -----------------------------------------------------------------------
!  reallocate a real array so that its length is at least new_length.
!  If the array is lready allocated and of length at least new_length, the
!  allocation will be skipped and new_length replaced by SIZE(ARRAY)
!  -----------------------------------------------------------------------

!  History -
!   fortran 2003 version first released in SIFDECODE/CUTEst, 26th November 2012

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

     INTEGER, INTENT( OUT ) :: alloc_status
     INTEGER, INTENT( INOUT ) :: new_length
     REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: ARRAY

     IF ( ALLOCATED( ARRAY ) ) THEN
       IF ( SIZE( ARRAY ) < new_length ) THEN
         DEALLOCATE( ARRAY, STAT = alloc_status )
         IF ( alloc_status /= 0 ) RETURN
       ELSE
         new_length = SIZE( ARRAY )
         alloc_status = 0
         RETURN
       END IF
     END IF
     ALLOCATE( ARRAY( new_length ), STAT = alloc_status )

     RETURN

!  end of subroutine ALLOCATE_array_real

     END SUBROUTINE ALLOCATE_array_real

! -*-*-  A L L O C A T E _ a r r a y _ l o g i c a l  S U B R O U T I N E  -*-*-

     SUBROUTINE ALLOCATE_array_logical( ARRAY, new_length, alloc_status )

!  -----------------------------------------------------------------------
!  reallocate a real array so that its length is at least new_length.
!  If the array is lready allocated and of length at least new_length, the
!  allocation will be skipped and new_length replaced by SIZE(ARRAY)
!  -----------------------------------------------------------------------

!  History -
!   fortran 2003 version first released in SIFDECODE/CUTEst, 26th November 2012

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

     INTEGER, INTENT( OUT ) :: alloc_status
     INTEGER, INTENT( INOUT ) :: new_length
     LOGICAL, ALLOCATABLE, DIMENSION( : ) :: ARRAY

     IF ( ALLOCATED( ARRAY ) ) THEN
       IF ( SIZE( ARRAY ) < new_length ) THEN
         DEALLOCATE( ARRAY, STAT = alloc_status )
         IF ( alloc_status /= 0 ) RETURN
       ELSE
         new_length = SIZE( ARRAY )
         alloc_status = 0
         RETURN
       END IF
     END IF
     ALLOCATE( ARRAY( new_length ), STAT = alloc_status )

     RETURN

!  end of subroutine ALLOCATE_array_logical

     END SUBROUTINE ALLOCATE_array_logical

! -  A L L O C A T E _ a r r a y _ c h a r a c t e r    S U B R O U T I N E  -

     SUBROUTINE ALLOCATE_array_character( ARRAY, new_length, alloc_status )

!  -----------------------------------------------------------------------
!  reallocate a real array so that its length is at least new_length.
!  If the array is lready allocated and of length at least new_length, the
!  allocation will be skipped and new_length replaced by SIZE(ARRAY)
!  -----------------------------------------------------------------------

!  History -
!   fortran 2003 version first released in SIFDECODE/CUTEst, 26th November 2012

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

     INTEGER, INTENT( OUT ) :: alloc_status
     INTEGER, INTENT( INOUT ) :: new_length
     CHARACTER ( LEN = * ), ALLOCATABLE, DIMENSION( : ) :: ARRAY

     IF ( ALLOCATED( ARRAY ) ) THEN
       IF ( SIZE( ARRAY ) < new_length ) THEN
         DEALLOCATE( ARRAY, STAT = alloc_status )
         IF ( alloc_status /= 0 ) RETURN
       ELSE
         new_length = SIZE( ARRAY )
         alloc_status = 0
         RETURN
       END IF
     END IF
     ALLOCATE( ARRAY( new_length ), STAT = alloc_status )

     RETURN

!  end of subroutine ALLOCATE_array_character

     END SUBROUTINE ALLOCATE_array_character

! -*-*- a l l o c a t e _ a r r a y 2 _ i n t e g e r  S U B R O U T I N E -*-*-

     SUBROUTINE ALLOCATE_array2_integer( ARRAY, new_length1, new_length2,      &
                                         alloc_status )

!  -----------------------------------------------------------------------
!  reallocate an integer array so that its length is at least new_length.
!  If the array is lready allocated and of length at least new_length, the
!  allocation will be skipped and new_length replaced by SIZE(ARRAY)
!  -----------------------------------------------------------------------

!  History -
!   fortran 2003 version first released in SIFDECODE/CUTEst, 26th November 2012

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

     INTEGER, INTENT( OUT ) :: alloc_status
     INTEGER, INTENT( INOUT ) :: new_length1, new_length2
     INTEGER, ALLOCATABLE, DIMENSION( : , : ) :: ARRAY

     IF ( ALLOCATED( ARRAY ) ) THEN
       IF ( SIZE( ARRAY, 1 ) /= new_length1 .OR.                               &
            SIZE( ARRAY, 2 ) < new_length2 ) THEN
         DEALLOCATE( ARRAY, STAT = alloc_status )
         IF ( alloc_status /= 0 ) RETURN
       ELSE
         new_length1 = SIZE( ARRAY, 1 ) ; new_length2 = SIZE( ARRAY, 2 )
         alloc_status = 0
         RETURN
       END IF
     END IF
     ALLOCATE( ARRAY( new_length1, new_length2 ), STAT = alloc_status )

     RETURN

!  end of subroutine ALLOCATE_array2_integer

     END SUBROUTINE ALLOCATE_array2_integer

! -*-*-  A L L O C A T E _ a r r a y 2 _ r e a l  S U B R O U T I N E  -*-*-*-

     SUBROUTINE ALLOCATE_array2_real( ARRAY, new_length1, new_length2,         &
                                      alloc_status )

!  -----------------------------------------------------------------------
!  reallocate a real array so that its length is at least new_length.
!  If the array is lready allocated and of length at least new_length, the
!  allocation will be skipped and new_length replaced by SIZE(ARRAY)
!  -----------------------------------------------------------------------

!  History -
!   fortran 2003 version first released in SIFDECODE/CUTEst, 26th November 2012

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

     INTEGER, INTENT( OUT ) :: alloc_status
     INTEGER, INTENT( INOUT ) :: new_length1, new_length2
     REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( :, : ) :: ARRAY

     IF ( ALLOCATED( ARRAY ) ) THEN
       IF ( SIZE( ARRAY, 1 ) /= new_length1 .OR.                               &
            SIZE( ARRAY, 2 ) < new_length2 ) THEN
         DEALLOCATE( ARRAY, STAT = alloc_status )
         IF ( alloc_status /= 0 ) RETURN
       ELSE
         new_length1 = SIZE( ARRAY, 1 ) ; new_length2 = SIZE( ARRAY, 2 )
         alloc_status = 0
         RETURN
       END IF
     END IF
     ALLOCATE( ARRAY( new_length1, new_length2 ), STAT = alloc_status )

     RETURN

!  end of subroutine ALLOCATE_array2_real

     END SUBROUTINE ALLOCATE_array2_real

! - A L L O C A T E _ a r r a y 2 _ c h a r a c t e r     S U B R O U T I N E -

     SUBROUTINE ALLOCATE_array2_character( ARRAY, new_length1, new_length2,    &
                                           alloc_status )

!  -----------------------------------------------------------------------
!  reallocate a real array so that its length is at least new_length.
!  If the array is lready allocated and of length at least new_length, the
!  allocation will be skipped and new_length replaced by SIZE(ARRAY)
!  -----------------------------------------------------------------------

!  History -
!   fortran 2003 version first released in SIFDECODE/CUTEst, 26th November 2012

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

     INTEGER, INTENT( OUT ) :: alloc_status
     INTEGER, INTENT( INOUT ) :: new_length1, new_length2
     CHARACTER ( LEN = * ), ALLOCATABLE, DIMENSION( :, : ) :: ARRAY

     IF ( ALLOCATED( ARRAY ) ) THEN
       IF ( SIZE( ARRAY, 1 ) /= new_length1 .OR.                               &
            SIZE( ARRAY, 2 ) < new_length2 ) THEN
         DEALLOCATE( ARRAY, STAT = alloc_status )
         IF ( alloc_status /= 0 ) RETURN
       ELSE
         new_length1 = SIZE( ARRAY, 1 ) ; new_length2 = SIZE( ARRAY, 2 )
         alloc_status = 0
         RETURN
       END IF
     END IF
     ALLOCATE( ARRAY( new_length1, new_length2 ), STAT = alloc_status )

     RETURN

!  end of subroutine ALLOCATE_array2_character

     END SUBROUTINE ALLOCATE_array2_character

! -*-*- a l l o c a t e _ a r r a y 2 _ i n t e g e r  S U B R O U T I N E -*-*-

     SUBROUTINE ALLOCATE_array3_integer( ARRAY, new_length1, new_length2,      &
                                         new_length3, alloc_status )

!  -----------------------------------------------------------------------
!  reallocate an integer array so that its length is at least new_length.
!  If the array is lready allocated and of length at least new_length, the
!  allocation will be skipped and new_length replaced by SIZE(ARRAY)
!  -----------------------------------------------------------------------

!  History -
!   fortran 2003 version first released in SIFDECODE/CUTEst, 26th November 2012

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

     INTEGER, INTENT( OUT ) :: alloc_status
     INTEGER, INTENT( INOUT ) :: new_length1, new_length2, new_length3
     INTEGER, ALLOCATABLE, DIMENSION( : , : , : ) :: ARRAY

     IF ( ALLOCATED( ARRAY ) ) THEN
       IF ( SIZE( ARRAY, 1 ) /= new_length1 .OR.                               &
            SIZE( ARRAY, 2 ) /= new_length2 .OR.                               &
            SIZE( ARRAY, 3 ) < new_length3 ) THEN
         DEALLOCATE( ARRAY, STAT = alloc_status )
         IF ( alloc_status /= 0 ) RETURN
       ELSE
         new_length1 = SIZE( ARRAY, 1 ) ; new_length2 = SIZE( ARRAY, 2 )
         new_length3 = SIZE( ARRAY, 3 )
         alloc_status = 0
         RETURN
       END IF
     END IF
     ALLOCATE( ARRAY( new_length1, new_length2, new_length3 ),                 &
               STAT = alloc_status )

     RETURN

!  end of subroutine ALLOCATE_array3_integer

     END SUBROUTINE ALLOCATE_array3_integer

! -*-*-  E X T E N D _ a r r a y _ i n t e g e r  S U B R O U T I N E - -*-*-

     SUBROUTINE EXTEND_array_integer( ARRAY, old_length, used_length,          &
                                      new_length, min_length, buffer,          &
                                      status, alloc_status, array_name )

!  -------------------------------------------------------------------------
!  extend an integer array so that its length is increaed from old_length to
!  as close to new_length as possible while keeping existing data intact
!  -------------------------------------------------------------------------

!  History -
!   fortran 90 version released pre GALAHAD Version 1.0. February 7th 1995 as
!     EXTEND_array_integer as part of the GALAHAD module EXTEND
!   fortran 2003 version released in SIFDECODE/CUTEst, 5th November 2012

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

     INTEGER, INTENT( IN ) :: old_length, buffer
     INTEGER, INTENT( OUT ) :: status, alloc_status
     INTEGER, INTENT( INOUT ) :: used_length, min_length, new_length
     CHARACTER ( LEN = * ), INTENT( IN ) :: array_name
     INTEGER, ALLOCATABLE, DIMENSION( : ) :: ARRAY

!  local variables

     INTEGER :: length
     LOGICAL :: file_open
     INTEGER, ALLOCATABLE, DIMENSION( : ) :: DUMMY

!  make sure that the new length is larger than the old

     IF ( new_length <= old_length ) new_length = 2 * old_length

!  ensure that the input data is consistent

     used_length = MIN( used_length, old_length )
     min_length = MAX( old_length + 1, MIN( min_length, new_length ) )

!  if possible, allocate DUMMY to hold the old values of ARRAY

     ALLOCATE( DUMMY( used_length ), STAT = alloc_status )

!  if the allocation failed, resort to using an external unit

     IF ( alloc_status /= 0 ) GO TO 100

     DUMMY( : used_length ) = ARRAY( : used_length )

!  extend the length of ARRAY

     DEALLOCATE( ARRAY )
     length = new_length

  10 CONTINUE
     ALLOCATE( ARRAY( length ), STAT = alloc_status )

!  if the allocation failed, reduce the new length and retry

     IF ( alloc_status /= 0 ) THEN
       length = length + ( length - min_length ) / 2

!  if there is insufficient room for both ARRAY and DUMMY, use an external unit

       IF ( length < min_length ) THEN

!  rewind the buffer i/o unit

         INQUIRE( UNIT = buffer, OPENED = file_open )
         IF ( file_open ) THEN
           REWIND( UNIT = buffer )
         ELSE
           OPEN( UNIT = buffer )
         END IF

!  copy the contents of ARRAY into the buffer i/o area

         WRITE( UNIT = buffer, FMT = * ) DUMMY( : used_length )

!  extend the length of ARRAY

         DEALLOCATE( DUMMY )
         GO TO 110
       END IF
       GO TO 10
     END IF

!  copy the contents of ARRAY back from the buffer i/o area

     ARRAY( : used_length ) = DUMMY( : used_length )
     DEALLOCATE( DUMMY )
     new_length = length
     GO TO 200

!  use an external unit for writing

 100 CONTINUE

!  rewind the buffer i/o unit

     INQUIRE( UNIT = buffer, OPENED = file_open )
     IF ( file_open ) THEN
       REWIND( UNIT = buffer )
     ELSE
       OPEN( UNIT = buffer )
     END IF

!  copy the contents of ARRAY into the buffer i/o area

     WRITE( UNIT = buffer, FMT = * ) ARRAY( : used_length )

!  extend the length of ARRAY

     DEALLOCATE( ARRAY )

 110 CONTINUE
     ALLOCATE( ARRAY( new_length ), STAT = alloc_status )

!  if the allocation failed, reduce the new length and retry

     IF ( alloc_status /= 0 ) THEN
       new_length = min_length + ( new_length - min_length ) / 2
       IF ( new_length < min_length ) THEN
         status = 12
         RETURN
       END IF
       GO TO 110
     END IF

!  copy the contents of ARRAY back from the buffer i/o area

     REWIND( UNIT = buffer )
     READ( UNIT = buffer, FMT = * ) ARRAY( : used_length )

!  successful exit

 200 CONTINUE
     IF ( debug_extend ) WRITE( 6, "( A, 2( 1X, I0 ) )" )                      &
       array_name, old_length, new_length
     status = 0
     RETURN

!  end of subroutine EXTEND_array_integer

     END SUBROUTINE EXTEND_array_integer

!-*-*-*-*-*-  E X T E N D _ a r r a y _ r e a l  S U B R O U T I N E -*-*-*-*-*-

     SUBROUTINE EXTEND_array_real( ARRAY, old_length, used_length,             &
                                   new_length, min_length, buffer,             &
                                   status, alloc_status, array_name )

!  ---------------------------------------------------------------------
!  extend a real array so that its length is increaed from old_length to
!  as close to new_length as possible while keeping existing data intact
!  ---------------------------------------------------------------------

!  History -
!   fortran 90 version released pre GALAHAD Version 1.0. February 7th 1995 as
!     EXTEND_array_real as part of the GALAHAD module EXTEND
!   fortran 2003 version released in SIFDECODE/CUTEst, 5th November 2012

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

     INTEGER, INTENT( IN ) :: old_length, buffer
     INTEGER, INTENT( OUT ) :: status, alloc_status
     INTEGER, INTENT( INOUT ) :: used_length, min_length, new_length
     CHARACTER ( LEN = * ), INTENT( IN ) :: array_name
     REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: ARRAY

!  local variables

     INTEGER :: length
     LOGICAL :: file_open
     REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: DUMMY

!  make sure that the new length is larger than the old

     IF ( new_length <= old_length ) new_length = 2 * old_length

!  ensure that the input data is consistent

     used_length = MIN( used_length, old_length )
     min_length = MAX( old_length + 1, MIN( min_length, new_length ) )

!  if possible, allocate DUMMY to hold the old values of ARRAY

     ALLOCATE( DUMMY( used_length ), STAT = alloc_status )

!  if the allocation failed, resort to using an external unit

     IF ( alloc_status /= 0 ) GO TO 100

     DUMMY( : used_length ) = ARRAY( : used_length )

!  extend the length of ARRAY

     DEALLOCATE( ARRAY )
     length = new_length

  10 CONTINUE
     ALLOCATE( ARRAY( length ), STAT = alloc_status )

!  if the allocation failed, reduce the new length and retry

     IF ( alloc_status /= 0 ) THEN
       length = length + ( length - min_length ) / 2

!  if there is insufficient room for both ARRAY and DUMMY, use an external unit

       IF ( length < min_length ) THEN

!  rewind the buffer i/o unit

         INQUIRE( UNIT = buffer, OPENED = file_open )
         IF ( file_open ) THEN
           REWIND( UNIT = buffer )
         ELSE
           OPEN( UNIT = buffer )
         END IF

!  copy the contents of ARRAY into the buffer i/o area

         WRITE( UNIT = buffer, FMT = * ) DUMMY( : used_length )

!  extend the length of ARRAY

         DEALLOCATE( DUMMY )
         GO TO 110
       END IF
       GO TO 10
     END IF

!  copy the contents of ARRAY back from the buffer i/o area

       ARRAY( : used_length ) = DUMMY( : used_length )
       DEALLOCATE( DUMMY )
       new_length = length
       GO TO 200

!  use an external unit for writing

 100   CONTINUE

!  rewind the buffer i/o unit

     INQUIRE( UNIT = buffer, OPENED = file_open )
     IF ( file_open ) THEN
       REWIND( UNIT = buffer )
     ELSE
       OPEN( UNIT = buffer )
     END IF

!  copy the contents of ARRAY into the buffer i/o area

     WRITE( UNIT = buffer, FMT = * ) ARRAY( : used_length )

!  extend the length of ARRAY

     DEALLOCATE( ARRAY )

 110 CONTINUE
     ALLOCATE( ARRAY( new_length ), STAT = alloc_status )

!  if the allocation failed, reduce the new length and retry

     IF ( alloc_status /= 0 ) THEN
       new_length = min_length + ( new_length - min_length ) / 2
       IF ( new_length < min_length ) THEN
          status = 12
          RETURN
       END IF
       GO TO 110
     END IF

!  copy the contents of ARRAY back from the buffer i/o area

     REWIND( UNIT = buffer )
     READ( UNIT = buffer, FMT = * ) ARRAY( : used_length )

!  successful exit

 200 CONTINUE
     status = 0
     IF ( debug_extend ) WRITE( 6, "( A, 2( 1X, I0 ) )" )                      &
       array_name, old_length, new_length
     RETURN

!  end of subroutine EXTEND_array_real

     END SUBROUTINE EXTEND_array_real

!-*-  E X T E N D _ a r r a y _ c h a r a c t e r     S U B R O U T I N E  -*-

     SUBROUTINE EXTEND_array_character( ARRAY, old_length, used_length,        &
                                        new_length, min_length, buffer,        &
                                        status, alloc_status, array_name )

!  ----------------------------------------------------------------------------
!  extend a character array so that its length is increaed from old_length
!  to as close to new_length as possible while keeping existing data intact
!  ----------------------------------------------------------------------------

!  History -
!   fortran 90 version released pre GALAHAD Version 1.0. February 7th 1995 as
!     EXTEND_array_real as part of the GALAHAD module EXTEND
!   fortran 2003 version released in SIFDECODE/CUTEst, 5th November 2012

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

     INTEGER, INTENT( IN ) :: old_length, buffer
     INTEGER, INTENT( OUT ) :: status, alloc_status
     INTEGER, INTENT( INOUT ) :: used_length, min_length, new_length
     CHARACTER ( LEN = * ), INTENT( IN ) :: array_name
     CHARACTER ( LEN = * ), ALLOCATABLE, DIMENSION( : ) :: ARRAY

!  local variables

     INTEGER :: length
     LOGICAL :: file_open
     CHARACTER ( LEN = LEN( ARRAY) ), ALLOCATABLE, DIMENSION( : ) :: DUMMY

!  make sure that the new length is larger than the old

     IF ( new_length <= old_length ) new_length = 2 * old_length

!  ensure that the input data is consistent

     used_length = MIN( used_length, old_length )
     min_length = MAX( old_length + 1, MIN( min_length, new_length ) )

!  if possible, allocate DUMMY to hold the old values of ARRAY

     ALLOCATE( DUMMY( used_length ), STAT = alloc_status )

!  if the allocation failed, resort to using an external unit

     IF ( alloc_status /= 0 ) GO TO 100

     DUMMY( : used_length ) = ARRAY( : used_length )

!  extend the length of ARRAY

     DEALLOCATE( ARRAY )
     length = new_length

  10 CONTINUE
     ALLOCATE( ARRAY( length ), STAT = alloc_status )

!  if the allocation failed, reduce the new length and retry

     IF ( alloc_status /= 0 ) THEN
       length = length + ( length - min_length ) / 2

!  if there is insufficient room for both ARRAY and DUMMY, use an external unit

       IF ( length < min_length ) THEN

!  rewind the buffer i/o unit

         INQUIRE( UNIT = buffer, OPENED = file_open )
         IF ( file_open ) THEN
           REWIND( UNIT = buffer )
         ELSE
           OPEN( UNIT = buffer )
         END IF

!  copy the contents of ARRAY into the buffer i/o area

         WRITE( UNIT = buffer, FMT = * ) DUMMY( : used_length )

!  extend the length of ARRAY

         DEALLOCATE( DUMMY )
         GO TO 110
       END IF
       GO TO 10
     END IF

!  copy the contents of ARRAY back from the buffer i/o area

       ARRAY( : used_length ) = DUMMY( : used_length )
       DEALLOCATE( DUMMY )
       new_length = length
       GO TO 200

!  use an external unit for writing

 100   CONTINUE

!  rewind the buffer i/o unit

     INQUIRE( UNIT = buffer, OPENED = file_open )
     IF ( file_open ) THEN
       REWIND( UNIT = buffer )
     ELSE
       OPEN( UNIT = buffer )
     END IF

!  copy the contents of ARRAY into the buffer i/o area

     WRITE( UNIT = buffer, FMT = * ) ARRAY( : used_length )

!  extend the length of ARRAY

     DEALLOCATE( ARRAY )

 110 CONTINUE
     ALLOCATE( ARRAY( new_length ), STAT = alloc_status )

!  if the allocation failed, reduce the new length and retry

     IF ( alloc_status /= 0 ) THEN
       new_length = min_length + ( new_length - min_length ) / 2
       IF ( new_length < min_length ) THEN
          status = 12
          RETURN
       END IF
       GO TO 110
     END IF

!  copy the contents of ARRAY back from the buffer i/o area

     REWIND( UNIT = buffer )
     READ( UNIT = buffer, FMT = * ) ARRAY( : used_length )

!  successful exit

 200 CONTINUE
     status = 0
     IF ( debug_extend ) WRITE( 6, "( A, 2( 1X, I0 ) )" )                      &
       array_name, old_length, new_length
     RETURN

!  end of subroutine EXTEND_array_character

     END SUBROUTINE EXTEND_array_character

! -*-*-  E X T E N D _ a r r a y 2 _ i n t e g e r  S U B R O U T I N E - -*-*-

     SUBROUTINE EXTEND_array2_integer( ARRAY, old_length1, old_length2,        &
                                       used_length1, used_length2,             &
                                       new_length1, new_length2,               &
                                       min_length1, min_length2, buffer,       &
                                       status, alloc_status, array_name )

!  -------------------------------------------------------------------------
!  extend an integer array so that its length is increaed from old_length to
!  as close to new_length as possible while keeping existing data intact
!  -------------------------------------------------------------------------

!  History -
!   fortran 90 version released pre GALAHAD Version 1.0. February 7th 1995 as
!     EXTEND_array_integer as part of the GALAHAD module EXTEND
!   fortran 2003 version released in SIFDECODE/CUTEst, 5th November 2012

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

     INTEGER, INTENT( IN ) :: old_length1, old_length2, buffer
     INTEGER, INTENT( OUT ) :: status, alloc_status
     INTEGER, INTENT( INOUT ) :: used_length1, used_length2
     INTEGER, INTENT( INOUT ) :: new_length1, new_length2
     INTEGER, INTENT( INOUT ) :: min_length1, min_length2
     CHARACTER ( LEN = * ), INTENT( IN ) :: array_name
     INTEGER, ALLOCATABLE, DIMENSION( : , : ) :: ARRAY

!  local variables

     INTEGER :: length1, length2
     LOGICAL :: file_open
     INTEGER, ALLOCATABLE, DIMENSION( : , : ) :: DUMMY

!  ensure that the input data is consistent

     used_length1 = MIN( used_length1, old_length1 )
     used_length2 = MIN( used_length2, old_length2 )
     min_length1 = MAX( old_length1, MIN( min_length1, new_length1 ) )
     min_length2 = MAX( old_length2, MIN( min_length2, new_length2 ) )

!  if possible, allocate DUMMY to hold the old values of ARRAY

     ALLOCATE( DUMMY( used_length1, used_length2 ), STAT = alloc_status )

!  if the allocation failed, resort to using an external unit

     IF ( alloc_status /= 0 ) GO TO 100

     DUMMY( : used_length1, : used_length2 )                                   &
       = ARRAY( : used_length1, : used_length2 )

!  extend the length of ARRAY

     DEALLOCATE( ARRAY )
     length1 = new_length1 ; length2 = new_length2

  10 CONTINUE
     ALLOCATE( ARRAY( length1, length2 ), STAT = alloc_status )

!  if the allocation failed, reduce the new length and retry

     IF ( alloc_status /= 0 ) THEN
       length1 = length1 + ( length1 - min_length1 ) / 2
       length2 = length2 + ( length2 - min_length2 ) / 2

!  if there is insufficient room for both ARRAY and DUMMY, use an external unit

       IF ( length1 < min_length1 .OR. length2 < min_length2 ) THEN

!  rewind the buffer i/o unit

         INQUIRE( UNIT = buffer, OPENED = file_open )
         IF ( file_open ) THEN
           REWIND( UNIT = buffer )
         ELSE
           OPEN( UNIT = buffer )
         END IF

!  copy the contents of ARRAY into the buffer i/o area

         WRITE( UNIT = buffer, FMT = * )                                       &
           DUMMY( : used_length1 , : used_length2 )

!  extend the length of ARRAY

         DEALLOCATE( DUMMY )
         GO TO 110
       END IF
       GO TO 10
     END IF

!  copy the contents of ARRAY back from the buffer i/o area

     ARRAY( : used_length1, : used_length2 )                                   &
       = DUMMY( : used_length1, : used_length2 )
     DEALLOCATE( DUMMY )
     new_length1 = length1 ; new_length2 = length2
     GO TO 200

!  use an external unit for writing

 100 CONTINUE

!  rewind the buffer i/o unit

     INQUIRE( UNIT = buffer, OPENED = file_open )
     IF ( file_open ) THEN
       REWIND( UNIT = buffer )
     ELSE
       OPEN( UNIT = buffer )
     END IF

!  copy the contents of ARRAY into the buffer i/o area

     WRITE( UNIT = buffer, FMT = * ) ARRAY( : used_length1, : used_length2 )

!  extend the length of ARRAY

     DEALLOCATE( ARRAY )

 110 CONTINUE
     ALLOCATE( ARRAY( new_length1, new_length2 ), STAT = alloc_status )

!  if the allocation failed, reduce the new length and retry

     IF ( alloc_status /= 0 ) THEN
       new_length1 = min_length1 + ( new_length1 - min_length1 ) / 2
       new_length2 = min_length2 + ( new_length2 - min_length2 ) / 2
       IF ( new_length1 < min_length1 .OR. new_length2 < min_length2 ) THEN
         status = 12
         RETURN
       END IF
       GO TO 110
     END IF

!  copy the contents of ARRAY back from the buffer i/o area

     REWIND( UNIT = buffer )
     READ( UNIT = buffer, FMT = * ) ARRAY( : used_length1, : used_length2 )

!  successful exit

 200 CONTINUE
     IF ( debug_extend ) WRITE( 6, "( A, 4( 1X, I0 ) )" )                      &
       array_name, old_length1, new_length1, old_length2, new_length2
     status = 0
     RETURN

!  end of subroutine EXTEND_array2_integer

     END SUBROUTINE EXTEND_array2_integer

!-*-*-*-*-  E X T E N D _ a r r a y 2 _ r e a l  S U B R O U T I N E  -*-*-*-*-

     SUBROUTINE EXTEND_array2_real( ARRAY, old_length1, old_length2,           &
                                    used_length1, used_length2,                &
                                    new_length1, new_length2,                  &
                                    min_length1, min_length2, buffer,          &
                                    status, alloc_status, array_name )

!  ---------------------------------------------------------------------
!  extend a real array so that its length is increaed from old_length to
!  as close to new_length as possible while keeping existing data intact
!  ---------------------------------------------------------------------

!  History -
!   fortran 90 version released pre GALAHAD Version 1.0. February 7th 1995 as
!     EXTEND_array_real as part of the GALAHAD module EXTEND
!   fortran 2003 version released in SIFDECODE/CUTEst, 5th November 2012

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

     INTEGER, INTENT( IN ) :: old_length1, old_length2, buffer
     INTEGER, INTENT( OUT ) :: status, alloc_status
     INTEGER, INTENT( INOUT ) :: used_length1, used_length2
     INTEGER, INTENT( INOUT ) :: new_length1, new_length2
     INTEGER, INTENT( INOUT ) :: min_length1, min_length2
     CHARACTER ( LEN = * ), INTENT( IN ) :: array_name
     REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : , : ) :: ARRAY

!  local variables

     INTEGER :: length1, length2
     LOGICAL :: file_open
     REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : , : ) :: DUMMY

!  ensure that the input data is consistent

     used_length1 = MIN( used_length1, old_length1 )
     used_length2 = MIN( used_length2, old_length2 )
     min_length1 = MAX( old_length1, MIN( min_length1, new_length1 ) )
     min_length2 = MAX( old_length2, MIN( min_length2, new_length2 ) )

!  if possible, allocate DUMMY to hold the old values of ARRAY

     ALLOCATE( DUMMY( used_length1, used_length2 ), STAT = alloc_status )

!  if the allocation failed, resort to using an external unit

     IF ( alloc_status /= 0 ) GO TO 100

     DUMMY( : used_length1, : used_length2 )                                   &
       = ARRAY( : used_length1, : used_length2 )

!  extend the length of ARRAY

     DEALLOCATE( ARRAY )
     length1 = new_length1 ; length2 = new_length2

  10 CONTINUE
     ALLOCATE( ARRAY( length1, length2 ), STAT = alloc_status )

!  if the allocation failed, reduce the new length and retry

     IF ( alloc_status /= 0 ) THEN
       length1 = length1 + ( length1 - min_length1 ) / 2
       length2 = length2 + ( length2 - min_length2 ) / 2

!  if there is insufficient room for both ARRAY and DUMMY, use an external unit

       IF ( length1 < min_length1 .OR. length2 < min_length2 ) THEN

!  rewind the buffer i/o unit

         INQUIRE( UNIT = buffer, OPENED = file_open )
         IF ( file_open ) THEN
           REWIND( UNIT = buffer )
         ELSE
           OPEN( UNIT = buffer )
         END IF

!  copy the contents of ARRAY into the buffer i/o area

         WRITE( UNIT = buffer, FMT = * )                                       &
           DUMMY( : used_length1 , : used_length2 )

!  extend the length of ARRAY

         DEALLOCATE( DUMMY )
         GO TO 110
       END IF
       GO TO 10
     END IF

!  copy the contents of ARRAY back from the buffer i/o area

     ARRAY( : used_length1, : used_length2 )                                   &
       = DUMMY( : used_length1, : used_length2 )
     DEALLOCATE( DUMMY )
     new_length1 = length1 ; new_length2 = length2
     GO TO 200

!  use an external unit for writing

 100 CONTINUE

!  rewind the buffer i/o unit

     INQUIRE( UNIT = buffer, OPENED = file_open )
     IF ( file_open ) THEN
       REWIND( UNIT = buffer )
     ELSE
       OPEN( UNIT = buffer )
     END IF

!  copy the contents of ARRAY into the buffer i/o area

     WRITE( UNIT = buffer, FMT = * ) ARRAY( : used_length1, : used_length2 )

!  extend the length of ARRAY

     DEALLOCATE( ARRAY )

 110 CONTINUE
     ALLOCATE( ARRAY( new_length1, new_length2 ), STAT = alloc_status )

!  if the allocation failed, reduce the new length and retry

     IF ( alloc_status /= 0 ) THEN
       new_length1 = min_length1 + ( new_length1 - min_length1 ) / 2
       new_length2 = min_length2 + ( new_length2 - min_length2 ) / 2
       IF ( new_length1 < min_length1 .OR. new_length2 < min_length2 ) THEN
         status = 12
         RETURN
       END IF
       GO TO 110
     END IF

!  copy the contents of ARRAY back from the buffer i/o area

     REWIND( UNIT = buffer )
     READ( UNIT = buffer, FMT = * ) ARRAY( : used_length1, : used_length2 )

!  successful exit

 200 CONTINUE
     IF ( debug_extend ) WRITE( 6, "( A, 4( 1X, I0 ) )" )                      &
       array_name, old_length1, new_length1, old_length2, new_length2
     status = 0
     RETURN

!  end of subroutine EXTEND_array2_real

     END SUBROUTINE EXTEND_array2_real

!-*-  E X T E N D _ a r r a y 2 _ c h a r a c t e r 1 0  S U B R O U T I N E -*-

     SUBROUTINE EXTEND_array2_character( ARRAY, old_length1, old_length2,      &
                                         used_length1, used_length2,           &
                                         new_length1, new_length2,             &
                                         min_length1, min_length2, buffer,     &
                                         status, alloc_status, array_name )

!  ----------------------------------------------------------------------------
!  extend a character array so that its length is increaed from old_length
!  to as close to new_length as possible while keeping existing data intact
!  ----------------------------------------------------------------------------

!  History -
!   fortran 90 version released pre GALAHAD Version 1.0. February 7th 1995 as
!     EXTEND_array_real as part of the GALAHAD module EXTEND
!   fortran 2003 version released in SIFDECODE/CUTEst, 5th November 2012

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

     INTEGER, INTENT( IN ) :: old_length1, old_length2, buffer
     INTEGER, INTENT( OUT ) :: status, alloc_status
     INTEGER, INTENT( INOUT ) :: used_length1, used_length2
     INTEGER, INTENT( INOUT ) :: new_length1, new_length2
     INTEGER, INTENT( INOUT ) :: min_length1, min_length2
     CHARACTER ( LEN = * ), INTENT( IN ) :: array_name
     CHARACTER ( LEN = * ), ALLOCATABLE, DIMENSION( : , : ) :: ARRAY

!  local variables

     INTEGER :: length1, length2
     LOGICAL :: file_open
     CHARACTER ( LEN = LEN( ARRAY ) ), ALLOCATABLE, DIMENSION( : , :) :: DUMMY

!  ensure that the input data is consistent

     used_length1 = MIN( used_length1, old_length1 )
     used_length2 = MIN( used_length2, old_length2 )
     min_length1 = MAX( old_length1, MIN( min_length1, new_length1 ) )
     min_length2 = MAX( old_length2, MIN( min_length2, new_length2 ) )

!  if possible, allocate DUMMY to hold the old values of ARRAY

     ALLOCATE( DUMMY( used_length1, used_length2 ), STAT = alloc_status )

!  if the allocation failed, resort to using an external unit

     IF ( alloc_status /= 0 ) GO TO 100

     DUMMY( : used_length1, : used_length2 )                                   &
       = ARRAY( : used_length1, : used_length2 )

!  extend the length of ARRAY

     DEALLOCATE( ARRAY )
     length1 = new_length1 ; length2 = new_length2

  10 CONTINUE
     ALLOCATE( ARRAY( length1, length2 ), STAT = alloc_status )

!  if the allocation failed, reduce the new length and retry

     IF ( alloc_status /= 0 ) THEN
       length1 = length1 + ( length1 - min_length1 ) / 2
       length2 = length2 + ( length2 - min_length2 ) / 2

!  if there is insufficient room for both ARRAY and DUMMY, use an external unit

       IF ( length1 < min_length1 .OR. length2 < min_length2 ) THEN

!  rewind the buffer i/o unit

         INQUIRE( UNIT = buffer, OPENED = file_open )
         IF ( file_open ) THEN
           REWIND( UNIT = buffer )
         ELSE
           OPEN( UNIT = buffer )
         END IF

!  copy the contents of ARRAY into the buffer i/o area

         WRITE( UNIT = buffer, FMT = * )                                       &
           DUMMY( : used_length1 , : used_length2 )

!  extend the length of ARRAY

         DEALLOCATE( DUMMY )
         GO TO 110
       END IF
       GO TO 10
     END IF

!  copy the contents of ARRAY back from the buffer i/o area

     ARRAY( : used_length1, : used_length2 )                                   &
       = DUMMY( : used_length1, : used_length2 )
     DEALLOCATE( DUMMY )
     new_length1 = length1 ; new_length2 = length2
     GO TO 200

!  use an external unit for writing

 100 CONTINUE

!  rewind the buffer i/o unit

     INQUIRE( UNIT = buffer, OPENED = file_open )
     IF ( file_open ) THEN
       REWIND( UNIT = buffer )
     ELSE
       OPEN( UNIT = buffer )
     END IF

!  copy the contents of ARRAY into the buffer i/o area

     WRITE( UNIT = buffer, FMT = * ) ARRAY( : used_length1, : used_length2 )

!  extend the length of ARRAY

     DEALLOCATE( ARRAY )

 110 CONTINUE
     ALLOCATE( ARRAY( new_length1, new_length2 ), STAT = alloc_status )

!  if the allocation failed, reduce the new length and retry

     IF ( alloc_status /= 0 ) THEN
       new_length1 = min_length1 + ( new_length1 - min_length1 ) / 2
       new_length2 = min_length2 + ( new_length2 - min_length2 ) / 2
       IF ( new_length1 < min_length1 .OR. new_length2 < min_length2 ) THEN
         status = 12
         RETURN
       END IF
       GO TO 110
     END IF

!  copy the contents of ARRAY back from the buffer i/o area

     REWIND( UNIT = buffer )
     READ( UNIT = buffer, FMT = * ) ARRAY( : used_length1, : used_length2 )

!  successful exit

 200 CONTINUE
     IF ( debug_extend ) WRITE( 6, "( A, 4( 1X, I0 ) )" )                      &
       array_name, old_length1, new_length1, old_length2, new_length2
     status = 0
     RETURN

!  end of subroutine EXTEND_array2_character

     END SUBROUTINE EXTEND_array2_character

! -*-*-  E X T E N D _ a r r a y 3 _ i n t e g e r  S U B R O U T I N E - -*-*-

     SUBROUTINE EXTEND_array3_integer( ARRAY, old_length1, old_length2,        &
                                       old_length3, used_length1,              &
                                       used_length2, used_length3,             &
                                       new_length1, new_length2, new_length3,  &
                                       min_length1, min_length2,               &
                                       min_length3, buffer,                    &
                                       status, alloc_status, array_name )

!  -------------------------------------------------------------------------
!  extend an integer array so that its length is increaed from old_length to
!  as close to new_length as possible while keeping existing data intact
!  -------------------------------------------------------------------------

!  History -
!   fortran 90 version released pre GALAHAD Version 1.0. February 7th 1995 as
!     EXTEND_array_integer as part of the GALAHAD module EXTEND
!   fortran 2003 version released in SIFDECODE/CUTEst, 5th November 2012

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

     INTEGER, INTENT( IN ) :: old_length1, old_length2, old_length3, buffer
     INTEGER, INTENT( OUT ) :: status, alloc_status
     INTEGER, INTENT( INOUT ) :: used_length1, used_length2, used_length3
     INTEGER, INTENT( INOUT ) :: new_length1, new_length2, new_length3
     INTEGER, INTENT( INOUT ) :: min_length1, min_length2, min_length3
     CHARACTER ( LEN = * ), INTENT( IN ) :: array_name
     INTEGER, ALLOCATABLE, DIMENSION( : , : , : ) :: ARRAY

!  local variables

     INTEGER :: length1, length2, length3
     LOGICAL :: file_open
     INTEGER, ALLOCATABLE, DIMENSION( : , : , : ) :: DUMMY

!  ensure that the input data is consistent

     used_length1 = MIN( used_length1, old_length1 )
     used_length2 = MIN( used_length2, old_length2 )
     used_length3 = MIN( used_length3, old_length3 )
     min_length1 = MAX( old_length1, MIN( min_length1, new_length1 ) )
     min_length2 = MAX( old_length2, MIN( min_length2, new_length2 ) )
     min_length3 = MAX( old_length3, MIN( min_length3, new_length3 ) )

!  if possible, allocate DUMMY to hold the old values of ARRAY

     ALLOCATE( DUMMY( used_length1, used_length2, used_length3 ),              &
               STAT = alloc_status )

!  if the allocation failed, resort to using an external unit

     IF ( alloc_status /= 0 ) GO TO 100

     DUMMY( : used_length1, : used_length2, : used_length3 )                   &
       = ARRAY( : used_length1, : used_length2, : used_length3 )

!  extend the length of ARRAY

     DEALLOCATE( ARRAY )
     length1 = new_length1 ; length2 = new_length2 ; length3 = new_length3

  10 CONTINUE
     ALLOCATE( ARRAY( length1, length2, length3 ), STAT = alloc_status )

!  if the allocation failed, reduce the new length and retry

     IF ( alloc_status /= 0 ) THEN
       length1 = length1 + ( length1 - min_length1 ) / 2
       length2 = length2 + ( length2 - min_length2 ) / 2
       length3 = length3 + ( length3 - min_length3 ) / 2

!  if there is insufficient room for both ARRAY and DUMMY, use an external unit

       IF ( length1 < min_length1 .OR. length2 < min_length2 .OR.              &
            length3 < min_length3 ) THEN

!  rewind the buffer i/o unit

         INQUIRE( UNIT = buffer, OPENED = file_open )
         IF ( file_open ) THEN
           REWIND( UNIT = buffer )
         ELSE
           OPEN( UNIT = buffer )
         END IF

!  copy the contents of ARRAY into the buffer i/o area

         WRITE( UNIT = buffer, FMT = * )                                       &
           DUMMY( : used_length1 , : used_length2, : used_length3 )

!  extend the length of ARRAY

         DEALLOCATE( DUMMY )
         GO TO 110
       END IF
       GO TO 10
     END IF

!  copy the contents of ARRAY back from the buffer i/o area

     ARRAY( : used_length1, : used_length2, : used_length3 )                   &
       = DUMMY( : used_length1, : used_length2, : used_length3 )
     DEALLOCATE( DUMMY )
     new_length1 = length1 ; new_length2 = length2 ; new_length3 = length3
     GO TO 200

!  use an external unit for writing

 100 CONTINUE

!  rewind the buffer i/o unit

     INQUIRE( UNIT = buffer, OPENED = file_open )
     IF ( file_open ) THEN
       REWIND( UNIT = buffer )
     ELSE
       OPEN( UNIT = buffer )
     END IF

!  copy the contents of ARRAY into the buffer i/o area

     WRITE( UNIT = buffer, FMT = * )                                           &
       ARRAY( : used_length1, : used_length2, : used_length3 )

!  extend the length of ARRAY

     DEALLOCATE( ARRAY )

 110 CONTINUE
     ALLOCATE( ARRAY( new_length1, new_length2, new_length3 ),                 &
               STAT = alloc_status )

!  if the allocation failed, reduce the new length and retry

     IF ( alloc_status /= 0 ) THEN
       new_length1 = min_length1 + ( new_length1 - min_length1 ) / 2
       new_length2 = min_length2 + ( new_length2 - min_length2 ) / 2
       new_length3 = min_length3 + ( new_length3 - min_length3 ) / 2
       IF ( new_length1 < min_length1 .OR. new_length2 < min_length2 .OR.      &
            new_length3 < min_length3 ) THEN
         status = 12
         RETURN
       END IF
       GO TO 110
     END IF

!  copy the contents of ARRAY back from the buffer i/o area

     REWIND( UNIT = buffer )
     READ( UNIT = buffer, FMT = * )                                            &
       ARRAY( : used_length1, : used_length2, : used_length3 )

!  successful exit

 200 CONTINUE
     IF ( debug_extend ) WRITE( 6, "( A, 6( 1X, I0 ) )" )                      &
       array_name, old_length1, new_length1, old_length2, new_length2,         &
       old_length3, new_length3
     status = 0
     RETURN

!  end of subroutine EXTEND_array3_integer

     END SUBROUTINE EXTEND_array3_integer

    END MODULE SIFDECODE

