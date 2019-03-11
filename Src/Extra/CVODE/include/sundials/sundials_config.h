/*
 * -----------------------------------------------------------------
 * Copyright (c) 2005, The Regents of the University of California.
 * Produced at the Lawrence Livermore National Laboratory.
 * All rights reserved.
 * For details, see the LICENSE file.
 *------------------------------------------------------------------
 * SUNDIALS configuration header file
 *------------------------------------------------------------------
 */

/* Define SUNDIALS version number */
#define SUNDIALS_PACKAGE_VERSION "2.4.0"

/* Define precision of SUNDIALS data type 'realtype' 
 * Depending on the precision level, one of the following 
 * three macros will be defined:
 *     #define SUNDIALS_SINGLE_PRECISION 1
 *     #define SUNDIALS_DOUBLE_PRECISION 1
 *     #define SUNDIALS_EXTENDED_PRECISION 1
 */
#define SUNDIALS_DOUBLE_PRECISION 1

/* Use generic math functions 
 * If it was decided that generic math functions can be used, then
 *     #define SUNDIALS_USE_GENERIC_MATH 1
 * otherwise
 *     #define SUNDIALS_USE_GENERIC_MATH 0
 */
#define SUNDIALS_USE_GENERIC_MATH 1

/* Blas/Lapack available
 * If working libraries for Blas/lapack support were found, then
 *     #define SUNDIALS_BLAS_LAPACK 1
 * otherwise
 *     #define SUNDIALS_BLAS_LAPACK 0
 */
#define SUNDIALS_BLAS_LAPACK 0

/* Mark SUNDIALS API functions for export/import
 * When building shared SUNDIALS libraries under Windows, use
 *      #define SUNDIALS_EXPORT __declspec(dllexport)
 * When linking to shared SUNDIALS libraries under Windows, use
 *      #define SUNDIALS_EXPORT __declspec(dllimport)
 * In all other cases (other platforms or static libraries under
 * Windows), the SUNDIALS_EXPORT macro is empty
 */
#define SUNDIALS_EXPORT __declspec(dllexport)
