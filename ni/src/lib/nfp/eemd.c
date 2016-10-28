/* Copyright 2013 Perttu Luukko

 * This file is part of libeemd.

 * libeemd is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.

 * libeemd is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with libeemd.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "eemd.h"

// If we are using OpenMP for parallel computation, we need locks to ensure
// that the same output data is not written by several threads at the same
// time.
#ifdef _OPENMP
typedef omp_lock_t lock;
inline static void init_lock(lock* l) { omp_init_lock(l); }
inline static void destroy_lock(lock* l) { omp_destroy_lock(l); }
inline static void get_lock(lock* l) { omp_set_lock(l); }
inline static void release_lock(lock* l) { omp_unset_lock(l); }
#else
// If we don't use OpenMP, we provide a dummy lock that does nothing. This
// avoids littering the code with too many #ifdefs for _OPENMP.
typedef char lock;
inline static void init_lock(__attribute__((unused)) lock* l) {}
inline static void destroy_lock(__attribute__((unused)) lock* l) {}
inline static void get_lock(__attribute__((unused)) lock* l) {}
inline static void release_lock(__attribute__((unused)) lock* l) {}
#endif


// Helper functions for working with data arrays
inline static void array_copy(double const* restrict src, size_t n, double* restrict dest) {
	memcpy(dest, src, n*sizeof(double));
}

inline static void array_add(double const* src, size_t n, double* dest) {
	for (size_t i=0; i<n; i++)
		dest[i] += src[i];
}

inline static void array_add_to(double const* src1, double const* src2, size_t n, double* dest) {
	for (size_t i=0; i<n; i++)
		dest[i] = src1[i] + src2[i];
}

inline static void array_addmul_to(double const* src1, double const* src2, double val, size_t n, double* dest) {
	for (size_t i=0; i<n; i++)
		dest[i] = src1[i] + val*src2[i];
}

inline static void array_sub(double const* src, size_t n, double* dest) {
	for (size_t i=0; i<n; i++)
		dest[i] -= src[i];
}

inline static void array_mult(double* dest, size_t n, double val) {
	for (size_t i=0; i<n; i++)
		dest[i] *= val;
}

// Helper function for extrapolating data at the ends. For a line passing
// through (x0, y0), (x1, y1), and (x, y), return y for a given x.
inline static double linear_extrapolate(double x0, double y0,
		double x1, double y1, double x) {
	assert(x1 != x0);
	return y0 + (y1-y0)*(x-x0)/(x1-x0);
}

// In the following part the necessary workspace memory structures for several
// EMD operations are defined

// For sifting we need arrays for storing the found extrema of the signal, and memory required
// to form the spline envelopes
typedef struct {
	// Number of samples in the signal
	size_t N;
	// Found extrema
	double* restrict maxx;
	double* restrict maxy;
	double* restrict minx;
	double* restrict miny;
	// Upper and lower envelope spline values
	double* restrict maxspline;
	double* restrict minspline;
	// Extra memory required for spline evaluation
	double* restrict spline_workspace;
} sifting_workspace;

sifting_workspace* allocate_sifting_workspace(size_t N) {
	sifting_workspace* w = malloc(sizeof(sifting_workspace));
	w->N = N;
	w->maxx = malloc(N*sizeof(double));
	w->maxy = malloc(N*sizeof(double));
	w->minx = malloc(N*sizeof(double));
	w->miny = malloc(N*sizeof(double));
	w->maxspline = malloc(N*sizeof(double));
	w->minspline = malloc(N*sizeof(double));
	// Spline evaluation requires 5*m-10 doubles where m is the number of
	// extrema. The worst case scenario is that every point is an extrema, so
	// use m=N to be safe.
	const size_t spline_workspace_size = (N > 2)? 5*N-10 : 0;
	w->spline_workspace = malloc(spline_workspace_size*sizeof(double));
	return w;
}

void free_sifting_workspace(sifting_workspace* w) {
	free(w->spline_workspace); w->spline_workspace = NULL;
	free(w->minspline); w->minspline = NULL;
	free(w->maxspline); w->maxspline = NULL;
	free(w->miny); w->miny = NULL;
	free(w->minx); w->minx = NULL;
	free(w->maxy); w->maxy = NULL;
	free(w->maxx); w->maxx = NULL;
	free(w); w = NULL;
}


// For EMD we need space to do the sifting and somewhere to save the residual from the previous run.
// We also leave room for an array of locks to protect multi-threaded EMD.
typedef struct {
	size_t N;
	// Previous residual for EMD
	double* restrict res;
	// What is needed for sifting
	sifting_workspace* restrict sift_w;
	// A pointer for shared locks. These locks are used to make EMD thread-safe
	// even when several threads run EMD with the same output matrix (we'll do
	// this in EEMD).
	lock** locks;
} emd_workspace;

emd_workspace* allocate_emd_workspace(size_t N) {
	emd_workspace* w = malloc(sizeof(emd_workspace));
	w->N = N;
	w->res = malloc(N*sizeof(double));
	w->sift_w = allocate_sifting_workspace(N);
	w->locks = NULL; // The locks are assumed to be allocated and freed independently
	return w;
}

void free_emd_workspace(emd_workspace* w) {
	free_sifting_workspace(w->sift_w);
	free(w->res); w->res = NULL;
	free(w); w = NULL;
}


// EEMD needs a random number generator in addition to emd_workspace. We also need a place to store
// the member of the ensemble (input signal + realization of noise) to be worked on.
typedef struct {
	size_t N;
	// The random number generator
	gsl_rng* r;
	// The ensemble member signal
	double* restrict x;
	// What is needed for running EMD
	emd_workspace* restrict emd_w;
} eemd_workspace;

eemd_workspace* allocate_eemd_workspace(size_t N) {
	eemd_workspace* w = malloc(sizeof(eemd_workspace));
	w->N = N;
	w->r = gsl_rng_alloc(gsl_rng_mt19937);
	w->x = malloc(N*sizeof(double));
	w->emd_w = allocate_emd_workspace(N);
	return w;
}

void set_rng_seed(eemd_workspace* w, unsigned long int rng_seed) {
	gsl_rng_set(w->r, rng_seed);
}

void free_eemd_workspace(eemd_workspace* w) {
	free_emd_workspace(w->emd_w);
	free(w->x); w->x = NULL;
	gsl_rng_free(w->r); w->r = NULL;
	free(w); w = NULL;
}

// Forward declaration of a helper function used internally for making a single
// EMD run with a preallocated workspace
static libeemd_error_code _emd(double* restrict input, emd_workspace* restrict w,
		double* restrict output, size_t M,
		unsigned int S_number, unsigned int num_siftings);

// Forward declaration of a helper function for applying the sifting procedure to
// input until it is reduced to an IMF according to the stopping criteria given
// by S_number and num_siftings
static libeemd_error_code _sift(double* restrict input, sifting_workspace*
		restrict w, unsigned int S_number, unsigned int num_siftings, unsigned int*
		sift_counter);

// Forward declaration of a helper function for parameter validation shared by functions eemd and ceemdan
static inline libeemd_error_code _validate_eemd_parameters(unsigned int ensemble_size, double noise_strength, unsigned int S_number, unsigned int num_siftings);

// Main EEMD decomposition routine definition
libeemd_error_code eemd(double const* restrict input, size_t N,
		double* restrict output, size_t M,
		unsigned int ensemble_size, double noise_strength, unsigned int
		S_number, unsigned int num_siftings, unsigned long int rng_seed) {
	gsl_set_error_handler_off();
	// Validate parameters
	libeemd_error_code validation_result = _validate_eemd_parameters(ensemble_size, noise_strength, S_number, num_siftings);
	if (validation_result != EMD_SUCCESS) {
		return validation_result;
	}
	// For empty data we have nothing to do
	if (N == 0) {
		return EMD_SUCCESS;
	}
	if (M == 0) {
		M = emd_num_imfs(N);
	}
	// The noise standard deviation is noise_strength times the standard deviation of input data
	const double noise_sigma = (noise_strength != 0)? gsl_stats_sd(input, 1, N)*noise_strength : 0;
	// Initialize output data to zero
	memset(output, 0x00, M*N*sizeof(double));
	// Each thread gets a separate workspace if we are using OpenMP
	eemd_workspace** ws = NULL;
	// The locks are shared among all threads
	lock** locks;
	// Don't start unnecessary threads if the ensemble is small
	#ifdef _OPENMP
	if (omp_get_num_threads() > (int)ensemble_size) {
		omp_set_num_threads(ensemble_size);
	}
	#endif
	unsigned int ensemble_counter = 0;
	// The following section is executed in parallel
	libeemd_error_code emd_err = EMD_SUCCESS;
	#pragma omp parallel
	{
		#ifdef _OPENMP
		const int num_threads = omp_get_num_threads();
		const int thread_id = omp_get_thread_num();
		#if EEMD_DEBUG >= 1
		#pragma omp single
		fprintf(stderr, "Using %d thread(s) with OpenMP.\n", num_threads);
		#endif
		#else
		const int num_threads = 1;
		const int thread_id = 0;
		#endif
		#pragma omp single
		{
			ws = malloc(num_threads*sizeof(eemd_workspace*));
			locks = malloc(M*sizeof(lock*));
			for (size_t i=0; i<M; i++) {
				locks[i] = malloc(sizeof(lock));
				init_lock(locks[i]);
			}
		}
		// Each thread allocates its own workspace
		ws[thread_id] = allocate_eemd_workspace(N);
		eemd_workspace* w = ws[thread_id];
		// All threads share the same array of locks
		w->emd_w->locks = locks;
		// Loop over all ensemble members, dividing them among the threads
		#pragma omp for
		for (size_t en_i=0; en_i<ensemble_size; en_i++) {
			// Check if an error has occured in other threads
			#pragma omp flush(emd_err)
			if (emd_err != EMD_SUCCESS) {
				continue;
			}
			// Initialize ensemble member as input data + noise
			if (noise_strength == 0.0) {
				array_copy(input, N, w->x);
			}
			else {
				// set rng seed based on ensemble member to ensure
				// reproducibility even in a multithreaded case
				set_rng_seed(w, rng_seed+en_i);
				for (size_t i=0; i<N; i++) {
					w->x[i] = input[i] + gsl_ran_gaussian(w->r, noise_sigma);
				}
			}
			// Extract IMFs with EMD
			emd_err = _emd(w->x, w->emd_w, output, M, S_number, num_siftings);
			#pragma omp flush(emd_err)
			#pragma omp atomic
			ensemble_counter++;
			#if EEMD_DEBUG >= 1
			fprintf(stderr, "Ensemble iteration %u/%u done.\n", ensemble_counter, ensemble_size);
			#endif
		}
		// Free resources
		free_eemd_workspace(w);
		#pragma omp single
		{
			free(ws); ws = NULL;
			for (size_t i=0; i<M; i++) {
				destroy_lock(locks[i]);
				free(locks[i]);
			}
			free(locks); locks = NULL;
		}
	} // End of parallel block
	if (emd_err != EMD_SUCCESS) {
		return emd_err;
	}
	// Divide output data by the ensemble size to get the average
	if (ensemble_size != 1) {
		const double one_per_ensemble_size = 1.0/ensemble_size;
		array_mult(output, N*M, one_per_ensemble_size);
	}
	return EMD_SUCCESS;
}

// Main CEEMDAN decomposition routine definition
libeemd_error_code ceemdan(double const* restrict input, size_t N,
		double* restrict output, size_t M,
		unsigned int ensemble_size, double noise_strength, unsigned int
		S_number, unsigned int num_siftings, unsigned long int rng_seed) {
	gsl_set_error_handler_off();
	// Validate parameters
	libeemd_error_code validation_result = _validate_eemd_parameters(ensemble_size, noise_strength, S_number, num_siftings);
	if (validation_result != EMD_SUCCESS) {
		return validation_result;
	}
	// For empty data we have nothing to do
	if (N == 0) {
		return EMD_SUCCESS;
	}
	// For M == 1 the only "IMF" is the residual
	if (M == 1) {
		memcpy(output, input, N*sizeof(double));
		return EMD_SUCCESS;
	}
	if (M == 0) {
		M = emd_num_imfs(N);
	}
	const double one_per_ensemble_size = 1.0/ensemble_size;
	// Initialize output data to zero
	memset(output, 0x00, M*N*sizeof(double));
	// Each thread gets a separate workspace if we are using OpenMP
	eemd_workspace** ws = NULL;
	// All threads need to write to the same row of the output matrix
	// so we need only one shared lock
	lock* output_lock = malloc(sizeof(lock));
	init_lock(output_lock);
	// The threads also share the same precomputed noise
	double* noises = malloc(ensemble_size*N*sizeof(double));
	// Since we need to decompose this noise by EMD, we also need arrays for storing
	// the residuals
	double* noise_residuals = malloc(ensemble_size*N*sizeof(double));
	// Don't start unnecessary threads if the ensemble is small
	#ifdef _OPENMP
	if (omp_get_num_threads() > (int)ensemble_size) {
		omp_set_num_threads(ensemble_size);
	}
	#endif
	int num_threads;
	// The following section is executed in parallel
	#pragma omp parallel
	{
		#ifdef _OPENMP
		num_threads = omp_get_num_threads();
		const int thread_id = omp_get_thread_num();
		#if EEMD_DEBUG >= 1
		#pragma omp single
		fprintf(stderr, "Using %d thread(s) with OpenMP.\n", num_threads);
		#endif
		#else
		num_threads = 1;
		const int thread_id = 0;
		#endif
		#pragma omp single
		{
			ws = malloc(num_threads*sizeof(eemd_workspace*));
		}
		// Each thread allocates its own workspace
		ws[thread_id] = allocate_eemd_workspace(N);
		eemd_workspace* w = ws[thread_id];
		// Precompute and store white noise, since for each mode of the data we
		// need the same mode of the corresponding realization of noise
		#pragma omp for
		for (size_t en_i=0; en_i<ensemble_size; en_i++) {
			// set rng seed based on ensemble member to ensure
			// reproducibility even in a multithreaded case
			set_rng_seed(w, rng_seed+en_i);
			for (size_t j=0; j<N; j++) {
				noises[N*en_i+j] = gsl_ran_gaussian(w->r, 1.0);
			}
		}
	} // Return to sequental mode
	// Allocate memory for the residual shared among all threads
	double* restrict res = malloc(N*sizeof(double));
	// For the first iteration the residual is the input signal
	array_copy(input, N, res);
	// Each mode is extracted sequentially, but we use parallelization in the inner loop
	// to loop over ensemble members
	for (size_t imf_i=0; imf_i<M; imf_i++) {
		// Provide a pointer to the output vector where this IMF will be stored
		double* const imf = &output[imf_i*N];
		// Then we go parallel to compute the different ensemble members
		libeemd_error_code sift_err = EMD_SUCCESS;
		#pragma omp parallel
		{
			#ifdef _OPENMP
			const int thread_id = omp_get_thread_num();
			#else
			const int thread_id = 0;
			#endif
			eemd_workspace* w = ws[thread_id];
			unsigned int sift_counter = 0;
			#pragma omp for
			for (size_t en_i=0; en_i<ensemble_size; en_i++) {
				// Check if an error has occured in other threads
				#pragma omp flush(sift_err)
				if (sift_err != EMD_SUCCESS) {
					continue;
				}
				// Provide a pointer to the noise vector and noise residual used by
				// this ensemble member
				double* const noise = &noises[N*en_i];
				double* const noise_residual = &noise_residuals[N*en_i];
				// Initialize input signal as data + noise.
				// The noise standard deviation is noise_strength times the
				// standard deviation of input data divided by the standard
				// deviation of the noise. This is used to fix the SNR at each
				// stage.
				const double noise_sigma = noise_strength*gsl_stats_sd(res, 1, N)/gsl_stats_sd(noise, 1, N);
				array_addmul_to(res, noise, noise_sigma, N, w->x);
				// Sift to extract first EMD mode
				sift_err = _sift(w->x, w->emd_w->sift_w, S_number, num_siftings, &sift_counter);
				#pragma omp flush(sift_err)
				// Sum to output vector
				get_lock(output_lock);
				array_add(w->x, N, imf);
				release_lock(output_lock);
				// Extract next EMD mode of the noise. This is used as the noise for
				// the next mode extracted from the data
				if (imf_i == 0) {
					array_copy(noise, N, noise_residual);
				}
				else {
					array_copy(noise_residual, N, noise);
				}
				sift_err = _sift(noise, w->emd_w->sift_w, S_number, num_siftings, &sift_counter);
				#pragma omp flush(sift_err)
				array_sub(noise, N, noise_residual);
			}
		} // Parallel section ends
		if (sift_err != EMD_SUCCESS) {
			return sift_err;
		}
		// Divide with ensemble size to get the average
		array_mult(imf, N, one_per_ensemble_size);
		// Subtract this IMF from the previous residual to form the new one
		array_sub(imf, N, res);
	}
	// Save final residual
	get_lock(output_lock);
	array_add(res, N, output+N*(M-1));
	release_lock(output_lock);
	// Free global resources
	for (int thread_id=0; thread_id<num_threads; thread_id++) {
		free_eemd_workspace(ws[thread_id]);
	}
	free(ws); ws = NULL;
	free(res); res = NULL;
	free(noise_residuals); noise_residuals = NULL;
	free(noises); noises = NULL;
	destroy_lock(output_lock);
	free(output_lock); output_lock = NULL;
	return EMD_SUCCESS;
}

static inline libeemd_error_code _validate_eemd_parameters(unsigned int ensemble_size, double noise_strength, unsigned int S_number, unsigned int num_siftings) {
	if (ensemble_size < 1) {
		return EMD_INVALID_ENSEMBLE_SIZE;
	}
	if (noise_strength < 0) {
		return EMD_INVALID_NOISE_STRENGTH;
	}
	if (ensemble_size == 1 && noise_strength > 0) {
		return EMD_NOISE_ADDED_TO_EMD;
	}
	if (ensemble_size > 1 && noise_strength == 0) {
		return EMD_NO_NOISE_ADDED_TO_EEMD;
	}
	if (S_number == 0 && num_siftings == 0) {
		return EMD_NO_CONVERGENCE_POSSIBLE;
	}
	return EMD_SUCCESS;
}

// Helper function for applying the sifting procedure to input until it is
// reduced to an IMF according to the stopping criteria given by S_number and
// num_siftings. The required number of siftings is saved to sift_counter.
static libeemd_error_code _sift(double* restrict input, sifting_workspace*
		restrict w, unsigned int S_number, unsigned int num_siftings,
		unsigned int* sift_counter) {
	const size_t N = w->N;
	// Provide some shorthands to avoid excessive '->' operators
	double* const maxx = w->maxx;
	double* const maxy = w->maxy;
	double* const minx = w->minx;
	double* const miny = w->miny;
	// Initialize counters that keep track of the number of siftings
	// and the S number
	*sift_counter = 0;
	unsigned int S_counter = 0;
	// Numbers of extrema and zero crossings are initialized to dummy values
	size_t num_max = (size_t)(-1);
	size_t num_min = (size_t)(-1);
	size_t num_zc = (size_t)(-1);
	size_t prev_num_max = (size_t)(-1);
	size_t prev_num_min = (size_t)(-1);
	size_t prev_num_zc = (size_t)(-1);
	while (num_siftings == 0 || *sift_counter < num_siftings) {
		(*sift_counter)++;
		#if EEMD_DEBUG >= 1
		if (*sift_counter == 10000) {
			fprintf(stderr, "Something is probably wrong. Sift counter has reached 10000.\n");
		}
		#endif
		prev_num_max = num_max;
		prev_num_min = num_min;
		prev_num_zc = num_zc;
		// Find extrema and count zero crossings
		emd_find_extrema(input, N, maxx, maxy, &num_max, minx, miny, &num_min, &num_zc);
		// Check if we are finished based on the S-number criteria
		if (S_number != 0) {
			const int max_diff = (int)num_max - (int)prev_num_max;
			const int min_diff = (int)num_min - (int)prev_num_min;
			const int zc_diff = (int)num_zc - (int)prev_num_zc;
			if (abs(max_diff)+abs(min_diff)+abs(zc_diff) <= 1) {
				S_counter++;
				if (S_counter >= S_number) {
					const int num_diff = (int)num_min + (int)num_max - 4 - (int)num_zc;
					if (abs(num_diff) <= 1) {
						// Number of extrema has been stable for S_number steps
						// and the number of *interior* extrema and zero
						// crossings differ by at most one -- we are converged
						// according to the S-number criterion
						break;
					}
				}
			}
			else {
				S_counter = 0;
			}
		}
		// Fit splines, choose order of spline based on the number of extrema
		libeemd_error_code max_errcode = emd_evaluate_spline(maxx, maxy, num_max, w->maxspline, w->spline_workspace);
		if (max_errcode != EMD_SUCCESS) {
			return max_errcode;
		}
		libeemd_error_code min_errcode = emd_evaluate_spline(minx, miny, num_min, w->minspline, w->spline_workspace);
		if (min_errcode != EMD_SUCCESS) {
			return min_errcode;
		}
		// Subtract envelope mean from the data
		for (size_t i=0; i<N; i++) {
			input[i] -= 0.5*(w->maxspline[i] + w->minspline[i]);
		}
	}
	return EMD_SUCCESS;
}

// Helper function for extracting all IMFs from input using the sifting
// procedure defined by _sift. The contents of the input array are destroyed in
// the process.
static libeemd_error_code _emd(double* restrict input, emd_workspace* restrict w,
		double* restrict output, size_t M,
		unsigned int S_number, unsigned int num_siftings) {
	// Provide some shorthands to avoid excessive '->' operators
	const size_t N = w->N;
	double* const res = w->res;
	lock** locks = w->locks;
	if (M == 0) {
		M = emd_num_imfs(N);
	}
	// We need to store a copy of the original signal so that once it is
	// reduced to an IMF we have something to subtract the IMF from to form
	// the residual for the next iteration
	array_copy(input, N, res);
	// Loop over all IMFs to be separated from input
	unsigned int sift_counter;
	for (size_t imf_i=0; imf_i<M-1; imf_i++) {
		if (imf_i != 0) {
			// Except for the first iteration, restore the previous residual
			// and use it as an input
			array_copy(res, N, input);
		}
		// Perform siftings on input until it is an IMF
		libeemd_error_code sift_err = _sift(input, w->sift_w, S_number, num_siftings, &sift_counter);
		if (sift_err != EMD_SUCCESS) {
			return sift_err;
		}
		// Subtract this IMF from the saved copy to form the residual for
		// the next round
		array_sub(input, N, res);
		// Add the discovered IMF to the output matrix. Use locks to ensure
		// other threads are not writing to the same row of the output matrix
		// at the same time
		get_lock(locks[imf_i]);
		array_add(input, N, output+N*imf_i);
		release_lock(locks[imf_i]);
		#if EEMD_DEBUG >= 2
		fprintf(stderr, "IMF %zd saved after %u siftings.\n", imf_i+1, sift_counter);
		#endif
	}
	// Save final residual
	get_lock(locks[M-1]);
	array_add(res, N, output+N*(M-1));
	release_lock(locks[M-1]);
	return EMD_SUCCESS;
}

void emd_find_extrema(double const* restrict x, size_t N,
		double* restrict maxx, double* restrict maxy, size_t* nmax,
		double* restrict minx, double* restrict miny, size_t* nmin,
		size_t* nzc) {
	// Set the number of extrema and zero crossings to zero initially
	*nmax = 0;
	*nmin = 0;
	*nzc = 0;
	// Handle empty array as a special case
	if (N == 0) {
		return;
	}
	// Add the ends of the data as both local minima and maxima. These
	// might be changed later by linear extrapolation.
	maxx[0] = 0;
	maxy[0] = x[0];
	(*nmax)++;
	minx[0] = 0;
	miny[0] = x[0];
	(*nmin)++;
	// If we had only one data point this is it
	if (N == 1) {
		return;
	}
	// Now starts the main extrema-finding loop. The loop detects points where
	// the slope of the data changes sign. In the case of flat regions at the
	// extrema, the center point of the flat region will be considered the
	// extremal point. While detecting extrema, the loop also counts the number
	// of zero crossings that occur.
	enum slope { UP, DOWN, NONE };
	enum sign { POS, NEG, ZERO };
	enum slope previous_slope = NONE;
	enum sign previous_sign = (x[0] < -0)? NEG : ((x[0] > 0)? POS : ZERO);
	int flat_counter = 0;
	for (size_t i=0; i<N-1; i++) {
		if (x[i+1] > x[i]) { // Going up
			if (previous_slope == DOWN) {
				// Was going down before -> local minimum found
				minx[*nmin] = (double)(i)-(double)(flat_counter)/2;
				miny[*nmin] = x[i];
				(*nmin)++;
			}
			if (previous_sign == NEG && x[i+1] > 0) { // zero crossing from neg to pos
				(*nzc)++;
				previous_sign = POS;
			}
			else if (previous_sign == ZERO && x[i+1] > 0) {
				// this needs to be handled as an unfortunate special case
				previous_sign = POS;
			}
			previous_slope = UP;
			flat_counter = 0;
		}
		else if (x[i+1] < x[i]) { // Going down
			if (previous_slope == UP) {
				// Was going up before -> local maximum found
				maxx[*nmax] = (double)(i)-(double)(flat_counter)/2;
				maxy[*nmax] = x[i];
				(*nmax)++;
			}
			if (previous_sign == POS && x[i+1] < -0) { // zero crossing from pos to neg
				(*nzc)++;
				previous_sign = NEG;
			}
			else if (previous_sign == ZERO && x[i+1] < -0) {
				// this needs to be handled as an unfortunate special case
				previous_sign = NEG;
			}
			previous_slope = DOWN;
			flat_counter = 0;
		}
		else { // Staying flat
			flat_counter++;
			#if EEMD_DEBUG >= 3
			fprintf(stderr, "Warning: a flat slope found in data. The results will differ from the reference EEMD implementation.\n");
			#endif
		}
	}
	// Add the other end of the data as extrema as well.
	maxx[*nmax] = N-1;
	maxy[*nmax] = x[N-1];
	(*nmax)++;
	minx[*nmin] = N-1;
	miny[*nmin] = x[N-1];
	(*nmin)++;
	// If we have at least two interior extrema, test if linear extrapolation provides
	// a more extremal value.
	if (*nmax >= 4) {
		const double max_el = linear_extrapolate(maxx[1], maxy[1],
				maxx[2], maxy[2], 0);
		if (max_el > maxy[0])
			maxy[0] = max_el;
		const double max_er = linear_extrapolate(maxx[*nmax-3], maxy[*nmax-3],
				maxx[*nmax-2], maxy[*nmax-2], N-1);
		if (max_er > maxy[*nmax-1])
			maxy[*nmax-1] = max_er;
	}
	if (*nmin >= 4) {
		const double min_el = linear_extrapolate(minx[1], miny[1],
				minx[2], miny[2], 0);
		if (min_el < miny[0])
			miny[0] = min_el;
		const double min_er = linear_extrapolate(minx[*nmin-3], miny[*nmin-3],
				minx[*nmin-2], miny[*nmin-2], N-1);
		if (min_er < miny[*nmin-1])
			miny[*nmin-1] = min_er;
	}
	return;
}

size_t emd_num_imfs(size_t N) {
	if (N == 0) {
		return 0;
	}
	if (N <= 3) {
		return 1;
	}
	return (size_t)(log2(N));
}

libeemd_error_code emd_evaluate_spline(double const* restrict x, double const* restrict y,
		size_t N, double* restrict spline_y, double* restrict spline_workspace) {
	gsl_set_error_handler_off();
	const size_t n = N-1;
	const size_t max_j = (size_t)x[n];
	if (N <= 1) {
		return EMD_NOT_ENOUGH_POINTS_FOR_SPLINE;
	}
	// perform more assertions only if EEMD_DEBUG is on,
	// as this function is meant only for internal use
	#if EEMD_DEBUG >= 1
	if (x[0] != 0) {
		return EMD_INVALID_SPLINE_POINTS;
	}
	for (size_t i=1; i<N; i++) {
		if (x[i] <= x[i-1]) {
			return EMD_INVALID_SPLINE_POINTS;
		}
	}
	#endif
	// Fall back to linear interpolation (for N==2) or polynomial interpolation
	// (for N==3)
	if (N <= 3) {
		int gsl_status = gsl_poly_dd_init(spline_workspace, x, y, N);
		if (gsl_status != GSL_SUCCESS) {
			fprintf(stderr, "Error reported by gsl_poly_dd_init: %s\n",
				gsl_strerror(gsl_status));
			return EMD_GSL_ERROR;
		}
		for (size_t j=0; j<=max_j; j++) {
			spline_y[j] = gsl_poly_dd_eval(spline_workspace, x, N, j);
		}
		return EMD_SUCCESS;
	}
	// For N >= 4, interpolate by using cubic splines with not-a-node end conditions.
	// This algorithm is described in "Numerical Algorithms with C" by
	// G. Engeln-Müllges and F. Uhlig, page 257.
	//
	// Extra homework assignment for anyone reading this: Implement this
	// algorithm in GSL, so that next time someone needs these end conditions
	// they can just use GSL.
	const size_t sys_size = N-2;
	double* const c = spline_workspace;
	double* const diag = c+N;
	double* const supdiag = diag + sys_size;
	double* const subdiag = supdiag + (sys_size-1);
	double* const g = subdiag + (sys_size-1);
	// Define some constants for easier comparison with Engeln-Mullges & Uhlig
	// and let the compiler optimize them away.
	const double h_0 = x[1]-x[0];
	const double h_1 = x[2]-x[1];
	const double h_nm1 = x[n]-x[n-1];
	const double h_nm2 = x[n-1]-x[n-2];
	// Describe the (N-2)x(N-2) linear system Ac=g with the tridiagonal
	// matrix A defined by subdiag, diag and supdiag
	// first row
	diag[0] = h_0 + 2*h_1;
	supdiag[0] = h_1 - h_0;
	g[0] = 3.0/(h_0 + h_1)*((y[2]-y[1]) - (h_1/h_0)*(y[1]-y[0]));
	// rows 2 to n-2
	for (size_t i=2; i<=n-2; i++) {
		const double h_i = x[i+1] - x[i];
		const double h_im1 = x[i] - x[i-1];
		
		subdiag[i-2] = h_im1;
		diag[i-1] = 2*(h_im1 + h_i);
		supdiag[i-1] = h_i;
		g[i-1] = 3.0*((y[i+1]-y[i])/h_i - (y[i]-y[i-1])/h_im1);
	}
	// final row
	subdiag[n-3] = h_nm2 - h_nm1;
	diag[n-2] = 2*h_nm2 + h_nm1;
	g[n-2] = 3.0/(h_nm1 + h_nm2)*((h_nm2/h_nm1)*(y[n]-y[n-1]) - (y[n-1]-y[n-2]));
	// Solve to get c_1 ... c_{n-1}
	gsl_vector_view diag_vec = gsl_vector_view_array(diag, n-1);
	gsl_vector_view supdiag_vec = gsl_vector_view_array(supdiag, n-2);
	gsl_vector_view subdiag_vec = gsl_vector_view_array(subdiag, n-2);
	gsl_vector_view g_vec = gsl_vector_view_array(g, n-1);
	gsl_vector_view solution_vec = gsl_vector_view_array(c+1, n-1);
	int gsl_status = gsl_linalg_solve_tridiag(&diag_vec.vector,
			                                    &supdiag_vec.vector,
												&subdiag_vec.vector,
												&g_vec.vector,
												&solution_vec.vector);
	if (gsl_status != GSL_SUCCESS) {
		fprintf(stderr, "Error reported by gsl_linalg_solve_tridiag: %s\n",
				gsl_strerror(gsl_status));
		return EMD_GSL_ERROR;
	}
	// Compute c[0] and c[n]
	c[0] = c[1] + (h_0/h_1)*(c[1]-c[2]);
	c[n] = c[n-1] + (h_nm1/h_nm2)*(c[n-1]-c[n-2]);
	// The coefficients b_i and d_i are computed from the c_i's, so just
	// evaluate the spline at the required points. In this case it is easy to
	// find the required interval for spline evaluation, since the evaluation
	// points j just increase monotonically from 0 to max_j.
	size_t i = 0;
	for (size_t j=0; j<=max_j; j++) {
		if (j > x[i+1]) {
			i++;
			assert(i < n);
		}
		const double dx = j-x[i];
		if (dx == 0) {
			spline_y[j] = y[i];
			continue;
		}
		// Compute coefficients b_i and d_i
		const double h_i = x[i+1] - x[i];
		const double a_i = y[i];
		const double b_i = (y[i+1]-y[i])/h_i - (h_i/3.0)*(c[i+1]+2*c[i]);
		const double c_i = c[i];
		const double d_i = (c[i+1]-c[i])/(3.0*h_i);
		// evaluate spline at x=j using the Horner scheme
		spline_y[j] = a_i + dx*(b_i + dx*(c_i + dx*d_i));
	}
	return EMD_SUCCESS;
}

// Helper functions for printing what error codes mean
void emd_report_to_file_if_error(FILE* file, libeemd_error_code err) {
	if (err == EMD_SUCCESS) {
		return;
	}
	fprintf(file, "libeemd error: ");
	switch (err) {
		case EMD_INVALID_ENSEMBLE_SIZE :
			fprintf(file, "Invalid ensemble size (zero or negative)\n");
			break;
		case EMD_INVALID_NOISE_STRENGTH :
			fprintf(file, "Invalid noise strength (negative)\n");
			break;
		case EMD_NOISE_ADDED_TO_EMD :
			fprintf(file, "Positive noise strength but ensemble size is one (regular EMD)\n");
			break;
		case EMD_NO_NOISE_ADDED_TO_EEMD :
			fprintf(file, "Ensemble size is more than one (EEMD) but noise strength is zero\n");
			break;
		case EMD_NO_CONVERGENCE_POSSIBLE :
			fprintf(file, "Stopping criteria invalid: would never converge\n");
			break;
		case EMD_NOT_ENOUGH_POINTS_FOR_SPLINE :
			fprintf(file, "Spline evaluation tried with insufficient points\n");
			break;
		case EMD_INVALID_SPLINE_POINTS :
			fprintf(file, "Spline evaluation points invalid\n");
			break;
		case EMD_GSL_ERROR :
			fprintf(file, "Error reported by GSL library\n");
			break;
		default :
			fprintf(file, "Error code with unknown meaning. Please file a bug!\n");
	}
}

void emd_report_if_error(libeemd_error_code err) {
	emd_report_to_file_if_error(stderr, err);
}
