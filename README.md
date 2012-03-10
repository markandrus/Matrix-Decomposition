Matrix Decomposition
====================
This source repository contains my solutions to Homework 6, consisting of the three programs

1. `main`
2. `tests` 
3. `lsa`

Building
--------
All three programs are written in Haskell and rely on GHC and a number of Cabal packages.

The included `Makefile` handles compilation, and the three binaries may be built with `make`.

### Package Requirements
If `make` fails, it is probably because you are missing a cabal package. These programs rely on [HMatrix](https://github.com/AlbertoRuiz/hmatrix), which can be installed with the included `setup.sh` script, or with the command `cabal install hmatrix`.

* HMatrix
* QuickCheck

Eigendecomposition and Singular Value Decomposition (SVD)
---------------------------------------------------------
Parts 1 and 2 of Homework 6 require implementation of [Eigendecomposition](http://en.wikipedia.org/wiki/Eigendecomposition) and [SVD](http://en.wikipedia.org/wiki/Singular_Value_Decomposition).

My implementation of eigendecomposition uses the Power Method ([power iteration](http://en.wikipedia.org/wiki/Power_iteration) and [deflation](http://www.miislita.com/information-retrieval-tutorial/matrix-tutorial-3-eigenvalues-eigenvectors.html)) to compute `A = V \Lambda V^T` for a real-valued matrix `A`. See `PowerMethod.hs` for a detailed comments.

My SVD implementation also relies on eigendecomposition to compute `A = U S V^T`. See `SVD.hs` for detailed comments.

### Usage

	$ ./main -h
	main
		-d INT     --decimal-places=INT  Number of decimal places to display
		-e DOUBLE  --epsilon=DOUBLE      Precision parameter
		-P         --power-method        Compute the eigen-vectors and values of the 2D matrix
		-S         --svd                 Compute the singular value decomposition of the 2D matrix
		-R         --recombine           Compute the Frobenius norm of the difference between the 2D matrix and its reconstructions
		-T         --test-truncation     Compute the Frobenius norms of the differences between the 2D matrix and its reconstructions from truncated singular values
		-v         --verbose             Enable verbose messages
		-h         --help                Show help

Executing `./main -v -P -S -R -T path/to/A.txt`, where `A.txt` is a 2D ASCII representation of the real-valued matrix `A`, will generate a report demonstrating

1. Eigenvector decomposition of the input matrix, `A`, including (enabled with `-P`)
	* Reconstruction of `A'` from `V \Lambda V^T` (enabled with `-R`)
	* The Frobenius norm of the difference between the input matrix and its reconstruction (enabled with `-R`)
2. Singular value decomposition of the input matrix, `A`, including (enabled with `-S`)
	* Reconstruction of `A'` from `U S V^T` (enabled with `-R`)
	* The Frobenius norm of the difference between the input matrix and its reconstruction (enabled with `-R`)
3. The Frobenius norm of a 10 by 20 random matrix minus its recomposition, `U S_i V^T`--where `S_i` is a diagonal matrix with all but the first `i` singular values truncated (enabled with `-T`)
