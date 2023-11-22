program affineScaling
    implicit none
    ! Setup: Take input A, b, c, step size, tolerance ε, and initial guess x0>0
    real, dimension(:,:), allocatable :: A ! switch cases?
    real, dimension(:), allocatable :: b
    real, dimension(:) allocatable, :: c
    real, dimension(:), allocatable :: x0
    real :: stepSize, tolerance ! user input?
    integer :: i

    ! to start we can test with hard coding, maybe one from a known reference

    xk = x0
    do i = 1,100 ! can change this later
        ! Step 1: Start an an interior feasible point
        vk = b - A*xk
        D = diag(vk) ! probably need to code this too
        ! Step 2: Transform to new space using affine-scaling
        ! 2a. Compute vector of dual estimates
        w = computeVectorOfDualEstimates(A,D,c)
        ! 2b. Compute vector of reduced costs
        r = computeVectorOfReducedCosts(c, A, w)
        ! 2c. Step 4: Check optimality
        if (conditions) then 
            write(xk, wk) 
            stop
        end if

        ! Step 3: Compute steepest-descent direction
        d = computeSteepestDescentDirection(D, r)

        ! Step 4: Check for unbounded and constant objective value
        if (condition) then 
            write(unbounded)
            stop
        else if (condition) then 
            write(xk)
        end if

        ! Step 5: Perform the translation
        xk = computeTranslation(xk, stepSize, D, r)
        if (i == 100) then
            write(to many iterations)
            stop
        end if
    end do

    contains
        function diag(xk)
            implicit none
            ! puts elements of vector xk in diagonal of n by n matrix, rest 0's
        end function

        function computeVectorOfDualEstimates(A,D,c)
            implicit none
            ! compute this
            ! will need inverse function
            stuffInverse = inverse(A, D)
        end function

        function computeVectorOfReducedCosts(c, A, w)
            implicit none
            ! compute this
        end function

        function computeSteepestDescentDirection(D, r)
            implicit none
            ! compute this
        end function

        function computeTranslation(xk, stepSize, D, r)
            implicit none
            ! compute this
        end function

        function inverse(A,D)
            ! compute this probably with LU factorization
        end function
end program affineScaling

! use slides and reference https://www.ise.ncsu.edu/fuzzy-neural/wp-content/uploads/sites/9/2019/10/chapter7.pdf
