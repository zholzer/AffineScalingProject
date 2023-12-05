program affineScaling
    implicit none
    ! Setup: Take input A, b, c, step size, tolerance ε, and initial guess x0>0
    real, dimension(:,:), allocatable :: A, b, D, oneVec, c, xk, dk, r, vk, w 
    real :: stepSize, tolerance, start, finish
    real, dimension(1,1) :: check
    integer :: i,j, itNum, stat
    logical :: posdk, zerodk, posR
    character :: testChoice

    ! using 7.1 from https://www.ise.ncsu.edu/fuzzy-neural/wp-content/uploads/sites/9/2019/10/chapter7.pdf
    write(*,*) ''
    write(*,*) 'Welcome to the Affine Scaling Program.'
    write(*,*) ''
    write(*,*) 'Case "A" solves the problem: '
    write(*,*) 'Minimize -2x_1 + x_2'
    write(*,*) 'Subject to conditions x_1 - x_2 <= 15, x_2 <= 15, and {x_1, x_2} >= 0'

    write(*,*) ''
    write(*,*) 'Case "B" solves the problem: '
    write(*,*) 'Minimize -x_1 - x_2 + x_3 + x_4'
    write(*,*) 'Subject to conditions x_1 + x_3 = 1, x_2 + x_4 = 2, and {x_1, x_2, x_3, x_4} >= 0'

    write(*,*) ''
    write(*,*) 'Case "C" solves the problem: '
    write(*,*) 'Minimize x_1 - 2x_2'
    write(*,*) 'Subject to conditions x_1 + x_2 <= 40, 2x_1 + x_2 <= 60, and {x_1, x_2} >= 0'

    write(*,*) ''
    write(*,*) 'Case "D" solves the problem: '
    write(*,*) 'Minimize -x_1 - x_2'
    write(*,*) 'Subject to conditions x_1 + 2x_2 <= 3, 2x_1 + x_2 <= 3, and {x_1, x_2} >= 0'

    write(*,*) ''
    write(*,*) 'Case "E" solves the problem: '
    write(*,*) 'Minimize '
    write(*,*) 'Subject to conditions x_1 + 2x_2 <= 3, 2x_1 + x_2 <= 3, and {x_1, x_2} >= 0'

    write(*,'(a)', advance = 'no') 'Please enter a test case as "A", "B", "C", "D", or "E":     '
    read(*,*, iostat=stat) testChoice

 1   select case (testChoice) ! switches with user input
        case ('A')
            allocate(A(2,4)) ! allocate space depending on choice
            allocate(b(1,2))
            allocate(c(4,1))
            allocate(xk(4,1))
            A = reshape([1.0, 0.0, -1.0, 1.0, 1.0, 0.0, 0.0, 1.0], [2, 4])  ! fill in problem vectors
            b = reshape([15.0, 15.0], [1,2])
            c = reshape([-2.0, 1.0, 0.0, 0.0],[4,1])
            xk = reshape([10.0, 2.0, 7.0, 13.0],[4,1])
            ! answer is (30 15 0 0)
            ! reference https://www.ise.ncsu.edu/fuzzy-neural/wp-content/uploads/sites/9/2019/10/chapter7.pdf

        case ('B')
            allocate(A(2,4))
            allocate(b(1,2))
            allocate(c(4,1))
            allocate(xk(4,1))
            A = reshape([1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 1.0], [2, 4]) 
            b = reshape([1.0, 2.0], [1,2])
            c = reshape([-1.0, -1.0, 1.0, 1.0],[4,1])
            xk = reshape([0.9, 1.9, 0.1, 0.1],[4,1])
            ! answer is (1 2 0 0)
        ! reference https://homepages.rpi.edu/~mitchj/handouts/interior_html/interior.html
        
        case ('C')
            allocate(A(2,4))
            allocate(b(1,2))
            allocate(c(4,1))
            allocate(xk(4,1))
            A = reshape([1.0, 1.0, 1.0, 0.0, 2.0, 1.0, 0.0, 1.0], [2, 4]) 
            b = reshape([40.0, 60.0], [1,2])
            c = reshape([1.0, -2.0, 0.0, 0.0],[4,1])
            xk = reshape([8.0, 30.0, 2.0, 14.0],[4,1])
            ! Answer is (0 40 0 20) 
        ! reference https://www.ise.ncsu.edu/fuzzy-neural/wp-content/uploads/sites/9/2021/10/Lecture-6.pdf

        case ('D')
            allocate(A(2,4))
            allocate(b(1,2))
            allocate(c(4,1))
            allocate(xk(4,1))
            A = reshape([1.0, 2.0, 1.0, 0.0, 2.0, 1.0, 0.0, 1.0], [2, 4]) 
            b = reshape([3.0, 3.0], [2,1])
            c = reshape([-1.0, -1.0, 0.0, 0.0],[4,1])
            xk = reshape([0.9, 0.9, 0.3, 0.3],[4,1])
                 ! (1 1 ? ?), beta = .5
        ! reference https://juliabook.chkwon.net/book/interior

            case('E') ! trivial case
            allocate(A(10,20))
            allocate(b(1,10))
            allocate(c(20,1))
            allocate(xk(20,1))
            A = reshape([1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, & 
                0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
                0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
                0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
                0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
                0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, &
                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, &
                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, &
                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, &
                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0], &    
                [10, 20]) 
                b = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0], [1,10])
                c = reshape([1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, -1.0, 0.0, &
                            0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],[20,1])
                xk = reshape([.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, &
                              .5, .5, .5, .5, .5, .5, .5, .5, .5, .5],[20,1])

        case default ! if input is not valid, retry
            write(*,*) 'Please enter a valid test case selection.'
            write(*,'(a)', advance = 'no') 'Please enter a test case as "A", "B", "C", "D", or "E":     '
            read(*,*, iostat=stat) testChoice
            go to 1 ! do not pass go! go back and try the switch case again

    end select
    
    write(*,'(a)', advance = 'no') 'Please enter a step size between 0 and 1:   '
    read(*,*, iostat=stat) stepSize
    
    do while ((stat .ne. 0) .or. (stepSize <= 0) .or. (stepSize >= 1))      
        write(*,*) 'Please input a number from 0 to 1 exclusive. Retry.'
        write(*,'(a)', advance = 'no') 'Please enter a step size between 0 and 1:   '
        read(*,*, iostat=stat) stepSize
    end do

    tolerance = 10.0**(-6) ! pick stopping condition

    allocate(D(size(xk),size(xk))) ! allocate other stuff based on test case
    allocate(dk(size(xk),1))
    allocate(r(size(xk),1))
    allocate(vk(size(xk),1))
    allocate(w(size(A, dim=1),1))
    allocate(oneVec(1,size(A, dim=2)))

    call cpu_time(start) ! start cpu timer
    itNum = 1000 ! can change this to allow for more iterations
    do i = 1,itNum 
        
        ! Step 1: Start an an interior feasible point
        D = diagonalMatrix(xk,size(xk)) 
        
        ! Step 2: Transform to new space using affine-scaling
        ! 2a. Compute vector of dual estimates
        call computeVectorOfDualEstimates(A, D, c, w)
       
        ! 2b. Compute vector of reduced costs
        call computeVectorOfReducedCosts(A, c, w, r)
        
        ! 2c. Check optimality
        posR = .true.
        do j = 1,size(r)
            if (r(j,1) > 0) then ! if all element is positive, r>0
                continue
            else ! if one element is negative, r is not >0
                posR = .false.
            end if
        end do

        check = MATMUL(oneVec,MATMUL(D,r)) ! tolerence check  

        if ((posR .and. (check(1,1)) < tolerance)) then ! if true, primal optimal value has been found
            write(*, *) 'Primal optimal value for xk: '
            call displayMatrix(xk, size(xk), 1)
            call cpu_time(finish) ! end timer
            write(*,*) 'Code took ', finish - start, 'seconds.'
            write(*,'(a)', advance = 'no') 'Ended with '
            write(*,'(i3)', advance = 'no') i
            write(*,'(a)') ' iterations.'
            stop
        end if

        ! Step 3: Compute steepest-descent direction
        call computeSteepestDescentDirection(D, r, dk)
    
        ! Step 4: Check for unbounded and constant objective value
        posdk = .true.;
        do j = 1,size(dk)
            if (dk(j,1) < 0) then
                continue
            else
                posdk = .false.
            end if
        end do

        zerodk = .true.;
        do j = 1,size(dk)
            if (dk(j,1) <= tolerance) then
                continue
            else
                zerodk = .false.
            end if
        end do

        if (posdk) then ! if direction is positive, the problem is unbounded
            write(*,*)"This problem is unbounded"
            stop
        else if (zerodk) then ! if direction is close to 0, primal optimal value has been found
            write(*,*) "Primal Optimal value xk: "
            call displayMatrix(xk, size(xk), 1)
            call cpu_time(finish)
            write(*,*) 'Code took ', finish - start, 'seconds.'
            write(*,'(a)', advance = 'no') 'Ended with '
            write(*,'(i3)', advance = 'no') i
            write(*,'(a)') ' iterations.'
            stop
        end if
        ! ^ if the above conditions are unsatisfied, go to the next step

        ! Step 5: Perform the translation
        call computeTranslation(xk, stepSize, dk, D)

        if (i == itNum) then ! if maximum iterations are reached, print xk
            write(*,*) "xk is: " 
            call displayMatrix(xk, size(xk), 1)
            call cpu_time(finish)
            write(*,*) 'Code took ', finish - start, 'seconds.'
            write(*,'(a)') 'Ended with 100 iterations.'
            stop
        end if
    end do 

    deallocate(A) ! deallocate all vectors
    deallocate(b)
    deallocate(c)
    deallocate(xk)
    deallocate(oneVec)
    deallocate(D)
    deallocate(dk)
    deallocate(r)
    deallocate(w)

    contains
        subroutine computeVectorOfDualEstimates(A,D,c,w)
            implicit none
            real, dimension(:,:), intent(in) :: A
            real, dimension(:,:), intent(in) :: D
            real, dimension(:,:), intent(in) :: c
            real, dimension(size(A, dim=1),size(D, dim=2)) :: H
            real, dimension(size(A, dim=1),size(A, dim=1)) :: F
            real, dimension(size(A, dim=1),1) :: bInv
            real, dimension(size(A, dim=1),1), intent(out) :: w
            ! computes (AD(AD)^T)w = AD^2c for w 
            H = MATMUL(A, D) !H = A*D
            F = MATMUL(H, TRANSPOSE(H)) !F = (AD*(AD)^T)
            bInv = MATMUL(H, MATMUL(D, c)) !bInv = A*(D^2)*c
            w = conjugateGradient(F,bInv) ! uses the conjugate gradient numerical system solver F x = bInv
        end subroutine computeVectorOfDualEstimates

        ! computes the conjugate gradient https://en.wikipedia.org/wiki/Conjugate_gradient_method
        function conjugateGradient(F, b) result(wFinal) 
            implicit none
            real, intent(in) :: F(:,:)
            real, intent(in) :: b(:,:)
            real, dimension(size(b)) :: w, r , p, Ap
            real(4) :: tol, max_iter, alpha, beta
            real, dimension(size(w),1) :: wFinal
            integer :: i
            tol = 1.0E-6
            max_iter = 100
            w = 0.0
            !initialize vairables
            r = reshape(b, [size(b)]) - MATMUL(F, w) !residual r = b - F*w
            p = r !inital search direction is the same as residual vector
            i = 0 ! i-th step 
            !keep looping until converage or reach the maximum of iterations
            do while (i < max_iter .and. maxval(abs(r)) > tol)
                Ap = MATMUL(F, p)
                alpha = dot_product(r, r) / dot_product(p, Ap)
                w = w + alpha * p !update w
                r = r - alpha * Ap !update residual vector r
                beta = dot_product(r, r) / dot_product(p, Ap)
                p = r + beta * p !compute search direction
                i = i + 1
            end do
            wFinal = reshape(w, [size(w),1])
        end function conjugateGradient

        subroutine computeVectorOfReducedCosts(A, c, w, r)
            implicit none
            real, dimension(:,:), intent(in) :: A
            real, dimension(:,:), intent(in) :: c
            real, dimension(:,:), intent(in) :: w
            real, dimension(size(A), 1), intent(out) :: r
            ! computes r = c - A^Tw
            r = c - MATMUL(transpose(A), w)
        end subroutine computeVectorOfReducedCosts

        subroutine computeSteepestDescentDirection(D, r, dk)
            implicit none
            real, dimension(:,:), intent(in) :: D
            real, dimension(:,:), intent(in) :: r
            real, dimension(:,:), intent(out) :: dk
            ! computes -Dr
            dk = MATMUL(-1*D, r)
        end subroutine computeSteepestDescentDirection

        subroutine computeTranslation(xk, stepSize, dk, D)
            implicit none
            real, dimension(:,:), intent(inout) :: xk, dk
            real, intent(in) :: stepSize
            real :: mu
            real, dimension(:,:), intent(in) :: D
            ! computes steplength mu = stepsize/max(-dk)
            ! formula from source https://homepages.rpi.edu/~mitchj/handouts/interior_html/interior.html
            mu = stepSize/maxval(-dk)
            ! translate xk with xk+1 = xk + muDdk
            xk = xk + mu*MATMUL(D,dk)
        end subroutine computeTranslation

        ! Helper Function
        ! creates a matrix D with elements of xk on the diagonal
        function diagonalMatrix(v,n) result(D)
            implicit none
            integer :: n
            real, intent(in) :: v(:,:)
            real :: D(n,n)
            integer :: i
            D = 0.0
            do i = 1, n
                D(i,i) = v(i,1)
            end do
        end function
        ! displays a matrix
        subroutine displayMatrix(D, m, n)
            implicit none
            integer, intent(in) :: n, m
            integer :: i, j  
            real, dimension(:,:) :: D
            do i = 1, m 
                write(*, '(F8.2," ")') (D(i,j), j = 1, n) 
            end do
        end subroutine displayMatrix
end program affineScaling
