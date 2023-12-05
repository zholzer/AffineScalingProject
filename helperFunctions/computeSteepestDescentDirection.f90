program main
    implicit none
    real, dimension(2,4) :: D
    real, dimension(4) :: r
    real, dimension(2,1) :: d ! is this the correct dimension?

    D = reshape([1.0, 0.0, -1.0, 1.0, 1.0, 0.0, 0.0, 1.0],[2,4])
    r = [-2.0, -1.0, 2.5, 0.5]

    call computeSteepestDescentDirection(D,r)

    contains 
    function computeSteepestDescentDirection(D, r, d)
        implicit none
        real, dimension(:,:), intent(in) :: D
        real, dimension(:), intent(in) :: r
        real, dimension(:), intent(out) :: d
        
        d = -D * r

        write(*,*)"Steepest-descent direction: ", d
    end function
end program main
