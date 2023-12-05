program main
    implicit none
    real, dimension(2, 4) :: A
    real, dimension(2) :: w
    real, dimension(4) :: c, r

    A = reshape([1.0, 0.0, -1.0, 1.0, 1.0, 0.0, 0.0, 1.0], [2, 4]) 
    c = [-2.0, 1.0, 0.0, 0.0] 
    w = [-1.33353, -0.00771]
    
    call computeVectorOfReducedCosts(A, c, w, r)

    contains 
        subroutine computeVectorOfReducedCosts(A, c, w,r)
            implicit none
            real, dimension(:,:), intent(in) :: A
            real, dimension(:), intent(in) :: c
            real, dimension(:), intent(in) :: w
            real, dimension(size(TRANSPOSE(A), 1)), intent(out) :: r
    
            r = c - MATMUL(TRANSPOSE(A), w)
            !TRANPOSE A 4 x 2
            !w 2 x 1
            !c 4 x 1 
            write(*, *) 'Result vector r:'
            write(*, *) r
        end subroutine computeVectorOfReducedCosts
end program main
