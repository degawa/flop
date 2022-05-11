program main
    use, intrinsic :: iso_fortran_env
    use :: space_Cartesian
    use :: grid_uniform_staggered_2d
    use :: grid_uniform_staggered_variables
    use :: grid_uniform_staggered_operators
    implicit none

    type(Cartesian_2d_type) :: space
    type(staggered_uniform_grid_2d_type), target :: grid

    call space%construct(x_coord_val=[0d0, 2d0*acos(-1d0)], &
                         y_coord_val=[0d0, 2d0*acos(-1d0)])
    call grid%construct(space, number_of_grid_points=[33, 33])
    print *, grid%x
    print *, grid%y
    print *, grid%xc
    print *, grid%yc
    block
        type(scalar_2d_type) :: p, lap
        type(Ax_laplacian_type) :: lap_p
        type(Ax_eq_b_type) :: poisson
        integer(int32) :: i, j, sr(4)
        call p%construct(grid)
        sr = grid%get_scalar_range()
        do j = sr(y_min_index), sr(y_max_index)
        do i = sr(x_min_index), sr(x_max_index)
            p%val(i, j) = -cos(grid%xc(i))*cos(grid%yc(j))
        end do
        end do
        call p%output("pot.txt")

        lap_p = laplacian(p)
        print *, lap_p%x%val
        lap = lap_p%eval()
        call lap%output("lap.txt")

        lap_p%x%val = 0d0
        poisson = lap_p.results.lap
        call poisson%b%output("rhs.txt")
        call poisson%Ax%x%output("unknown.txt")
        p = .inverse.poisson
        call p%output("p.txt")
    end block
end program main
