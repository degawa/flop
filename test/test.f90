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
        type(scalar_2d_type) :: pot
        type(vector_2d_type) :: velo
        integer(int32) :: i, j, sr(4)
        call pot%construct(grid)
        sr = grid%get_scalar_range()
        do j = sr(y_min_index), sr(y_max_index)
        do i = sr(x_min_index), sr(x_max_index)
            pot%val(i, j) = -cos(grid%xc(i))*cos(grid%yc(j))
        end do
        end do
        call pot%output("pot.txt")

        velo = .grad.pot
        call velo%output("velo_grad.txt")
    end block
    block
        type(scalar_2d_type) :: dive
        type(vector_2d_type) :: velo, lap, conv, mvelo
        integer(int32) :: i, j, vr(2, 4)

        call velo%construct(grid)
        vr = grid%get_vector_range()

        do j = vr(x_dir_index, y_min_index), vr(x_dir_index, y_max_index)
        do i = vr(x_dir_index, x_min_index), vr(x_dir_index, x_max_index)
            velo%x(i, j) = sin(grid%x(i))*cos(grid%yc(j))
        end do
        end do
        do j = vr(y_dir_index, y_min_index), vr(y_dir_index, y_max_index)
        do i = vr(y_dir_index, x_min_index), vr(y_dir_index, x_max_index)
            velo%y(i, j) = -cos(grid%xc(i))*sin(grid%y(j))
        end do
        end do
        call velo%output("velo.txt")

        mvelo = -velo
        call mvelo%output("mvelo.txt")

        mvelo = 0.1d0*velo
        call mvelo%output("fvelo.txt")

        dive = .div.velo
        call dive%output("div.txt")

        lap = .laplacian.velo
        call lap%output("lap.txt")

        conv = .div. (velo.times.velo)
        call conv%output("conv_c.txt")

        conv = .l.(velo.dot.nabla).r.velo !&
        call conv%output("conv.txt")
    end block
end program main
