program bc
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_operators
    implicit none

    type(vector_value_on_boundary_type) :: val_on_bnd
    type(Dirichlet_vector_value_type) :: val

    type(vector_boundary_condition_type) :: BC_u

    val%value(:) = [1d0, 2d0]
    val_on_bnd = val.on.B1

    print *, val_on_bnd%vector_value
    print *, val_on_bnd%boundary_index

    val_on_bnd = Dirichlet([0d0, 1d0]) .on.B4
    print *, val_on_bnd%vector_value
    print *, val_on_bnd%boundary_index

    print *, BC_u

    BC_u = BC_u .set. (Dirichlet([0d0, 0d0]) .on. B1) !&
    print *, BC_u

    BC_u = BC_u .set. (Dirichlet([0d0, 0d0]) .on. B2) !&
    print *, BC_u

    BC_u = BC_u .set. (Dirichlet([0d0, 0d0]) .on. B3) !&
    print *, BC_u

    BC_u = BC_u .set. (Dirichlet([1d0, 0d0]) .on. B4) !&
    print *, BC_u
    BC_u = BC_u .set. (Dirichlet([0d0, 0d0]) .on. B1) &
                .set. (Dirichlet([0d0, 0d0]) .on. B2) &
                .set. (Dirichlet([0d0, 0d0]) .on. B3) &
                .set. (Dirichlet([1d0, 0d0]) .on. B4) !&

    block
        use :: space_Cartesian
        use :: grid_uniform_staggered_2d
        use :: grid_uniform_staggered_vars_vector_2d
        type(Cartesian_2d_type) :: space
        type(staggered_uniform_grid_2d_type), target :: grid

        type(vector_2d_type) :: u

        call space%construct(x_coord_val=[0d0, 1d0], &
                             y_coord_val=[0d0, 1d0])
        call grid%construct(space, number_of_grid_points=[41, 41])

        call u%init_on(grid)
        u%x = 0d0
        u%y = 0d0
        u = u.impose.BC_u
        call u%output("u.txt")
    end block
end program bc
