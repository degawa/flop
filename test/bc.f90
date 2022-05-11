program bc
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_operators
    implicit none

    type(scalar_gradient_on_boundary_type) :: grad_on_bnd
    type(Neumann_scalar_gradient_type) :: grad
    type(scalar_boundary_condition_type) :: BC_p

    grad%gradient = 0d0
    print *, grad%gradient

    grad_on_bnd = grad.on.B1
    print *, grad_on_bnd%scalar_gradient
    print *, grad_on_bnd%boundary_index

    grad_on_bnd = Neumann(1d0) .on.B3
    print *, grad_on_bnd%scalar_gradient
    print *, grad_on_bnd%boundary_index

    BC_p = BC_p.set. (Neumann(4d0) .on.B1)
    BC_p = BC_p.set. (Neumann(3d0) .on.B2)
    BC_p = BC_p.set. (Neumann(2d0) .on.B3)
    BC_p = BC_p.set. (Neumann(1d0) .on.B4)
    print *, BC_p

    block
        use :: space_Cartesian
        use :: grid_uniform_staggered_2d
        use :: grid_uniform_staggered_vars_scalar_2d
        type(Cartesian_2d_type) :: space
        type(staggered_uniform_grid_2d_type), target :: grid

        type(scalar_2d_type) :: p, rhs
        type(Ax_laplacian_type) :: Ap
        type(Ax_eq_b_type) :: eq

        call space%construct(x_coord_val=[0d0, 1d0], &
                             y_coord_val=[0d0, 1d0])
        call grid%construct(space, number_of_grid_points=[11, 11])

        p = .init. (p.on.grid)
        p%val = 1d0

        rhs = .init. (rhs.on.grid)
        rhs%val = 2d0

        BC_p = BC_p .set. (Neumann(4d0) .on.B1) &
                    .set. (Neumann(3d0) .on.B2) &
                    .set. (Neumann(2d0) .on.B3) &
                    .set. (Neumann(1d0) .on.B4) !&

        Ap = laplacian(p)
        print *, Ap%x%val
        Ap = Ap.with.BC_p
        print *, Ap%BC

        eq = Ap.results.rhs
        print *, eq%b%val

        eq = eq.until.1d-9
        print *, eq%Ax%x%val
        print *, eq%Ax%err_tol
    end block
    block
        use :: space_Cartesian
        use :: grid_uniform_staggered_2d
        use :: grid_uniform_staggered_vars_scalar_2d
        type(Cartesian_2d_type) :: space
        type(staggered_uniform_grid_2d_type), target :: grid

        type(scalar_2d_type) :: p, rhs

        call space%construct(x_coord_val=[0d0, 2d0*acos(-1d0)], &
                             y_coord_val=[0d0, 2d0*acos(-1d0)])
        call grid%construct(space, number_of_grid_points=[21, 21])

        p = .init. (p.on.grid)
        rhs = .init. (rhs.on.grid)
        p%val(:, :) = 0d0
        block
            integer(int32) :: ic, jc
            do jc = 0, 21
            do ic = 0, 21
                rhs%val(ic, jc) = -2d0*sin(grid%xc(ic))*sin(grid%yc(jc))
            end do
            end do
        end block
        BC_p = BC_p .set. (Dirichlet(0d0) .on.B1) &
                    .set. (Dirichlet(0d0) .on.B2) &
                    .set. (Dirichlet(0d0) .on.B3) &
                    .set. (Dirichlet(0d0) .on.B4) !&

        p = .inverse. (((laplacian(p.impose.BC_p) .with.BC_p) .results.rhs) .until.1d-4)
        call p%output("p.txt")
    end block
end program bc
