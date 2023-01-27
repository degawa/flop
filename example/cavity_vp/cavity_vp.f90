program cavity_flow
    use, intrinsic :: iso_fortran_env
    use :: fluid_knownFluids, only:Water
    use :: flop
    implicit none

    type(Cartesian_2d_type) :: space !! 計算領域
    type(staggered_uniform_grid_2d_type), target :: grid !! 空間格子
    type(time_axis_type) :: t !! 計算時間
    type(discrete_time_type) :: delta_t !! 時間積分の設定

    real(real64) :: kvisc !! 動粘度
    real(real64) :: dens !! 密度
    real(real64) :: relct !! 抵抗率
    real(real64) :: U_wall !! 移動壁の速度
    real(real64) :: dt !! 計算時間間隔

    type(characteristics_type) :: characteristics

    U_wall = 0.01d0

    !&<
    characteristics = characteristics .set. Reynolds_number(1000d0) &
                                      .set. kinetic_viscosity(Water%kinetic_viscosity) &
                                      .set. characteristic_velocity(U_wall)
    !&>

    dens = Water%density
    kvisc = characteristics.value.of_kinetic_viscosity !Water%kinetic_viscosity

    block
        type(axis_type) :: x, y
        real(real64) :: l !! キャビティの1辺の長さ
        type(stability_conditions_type) :: stability_conditions

        l = characteristics.value.of_length !Re*kvisc/U_wall
        x = x.set. [0d0, l]
        y = y.set. [0d0, l]
        space = space.set.Cartesian([x, y])
        grid = .divide. (space.into.cells([50, 50]))

        t = t.set. [0d0, 100d0*l/U_wall] !壁がキャビティを50回通過する時間
        dt = 0.25d0

        stability_conditions = stability_conditions &
                               .set. Courant(grid.value.of_minimum_interval, U_wall, 0.1d0) &
                               .set. Diffusion(grid.value.of_minimum_interval, kvisc, 0.5d0) !&
        !    .set. Penalization(dt, 1d0/relct, 1d0) !&
        dt = .stabilize.(dt .by. stability_conditions) !&

        delta_t = .divide. (t.into.intervals(dt))
        relct = .stabilize.Reluctivity(delta_t.value.of_time_interval)
    end block

    block
        type(vector_2d_type) :: u !! 速度
        type(scalar_2d_type) :: p !! 圧力
        type(vector_2d_type) :: u_aux !! 中間速度
        type(scalar_2d_type) :: m
        type(vector_2d_type) :: u_s

        type(vector_boundary_condition_type) :: BC_u !! 速度境界条件
        type(scalar_boundary_condition_type) :: BC_p !! 圧力境界条件

        integer(int32) :: n, Nt !! 時間積分回数
        real(real64) :: below_criterion = 1d-9 !! 収束判定条件

        Nt = delta_t%get_number_of_integration()
        dt = delta_t%get_time_interval()

        ! initialize
        u = .init. (u.on.grid)
        p = .init. (p.on.grid)

        ! set boundary condition
        BC_u = BC_u .set. (Dirichlet([   0d0, 0d0]) .on. B1) &
                    .set. (Dirichlet([   0d0, 0d0]) .on. B2) &
                    .set. (Dirichlet([   0d0, 0d0]) .on. B3) &
                    .set. (Dirichlet([U_wall, 0d0]) .on. B4) !&
        BC_p = BC_p .set. (Neumann(0d0) .on. B1) &
                    .set. (Neumann(0d0) .on. B2) &
                    .set. (Neumann(0d0) .on. B3) &
                    .set. (Neumann(0d0) .on. B4) !&

        u = u .impose. BC_u !&

        u_s = .init. (u_s.on.grid)
        m = .init. (m.on.grid)
        m = input(m .in. npy .from. "mask") !&

        do n = 1, Nt
            print *, n, Nt

            !Fractional Step法で時間積分を実行

            u_aux = (u + dt*(-(.div.(u.times.u)) &
                             + kvisc*.laplacian.u &
                             + relct*(m.times.(u_s-u)) &
                            ) &
                    ) .impose. BC_u !&
            ! u_aux = (u + dt*(-((u.dot.nabla)*u) &
            !                  + kvisc*.laplacian.u &
            !                  + relct*(m*(u_s-u)) &
            !                 ) &
            !         ) .impose. BC_u !&

            p = .inverse.( &
                  ((laplacian(p).with.BC_p) == (dens/dt*.div.u_aux)) &
                  .using. CG() & !RBSOR(1.9d0) & ! SOR(1.9d0) is also available
                  .until. below_criterion &
                ) !&

            u = (u_aux - dt/dens*.grad.p) &
                .impose. BC_u !&
        end do

        call output(p .as. csv .to. "p") !&
        call output(u .as. csv .to. "u") !&
    end block
end program cavity_flow
