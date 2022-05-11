!>非圧縮性流れの計算を行う際の安定条件に関係する手続を定義する．
!>
!>手続には，移流と拡散の安定条件に基づいて時間刻みを計算する手続が含まれる．
!>
module incompressible_condition_stability
    use, intrinsic :: iso_fortran_env
    use :: flop
    implicit none
    private
    public :: stabilize

    interface stabilize
        procedure :: stabilize_time_interval
    end interface

contains
    !>安定条件を満たす計算時間間隔を返す．
    !>安定条件として，
    !>
    !>- 移流方程式に対する安定条件 \(u\frac{\varDelta t}{\varDelta x}<1\)
    !>- 拡散方程式に対する安定条件 \(\nu\frac{\varDelta t}{\varDelta x^2}<0.5\)
    !>
    !>を考慮する．
    function stabilize_time_interval(dt, grid, &
                                     velocity, kinetic_viscosity, &
                                     Courant, Diffusion) result(time_interval)
        implicit none
        !&<
        real(real64)                        , intent(in)            :: dt
            !! 安定化したい計算時間間隔
        type(staggered_uniform_grid_2d_type), intent(in)            :: grid
            !! 空間離散化情報
        real(real64)                        , intent(in)            :: velocity
            !! 代表速度
        real(real64)                        , intent(in)            :: kinetic_viscosity
            !! 作動流体の動粘度
        real(real64)                        , intent(in), optional  :: Courant
            !! クーラン数<br>
            !! これが渡されない場合は，移流に関する安定条件を考慮しない．
        real(real64)                        , intent(in), optional  :: Diffusion
            !! 拡散数
            !! これが渡されない場合は，拡散に関する安定条件を考慮しない．
        !&>
        real(real64) :: time_interval

        time_interval = dt

        ! 移流方程式の安定条件
        if (present(Courant)) &
            time_interval = min(time_interval, &
                                Courant*minval(grid%get_interval())/velocity)

        ! 拡散方程式の安定条件
        if (present(Diffusion)) &
            time_interval = min(time_interval, &
                                Diffusion*minval(grid%get_interval()**2)/kinetic_viscosity)

    end function stabilize_time_interval
end module incompressible_condition_stability

program cavity_flow
    use, intrinsic :: iso_fortran_env
    use :: fluid_knownFluids, only:Water
    use :: flop
    use :: incompressible_condition_stability
    implicit none

    type(Cartesian_2d_type) :: space !! 計算領域
    type(staggered_uniform_grid_2d_type), target :: grid !! 空間格子
    type(time_axis_type) :: t !! 計算時間
    type(discrete_time_type) :: discrete_time !! 時間積分の設定

    real(real64) :: Re !! レイノルズ数
    real(real64) :: kvisc !! 動粘度
    real(real64) :: dens !! 密度
    real(real64) :: U_wall !! 移動壁の速度
    real(real64) :: l !! キャビティの1辺の長さ
    real(real64) :: dt !! 計算時間間隔

    Re = 1000d0
    kvisc = Water%kinetic_viscosity
    dens = Water%density
    U_wall = 0.01d0
    l = Re*kvisc/U_wall

    block
        type(axis_type) :: x, y
        x = x.set. [0d0, l]
        y = y.set. [0d0, l]
        space = space.set.Cartesian([x, y])
    end block
    grid = .divide.space.into.cells([40, 40])

    t = t.set. [0d0, 50d0*l/U_wall] !壁がキャビティを50回通過する時間
    dt = 0.25d0
    dt = stabilize(dt, grid, U_wall, kvisc, Courant=0.1d0)
    discrete_time = .divide.t.into.intervals(dt)

    block
        type(vector_2d_type) :: u !! 速度
        type(scalar_2d_type) :: p !! 圧力
        type(vector_2d_type) :: u_aux !! 中間速度

        type(vector_boundary_condition_type) :: BC_u !! 速度境界条件
        type(scalar_boundary_condition_type) :: BC_p !! 圧力境界条件

        integer(int32) :: n, Nt !! 時間積分回数
        real(real64) :: below_criterion = 1d-9 !! 収束判定条件

        Nt = discrete_time%get_number_of_integration()
        dt = discrete_time%get_time_interval()

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

        do n = 1, 10 !Nt
            print *, n

            !Fractional Step法で時間積分を実行

            u_aux = (u + dt*(-(.div.(u.times.u)) + kvisc*.laplacian.u)) &
                    .impose. BC_u !&
            ! u_aux = (u + dt*(-(.l.(u.dot.nabla).r.u) + kvisc*.laplacian.u)) &
            !         .impose. BC_u !&

            p = .inverse.(( &
                  (laplacian(p).with.BC_p) .results. (dens/dt*.div.u_aux)) &
                  .until. below_criterion &
                ) !&

            u = (u_aux - dt/dens*.grad.p) .impose. BC_u !&

        end do

        call output((p.as.csv) .to.unit("p.txt"))
        call output((u.as.csv) .to.unit("u.txt"))

        close (unit("p.txt"))
        close (unit("u.txt"))
    end block
end program cavity_flow
