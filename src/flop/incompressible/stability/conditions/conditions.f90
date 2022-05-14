!>非圧縮性流れの数値計算の安定化に関係する派生型を提供する．
!>
!>派生型には，複数の安定条件をまとめて取り扱う派生型が含まれる．
!>
module incompressible_stability_conditions
    use :: incompressible_op_var_stabilizer_time
    use :: incompressible_op_var_stabilizer_time_advection
    use :: incompressible_op_var_stabilizer_time_diffusion
    implicit none
    private

    !>複数の安定条件をまとめて取り扱う派生型．
    type, public :: stability_conditions_type
        type(advection_stabilizer_type), allocatable :: Advection_stabilizer
            !! 移流に関する安定条件
        type(diffusion_stabilizer_type), allocatable :: Diffusion_stabilizer
            !! 拡散に関する安定条件
    contains
        procedure, public, pass :: stabilize
        !* 安定化した計算時間間隔を返却
    end type stability_conditions_type

contains
    !>型で考慮される全ての安定条件を利用して安定化した計算時間間隔を返す．
    function stabilize(this, time_interval) result(stabilized_time_interval)
        use, intrinsic :: iso_fortran_env
        implicit none
        !&<
        class(stability_conditions_type), intent(in) :: this
            !! 当該実体仮引数
        real(real64)                    , intent(in) :: time_interval
            !! 安定化したい計算時間間隔
        !&>
        real(real64) :: stabilized_time_interval
            !! 安定化された計算時間間隔

        real(real64) :: dt
        dt = time_interval ! 返値の変数名が長いので短い一時変数を使う

        ! 移流の安定条件が設定されていれば，計算時間間隔を変更して安定化する
        if (allocated(this%Advection_stabilizer)) &
            dt = this%Advection_stabilizer%stabilize(dt)

        ! 拡散の安定条件が設定されていれば，計算時間間隔を変更して安定化する
        if (allocated(this%Diffusion_stabilizer)) &
            dt = this%Diffusion_stabilizer%stabilize(dt)

        stabilized_time_interval = dt
    end function stabilize
end module incompressible_stability_conditions
