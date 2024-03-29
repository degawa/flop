!>時間積分の安定化に関係する派生型や手続を提供する．
!>
!>派生型には，拡散に起因する安定条件を取り扱う派生型が含まれる．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
!>手続には，拡散に起因する安定条件を取り扱う型のコンストラクタが含まれる．
!>
module incompressible_op_vars_stabilizer_time_diffusion
    use, intrinsic :: iso_fortran_env
    use :: incompressible_op_vars_stabilizer_time
    use :: grid_uniform_stg_2d
    implicit none
    private
    public :: Diffusion

    !>拡散に起因する安定条件を取り扱う派生型．
    !>`time_interval_stabilizer_atype`を拡張する．
    type, public, extends(time_stabilizer_atype) :: diffusion_stabilizer_type
        real(real64) :: kinetic_viscosity
            !! 動粘度
    contains
        procedure, public, pass :: stabilize
        !* 安定条件を考慮した計算時間間隔を返す
    end type diffusion_stabilizer_type

    !>コンストラクタを`Diffusion`として呼ぶためのインタフェース
    interface Diffusion
        procedure :: construct_diffusion_stabilizer
    end interface

contains
    !>拡散に関する安定条件のコンストラクタ．
    !>拡散に関する安定条件を構築して返す．
    function construct_diffusion_stabilizer(minimum_interval, kinetic_viscosity, Diffusion_number) result(new_stabilizer)
        implicit none
        !&<
        real(real64), intent(in) :: minimum_interval
            !! 格子幅の最小値
        real(real64), intent(in) :: kinetic_viscosity
            !! 動粘度
        real(real64), intent(in) :: Diffusion_number
            !! 拡散に起因する安定性に関わる定数（拡散数）
        !&>
        type(diffusion_stabilizer_type) :: new_stabilizer
           !! 構築された安定条件

        new_stabilizer%min_spatial_interval = minimum_interval
        new_stabilizer%kinetic_viscosity = kinetic_viscosity
        new_stabilizer%stability_coefficient = Diffusion_number
    end function construct_diffusion_stabilizer

    !>拡散に関する安定条件を考慮した計算時間間隔を返す．
    function stabilize(this, dt) result(stabilized_dt)
        implicit none
        !&<
        class(diffusion_stabilizer_type), intent(in) :: this
            !! 当該実体仮引数
        real(real64)                    , intent(in) :: dt
            !! 安定化したい計算時間間隔
        !&>
        real(real64) :: stabilized_dt
            !! 安定条件を考慮した計算時間間隔

        !|安定条件を考慮した計算時間間隔は，安定条件
        !\[
        !   D=\nu\frac{\varDelta t}{d^{-1}\min\left(\varDelta x^2, \varDelta y^2\right)}<0.5
        !\]
        !から決定する．
        !ここで，\(d\)は計算する空間次元であり，今\(d=2\)である．

        stabilized_dt = min(dt, &
                            this%stability_coefficient &
                            *0.5d0*this%min_spatial_interval**2 &
                            /this%kinetic_viscosity)
    end function stabilize
end module incompressible_op_vars_stabilizer_time_diffusion
