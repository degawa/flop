!>時間積分の安定化に関係する派生型や手続を提供する．
!>
!>派生型には，移流に起因する安定条件を取り扱う派生型が含まれる．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
!>手続には，移流に起因する安定条件を取り扱う型のコンストラクタが含まれる．
!>
module incompressible_op_vars_stabilizer_time_advection
    use, intrinsic :: iso_fortran_env
    use :: incompressible_op_vars_stabilizer_time
    use :: grid_uniform_stg_2d
    implicit none
    private
    public :: Courant

    !>移流に起因する安定条件を取り扱う派生型．
    !>`time_interval_stabilizer_atype`を拡張する．
    type, public, extends(time_stabilizer_atype) :: advection_stabilizer_type
        real(real64) :: velocity
            !! 代表速度
    contains
        procedure, public, pass :: stabilize
        !* 安定条件を考慮した計算時間間隔を返却
    end type advection_stabilizer_type

    !>コンストラクタを`Courant`として呼ぶためのインタフェース
    interface Courant
        procedure :: construct_advection_stabilizer
    end interface
contains
    !>移流に関する安定条件のコンストラクタ．
    !>移流に関する安定条件を構築して返す．
    function construct_advection_stabilizer(grid, velocity, Courant_number) result(new_stabilizer)
        implicit none
        !&<
        type(staggered_uniform_grid_2d_type), pointer   , intent(in) :: grid
            !! 格子（空間離散化情報）
        real(real64)                                    , intent(in) :: velocity
            !! 代表速度
        real(real64)                                    , intent(in) :: Courant_number
            !! 移流に起因する安定性に関わる定数（Courant数）
        !&>
        type(advection_stabilizer_type) :: new_stabilizer
            !! 構築された安定条件

        new_stabilizer%min_spatial_interval = minval(grid%get_interval())
        new_stabilizer%velocity = velocity
        new_stabilizer%stability_coefficient = Courant_number
    end function construct_advection_stabilizer

    !>移流に関する安定条件を考慮した計算時間刻みを返す．
    function stabilize(this, dt) result(stabilized_dt)
        implicit none
        !&<
        class(advection_stabilizer_type)  , intent(in) :: this
            !! 当該実体仮引数
        real(real64)                    , intent(in) :: dt
            !! 安定化したい計算時間間隔
        !&>
        real(real64) :: stabilized_dt
            !! 安定条件を考慮した計算時間間隔

        stabilized_dt = min(dt, &
                            this%stability_coefficient &
                            *this%min_spatial_interval/this%velocity)
    end function stabilize
end module incompressible_op_vars_stabilizer_time_advection
