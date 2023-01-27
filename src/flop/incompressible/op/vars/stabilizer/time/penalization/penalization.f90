!>時間積分の安定化に関係する派生型や手続を提供する．
!>
!>派生型には，Penalizationに起因する安定条件を取り扱う派生型が含まれる．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
!>手続には，Penalizationに起因する安定条件を取り扱う型のコンストラクタが含まれる．
!>
module incompressible_op_vars_stabilizer_time_penalization
    use, intrinsic :: iso_fortran_env
    use :: incompressible_op_vars_stabilizer_time
    use :: discreteTime_variables
    implicit none
    private
    public :: Penalization

    !>Penalizationに起因する安定条件を取り扱う派生型．
    type, public, extends(time_stabilizer_atype) :: penalization_stabilizer_type
        real(real64) :: time_interval
            !! 計算時間間隔
        real(real64) :: permeability
            !! 多孔体の透過率 [1/s]
    contains
        procedure, public, pass :: stabilize
        !* 安定条件を考慮した計算時間間隔を返す
    end type penalization_stabilizer_type

    !>コンストラクタを`Penalization`として呼ぶためのインタフェース
    interface Penalization
        procedure :: construct_penalization_stabilizer
    end interface
contains
    !>Penalizationに関する安定条件のコンストラクタ．
    !>Penalizationを考慮した時間間隔の安定化手続を構築して返す．
    function construct_penalization_stabilizer(time_interval, permeability, Penalty_number) result(new_stabilizer)
        implicit none
        !&<
        real(real64), intent(in) :: time_interval
            !! 計算時間間隔
        real(real64), intent(in) :: permeability
            !! 透過率\(K \text{[1/s]}\)
        real(real64), intent(in) :: Penalty_number
            !! Penalizationに起因する安定性に関わる定数
        !&>
        type(penalization_stabilizer_type) :: new_stabilizer
            !! 構築された安定条件

        new_stabilizer%time_interval = time_interval
        new_stabilizer%permeability = permeability
        new_stabilizer%stability_coefficient = Penalty_number
    end function construct_penalization_stabilizer

    !>Penalizationに関する安定条件を考慮した計算時間間隔を返す．
    function stabilize(this, dt) result(stabilized_dt)
        implicit none
        !&<
        class(penalization_stabilizer_type) , intent(in) :: this
            !! 当該実体仮引数
        real(real64)                        , intent(in) :: dt
            !! 安定化したい計算時間間隔
        !&>
        real(real64) :: stabilized_dt
            !! 安定条件を考慮した計算時間間隔

        !|安定条件を考慮した計算時間間隔は，安定条件
        !\[
        !   P=\frac{\varDelta t}{K}<1
        !\]
        !から決定する．

        stabilized_dt = min(dt, &
                            this%stability_coefficient &
                            *this%permeability)
    end function stabilize
end module incompressible_op_vars_stabilizer_time_penalization
