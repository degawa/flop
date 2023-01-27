!>計算の安定化に関係する派生型や手続を提供する．
!>
!>派生型には，安定条件を満足するように
!>Penalizationに関係するパラメータを決定する派生型が含まれる．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
!>手続には，安定条件を満足するようにパラメータを決定する派生型の
!>コンストラクタが含まれる．
!>
module incompressible_op_vars_stabilizer_characteristics_reluctivity
    use, intrinsic :: iso_fortran_env
    use :: incompressible_vars_stability_specification, only:Penalization_number_stability_limit
    implicit none
    private
    public :: Reluctivity

    !>安定条件に基づいて抵抗率（=1/透過率）を決定する派生型．
    type, public :: reluctivity_stabilizer_type
        real(real64), private :: reluctivity = huge(0d0)
            !! 抵抗率（=1/透過率）[s]
        real(real64), private :: time_interval
            !! 計算時間間隔
    contains
        procedure, public, pass :: stabilize
        !* 安定条件を考慮した抵抗率を返却
    end type reluctivity_stabilizer_type

    !>コンストラクタを`Reluctivity`として呼ぶためのインタフェース
    interface Reluctivity
        procedure :: construct_recultivity_stabilizer
    end interface
contains
    !>安定条件を満足するようにPenalizationの抵抗率を決定する派生型を
    !>構築して返す．
    function construct_recultivity_stabilizer(time_interval, reluctivity) result(new_stabilizer)
        implicit none
        !&<
        real(real64), intent(in)            :: time_interval
            !! 計算時間間隔
        real(real64), intent(in), optional  :: reluctivity
            !! 抵抗率
        !&>
        type(reluctivity_stabilizer_type) :: new_stabilizer
            !! 構築された抵抗率安定化子

        new_stabilizer%time_interval = time_interval
        if (present(reluctivity)) &
            new_stabilizer%reluctivity = reluctivity
    end function construct_recultivity_stabilizer

    !>安定条件を満足する抵抗率を返す．
    function stabilize(this) result(stabilized_reluctivity)
        implicit none
        class(reluctivity_stabilizer_type), intent(in) :: this
            !! 当該実体仮引数

        real(real64) :: stabilized_reluctivity
            !! 安定化された抵抗率

        stabilized_reluctivity = min(this%reluctivity, &
                                     Penalization_number_stability_limit &
                                     /this%time_interval)
    end function stabilize
end module incompressible_op_vars_stabilizer_characteristics_reluctivity
