!>計算の安定化に関係する手続を提供する．
!>
!>安定条件を適用して安定化された値を返す手続が含まれる．
!>
module incompressible_op_stabilize
    use, intrinsic :: iso_fortran_env
    use :: incompressible_op_vars_stabilizer_dtByStabilityConditions
    use :: incompressible_op_by
    implicit none
    private
    public :: operator(.stabilize.)

    !>ユーザ定義演算子`.stabilize.`を定義するインタフェース
    interface operator(.stabilize.)
        procedure :: stabilize_time_interval
    end interface

contains
    !>安定条件を満たす計算時間間隔を返す．
    function stabilize_time_interval(dt_by_stability_conditions) result(time_interval)
        implicit none
        !&<
        type(dt_by_stability_conditions_type), intent(in) :: dt_by_stability_conditions
            !! 安定化したい計算時間間隔
        !&>
        real(real64) :: time_interval
            !! 安定化された計算時間間隔

        time_interval = dt_by_stability_conditions &
                        %stability_conditions &
                        %stabilize(dt_by_stability_conditions%dt)
    end function stabilize_time_interval
end module incompressible_op_stabilize
