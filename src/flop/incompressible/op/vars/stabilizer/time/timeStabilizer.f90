!>時間積分の安定化に関係する派生型を提供する．
!>
!>派生型には，移流や拡散などに起因する安定条件を取り扱う抽象型が含まれる．
!>
module incompressible_op_var_stabilizer_time
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    !>安定条件を取り扱う抽象型．
    !>
    !>複数の安定条件で共通に用いられる値（最小格子幅，安定性に関わる定数），
    !>安定化するための型束縛手続きのインタフェースを定義する．
    type, public, abstract :: time_stabilizer_atype
        real(real64) :: min_spatial_interval
            !! 最小の格子幅
        real(real64) :: stability_coefficient
            !! 安定性に関わる定数
    contains
        procedure(Istabilize), public, pass, deferred :: stabilize
        !* 安定条件を考慮した計算時間間隔を返却
    end type time_stabilizer_atype

    abstract interface
        !>安定条件を考慮した計算時間間隔を返す手続の抽象インタフェース
        function Istabilize(this, dt) result(stabilized_dt)
            use, intrinsic :: iso_fortran_env
            import time_stabilizer_atype
            class(time_stabilizer_atype), intent(in) :: this
                !! 当該実体仮引数
            real(real64), intent(in) :: dt
                !! 計算時間間隔
            real(real64) :: stabilized_dt
                !! 安定条件を考慮した計算時間間隔
        end function Istabilize
    end interface
end module incompressible_op_var_stabilizer_time
