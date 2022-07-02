!>時間方向の軸の情報関する定数や派生型を提供する．
!>
!>定義される型には，時間方向の情報（開始時間，終了時間）
!>を取り扱う派生型が含まれる．
!>また，開始時間や終了時間を配列として扱うので，
!>配列を参照するための定数も定義される．
!>
module time_vars_axis
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    implicit none
    private
    public :: time_axis_begin_index, time_axis_end_index

    enum, bind(c)
        enumerator :: time_axis_begin_index = 1
            !! 開始時間（時間軸の最小値）を参照するための配列添字
        enumerator :: time_axis_end_index
            !! 終了時間（時間軸の最大値）を参照するための配列添字
    end enum

    !>時間軸を取り扱う派生型．
    !>開始時間，終了時間が含まれる．
    type, public :: time_axis_type
        real(real64), private :: begin = 0d0
            !! 開始時間（時間軸の最小値）
        real(real64), private :: end = 0d0
            !! 終了時間（時間軸の最大値）
    contains
        !&<
        procedure, public, pass :: construct_by_values
        !* 値に基づいて変数を生成
        procedure, public, pass :: construct_by_array
        !* 配列に基づいて変数を生成
        generic :: construct => construct_by_values, &
                                construct_by_array
        !&>
        procedure, public, pass :: set_period
        !* 開始時間と終了時間を設定
        procedure, public, pass :: get_period
        !* 開始時間と終了時間を返却
        procedure, public, pass :: get_duration
        !* 計算時間（開始時間と終了時間の差）を返却

        procedure, public, pass :: assign_array
        !* 開始時間と終了時間を持った配列を代入
        procedure, public, pass :: assign_time_axis
        !* `time_axis`型の変数を代入
        generic :: assignment(=) => assign_array, assign_time_axis
    end type time_axis_type

contains

    !>開始時間と終了時間を持った配列を代入する．
    !>単体で呼び出すことはなく，代入演算子`=`をオーバーロード
    !>して利用する．
    subroutine assign_array(this, period_val)
        implicit none
        !&<
        class(time_axis_type) , intent(inout) :: this
            !! 当該実体仮引数
        real(real64), intent(in)    :: period_val(2)
            !! 軸の最小値と最大値<br>
            !! `[min, max]`の順に格納
        !&>

        this%begin = period_val(time_axis_begin_index)
        this%end = period_val(time_axis_end_index)
    end subroutine assign_array

    !>`time_axis`型の変数の値をコピーする．
    !>単体で呼び出すことはなく，代入演算子`=`をオーバーロード
    !>して利用する．
    subroutine assign_time_axis(this, ref_axis)
        implicit none
        !&<
        class(time_axis_type), intent(inout) :: this
            !! 当該実体仮引数
        type(time_axis_type), intent(in) :: ref_axis
            !! コピー元の`time_axis_type`変数
        !&>
        this%begin = ref_axis%begin
        this%end = ref_axis%end
    end subroutine assign_time_axis

    !------------------------------------------------------------------!
    !>引数で指定した，開始時間，終了時間に基づいて
    !>`time_axis_type`変数を生成する．
    !> 動作は`set_period`と同じであるが，
    !> 他の型とインタフェースを統一するために定義される．
    subroutine construct_by_values(this, time_begin, time_end)
        implicit none
        !&<
        class(time_axis_type)   , intent(inout) :: this
            !! 当該実体仮引数
        real(real64)            , intent(in)    :: time_begin
            !! 開始時間
        real(real64)            , intent(in)    :: time_end
            !! 終了時間
        !&>

        ! `set_period`に処理を委譲する
        call this%set_period(period_val=[time_begin, time_end])
    end subroutine construct_by_values

    !>引数で指定した，開始時間，終了時間を持つ配列に基づいて
    !>`time_axis_type`変数を生成する．
    subroutine construct_by_array(this, period_val)
        implicit none
        !&<
        class(time_axis_type), intent(inout) :: this
            !! 当該実体仮引数
        real(real64), intent(in) :: period_val(2)
            !! 開始時間と終了時間<br>
            !! `[begin, end]`の順に格納
        !&>

        call this%set_period(period_val=period_val)
    end subroutine construct_by_array

    !------------------------------------------------------------------!
    !>開始時間と終了時間を設定する．
    subroutine set_period(this, period_val)
        implicit none
        !&<
        class(time_axis_type) , intent(inout) :: this
            !! 当該実体仮引数
        real(real64), intent(in)    :: period_val(2)
            !! 開始時間と終了時間<br>
            !! `[begin, end]`の順に格納
        !&>

        this = period_val ! 代入演算子を利用
    end subroutine set_period

    !>開始時間と終了時間を返す．
    function get_period(this) result(period_val)
        implicit none
        !&<
        class(time_axis_type), intent(in) :: this
            !! 当該実体仮引数
        !&>
        real(real64) :: period_val(2)
            !! 開始時間と終了時間<br>
            !! `[begin, end]`の順に格納

        period_val(time_axis_begin_index) = this%begin
        period_val(time_axis_end_index) = this%end
    end function get_period

    !------------------------------------------------------------------!
    !>計算時間（開始時間と終了時間の差）を返す．
    function get_duration(this) result(duration)
        implicit none
        !&<
        class(time_axis_type), intent(in) :: this
            !! 当該実体仮引数
        !&>
        real(real64) :: duration
            !! 計算時間

        duration = this%end - this%begin
    end function get_duration
end module time_vars_axis
