!| 時間方向の離散化に関する型を提供する．
!
!定義される型には，時間軸の情報（計算開始時間と終了時間）
!を取り扱う派生型が含まれる．
!
module discreteTime
    use, intrinsic :: iso_fortran_env
    use :: time_axis
    implicit none
    private

    !| 時間方向の離散化情報を取り扱う派生型．
    type, public :: discrete_time_type
        type(time_axis_type), private :: time
            !! 時間軸の情報
        real(real64), private :: dt
            !! 計算時間間隔<br>
            !! 計算開始から終了まで一定．
        integer(int32), private :: Nt
            !! 時間積分回数
    contains
        !&<
        procedure, public, pass :: construct_by_time_interval
            !! 時間軸と計算時間間隔を用いて時間離散化
        procedure, public, pass :: construct_by_number_of_integration
            !! 時間軸と時間積分回数を用いて時間離散化
        generic :: construct => &
                        construct_by_number_of_integration, &
                        construct_by_time_interval
        !&>
        procedure, public, pass :: get_time_interval
            !! 計算時間間隔を返却
        procedure, public, pass :: get_number_of_integration
            !! 時間積分回数を返却
    end type discrete_time_type

contains

    !| 時間軸と計算時間間隔を用いて時間離散化情報を作成する．
    subroutine construct_by_time_interval(this, time, time_interval)
        implicit none
        !&<
        class(discrete_time_type)   , intent(inout) :: this
            !! 当該実体仮引数
        type(time_axis_type)        , intent(in)    :: time
            !! 時間軸
        real(real64)                , intent(in)    :: time_interval
            !! 計算時間間隔
        !&>

        this%time = time

        this%dt = time_interval
        this%Nt = nint(time%get_duration()/time_interval)
        ! 誤差の都合で199.99999->199とならないように最近傍値に丸める
    end subroutine construct_by_time_interval

    !| 時間軸と計算時間間隔を用いて時間離散化情報を作成する．
    subroutine construct_by_number_of_integration(this, time, number_of_integration)
        implicit none
        !&<
        class(discrete_time_type)   , intent(inout) :: this
            !! 当該実体仮引数
        type(time_axis_type)        , intent(in)    :: time
            !! 時間軸
        integer(int32)              , intent(in)    :: number_of_integration
            !! 時間積分回数
        !&>

        this%time = time

        this%Nt = number_of_integration
        this%dt = time%get_duration()/dble(number_of_integration)
        ! 積分は微小区間の数だけ行われるので，
        ! 格子点間隔を出す式に現れる -1 は不要
        !
        ! dt = L/4
        !     |<-dt ->|                         number of time integration = 4
        ! n   |   1   |   2       3       4     time integration counts
        !     o-------o-------o-------o-------o
        ! i   |1      |2      3       4      5| grid point numbers
        !     |<-dx ->|                       | number of grid points = 5
        !     |<------------- L ------------->|
        ! dx = L/(5-1)
        !
    end subroutine construct_by_number_of_integration

    !| 計算時間間隔を返す．
    function get_time_interval(this) result(interval)
        implicit none

        class(discrete_time_type), intent(in) :: this
            !! 当該実体仮引数

        real(real64) :: interval
            !! 計算時間間隔

        interval = this%dt
    end function get_time_interval

    !| 時間積分回数を返す．
    function get_number_of_integration(this) result(num_integration)
        implicit none

        class(discrete_time_type), intent(in) :: this
            !! 当該実体仮引数

        integer(int32) :: num_integration
            !! 時間積分回数

        num_integration = this%Nt
    end function get_number_of_integration
end module discreteTime
