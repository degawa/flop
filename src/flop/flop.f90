!>Flopを構成するモジュールを一括して`use`するためのモジュール
module flop
    use :: time_axis
    use :: discreteTime
    use :: time_op

    use :: space_axis
    use :: space_Cartesian
    use :: space_op

    use :: discretization_op

    use :: grid_uniform_staggered_2d
    use :: grid_uniform_staggered_variables
    use :: grid_uniform_staggered_operators

    public
end module flop
