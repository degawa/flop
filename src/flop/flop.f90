!>Flopを構成するモジュールを一括して`use`するためのモジュール
module flop
    use :: space_variables
    use :: space_op

    use :: time_variables
    use :: time_op

    use :: discreteTime_variables

    use :: discretization_op

    use :: grid_uniform_staggered_2d
    use :: grid_uniform_staggered_variables
    use :: grid_uniform_staggered_operators

    use :: incompressible_characteristics
    use :: incompressible_stability
    use :: incompressible_op
    public
end module flop
