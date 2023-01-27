!>Flopを構成するモジュールを一括して`use`するためのモジュール
module flop
    use :: space_variables
    use :: space_operators

    use :: time_variables
    use :: time_operators

    use :: discreteTime_variables
    use :: discreteTime_operators

    use :: discretization_operators

    use :: grid_uniform_stg_variables
    use :: grid_uniform_stg_operators

    use :: incompressible_variables
    use :: incompressible_operators
    public
end module flop
