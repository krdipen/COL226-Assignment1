type vector = float list
type matrix = float list list

exception InvalidInput
exception UnequalVectorSize
exception UnequalMatrixShape
exception IncompatibleMatrixShape
exception SingularMatrix

let rec vdim (v:vector): int = match v with
        [] -> 0
    |   x :: xs -> 1 + vdim xs ;;
(* returns the dimension of a given vector
# vdim [1.0;2.0;3.0] ;;
- : int = 3 *)

let rec mkzerov (n:int): vector = if n = 0 then [] else 0.0 :: mkzerov (n-1) ;;
(* returns the zero vector of that dimension
# mkzerov 5 ;;
- : vector = [0.; 0.; 0.; 0.; 0.] *)

let rec iszerov (v:vector): bool = match v with
        [] -> true
    |   x :: xs -> if x = 0.0 then iszerov xs else false ;;
(* checks if a given vector is a  zero vector
# iszerov [0.; 0.; 0.; 0.; 0.] ;;
- : bool = true
# iszerov [0.; 0.; 3.8; 0.] ;;
- : bool = false *)

let rec addv (v1:vector) (v2:vector): vector = match v1 , v2 with
        [] , [] -> []
    |   x1 :: x1s , x2 :: x2s -> ( x1 +. x2 ) :: addv x1s x2s
    |   [] , x2 :: x2s -> []
    |   x1 :: x1s , [] -> [] ;;
(* adds two vectors v1 and v2 (of the same dimension)
# addv [1.8;2.4;5.7] [9.4;8.6;5.1] ;;
- : vector = [11.2000000000000011; 11.; 10.8] *)

let rec scalarmultv (c:float) (v:vector): vector = match v with
        [] -> []
    |   x :: xs -> ( x *. c ) :: scalarmultv c xs ;;
(* given a scalar c and a vector v, performs the scalar multiplication
# scalarmultv 2.2 [1.2;4.3;7.0] ;;
- : vector = [2.64; 9.46; 15.4000000000000021] *)

let rec dotprodv (v1:vector) (v2:vector): float = match v1 , v2 with
        [] , [] -> 0.0
    |   x1 :: x1s , x2 :: x2s -> ( x1 *. x2 ) +. dotprodv x1s x2s
    |   [] , x2 :: x2s -> 0.0
    |   x1 :: x1s , [] -> 0.0 ;;
(* given two vectors  v1 and v2 of the same dimension, returns their dot product v1 . v2
# dotprodv [1.8;2.4;5.7] [9.4;8.6;5.1] ;;
- : float = 66.63 *)

let rec crossprodv (v1:vector) (v2:vector): vector =
    [ ( List.nth v1 1 *. List.nth v2 2 -. List.nth v1 2 *. List.nth v2 1 ) ;
      ( List.nth v1 2 *. List.nth v2 0 -. List.nth v1 0 *. List.nth v2 2 ) ;
      ( List.nth v1 0 *. List.nth v2 1 -. List.nth v1 1 *. List.nth v2 0 ) ] ;;
(* given two vectors v1 and v2 in 3 dimensions, returns their cross product  v1 x v2
# crossprodv [1.0;2.0;3.0] [4.0;5.0;6.0] ;;
- : vector = [-3.; 6.; -3.] *)

let rec mdim (m:matrix): int*int = match m with
        [] -> 0 , 0
    |   x :: xs -> List.length x , 1 + List.length xs ;;
(* returns the dimensions of a given matrix
# mdim [[1.;2.];[2.;3.];[3.;4.]] ;;
- : int * int = (2, 3) *)

let rec mkzerom (m_:int) (n_:int): matrix = if n_ = 0 then [] else mkzerov m_ :: mkzerom m_ (n_-1) ;;
(* given a dimension m, n > 0, returns the zero m x n matrix
# mkzerom 2 3 ;;
- : matrix = [[0.; 0.]; [0.; 0.]; [0.; 0.]] *)

let rec iszerom (m:matrix): bool = match m with
        [] -> true
    |   x :: xs -> if iszerov x then iszerom xs else false ;;
(* checks if a given matrix is a  zero matrix
# iszerom [[0.; 0.]; [0.; 0.]; [0.; 0.]] ;;
- : bool = true
# iszerom [[0.; 0.]; [0.; 0.]; [0.;1.]] ;;
- : bool = false *)

let rec mkunitv (m_:int) (n_:int): vector = if m_ = 0 then
                                                []
                                            else
                                                if m_ == n_ then
                                                    1.0 :: mkunitv (m_-1) n_
                                                else
                                                    0.0 :: mkunitv (m_-1) n_ ;;

let rec mkunitm_h (m_:int) (n_:int): matrix =   if n_ = 0 then
                                                    []
                                                else
                                                    mkunitv m_ n_ :: mkunitm_h m_ (n_-1) ;;

let rec mkunitm (m_:int): matrix = mkunitm_h m_ m_ ;;
(* given a dimension m > 0, returns the unit m x m (square) matrix
# mkunitm 3 ;;
- : matrix = [[1.; 0.; 0.]; [0.; 1.; 0.]; [0.; 0.; 1.]] *)

let rec isunitv (v:vector) (m_:int) (n_:int) (l_:int): bool = match v with
        [] -> false
    |   x :: xs ->  if m_ = n_ then
                        if x = 1.0 then
                            if (n_+1) > l_ then
                                true
                            else
                                isunitv xs m_ (n_+1) l_
                        else
                            false
                    else
                        if x = 0.0 then
                            if (n_+1) > l_ then
                                true
                            else
                                isunitv xs m_ (n_+1) l_
                        else
                            false ;;

let rec isunitm_h (m:matrix) (m_:int): bool = match m with
        [] -> false
    |   x :: xs -> if isunitv x m_ 1 ( vdim x ) then if (m_+1) > ( vdim x ) then true else isunitm_h xs (m_+1) else false ;;

let rec isunitm (m:matrix): bool = isunitm_h m 1 ;;
(* checks if a given matrix is a unit (square) matrix
# isunitm [[1.; 0.; 0.]; [0.; 1.; 0.]; [0.; 0.; 1.]] ;;
- : bool = true
# isunitm [[1.; 0.; 0.]; [0.; 1.; 0.]; [0.; 0.; 0.]] ;;
- : bool = false *)

let rec addm (m1:matrix) (m2:matrix): matrix = match m1 , m2 with
        [] , [] -> []
    |   x1 :: x1s , x2 :: x2s -> addv x1 x2 :: addm x1s x2s
    |   [] , x2 :: x2s -> []
    |   x1 :: x1s , [] -> [] ;;
(* adds two matrices m1 and m2 (of the same dimensions)
# addm [[1.; 0.; 0.]; [0.; 1.; 0.]; [0.; 0.; 1.]] [[1.; 0.; 0.]; [0.; 1.; 0.]; [0.; 0.; 1.]] ;;
- : matrix = [[2.; 0.; 0.]; [0.; 2.; 0.]; [0.; 0.; 2.]] *)

let rec scalarmultm (c:float) (m:matrix): matrix = match m with
        [] -> []
    |   x :: xs -> scalarmultv c x :: scalarmultm c xs ;;
(* given a scalar c and a matrix m, performs the scalar multiplication
# scalarmultm 2.0 [[1.; 0.; 0.]; [0.; 1.; 0.]; [0.; 0.; 1.]] ;;
- : matrix = [[2.; 0.; 0.]; [0.; 2.; 0.]; [0.; 0.; 2.]] *)

let rec transm (m:matrix): matrix = match m with
        [] -> []
    |   [] :: xss -> transm xss
    |   ( x :: xs ) :: xss -> ( x :: List.map List.hd xss ) :: transm ( xs :: List.map List.tl xss ) ;;
(* transpose a given matrix
# transm [[1.;2.];[2.;3.];[5.;7.]] ;;
- : matrix = [[1.; 2.; 5.]; [2.; 3.; 7.]] *)

let rec multv (v:vector) (m:matrix): vector = match m with
        [] -> []
    |   x :: xs -> dotprodv v x :: multv v xs ;;

let rec multm (m1:matrix) (m2:matrix): matrix = match m1 with
        [] -> []
    |   [] :: xss -> multm xss m2
    |   ( x1 :: x1s ) :: x1ss -> transm ( multv ( x1 :: List.map List.hd x1ss ) m2 :: transm ( multm ( x1s :: List.map List.tl x1ss ) m2 ) ) ;;
(* multiply two matrices m1 and m2 (assuming their dimensions allow them to be multiplied
# multm [[1.;2.];[2.;3.];[5.;7.]] [[1.; 0.; 0.]; [0.; 1.; 0.]; [0.; 0.; 1.]] ;;
- : matrix = [[1.; 2.]; [2.; 3.]; [5.; 7.]] *)

let rec detm_hh (m:matrix) (m_:int) (n_:int): matrix = match m with
        [] -> []
    |   x :: xs -> if n_ = m_ then detm_hh xs m_ (n_+1) else x :: detm_hh xs m_ (n_+1) ;;

let rec detm (m:matrix): float = match m with
        [] -> 1.0
    |   x :: xs -> detm_h x xs 1
and detm_h (v:vector) (m:matrix) (m_:int): float = match v with
        [] -> 0.0
    |   x :: xs ->  if m_ mod 2 = 1 then
                        0.0 +. ( x *. detm ( transm ( detm_hh ( transm m ) m_ 1 ) ) ) +. detm_h xs m (m_+1)
                    else
                        0.0 -. ( x *. detm ( transm ( detm_hh ( transm m ) m_ 1 ) ) ) +. detm_h xs m (m_+1) ;;
(* compute the determinant of a matrix (assuming it is a square matrix)
# detm [[1.;2.;3.];[4.;5.;6.];[7.;8.;8.]] ;;
- : float = 3. *)

let rec invm_h (v:vector) (m:matrix) (m_:int) (n_:int): vector = match v with
        [] -> []
    |   x :: xs ->  if m_ mod 2 = 1 then
                        if n_ mod 2 = 1 then
                            ( 0.0 +. detm ( transm ( detm_hh ( transm m ) m_ 1 ) ) ) :: invm_h xs m (m_+1) n_
                        else
                            ( 0.0 -. detm ( transm ( detm_hh ( transm m ) m_ 1 ) ) ) :: invm_h xs m (m_+1) n_
                    else
                        if n_ mod 2 = 1 then
                            ( 0.0 -. detm ( transm ( detm_hh ( transm m ) m_ 1 ) ) ) :: invm_h xs m (m_+1) n_
                        else
                            ( 0.0 +. detm ( transm ( detm_hh ( transm m ) m_ 1 ) ) ) :: invm_h xs m (m_+1) n_ ;;

let rec inverse (m:matrix) (n:matrix) (m_:int): matrix = match m with
        [] -> []
    |   x :: xs -> invm_h x ( detm_hh n m_ 1 ) 1 m_ :: inverse xs n (m_+1) ;;

let rec invm (m:matrix): matrix = scalarmultm ( 1.0 /. ( detm m ) ) ( transm ( inverse m m 1 ) ) ;;
(* return the inverse of a given matrix (if defined)
# invm [[1.;2.;3.];[4.;5.;6.];[7.;8.;8.]] ;;
- : matrix = [[-2.66666666666666652; 2.66666666666666652; -1.]; [3.33333333333333304; -4.33333333333333304; 2.]; [-1.; 2.; -1.]] *)
