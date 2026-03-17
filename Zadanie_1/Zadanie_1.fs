(*
Вриант 8
Дерево содержит вещественные числа. Округлить каждый элемент.
*)
open System

/// Структура бинарного дерева.
type Tree =
    | Empty
    | Node of float * Tree * Tree

/// функция обхода дерева дерево.
let rec treeMap f tree =
    match tree with
    | Empty -> Empty
    | Node(value, left, right) ->
        Node(f value, treeMap f left, treeMap f right)

/// Функция вставки 
let rec insert value tree =
    match tree with
    | Empty -> Node(value, Empty, Empty)
    | Node(root, left, right) ->
        if value < root then
            Node(root, insert value left, right)
        else
            Node(root, left, insert value right)

/// Генерация дерева
let rnd = Random()
let rec generateRandomTree n tree =
    if n <= 0 then 
        tree
    else
        let newValue = rnd.NextDouble() * 100.0
        generateRandomTree (n - 1) (insert newValue tree)

/// Вывод дерева в консоль.
let rec printTree indent prefix tree =
    match tree with
    | Empty -> ()
    | Node(value, left, right) ->
        printTree (indent + "    ") " " right
        printfn "%s%s%.2f" indent prefix value
        printTree (indent + "    ") " " left

[<EntryPoint>]
let main _ =
    // Генерация исходного дерева.
    let originalTree = generateRandomTree 10 Empty
    
    printfn "--- Исходное дерево ---"
    
    printTree "" "" originalTree
    
    // строим новое дерево, округляя элементы.
    let roundedTree =
        originalTree |> treeMap (fun x -> Math.Round(x))
    
    printfn "\n--- Новое дерево ---"
    
    printTree "" "" roundedTree
    
    0