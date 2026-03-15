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

/// Генерация случайного дерева.
let rnd = Random()
let rec generateRandomTree depth =
    if depth <= 0 then
        Empty
    else
        let value = rnd.NextDouble() * 100.0
        Node(value, generateRandomTree (depth - 1), generateRandomTree (depth - 1))

/// Вывод дерева в консоль.
let rec printTree indent tree =
    match tree with
    | Empty -> ()
    | Node(value, left, right) ->
        printTree (indent + "    ") right
        printfn "%s%.2f" indent value
        printTree (indent + "    ") left

[<EntryPoint>]
let main _ =
    // Генерация исходного дерева.
    let originalTree = generateRandomTree 3
    
    printfn "--- Исходное дерево ---"
    
    printTree "" originalTree
    
    // строим новое дерево, округляя элементы.
    let roundedTree = originalTree |> treeMap (fun x -> Math.Round(x))
    
    printfn "\n--- Новое дерево ---"
    
    printTree "" roundedTree
    
    0