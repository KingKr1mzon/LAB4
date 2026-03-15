(*
Вриант 8
Сколько раз входит заданный элемент в дерево?
*)

open System

/// Структура бинарного дерева.
type Tree =
    | Empty
    | Node of int * Tree * Tree

/// f - функция, которая берет аккумулятор и значение узла, возвращая новый аккумулятор
let rec treeFold f acc tree =
    match tree with
    | Empty -> acc
    | Node(value, left, right) ->
        // Сначала сворачиваем левое поддерево
        let accLeft = treeFold f acc left
        // Применяем функцию к текущему значению
        let accCurrent = f accLeft value
        // Затем сворачиваем правое поддерево
        treeFold f accCurrent right

/// Функция для генерации дерева
let rnd = Random()
let rec generateRandomTree depth =
    if depth <= 0 then
        Empty
    else
        let value = rnd.Next(1, 6) 
        Node(value, generateRandomTree (depth - 1), generateRandomTree (depth - 1))
        
/// Вывод дерева в консоль.
let rec printTree indent tree =
    match tree with
    | Empty -> ()
    | Node(value, left, right) ->
        printTree (indent + "    ") right
        printfn "%s% i" indent value
        printTree (indent + "    ") left

[<EntryPoint>]
let main _ =
    let tree = generateRandomTree 3
    
    printf "Введите искомый элемент: "
    
    let target = int (Console.ReadLine()) // Искомый элемент
    
    printTree "" tree
    
    let count = tree |> treeFold (fun acc x -> if x = target then acc + 1 else acc) 0
    
    printfn "Количество повторов %i в дереве: %d" target count
    0