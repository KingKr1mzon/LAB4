(*
Вриант 8
Сколько раз входит заданный элемент в дерево?
*)

open System

/// Структура бинарного дерева.
type Tree =
    | Empty
    | Node of int * Tree * Tree

/// f - функция, которая берет аккумулятор и значение узла, возвращая новый аккумулятор.
let rec treeFold f acc tree =
    match tree with
    | Empty -> acc
    | Node(value, left, right) ->
        // Сначала сворачиваем левое поддерево.
        let accLeft = treeFold f acc left
        // Применяем функцию к текущему значению.
        let accCurrent = f accLeft value
        // Затем сворачиваем правое поддерево.
        treeFold f accCurrent right

/// Функция вставки.
let rec insert value tree =
    match tree with
    | Empty -> Node(value, Empty, Empty)
    | Node(root, left, right) ->
        if value < root then
            Node(root, insert value left, right)
        else
            Node(root, left, insert value right)

/// Генерация дерева.
let rnd = Random()
let rec generateRandomTree n tree =
    if n <= 0 then 
        tree
    else
        let newValue = rnd.Next(1, 6)
        generateRandomTree (n - 1) (insert newValue tree)
        
/// Вывод дерева в консоль.
let rec printTree indent prefix tree =
    match tree with
    | Empty -> ()
    | Node(value, left, right) ->
        printTree (indent + "    ") " " right
        printfn "%s%s%i" indent prefix value
        printTree (indent + "    ") " " left

[<EntryPoint>]
let main _ =
    // Генерация дерева из 10 элементов.
    let tree = generateRandomTree 10 Empty
    
    printf "Введите искомый элемент: "
    
    let target = int (Console.ReadLine())
    
    printTree "" "" tree
    
    let count =
        tree
        |> treeFold
             (fun acc x -> if x = target then acc + 1 else acc) 0
    
    printfn "Количество повторов %i в дереве: %d" target count
    
    0