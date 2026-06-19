namespace ProyectoCollatz
open System
open System.Collections.Generic
open System.Numerics
module ArbolComparador =
    let revisarIsDuplicateMal (arbol: List<Dictionary<string, NodoCollatz>>) (objetivo: BigInteger * BigInteger) (nivelFallo: int) =
        let d_obj, p_obj = objetivo
        printfn "--- AUDITORÍA DE DUPLICADO para (%A, %A) ---" d_obj p_obj
        let mutable encontrado = false
        for i in 0 .. nivelFallo do
            let dictNivel = arbol.[i]
            for kvp in dictNivel do
                let nodo = kvp.Value
                if nodo.Desplazamiento = d_obj && nodo.Progresion = p_obj then
                    if not encontrado then
                        printfn "ORIGINAL ENCONTRADO en Nivel %d, Ruta: %s" i nodo.Ruta
                        encontrado <- true
                    else
                        printfn "OTRA APARICIÓN en Nivel %d, Ruta: %s" i nodo.Ruta
        if not encontrado then 
            printfn "Raro: El patrón no existe en niveles anteriores. El 'true' podría estar mal."
        printfn "-------------------------------------------"
    let compararArboles (arbol1: List<Dictionary<string, NodoCollatz>>) (arbol2: List<Dictionary<string, NodoCollatz>>) =
        if arbol1.Count <> arbol2.Count then
            printfn "Fallo: Los árboles tienen diferentes niveles (%d vs %d)" arbol1.Count arbol2.Count
            false
        else
            let mutable todoEsIgual = true
        
            for i in 0 .. arbol1.Count - 1 do
                let dict1 = arbol1.[i]
                let dict2 = arbol2.[i]
            
                if dict1.Count <> dict2.Count then
                    printfn "Fallo en Nivel %d: Diferente número de nodos (%d vs %d)" i dict1.Count dict2.Count
                    todoEsIgual <- false
                else
                    let llaves1 = dict1.Keys |> Seq.sort |> Seq.toList
                    let llaves2 = dict2.Keys |> Seq.sort |> Seq.toList
                
                    if llaves1 <> llaves2 then
                        printfn "Fallo en Nivel %d: Las rutas (llaves) no coinciden" i
                        todoEsIgual <- false
                    else
                        for ruta in llaves1 do
                            let nodo1 = dict1.[ruta]
                            let nodo2 = dict2.[ruta]
                            if nodo1 <> nodo2 then
                                printfn "Fallo en Nivel %d, Ruta %s: Los datos del nodo son distintos" i ruta
                                printfn "nodo1 %A" nodo1
                                printfn "nodo2 %A" nodo2
                                printfn "zzzzzzzzzzzz"
                                revisarIsDuplicateMal arbol1 (nodo1.Desplazamiento, nodo1.Progresion) nodo2.Nivel
                                printfn "zzzzzzzzzzzz"
                                todoEsIgual <- false
            todoEsIgual