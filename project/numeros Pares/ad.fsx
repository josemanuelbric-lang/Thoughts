open System
open System.IO

// Cambiamos el límite a 260 para probar el salto de 255 a 256+
let limite = 1048576

type Registro = {
    numero: int
    bin: string
    pos: int list
    sumas: int list
    lista: int list
    tipo: string
    count: int
    isRecord: bool
}

// Ampliamos el rango de bits buscados para manejar números > 255
let obtenerPosiciones (n: int) =
    [ for i in 30 .. -1 .. 0 do
        if (n &&& (1 <<< i)) <> 0 then
            yield (i + 1) ]

let calcularSumasUnicas (posiciones: int list) =
    let combinaciones = 
        [ for i in 0 .. (posiciones.Length - 1) do
            for j in i .. (posiciones.Length - 1) do
                yield posiciones.[i] + posiciones.[j] ]
    combinaciones |> Set.ofList |> Set.toList |> List.sort

let evaluarTipo (sumas: int list) =
    let pares = sumas |> List.filter (fun x -> x % 2 = 0)
    if pares.IsEmpty then None
    else
        let maxSuma = pares |> List.max
        // Comprobar secuencia continua de pares
        let esTipo2 = (pares = [2 .. 2 .. maxSuma])
        let esTipo4 = (pares = [4 .. 2 .. maxSuma])
        if esTipo2 then Some (pares, "2", pares.Length)
        elif esTipo4 then Some (pares, "4", pares.Length)
        else None

let mutable maxCount2 = 0
let mutable maxCount4 = 0

let datosProcesados = 
    [ for i in 1 .. limite ->
        let pos = obtenerPosiciones i
        let sumas = calcularSumasUnicas pos
        
        match evaluarTipo sumas with
        | Some (lista, tipo, count) ->
            let isRecord = 
                if tipo = "2" && count > maxCount2 then 
                    maxCount2 <- count; true 
                elif tipo = "4" && count > maxCount4 then 
                    maxCount4 <- count; true 
                else false
            
            Some { numero = i; bin = Convert.ToString(i, 2); pos = pos; sumas = sumas; lista = lista; tipo = tipo; count = count; isRecord = isRecord }
        | None -> None
    ] |> List.choose id

let formatear (d: Registro) = 
    let pStr = d.pos |> List.map string |> String.concat " "
    let sStr = d.sumas |> List.map string |> String.concat " "
    let lStr = d.lista |> List.map string |> String.concat " "
    sprintf "%d\t%s\t[%s]\t[%s]\t[%s]:(%s|%d)" d.numero d.bin pStr sStr lStr d.tipo d.count

let header = "numero\tbinario\tposiciones\tsumas\tfiltro_final"

File.WriteAllLines("lista_final.tsv", header :: (datosProcesados |> List.map formatear))
File.WriteAllLines("lista_2record.tsv", header :: (datosProcesados |> List.filter (fun d -> d.isRecord && d.tipo = "2") |> List.map formatear))
File.WriteAllLines("lista_4record.tsv", header :: (datosProcesados |> List.filter (fun d -> d.isRecord && d.tipo = "4") |> List.map formatear))

printfn "Proceso finalizado. Se han guardado los archivos hasta el número %d." limite