import time
import json
me_generation = [[]]
def Primos_y_sus_Susecivas_generaciones(xnun):
    for q1 in range((2**(xnun-1)),(2**xnun)+1):
        me_q1 = 0
        for q2 in me_generation:
            for q3 in q2:
                if(q1%q3 == 0):
                    me_q1 += 1
                    break
                if(q1/q3 < 2):
                    break
        if(me_q1 > len(me_generation)-1):
            me_generation.append([])
        me_generation[me_q1].append(q1)
    return me_generation
def guardar_lista_en_lineas(lista_de_listas, nombre_archivo):
  mytxt="["
  for sublista in lista_de_listas: 
     me_list2=[]
     for q2 in sublista:
     	me_list2.append(str(q2).zfill(5))
     mytxt += "\n" + str(me_list2)
  mytxt += "]"
  with open(nombre_archivo, 'w') as archivo:
      archivo.write(mytxt)

mytimeTotal = 0
for q1 in range(2,10):
  mytime1 = time.time()
  guardar_lista_en_lineas(Primos_y_sus_Susecivas_generaciones(q1), "data" + str(q1) + "_.json")
  mytime2 = time.time()
  mytimeTotal += (mytime2 - mytime1)
  print(f"{q1}\tTotal: { int(mytimeTotal) }\tTime {mytime2 - mytime1} ")
  