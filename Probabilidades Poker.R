# Probabilidades Poker.

# Lista con las 48 cartas. 
# Representación de las cartas: [1-12] Diamantes, [13-24] Corazones, [25-36] Treboles y [37-48] Picas.
cartas<-1:48

# Simulación de sacar cinco cartas.
mano<-sample(cartas, 5, replace = FALSE)
mano

# Calcula los números de las cartas que hemos sacado independientemente de su palo.
v<-rep(0,5)
for (i in 1:5){v[i]=mano[i]%%12}
v

# Función que detecta un posible póker.

Poker<-function(cartas){
  a<-cartas[1]%%12
  b<-0
  i<-2
  while(cartas[i]%%12==cartas[1]%%12 & i<6){
    i<-i+1
  }
  if(i<6){b<-cartas[i]%%12} else{ return(TRUE)}
  i<-2
  na<-1
  nb<-0
  while(i<6){
    if (cartas[i]%%12==b){nb<-nb+1} else {if (cartas[i]%%12==a){na<-na+1}}
    i<-i+1
  }
  if (na>=4 || nb>=4) {return(TRUE)} else { return(FALSE)}
}

# Comprobamos si tenemos un poker para las cinco cartas que hemos sacado antes.

if (Poker(mano)){print("Tenemos Poker")} else {print("No hay Poker")}

# Simulación del número de pokers en 10000 manos de cinco cartas

np<-0
for (j in 1:10000){
muestra<-sample(cartas, 5, replace = FALSE)
if (Poker(muestra)) {np<-np+1; print(muestra)}
}
# El número de poker obtenidos es:
np
# El porcentaje de obtener poker en 10000 manos será por tanto:
np/10000*100
# Un porcentaje muy parecido al calculado en el papel.

# Función para saber si en nuestra mano hay color.
Color<-function(cartas){
  i<-1
  n1<-0
  n2<-0
  n3<-0
  n4<-0
  while (i<6) {
    if (cartas[i]<=12){n1<-n1+1}
    else if (cartas[i]<=24){n2<-n2+1}
      else if (cartas[i]<=36){n3<-n3+1}
        else {n4<-n4+1}
    i<-i+1
  }
  if (n4==5 || n3==5 || n2==5 || n1==5) {return(TRUE)} else {return(FALSE)}
}

# Comprobamos si tenemos Color para la mano que hemos sacado antes.
if (Color(mano)) {print("Hay Color")} else {print("No hay Color")}

# Simulación del número de colores en 10000 manos de cinco cartas

nc<-0
for (j in 1:10000){
  muestra<-sample(cartas, 5, replace = FALSE)
  if (Color(muestra)) {nc<-nc+1; print(muestra)}
}
# El número de colores obtenidos es:
nc
# El porcentaje de obtener color en 10000 manos será por tanto:
nc/10000*100
# Un porcentaje muy parecido al calculado en el papel.

# Función para saber si hay un trío en nuestra mano.
Trio<-function(cartas){
  a<-cartas[1]%%12
  b<-0
  c<-(-1)
  i<-2
  while(cartas[i]%%12==cartas[1]%%12 & i<6){
    i<-i+1
  }
  if(cartas[i]%%12!=cartas[1]%%12){b<-cartas[i]%%12} else{return(FALSE)}
  
  i<-2
  while((cartas[i]%%12==a || cartas[i]%%12==b) & i<6){
    i<-i+1
  }
  if(i<6){c<-cartas[i]%%12} 
  
  i<-2
  na<-1
  nb<-0
  nc<-0
  while(i<6){
    if (cartas[i]%%12==b){nb<-nb+1} 
    else {if (cartas[i]%%12==a){na<-na+1}
      else if (cartas[i]%%12==c & c>=0){nc<-nc+1}}
    i<-i+1
  }
  if (na==3 || nb==3 || nc==3) {return(TRUE)} else { return(FALSE)}
}

# Comprobamos si tenemos un trío para la mano que hemos sacado antes.
if (Trio(mano)) {print("Hay Trio")} else {print("No hay Trio")}

# Simulación del número de tríos en 10000 manos de cinco cartas
nt<-0
for (j in 1:10000){
  muestra<-sample(cartas, 5, replace = FALSE)
  if (Trio(muestra)==T) {nt<-nt+1; print(muestra)}
}
# El número de Trios obtenidos es:
nt
# El porcentaje de obtener tríos en 10000 manos será por tanto:
nt/10000*100

