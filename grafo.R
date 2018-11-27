
# tot ore = 4,5


Nodo <- setRefClass("nodo", fields = list(nome = "character",   
                                          value = "numeric",
                                          visitato = "logical",
                                          distanza = "numeric"))



InsiemeNodi <- setRefClass("insiemeNodi", fields = list(listaNodi = "list"), methods = list(
  
  
  getNumeroNodi = function(){
    
    return(length(listaNodi))
  },
  
  
  # restituisce la lista dei nomi dei nodi
  
  getNomeNodi = function(){
    
      nameList <- list()
      for(v in listaNodi)
        nameList <- c(nameList, v$nome)
      
      return(nameList)
  },
  
  
  # restituisce il nodo corrispondente al nome inserito se esiste nell'insieme
  
  getNodo = function(nome){
    
    lista <- Filter({function (x) (x$nome == nome)}, listaNodi) # lista di tutti i nodi dell'insieme che hanno per nome "nome"
    if(length(lista) > 0)
      return(lista[[1]])
    
    return(NULL)
  },
  
  aggiungiNodo = function(nodo) {
    listaNodi <<- c(listaNodi, nodo)
  },
  
  rimuoviNodo = function(nodo) {
    listaNodi <<- Filter({function (x) (x$nome != nodo$nome)}, listaNodi)
  }
))



Arco <- setRefClass("arco", fields = list(sorgente = "nodo", 
                                          dest = "nodo", 
                                          peso = "numeric"))


InsiemeArchi <- setRefClass("insiemeArchi", fields = list(listaArchi = "list"), methods = list(
  
  getArco = function(nome1, nome2) {
    for(arco in listaArchi){
      if((arco$sorgente$nome == nome1 && arco$destinazione$nome == nome2) ||
         (arco$destinazione$nome == nome1 && arco$sorgente$nome == nome2)) {
        return(arco)
      }
    }
    
    return(NULL)
  },
  
  aggiungiArco = function(s, d, p) {
    listaArchi <<- c(listaNodi, Arco(sorgente = s, dest = d, peso = p))
  },
  
  rimuoviArco = function(s, d) {
    listaArchi <<- Filter(
      {function (x) (x$sorgente$nome != s$nome && x$dest$nome != d$nome)},
      listaArchi
    )
  },
  
  rimuoviArchi = function(nodo) {
    listaArchi <<- Filter(
      {function (x) (x$sorgente$nome != nodo$nome && x$dest$nome != nodo$nome)},
      listaArchi
    )
  }
))


# funzione che crea una matrice di adiscenza 

creaMatrice <- function(nodi, archi){

  m <- matrix(0, nrow = nodi$getNumeroNodi(), ncol = nodi$getNumeroNodi())
  
  colnames(m) <- nodi$getNomeNodi()
  rownames(m) <- nodi$getNomeNodi()
  
  for(arco in archi$listaArchi){
    m[arco$sorgente$nome, arco$dest$nome] <- arco$peso
    m[arco$dest$nome, arco$sorgente$nome] <- arco$peso
  }
  
  return(m)
}


#punto 1a

Graph <- setRefClass("graph", fields = list(matrice = "matrix", archi = "insiemeArchi", nodi = "insiemeNodi"), methods = list(
  
  
  cercaNodoPerNome = function(nome){
    return(!is.null(nodi$getNodo(nome)))
  },
  
  cercaNodo = function(x){
    return(cercaNodoPerNome(x$nome))
  },
  
  
  # punto 1a.1
  
  cercaArcoPerNome = function(nomeX, nomeY) {
    
    if(cercaNodoPerNome(nomeX) && cercaNodoPerNome(nomeY))
      if(matrice[nomeX, nomeY] > 0)
        return(TRUE)
    
    return(FALSE)
  },
  
  cercaArco = function(x, y){
    return(cercaArcoPerNome(x$nome, y$nome))
  },
  
  
  # punto 1a.2
  
  getAdiacenti = function(x){
    
    listaAdiacenti <- list();
    
    for(nodo in nodi$listaNodi)
      if(matrice[x$nome, nodo$nome] > 0)
        listaAdiacenti <- c(listaAdiacenti, nodo)
      
      return(listaAdiacenti)
  },
  
  
  # punto 1a.3
  
  aggiungiNodo = function(x) {
    
    if(!cercaNodo(x)) {
      
      nodi$aggiungiNodo(x)
      
      matrice <<- cbind(matrice, 0)
      matrice <<- rbind(matrice, 0)
      
      rownames(matrice)[nodi$getNumeroNodi()] <<- x$nome
      colnames(matrice)[nodi$getNumeroNodi()] <<- x$nome
    } 
    else
      print(paste("il nodo", x$nome, "esiste, pertanto non verrà aggiunto", sep = " "))
  },
  
  
  # punto 1a.4
  
  rimuoviNodo = function(x){
    
    if(cercaNodo(x)) {
      matrice <<- matrice[!rownames(matrice) %in% x$nome,
                          !colnames(matrice) %in% x$nome]
      
      nodi$rimuoviNodo(x)
      archi$rimuoviArchi(x)
    }
  },
  
  
  # punto 1a.5
  
  aggiungiArco = function(x, y) {
    
    if(!cercaArco(x, y)){
      matrice[x$nome, y$nome] <<- 1
      matrice[y$nome, x$nome] <<- 1
      archi$aggiungiArco(x, y, 1)
    }
  },
  
  
  # punto 1a.6
  
  rimuoviArco = function(x, y) {
    if(cercaArco(x, y)){
      matrice[x$nome, y$nome] <<- 0
      matrice[y$nome, x$nome] <<- 0
      archi$rimuoviArco(x, y)
    }
  },
  
  
  # punto 1a.7
  
  valoreNodo = function(nome){
    nodo = nodi$getNodo(nome)
    if(!is.null(nodo))
      return(nodo$value)
  },
  
  
  # punto 1a.8
  
  impostaValoreNodo = function(nome, valore){
    nodo = nodi$getNodo(nome)
    if(!is.null(nodo))
      nodo$value <- valore
  },
  
  
  # punto 1a.9
  
  valoreArco = function(nomeSorg, nomeDest) {
    if (cercaNodoPerNome(nomeSorg) && cercaNodoPerNome(nomeDest))
      return(matrice[nomeSorg, nomeDest])
    
    return(0)
  },
  
  
  # punto 1a.10
  
  impostaValoreArco = function(nomeSorg, nomeDest, peso){
    if(cercaArcoPerNome(nomeSorg, nomeDest)){
      matrice[nomeSorg, nomeDest] <<- peso
      matrice[nomeDest, nomeSorg] <<- peso
      a <- archi$getArco(nomeSor, nomeDest)
      a$peso <- peso
    }
  }
))




# punto 1.b : la lista di adiacenza è rappresentata con un dataframe

listaAdiacenza <- function(grafo){
  
  df <- data.frame(ncol = 2)
  
  for(arco in grafo$archi$listaArchi){
    
    # creo una nuova riga
    
    newRow <- data.frame(arco$sorgente$nome, arco$dest$nome)
    names(newRow) <- c("nodo1", "nodo2")
    
    
    #aggiungo la riga al dataframe
    
    df <- rbind(df, newRow)
  }
  
  return(df)
}


# punto 2 : il grafo viene mostrato in un plot utilizzando il pacchetto 'igraph'

library(igraph)

plotGrafo <- function(grafo) {
  plot(graph_from_adjacency_matrix(grafo$matrice, mode = "undirected", weighted = T))
}


# punto 3

visitaAmpiezza <- function(grafo, partenza) {
  q <- c(partenza)
  
  while(length(q) > 0){
    v <- q[[1]] # prendo il primo nodo nella lista e...
    q <- q[-1] # ... lo rimuovo
    v$visitato = T
    print(v$nome)
    for(adiacente in grafo$getAdiacenti(v)){
      if(!adiacente$visitato)
        q <- c(q, adiacente)
    }
  }
}


dijkstra <- function(grafo, partenza, arrivo) {
  partenza$distanza = 0;
  q <- grafo$nodi$listaNodi
  q[order(sapply(q, function (x) x$distanza), decreasing = F)]
  while(length(q) > 0){
    u <- q[[1]]
    q <- q[-1]
    for(v in grafo$getAdiacenti(u)){
      alt <- u$distanza + grafo$valoreArco(u$nome, v$nome)
      if(alt < v$distanza){
        v$distanza <- alt
        q[order(sapply(q, function (x) x$distanza), decreasing = F)]
      }
    }
  }
}


nodoA <- Nodo(nome = "A", value = 1, visitato = F, distanza = Inf);
nodoB <- Nodo(nome = "B", value = 2, visitato = F, distanza = Inf);
nodoC <- Nodo(nome = "C", value = 10, visitato = F, distanza = Inf);

arco1 <- Arco(sorgente = nodoA, dest = nodoB, peso = 3);
arco2 <- Arco(sorgente = nodoA, dest = nodoC, peso = 5);

archi <- InsiemeArchi(listaArchi = c(arco1, arco2))

nodi <- InsiemeNodi(listaNodi = c(nodoA, nodoB, nodoC))

matrice <- creaMatrice(nodi, archi)

grafo <- Graph(matrice = matrice, archi = archi, nodi = nodi)
