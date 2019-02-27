#Randomly distributed population of plants is visited by pollinator, which
#choose of next plant/flower depends on distance to the next plant. 
#Revisitation of plants is possible, but restricted  by the position of 
#the revisited plant in foraging sequence.  

#Sorry, code is mostly written in Czech




#Necht existuje hypoteticka populace rostlin s hypotetickou strukturou a hypoteticky opylovac s 
#hypotetickymi rozhodovacimi vlastnostmi 
#Necht se tento opylovac pohybuje v populaci od rostliny k rostline na z?klade sveho rozhodnuti 
#Necht je vystupem graf pohybu opylovace zvyrazneny cervenou lini? 

pollinator <- function(revisitation_rate,maximal_number_of_visits,array_size,number_of_flowers){
      
  #vygeneruje dataset s nahodne umistenymi hodnotami (1 nebo 0) ve sloupci "pocet" 
  # - tedy udela to nahodne mista v array s "rostlinami"
  #vytvori vektor souradnic x o hrane velikost areny
   
  x <- rep(1:array_size,each=array_size)
  
  #vytvori vektor souradnic y o hrane velikost areny
  y <- rep(seq(1,array_size,1),times=array_size)
  
  #vytvor vektor pocet s nenahodnou velikosti, nenahodnym poctem rostlin, ale nahodny vyskytem   
  pocet <- sample(x=c(rep(1,
                      times=number_of_flowers),
                      rep(0,(array_size*array_size)-number_of_flowers)),
                      array_size*array_size, replace = FALSE)

  #vytvori vektor id pro kazde misto v arene
  id <-seq(1:(array_size*array_size))
  
  #slozi dohromady dataframe se vsemi udaji
  datakytky <- data.frame(id=c(id),
                          x=c(x),
                          y=c(y),
                          pocet=c(pocet))
  
  #vybere z puvodniho datasetu subdataset, kde je hodnota v sloupci "pocet" ==1 
   #(tedy jen ty mista kde rostliny jsou)
  kdekytkaje <- datakytky[datakytky$pocet!=0,]
   
  #nahodne vybere jeden radek ze sloupce "pocet" (tedy 1. rostlinu na kterou opylovac prileti)
  prvnirostlina <- kdekytkaje[kdekytkaje$id==sample(kdekytkaje$id,size=1),]
  prvnirostlina <- data.frame(id=prvnirostlina$id,
                              x=prvnirostlina$x,
                              y=prvnirostlina$y,
                              pocet=prvnirostlina$pocet,
                              tamnechci=0,
                              kdeuzbyl=0,
                              vzdalenost=0)
  #vytvori graf array
  par(mfrow=c(1,2))
  plot(datakytky$x,datakytky$y, cex=datakytky$pocet, xlab = "x", ylab = "y") 

  #v array obarvi 1. nahodne vybranou navstivenou rostlinu na cerveno
  points(y=prvnirostlina$y,x=prvnirostlina$x,col="red",cex=prvnirostlina$pocet, pch=19)
    
  #vytvori novy sloupec "kdeuzbyl" 
  kdeuzbyl <-rep(x=0,length.out=length(kdekytkaje$id))
    
  #vytvori novy dataset z kdekytkaje a prida do nej sloupec "kdeuzbyl"
  kytkybezprvni <- data.frame(kdeuzbyl=kdeuzbyl,
                              id=kdekytkaje$id,
                              x=kdekytkaje$x,
                              y=kdekytkaje$y,
                              pocet=kdekytkaje$pocet)

    
  #do kytkybezprvni prida do radku prvnirostlina do sloupce "kdeuzbyl" hodnotu 10 
  #- protoze tam opylovac uz byl 
  kytkybezprvni[kytkybezprvni$id==prvnirostlina$id,"kdeuzbyl"]<-revisitation_rate
   
   #ted musi vybrat druhou rostlinu na zaklade vzdalenosti, jestli na ni jiz byl a popripade na zaklade 
    #dalsich promenych
   
   #vytvori dataframe, kde budou odecteny souradnice od "kytkybezprvni" z "prvnirostlina" 
  druhakytka <- data.frame(id=kytkybezprvni$id,
                           x=(kytkybezprvni$x-prvnirostlina$x),
                           y=(kytkybezprvni$y-prvnirostlina$y),
                           pocet=kytkybezprvni$pocet)

  #vypocita vzdalenost v absolutni hodnote od "prvnirostlina"     
  vzdalenost <- data.frame(vzdalenost = (sqrt((druhakytka$x)^2+druhakytka$y^2)),
                           kdeuzbyl=(kytkybezprvni$kdeuzbyl),
                           id=kytkybezprvni$id,
                           x=kytkybezprvni$x,
                           y=kytkybezprvni$y,
                           pocet=kytkybezprvni$pocet)
    
    
  #vytvori sloupec "tamnechci" kam secte "vzdalenost" a "kdeuzbyl" popripade zohledni jine parametry
  tamnechci <- data.frame(id=kytkybezprvni$id,
                          x=kytkybezprvni$x,
                          y=kytkybezprvni$y,
                          pocet=kytkybezprvni$pocet,
                          tamnechci=((vzdalenost$vzdalenost)+(vzdalenost$kdeuzbyl))/
                            (vzdalenost$pocet),
                          kdeuzbyl=(kytkybezprvni$kdeuzbyl),
                          vzdalenost = (sqrt(druhakytka$x^2+druhakytka$y^2)))
  #seradi "tamnechci" podle sloupce "tamnechci" 
  serad <- tamnechci[order(tamnechci$tamnechci),]
    
  #ze "serad" vybere 2.kytku - rostlinu, ktera ma nejmensi hodnotu tam nechci 
  druhakytkaaa <- serad[1,]
    
  #vytvori linii od 1. k 2. rostline na zaklade vyberu opylovace
  lines(x=c(prvnirostlina$x,druhakytkaaa$x),
        y=c(prvnirostlina$y,druhakytkaaa$y),
        col="red") 
  #obarvi 2.rostlinu na cerveno
  points(y=druhakytkaaa$y,x=druhakytkaaa$x,col="red", pch=19)
  tamnechcidalsi <- tamnechci
  x <- 1
 #vytvori taulku vysledku, 2 odecita, protoze prvni a druha rostlina se pridava pred
  vysledkovka <- data.frame(id = rep(NA,maximal_number_of_visits-2),
                            x = rep(NA,maximal_number_of_visits-2),
                            y = rep(NA,maximal_number_of_visits-2),
                            pocet= rep(NA,maximal_number_of_visits-2),
                            tamnechci = rep(NA,maximal_number_of_visits-2),
                            kdeuzbyl = rep(NA,maximal_number_of_visits-2),
                            vzdalenost = rep(NA,maximal_number_of_visits-2))
 #odsud necht se to opakuje
 repeat{
      predchoziserad <-tamnechcidalsi[order(tamnechcidalsi$tamnechci),]
      predchozi <- predchoziserad[sample(1),]
      kytkybezpredchozi <- predchoziserad
      kytkybezpredchozi[kytkybezpredchozi$id==predchozi$id,
                        "kdeuzbyl"]<-kytkybezpredchozi[kytkybezpredchozi$id==predchozi$id,
                        "kdeuzbyl"]+revisitation_rate
      
      #vypocita vzdalenosti k dalsim rostlinam
      vzdalenostdalsi <- data.frame(vzdalenost = sqrt((kytkybezpredchozi$x-predchozi$x)^2
                                                      +(kytkybezpredchozi$y-predchozi$y)^2),
                                    kdeuzbyl=(kytkybezpredchozi$kdeuzbyl),
                                    id=kytkybezpredchozi$id,
                                    x=kytkybezpredchozi$x,
                                    y=kytkybezpredchozi$y,
                                    pocet=kytkybezpredchozi$pocet)
      
       #dopocita tamnechci index
      tamnechcidalsi <- data.frame(id=vzdalenostdalsi$id,
                                   x=vzdalenostdalsi$x,
                                   y=vzdalenostdalsi$y,
                                   pocet=vzdalenostdalsi$pocet,
                                   tamnechci=((vzdalenostdalsi$vzdalenost)+
                                                (vzdalenostdalsi$kdeuzbyl))/
                                   (vzdalenostdalsi$pocet),
                                   kdeuzbyl=(vzdalenostdalsi$kdeuzbyl),
                                   vzdalenost = vzdalenostdalsi$vzdalenost) 
      
      #seradi rostliny dle jejich nepritazlivosti pro opylovace
      seraddalsi <- tamnechcidalsi[order(tamnechcidalsi$tamnechci),]
     
      #po serazeni vybere dalsi rostlinu na kterou poleti opylovac
      dalsi <- seraddalsi[1,]
      
      #spoji rosltiny, ktere byli navstiveny cervenou linii
      lines(x=c(predchozi$x,dalsi$x),y=c(predchozi$y,dalsi$y),col="red") 
      
      #obarvi bod na ktery opylovac prileti
      points(y=dalsi$y,x=dalsi$x,cex=dalsi$pocet,col="red", pch=19)
      
      #zacne plnit tabulku vysledku
      vysledkovka[x,]<-c(dalsi$id,
                           dalsi$x,
                           dalsi$y,
                           dalsi$pocet,
                           dalsi$tamnechci,
                           dalsi$kdeuzbyl,
                           dalsi$vzdalenost)
      
      #ukonci repeat pote co se opylovac proleti
      x = x+1
      if (x == maximal_number_of_visits-1){
        break
      }
 }
  #vytvori tabulku vysledku
  vysledkovka1 <- rbind(prvnirostlina,druhakytkaaa,vysledkovka)
  print(vysledkovka1)
  #vytvori histogram vzdalenosti na ktere opylovaci letaji
   hist(vysledkovka1$vzdalenost,
       col = "black",
       main="Histogram of flight distance",
       xlab = "flight distance")
  }
      
#function

  pollinator(maximal_number_of_visits=40,revisitation_rate=12,array_size=80,number_of_flowers=70)  


#the end
  