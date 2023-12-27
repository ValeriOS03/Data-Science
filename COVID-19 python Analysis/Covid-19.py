
# Importiamo le librerie necessarie per i calcoli richiesti
import requests
import json
import matplotlib.pyplot as plt
import sqlite3

#---------------------------------------------------------------------------------------------Comune--------------------------------------------------------------------------------------------------------

# Creiamo una lista con i nomi dei campi presenti nel dataset
campiDataset = ['ricoverati_con_sintomi', 'terapia_intensiva',
                 'totale_ospedalizzati', 'isolamento_domiciliare', 'totale_positivi',
                 'variazione_totale_positivi', 'nuovi_positivi', 'dimessi_guariti',
                 'deceduti', 'casi_da_sospetto_diagnostico','casi_da_screening',
                 'totale_casi', 'tamponi', 'casi_testati', 'ingressi_terapia_intensiva',
                 'totale_positivi_test_molecolare', 'totale_positivi_test_antigenico_rapido',
                 'tamponi_test_molecolare', 'tamponi_test_antigenico_rapido']


# Dichiariamo la funzione raccoltaDati che raccoglie i dati dalla pagina web e che ritorna in output il dataset che viene successivamente scritto su un file JSON
def raccoltaDati():
    urlBase = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-andamento-nazionale.json" #L'URL della pagina web dove sono contenuti i dati
    conn = requests.get(urlBase)
    return json.loads(conn.text)

# Scrittura in un file JSON dei dati provenienti dal dataset
f = open("datasetCovid.json", "w")
json.dump(raccoltaDati(), f)
f.close()

# Lettura dei dati dal file JSON datasetCovid.json
k = open("datasetCovid.json", "r")
datiCovid = json.load(k)
k.close()

# Definiamo le date di controllo, queste saranno la prima e l'ultima data disponibile nel dataset
dataControlloInizio = datiCovid[0]["data"][:10]
dataControlloFine = datiCovid[-1]["data"][:10]

# Definiamo la lista contenente le due date di controllo
dateControllo = [dataControlloInizio, dataControlloFine]

# Il messaggio che viene mostrato ad esecuzione completata del programma
fine = "Dati calcolati con successo"

# Il messaggio che viene mostrato in caso di esecuzione del programma terminata prima del previsto o altri imprevisti
secondaFine = "Fine"


#-----------------------------------------------------------------------------------------Media Mobile------------------------------------------------------------------------------------------------------

def mediaMobile(dataset, data_inizio, data_fine, size, osservabile):
    '''
    La funzione mediaMobile ritorna una lista di medie tra n elementi dove n corrisponde al parametro size che viene fornito dall'utente. Essa prende in input 5 parametri:
    1. Il dataset viene preso dal file JSON e caricato nella variabile datiCovid che a sua volta viene inserita per il parametro dataset
    2. La data d'inizio del periodo assegnato
    3. La data di fine del periodo assegnato
    4. Size indica il numero di elementi tra cui viene fatta la media. Questa viene calcolata ripetutamente spostandosi di un elemento ad ogni ciclo.
    5. L'osservabile di cui vogliamo consultare i valori
    '''

    # Lista di elementi osservati
    elem_oss = []

    # Lettura dei dati da datiCovid
    for i in datiCovid:
        data_agg = i["data"][:10]
        if i[osservabile] is not None and data_agg >= data_inizio and data_agg <= data_fine: # Controlliamo se la data è compresa nell'arco temporale assegnato
            elem_oss.append(i[osservabile])

    # Creazione lista contenente i risultati che verranno forniti in output
    result = []

    # Tramite un doppio ciclo for andiamo a calcolare le medie mobili e ad inserirle nell'apposita lista
    for i in range(0, len(elem_oss) - size):
        sum = 0
        for j in range(0, size-1):
            sum += elem_oss[i + j]
        result.append(sum / size)

    # Controlliamo se la lista è vuota e in tal caso viene mostrato un messaggio di avviso all'utente
    # Se invece sono presenti dei valori essi verranno mostrati in output
    if len(result)==0:
        print("Numero minimo valori per calcolare la media mobile non raggiunto")
        return secondaFine
    else:
        return result

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------Grafico osservabile----------------------------------------------------------------------------------------------

def grafico(dataset, data_inizio, data_fine, osservabile):
    '''
    La funzione grafico crea un grafico che rappresenta l'andamento dei valori di un osservabile assegnata compreso tra una data d'inizio e una di fine.
    Inoltre viene creato un database contenente i valori dell'osservabile nel periodo assegnato ottenuti in precedenza.
    '''

    # Creiamo due liste, nella prima vengono inseriti i valori degli osservabili compresi tra la data d'inizio e la data di fine del periodo assegnato
    # Nella seconda lista vengono inserite le date degli osservabili comprese tra la data d'inizio e la data di fine del periodo assegnato
    elem_oss = []
    data_lista = []

    # Viene letto ogni record del file JSON datiCovid e viene aggiornata la data riducendola ai primi 10 caratteri
    for i in datiCovid:
        data_agg = i["data"][:10]

        # Se la data letta è compresa tra la data d'inizio e la data di fine l'osservabile e la relativa data vengnono inseriti nelle rispettive liste
        if data_agg >= data_inizio and data_agg <= data_fine:
            elem_oss.append(i[osservabile])
            data_lista.append(data_agg)

    # Modifichiamo le date d'inizio, fine e il nome degli osservabili per una migliore leggibilità
    data_inizio = data_inizio.replace("-", "/")
    data_fine = data_fine.replace("-", "/")
    osservabile = osservabile.replace("_", " ")

    # Definizione delle caratteristiche del grafico
    plt.figure(figsize=(30, 15))
    plt.title(f"{osservabile} dal {data_inizio} al {data_fine}")
    plt.xticks(rotation=60)
    plt.plot(data_lista, elem_oss, color="red")
    plt.legend(["Andamento dell'osservabile"], loc="upper left", markerscale=0.7, scatterpoints=1, fontsize=20)
    plt.grid(axis='y')

    assex = plt.gca()  # Tramite il metodo gca() troviamo le posizioni esatte di ogni indice sull'asse X
    numeroIndiciOriginale = assex.xaxis.get_ticklocs()  # Tramite il metodo get_ticklocs() otteniamo le locazioni di ogni indice, indicando sempre che l'operazione andrà svolta sull'asse X
    numeroIndiciAsseX = 20  # Impostiamo un numero d'indici sull'asse X che consenta le leggibilità ottimale
    intervalli = len(numeroIndiciOriginale) // (numeroIndiciAsseX) # Eseguiamo una divisione parte intera (floor division), in cui otteniamo il risultato arrotondato per difetto all'intero più vicino
    nuoviIndici = numeroIndiciOriginale[::intervalli] # Andiamo a definire gli indici che verranno mostrati sul grafico
    assex.xaxis.set_ticks(nuoviIndici) # Impostiamo i nuovi indici che saranno presenti sul grafico

    plt.savefig(f"GraficoAnaliticaCovid.png", dpi=300) #Salviamo il grafico tramite la funzione savefig() e impostiamo una risoluzione d'immagine di 300 DPI
    plt.show()

    print("Grafico salvato con successo")
    print()

    # Creazione della lista di date ottimizzate per la lettura nel database che andremo a creare
    data_lista_agg = []

    # Inserimento delle date corrette nella lista apposita
    for i in data_lista:
        i = i.replace("-", "/")
        data_lista_agg.append(i)


    elem_sqlite = []

    for i in elem_oss:
        if i is None:
            i = "NULL"
            elem_sqlite.append(i)
        else:
            elem_sqlite.append(i)

    # Apportiamo delle modifiche alle date per renderle utilizzabili con i database
    data_inizio = data_inizio.replace("/", "_")
    data_fine = data_fine.replace("/", "_")
    osservabile = osservabile.replace(" ", "_")

    # Creiamo il nome della tabella che conterrà i valori osservati e le relative date
    nome_tabella = f"{osservabile}_dal_{data_inizio}_al_{data_fine}"

    # Creiamo il database e la tabella contenente i valori (Comando di tipo DDL - Data Definition Language)
    db = sqlite3.connect(f"{nome_tabella}.db")
    createTable = f"CREATE TABLE IF NOT EXISTS {nome_tabella} (data TEXT, {osservabile} REAL )"
    db.execute(createTable)

    # Confermiamo la scrittura sul disco dei dati ottenuti tramite le istruzioni precedenti
    db.commit()

    # Inserimento dei dati nella tabella tramite un ciclo for
    for i,j in zip(data_lista_agg, elem_sqlite):
        i = "'" + i + "'"
        insert = f"INSERT INTO {nome_tabella} (data, {osservabile}) VALUES ({i},{j})" # Query di inserimento dei dati nella tabella (DML - Data Manipulation Language)
        db.execute(insert) # Esecuzione della query
        db.commit() # Conferma e scrittura sul disco

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------Somma Cumulata--------------------------------------------------------------------------------------------------

# Creazione della Funzione Somma Cumulata
def sommatoria(dataset, data_inizio, data_fine, osservabile): #Fatto
# Usando il comando help(sommatoria) viene stampata la seguente guida sull'utilizzo della funzione.
    '''
        La funzione sommatoria prende in input 4 parametri:

        1 | dataset --> contiene le coppie chiave-valore, che vengono scaricate dal sito web e scritte sul file JSON.
        2 | data_inizio --> la data d'inizio da cui si vuole far partire la somma cumulata (N.B. in formato YYYY-MM-GG)
        3 | data_fine --> la data fino a cui vogliamo la somma cumulata (N.B. in formato YYYY-MM-GG)
        4 | osservabile --> il dato di cui si vogliono sapere le informazioni (ad esempio deceduti)

        Il programma ritornerà una lista di N elementi cumulati dell'osservabile scelta
        dal periodo che va dalla data d'inizio a quella di fine.

    '''
    data_lista = []
    result = []  # Lista contenente il risultato, che sarà il nostro output
    somma_cumulata = 0  # Facciamo partire la somma cumulata da 0, successivamente andremo ad "appendere" i valori
    osservabili_cumulate = ['dimessi_guariti', 'deceduti', 'tamponi', 'totale_casi', 'casi_testati',
                            'totale_positivi_test_molecolare', 'totale_positivi_test_antigenico_rapido',
                            'tamponi_test_molecolare', 'tamponi_test_antigenico_rapido'] # Creiamo una lista contenente le osservabili che sono già cumulate nel dataset
    for line in datiCovid:  # Ciclo for

        if line[osservabile] is not None and data_inizio <= line['data'][:10] <= data_fine: # Controlliamo se la data letta è compresa nel periodo assegnato
            if osservabile not in osservabili_cumulate:  # Controlliamo se l'osservabile non è nella lista di quelle già cumulate
                somma_cumulata += line[osservabile]  # Cumuliamo i valori dell'osservabile
                result.append(somma_cumulata)
                data_lista.append(line["data"][:10])

            elif osservabile in osservabili_cumulate:  #Controlliamo se l'osservabile è nella lista di quelle già cumulate
                somma_cumulata = line[osservabile]  # Aggiungiamo i valori dell'osservabile senza cumularli
                result.append(somma_cumulata)
                data_lista.append(line["data"][:10]) # Prendiamo fino al decimo carattere della data

    # Definizione delle caratteristiche del grafico
    plt.figure(figsize=(30, 15))
    plt.title(f"{osservabile} dal {data_inizio} al {data_fine}")
    plt.xticks(rotation = 60)
    plt.plot(data_lista, result, color="red")
    plt.legend(["Andamento dell'osservabile"], loc="upper left", markerscale=0.7, scatterpoints=1, fontsize=20)
    plt.grid(axis='y')

    assex = plt.gca()  # Tramite il metodo gca() troviamo le posizioni esatte di ogni indice sull'asse X
    numeroIndiciOriginale = assex.xaxis.get_ticklocs()  # Tramite il metodo get_ticklocs() otteniamo le locazioni di ogni indice, indicando sempre che l'operazione andrà svolta sull'asse X
    numeroIndiciAsseX = 20  # Impostiamo un numero d'indici sull'asse X che consenta le leggibilità ottimale
    intervalli = len(numeroIndiciOriginale) // (numeroIndiciAsseX) # Eseguiamo una divisione parte intera (floor division), in cui otteniamo il risultato arrotondato per difetto all'intero più vicino
    nuoviIndici = numeroIndiciOriginale[::intervalli] # Andiamo a definire gli indici che verranno mostrati sul grafico
    assex.xaxis.set_ticks(nuoviIndici) # Impostiamo i nuovi indici che saranno presenti sul grafico

    plt.savefig(f"GraficoSommaCumulata.png", dpi=300)  # Salviamo il grafico tramite la funzione savefig() e impostiamo una risoluzione d'immagine di 300 DPI
    plt.show()

    print("Grafico salvato con successo")
    print()

    return result  # Definiamo l'output della funzione

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------Massimo---------------------------------------------------------------------------------------------------------

# Dichiaro la funzione valoreMassimo
def valoreMassimo(dataset, dataInizio, dataFine, osservabile):
    '''
    La funzione valoreMassimo prende in input quattro parametri:
    1. dataset: i valori della pagina web vengono scaricati e scritti in un file JSON e in seguito vengono caricati in una variabile che viene data come primo parametro ovvero dataset
    2. dataInizio: la data d'inizio del periodo in cui si vuole ricercare un valore massimo, ad esempio: 2021-01-01
    3. dataFine: la data di fine del periodo in cui si vuole ricercare un valore massimo, ad esempio: 2022-01-01
    4. osservabile: il dato di cui vogliamo andare a trovare il massimo, ad esempio: tamponi

    Per utilizzare la seguente funzione inserire i valori richiesti e attendere il calcolo del massimo da parte del programma.
    Verrà restituito il valore massimo dell'osservabile richiesta nel periodo assegnato.

    In caso venga inserito un valore errato il programma ne richiederà l'inserimento di uno valido.
    '''

    # Raccogliamo soltanto i primi 10 caratteri della stringa ottenuta in input contenente la data
    dataInizio = dataInizio[:10]
    dataFine = dataFine[:10]

    # Due liste, la prima contenente i valori raccolti dell'osservabile richiesta, la seconda contiene date relative a ciascun osservabile
    listaOsservabili = []
    data = []

    # Utilizziamo un ciclo for per raccogliere solo i primi 10 caratteri della data di ciascun osservabile e se quest'ultimo risulta
    # Non null e con data inclusa all'interno del periodo richiesto viene scritto nella lista listaOsservabili mentre la sua data viene scritta nella lista data
    for i in dataset:
        dataAttuale = i["data"][:10] #Data dell'osservabile
        if i[osservabile] is not None and dataInizio <= dataAttuale and dataAttuale <= dataFine:
            listaOsservabili.append(float(i[osservabile]))
            data.append(dataAttuale)

    # Tramite il costrutto try except andiamo ad eseguire normalmente le istruzioni e nel caso la lista degli osservabili sia vuota
    # il programma restituirà un avviso e terminerà l'esecuzione del programma
    try:
        # Definiamo il massimo iniziale che verrà poi sostituito con il massimo reale, allo stesso tempo definiamo anche la data
        # del massimo che subirà lo stesso processo di sostituzione
        massimo = listaOsservabili[0]
        dataMassimo = data[0]

        # Tramite un ciclo for andiamo a trovare il massimo e la relativa data
        for k in range(len(listaOsservabili)):

            if listaOsservabili[k] > massimo:
                massimo = listaOsservabili[k]
                dataMassimo = data[k]

        print()
        print(f"Il massimo è: {massimo} e ha data: {dataMassimo}")  #Messaggio per l'utente con i dati richiesti
        print()

    except:
        # Se la lista degli osservati è vuota significa che i valori sono solo null e quindi non sono presenti valori registrati
        massimo = "Nessun valore registrato"
        print(massimo)

    if massimo == "Nessun valore registrato":
        return secondaFine
    else:
        return fine # Restituzione del messaggio di dati calcolati con successo

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------Minimo----------------------------------------------------------------------------------------------------------

# Dichiaro la funzione valoreMinimo
def valoreMinimo(dataset, dataInizio, dataFine, osservabile):
    '''
    La funzione valoreMinimo prende in input quattro parametri:
    1. dataset: i valori della pagina web vengono scaricati e scritti in un file JSON e in seguito vengono caricati in una variabile che viene data come primo parametro ovvero dataset
    2. dataInizio: la data d'inizio del periodo in cui si vuole ricercare un valore minimo, ad esempio: 2021-01-01
    3. dataFine: la data di fine del periodo in cui si vuole ricercare un valore minimo, ad esempio: 2022-01-01
    4. osservabile: il dato di cui vogliamo andare a trovare il minimo, ad esempio: tamponi

    Per utilizzare la seguente funzione inserire i valori richiesti e attendere il calcolo del minimo da parte del programma.
    Verrà restituito il valore minimo dell'osservabile richiesta nel periodo assegnato.

    In caso venga inserito un valore errato il programma ne richiederà l'inserimento di uno valido.
    '''

    # Raccogliamo soltanto i primi 10 caratteri della stringa ottenuta in input contenente la data
    dataInizio = dataInizio[:10]
    dataFine = dataFine[:10]

    # Due liste, la prima contenente i valori raccolti dell'osservabile richiesta, la seconda contiene date relative a ciascun osservabile
    listaOsservabili = []
    data = []

    # Utilizziamo un ciclo for per raccogliere solo i primi 10 caratteri della data di ciascun osservabile e se quest'ultimo risulta non null e con data inclusa all'interno del periodo richiesto viene scritto nella lista listaOsservabili mentre la sua data viene scritta nella lista data
    for i in dataset:
        dataAttuale = i["data"][:10] #Data dell'osservabile
        if i[osservabile] is not None and dataInizio <= dataAttuale and dataAttuale <= dataFine:
            listaOsservabili.append(float(i[osservabile]))
            data.append(dataAttuale)

    # Tramite il costrutto try except andiamo ad eseguire normalmente le istruzioni e nel caso la lista degli osservabili sia vuota il programma restituirà un avviso e terminerà l'esecuzione del programma
    try:
        # Definiamo il minimo iniziale che verrà poi sostituito con il minimo reale, allo stesso tempo definiamo anche la data del minimo che subirà lo stesso processo di sostituzione
        minimo = listaOsservabili[0]
        dataMinimo = data[0]

        # Tramite un ciclo for andiamo a trovare il minimo e la relativa data
        for k in range(len(listaOsservabili)):

            if listaOsservabili[k] < minimo:
                minimo = listaOsservabili[k]
                dataMinimo = data[k]

        print()
        print(f"Il minimo è: {minimo} e ha data: {dataMinimo}") # Messaggio per l'utente con i dati richiesti
        print()

        plt.figure(figsize=(30, 15)) # Impostiamo le dimensioni del grafico a 16 pollici di larghezza e 9 di altezza
        plt.title(f"Grafico dell'osservabile {osservabile} dal giorno {dataInizio} al giorno {dataFine}", fontsize=18) # Impostiamo il titolo del grafico tramite una stringa formattata e dichiariamo la grandezza del carattere a 20
        plt.xticks(rotation=60) # Impostiamo l'angolo d'inclinazione degli indici sull'asse X
        plt.xlabel("Date", fontsize=15) # Impostiamo il sottotitolo per l'asse X
        plt.ylabel("Valori dell'osservabile", fontsize=15) # Impostiamo il sottotitolo per l'asse Y
        plt.plot(data, listaOsservabili, label="Andamento dell'osservabile", color="red") # Dichiariamo i parametri e le caratteristiche della linea che andrà a comporre parte del grafico
        plt.scatter(dataMinimo, minimo, label="Minimo", c="yellow", linewidths=2, marker="^", edgecolor="red", s=200)  # Dichiariamo i parametri e le caratteristiche del punto che andrà ad evidenziare il minimo sulla linea del grafico, "c" è il colore, mentre "s" è la grandezza del punto marcato, linewidth

        # Per una lettura ottimale della legenda andiamo a specificare i parametri: loc (la posizione della legenda nel grafico), handlelegth (la lunghezza della legenda), handleheight (la larghezza), labelspacing (lo spazio tra ogni scritta sulla legenda),
        # borderpad (la distanza tra il testo e il bordo della legenda), fontsize (la grandezza del testo), prop (le proprietà del testo, in questo caso definiamo un normale grassetto)
        plt.legend(loc="upper left", handlelength=5, handleheight=5, labelspacing=0.75, borderpad=0.5, fontsize=18, prop={"weight":"bold"}) # Impostiamo la legenda del grafico

        plt.grid(axis='y') # Impostiamo la griglia orizzontale sul grafico

        assex = plt.gca()  # Tramite il metodo gca() troviamo le posizioni esatte di ogni indice sull'asse X
        numeroIndiciOriginale = assex.xaxis.get_ticklocs()  # Tramite il metodo get_ticklocs() otteniamo le locazioni di ogni indice, indicando sempre che l'operazione andrà svolta sull'asse X
        numeroIndiciAsseX = 20  # Impostiamo un numero d'indici sull'asse X che consenta le leggibilità ottimale
        intervalli = len(numeroIndiciOriginale) // numeroIndiciAsseX  # Eseguiamo una divisione parte intera (floor division), in cui otteniamo il risultato arrotondato per difetto all'intero più vicino
        nuoviIndici = numeroIndiciOriginale[::intervalli]  # Andiamo a definire gli indici che verranno mostrati sul grafico
        assex.xaxis.set_ticks(nuoviIndici)  # Impostiamo i nuovi indici che saranno presenti sul grafico

        plt.savefig(f"GraficoMinimo{osservabile}Dal{dataInizio}Al{dataFine}.png", dpi=300)  # Salviamo il grafico tramite la funzione savefig() e impostiamo una risoluzione d'immagine di 300 DPI
        plt.show()

        print("Grafico salvato con successo")
        print()

    except:
        # Se la lista degli osservati è vuota significa che i valori sono solo null e quindi non sono presenti valori registrati
        minimo = "Nessun valore registrato"
        print(minimo)
        print()

    if minimo == "Nessun valore registrato":
        return secondaFine
    else:
        return fine # Restituzione del messaggio di dati calcolati con successo

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------------------Menù----------------------------------------------------------------------------------------------

def menu(funzioneDaEseguire):


    messaggioIntroduzione = """Inserire un osservabile tra i seguenti:
    - ricoverati_con_sintomi
    - terapia_intensiva
    - totale_ospedalizzati
    - isolamento_domiciliare
    - totale_positivi
    - variazione_totale_positivi
    - nuovi_positivi
    - dimessi_guariti
    - deceduti
    - casi_da_sospetto_diagnostico
    - casi_da_screening
    - totale_casi
    - tamponi
    - casi_testati
    - ingressi_terapia_intensiva
    - totale_positivi_test_molecolare
    - totale_positivi_test_antigenico_rapido
    - tamponi_test_molecolare
    - tamponi_test_antigenico_rapido
    """

    # Stampa a video del messaggio introduttivo al programma
    print(messaggioIntroduzione)
    print()

    osservabileDesiderato = input(
        "Inserire l'osservabile desiderato: ")  # Inserimento in input da parte dell'utente dell'osservabile desiderato

    # Verifichiamo che l'osservabile inserito sia uno dei campi presenti nel dataset
    while (osservabileDesiderato not in campiDataset):
        print(
            "L'osservabile inserito non è presente nel dataset, inserirne uno valido")  # Messaggio di avviso per l'utente per l'inserimento di un osservabile non presente nel dataset
        osservabileDesiderato = input("Inserire l'osservabile desiderato: ")

    dataInizioDesiderata = input(f"Inserire la data di inizio desiderata nel seguente formato (YYYY-MM-DD), la prima data disponibile è il {dataControlloInizio} : ")
    dataFineDesiderata = input(f"Inserire la data di fine desiderata nel seguente formato (YYYY-MM-DD), l'ultima data disponibile è il {dataControlloFine} : ")

    # Inserimento delle date d'inizio e di fine del periodo
    # Tramite dei cicli while controlliamo se le date inserite sono corrette e presenti del dataset

    while not (dataInizioDesiderata >= dateControllo[0] and dataInizioDesiderata <= dateControllo[1]):
        print()
        print(f"Inserire una data d'inizio valida. La prima data disponibile è il {dataControlloInizio}")
        dataInizioDesiderata = input("Inserire la data di inizio desiderata nel seguente formato (YYYY-MM-DD): ")

    print()

    while not (dataFineDesiderata >= dateControllo[0] and dataFineDesiderata <= dateControllo[1]):
        print(f"Inserire una data di fine valida. L'ultima data disponibile è il {dataControlloFine}")
        dataFineDesiderata = input("Inserire la data di fine desiderata nel seguente formato (YYYY-MM-DD): ")
        print()


    if funzioneDaEseguire == "1":

        sizeMediaMobile = int(input("Inserire il numero di blocchi da cui sarà composta la media mobile: "))

        while not(sizeMediaMobile > 0 and not None):
            sizeMediaMobile = int(input("Valore blocchi errato, inserirne uno valido: "))

        # Stampa del risultato dell'esecuzione della funzione fornendo in input i parametri richiesti
        print(mediaMobile(datiCovid, dataInizioDesiderata, dataFineDesiderata, (sizeMediaMobile), osservabileDesiderato))

    elif funzioneDaEseguire == "2":
        grafico(datiCovid, dataInizioDesiderata, dataFineDesiderata, osservabileDesiderato)

    elif funzioneDaEseguire == "3":
        print(sommatoria(datiCovid, dataInizioDesiderata, dataFineDesiderata, osservabileDesiderato))

    elif funzioneDaEseguire == "4":
        # Stampa del risultato dell'esecuzione della funzione fornendo in input i parametri richiesti
        print(valoreMassimo(datiCovid, dataInizioDesiderata, dataFineDesiderata, osservabileDesiderato))

    elif funzioneDaEseguire == "5":
        # Stampa del risultato dell'esecuzione della funzione fornendo in input i parametri richiesti
        print(valoreMinimo(datiCovid, dataInizioDesiderata, dataFineDesiderata, osservabileDesiderato))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


cont = 1 # Codice di controllo per il ritorno al menù principale
listaFunzioni = ["1","2","3","4","5"] # Lista di controllo per definire quali sono gli unici valori corretti assegnabili alla variabile funzione

while(cont==1):

    print()
    print("""Menu funzioni:
        1. Media mobile per settimana di un osservabile in un periodo assegnato
        2. Grafico di un osservabile in un periodo assegnato
        3. Somma cumulata di un osservabile in periodo assegnato
        4. Massimo di un osservabile in un periodo assegnato
        5. Minimo di un osservabile in un periodo assegnato
        """)

    funzione = input("Inserire la funzione da eseguire: ")

    while(funzione not in listaFunzioni):
        funzione = input("Funzione non trovata, inserire una valida tra quelle presenti nel menù: ")

    menu(funzione)

    destinoProgramma = input("Per tornare al menù principale inserisci 1, altrimenti per terminare il programma inserisci 0: ")

    if (destinoProgramma == "0"):
        cont = 0
        print(secondaFine)
    elif(destinoProgramma != "0" and destinoProgramma != "1"):
        cont = 0
        print(secondaFine)























