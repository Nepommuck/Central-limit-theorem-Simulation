# Paweł Konopka
# Symulacja - Centralne twierdzenie graniczne


# STAŁE SYMULACJI
# ---------------

# DISTRIBUTION_TYPE określa z jakiego rozkładu pochodzą nasze zmienne losowe.
# Aby skorzystać z rozkładu najłatwiej jest odkomentowywać daną linię.

DISTRIBUTION_TYPE = "uniform"       # Rozkład jednostajny ciągły
# DISTRIBUTION_TYPE = "triangle"      # Rozkład z funkcji gęstości y=ax
# DISTRIBUTION_TYPE = "triangles"     # Rozkład w kształcie trzech trójkątów
# DISTRIBUTION_TYPE = "pareto"        # 20% wartości otrzymamy na 80% 
# DISTRIBUTION_TYPE = "double-normal" # Połączenie dwóch rozkładów normalnych
# DISTRIBUTION_TYPE = "towers"        # Kilka rozkładów jednostajnych
# DISTRIBUTION_TYPE = "interference"  # Obraz interferencyjny fali światła :O


MIN_VAL = 0              # Wartości minimalne i maksymalne zmiennej losowej
MAX_VAL = 1
NUMBER_OF_DATA = 10*1000 # Liczba zmiennych pochodząca z rozkładu
ELEMENTS_IN_SAMPLE = 15  # Liczność każdej próbki, z której liczymy średnią

ITERATIONS = 8000   # Liczba iteracji - im więcej tym lepiej i dłużej
# ITERATIONS = Inf  # Ustawiając tę stałą na Inf możemy oglądać na żywo

FPS = 4  # Maksymalna liczba klatek na sekundę (tylko dla ITERATIONS = Inf)  
NUMBER_OF_BARS = 100 # Liczba słupków w wykresach
VERTICAL = TRUE # Stała boolowska określająca czy chcemy pionowy/poziomy ekran


# FUNKCJE I PROCEDURY
# -------------------

# Funkcja zwraca współrzędną punktu określonego jako część 
# dystansu pomiędzy MIN_VAL, a MAX_VAL 
percToVal = function(fraction) {
  return (MIN_VAL + fraction * (MAX_VAL - MIN_VAL))
}

# Funkcja zwraca słupki do wykresów
getLinearBreakpoints = function(barNumber, minVal, maxVal) {
  result = numeric()
  for (i in 0:barNumber)
    result = append(
      result,
      minVal + i * (maxVal - minVal) / barNumber
    )
  return (result)
}

# Funkcja tworzy rozkład "triangle"
getTriangleDistribution = function(n, minVal, maxVal) {
  result = numeric()
  for (i in 1:n) {
    point = runif(2, minVal, maxVal)
    result = append(result, max(point))
  }
  return (result)
}

# Procedura rysująca histogramy
drawHistograms = function(distribution, sample, averages, breakpoints, 
                          drawSample=TRUE) {
  hist(distribution, breakpoints, col="lightblue")
  if (drawSample)
    hist(sample, breakpoints)
  hist(averages, breakpoints, col="lightgreen")
}


# PROCEDURA MAIN
# --------------

main = function() {
  # Tworzenie wybranej dystrybucji
  distribution = switch(DISTRIBUTION_TYPE,
                        
    "uniform" = runif(NUMBER_OF_DATA, MIN_VAL, MAX_VAL),
    
    "triangle" = getTriangleDistribution(NUMBER_OF_DATA, MIN_VAL, MAX_VAL),
    
    "triangles" = c(
      getTriangleDistribution(
        NUMBER_OF_DATA * 0.3, MIN_VAL, percToVal(0.1)),
      getTriangleDistribution(
        NUMBER_OF_DATA * 0.6, percToVal(0.1), percToVal(0.7)),
      getTriangleDistribution(
        NUMBER_OF_DATA * 0.1, percToVal(0.7), MAX_VAL)
    ),
    "pareto" = c(
      runif(NUMBER_OF_DATA * 0.8, MIN_VAL, percToVal(0.2)),
      runif(NUMBER_OF_DATA * 0.2, percToVal(0.2), MAX_VAL)
    ),
    "double-normal" = c(
      rnorm(NUMBER_OF_DATA/3, percToVal(1/3), 0.07 * (MAX_VAL-MIN_VAL)),
      rnorm(NUMBER_OF_DATA * (2/3), percToVal(2/3), 0.07 * (MAX_VAL-MIN_VAL))
    ),
    "towers"= c(
      runif(NUMBER_OF_DATA * 0.3, percToVal(0.1), percToVal(0.3)),
      runif(NUMBER_OF_DATA * 0.3, percToVal(0.3), percToVal(0.4)),
      runif(NUMBER_OF_DATA * 0.1, percToVal(0.4), percToVal(0.75)),
      runif(NUMBER_OF_DATA * 0.2, percToVal(0.8), percToVal(0.9)),
      runif(NUMBER_OF_DATA * 0.1, percToVal(0.9), MAX_VAL)
    ),
    "interference"= c(
      runif(NUMBER_OF_DATA * 0.5, MIN_VAL, percToVal(0.15)),
      runif(NUMBER_OF_DATA * 0.5, percToVal(0.85), MAX_VAL)
    )
  ) 
  if (is.null(distribution))
    stop("Distribution type not found")
  
  averages = numeric()
  breakpoints = getLinearBreakpoints(NUMBER_OF_BARS, MIN_VAL, MAX_VAL)
  
  # Ustawienie jednocześnie wyświetlanych wykresów na 2 (lub 3 gdy 
  # ITERATIONS = Inf) pionowo lub poziomo zależnie od wyboru.
  numberOfPlots = (if(is.finite(ITERATIONS)) 2 else 3)
  par(mfrow = if(VERTICAL) c(numberOfPlots, 1) else c(1, numberOfPlots))
  
  progressGoal = 0
  i = 0
  while (i < ITERATIONS) {
    # Wygenerowanie próbki z danych, obliczenie średniej oraz dołożenie 
    # do listy zapisanych próbek
    sample = sample(distribution, ELEMENTS_IN_SAMPLE)
    avg = mean(sample)
    averages = append(averages, avg)

    # Dla nieskończonej symulacji rysujemy histogramy oraz wstrzymujemy 
    # wykonanie programu na moment
    if (is.infinite(ITERATIONS)) {
      drawHistograms(distribution, sample, averages, breakpoints, TRUE)
      Sys.sleep(1 / FPS)
    }
    
    # Dla skończonej symulacji co 10% wypisujemy informację o postępach 
    # i rysujemy histogramy
    else if (i >= progressGoal) {
      progressGoal = progressGoal + 0.1 * ITERATIONS
      cat(round(i / ITERATIONS * 100), "%  DONE\n")
      
      drawHistograms(distribution, sample, averages, breakpoints, FALSE)
    }
    
    i = i+1
  }
  
  cat("FINISHED!\n")
  drawHistograms(distribution, sample, averages, breakpoints, !is.finite(ITERATIONS))
}


main()
