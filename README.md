# VD-PAC2 — Data Visualization Project

This repository contains the code and datasets used for **PAC2** of the *Visualització de Dades* course.  
The goal is to explore, clean, and visualize different types of data using **R**.

---

## 📁 Main scripts

| File | Description |
|------|--------------|
| **waterfallPlot.R** | Loads and cleans housing price index data, then creates a waterfall chart using *ggplot2* and *waterfalls*. |
| **raincloudPlot.R** | Visualizes simulated distributions (normal, bimodal, asymmetric) as raincloud plots. |
| **sankeyPlot.R** | Builds a Sankey diagram of European flight routes from OpenFlights data. |

---

## 📦 Dependencies

Make sure you have these R packages installed:

tidyverse  
waterfalls  
ggplot2  
ggdist  
readr  
networkD3  

You can install them all at once with:

```
install.packages(c("tidyverse", "waterfalls", "ggplot2", "ggdist", "readr", "networkD3"))
```

---

## 📈 Data sources

- **Housing Price Index:** dataset from [INE / Eurostat](https://www.ine.es/consul/serie.do?d=true&s=IPV949).  
- **Flight routes:** data from [OpenFlights.org](https://openflights.org/data).  
- **Simulated distributions:** generated using R random functions (rnorm, runif, etc.).

---

## 🧑‍💻 Author

**Laura Figueras**  
Visualització de Dades — UOC, 2025

---

## 📝 License

This project is open-source and available under the **MIT License**.
