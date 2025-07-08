# Installation & Setup Guide

This repository ships a **Shiny** application (`App/`) that depends on a small
set of CRAN/Bioconductor packages and confidential data placed in `Data/`.  
Follow these concise steps to get it running on any fresh machine.

---

## 1. Create an isolated R environment with mamba

```bash
# 1‑a. Create environment with recent R build (example: 4.5)
mamba create -n cgxb r-base=4.5 r-essentials

# 1‑b. Activate it whenever you work on this project
mamba activate cgxb
```

---

## 2. Clone (or unpack) this repository

```bash
git clone https://github.com/HRYKKG/Callitriche_GXB.git
cd Callitriche_GXB
```

Expected layout:

```
Callitriche_gxb/
├── App/          # app.R, server_msa.R, ui files, etc.
├── Data/         # **empty** for now – see Step 3
├── Prep.R     # package installer script
├── README.md  # This file
└── ...
```

---

## 3. Retrieve the confidential data

`Data/` is **not** version‑controlled. Download it from the secure location
provided by the project lead and extract it into `Data/`, e.g.:

```bash
# placeholder – replace with actual URL or instructions
wget -O Data/data_secure.zip "https://example.org/secure/download?id=XXXXX"
unzip Data/data_secure.zip -d Data
```

---

## 4. Install R / Bioconductor packages

With the `cgxb` environment activated:

```bash
Rscript Prep.R
```

The script installs any missing CRAN packages (from <https://cloud.r-project.org>)
and Bioconductor packages via `BiocManager`.  
You should see **“All packages loaded successfully.”** when done.

---

## 5. Launch the Shiny app (command‑line)

```bash
Rscript -e "shiny::runApp('App', launch.browser = TRUE)"
```

This starts the app on a local port and opens it in your default browser.

---

## 6. Troubleshooting & Updating

| Symptom | Remedy |
|---------|--------|
| *Package compile errors* | Ensure build tools & system libs (`make`, `g++`, `libcurl`, `openssl`, etc.) are installed. |
| *CRAN mirror error* | Run `options(repos = c(CRAN='https://cloud.r-project.org'))` before `Prep.R`. |
| *Missing data files* | Confirm files exist under `Data/` and have correct permissions. |

To update dependencies after pulling new code, simply re‑run:

```bash
Rscript Prep.R
```

---

### Optional: Using RStudio

RStudio is **not required** to run the app.  
If you prefer its IDE features, install it separately and open `App/app.R`, then
click **“Run App”**—all other steps remain the same.

---

*Last updated: 2025‑07‑08*
