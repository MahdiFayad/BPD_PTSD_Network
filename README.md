# Identifying Bridge Symptoms between Borderline Personality Disorder and Post-Traumatic Stress Disorder: A Network Analysis of a National Cohort

This repository contains the data management and analysis code for the research article titled "Identifying Bridge Symptoms between Borderline Personality Disorder and Post-Traumatic Stress Disorder: A Network Analysis of a National Cohort". 

Our study aims to explore the interconnectedness of symptoms between Borderline Personality Disorder (BPD) and Post-Traumatic Stress Disorder (PTSD) using network analysis techniques on a large national cohort dataset (NESARC, National Epidemiological Survey on Alcohol and Related Conditions).

## Repository Contents

- `bpd_ptsd_JCP.ipynb`: A Jupyter notebook that details the data management process, including data cleaning, transformation, and preparation steps necessary for the subsequent network analysis.
- `bpd_network.R`: An R script that performs the network analysis for the Borderline Personality Disorder network, including the estimation of network structure, centrality indices, and bootstraps (non-parametric and case-drop bootstraps).
- `ptsd_network.R`: An R script that performs the network analysis for the Post-Traumatic Stress Disorder network, including the estimation of network structure, centrality indices, and bootstraps (non-parametric and case-drop bootstraps).
- `bpd_ptsd_network.R`: An R script that performs the network analysis for the BPD/PTSD common network, including the estimation of network structure, bridge centrality indices, and bootstraps (non-parametric and case-drop bootstraps).
- `sensitivity_analysis.R`: An R script that performs the sensitivity analysis of the BPD/PTSD network (i.e., only including subjects who met the primary criterion for PTSD, with no skip-structure imputation), with network structure and bridge centrality indices. 

## Usage

To run the Jupyter notebook, you will need to have a Python environment with necessary libraries installed (e.g., pandas, numpy, scipy). The R script requires R to be installed on your machine, along with the packages used in the script (e.g., qgraph, bootnet).

1. Clone the repository or download the files.
2. Open the Jupyter notebook in an environment where Python is installed and execute the cells sequentially.
3. Run the R script in an R environment, or use an IDE such as RStudio for an interactive session.

## Data

Due to privacy and ethical considerations, the dataset used for this analysis is not publicly available within this repository. The code is provided for transparency and reproducibility purposes for those who have authorized access to the data.


## License

This project is licensed under the terms of the [MIT license](LICENSE).

## Contact

For any queries related to the code or the research, please open an issue in this repository or contact the corresponding author at mahdi.fayad@aphp.fr.
