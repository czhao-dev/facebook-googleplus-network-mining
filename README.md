# Facebook and Google+ Network Analysis

This project, developed as part of UCLA EE232E - *Graphs and Network Flows*, explores real-world social network data (Facebook and Google+) using various graph-theoretic and statistical techniques to analyze structure, detect communities, and evaluate social metrics such as embeddedness and dispersion.


## Project Overview

### Part 1: Facebook Network Analysis

Utilizing a dataset of 4,039 nodes and 88,234 edges from Facebook, the following analyses were conducted:

1. **Network Connectivity & Degree Analysis**:
   - Confirmed network is connected with diameter = 8.
   - Degree distribution fits best to an exponential model (MSE = 8.19).

2. **Personal Network of Node 1**:
   - Node 1 has 347 neighbors.
   - Personal subgraph: 348 nodes, 2,866 edges.

3. **Core Nodes & Community Detection**:
   - Identified 40 core nodes (degree > 200).
   - Average degree: 279.38.
   - Community detection on node 1's personal network using:
     - Fast-Greedy
     - Edge-Betweenness
     - Infomap

4. **Impact of Core Node Removal**:
   - Increased number of communities.
   - More singleton/isolated nodes.

5. **Embeddedness and Dispersion Metrics**:
   - Calculated for each core node's personal network.
   - Identified nodes with maximum:
     - Embeddedness
     - Dispersion
     - Dispersion/Embeddedness ratio
   - Visualized networks showing community structure and key nodes.

6. **Recurring Community Types Across Networks**:
   - Developed statistical features:
     - Size, Density, Modularity, Clustering Coefficient, Betweenness, Assortativity
   - Defined:
     - **Type 1**: High betweenness & density
     - **Type 2**: High clustering & low modularity
   - Identified these recurring types across multiple personal networks.


### Part 2: Google+ Ego Network Analysis

Analyzed directed ego-networks from Google+, focusing on community detection and user-defined "circles":

- **Data**: Directed graphs with users’ edges and circle tags.
- **Community Detection**:
  - Algorithms used: Walktrap, Infomap.
- **Circle Matching Evaluation**:
  - Compared algorithm-detected communities vs. user-defined circles.
  - Calculated average match scores per user.
  - Noted variance in tagging habits:
    - High match scores indicate community-aligned tagging.
    - Low scores suggest overlapping or inconsistent user tagging behavior.


## Technologies Used

- **Language**: R
- **Libraries**: `igraph`
- **Data**:
  - Facebook: `facebook_combined.txt`
  - Google+: `gplus/` folder (with `.edges` and `.circles` files)


## How to Run

### Prerequisites

- R installed on your system
- `igraph` package (`install.packages("igraph")`)

### Steps

1. **Clone the repository**:
   ```bash
   git clone https://github.com/yourusername/ee232e-project1.git
   cd ee232e-project1
   ```

2. **Run the R script**:
   ```r
   source("Project1.R")
   ```

3. **Modify paths as needed**:
   - Change `read.graph()` path for Facebook dataset.
   - Set working directory using `setwd()` for Google+ data (`gplus/` folder).

4. **Execution Notes**:
   - For Google+ analysis, the script will iterate through all ego networks.
   - Console will print progress for each user.


## Results Highlights

- Facebook network demonstrates small-world properties and strong clustering.
- Node 1’s personal network provides a microcosm of wider graph dynamics.
- Infomap and Walktrap outperform in matching Google+ circles with actual community structure.
- Novel identification of two common community types using statistical measures.


## File Structure

```
.
├── Project1.R               # Main R script for all analyses
├── facebook_combined.txt   # Facebook network edgelist (not included)
├── gplus/                   # Directory with Google+ .edges and .circles files (not included)
└── README.md               # Project overview and instructions
```


## References

- [igraph R documentation](https://igraph.org/r/)
- Facebook & Google+ datasets from Stanford SNAP
- EE232E course materials

## License
This project is released for academic and research purposes. Please credit the source if used in publications or derivative works.
