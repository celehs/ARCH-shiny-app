## ARCH network


### Objective of the ARCH Network


Electronic Health Record (EHR) systems house vast amounts of clinical data, spanning both codified entries and free-text narrative notes. These systems cover hundreds of thousands of clinical concepts essential for research and patient care. However, the complex, massive, heterogeneous, and noisy nature of EHR data presents significant challenges for feature representation, information extraction, and uncertainty quantification.


To address these challenges, the Aggregated naRrative Codified Health (ARCH) algorithm efficiently analyzes EHR records, generating a large-scale knowledge graph (KG) that incorporates both codified and narrative features. Based on co-occurrence matrices of EHR features from Veterans Affairs (VA), ARCH produces high-quality knowledge graphs for over 60,000 EHR concepts, including 9,586 codified concepts and 51,423 clinical concepts with unique identifiers (CUIs). Codified concepts encompass ICD diagnosis codes (rolled up to PheCodes), medications (mapped to RxNorm at the ingredient level), procedures (rolled up to procedure categories), and laboratory test results (mapped to LOINC, VA short names, or local lab codes).


The ARCH network algorithm empowers researchers by automatically identifying significant concepts related to specific target concepts, thereby assisting in downstream analyses.


### Description of ARCH


ARCH operates by analyzing relationships between EHR features using co-occurrence matrices from VA data. For each feature pair, it calculates a cosine similarity score (ranging from -1 to 1) and conducts statistical tests to identify conditionally dependent pairs. This means that ARCH determines whether the occurrence of one feature, given the probability of others, affects the likelihood of another feature occurring.


In the resulting knowledge graph, an edge between two nodes represents a conditional dependence between the features, with the edge weight indicating the strength of that dependence (cosine similarity). For example, if you're researching coronary artery disease (CAD), you can search for its corresponding PheCode in ARCH. By setting parameters such as maximum depth and cosine similarity threshold, the knowledge graph will display features directly related to CAD, as well as features related to those related features. This allows you to discover patterns and inspire new research.


Once ARCH identifies features linked to your target code, you can leverage these insights in your own research. For example, if you're phenotyping patients with CAD, you can use the features selected by ARCH and apply the PheNorm algorithm to classify patients.


ARCH also allows for searches across both codified and narrative representations. While it is generally recommended to start with structured codes like PheCodes (for diseases) and RxNorm (for medications) due to their stable performance, NLP-derived features can serve as valuable additional notes to enhance your research.






### Using the app


This tool allows to infer relatedness among diseases, treatment, procedures and laboratory measurements. By performing ARCH across all codified codes and CUIs, we create a knowledge map to help identify and visualize :


- the hierarchy of nodes connected to a selected target node


- the table of nodes connected to a selected target node, including connected nodes' ID and description, category, cosine similarity with the target node and count number


- the network between the target concepts and the concepts that related to at least one of the target concepts


- the circular plot of the cosine similarity between the target concept and the related codified codes, and the circular plot of the cosine similarity between the target concept and the related CUIs


More information for codified codes (i.e., PheCodes, RxNorm, LOINC codes and Procedures) and CUIs:


 - **PheCode Hierarcy:**
   * ICD9: https://phewascatalog.org/phecodes
   * ICD10-CM: https://phewascatalog.org/phecodes_icd10cm
  - **RxNorm Hierarchy:** https://mor.nlm.nih.gov/RxNav/
   * RxNorm disclaimer: This product uses publicly available data courtesy of the U.S. National Library of Medicine (NLM), National Institutes of Health, Department of Health and Human Services; NLM is not responsible for the product and does not endorse or recommend this or any other product.


 - **LOINC Hierarchy:** https://loinc.org/multiaxial-hierarchy/
   * LOINC disclaimer: This material contains content from LOINC (http://loinc.org). LOINC is copyright © 1995-2023, Regenstrief Institute, Inc. and the Logical Observation Identifiers Names and Codes (LOINC) Committee and is available at no cost under the license at http://loinc.org/license. LOINC® is a registered United States trademark of Regenstrief Institute, Inc.
  
 - **Procedure codes:** A unique ID is assigned to each of the procedure category.




  
 - **Concept Unique Identifiers (CUI):**
   * Concept Unique Identifiers or CUIs are unique identifiers that represent a specific concept such as drugs, disorders, procedures etc within the Unified Medical Language System (UMLS) - a compendium of biomedical vocabularies maintained by U.S. National Library of Medicine. UMLS groups names from different terminologies into concepts thus CUIs also serve as a link between different biomedical vocabularies. A CUI is composed of the letter 'C' followed by seven numbers.
   Eg: The concept Diabetes Mellitus is represented by the CUI C0011849. "Unspecified diabetes mellitus", "DM - Diabetes mellitus", "DM" are few other terms mapped to this CUI by UMLS. These terms were sourced from ICD10CM, RCD and MEDLINEPLUS vocabularies respectively.
  
   * The hierarchy information and related concepts from other vocabularies for a CUI can be explored using the UMLS browser at https://uts.nlm.nih.gov/uts/umls

   * Category abbr.

      **ACTI** Activities & Behaviors
      
      **ANAT** Anatomy
      
      **CHEM** Chemicals & Drugs
      
      **CONC** Concepts & Ideas
      
      **DEVI** Devices
      
      **DISO** Disorders
      
      **GENE** Genes & Molecular Sequences
      
      **GEOG** Geographic Areas
      
      **LIVB** Living Beings
      
      **OBJC** Objects
      
      **OCCU** Occupations
      
      **ORGA** Organizations
      
      **PHEN** Phenomena
      
      **PHYS** Physiology
      
      **PROC** Procedures
      
   * See more information at https://www.nlm.nih.gov/research/umls/index.html.




