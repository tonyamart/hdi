### Preliminary tests on the authorship of HDI fragments (from Fonds Vandeul)
  
This repository contains some attribution tests done in order to check the authorship of selected fragments from *Histoire de deux Indes*. The corpus was compiled by Julian Csapo, the test done by Antonina Martynenko; pipeline for the tests by Artjoms Šeļa.  
  
[Corpus](##Corpus)  
[Methodology](##Methodology)  
[Results](##Restults)  

## Corpus
### Reference set
The corpus was collected by Julian Csapo. Corpus metadata is stored as `metadata/corpus_metadata.csv`. The files in the corpus are presented as one file = one book.  
All together the corpus comprises 175 books by 17 authors:  
Baudeau, Chastellux, Condorcet, Deleyre, d'Holbach, Diderot (two sets), Guibert, Jaucourt, Jussieu, La Grange, Meister, Morellet, Naigeon, Pechméja, Raynal, Rivière, Saint-Lambert.  

### Test set
The fragments from the HDI are collected from ARTFL project, the third edition of the HDI (1780). ONLY framgents from **Fonds Vandeul** were taken.  
Each fragment is supplied with the metadata on its appearance in different editions and inclusion in *Pensées détachées* and/or *Mélanges*. The metadata for fragments is stored as `fragments_attributions_upd.csv` (current fragments & metadata are prepared by Julian Csapo).

#### Groups of segments tested
- Segments from different editions:  
  - appeared 1770 - not changed 1774 - not changed 1780  
  - appeared 1770 - changed 1774 - not changed 1780  
  - appeared 1770 - not changed 1774 - changed 1780  
  - appeared 1770 - changed 1774 - changed 1780  
  - appeared 1774 - not changed 1780  
  - appeared 1774 - changed 1780  
  - appeared 1780  
  
- Segments from Pénsées Détachées and Mélanges:
  - segments in pencil (PD)
  - segments in ink (M)
  - segments included in ink, but not in pencil (M without PD)
  
- Separate fragments:
  - Fragments on Russia (68, 310) and China (12) combined together
  - Fragment 'Les avantages de la vie sauvage'
    
The analysis of the fragments in full can be found in `scr/03_analysis.md`, the last three fragments from the list are in `scr/03_2_analysis_additiona.md`.  

The steps on corpus creation and cleaning as well as segments manipulation are shown in `01_preprocessing.qmd` and `02_1_datawriting.qmd` respectively.

## Methodology
The analysis is fully done in R using `stylo()` package and `tidyverse` and `quatro` environment. As the corpus is unbalanced, the main addition to the basic `stylo()` analysis lies in repetitive sampling (independent samples).  
  
Preprocessing
- As OCR sources of the books differ, we tried to clean most frequent errors (long-s issues, -oit => -ait endings, etc.);
- Overlong books by each author were sampled down to 60k random tokens;  
  
Preliminary analysis  
- The books by each author were combined together in one set, two samples taken from each author; 
  - Sample size depends on the size of the document in question, e.g., if the text in question is only 8,000 words, only 2 samples of 4,000 words from each author were taken.
- To see the positioning of texts in question basic dendrogramms (200MFW, cosine delta, Ward's method) and bootstrap consensus trees (50-250 MFW) were built on random samples. - **'Diderot's question'**: Diderot's writings were separated in two sets - Diderot II (Correspondences littéraires) and Diderot I (the rest of the books). In all trials (incl. GI) these two sets of Diderot shows great closeness, proving the overall method to work on our data and with current sampling & processing methods.   
  
Main part of the analysis: General impostors  
- For the main part of the analysis GI method was used (stylo implementation, cosine delta);
  - GI outputs result of 100 trials showing how many times an author was the closest to the text in question: 0 means 'in none of the trials', 1 means 'in all of the trials';  
- 100 iterations of GI, for each a new set of random independent samples were taken from the corpus;  
- The distribution of the 100 GI distributions (from 0 to 1) presented as box plots.  


## Results
### Editions
#### Segments appeared in ed. 1770
Based on the current corpus, in all tests (both BTC and GI) these segments demonstrate closeness to d'Holbach writings. This is the most robust result in all tests done so far.

[plot-1770-nch]

The results for the fragments appeared in the 1770 and changed twice show us similar picture: it is highly likely that d'Holbach might be the author. Though the algorithm is slightly less sure about the fragments which appeared in 1774 but have been changed in the second edition.  

[plot-1770-ch-ch]
[plot-1770-ch-nch]

#### Segments appeared in ed. 1774
The situation with the fragments appeared in 1774 which have not been edited later is more intriguing as in this case the algorithm is not sure about the authorship of any author. The plot below shows us that no author in the set is close enough in their word usage to the fragment in question. As some authors as Pechmeja, Diderot, Baudeau and Riviere all sometimes have results higher than 40-50%, there is not enough evidence to say if any of them is a likely candidate. One of the explanation here could be that the fragments combined as "edition 1774 - not changed" are in fact belong to multiple authors from the set or outside it.

[plot-1774-nch]

#### Segments appeared in ed. 1780
In the case of the latest added fragments, the situation is mixed: on the one hand, there is some signs of similarity with d'Holbach's writings. However, the authorship signal is much less clear in comparison with ed. 1770.

[plot-1780]

### PD vs M
In the case of red pencil marks our results are quite similar to that of the edition of 1770. There is quite strong signal of d'Holbach authorship, although in this case the algorithm is less sure than in case of fragments from the earlier editions. 

[plot-PD]

The observations on the word usage in Mélanges are more questionable, as this selection leads us to the mixed-authorship signal: sporadically some text fragments are resembling some of the authors from our corpus. Nevertheless, we do not have an authorship signal strong enough to select one candidate in this case as well.

[plot-M]
[plot-M-without-PD]

### Separate fragments
The combined set of fragments about Russia and China shows no clear authorship signal. Nevertheless, the fragment 'Les avantages de la vie sauvage' hints to be close do Diderot (thought the result is not yet robust enough at the moment)

[plot-sauvage]
