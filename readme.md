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
The fragments from the HDI are collected from ARTFL project, the third edition of the HDI (1780). Each fragment is supplied with the metadata on its appearance in different editions and inclusion in *Pensées détachées* and/or *Mélanges*. The metadata for fragments is stored as `fragments_attributions_upd.csv` (current fragments & metadata are prepared by Julian Csapo).

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

## Methodology
