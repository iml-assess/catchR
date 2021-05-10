# Raw data

This is metadata used to efficiently read in DFO/NAFO data.

*NOTE: commas have been and should be systematically replaced*

## ZIFF

- ziff_meta_csv
    - Previous name: Documentation_Versiontotale.xlsx
    - From: \\dcqcimlna01a\BD_Peches\Ziff\Format CSV\Fichiers de données
    - Adapted: changed column names and added english info (in accordance with nafo database)
- ziff_meta_dat
    - From: Sylvain H.
    - Adapted: changed column names and added english info
- ziff_gear
    - Previous name: Codes_engin.xls
    - From: \\dcqcimlna01a\BD_Peches\Ziff\Documentation\Codes_engin.xls
    - Adapted: 
			+ added NAFO gear codes
			+ fused 99 and 72 because they are doubles (one code can only indicate one gear)
- ziff_tonnage
    - Previous name: Documentation_Versiontotale.xlsx (CL_ton tab)
    - From: \\dcqcimlna01a\BD_Peches\Ziff\Format CSV\Fichiers de données
    - Adapted: tonnage_class column has name/levels identical to NAFO database
- ziff_region
- ziff_species
    - From: \\dcqcimlna01a\BD_Peches\Ziff\Documentation\Codes_especes.xls

## length-frequencies
- lf_key_freq
    - Info: fixed width values for frequency data - lines starting with 5 in lf data
    - From: S:\SAS\Peche\Infile_EntFrq.txt with names from JOP
- lf_key_set
    - Info: fixed width values for set ('trait') data - lines starting with 4 in lf data
    - From: S:\SAS\Peche\Infile_EntFrq.txt with names from JOP
- lf_..... 
	From the meta data in the same text file

##  carbio
- bio_key
    - Info: from R scripts
- bio_zones
    - Info added line for 4SZ that is in lf files but not carbio
	
	