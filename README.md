# COBOL-Annulus-Mask-Moir-Pattern-Generator
The COBOL and python uploaded here allows the user to generate an annulus with a changeable diameter, and it allows you to generate Moire patterns by intentionally using integer overflow and modular arithmetic


1. Required Python Libraries:

Pillow (In code: PIL) – Used for opening, saving, and manipulating images.

NumPy (In code: numpy) – Used for array math and coordinate grids.

Pandas (In code: pandas) – Used in process_image.py to organize data into a CSV/DAT file.

Matplotlib (In code: matplotlib) – Used in COLOURIZER.py to generate the heatmaps/color gradients. Only necessary if you want to convert your data files to coloured heatmaps. 



2. Setting up the COBOL code: 

If you are using GnuCOBOL please open both COBOL files and set the file paths to the .dat files on your computer. For a brief explanation of the moire masks and how to generate them see line 60 of the MoirePatterns.cbl file. 


3. How to Generate:

3.1 	Move an image with the name 'image.png' into a folder with all the other codes and .dat files .

3.2  	Make sure that the file paths in the COBOL is updated. 

3.3  	Run a terminal and make sure that the file path is set to your folder using the cd command. 

3.4	  Run process_image.py, Annulus.cbl/MoirePatterns.cbl and lastly data_to_image.py in that order. colourizer.PY can also be used after you have run your COBOL.
