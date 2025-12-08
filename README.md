# Community Notes Project

This repository contains the code for our project looking into X's Community 
Notes content moderation. More information about Community Notes can be 
found [here](https://communitynotes.x.com/guide/en/about/introduction). 


Files contained in this repo: 
- `data_cleaning.Rmd` - Contains the run through and explanation of the data cleaning process.
- `cleaning.R` - Contains the functions used in cleaning the Community Notes data.
- `llm_prompting.Rmd` - Contains the run through and explanation of the Grok prompting process
- `grok_notes_prompting.R` - Contains the functions used in prompting Grok.
- `topic_selection.Rmd` - Contains the process of doing topic modeling on the notes data.
- `topic_modeling.R` - Contains the functions used in the topic modeling process. 
- `status_classification_modeling.R` - Contains the process of modeling the status outcome of the notes.
- `classification.R` - Contains the functions used in modeling the status outcomes.
- `additional_work` - Contains additional files `mini_data.ipynb`, `test_commnotes.ipynb` and 
`unused_methods.R`. These contain work that is not relevant to the current version of this 
project, but may be useful in future iterations. 
- `gen_ai.md` - Contains a summary of the use of generative AI in this project. 
- `.gitignore` - Contains declarations of variables to ignore when pushing this repo. 

Directories:
- `additional_work` - Contains code to run X's scoring algorithm locally.
- `plots` - Contains plots used in the executive summary and technical appendix.
- `data` - Contains only the small amount of grok generated data. 

Note: The `mini_data.ipynb` and `test_commnotes.ipynb` files are both from an 
earlier iteration of the project in which we attempted to run the Community Notes
ranking algorithm. The hope here was to intergrate LLM generated notes into this.


The main issue we ran into was the need to alter all the tensor movements from GPU to CPU. 
Debugging this problem became too onerous for the scope of this project. However the files
are included. 

The `mini_data.ipynb` file should be within the `.data` directory, which in turn should be in 
`communitynotes/scoring/src` of a cloned https://github.com/twitter/communitynotes repo. Our attempt 
at running the code, `test_commnotes.ipynb` should also be placed in the `src` file, as it mirrors 
the `main.py` file in the same directory. 

